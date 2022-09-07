{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan.Unagi qualified as Unagi
import Data.ByteString qualified as BS
import Data.String.Interpolate (i)
import Ki qualified
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Relude hiding (state)
import System.Posix.Pty qualified as Pty

import Lucid
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid

import Lucid.XStatic
import Servant.XStatic
import XStatic.Xterm qualified as XStatic

type ServerAPI =
    Get '[HTML] (Html ())
        :<|> "connect" :> WebSocket
        :<|> "xstatic" :> Raw

application :: (Unagi.InChan ByteString, IO (Unagi.OutChan ByteString)) -> Server ServerAPI
application (inWriter, mkOutReader) = pure index :<|> connectRoute :<|> xstaticServant xfiles
  where
    xfiles = XStatic.xterm
    index = do
        doctypehtml_ do
            head_ do
                title_ "Websocket-Ki-Htmx"
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                xstaticScripts xfiles
            with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
                with div_ [id_ "terminal"] mempty
                script_ client

    connectRoute conn = liftIO $ Ki.scoped \scope -> do
        outReader <- mkOutReader
        scope `Ki.fork_` forever do
            termOut <- Unagi.readChan outReader
            WS.sendDataMessage conn (WS.Binary (BS.fromStrict termOut))
        WS.sendDataMessage conn (WS.Binary ("Welcome!\n\r"))
        WS.withPingThread conn 30 (pure ()) $ forever do
            buf <- WS.receiveData @ByteString conn
            Unagi.writeChan inWriter buf

client :: Text
client =
    [i|
// Start terminal
var term = new Terminal();
term.open(document.getElementById('terminal'));
term.write('Connecting...\\n\\r');

// Connect to server
const socket = new WebSocket("ws://" + window.location.host + "/connect");
socket.binaryType = 'arraybuffer';


// Handle i/o
term.onData(d => {
  // console.log("Input to server:", d);
  socket.send(d)
});
socket.addEventListener('message', (event) => {
    const buf = new Uint8Array(event.data)
    // console.log("Output from server", buf);
    term.write(buf)
});
|]

main :: IO ()
main = do
    let args = ["--ro-bind", "/usr", "/usr", "--proc", "/proc", "--ro-bind", "/lib64", "/lib64", "--ro-bind", "/etc", "/etc", "--unshare-all", "bash"]

    (inWriter, inReader) <- Unagi.newChan
    (outWriter, _outReader) <- Unagi.newChan

    Ki.scoped $ \scope ->
        do
            (pty, _handle) <- Pty.spawnWithPty Nothing True "bwrap" args (80, 25)
            threadDelay 1_000

            -- web service thread
            scope `Ki.fork_` do
                putStrLn "Listening on :8080"
                Warp.run 8080 (Servant.serve (Proxy @ServerAPI) (application (inWriter, Unagi.dupChan outWriter)))
                fail "Warp exited?"

            -- shell writer thread
            scope `Ki.fork_` forever do
                inputData <- Unagi.readChan inReader
                print $ "sending: " <> inputData
                Pty.writePty pty inputData

            -- shell reader thread
            forever do
                Pty.threadWaitReadPty pty
                outputData <- Pty.readPty pty
                print $ "read   : " <> outputData
                Unagi.writeChan outWriter outputData
