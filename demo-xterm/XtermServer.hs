{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent.STM
import Data.ByteString qualified as BS
import Data.String.Interpolate (i)
import Ki qualified
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Relude hiding (atomically, state)
import System.Posix.Pty qualified as Pty

import Lucid
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid

import Lucid.XStatic
import Servant.XStatic
import XStatic.Xterm qualified as XStatic

data XtermServer = XtermServer
    { inputChan :: TChan ByteString
    , outputChan :: TChan ByteString
    }

newXtermServer :: STM XtermServer
newXtermServer = XtermServer <$> newTChan <*> newBroadcastTChan

type ServerAPI =
    Get '[HTML] (Html ())
        :<|> "term" :> WebSocket
        :<|> "xstatic" :> Raw

application :: XtermServer -> Server ServerAPI
application server = pure index :<|> termRoute :<|> xstaticServant xfiles
  where
    xfiles = XStatic.xterm
    index = do
        doctypehtml_ do
            head_ do
                title_ "demo-xterm"
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                xstaticScripts xfiles
            with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
                with div_ [id_ "terminal"] mempty
                script_ client

    termRoute conn = liftIO $ WS.withPingThread conn 30 (pure ()) $ Ki.scoped \scope -> do
        WS.sendDataMessage conn (WS.Binary ("\nConnected!\n\r"))

        -- Thread to read terminal input
        scope `Ki.fork_` forever do
            buf <- WS.receiveData @ByteString conn
            atomically $ writeTChan server.inputChan buf

        -- Thread to forward terminal output
        outReader <- atomically (dupTChan server.outputChan)
        forever do
            termOut <- atomically $ readTChan outReader
            WS.sendDataMessage conn (WS.Binary (BS.fromStrict termOut))

cols, rows :: Int
cols = 100
rows = 40

client :: Text
client =
    [i|
// Start terminal
var term = new Terminal();
term.open(document.getElementById("terminal"));
term.writeln("== haskell-xstatic xterm demo ==");
term.writeln("If keyboard control sequences appears as '^[[' or '^L', then run 'env - TERM=xterm bash'");

// Connect to server
const socket = new WebSocket("ws://" + window.location.host + "/term");
socket.binaryType = 'arraybuffer';

// Handle i/o
term.onData(data => {
  // console.log("Input to server:", data);
  socket.send(data)
});
socket.addEventListener('message', (event) => {
    const buf = new Uint8Array(event.data)
    // console.log("Output from server", buf);
    term.write(buf)
});

// Fix size
term.resize(#{cols}, #{rows})
// somehow the viewport width doesn't match the screen, the next expression fix that.
term._core._viewportElement.style.width = term._core.screenElement.style.width
|]

main :: IO ()
main = do
    (cmd, args) <- do
        args <- getArgs
        pure $ case args of
            cmd : cmdArgs -> (cmd, cmdArgs)
            [] -> ("bwrap", ["--ro-bind", "/usr", "/usr", "--proc", "/proc", "--ro-bind", "/lib64", "/lib64", "--ro-bind", "/etc", "/etc", "--unshare-all", "bash"])

    server <- atomically newXtermServer

    Ki.scoped $ \scope ->
        do
            (pty, _handle) <- Pty.spawnWithPty Nothing True cmd args (cols, rows)

            -- web service thread
            scope `Ki.fork_` do
                putBSLn "Listening on :8080"
                Warp.run 8080 $ Servant.serve (Proxy @ServerAPI) $ application server
                fail "Warp exited?"

            -- shell writer thread
            scope `Ki.fork_` forever do
                inputData <- atomically $ readTChan server.inputChan
                Prelude.putStrLn $ "sending: " <> Prelude.show inputData
                Pty.writePty pty inputData

            -- shell reader thread
            forever do
                Pty.threadWaitReadPty pty
                outputData <- Pty.readPty pty
                -- Prelude.print $ "read   : " <> outputData
                atomically $ writeTChan server.outputChan outputData
