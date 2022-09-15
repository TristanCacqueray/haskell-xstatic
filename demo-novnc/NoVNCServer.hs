{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.String.Interpolate (i)
import Ki qualified
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import System.Environment (getArgs)

import Network.Run.TCP (runTCPClient)
import Network.Socket.ByteString (recv, sendAll)

import Lucid
import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid

import Servant.XStatic
import XStatic.NoVNC qualified as XStatic

type ServerAPI = Get '[HTML] (Html ()) :<|> "vnc" :> WebSocket :<|> Raw

application :: String -> String -> Server ServerAPI
application host port = pure index :<|> clientRoute :<|> xstaticServant XStatic.noVNC
  where
    index = do
        doctypehtml_ do
            head_ do
                title_ "demo-novnc"
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                with (script_ client) [type_ "module"]
            body_ do
                h1_ "haskell-xstatic >>= demo noVNC"
                with div_ [id_ "terminal"] mempty

    client =
        [i|
      import RFB from '/noVNC.js';
      let rfb = new RFB(document.getElementById('terminal'), "ws://" + window.location.host + "/vnc");
      rfb.qualityLevel = 0; rfb.compressionLevel = 9;
      globalThis.rfb = rfb;
|]

    clientRoute conn = liftIO $ WS.withPingThread conn 30 (pure ()) $ Ki.scoped \scope -> do
        putStrLn $ "Connecting client!"
        runTCPClient host port $ \skt -> do
            scope `Ki.fork_` forever do
                buf <- WS.receiveData @BS.ByteString conn
                sendAll skt buf

            forever do
                buf <- recv skt 4096
                WS.sendDataMessage conn (WS.Binary (BS.fromStrict buf))

main :: IO ()
main =
    getArgs >>= \case
        [host, port] -> do
            putStrLn $ "Listening on :8080 for " <> host <> ":" <> port
            Warp.run 8080 $ Servant.serve (Proxy @ServerAPI) $ application host port
        _ -> putStrLn "usage: demo-novnc localhost 5900"
