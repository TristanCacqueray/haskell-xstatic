-- Run with: ghcid -W --test main
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Network.Socket (SockAddr)
import Network.Wai.Handler.Warp qualified as Warp

import Servant
import Servant.API.WebSocket
import Servant.HTML.Lucid

import Ki qualified

import Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx

import Control.Lens ((^?))
import Data.Aeson.Lens

import Network.WebSockets qualified as WS
import Relude hiding (state)

import XStatic qualified as XStatic
import XStatic.Htmx qualified as XStatic
import XStatic.Remixicon qualified as XStatic
import XStatic.Tailwind qualified as XStatic

type ServerAPI =
    Get '[HTML] (Html ())
        :<|> "connect" :> RemoteHost :> WebSocket
        :<|> "reconnect" :> WebSocket

data ServerState = ServerState
    { clients :: TVar [Client]
    , history :: TVar [Text]
    , content :: TVar Text
    }

data Client = Client
    { name :: Text
    , conn :: WS.Connection
    }

newState :: STM ServerState
newState = ServerState <$> newTVar mempty <*> newTVar initHistory <*> newTVar initContent
  where
    initHistory = [renderStrict $ pre_ "Welcome, type 'help' for instructions"]
    -- initHistory = map fakeMessage (reverse [0 .. 10])
    --  where
    --    fakeMessage c = toStrict $ renderText $ pre_ $ toHtml ("hello " <> show @Text @Int c)
    initContent = toStrict $ renderText $ with div_ [id_ "butler-window", class_ "m-auto"] "Welcome"

addClient :: ServerState -> SockAddr -> WS.Connection -> STM Client
addClient state remoteAddr connection = do
    let client = Client (show remoteAddr) connection
    modifyTVar' state.clients (client :)
    pure client

removeClient :: ServerState -> Client -> STM ()
removeClient state client = modifyTVar' state.clients $ filter (\c -> c.name /= client.name)

numClients :: ServerState -> STM Int
numClients state = length <$> readTVar state.clients

addHistory :: ServerState -> Html () -> STM ()
addHistory state msg = modifyTVar' state.history (toStrict (renderText msg) :)

getHistory :: ServerState -> STM (Html ())
getHistory state = toHtmlRaw . unlines . reverse <$> readTVar state.history

setContent :: ServerState -> Html () -> STM ()
setContent state body = writeTVar state.content $ toStrict (renderText body)

getContent :: ServerState -> STM (Html ())
getContent state = toHtmlRaw <$> readTVar state.content

sendText :: Client -> LText -> IO ()
sendText client = WS.sendTextData client.conn

broadcast :: ServerState -> Html () -> IO ()
broadcast state message = do
    xs <- atomically (readTVar state.clients)
    let body = renderText message
    forM_ xs $ \client -> sendText client body

indexHtml :: Html ()
indexHtml = do
    doctypehtml_ do
        head_ do
            title_ "Websocket-Ki-Htmx"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            xCss [XStatic.remixiconCss]
            xJs [XStatic.tailwind, XStatic.htmx, XStatic.htmxExtWS]

        with body_ [class_ "font-mono cursor-default bg-stone-100 h-screen"] do
            with div_ [id_ "butler-ws", makeAttribute "hx-ext" "ws", makeAttribute "ws-connect" "/connect"] do
                with div_ [id_ "butler-root"] mempty

            -- Update the status when the connection is dropped
            script_ do
                "document.body.addEventListener('htmx:wsError', function(evt) { \n\
                \ htmx.find('#butler-status').innerText = '<disconnected>';   });"

            -- Change the reconnect delay to a constant value
            script_ "htmx.config.wsReconnectDelay = (retryCount) => 1000;"
  where
    xJs = traverse_ (\xf -> with (script_ mempty) [src_ $ "/xstatic" <> decodeUtf8 xf.xfPath])
    xCss = traverse_ (\xf -> link_ [href_ $ "/xstatic" <> decodeUtf8 xf.xfPath, rel_ "stylesheet"])

bodyHtml :: Html () -> Html () -> Html ()
bodyHtml content' chat =
    with div_ [id_ "butler-root", class_ "h-full p-5"] do
        with div_ [class_ "h-96 flex flex-row"] do
            with div_ [class_ "basis-2/3 flex h-full p-1 border border-black"] do
                content'

            with div_ [class_ "basis-1/3 p-1 border border-grey inline-block overflow-y-auto"] do
                with div_ [id_ "butler-chat"] chat

        with div_ [class_ "flex flex-row"] do
            with div_ [class_ "basis-2/3 p-1 text-right"] do
                with span_ [id_ "butler-status"] do
                    "<connected>"
                    -- After connection, we switch the socket url to the reload endpoint,
                    -- so that the page is reloaded when reconnected
                    script_ "htmx.find('#butler-ws').setAttribute('ws-connect', '/reconnect')"
                with span_ [id_ "butler-count"] mempty
            with div_ [class_ "basis-1/3 w-full"] do
                with form_ [id_ "chat-form", wsSend] do
                    with (input_ mempty) [name_ "msg", class_ "w-full border border-grey p-1 mt-2"]

userCountHtml :: Int -> Html ()
userCountHtml x =
    with div_ [id_ "butler-count", class_ "p-1 m-1 border border-black rounded inline-block"] do
        with span_ [class_ "font-bold"] (toHtml (show @Text x))
        with i_ [class_ "ri-user-line align-bottom pt-2"] mempty

messageHtml :: Client -> Text -> Html ()
messageHtml client msg =
    pre_ do
        toHtml (client.name)
        ": "
        toHtml msg

helpHtml :: Html ()
helpHtml = do
    with h1_ [class_ "font-bold text-center"] "Help"
    ul_ do
        li_ "help: show help"
        li_ "bang: show bang"
        li_ "_: chat"

bangHtml :: Html ()
bangHtml = with i_ [class_ "font-bold text-8xl ri-star-line"] mempty

application :: Ki.Scope -> ServerState -> Server ServerAPI
application scope state = pure indexHtml :<|> connectRoute :<|> reconnectRoute
  where
    reconnectRoute connection = liftIO do
        sendData connection do
            with span_ [id_ "butler-status"] do
                "<reconnecting...>"
                script_ "window.location.reload()"

    connectRoute clientAddr connection = liftIO do
        -- Add the client to server state
        client <- atomically $ addClient state clientAddr connection

        -- Fork a ki thread with the client handler and wait for completion or crash
        thread <- Ki.forkTry @SomeException scope (doConnectRoute client)
        void $ atomically $ Ki.await thread

        -- Remove the client from the server state and update the connected counter
        atomically $ removeClient state client
        updateCounter

    doConnectRoute client = do
        -- Send the page body with the current content and chat history
        body <- atomically (bodyHtml <$> getContent state <*> getHistory state)
        sendData client.conn body

        -- broadcast the new client count
        updateCounter

        -- wait for user events
        WS.withPingThread client.conn 30 (pure ()) $ forever do
            buf <- WS.receiveData @Text client.conn
            -- The "msg" key is defined by the chat form input name
            case buf ^? key "msg" . _String of
                Just msg -> handleMessage client msg
                Nothing -> putTextLn $ "unknown payload: " <> buf

    handleMessage client = \case
        "help" -> updateContent helpHtml
        "bang" -> updateContent bangHtml
        msg -> updateChat (messageHtml client msg)

    updateChat msg = do
        -- Add the message to the server state history
        atomically $ addHistory state msg
        -- Broadcast the message to be added at the bottom of the butler-chat div
        broadcast state (with div_ [id_ "butler-chat", hxSwapOob_ "beforeend"] msg)

    updateContent body = do
        -- Update the window content and broadcast the div
        let dom = with div_ [id_ "butler-window", class_ "m-auto"] body
        atomically $ setContent state dom
        broadcast state dom

    updateCounter = do
        clientsCount <- atomically $ numClients state
        broadcast state (userCountHtml clientsCount)

    sendData connection = WS.sendTextData connection . renderText

main :: IO ()
main = do
    putStrLn "Running on localhost:8080"
    state <- atomically newState

    Ki.scoped $ \scope ->
        Warp.run 8080 $
            XStatic.xstaticMiddleware
                [XStatic.htmx, XStatic.htmxExtWS, XStatic.tailwind, XStatic.remixiconCss, XStatic.remixiconWoff2]
                (Servant.serve (Proxy @ServerAPI) (application scope state))

wsSend :: Attribute
wsSend = makeAttribute "ws-send" ""

renderStrict :: Html () -> Text
renderStrict = toStrict . renderText
