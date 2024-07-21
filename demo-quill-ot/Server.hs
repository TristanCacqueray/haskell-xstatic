{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- Run with: ghcid -W --test main

module Main (main) where

import Control.Applicative ((<|>))
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.String.QQ (s)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import Lucid
import Lucid.XStatic (xstaticScripts)
import Network.Socket (SockAddr)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets qualified as WS
import Say (sayShow)
import Servant hiding (Server)
import Servant.API.WebSocket
import Servant.HTML.Lucid

import XStatic qualified as XStatic
import XStatic.Quill qualified as XStatic

import Control.OperationalTransformation.Client qualified as OT
import Control.OperationalTransformation.Server qualified as OT
import Control.OperationalTransformation.Text qualified as OT

type OTClient = OT.ClientState OT.TextOperation
type OTServer = OT.ServerState Text OT.TextOperation

-- | Event from the server to the client
data ServerEvent
    = -- | Reset (optional error message, current revision, current body)
      Reset (Maybe String, OT.Revision, Text)
    | -- | Apply an operation
      ApplyOp (OT.Revision, OT.TextOperation)
    | DoAck OT.Revision
    deriving stock (Show)

-- | Event from the client to the server
data ServerRequest
    = DoReset (Maybe String)
    | DoApply OT.Revision OT.TextOperation

-- | Event from the remote client
data ClientEvent
    = ClientOp (OT.Revision, OT.TextOperation)
    | Ack OT.Revision
    deriving stock (Show)

data Server = Server
    { clients :: TVar [Client]
    -- ^ The list of connected clients
    , chan :: TChan (Client, ServerRequest)
    -- ^ Receive request through a channel
    }

data Client = Client
    { name :: Text
    , conn :: WS.Connection
    , chan :: TChan (Either ServerEvent ClientEvent)
    }

-- | The server thread process request one at a time
serverThread :: Server -> IO Void
serverThread server = go $ OT.initialServerState mempty
  where
    go :: OTServer -> IO Void
    go state = do
        (client, ev) <- atomically $ readTChan server.chan
        case ev of
            DoReset mErr -> resetClient client mErr state
            DoApply rev op ->
                case OT.applyOperation state rev op () of
                    Left err -> resetClient client (Just err) state
                    Right (newOp, _cursor, newState) -> do
                        let OT.ServerState newRev newDoc _op = newState
                        sayShow ("New state after " <> client.name, newRev, newDoc)
                        atomically $ broadcastOperation client newRev newOp
                        go newState

    sendClient client = writeTChan client.chan . Left

    -- Operation failed, reset the client and continue with existing state
    resetClient client mErr state@(OT.ServerState rev doc _op) = do
        atomically $ sendClient client $ Reset (mErr, rev, doc)
        go state

    broadcastOperation :: Client -> OT.Revision -> OT.TextOperation -> STM ()
    broadcastOperation client rev op = do
        -- The requesting user needs to Ack
        sendClient client $ DoAck rev

        -- The others need to ApplyOp
        clients <- readTVar server.clients
        forM_ clients \oclient -> do
            when (oclient.name /= client.name) do
                sendClient oclient $ ApplyOp (rev, op)

-- | The client thread process server and remote client events, ont at a time.
clientThread :: Server -> Client -> IO Void
clientThread server client = go OT.initialClientState
  where
    go :: OTClient -> IO Void
    go state = do
        atomically (readTChan client.chan) >>= \case
            Left serverEvent -> do
                sayShow ("reply for " <> client.name, encode serverEvent)
                handleServerEvent state serverEvent
            Right clientEvent -> do
                sayShow ("<- " <> client.name, clientEvent)
                handleClientEvent state clientEvent

    handleServerEvent state ev = case ev of
        ApplyOp (rev, op) -> case OT.applyServer state op of
            Left err -> do
                sayShow ("applyServer failed: " <> err)
                atomically $ writeTChan server.chan (client, DoReset (Just err))
                go OT.initialClientState
            Right (newOp, newState) -> do
                sendJSON client $ ApplyOp (rev, newOp)
                go newState
        -- Send the new document and reset the state
        Reset{} -> sendJSON client ev >> go OT.initialClientState
        -- Forward the ack to the remote client, we only acknowledge after a roundtrip
        DoAck{} -> sendJSON client ev >> go state

    handleClientEvent state = \case
        -- The remote client made a change
        ClientOp (rev, op) -> do
            case OT.applyClient state op of
                Left err -> do
                    sayShow ("applyClient failed: " <> err)
                    atomically $ writeTChan server.chan (client, DoReset (Just err))
                    go OT.initialClientState
                Right (forServer, newState) -> do
                    when forServer do
                        atomically $ writeTChan server.chan (client, DoApply rev op)
                    go newState
        -- The remote client received the DoAck, acknowledge now.
        Ack rev -> do
            case OT.serverAck state of
                Nothing -> go state
                Just (mOp, newState) -> do
                    forM_ mOp \sop ->
                        -- Forward extra events to the server
                        atomically $ writeTChan server.chan (client, DoApply rev sop)
                    go newState

-- | Create a new 'Server'
newServer :: STM Server
newServer = Server <$> newTVar mempty <*> newTChan

-- | Create a new 'Client' for 'withClient'
addClient :: Server -> SockAddr -> WS.Connection -> STM Client
addClient server remoteAddr connection = do
    let name = T.pack $ show remoteAddr
    client <- Client name connection <$> newTChan
    modifyTVar' server.clients (client :)
    pure client

-- | Delete a 'Client' by 'withClient'
removeClient :: Server -> Client -> STM ()
removeClient server client = modifyTVar' server.clients $ filter (\c -> c.name /= client.name)

withClient :: Server -> SockAddr -> WS.Connection -> (Client -> IO a) -> IO a
withClient server addr conn =
    bracket
        (atomically $ addClient server addr conn)
        (atomically . removeClient server)

sendJSON :: (ToJSON a) => Client -> a -> IO ()
sendJSON client message = WS.sendTextData client.conn $ Data.Aeson.encode message

indexHtml :: Html ()
indexHtml = do
    doctypehtml_ do
        head_ do
            title_ "quill-ot"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_ [rel_ "icon", href_ "data:;base64,="]
            xstaticScripts XStatic.quill

        body_ do
            with button_ [id_ "trig"] do
                "Simulate edits"
            with div_ [id_ "editor"] do
                pure ()
            script_ clientJS

clientJS :: Text
clientJS =
    [s|
// Quill use the following delta encoding:
// - {insert: "text"}
// - {retain: count}
// - {delete: count}

// ot.hs use the following encoding:
// - "text"
// - count (for retain)
// - -count (for delete)

// Convert delta from Quill to ot.hs
function encodeDelta(d) {
  if ('insert' in d) {
    return d.insert
  } else if ('retain' in d) {
    return d.retain
  } else if ('delete' in d) {
    return -1 * d.delete
  } else {
    throw ("Unknown delta: " + d)
  }
}

// Convert delta from ot.hs to Quill
function decodeDelta(d) {
  if (typeof d === 'string' || d instanceof String) {
    return {insert: d}
  } else if (d > 0) {
    return {retain: d}
  } else {
    return {delete: d * -1}
  }
}

// ot.hs requires the delta to cover the full text, including the bits after the edit
// so this funciton adds an extra retain if necessary
function addLastRetain(remaining, delta) {
  delta.forEach(op => {
    if ('insert' in op) {
      remaining -= op.insert.length
    } else if ('retain' in op) {
      remaining -= op.retain
    }
  })
  if (remaining > 0) {
    const newDelta = delta.slice()
    return newDelta.retain(remaining)
  } else {
    return delta
  }
}

// Helpers to simulate edits
function triggerEdits(quill) {
  function simulateEdits(quill, edits) {
    if (edits.length > 0) {
      setTimeout(() => {
        const edit = edits[0];
        quill.updateContents(edit, "user")
        simulateEdits(quill, edits.slice(1))
      }, 100)
    }
  }

  function makeEdits() {
    const xs = [
      {ops: [{insert: "hello "}]},
      {ops: [{retain: 6}, {insert: "world"}]},
    ]
    for (i = 0; i < 20; i++) {
      if (i == 10) {
        xs.push({ops: [{insert: "hi!"}]})
      }
      xs.push({ops: [{insert: "\n"}]})
    }
    return xs
  }
  console.log("Starting simulation...")
  setTimeout(() => simulateEdits(quill, makeEdits()), 1000)
}

function setupClient() {
  const webSocket = new WebSocket("ws://localhost:8080/ws");
  webSocket.onopen = (event) => {
    console.log('Connected!')
  };
  webSocket.onerror = (event) => {
    console.error('Disconnected!')
  };

  const quill = new Quill('#editor', {
    theme: 'snow',
    modules: {
      toolbar: '#toolbar'
    },
    formats: []
  });

  document.getElementById("trig").onclick = () => triggerEdits(quill)

  // Keep track of the revision sent by the server
  let revision = 0
  webSocket.onmessage = (event) => {
    console.log("Got event:", event.data)
    const msg = JSON.parse(event.data)
    if ('rev' in msg) {
      revision = msg.rev
      if ('body' in msg) {
        quill.setContents([{insert: msg.body + '\n'}], "api")
      } else {
        // Acknowledge round trip
        webSocket.send(JSON.stringify(revision))
      }
      if ('err' in msg) {
          console.error("Got an error: " + msg.err)
      }
    } else if (msg.length == 2) {
      const [rev, delta] = msg
      quill.updateContents({ops: delta.map(decodeDelta)}, "api")
      revision = rev;
    }
  };

  quill.on('text-change', (delta, oldDelta, source) => {
    console.log("text-change", source, JSON.stringify(delta), "old:" + JSON.stringify(oldDelta))
    if (source === "user") {
      // Remove one byte because quill always include a trailing \n
      const length = quill.getLength() - 1
      const ops = addLastRetain(length, delta).ops.map(encodeDelta)
      webSocket.send(JSON.stringify([revision, ops]))
    }
  });
}
setupClient()
|]

data API mode = API
    { index :: mode :- Get '[HTML] (Lucid.Html ())
    , ws :: mode :- "ws" :> RemoteHost :> WebSocket
    }
    deriving stock (Generic)

-- | The main web application
app :: Server -> Wai.Application
app server =
    Servant.serveWithContext
        (Proxy @(NamedRoutes API))
        EmptyContext
        API
            { index = pure indexHtml
            , ws = \addr conn -> liftIO $ WS.withPingThread conn 30 (pure ()) do
                withClient server addr conn handleNewClient
            }
  where
    handleNewClient :: Client -> IO ()
    handleNewClient client = race_ (clientThread server client) do
        sayShow ("New client: " <> client.name)
        atomically $ writeTChan server.chan (client, DoReset Nothing)
        forever do
            buf <- WS.receiveData @LBS.ByteString client.conn
            case decode @ClientEvent buf of
                Just ev -> atomically $ writeTChan client.chan (Right ev)
                Nothing -> putStrLn $ "Invalid data: " <> show buf

instance FromJSON ClientEvent where
    parseJSON v = (ClientOp <$> parseJSON v) <|> (Ack <$> parseJSON v)

instance ToJSON ServerEvent where
    toJSON = \case
        Reset (errM, rev, body) -> toJSON $ object $ maybe id (\e -> ("err" .= e :)) errM ["rev" .= rev, "body" .= body]
        ApplyOp tup -> toJSON tup
        DoAck rev -> toJSON $ object ["rev" .= rev]

-- | The demo entry point
main :: IO ()
main = do
    putStrLn "Running on localhost:8080"
    server <- atomically newServer
    race_ (serverThread server) do
        Warp.run 8080 (XStatic.xstaticMiddleware XStatic.quill $ app server)
