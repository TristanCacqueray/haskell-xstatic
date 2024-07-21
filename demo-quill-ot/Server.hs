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
{-# OPTIONS_GHC -Wno-partial-fields #-}

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
import Data.IntMap.Strict qualified as IM
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
import XStatic.QuillCursors qualified as XStatic

import Control.OperationalTransformation qualified as OT (compose)
import Control.OperationalTransformation.Client qualified as OT
import Control.OperationalTransformation.Selection qualified as OT
import Control.OperationalTransformation.Server qualified as OT
import Control.OperationalTransformation.Text qualified as OT

type OTClient = OT.ClientState OT.TextOperation
type OTServer = OT.ServerState Text OT.TextOperation
type Cursors = [(ClientID, OT.Range)]
type ClientID = Int
type ClientQueue = (OT.Revision, [OT.TextOperation])

-- | Event from the server to the client
data ServerEvent
    = -- | Reset (optional error message, current revision, current body)
      Reset {mErr :: Maybe String, rev :: OT.Revision, body :: Text}
    | -- | Apply an operation
      ApplyOp (OT.Revision, OT.TextOperation)
    | NewSelection {cid :: ClientID, mRange :: Maybe OT.Range}
    | NewClient {cid :: ClientID, name :: Text, color :: Text}
    | DelClient ClientID
    | DoAck OT.Revision

-- | Event from the client to the server
data ServerRequest
    = DoReset {err :: String}
    | DoApply OT.Revision OT.TextOperation
    | DoSelection (Maybe OT.Range)
    | ClientConnected
    | ClientLeft

-- | Event from the remote client
data ClientEvent
    = ClientOp (OT.Revision, OT.TextOperation)
    | SetSelection (Maybe OT.Range)
    | Ack OT.Revision
    deriving stock (Show)

newtype Server = Server {chan :: TChan (Client, ServerRequest)}

data Client = Client
    { name :: Text
    , conn :: WS.Connection
    , chan :: TChan (Either ServerEvent ClientEvent)
    }

-- | The server thread process request one at a time
serverThread :: Server -> IO Void
serverThread server = go (OT.initialServerState mempty) [] mempty
  where
    go :: OTServer -> Cursors -> IM.IntMap Client -> IO Void
    go state cursors clients = do
        (client, ev) <- atomically $ readTChan server.chan
        case ev of
            ClientConnected -> do
                sayShow ("new client" <> client.name)
                let cid = IM.size clients
                    color = getColor cid
                atomically do
                    -- advertize the new client
                    forM_ (IM.toList clients) \(ocid, oclient) -> do
                        sendClient oclient $ NewClient{cid, name = client.name, color}
                        sendClient client $ NewClient{cid = ocid, name = oclient.name, color = getColor ocid}
                    -- send body
                    let OT.ServerState rev doc _ = state
                    sendClient client $ Reset Nothing rev doc
                    -- send cursors
                    forM_ cursors \(ocid, range) ->
                        sendClient client $ NewSelection{cid = ocid, mRange = Just range}
                -- update client body
                go state cursors (IM.insert cid client clients)
            ClientLeft -> do
                sayShow ("client left" <> client.name)
                let newClients = IM.filter (\c -> c.name /= client.name) clients
                newCursors <- case getClientID client clients of
                    Nothing -> pure cursors
                    Just cid -> do
                        atomically $ forM_ (IM.elems clients) \oclient -> do
                            sendClient oclient $ DelClient cid
                        pure $ removeCursor cid cursors
                go state newCursors newClients
            DoSelection mRange -> case getClientID client clients of
                Nothing -> do
                    sayShow ("unknown client " <> client.name)
                    go state cursors clients
                Just clientID -> do
                    sayShow ("new selection " <> client.name, clientID, mRange)
                    atomically $ broadcastSelection clientID mRange (IM.toList clients)
                    let newCursors = case mRange of
                            Nothing -> removeCursor clientID cursors
                            Just range -> addCursor clientID range cursors
                    go state newCursors clients
            DoReset{err} -> resetClient client err
            DoApply rev op -> do
                sayShow ("Applying rev " <> client.name, rev)
                -- cursors update does not seems to work after 'applyOperation'
                -- so we rely on the client to send updated selection on changed event
                -- but that's not good, we need to update them server side to send the
                -- new positions right away. See the 'selection-change' handler below.
                case OT.applyOperation state rev op () of
                    Left err -> resetClient client err
                    Right (newOp, _newOTCursors, newState) -> do
                        let OT.ServerState newRev newDoc _op = newState
                        sayShow ("New state after " <> client.name, newRev, newDoc)
                        atomically $ broadcastOperation client newRev newOp (IM.elems clients)
                        go newState cursors clients
      where
        resetClient client err = do
            let OT.ServerState rev doc _ = state
            atomically $ sendClient client $ Reset (Just err) rev doc
            go state cursors clients

    getClientID client clients = case IM.toList (IM.filter (\c -> c.name == client.name) clients) of
        ((x, _) : _) -> Just x
        _ -> Nothing

    sendClient client = writeTChan client.chan . Left

    broadcastSelection :: ClientID -> Maybe OT.Range -> [(ClientID, Client)] -> STM ()
    broadcastSelection cid mRange clients = do
        forM_ clients \(ocid, oclient) -> do
            when (ocid /= cid) do
                sendClient oclient $ NewSelection{cid, mRange}

    broadcastOperation :: Client -> OT.Revision -> OT.TextOperation -> [Client] -> STM ()
    broadcastOperation client rev op clients = do
        -- The requesting user needs to Ack
        sendClient client $ DoAck rev

        -- The others need to ApplyOp
        forM_ clients \oclient -> do
            when (oclient.name /= client.name) do
                sendClient oclient $ ApplyOp (rev, op)

-- | The client thread process server and remote client events, ont at a time.
clientThread :: Server -> Client -> IO Void
clientThread server client = go OT.initialClientState Nothing
  where
    go :: OTClient -> Maybe ClientQueue -> IO Void
    go state queue = do
        atomically (readTChan client.chan) >>= \case
            Left serverEvent -> do
                sayShow ("reply for " <> client.name, encode serverEvent)
                handleServerEvent state serverEvent queue
            Right clientEvent -> do
                sayShow ("<- " <> client.name, clientEvent)
                handleClientEvent state clientEvent queue

    handleServerEvent state ev queue = case ev of
        ApplyOp (rev, op) -> case queue of
            -- We are not waiting for
            Nothing -> case OT.applyServer state op of
                Left err -> do
                    sayShow ("applyServer failed: " <> err)
                    atomically $ writeTChan server.chan (client, DoReset err)
                    go OT.initialClientState Nothing
                Right (newOp, newState) -> do
                    sendJSON client $ ApplyOp (rev, newOp)
                    go newState queue
            Just (_rev, xs) -> go state (Just (rev, op : xs))
        -- Send the new document and reset the state
        Reset{mErr} -> do
            forM_ mErr \err -> sayShow ("Got error: " <> err)
            sendJSON client ev >> go OT.initialClientState Nothing
        -- Forward the other events directly
        _ -> sendJSON client ev >> go state queue

    handleClientEvent state ev queue = case ev of
        -- The remote client made a change
        ClientOp (rev, op) -> do
            case OT.applyClient state op of
                Left err -> do
                    sayShow ("applyClient failed: " <> err)
                    atomically $ writeTChan server.chan (client, DoReset err)
                    go OT.initialClientState queue
                Right (forServer, newState) -> do
                    when forServer do
                        atomically $ writeTChan server.chan (client, DoApply rev op)
                    go newState queue
        -- The remote client received the DoAck, acknowledge now.
        Ack rev -> do
            newState <- case OT.serverAck state of
                Nothing -> pure state
                Just (mOp, newState) -> do
                    forM_ mOp \sop ->
                        -- Forward extra events to the server
                        atomically $ writeTChan server.chan (client, DoApply rev sop)
                    pure newState
            case queue of
                Nothing -> go newState Nothing
                Just (nextRev, ops) -> case reverse ops of
                    [] -> go newState Nothing
                    (x : rest) -> do
                        case foldM OT.compose x rest of
                            Left err -> do
                                sayShow ("composed queue failed: " <> err)
                                atomically $ writeTChan server.chan (client, DoReset err)
                                go OT.initialClientState Nothing
                            Right op -> handleServerEvent newState (ApplyOp (nextRev, op)) Nothing
        SetSelection mRange -> do
            atomically $ writeTChan server.chan (client, DoSelection mRange)
            go state queue

removeCursor :: ClientID -> Cursors -> Cursors
removeCursor cid = filter (\(ocid, _) -> ocid /= cid)

addCursor :: ClientID -> OT.Range -> Cursors -> Cursors
addCursor cid range xs = (cid, range) : removeCursor cid xs

-- | Create a new 'Server'
newServer :: STM Server
newServer = Server <$> newTChan

getColor :: ClientID -> Text
getColor cid = colors !! (cid `mod` length colors)
  where
    colors = ["blue", "brown", "burlywood", "pink", "hotpink", "plum", "magenta", "slateblue"]

-- | Create a new 'Client' for 'withClient'
addClient :: Server -> SockAddr -> WS.Connection -> STM Client
addClient server remoteAddr connection = do
    let name = T.pack $ show remoteAddr
    client <- Client name connection <$> newTChan
    writeTChan server.chan (client, ClientConnected)
    pure client

-- | Delete a 'Client' by 'withClient'
removeClient :: Server -> Client -> STM ()
removeClient server client = do
    writeTChan server.chan (client, ClientLeft)
    writeTChan server.chan (client, DoSelection Nothing)

withClient :: Server -> SockAddr -> WS.Connection -> (Client -> IO a) -> IO a
withClient server addr conn =
    bracket
        (atomically $ addClient server addr conn)
        (atomically . removeClient server)

sendJSON :: (ToJSON a) => Client -> a -> IO ()
sendJSON client message = WS.sendTextData client.conn $ Data.Aeson.encode message

xfiles :: [XStatic.XStaticFile]
xfiles = XStatic.quillCursorsJs : XStatic.quill

indexHtml :: Html ()
indexHtml = do
    doctypehtml_ do
        head_ do
            title_ "quill-ot"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_ [rel_ "icon", href_ "data:;base64,="]
            xstaticScripts xfiles

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
  const webSocket = new WebSocket("ws://" + window.location.host + "/ws");
  webSocket.onopen = (event) => {
    console.log('Connected!')
  };
  webSocket.onerror = (event) => {
    console.error('Disconnected!')
  };

  Quill.register('modules/cursors', QuillCursors);
  const quill = new Quill('#editor', {
    theme: 'snow',
    modules: {
      cursors: true,
      toolbar: '#toolbar'
    },
    formats: []
  });
  const cursors = quill.getModule('cursors');
  const clients = {}

  document.getElementById("trig").onclick = () => triggerEdits(quill)

  // Keep track of the revision sent by the server
  let revision = 0
  webSocket.onmessage = (event) => {
    console.log("Got event:", event.data)
    const msg = JSON.parse(event.data)

    if ('cid' in msg) {
      // cursor event
      if ('name' in msg) {
        // new clients
        clients[msg.cid] = {name: msg.name, color: msg.color}
      } else if ('sel' in msg) {
        const client = clients[msg.cid];
        if (msg.sel && 'head' in msg.sel) {
          // new selection
          const cursor = cursors.createCursor(msg.cid, client.name, client.color)
          cursor.range = {
            index: msg.sel.anchor,
            length: msg.sel.head - msg.sel.anchor
          }
          cursors.update()
        } else {
          // deselection
          cursors.removeCursor(msg.cid)
        }
      } else {
        // client left
        delete clients[msg.cid]
        cursors.removeCursor(msg.cid)
      }
    } else if ('rev' in msg) {
      // sync event
      revision = msg.rev
      if ('body' in msg) {
        // Reset
        quill.setContents([{insert: msg.body + '\n'}], "api")
      } else {
        // Acknowledge round trip
        webSocket.send(JSON.stringify(revision))
        quill.enable()
        quill.focus()
      }
      if ('err' in msg) {
          console.error("Got an error: " + msg.err)
      }
    } else if (msg.length == 2) {
      // delta event
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
      quill.disable()
    }
  })

  quill.on('selection-change', (range, oldRange, source) => {
    console.log("selection-change", source, JSON.stringify(range), JSON.stringify(oldRange))
    // Here we should only do the update when the source === "user"
    if (range !== oldRange) {
      let msg = null
      if (range !== null) {
        const isStart = range.index == 0 && range.length == 0
        const isEnd = range.index + 1 == quill.getLength() && range.length == 0
        if (!isStart && !isEnd) {
          msg = {anchor: range.index, head: range.index + range.length}
        }
      }
      webSocket.send(JSON.stringify(msg))
    }
  })
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
        forever do
            buf <- WS.receiveData @LBS.ByteString client.conn
            case decode @ClientEvent buf of
                Just ev -> atomically $ writeTChan client.chan (Right ev)
                Nothing -> putStrLn $ "Invalid data: " <> show buf

instance FromJSON ClientEvent where
    parseJSON v = (ClientOp <$> parseJSON v) <|> (Ack <$> parseJSON v) <|> (SetSelection <$> parseJSON v)

instance ToJSON ServerEvent where
    toJSON = \case
        Reset{mErr, rev, body} -> toJSON $ object $ maybe id (\e -> ("err" .= e :)) mErr ["rev" .= rev, "body" .= body]
        ApplyOp tup -> toJSON tup
        DoAck rev -> toJSON $ object ["rev" .= rev]
        NewSelection{cid, mRange} -> toJSON $ object ["cid" .= cid, "sel" .= mRange]
        NewClient{cid, name, color} -> toJSON $ object ["cid" .= cid, "name" .= name, "color" .= color]
        DelClient cid -> toJSON $ object ["cid" .= cid]

-- | The demo entry point
main :: IO ()
main = do
    putStrLn "Running on localhost:8080"
    server <- atomically newServer
    race_ (serverThread server) do
        Warp.run 8080 (XStatic.xstaticMiddleware xfiles $ app server)
