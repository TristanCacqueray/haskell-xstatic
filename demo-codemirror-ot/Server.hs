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
import XStatic.OTJS qualified as XStatic
import XStatic.CodeMirror qualified as XStatic

import Control.OperationalTransformation.Selection qualified as OT
import Control.OperationalTransformation.Server qualified as OT
import Control.OperationalTransformation.Text qualified as OT

type OTServer = OT.ServerState Text OT.TextOperation
type Cursors = [(ClientID, OT.Range)]
type ClientID = Int

-- | Event from the server to the client
data ServerResponse
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
    = ClientReset {err :: String}
    | ClientOp (OT.Revision, OT.TextOperation)
    | ClientSelection (Maybe OT.Range)
    | ClientConnected
    | ClientLeft
    deriving stock (Show)

newtype Server = Server {chan :: TChan (Client, ServerRequest)}

data Client = Client {name :: Text, conn :: WS.Connection, chan :: TChan Value}

-- | The server thread process request one at a time
serverThread :: Server -> IO Void
serverThread server = go (OT.initialServerState mempty) [] mempty
  where
    go :: OTServer -> Cursors -> IM.IntMap Client -> IO Void
    go state cursors clients = do
        (client, ev) <- atomically $ readTChan server.chan
        case ev of
            ClientConnected -> do
                sayShow ("new client: " <> client.name)
                let cid = IM.size clients
                    color = getColor cid
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
                sayShow ("client left: " <> client.name)
                let newClients = IM.filter (\c -> c.name /= client.name) clients
                newCursors <- case getClientID client clients of
                    Nothing -> pure cursors
                    Just cid -> do
                        forM_ (IM.elems clients) \oclient -> do
                            sendClient oclient $ DelClient cid
                        pure $ removeCursor cid cursors
                go state newCursors newClients
            ClientSelection mRange -> case getClientID client clients of
                Nothing -> do
                    sayShow ("unknown client " <> client.name)
                    go state cursors clients
                Just clientID -> do
                    sayShow ("new selection " <> client.name, clientID, mRange)
                    broadcastSelection clientID mRange (IM.toList clients)
                    let newCursors = case mRange of
                            Nothing -> removeCursor clientID cursors
                            Just range -> addCursor clientID range cursors
                    go state newCursors clients
            ClientReset{err} -> resetClient client err
            ClientOp (rev, op) -> do
                -- sayShow ("Applying rev " <> client.name, rev)
                -- cursors update does not seems to work after 'applyOperation'
                -- so we rely on the client to send updated selection on changed event
                -- but that's not good, we need to update them server side to send the
                -- new positions right away. See the 'selection-change' handler below.
                case OT.applyOperation state rev op () of
                    Left err -> do
                        sayShow ("Fail to apply op " <> client.name, rev, op)
                        resetClient client err
                    Right (newOp, _newOTCursors, newState) -> do
                        let OT.ServerState newRev newDoc _op = newState
                        sayShow ("new state " <> client.name, newRev, newDoc)
                        broadcastOperation client newRev newOp (IM.elems clients)
                        go newState cursors clients
      where
        resetClient client err = do
            let OT.ServerState rev doc _ = state
            sendClient client $ Reset (Just err) rev doc
            go state cursors clients

    getClientID client clients = case IM.toList (IM.filter (\c -> c.name == client.name) clients) of
        ((x, _) : _) -> Just x
        _ -> Nothing

    sendClient client = atomically . writeTChan client.chan . toJSON

    broadcastSelection :: ClientID -> Maybe OT.Range -> [(ClientID, Client)] -> IO ()
    broadcastSelection cid mRange clients = do
        forM_ clients \(ocid, oclient) -> do
            when (ocid /= cid) do
                sendClient oclient $ NewSelection{cid, mRange}

    broadcastOperation :: Client -> OT.Revision -> OT.TextOperation -> [Client] -> IO ()
    broadcastOperation client rev op clients = do
        -- The requesting user needs to Ack
        sendClient client $ DoAck rev

        -- The others need to ApplyOp
        forM_ clients \oclient -> do
            when (oclient.name /= client.name) do
                sendClient oclient $ ApplyOp (rev, op)

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
removeClient server client = writeTChan server.chan (client, ClientLeft)

withClient :: Server -> SockAddr -> WS.Connection -> (Client -> IO a) -> IO a
withClient server addr conn =
    bracket
        (atomically $ addClient server addr conn)
        (atomically . removeClient server)

sendJSON :: (ToJSON a) => Client -> a -> IO ()
sendJSON client message = WS.sendTextData client.conn $ Data.Aeson.encode message

xfiles :: [XStatic.XStaticFile]
xfiles = XStatic.otJs : XStatic.codemirror

indexHtml :: Html ()
indexHtml = do
    doctypehtml_ do
        head_ do
            title_ "codemirror-ot"
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
            link_ [rel_ "icon", href_ "data:;base64,="]
            xstaticScripts xfiles

        body_ do
            style_ ".cm-editor { height: 100% }\n"
            with button_ [id_ "trig"] do
                "Simulate edits"
            with div_ [id_ "toolbar"] do
                pure ()
            with div_ [id_ "editor"] do
                pure ()
            script_ clientJS

-- TODO: move the adapter glue between ot.js and quill into a dedicated library
clientJS :: Text
clientJS =
    [s|

// CodeMirror uses the following delta encoding:
// - insert: [0, "text"]
// - retain: count
// - delete: [count]

// ot.hs use the following encoding:
// - "text"
// - count (for retain)
// - -count (for delete)

// Convert delta from CodeMirror to ot.hs
function encodeDelta(d) {
  if (d.length) {
    if (d.length == 1) {
      return d[0] * -1
    } else if (d.length >= 2) {
      const ins = d.slice(1).join("\n")
      if (d[0] == 0)
        return ins
      return [d[0] * -1, ...ins]
    } else {
      throw ("Unknown delta array:" + d)
    }
  } else {
    return d
  }
}

// Convert delta from ot.hs to Quill
function decodeDelta(d) {
  if (typeof d === 'string' || d instanceof String) {
    return [0, ...d.split("\n")]
  } else if (d > 0) {
    return d
  } else {
    return [d * -1]
  }
}

// Helpers to simulate edits
function triggerEdits(codeMirror) {
  function simulateEdits(codeMirror, edits) {
    if (edits.length > 0) {
      setTimeout(() => {
        if (typeof edits[0] === "string") {
          edits[0] = [[0, ...edits[0].split("\n")], codeMirror.viewState.state.doc.length]
        }
        const edit = CodeMirror.ChangeSet.fromJSON(edits[0]);
        let transaction = codeMirror.viewState.state.update({changes: edit})
        codeMirror.dispatch(transaction)
        simulateEdits(codeMirror, edits.slice(1))
      }, 100)
    }
  }

  function makeEdits() {
    const xs = [
      "hello ",
    ]
    for (i = 0; i < 20; i++) {
      if (i == 10) {
        xs.push("\n---8<----8<----8<----8<----\n")
      }
      xs.push("\n")
    }
    return xs
  }
  console.log("Starting simulation...")
  setTimeout(() => simulateEdits(codeMirror, makeEdits()), 10)
}

function setupClient() {
  const webSocket = new WebSocket("ws://" + window.location.host + "/ws");
  webSocket.onopen = (event) => {
    console.log('Connected!')
  };
  webSocket.onerror = (event) => {
    console.error('Disconnected!')
  };

  const otPlugin =  CodeMirror.ViewPlugin.fromClass(class {
    constructor(view) {}
    update(update) {
      if (update.docChanged) {
        update.transactions.forEach(tr => {
          if (tr.annotations.indexOf("remote") == -1) {
            const cmOps = tr.changes.toJSON()
            const otOps = tr.changes.toJSON().flatMap(encodeDelta)
            console.log("Got transaction: ", JSON.stringify(cmOps))
            console.log("Sending        : ", JSON.stringify(otOps))
            const op = ot.TextOperation.fromJSON(otOps)
            client.applyClient(op)
          }
        })
      }
    }
  });

  // Setup codemirror editor instance
  const editor = new CodeMirror.EditorView({
    extensions: [CodeMirror.basicSetup, CodeMirror.markdown(), otPlugin],
    parent: document.getElementById("editor"),
  })

  // Plug the trigger script
  document.getElementById("trig").onclick = () => triggerEdits(editor)

  // The ot.js local client
  let client
  const newClient = (rev) => {
    client = new ot.Client(rev)
    client.sendOperation = (rev, op) => {
      // console.log("client sending", rev, op)
      webSocket.send(JSON.stringify([rev, op]))
    }
    client.applyOperation = (op) => {
      // console.log("client apply", op)
      const edit = CodeMirror.ChangeSet.fromJSON(op.ops.map(decodeDelta));
      const transaction = editor.viewState.state.update({changes: edit, annotations: ["remote"]})
      editor.dispatch(transaction)
    }
  }

  // Handle server events
  webSocket.onmessage = (event) => {
    console.log("Got event:", event.data)
    const msg = JSON.parse(event.data)

    if ('cid' in msg) {
      // cursor event
    } else if ('rev' in msg) {
      // sync event
      if ('body' in msg) {
        // Reset local state
        newClient(msg.rev)
        const tr = {
          changes: {from: 0, to: editor.state.doc.length, insert: msg.body},
          annotations: ["remote"]
        }

        // Start with a new doc
        if (msg.body == "") {
          tr.changes.insert = "# Hello World!";
          tr.annotations = []
        }
        editor.dispatch(tr)
      } else {
        // Rev without body is an acknowledge request
        client.serverAck()
      }
      if ('err' in msg) {
        console.error("Got an error: " + msg.err)
      }
    } else if (msg.length == 2) {
      // delta event
      const [rev, delta] = msg
      client.applyServer(ot.TextOperation.fromJSON(delta))
    }
  };
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
    handleNewClient client = race_ writerThread do
        forever do
            buf <- WS.receiveData @LBS.ByteString client.conn
            case decode @ServerRequest buf of
                Just ev -> do
                    sayShow ("<-" :: Text, ev)
                    atomically $ writeTChan server.chan (client, ev)
                Nothing -> putStrLn $ "Invalid data: " <> show buf
      where
        writerThread = forever do
            resp <- atomically $ readTChan client.chan
            sayShow ("->" :: Text, encode resp)
            sendJSON client resp

instance FromJSON ServerRequest where
    parseJSON v =
        (ClientReset <$> parseJSON v) <|> (ClientOp <$> parseJSON v) <|> (ClientSelection <$> parseJSON v)

instance ToJSON ServerResponse where
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
