module Demo.IPC.Server
  ( -- * Server
    IPCServer
  , startServer
  , stopServer
  , broadcast
  , broadcastToType

    -- * Client Handling
  , ClientHandle (..)
  , getClients
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Exception (SomeException, catch, finally)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Demo.IPC.Protocol
  ( ClientType (..)
  , IPCMessage (..)
  , decodeMessage
  , encodeMessage
  )
import Network.Socket
  ( Family (AF_UNIX)
  , SockAddr (SockAddrUnix)
  , Socket
  , SocketType (Stream)
  , accept
  , bind
  , close
  , listen
  , socket
  )
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.IO.Error (catchIOError)

-- | Handle to a connected client
data ClientHandle = ClientHandle
  { chSocket :: Socket
  , chType :: ClientType
  , chThreadId :: ThreadId
  }

-- | IPC Server state
data IPCServer = IPCServer
  { serverSocket :: Socket
  , serverPath :: FilePath
  , serverClients :: TVar (Map Int ClientHandle)
  , serverNextId :: TVar Int
  , serverAcceptThread :: TVar (Maybe ThreadId)
  , serverOnMessage :: IPCMessage -> ClientHandle -> IO ()
  }

-- | Start the IPC server
startServer :: FilePath -> (IPCMessage -> ClientHandle -> IO ()) -> IO IPCServer
startServer path onMessage = do
  -- Remove existing socket file
  removeFile path `catchIOError` const (pure ())

  -- Create and bind socket
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix path)
  listen sock 5

  clientsVar <- newTVarIO Map.empty
  nextIdVar <- newTVarIO 0
  acceptThreadVar <- newTVarIO Nothing

  let server =
        IPCServer
          { serverSocket = sock
          , serverPath = path
          , serverClients = clientsVar
          , serverNextId = nextIdVar
          , serverAcceptThread = acceptThreadVar
          , serverOnMessage = onMessage
          }

  -- Start accept loop
  tid <- forkIO $ acceptLoop server
  atomically $ writeTVar acceptThreadVar (Just tid)

  pure server

-- | Stop the IPC server
stopServer :: IPCServer -> IO ()
stopServer server = do
  -- Stop accept thread
  mAcceptTid <- atomically $ do
    tid <- readTVar (serverAcceptThread server)
    writeTVar (serverAcceptThread server) Nothing
    pure tid
  mapM_ killThread mAcceptTid

  -- Close all client connections
  clients <- readTVarIO (serverClients server)
  mapM_ (close . chSocket) (Map.elems clients)

  -- Close server socket
  close (serverSocket server)

  -- Remove socket file
  removeFile (serverPath server) `catchIOError` const (pure ())

-- | Broadcast a message to all connected clients
broadcast :: IPCServer -> IPCMessage -> IO ()
broadcast server msg = do
  clients <- readTVarIO (serverClients server)
  mapM_ (sendToClient msg) (Map.elems clients)

-- | Broadcast to clients of a specific type
broadcastToType :: IPCServer -> ClientType -> IPCMessage -> IO ()
broadcastToType server clientType msg = do
  clients <- readTVarIO (serverClients server)
  let filtered = Map.filter ((== clientType) . chType) clients
  mapM_ (sendToClient msg) (Map.elems filtered)

-- | Get all connected clients
getClients :: IPCServer -> IO [ClientHandle]
getClients server = Map.elems <$> readTVarIO (serverClients server)

-- | Accept loop for incoming connections
acceptLoop :: IPCServer -> IO ()
acceptLoop server = forever $ do
  (clientSock, _) <- accept (serverSocket server)
  void $ forkIO $ handleClient server clientSock

-- | Handle a single client connection
handleClient :: IPCServer -> Socket -> IO ()
handleClient server sock = do
  clientId <- atomically $ do
    cid <- readTVar (serverNextId server)
    modifyTVar' (serverNextId server) (+ 1)
    pure cid

  -- Wait for registration message
  mClientType <- waitForRegistration sock

  case mClientType of
    Nothing -> close sock
    Just clientType -> do
      -- Create a placeholder thread id (will be updated)
      currentTid <- forkIO (pure ())

      let handle = ClientHandle sock clientType currentTid

      -- Send registration acknowledgment
      sendToClient MsgRegistered handle

      -- Register client
      atomically $
        modifyTVar' (serverClients server) (Map.insert clientId handle)

      -- Run client loop
      clientLoop server clientId sock
        `finally` do
          atomically $
            modifyTVar' (serverClients server) (Map.delete clientId)
          close sock

-- | Wait for client registration
waitForRegistration :: Socket -> IO (Maybe ClientType)
waitForRegistration sock = do
  result <- receiveMessageFromSocket sock BS.empty
  case result of
    Left _ -> pure Nothing
    Right (MsgRegister ct, _) -> pure (Just ct)
    Right _ -> pure Nothing

-- | Client message loop
clientLoop :: IPCServer -> Int -> Socket -> IO ()
clientLoop server clientId sock = go BS.empty
 where
  go buffer = do
    chunk <- recv sock 4096
    if BS.null chunk
      then pure () -- Client disconnected
      else do
        let newBuffer = buffer <> chunk
        case decodeMessage newBuffer of
          Left _ -> go newBuffer -- Need more data
          Right (msg, remaining) -> do
            -- Get client handle
            clients <- readTVarIO (serverClients server)
            case Map.lookup clientId clients of
              Just handle -> do
                serverOnMessage server msg handle
                  `catch` \(_ :: SomeException) -> pure ()
              Nothing -> pure ()
            go remaining

-- | Receive a complete message from socket
receiveMessageFromSocket :: Socket -> ByteString -> IO (Either String (IPCMessage, ByteString))
receiveMessageFromSocket sock buffer = do
  case decodeMessage buffer of
    Right result -> pure (Right result)
    Left _ -> do
      chunk <- recv sock 4096
      if BS.null chunk
        then pure (Left "Connection closed")
        else receiveMessageFromSocket sock (buffer <> chunk)

-- | Send a message to a client
sendToClient :: IPCMessage -> ClientHandle -> IO ()
sendToClient msg handle =
  sendAll (chSocket handle) (encodeMessage msg)
    `catch` \(_ :: SomeException) -> pure ()
