module Demo.IPC.Client
  ( -- * Client
    IPCClient
  , connectToServer
  , disconnectFromServer

    -- * Communication
  , sendMessage
  , receiveMessage
  , setMessageHandler
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Demo.IPC.Protocol
  ( ClientType
  , IPCMessage (..)
  , decodeMessage
  , encodeMessage
  )
import Network.Socket
  ( Family (AF_UNIX)
  , SockAddr (SockAddrUnix)
  , Socket
  , SocketType (Stream)
  , close
  , connect
  , socket
  )
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString qualified as BS

-- | IPC Client state
data IPCClient = IPCClient
  { clientSocket :: Socket
  , clientHandler :: TVar (IPCMessage -> IO ())
  , clientRecvThread :: TVar (Maybe ThreadId)
  , clientConnected :: TVar Bool
  }

-- | Connect to the slides server
connectToServer :: FilePath -> ClientType -> IO (Either String IPCClient)
connectToServer path clientType = do
  result <- tryConnect path
  case result of
    Left err -> pure (Left err)
    Right sock -> do
      handlerVar <- newTVarIO (\_ -> pure ())
      recvThreadVar <- newTVarIO Nothing
      connectedVar <- newTVarIO True

      let client =
            IPCClient
              { clientSocket = sock
              , clientHandler = handlerVar
              , clientRecvThread = recvThreadVar
              , clientConnected = connectedVar
              }

      -- Send registration
      sendAll sock (encodeMessage (MsgRegister clientType))

      -- Wait for acknowledgment
      ackResult <- waitForAck sock
      case ackResult of
        Left err -> do
          close sock
          pure (Left err)
        Right _ -> do
          -- Start receive loop
          tid <- forkIO $ receiveLoop client
          atomically $ writeTVar recvThreadVar (Just tid)
          pure (Right client)
 where
  tryConnect :: FilePath -> IO (Either String Socket)
  tryConnect p = do
    sock <- socket AF_UNIX Stream 0
    (connect sock (SockAddrUnix p) >> pure (Right sock))
      `catch` \(e :: SomeException) -> do
        close sock
        pure (Left $ "Failed to connect: " <> show e)

  waitForAck :: Socket -> IO (Either String ())
  waitForAck sock = do
    chunk <- recv sock 4096
    if BS.null chunk
      then pure (Left "Connection closed before ack")
      else case decodeMessage chunk of
        Right (MsgRegistered, _) -> pure (Right ())
        Right (MsgError err, _) -> pure (Left $ "Server error: " <> show err)
        Right _ -> pure (Left "Unexpected message instead of ack")
        Left err -> pure (Left $ "Decode error: " <> err)

-- | Disconnect from the server
disconnectFromServer :: IPCClient -> IO ()
disconnectFromServer client = do
  atomically $ writeTVar (clientConnected client) False

  -- Stop receive thread
  mRecvTid <- atomically $ do
    tid <- readTVar (clientRecvThread client)
    writeTVar (clientRecvThread client) Nothing
    pure tid
  mapM_ killThread mRecvTid

  -- Close socket
  close (clientSocket client)

-- | Send a message to the server
sendMessage :: IPCClient -> IPCMessage -> IO ()
sendMessage client msg = do
  connected <- readTVarIO (clientConnected client)
  when connected $
    sendAll (clientSocket client) (encodeMessage msg)
      `catch` \(_ :: SomeException) -> pure ()

-- | Receive a single message (blocking)
receiveMessage :: IPCClient -> IO (Either String IPCMessage)
receiveMessage client = go BS.empty
 where
  go buffer = do
    connected <- readTVarIO (clientConnected client)
    if not connected
      then pure (Left "Not connected")
      else do
        chunk <- recv (clientSocket client) 4096
        if BS.null chunk
          then pure (Left "Connection closed")
          else case decodeMessage (buffer <> chunk) of
            Right (msg, _) -> pure (Right msg)
            Left _ -> go (buffer <> chunk)

-- | Set the message handler for incoming messages
setMessageHandler :: IPCClient -> (IPCMessage -> IO ()) -> IO ()
setMessageHandler client handler =
  atomically $ writeTVar (clientHandler client) handler

-- | Receive loop for incoming messages
receiveLoop :: IPCClient -> IO ()
receiveLoop client = go BS.empty `catch` cleanup
 where
  go buffer = do
    connected <- readTVarIO (clientConnected client)
    when connected $ do
      chunk <- recv (clientSocket client) 4096
      if BS.null chunk
        then pure () -- Connection closed
        else processMessages (buffer <> chunk)

  processMessages buffer =
    case decodeMessage buffer of
      Left _ -> go buffer -- Need more data
      Right (msg, remaining) -> do
        handler <- readTVarIO (clientHandler client)
        handler msg `catch` \(_ :: SomeException) -> pure ()
        processMessages remaining

  cleanup :: SomeException -> IO ()
  cleanup _ = atomically $ writeTVar (clientConnected client) False
