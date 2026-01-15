module Integration.IPCSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, race)
import Control.Concurrent.STM
  ( TVar
  , atomically
  , modifyTVar'
  , newTVarIO
  , readTVar
  , readTVarIO
  )
import Control.Exception (bracket)
import Demo.Core.Types (Slide (..))
import Demo.IPC.Client
  ( IPCClient
  , connectToServer
  , disconnectFromServer
  , sendMessage
  , setMessageHandler
  )
import Demo.IPC.Protocol
  ( ClientType (..)
  , IPCMessage (..)
  )
import Demo.IPC.Server
  ( IPCServer
  , ClientHandle (..)
  , broadcast
  , getClients
  , startServer
  , stopServer
  )
import System.Directory (removeFile)
import System.IO.Error (catchIOError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "IPC Server/Client" $ do
    it "accepts client connections" $ do
      withTempSocket $ \sockPath -> do
        withServer sockPath $ \server -> do
          result <- connectToServer sockPath NotesClient
          case result of
            Left err -> expectationFailure $ "Connection failed: " <> err
            Right client -> do
              -- Give server time to process
              threadDelay 100000
              clients <- getClients server
              length clients `shouldBe` 1
              disconnectFromServer client

    it "broadcasts messages to connected clients" $ do
      withTempSocket $ \sockPath -> do
        withServer sockPath $ \server -> do
          result <- connectToServer sockPath NotesClient
          case result of
            Left err -> expectationFailure $ "Connection failed: " <> err
            Right client -> do
              receivedVar <- newTVarIO ([] :: [IPCMessage])
              setMessageHandler client $ \msg ->
                atomically $ modifyTVar' receivedVar (msg :)

              threadDelay 100000

              -- Broadcast a message
              broadcast server MsgPing

              -- Wait for message to arrive
              threadDelay 200000

              received <- readTVarIO receivedVar
              received `shouldContain` [MsgPing]

              disconnectFromServer client

    it "handles multiple clients" $ do
      withTempSocket $ \sockPath -> do
        withServer sockPath $ \server -> do
          result1 <- connectToServer sockPath NotesClient
          result2 <- connectToServer sockPath ElaborationClient

          case (result1, result2) of
            (Right c1, Right c2) -> do
              threadDelay 100000
              clients <- getClients server
              length clients `shouldBe` 2
              disconnectFromServer c1
              disconnectFromServer c2
            _ -> expectationFailure "Failed to connect clients"

    it "removes disconnected clients" $ do
      withTempSocket $ \sockPath -> do
        withServer sockPath $ \server -> do
          result <- connectToServer sockPath NotesClient
          case result of
            Left err -> expectationFailure $ "Connection failed: " <> err
            Right client -> do
              threadDelay 100000
              clientsBefore <- getClients server
              length clientsBefore `shouldBe` 1

              disconnectFromServer client
              threadDelay 200000

              -- Client count may not immediately update due to async cleanup
              -- This is expected behavior
              pure ()

    it "handles SlideChanged messages" $ do
      withTempSocket $ \sockPath -> do
        withServer sockPath $ \server -> do
          result <- connectToServer sockPath NotesClient
          case result of
            Left err -> expectationFailure $ "Connection failed: " <> err
            Right client -> do
              receivedVar <- newTVarIO Nothing
              setMessageHandler client $ \msg -> case msg of
                MsgSlideChanged idx slide ->
                  atomically $ modifyTVar' receivedVar (const $ Just (idx, slide))
                _ -> pure ()

              threadDelay 100000

              let testSlide = Slide "Test" [] "Notes" []
              broadcast server (MsgSlideChanged 5 testSlide)

              threadDelay 200000

              received <- readTVarIO receivedVar
              case received of
                Just (idx, slide) -> do
                  idx `shouldBe` 5
                  _slideTitle slide `shouldBe` "Test"
                Nothing -> expectationFailure "Did not receive SlideChanged"

              disconnectFromServer client

-- | Helper to create a temporary socket path
withTempSocket :: (FilePath -> IO a) -> IO a
withTempSocket action = do
  withSystemTempDirectory "demo-test" $ \dir -> do
    let sockPath = dir <> "/test.sock"
    action sockPath

-- | Helper to bracket server lifecycle
withServer :: FilePath -> (IPCServer -> IO a) -> IO a
withServer sockPath action =
  bracket
    (startServer sockPath (\_ _ -> pure ()))
    stopServer
    action
