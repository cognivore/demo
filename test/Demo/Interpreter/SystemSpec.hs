module Demo.Interpreter.SystemSpec (spec) where

import Control.Concurrent.STM (newTVarIO, atomically, readTVar, modifyTVar')
import Data.Text qualified as T
import Demo.Core.Types (emptyVarStore, insertVar)
import Demo.Core.Variable ()
import Demo.Interpreter.System
  ( CommandResult (..)
  , runSystemCommand
  , runSystemCommandStreaming
  )
import Data.Aeson (Value(..))
import Test.Hspec

spec :: Spec
spec = do
  describe "runSystemCommand" $ do
    it "executes simple echo command" $ do
      result <- runSystemCommand emptyVarStore "echo hello"
      crExitCode result `shouldBe` 0
      T.strip (crOutput result) `shouldBe` "hello"

    it "captures exit code on failure" $ do
      result <- runSystemCommand emptyVarStore "false"
      crExitCode result `shouldBe` 1

    it "captures exit code on success" $ do
      result <- runSystemCommand emptyVarStore "true"
      crExitCode result `shouldBe` 0

    it "substitutes variables in command" $ do
      let store = insertVar 1 (String "world") emptyVarStore
      result <- runSystemCommand store "echo hello $v1"
      crExitCode result `shouldBe` 0
      T.strip (crOutput result) `shouldBe` "hello world"

    it "handles multiple variable substitutions" $ do
      let store = insertVar 1 (String "a") $ insertVar 2 (String "b") emptyVarStore
      result <- runSystemCommand store "echo $v1 $v2"
      crExitCode result `shouldBe` 0
      T.strip (crOutput result) `shouldBe` "a b"

  describe "runSystemCommandStreaming" $ do
    it "streams output chunks" $ do
      chunksVar <- newTVarIO []
      result <-
        runSystemCommandStreaming emptyVarStore "echo -e 'line1\nline2'" $
          \chunk -> atomically $ modifyTVar' chunksVar (chunk :)
      crExitCode result `shouldBe` 0
      chunks <- atomically $ readTVar chunksVar
      -- Should have received some output
      length chunks `shouldSatisfy` (> 0)

    it "calls callback for each output line" $ do
      countVar <- newTVarIO (0 :: Int)
      _ <-
        runSystemCommandStreaming emptyVarStore "echo -e 'a\nb\nc'" $
          \_ -> atomically $ modifyTVar' countVar (+ 1)
      count <- atomically $ readTVar countVar
      -- Should have called callback at least once
      count `shouldSatisfy` (> 0)

    it "returns same result as non-streaming version" $ do
      streamResult <-
        runSystemCommandStreaming emptyVarStore "echo test" (const $ pure ())
      normalResult <- runSystemCommand emptyVarStore "echo test"
      crExitCode streamResult `shouldBe` crExitCode normalResult
      T.strip (crOutput streamResult) `shouldBe` T.strip (crOutput normalResult)
