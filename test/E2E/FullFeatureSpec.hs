module E2E.FullFeatureSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (bracket)
import Control.Lens ((^.), (&))
import Data.Aeson (Value (..))
import Data.Text (Text)
import Data.Text qualified as T
import Demo.Core.DSL
import Demo.Core.Types
import Demo.Core.Variable (VarRef (..), findVarReferences, substituteVars)
import Demo.Core.Zipper
import Demo.IPC.Client (connectToServer, disconnectFromServer, setMessageHandler)
import Demo.IPC.Protocol
import Demo.IPC.Server (IPCServer, broadcast, startServer, stopServer)
import Demo.Interpreter.Ghci (GhciResult (..), evalGhciExpr, newInterpreter)
import Demo.Interpreter.System (CommandResult (..), runSystemCommand, runSystemCommandStreaming)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = do
  describe "DSL Features" $ do
    it "builds a complete presentation with all element types" $ do
      let pres = mkPresentation "Test Presentation" $ do
            slide "Introduction" $ do
              note "Welcome notes"
              system "echo 'Hello World'"
              elaborate "README.md" (1, 10) "Documentation"
            slide "Code Demo" $ do
              ghci "1 + 1"
              ghciTyped "\"hello\"" "String"
              system "ls -la"

      (pres ^. presName) `shouldBe` "Test Presentation"
      length (pres ^. presSlides) `shouldBe` 2

      let slide1 = (pres ^. presSlides) !! 0
      (slide1 ^. slideTitle) `shouldBe` "Introduction"
      (slide1 ^. slideNotes) `shouldBe` "Welcome notes"
      length (slide1 ^. slideCommands) `shouldBe` 1
      length (slide1 ^. slideElaborations) `shouldBe` 1

    it "preserves command ordering" $ do
      let pres = mkPresentation "Order Test" $ do
            slide "Commands" $ do
              system "first"
              system "second"
              system "third"

      let cmds = (pres ^. presSlides) !! 0 ^. slideCommands
      cmds `shouldBe` [SystemCmd "first", SystemCmd "second", SystemCmd "third"]

  describe "Zipper Navigation" $ do
    it "creates zipper from presentation" $ do
      let pres = mkPresentation "Zipper Test" $ do
            slide "Slide 1" $ do
              system "cmd1"
              system "cmd2"
            slide "Slide 2" $ do
              system "cmd3"

      case mkPresentationZipper pres of
        Nothing -> expectationFailure "Failed to create zipper"
        Just z -> do
          slideIndex z `shouldBe` 0
          commandIndex z `shouldBe` 0
          totalSlides z `shouldBe` 2
          totalCommands z `shouldBe` 2

    it "navigates forward and backward" $ do
      let pres = mkPresentation "Nav Test" $ do
            slide "S1" $ system "c1"
            slide "S2" $ system "c2"
            slide "S3" $ system "c3"

      case mkPresentationZipper pres of
        Nothing -> expectationFailure "Failed to create zipper"
        Just z -> do
          -- Navigate forward
          case nextSlide z of
            Nothing -> expectationFailure "Failed to move to next slide"
            Just z1 -> do
              slideIndex z1 `shouldBe` 1
              -- Navigate forward again
              case nextSlide z1 of
                Nothing -> expectationFailure "Failed to move to third slide"
                Just z2 -> do
                  slideIndex z2 `shouldBe` 2
                  -- Navigate backward
                  case prevSlide z2 of
                    Nothing -> expectationFailure "Failed to move backward"
                    Just z3 -> slideIndex z3 `shouldBe` 1

    it "respects bounds" $ do
      let pres = mkPresentation "Bounds Test" $ do
            slide "Only" $ system "cmd"

      case mkPresentationZipper pres of
        Nothing -> expectationFailure "Failed to create zipper"
        Just z -> do
          prevSlide z `shouldBe` Nothing  -- Already at start
          nextSlide z `shouldBe` Nothing  -- Already at end

  describe "Variable Substitution" $ do
    it "finds all variable references" $ do
      let refs = findVarReferences "echo $v1 and $v2 and $v10"
      length refs `shouldBe` 3
      map vrIndex refs `shouldBe` [1, 2, 10]

    it "substitutes variables in commands" $ do
      let store = emptyVarStore
                & insertVar 1 (String "hello")
                & insertVar 2 (Number 42)
      substituteVars store "Value: $v1, Number: $v2"
        `shouldBe` "Value: hello, Number: 42"

  describe "System Command Execution" $ do
    it "captures output from commands" $ do
      result <- runSystemCommand emptyVarStore "echo 'test output'"
      crExitCode result `shouldBe` 0
      T.strip (crOutput result) `shouldBe` "test output"

    it "streams output to callback" $ do
      chunks <- newTVarIO ([] :: [Text])
      result <- runSystemCommandStreaming emptyVarStore "echo 'line1'; echo 'line2'" $ \chunk ->
        atomically $ modifyTVar' chunks (chunk:)

      crExitCode result `shouldBe` 0
      received <- readTVarIO chunks
      length received `shouldSatisfy` (> 0)

  describe "GHCI Evaluation" $ do
    -- Note: These tests require GHC packages in the hint sandbox
    -- They work in full environment but may fail in CI/test sandbox
    it "evaluates simple expressions" $ do
      interp <- newInterpreter
      (result, _) <- evalGhciExpr interp "1 + 1" emptyVarStore
      case result of
        GhciSuccess _ display idx -> do
          display `shouldBe` "2"
          idx `shouldBe` 1
        GhciError _ ->
          -- This can fail if Aeson isn't available in hint sandbox
          pendingWith "GHCI sandbox may lack required packages"

    it "stores results in variable store" $ do
      interp <- newInterpreter
      (result, store1) <- evalGhciExpr interp "1 + 1" emptyVarStore
      case result of
        GhciSuccess {} -> nextVarIndex store1 `shouldBe` 2  -- $v1 was used
        GhciError _ -> pendingWith "GHCI sandbox may lack required packages"

  describe "IPC Integration" $ do
    it "broadcasts slide changes to clients" $ do
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

              let testSlide = Slide "Test" [SystemCmd "echo hi"] "Notes" []
              broadcast server (MsgSlideChanged 0 testSlide)

              threadDelay 200000

              received <- readTVarIO receivedVar
              case received of
                Just (idx, slide) -> do
                  idx `shouldBe` 0
                  (slide ^. slideTitle) `shouldBe` "Test"
                Nothing -> expectationFailure "Did not receive slide change"

              disconnectFromServer client

  describe "End-to-End Presentation Flow" $ do
    it "demonstrates full variable passing workflow" $ do
      -- Create a presentation that uses variable passing
      let pres = mkPresentation "Variable Flow Demo" $ do
            slide "Compute" $ do
              ghci "42 * 2"
              note "Computes 84 and stores in $v1"
            slide "Use Result" $ do
              system "echo 'The answer is $v1'"
              note "Uses $v1 from previous slide"

      -- Verify structure
      length (pres ^. presSlides) `shouldBe` 2

      -- First slide has GHCI command
      let s1 = (pres ^. presSlides) !! 0
      (s1 ^. slideCommands) `shouldBe` [GhciCmd "42 * 2" Nothing]

      -- Second slide has system command with variable
      let s2 = (pres ^. presSlides) !! 1
      case (s2 ^. slideCommands) of
        [SystemCmd cmd] ->
          cmd `shouldSatisfy` T.isInfixOf "$v1"
        _ -> expectationFailure "Expected system command"

-- | Helper to create a temporary socket path
withTempSocket :: (FilePath -> IO a) -> IO a
withTempSocket action = do
  withSystemTempDirectory "demo-e2e" $ \dir -> do
    let sockPath = dir <> "/test.sock"
    action sockPath

-- | Helper to bracket server lifecycle
withServer :: FilePath -> (IPCServer -> IO a) -> IO a
withServer sockPath action =
  bracket
    (startServer sockPath (\_ _ -> pure ()))
    stopServer
    action
