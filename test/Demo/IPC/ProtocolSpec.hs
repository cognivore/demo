module Demo.IPC.ProtocolSpec (spec) where

import Data.Aeson (Value (..))
import Data.ByteString qualified as BS
import Demo.Core.Types (Command (..), Direction (..), Elaboration (..), Slide (..))
import Demo.IPC.Protocol
  ( ClientType (..)
  , IPCMessage (..)
  , decodeMessage
  , encodeMessage
  )
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "message encoding/decoding" $ do
    it "roundtrips MsgRegister NotesClient" $
      roundtripMessage (MsgRegister NotesClient)

    it "roundtrips MsgRegister ElaborationClient" $
      roundtripMessage (MsgRegister ElaborationClient)

    it "roundtrips MsgRegistered" $
      roundtripMessage MsgRegistered

    it "roundtrips MsgSlideChanged" $
      let slide =
            Slide
              { _slideTitle = "Test Slide"
              , _slideCommands = [SystemCmd "echo hello"]
              , _slideNotes = "Some notes"
              , _slideElaborations = []
              }
       in roundtripMessage (MsgSlideChanged 0 slide)

    it "roundtrips MsgCommandStarted with SystemCmd" $
      roundtripMessage (MsgCommandStarted (SystemCmd "ls -la"))

    it "roundtrips MsgCommandStarted with GhciCmd" $
      roundtripMessage (MsgCommandStarted (GhciCmd "1 + 1" Nothing))

    it "roundtrips MsgOutputChunk" $
      roundtripMessage (MsgOutputChunk "Hello, World!\n")

    it "roundtrips MsgCommandFinished" $
      roundtripMessage (MsgCommandFinished 0)

    it "roundtrips MsgVarUpdated with string" $
      roundtripMessage (MsgVarUpdated 1 (String "test"))

    it "roundtrips MsgVarUpdated with number" $
      roundtripMessage (MsgVarUpdated 2 (Number 42))

    it "roundtrips MsgRequestSlide Prev" $
      roundtripMessage (MsgRequestSlide Prev)

    it "roundtrips MsgRequestSlide Next" $
      roundtripMessage (MsgRequestSlide Next)

    it "roundtrips MsgRequestElab" $
      roundtripMessage (MsgRequestElab 5)

    it "roundtrips MsgElaborations" $
      let elabs =
            [ Elaboration "file.hs" 1 10 "Caption 1"
            , Elaboration "other.hs" 20 30 "Caption 2"
            ]
       in roundtripMessage (MsgElaborations elabs)

    it "roundtrips MsgPing" $
      roundtripMessage MsgPing

    it "roundtrips MsgPong" $
      roundtripMessage MsgPong

    it "roundtrips MsgError" $
      roundtripMessage (MsgError "Something went wrong")

  describe "framing" $ do
    it "handles multiple messages in buffer" $ do
      let msg1 = MsgPing
          msg2 = MsgPong
          combined = encodeMessage msg1 <> encodeMessage msg2
      case decodeMessage combined of
        Left err -> expectationFailure $ "Failed to decode: " <> err
        Right (decoded1, rest) -> do
          decoded1 `shouldBe` msg1
          case decodeMessage rest of
            Left err -> expectationFailure $ "Failed to decode second: " <> err
            Right (decoded2, _) -> decoded2 `shouldBe` msg2

    it "fails gracefully on incomplete data" $ do
      let msg = MsgPing
          encoded = encodeMessage msg
          incomplete = BS.take (BS.length encoded - 1) encoded
      case decodeMessage incomplete of
        Left _ -> pure () -- Expected
        Right _ -> expectationFailure "Should have failed on incomplete data"

    it "fails gracefully on empty data" $ do
      case decodeMessage BS.empty of
        Left _ -> pure () -- Expected
        Right _ -> expectationFailure "Should have failed on empty data"

-- | Helper to test message roundtripping
roundtripMessage :: IPCMessage -> Expectation
roundtripMessage msg = do
  let encoded = encodeMessage msg
  case decodeMessage encoded of
    Left err -> expectationFailure $ "Decode failed: " <> err
    Right (decoded, remaining) -> do
      decoded `shouldBe` msg
      remaining `shouldBe` BS.empty
