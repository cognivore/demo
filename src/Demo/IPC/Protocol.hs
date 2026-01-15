module Demo.IPC.Protocol
  ( -- * Messages
    IPCMessage (..)
  , ClientType (..)

    -- * Serialization
  , encodeMessage
  , decodeMessage

    -- * Socket Path
  , defaultSocketPath
  , socketPathForPresentation
  ) where

import Data.Aeson (FromJSON, ToJSON, Value, eitherDecode, encode)
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Word (Word32)
import Demo.Core.Types (Command, Direction, Elaboration, Slide)
import GHC.Generics (Generic)
import System.FilePath (takeBaseName)

-- | Type of client connecting to the server
data ClientType = NotesClient | ElaborationClient
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Messages exchanged between slides server and clients
data IPCMessage
  = -- | Client registration
    MsgRegister ClientType
  | -- | Server acknowledges registration
    MsgRegistered
  | -- | Slide has changed
    MsgSlideChanged Int Slide
  | -- | Command execution started
    MsgCommandStarted Command
  | -- | Output chunk from command
    MsgOutputChunk Text
  | -- | Command finished with exit code
    MsgCommandFinished Int
  | -- | Variable was updated
    MsgVarUpdated Int Value
  | -- | Client requests slide change
    MsgRequestSlide Direction
  | -- | Client requests specific elaboration
    MsgRequestElab Int
  | -- | Get current elaborations
    MsgElaborations [Elaboration]
  | -- | Heartbeat/ping
    MsgPing
  | -- | Heartbeat/pong
    MsgPong
  | -- | Error message
    MsgError Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Encode a message with length prefix for framing
encodeMessage :: IPCMessage -> ByteString
encodeMessage msg =
  let payload = BL.toStrict (encode msg)
      len = fromIntegral (BS.length payload) :: Word32
      lenBytes = word32ToBytes len
   in lenBytes <> payload

-- | Decode a length-prefixed message
decodeMessage :: ByteString -> Either String (IPCMessage, ByteString)
decodeMessage bs
  | BS.length bs < 4 = Left "Not enough data for length prefix"
  | otherwise =
      let (lenBytes, rest) = BS.splitAt 4 bs
          len = bytesToWord32 lenBytes
       in if BS.length rest < fromIntegral len
            then Left "Not enough data for message body"
            else
              let (payload, remaining) = BS.splitAt (fromIntegral len) rest
               in case eitherDecode (BL.fromStrict payload) of
                    Left err -> Left $ "JSON decode error: " <> err
                    Right msg -> Right (msg, remaining)

-- | Convert Word32 to 4 bytes (big-endian)
word32ToBytes :: Word32 -> ByteString
word32ToBytes w =
  BS.pack
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

-- | Convert 4 bytes to Word32 (big-endian)
bytesToWord32 :: ByteString -> Word32
bytesToWord32 bs =
  case BS.unpack (BS.take 4 bs) of
    [b0, b1, b2, b3] ->
      (fromIntegral b0 `shiftL` 24)
        + (fromIntegral b1 `shiftL` 16)
        + (fromIntegral b2 `shiftL` 8)
        + fromIntegral b3
    _ -> 0 -- Should not happen if length is checked

-- | Default socket path in /tmp
defaultSocketPath :: FilePath
defaultSocketPath = "/tmp/demo-slides.sock"

-- | Socket path for a specific presentation
socketPathForPresentation :: FilePath -> FilePath
socketPathForPresentation presPath =
  "/tmp/demo-" <> takeBaseName presPath <> ".sock"
