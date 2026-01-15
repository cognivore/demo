module Demo.Interpreter.System
  ( -- * Command Execution
    runSystemCommand
  , runSystemCommandStreaming

    -- * Results
  , CommandResult (..)
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, catch)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Demo.Core.Types (VarStore)
import Demo.Core.Variable (substituteVars)
import System.Exit (ExitCode (..))
import System.IO (hClose, hGetLine, hIsEOF)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , shell
  , waitForProcess
  )

-- | Result of a command execution
data CommandResult = CommandResult
  { crExitCode :: Int
  , crOutput :: Text
  }
  deriving stock (Show, Eq)

-- | Run a system command and return the complete result
runSystemCommand :: VarStore -> Text -> IO CommandResult
runSystemCommand store cmdText = do
  let cmd = substituteVars store cmdText
  (_, Just hout, Just herr, ph) <-
    createProcess
      (shell (T.unpack cmd))
        { std_out = CreatePipe
        , std_err = CreatePipe
        }

  -- Read stdout and stderr concurrently
  outAsync <- async $ readAllText hout
  errAsync <- async $ readAllText herr

  output <- wait outAsync
  errOutput <- wait errAsync

  exitCode <- waitForProcess ph

  let combinedOutput =
        if T.null errOutput
          then output
          else output <> "\n" <> errOutput

  pure $
    CommandResult
      { crExitCode = exitCodeToInt exitCode
      , crOutput = combinedOutput
      }
 where
  readAllText h = go []
   where
    go acc = do
      eof <- hIsEOF h
      if eof
        then do
          hClose h
          pure $ T.unlines (reverse acc)
        else do
          line <- TIO.hGetLine h
          go (line : acc)

-- | Run a system command with streaming output
runSystemCommandStreaming ::
  VarStore ->
  Text ->
  (Text -> IO ()) -> -- Output callback
  IO CommandResult
runSystemCommandStreaming store cmdText onOutput = do
  let cmd = substituteVars store cmdText
  (_, Just hout, Just herr, ph) <-
    createProcess
      (shell (T.unpack cmd))
        { std_out = CreatePipe
        , std_err = CreatePipe
        }

  -- Stream stdout
  outAsync <- async $ streamOutput hout
  -- Stream stderr
  errAsync <- async $ streamOutput herr

  output <- wait outAsync
  errOutput <- wait errAsync

  exitCode <- waitForProcess ph

  let combinedOutput =
        if T.null errOutput
          then output
          else output <> errOutput

  pure $
    CommandResult
      { crExitCode = exitCodeToInt exitCode
      , crOutput = combinedOutput
      }
 where
  streamOutput h = go []
   where
    go acc = do
      eof <- hIsEOF h
      if eof
        then do
          hClose h
          pure $ T.concat (reverse acc)
        else do
          line <-
            (T.pack <$> hGetLine h)
              `catch` \(_ :: SomeException) -> pure ""
          let lineWithNl = line <> "\n"
          onOutput lineWithNl
          go (lineWithNl : acc)

-- | Convert ExitCode to Int
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n
