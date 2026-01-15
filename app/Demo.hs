{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (SomeException, catch)
import Data.Bits (xor)
import Data.Char (ord)
import Demo.Loader (loadPresentation, formatLoadError, lpPresentationPath)
import Numeric (showHex)
import System.Directory (findExecutable, makeAbsolute)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess, readProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> runDemo path
    [path, "fin"] -> finishDemo path
    [path, "attach"] -> attachDemo path
    _ -> printUsage >> exitFailure

printUsage :: IO ()
printUsage = do
  hPutStrLn stderr "Usage: demo <presentation.hs> [command]"
  hPutStrLn stderr ""
  hPutStrLn stderr "Commands:"
  hPutStrLn stderr "  (none)     Start a new session (or attach if exists with same content)"
  hPutStrLn stderr "  attach     Attach to an existing session"
  hPutStrLn stderr "  fin        Terminate an existing session"
  hPutStrLn stderr ""
  hPutStrLn stderr "The tmux session name is derived from the file contents,"
  hPutStrLn stderr "preventing conflicts between different presentations."
  hPutStrLn stderr ""
  hPutStrLn stderr "Session layout:"
  hPutStrLn stderr "  - Main pane: slides"
  hPutStrLn stderr "  - Right pane: elaboration"
  hPutStrLn stderr "  - Second window: notes"

-- | Compute a simple hash of file contents for session naming
-- Uses a simple polynomial rolling hash
hashContents :: String -> String
hashContents s =
  let modulo = (2 :: Integer) ^ (32 :: Integer)
      hash = foldl (\acc c -> (acc * 31 + fromIntegral (ord c)) `mod` modulo) (0 :: Integer) s
  in take 8 $ showHex (hash `xor` (hash `div` 256)) ""

-- | Get session name for a presentation file
getSessionName :: FilePath -> IO String
getSessionName presPath = do
  content <- readFile presPath
  let hash = hashContents content
  pure $ "demo-" <> hash

-- | Check if a tmux session exists
sessionExists :: String -> IO Bool
sessionExists sessionName = do
  result <- tryReadProcess "tmux" ["has-session", "-t", sessionName] ""
  pure $ result == ""  -- tmux has-session returns empty on success, error on failure

-- | Run the demo presentation
runDemo :: FilePath -> IO ()
runDemo presPath = do
  -- Verify presentation loads and get absolute path
  result <- loadPresentation presPath
  absPath <- case result of
    Left err -> do
      hPutStrLn stderr $ "Error loading presentation: " <> formatLoadError err
      exitFailure
    Right lp -> pure $ lpPresentationPath lp

  -- Check if tmux is available
  checkTmux

  -- Compute content-addressed session name
  sessionName <- getSessionName absPath

  -- Check if session already exists
  exists <- sessionExists sessionName
  if exists
    then do
      hPutStrLn stderr $ "Session '" <> sessionName <> "' already exists."
      hPutStrLn stderr ""
      hPutStrLn stderr "Options:"
      hPutStrLn stderr $ "  • Attach:    demo " <> presPath <> " attach"
      hPutStrLn stderr $ "  • Terminate: demo " <> presPath <> " fin"
      hPutStrLn stderr ""
      hPutStrLn stderr "Attaching to existing session..."
      attachToSession sessionName
    else do
      createSession sessionName absPath
      attachToSession sessionName

  exitSuccess

-- | Finish (terminate) a demo session
finishDemo :: FilePath -> IO ()
finishDemo presPath = do
  absPath <- makeAbsolute presPath
  sessionName <- getSessionName absPath

  exists <- sessionExists sessionName
  if exists
    then do
      _ <- tryReadProcess "tmux" ["kill-session", "-t", sessionName] ""
      hPutStrLn stderr $ "Session '" <> sessionName <> "' terminated."
      exitSuccess
    else do
      hPutStrLn stderr $ "No session found for this presentation."
      hPutStrLn stderr $ "Session name would be: " <> sessionName
      exitFailure

-- | Attach to an existing demo session
attachDemo :: FilePath -> IO ()
attachDemo presPath = do
  absPath <- makeAbsolute presPath
  sessionName <- getSessionName absPath

  checkTmux

  exists <- sessionExists sessionName
  if exists
    then do
      attachToSession sessionName
      exitSuccess
    else do
      hPutStrLn stderr $ "No session found for this presentation."
      hPutStrLn stderr $ "Session name: " <> sessionName
      hPutStrLn stderr ""
      hPutStrLn stderr $ "Run 'demo " <> presPath <> "' to start a new session."
      exitFailure

-- | Check if tmux is available
checkTmux :: IO ()
checkTmux = do
  mTmux <- findExecutable "tmux"
  case mTmux of
    Nothing -> do
      hPutStrLn stderr "Error: tmux not found. Please install tmux."
      exitFailure
    Just _ -> pure ()

-- | Attach to a tmux session
attachToSession :: String -> IO ()
attachToSession sessionName = do
  -- First select the slides window to ensure we start there
  _ <- tryReadProcess "tmux" ["select-window", "-t", sessionName <> ":slides"] ""
  callProcess "tmux" ["attach-session", "-t", sessionName]

-- | Create a new tmux session for the presentation
createSession :: String -> FilePath -> IO ()
createSession sessionName absPath = do
  -- Get paths to our executables
  exePath <- getExecutablePath
  let binDir = takeDirectory exePath
      slidesPath = binDir </> "slides"
      notesPath = binDir </> "notes"
      elabPath = binDir </> "elaboration"

  -- Create new tmux session with slides
  callProcess
    "tmux"
    [ "new-session"
    , "-d"
    , "-s"
    , sessionName
    , "-n"
    , "slides"
    , slidesPath
    , absPath
    ]

  -- Split horizontally for elaboration
  callProcess
    "tmux"
    [ "split-window"
    , "-t"
    , sessionName <> ":slides"
    , "-h"
    , "-p"
    , "40"
    , elabPath
    , absPath
    ]

  -- Create second window for notes
  callProcess
    "tmux"
    [ "new-window"
    , "-t"
    , sessionName
    , "-n"
    , "notes"
    , notesPath
    , absPath
    ]

  -- Select the slides window
  _ <- tryReadProcess "tmux" ["select-window", "-t", sessionName <> ":slides"] ""

  hPutStrLn stderr $ "Created session: " <> sessionName

-- | Try to run readProcess, returning empty string on success, non-empty on failure
tryReadProcess :: FilePath -> [String] -> String -> IO String
tryReadProcess cmd cmdArgs input =
  (readProcess cmd cmdArgs input >> pure "")
    `catch` \(_ :: SomeException) -> pure "error"
