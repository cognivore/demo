{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (SomeException, catch)
import Control.Monad (filterM, when)
import Data.Bits (xor)
import Data.Char (ord)
import Demo.Loader (loadPresentation, formatLoadError, lpPresentationPath)
import Numeric (showHex)
import System.Directory (findExecutable, makeAbsolute, doesFileExist)
import System.Environment (getArgs, getExecutablePath, lookupEnv)
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
    [path, "notes"] -> attachNotes path
    _ -> printUsage >> exitFailure

printUsage :: IO ()
printUsage = do
  hPutStrLn stderr "Usage: demo <presentation.hs> [command]"
  hPutStrLn stderr ""
  hPutStrLn stderr "Commands:"
  hPutStrLn stderr "  (none)     Start a new session (or attach if exists with same content)"
  hPutStrLn stderr "  attach     Attach to the main slides session"
  hPutStrLn stderr "  notes      Attach to the speaker notes session (separate screen)"
  hPutStrLn stderr "  fin        Terminate all sessions for this presentation"
  hPutStrLn stderr ""
  hPutStrLn stderr "The tmux session name is derived from the file contents,"
  hPutStrLn stderr "preventing conflicts between different presentations."
  hPutStrLn stderr ""
  hPutStrLn stderr "Session layout:"
  hPutStrLn stderr "  - Main session: slides + elaboration side-by-side"
  hPutStrLn stderr "  - Notes session: speaker notes (separate tmux session)"

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

-- | Get the notes session name (separate session for speaker)
getNotesSessionName :: FilePath -> IO String
getNotesSessionName presPath = (<> "-notes") <$> getSessionName presPath

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

  -- Compute content-addressed session names
  sessionName <- getSessionName absPath
  notesSession <- getNotesSessionName absPath

  -- Check if session already exists
  exists <- sessionExists sessionName
  if exists
    then do
      hPutStrLn stderr $ "Session '" <> sessionName <> "' already exists."
      hPutStrLn stderr ""
      hPutStrLn stderr "Options:"
      hPutStrLn stderr $ "  • Attach slides: demo " <> presPath <> " attach"
      hPutStrLn stderr $ "  • Attach notes:  demo " <> presPath <> " notes"
      hPutStrLn stderr $ "  • Terminate:     demo " <> presPath <> " fin"
      hPutStrLn stderr ""
      hPutStrLn stderr "Attaching to existing slides session..."
      attachToSession sessionName
    else do
      createSession sessionName notesSession absPath
      hPutStrLn stderr ""
      hPutStrLn stderr $ "Speaker notes available in separate session:"
      hPutStrLn stderr $ "  demo " <> presPath <> " notes"
      hPutStrLn stderr ""
      attachToSession sessionName

  exitSuccess

-- | Finish (terminate) all demo sessions
finishDemo :: FilePath -> IO ()
finishDemo presPath = do
  absPath <- makeAbsolute presPath
  sessionName <- getSessionName absPath
  notesSession <- getNotesSessionName absPath

  mainExists <- sessionExists sessionName
  notesExists <- sessionExists notesSession

  if mainExists || notesExists
    then do
      when mainExists $ do
        _ <- tryReadProcess "tmux" ["kill-session", "-t", sessionName] ""
        hPutStrLn stderr $ "Session '" <> sessionName <> "' terminated."
      when notesExists $ do
        _ <- tryReadProcess "tmux" ["kill-session", "-t", notesSession] ""
        hPutStrLn stderr $ "Session '" <> notesSession <> "' terminated."
      exitSuccess
    else do
      hPutStrLn stderr $ "No sessions found for this presentation."
      hPutStrLn stderr $ "Session names would be: " <> sessionName <> ", " <> notesSession
      exitFailure

-- | Attach to an existing demo session (slides)
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

-- | Attach to the notes session (separate screen for speaker)
attachNotes :: FilePath -> IO ()
attachNotes presPath = do
  absPath <- makeAbsolute presPath
  notesSession <- getNotesSessionName absPath

  checkTmux

  exists <- sessionExists notesSession
  if exists
    then do
      callProcess "tmux" ["attach-session", "-t", notesSession]
      exitSuccess
    else do
      hPutStrLn stderr $ "No notes session found for this presentation."
      hPutStrLn stderr $ "Session name: " <> notesSession
      hPutStrLn stderr ""
      hPutStrLn stderr $ "Run 'demo " <> presPath <> "' to start a new session first."
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

-- | Create new tmux sessions for the presentation
-- Creates two separate sessions:
--   1. Main session with slides + elaboration
--   2. Notes session for speaker (can be on separate screen)
createSession :: String -> String -> FilePath -> IO ()
createSession sessionName notesSession absPath = do
  -- Get paths to our executables
  -- First try the same directory as demo (works when installed)
  -- Then fallback to findExecutable (works with cabal run)
  exePath <- getExecutablePath
  let binDir = takeDirectory exePath

  slidesPath <- findExeOrFallback "slides" (binDir </> "slides")
  notesPath <- findExeOrFallback "notes" (binDir </> "notes")
  elabPath <- findExeOrFallback "elaboration" (binDir </> "elaboration")

  -- Get DEMO_SRC_PATH to pass to subprocesses (tmux doesn't inherit env vars)
  mDemoSrc <- lookupEnv "DEMO_SRC_PATH"

  -- Build command with env wrapper if DEMO_SRC_PATH is set
  -- This ensures slides/notes/elaboration can find Demo.Core modules
  let withEnv cmd args = case mDemoSrc of
        Just srcPath -> ["env", "DEMO_SRC_PATH=" <> srcPath, cmd] <> args
        Nothing -> [cmd] <> args

  -- Create main tmux session with slides
  callProcess
    "tmux"
    ( [ "new-session"
      , "-d"
      , "-s"
      , sessionName
      , "-n"
      , "slides"
      ]
      <> withEnv slidesPath [absPath]
    )

  -- Split horizontally for elaboration
  callProcess
    "tmux"
    ( [ "split-window"
      , "-t"
      , sessionName <> ":slides"
      , "-h"
      , "-p"
      , "40"
      ]
      <> withEnv elabPath [absPath]
    )

  -- Select the slides pane as active
  _ <- tryReadProcess "tmux" ["select-pane", "-t", sessionName <> ":slides.0"] ""

  hPutStrLn stderr $ "Created slides session: " <> sessionName

  -- Create SEPARATE tmux session for notes (speaker's screen)
  callProcess
    "tmux"
    ( [ "new-session"
      , "-d"
      , "-s"
      , notesSession
      , "-n"
      , "notes"
      ]
      <> withEnv notesPath [absPath]
    )

  hPutStrLn stderr $ "Created notes session: " <> notesSession

-- | Try to run readProcess, returning empty string on success, non-empty on failure
tryReadProcess :: FilePath -> [String] -> String -> IO String
tryReadProcess cmd cmdArgs input =
  (readProcess cmd cmdArgs input >> pure "")
    `catch` \(_ :: SomeException) -> pure "error"

-- | Find an executable: check multiple locations for cabal builds and installed
findExeOrFallback :: String -> FilePath -> IO FilePath
findExeOrFallback name fallbackPath = do
  -- When running via cabal, executables are in separate directories
  -- e.g., .../x/slides/build/slides/slides instead of .../x/demo/build/demo/slides
  exePath <- getExecutablePath
  let cabalBuildPath = constructCabalPath exePath name

  -- Try in order: 1) same dir as demo, 2) cabal build path, 3) PATH
  candidates <- filterM doesFileExist [fallbackPath, cabalBuildPath]
  case candidates of
    (found:_) -> pure found
    [] -> do
      mExe <- findExecutable name
      case mExe of
        Just exe -> pure exe
        Nothing -> do
          hPutStrLn stderr $ "Error: Could not find executable '" <> name <> "'"
          hPutStrLn stderr $ "Checked: " <> fallbackPath
          hPutStrLn stderr $ "Checked: " <> cabalBuildPath
          hPutStrLn stderr "Make sure demo is properly installed or run via 'cabal run'"
          exitFailure

-- | Construct cabal build path for a sibling executable
-- Given: .../x/demo/build/demo/demo
-- Returns: .../x/{name}/build/{name}/{name}
constructCabalPath :: FilePath -> String -> FilePath
constructCabalPath demoExePath name =
  let dir = takeDirectory demoExePath  -- .../x/demo/build/demo
      parentDir = takeDirectory dir     -- .../x/demo/build
      grandParent = takeDirectory parentDir  -- .../x/demo
      xDir = takeDirectory grandParent  -- .../x
  in xDir </> name </> "build" </> name </> name
