{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (SomeException, catch, displayException, try)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Demo.Loader (loadPresentation, formatLoadError, lpPresentation, lpProjectRoot, lpPresentationPath)
import Demo.UI.Slides (SlidesConfig (..), runSlidesUI)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.IO (hPutStrLn, stderr, hFlush)
import System.Process (readProcessWithExitCode)

-- | Log file for persistent diagnostics (PERMANENT - do not remove after debugging)
logFile :: FilePath
logFile = "/Users/sweater/Github/demo/.cursor/slides.log"

-- | Get version info at runtime (includes git state)
getVersionInfo :: IO String
getVersionInfo = do
  -- Get commit hash
  (exitCode1, hash, _) <- readProcessWithExitCode "git" ["-C", "/Users/sweater/Github/demo", "rev-parse", "--short", "HEAD"] ""
  let commitHash = if exitCode1 == ExitSuccess then trim hash else "unknown"
  
  -- Check if dirty
  (exitCode2, status, _) <- readProcessWithExitCode "git" ["-C", "/Users/sweater/Github/demo", "status", "--porcelain"] ""
  let isDirty = exitCode2 == ExitSuccess && not (null (trim status))
  
  -- Get diff hash if dirty
  dirtyInfo <- if isDirty
    then do
      (exitCode3, diff, _) <- readProcessWithExitCode "git" ["-C", "/Users/sweater/Github/demo", "diff", "HEAD"] ""
      let diffHash = if exitCode3 == ExitSuccess then take 8 $ show $ abs $ simpleHash diff else "nodiff"
      pure $ "-dirty-" <> diffHash
    else pure ""
  
  pure $ "slides-" <> commitHash <> dirtyInfo
 where
  trim = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse
  simpleHash s = foldl (\h c -> 31 * h + fromEnum c) 0 s :: Int

-- | Write a timestamped log entry (PERMANENT - do not remove after debugging)
logEntry :: String -> String -> IO ()
logEntry component msg = do
  now <- getCurrentTime
  version <- getVersionInfo
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      entry = timestamp <> " [" <> version <> "] " <> component <> ": " <> msg <> "\n"
  _ <- try (appendFile logFile entry) :: IO (Either SomeException ())
  pure ()

-- | Log an error with full details (PERMANENT - do not remove after debugging)
logError :: String -> String -> IO ()
logError component msg = do
  logEntry component $ "ERROR: " <> msg
  -- Also write to stderr for tmux capture
  version <- getVersionInfo
  hPutStrLn stderr $ "[" <> version <> "] " <> component <> " ERROR: " <> msg
  hFlush stderr

main :: IO ()
main = do
  logEntry "main" "=== SLIDES STARTING ==="
  
  -- Log environment for diagnostics
  mDemoSrc <- lookupEnv "DEMO_SRC_PATH"
  logEntry "main" $ "DEMO_SRC_PATH=" <> show mDemoSrc
  
  args <- getArgs
  logEntry "main" $ "Arguments: " <> show args
  
  case args of
    [presPath] -> runSlides presPath `catch` \(e :: SomeException) -> do
      logError "main" $ "UNCAUGHT EXCEPTION: " <> displayException e
      exitFailure
    _ -> do
      logError "main" "Invalid arguments - expected: slides <presentation.hs>"
      exitFailure

runSlides :: FilePath -> IO ()
runSlides presPath = do
  logEntry "runSlides" $ "Loading presentation: " <> presPath
  
  result <- loadPresentation presPath
  case result of
    Left err -> do
      logError "runSlides" $ "LOAD FAILED: " <> formatLoadError err
      exitFailure
    Right loadedPres -> do
      logEntry "runSlides" "Presentation loaded successfully"
      let config =
            SlidesConfig
              { scPresPath = lpPresentationPath loadedPres
              , scPresentation = lpPresentation loadedPres
              , scProjectRoot = lpProjectRoot loadedPres
              }
      logEntry "runSlides" "Starting UI..."
      runSlidesUI config `catch` \(e :: SomeException) -> do
        logError "runSlides" $ "UI CRASHED: " <> displayException e
        exitFailure
      logEntry "runSlides" "=== SLIDES EXITED NORMALLY ==="
