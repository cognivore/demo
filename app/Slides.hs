{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (SomeException, catch, displayException)
import Demo.Loader (loadPresentation, formatLoadError, lpPresentation, lpProjectRoot, lpPresentationPath)
import Demo.UI.Slides (SlidesConfig (..), runSlidesUI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hFlush)

main :: IO ()
main = do
  -- #region agent log
  appendFile "/Users/sweater/Github/demo/.cursor/debug.log" "{\"location\":\"Slides.hs:main:start\",\"message\":\"Starting slides\",\"hypothesisId\":\"H0-start\"}\n"
  -- #endregion
  args <- getArgs
  -- #region agent log
  appendFile "/Users/sweater/Github/demo/.cursor/debug.log" $ "{\"location\":\"Slides.hs:main:args\",\"message\":\"Got args\",\"data\":{\"args\":" <> show args <> "},\"hypothesisId\":\"H0-args\"}\n"
  -- #endregion
  case args of
    [presPath] -> runSlides presPath `catch` \(e :: SomeException) -> do
      hPutStrLn stderr $ "Fatal error: " <> displayException e
      hFlush stderr
      exitFailure
    _ -> do
      hPutStrLn stderr "Usage: slides <presentation.hs>"
      exitFailure

runSlides :: FilePath -> IO ()
runSlides presPath = do
  -- #region agent log
  appendFile "/Users/sweater/Github/demo/.cursor/debug.log" $ "{\"location\":\"Slides.hs:runSlides\",\"message\":\"Before loadPresentation\",\"data\":{\"presPath\":\"" <> presPath <> "\"},\"hypothesisId\":\"H1-runslides\"}\n"
  -- #endregion
  result <- loadPresentation presPath
  -- #region agent log
  appendFile "/Users/sweater/Github/demo/.cursor/debug.log" "{\"location\":\"Slides.hs:runSlides:afterLoad\",\"message\":\"After loadPresentation\",\"hypothesisId\":\"H1-runslides\"}\n"
  -- #endregion
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error loading presentation: " <> formatLoadError err
      hFlush stderr
      exitFailure
    Right loadedPres -> do
      let config =
            SlidesConfig
              { scPresPath = lpPresentationPath loadedPres
              , scPresentation = lpPresentation loadedPres
              , scProjectRoot = lpProjectRoot loadedPres
              }
      runSlidesUI config
