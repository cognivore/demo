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
  args <- getArgs
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
  result <- loadPresentation presPath
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
