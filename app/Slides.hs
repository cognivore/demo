{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Demo.Loader (loadPresentation, formatLoadError, lpPresentation, lpProjectRoot, lpPresentationPath)
import Demo.UI.Slides (SlidesConfig (..), runSlidesUI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [presPath] -> runSlides presPath
    _ -> do
      hPutStrLn stderr "Usage: slides <presentation.hs>"
      exitFailure

runSlides :: FilePath -> IO ()
runSlides presPath = do
  result <- loadPresentation presPath
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error loading presentation: " <> formatLoadError err
      exitFailure
    Right loadedPres -> do
      let config =
            SlidesConfig
              { scPresPath = lpPresentationPath loadedPres
              , scPresentation = lpPresentation loadedPres
              , scProjectRoot = lpProjectRoot loadedPres
              }
      runSlidesUI config
