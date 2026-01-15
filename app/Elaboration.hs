{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Demo.Loader (loadPresentation, formatLoadError, lpPresentation, lpProjectRoot, lpPresentationPath, lpPresentationDir)
import Demo.UI.Elaboration (ElaborationConfig (..), runElaborationUI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [presPath] -> runElab presPath
    _ -> do
      hPutStrLn stderr "Usage: elaboration <presentation.hs>"
      exitFailure

runElab :: FilePath -> IO ()
runElab presPath = do
  result <- loadPresentation presPath
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error loading presentation: " <> formatLoadError err
      exitFailure
    Right loadedPres -> do
      let config =
            ElaborationConfig
              { ecPresPath = lpPresentationPath loadedPres
              , ecPresentation = lpPresentation loadedPres
              , ecProjectRoot = lpProjectRoot loadedPres
              , ecPresDir = lpPresentationDir loadedPres
              }
      runElaborationUI config
