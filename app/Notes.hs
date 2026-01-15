{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Demo.Loader (loadPresentation, formatLoadError, lpPresentation, lpProjectRoot, lpPresentationPath)
import Demo.UI.Notes (NotesConfig (..), NotesMode (..), runNotesUI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [presPath] -> runNotes presPath ConnectedMode
    ["--recall", presPath] -> runNotes presPath RecallMode
    [presPath, "--recall"] -> runNotes presPath RecallMode
    _ -> do
      hPutStrLn stderr "Usage: notes [--recall] <presentation.hs>"
      hPutStrLn stderr ""
      hPutStrLn stderr "Options:"
      hPutStrLn stderr "  --recall    Run in standalone recall mode (no IPC connection)"
      exitFailure

runNotes :: FilePath -> NotesMode -> IO ()
runNotes presPath mode = do
  result <- loadPresentation presPath
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error loading presentation: " <> formatLoadError err
      exitFailure
    Right loadedPres -> do
      let config =
            NotesConfig
              { ncPresPath = lpPresentationPath loadedPres
              , ncPresentation = lpPresentation loadedPres
              , ncProjectRoot = lpProjectRoot loadedPres
              , ncMode = mode
              }
      runNotesUI config
