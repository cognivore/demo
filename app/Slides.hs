{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (SomeException, catch, displayException, try)
import Demo.Loader (loadPresentation, formatLoadError, lpPresentation, lpProjectRoot, lpPresentationPath)
import Demo.UI.Slides (SlidesConfig (..), runSlidesUI)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, hFlush)

main :: IO ()
main = do
  -- #region agent log
  _ <- try (appendFile "/Users/sweater/Github/demo/.cursor/debug.log" "{\"location\":\"Slides.hs:main:entry\",\"message\":\"Slides main starting\",\"hypothesisId\":\"H1-demo-src-missing\"}\n") :: IO (Either SomeException ())
  -- #endregion
  
  -- #region agent log
  mDemoSrc <- lookupEnv "DEMO_SRC_PATH"
  _ <- try (appendFile "/Users/sweater/Github/demo/.cursor/debug.log" $ "{\"location\":\"Slides.hs:main:env\",\"message\":\"DEMO_SRC_PATH check\",\"data\":{\"value\":\"" <> show mDemoSrc <> "\"},\"hypothesisId\":\"H1-demo-src-missing\"}\n") :: IO (Either SomeException ())
  -- #endregion
  
  args <- getArgs
  case args of
    [presPath] -> runSlides presPath `catch` \(e :: SomeException) -> do
      -- #region agent log
      _ <- try (appendFile "/Users/sweater/Github/demo/.cursor/debug.log" $ "{\"location\":\"Slides.hs:main:fatal\",\"message\":\"Fatal exception caught\",\"data\":{\"error\":\"" <> take 200 (displayException e) <> "\"},\"hypothesisId\":\"H4-brick-crash\"}\n") :: IO (Either SomeException ())
      -- #endregion
      hPutStrLn stderr $ "Fatal error: " <> displayException e
      hFlush stderr
      exitFailure
    _ -> do
      hPutStrLn stderr "Usage: slides <presentation.hs>"
      exitFailure

runSlides :: FilePath -> IO ()
runSlides presPath = do
  -- #region agent log
  _ <- try (appendFile "/Users/sweater/Github/demo/.cursor/debug.log" $ "{\"location\":\"Slides.hs:runSlides:entry\",\"message\":\"runSlides starting\",\"data\":{\"presPath\":\"" <> presPath <> "\"},\"hypothesisId\":\"H2-presentation-load-fail\"}\n") :: IO (Either SomeException ())
  -- #endregion
  
  result <- loadPresentation presPath
  case result of
    Left err -> do
      -- #region agent log
      _ <- try (appendFile "/Users/sweater/Github/demo/.cursor/debug.log" $ "{\"location\":\"Slides.hs:runSlides:load-fail\",\"message\":\"Presentation load failed\",\"data\":{\"error\":\"" <> take 200 (formatLoadError err) <> "\"},\"hypothesisId\":\"H2-presentation-load-fail\"}\n") :: IO (Either SomeException ())
      -- #endregion
      hPutStrLn stderr $ "Error loading presentation: " <> formatLoadError err
      hFlush stderr
      exitFailure
    Right loadedPres -> do
      -- #region agent log
      _ <- try (appendFile "/Users/sweater/Github/demo/.cursor/debug.log" "{\"location\":\"Slides.hs:runSlides:load-ok\",\"message\":\"Presentation loaded successfully\",\"hypothesisId\":\"H2-presentation-load-fail\"}\n") :: IO (Either SomeException ())
      -- #endregion
      let config =
            SlidesConfig
              { scPresPath = lpPresentationPath loadedPres
              , scPresentation = lpPresentation loadedPres
              , scProjectRoot = lpProjectRoot loadedPres
              }
      -- #region agent log
      _ <- try (appendFile "/Users/sweater/Github/demo/.cursor/debug.log" "{\"location\":\"Slides.hs:runSlides:ui-start\",\"message\":\"Starting UI\",\"hypothesisId\":\"H4-brick-crash\"}\n") :: IO (Either SomeException ())
      -- #endregion
      runSlidesUI config
