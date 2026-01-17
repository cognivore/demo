-- | Presentation loader using hint interpreter
module Demo.Loader
  ( -- * Loading Presentations
    loadPresentation
  , LoadedPresentation (..)
  , lpPresentation
  , lpProjectRoot
  , lpPresentationPath
  , lpPresentationDir
  , LoadError (..)
  , formatLoadError

    -- * Project Root Detection
  , findProjectRoot
  , resolvePathFromRoot
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Demo.Core.Types (Presentation)
import Control.Exception (try, SomeException)
import Language.Haskell.Interpreter
  ( Extension (..)
  , GhcError (..)
  , InterpreterError (..)
  , OptionVal ((:=))
  , as
  , interpret
  , languageExtensions
  , loadModules
  , searchPath
  , set
  , setImportsQ
  , setTopLevelModules
  )
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import System.Directory (doesFileExist, doesDirectoryExist, getCurrentDirectory, makeAbsolute, findExecutable)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, (</>), takeFileName)
import System.Process (readProcess)

-- | A loaded presentation with context
data LoadedPresentation = LoadedPresentation
  { _lpPresentation :: Presentation
  , _lpProjectRoot :: FilePath      -- ^ Detected project root
  , _lpPresentationPath :: FilePath -- ^ Absolute path to presentation file
  , _lpPresentationDir :: FilePath  -- ^ Directory containing presentation
  }
  deriving stock (Show)

-- | Get the presentation from a loaded presentation
lpPresentation :: LoadedPresentation -> Presentation
lpPresentation = _lpPresentation

-- | Get the project root from a loaded presentation
lpProjectRoot :: LoadedPresentation -> FilePath
lpProjectRoot = _lpProjectRoot

-- | Get the absolute path to the presentation file
lpPresentationPath :: LoadedPresentation -> FilePath
lpPresentationPath = _lpPresentationPath

-- | Get the directory containing the presentation
lpPresentationDir :: LoadedPresentation -> FilePath
lpPresentationDir = _lpPresentationDir

-- | Errors that can occur when loading a presentation
data LoadError
  = FileNotFound FilePath
  | ParseError Text
  | InterpretError Text
  deriving stock (Show, Eq)

-- | Format a load error for display
formatLoadError :: LoadError -> String
formatLoadError = \case
  FileNotFound path -> "File not found: " <> path
  ParseError msg -> "Parse error: " <> T.unpack msg
  InterpretError msg -> "Interpret error: " <> T.unpack msg

-- | Find the project root by looking for marker files
-- Searches upward from the given path for:
--   1. A .cabal file
--   2. A .git directory
--   3. A cabal.project file
-- Falls back to the presentation's directory if no markers found
findProjectRoot :: FilePath -> IO FilePath
findProjectRoot startPath = do
  absPath <- makeAbsolute startPath
  let startDir = if takeFileName absPath == absPath
                 then absPath  -- It's already a directory
                 else takeDirectory absPath
  searchUpward startDir
 where
  searchUpward :: FilePath -> IO FilePath
  searchUpward dir = do
    -- Check for project markers
    hasCabal <- anyCabalFile dir
    hasGit <- doesDirectoryExist (dir </> ".git")
    hasCabalProject <- doesFileExist (dir </> "cabal.project")

    if hasCabal || hasGit || hasCabalProject
      then pure dir
      else do
        let parent = takeDirectory dir
        if parent == dir  -- Reached filesystem root
          then pure (takeDirectory startPath)  -- Fall back to start dir
          else searchUpward parent

  -- Check if any .cabal file exists in directory
  anyCabalFile :: FilePath -> IO Bool
  anyCabalFile dir = do
    -- Simple check: look for common patterns
    exists1 <- doesFileExist (dir </> "demo.cabal")
    exists2 <- doesFileExist (dir </> "package.cabal")
    -- Also check if there's a cabal file with the directory name
    let dirName = takeFileName dir
    exists3 <- doesFileExist (dir </> (dirName <> ".cabal"))
    pure (exists1 || exists2 || exists3)

-- | Resolve a path relative to the project root
-- Tries multiple locations in order:
--   1. Relative to project root
--   2. Relative to presentation directory
--   3. As absolute path
--   4. Relative to current directory
resolvePathFromRoot :: LoadedPresentation -> FilePath -> IO FilePath
resolvePathFromRoot lp relPath = do
  cwd <- getCurrentDirectory
  let candidates =
        [ lpProjectRoot lp </> relPath       -- Relative to project root
        , lpPresentationDir lp </> relPath   -- Relative to presentation
        , relPath                             -- As-is (might be absolute)
        , cwd </> relPath                     -- Relative to cwd
        ]
  findFirst candidates
 where
  findFirst [] = pure relPath  -- Fall back to original
  findFirst (p:ps) = do
    exists <- doesFileExist p
    if exists then pure p else findFirst ps

-- | Find the GHC package database path
findPackageDb :: IO (Maybe FilePath)
findPackageDb = do
  mGhcPkg <- findExecutable "ghc-pkg"
  case mGhcPkg of
    Nothing -> pure Nothing
    Just ghcPkg -> do
      result <- try $ readProcess ghcPkg ["list", "-v0"] ""
      case result of
        Left (_ :: SomeException) -> pure Nothing
        Right output -> do
          -- The first line of ghc-pkg list output is the package database path
          let firstLine = case lines output of
                (l:_) | not (null l) -> Just l
                _ -> Nothing
          pure firstLine

-- | Load a presentation from a Haskell source file
loadPresentation :: FilePath -> IO (Either LoadError LoadedPresentation)
loadPresentation path = do
  absPath <- makeAbsolute path
  exists <- doesFileExist absPath

  if not exists
    then pure $ Left $ FileNotFound absPath
    else do
      -- Find the project root for this presentation
      projectRoot <- findProjectRoot absPath
      let presDir = takeDirectory absPath

      -- Find the package database for GHC args
      mPkgDb <- findPackageDb
      let ghcArgs = case mPkgDb of
            Nothing -> []
            Just db -> ["-package-db=" <> db]

      -- Search paths for user presentation files
      let searchPaths = [ projectRoot </> "src", presDir, projectRoot ]

      result <- unsafeRunInterpreterWithArgs ghcArgs $ do
        -- Note: On Apple Silicon, the demo package must be compiled with
        -- -finter-module-far-jumps to avoid the PAGE21 relocation bug.
        -- This is handled in flake.nix for aarch64-darwin builds.
        set [ searchPath := searchPaths
            , languageExtensions :=
                [ OverloadedStrings
                , UnknownExtension "DerivingStrategies"
                , DeriveGeneric
                , DeriveAnyClass
                , DeriveFunctor
                , TemplateHaskell
                , UnknownExtension "NoFieldSelectors"
                , LambdaCase
                ]
            ]

        -- Load the module
        loadModules [absPath]

        -- Set up imports - the module name is derived from the file
        let moduleName = takeBaseName (dropExtension absPath)
        setTopLevelModules [moduleName]

        setImportsQ
          [ ("Prelude", Nothing)
          , ("Demo.Core.DSL", Nothing)
          , ("Demo.Core.Types", Nothing)
          ]

        -- Interpret the 'presentation' value
        interpret "presentation" (as :: Presentation)

      case result of
        Left err -> pure $ Left $ InterpretError (formatError err)
        Right pres -> pure $ Right $ LoadedPresentation
          { _lpPresentation = pres
          , _lpProjectRoot = projectRoot
          , _lpPresentationPath = absPath
          , _lpPresentationDir = presDir
          }

-- | Format interpreter error for display
formatError :: InterpreterError -> Text
formatError = \case
  UnknownError msg -> T.pack msg
  WontCompile errs -> T.unlines $ map (T.pack . errMsg) errs
  NotAllowed msg -> "Not allowed: " <> T.pack msg
  GhcException msg -> "GHC Exception: " <> T.pack msg
