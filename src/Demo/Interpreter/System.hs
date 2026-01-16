module Demo.Interpreter.System
  ( -- * Command Execution
    runSystemCommand
  , runSystemCommandStreaming
  , runSystemCommandWith
  , runSystemCommandStreamingWith

    -- * Execution Settings
  , ExecSettings (..)
  , NixDevelopConfig (..)
  , NixCommand (..)
  , DirenvConfig (..)
  , resolveExecSettings

    -- * Results
  , CommandResult (..)
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Exception (SomeException, bracket, catch)
import Control.Lens ((^.))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Demo.Core.Types
  ( VarStore
  , SystemPrelude
  , NixDevelopMode (..)
  , DirenvMode (..)
  , ExecMode (..)
  , spCwd
  , spDirenv
  , spExecMode
  , spNixDevelop
  )
import Demo.Core.Variable (substituteVars)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , removeFile
  )
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath (isAbsolute, (</>))
import System.IO (Handle, hClose, hGetLine, hIsEOF, openTempFile)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )

-- | Result of a command execution
data CommandResult = CommandResult
  { crExitCode :: Int
  , crOutput :: Text
  }
  deriving stock (Show, Eq)

-- | Configuration for nix develop execution
data NixDevelopConfig = NixDevelopConfig
  { ndcFlakeDir :: FilePath
  , ndcDevShell :: Maybe Text
  , ndcCommand :: NixCommand
  }
  deriving stock (Show, Eq)

data NixCommand = NixCommandDevelop | NixCommandShell
  deriving stock (Show, Eq)

-- | Configuration for direnv execution
data DirenvConfig = DirenvConfig
  { dcDir :: FilePath
  }
  deriving stock (Show, Eq)

-- | Execution settings for system commands
data ExecSettings = ExecSettings
  { esWorkingDir :: Maybe FilePath
  , esNixDevelop :: Maybe NixDevelopConfig
  , esDirenv :: Maybe DirenvConfig
  , esExecMode :: ExecMode
  }
  deriving stock (Show, Eq)

-- | Default execution settings (no wrappers)
defaultExecSettings :: ExecSettings
defaultExecSettings =
  ExecSettings
    { esWorkingDir = Nothing
    , esNixDevelop = Nothing
    , esDirenv = Nothing
    , esExecMode = ExecInline
    }

-- | Resolve execution settings from prelude and project context
resolveExecSettings :: FilePath -> FilePath -> SystemPrelude -> IO ExecSettings
resolveExecSettings projectRoot presDir prelude = do
  resolvedCwd <- resolveWorkingDir projectRoot presDir (prelude ^. spCwd)
  nixConfig <- resolveNixDevelop projectRoot resolvedCwd (prelude ^. spNixDevelop)
  direnvConfig <- resolveDirenv projectRoot resolvedCwd (prelude ^. spDirenv)
  pure $
    ExecSettings
      { esWorkingDir = resolvedCwd
      , esNixDevelop = nixConfig
      , esDirenv = direnvConfig
      , esExecMode = prelude ^. spExecMode
      }

-- | Run a system command and return the complete result
runSystemCommand :: VarStore -> Text -> IO CommandResult
runSystemCommand = runSystemCommandWith defaultExecSettings

-- | Run a system command with streaming output
runSystemCommandStreaming ::
  VarStore ->
  Text ->
  (Text -> IO ()) -> -- Output callback
  IO CommandResult
runSystemCommandStreaming = runSystemCommandStreamingWith defaultExecSettings

-- | Run a system command with execution settings
runSystemCommandWith :: ExecSettings -> VarStore -> Text -> IO CommandResult
runSystemCommandWith settings store cmdText =
  runCommandWith settings store cmdText Nothing

-- | Run a system command with streaming output and settings
runSystemCommandStreamingWith ::
  ExecSettings ->
  VarStore ->
  Text ->
  (Text -> IO ()) ->
  IO CommandResult
runSystemCommandStreamingWith settings store cmdText onOutput =
  runCommandWith settings store cmdText (Just onOutput)

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

data CommandSpec = CommandSpec
  { csCommand :: FilePath
  , csArgs :: [String]
  }
  deriving stock (Show, Eq)

runCommandWith ::
  ExecSettings ->
  VarStore ->
  Text ->
  Maybe (Text -> IO ()) ->
  IO CommandResult
runCommandWith settings store cmdText mOnOutput = do
  let cmd = substituteVars store cmdText
  withCommandSpec (esExecMode settings) cmd $ \baseSpec -> do
    let wrappedSpec = applyWrappers settings baseSpec
    (_, Just hout, Just herr, ph) <-
      createProcess
        (proc (csCommand wrappedSpec) (csArgs wrappedSpec))
          { std_out = CreatePipe
          , std_err = CreatePipe
          , cwd = esWorkingDir settings
          }

    outAsync <- async $ outputReader hout
    errAsync <- async $ outputReader herr

    output <- wait outAsync
    errOutput <- wait errAsync

    exitCode <- waitForProcess ph

    let combinedOutput = combineOutput output errOutput

    pure $
      CommandResult
        { crExitCode = exitCodeToInt exitCode
        , crOutput = combinedOutput
        }
 where
  outputReader = case mOnOutput of
    Nothing -> readAllText
    Just cb -> streamOutput cb

  combineOutput out err = case mOnOutput of
    Nothing ->
      if T.null err
        then out
        else out <> "\n" <> err
    Just _ ->
      if T.null err
        then out
        else out <> err

withCommandSpec :: ExecMode -> Text -> (CommandSpec -> IO a) -> IO a
withCommandSpec mode cmdText k = case mode of
  ExecInline -> k $ CommandSpec "sh" ["-c", T.unpack cmdText]
  ExecTempScript ->
    withTempScript cmdText $ \path ->
      k $ CommandSpec "sh" [path]

applyWrappers :: ExecSettings -> CommandSpec -> CommandSpec
applyWrappers settings =
  wrapDirenv settings . wrapNixDevelop settings

wrapNixDevelop :: ExecSettings -> CommandSpec -> CommandSpec
wrapNixDevelop settings = case esNixDevelop settings of
  Nothing -> id
  Just cfg ->
    let flakeRef =
          case ndcDevShell cfg of
            Nothing -> ndcFlakeDir cfg
            Just shellName -> ndcFlakeDir cfg <> "#" <> T.unpack shellName
        nixSubcommand = case ndcCommand cfg of
          NixCommandDevelop -> "develop"
          NixCommandShell -> "shell"
    in \spec ->
      CommandSpec
        { csCommand = "nix"
        , csArgs =
            [ nixSubcommand
            , flakeRef
            , "--command"
            , csCommand spec
            ]
              <> csArgs spec
        }

wrapDirenv :: ExecSettings -> CommandSpec -> CommandSpec
wrapDirenv settings = case esDirenv settings of
  Nothing -> id
  Just cfg ->
    \spec ->
      CommandSpec
        { csCommand = "direnv"
        , csArgs = ["exec", dcDir cfg, csCommand spec] <> csArgs spec
        }

readAllText :: Handle -> IO Text
readAllText h = go []
 where
  go acc = do
    eof <- hIsEOF h
    if eof
      then do
        hClose h
        pure $ T.unlines (reverse acc)
      else do
        line <- TIO.hGetLine h
        go (line : acc)

streamOutput :: (Text -> IO ()) -> Handle -> IO Text
streamOutput onOutput h = go []
 where
  go acc = do
    eof <- hIsEOF h
    if eof
      then do
        hClose h
        pure $ T.concat (reverse acc)
      else do
        line <-
          (T.pack <$> hGetLine h)
            `catch` \(_ :: SomeException) -> pure ""
        let lineWithNl = line <> "\n"
        onOutput lineWithNl
        go (lineWithNl : acc)

resolveWorkingDir :: FilePath -> FilePath -> Maybe FilePath -> IO (Maybe FilePath)
resolveWorkingDir _ _ Nothing = pure Nothing
resolveWorkingDir projectRoot presDir (Just path) = do
  expanded <- expandTilde path
  if isAbsolute expanded
    then pure $ Just expanded
    else do
      cwd <- getCurrentDirectory
      let candidates =
            [ projectRoot </> expanded
            , presDir </> expanded
            , cwd </> expanded
            ]
          fallback = projectRoot </> expanded
      resolved <- findFirst fallback candidates
      pure $ Just resolved
 where
  findFirst fallback [] = pure fallback
  findFirst fallback (p:ps) = do
    exists <- doesDirectoryExist p
    if exists then pure p else findFirst fallback ps

resolveNixDevelop ::
  FilePath ->
  Maybe FilePath ->
  NixDevelopMode ->
  IO (Maybe NixDevelopConfig)
resolveNixDevelop projectRoot resolvedCwd mode = case mode of
  NixDevelopOff -> pure Nothing
  NixDevelopOn shellName ->
    pure $ Just $ NixDevelopConfig (selectBase projectRoot resolvedCwd) shellName NixCommandDevelop
  NixDevelopAuto shellName -> do
    mDir <- firstWithFlake (candidateDirs projectRoot resolvedCwd)
    pure $ fmap (\dir -> NixDevelopConfig dir shellName NixCommandDevelop) mDir
  NixShellOn packageName ->
    pure $ Just $ NixDevelopConfig (selectBase projectRoot resolvedCwd) packageName NixCommandShell
  NixShellAuto packageName -> do
    mDir <- firstWithFlake (candidateDirs projectRoot resolvedCwd)
    pure $ fmap (\dir -> NixDevelopConfig dir packageName NixCommandShell) mDir

resolveDirenv ::
  FilePath ->
  Maybe FilePath ->
  DirenvMode ->
  IO (Maybe DirenvConfig)
resolveDirenv projectRoot resolvedCwd mode = case mode of
  DirenvOff -> pure Nothing
  DirenvOn -> pure $ Just $ DirenvConfig (selectBase projectRoot resolvedCwd)
  DirenvAuto -> do
    mDir <- firstWithEnvrc (candidateDirs projectRoot resolvedCwd)
    pure $ fmap DirenvConfig mDir

selectBase :: FilePath -> Maybe FilePath -> FilePath
selectBase projectRoot = maybe projectRoot id

candidateDirs :: FilePath -> Maybe FilePath -> [FilePath]
candidateDirs projectRoot resolvedCwd =
  case resolvedCwd of
    Just cwd -> [cwd, projectRoot]
    Nothing -> [projectRoot]

firstWithFlake :: [FilePath] -> IO (Maybe FilePath)
firstWithFlake [] = pure Nothing
firstWithFlake (d:ds) = do
  hasFlake <- doesFileExist (d </> "flake.nix")
  if hasFlake then pure (Just d) else firstWithFlake ds

firstWithEnvrc :: [FilePath] -> IO (Maybe FilePath)
firstWithEnvrc [] = pure Nothing
firstWithEnvrc (d:ds) = do
  hasEnvrc <- doesFileExist (d </> ".envrc")
  if hasEnvrc then pure (Just d) else firstWithEnvrc ds

expandTilde :: FilePath -> IO FilePath
expandTilde ('~':'/':rest) = do
  home <- getEnv "HOME"
  pure $ home </> rest
expandTilde ('~':rest) | null rest = getEnv "HOME"
expandTilde p = pure p

withTempScript :: Text -> (FilePath -> IO a) -> IO a
withTempScript content k = do
  tmpDir <- getTemporaryDirectory
  bracket
    (openTempFile tmpDir "demo-command.sh")
    (\(path, h) -> hClose h >> removeFile path)
    (\(path, h) -> do
      TIO.hPutStr h (content <> "\n")
      hClose h
      k path
    )

-- | Convert ExitCode to Int
exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n
