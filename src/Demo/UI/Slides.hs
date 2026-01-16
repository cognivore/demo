{-# LANGUAGE TemplateHaskell #-}

module Demo.UI.Slides
  ( -- * Running the UI
    runSlidesUI
  , SlidesConfig (..)
  ) where

import Brick
  ( App (..)
  , AttrMap
  , AttrName
  , BrickEvent (..)
  , EventM
  , Padding (..)
  , Widget
  , attrMap
  , attrName
  , customMain
  , fg
  , get
  , halt
  , hBox
  , modify
  , on
  , padAll
  , padLeft
  , padRight
  , txt
  , vBox
  , vLimit
  , viewport
  , withAttr
  , withBorderStyle
  , ViewportType (..)
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try, displayException)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Data.Text (Text)
import Data.Text qualified as T
import Demo.Core.Types
  ( Command (..)
  , Mode (..)
  , Presentation
  , Slide (..)
  , SlideState
  , initialSlideState
  , presName
  , presSlides
  , slideCommands
  , slideTitle
  , ssCurrentCommand
  , ssCurrentSlide
  , ssMode
  , ssOutputBuffer
  , ssPresentation
  , ssVarStore
  )
import Demo.IPC.Protocol
  ( IPCMessage (..)
  , socketPathForPresentation
  )
import Demo.IPC.Server
  ( IPCServer
  , ClientHandle (..)
  , broadcast
  , startServer
  , stopServer
  )
import Demo.Interpreter.Ghci (evalGhciExpr, ghciResultDisplay, ghciResultError, ghciResultVarIndex, newInterpreter)
import Demo.Interpreter.System (CommandResult (..), runSystemCommandStreaming)
import Demo.UI.Common (footerAttr, headerAttr, slideProgress)
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix (mkVty)
import Control.Lens ((%~), (.~), (^.), (^?), (&), ix)
import Lens.Micro.TH (makeLenses)

-- | Configuration for the slides UI
data SlidesConfig = SlidesConfig
  { scPresPath :: FilePath
  , scPresentation :: Presentation
  , scProjectRoot :: FilePath
  }

-- | Internal UI state
data UIState = UIState
  { _uiSlideState :: SlideState
  , _uiServer :: Maybe IPCServer
  , _uiGhciInput :: Text
  , _uiRunning :: Bool
  }

makeLenses ''UIState

-- | Custom events
data UIEvent
  = OutputReceived Text
  | CommandDone Int
  deriving stock (Show, Eq)

-- | Resource names
data Name = CommandPane | OutputPane | GhciInput
  deriving stock (Show, Eq, Ord)

-- | Get version info at runtime (includes git state)
getUIVersion :: IO String
getUIVersion = do
  (exitCode1, hash, _) <- readProcessWithExitCode "git" ["-C", "/Users/sweater/Github/demo", "rev-parse", "--short", "HEAD"] ""
  let commitHash = if exitCode1 == ExitSuccess then filter (/= '\n') hash else "unknown"
  
  (exitCode2, status, _) <- readProcessWithExitCode "git" ["-C", "/Users/sweater/Github/demo", "status", "--porcelain"] ""
  let isDirty = exitCode2 == ExitSuccess && not (null (filter (/= '\n') status))
  
  dirtyInfo <- if isDirty
    then do
      (exitCode3, diff, _) <- readProcessWithExitCode "git" ["-C", "/Users/sweater/Github/demo", "diff", "HEAD"] ""
      let diffHash = if exitCode3 == ExitSuccess then take 8 $ show $ abs $ foldl (\h c -> 31 * h + fromEnum c) (0 :: Int) diff else "nodiff"
      pure $ "-dirty-" <> diffHash
    else pure ""
  
  pure $ "ui-slides-" <> commitHash <> dirtyInfo

-- | Log file for UI diagnostics
uiLogFile :: FilePath
uiLogFile = "/Users/sweater/Github/demo/.cursor/slides.log"

-- | Write timestamped log entry
uiLog :: String -> IO ()
uiLog msg = do
  now <- getCurrentTime
  version <- getUIVersion
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      entry = timestamp <> " [" <> version <> "] UI: " <> msg <> "\n"
  _ <- try (appendFile uiLogFile entry) :: IO (Either SomeException ())
  pure ()

-- | Run the slides UI
runSlidesUI :: SlidesConfig -> IO ()
runSlidesUI config = do
  uiLog "runSlidesUI starting"
  
  let initialState =
        UIState
          { _uiSlideState = initialSlideState (scPresentation config)
          , _uiServer = Nothing
          , _uiGhciInput = ""
          , _uiRunning = False
          }

  -- Start IPC server
  let sockPath = socketPathForPresentation (scPresPath config)
  uiLog $ "Starting IPC server at: " <> sockPath
  serverResult <- try (startServer sockPath handleClientMessage)
  server <- case serverResult of
    Left (e :: SomeException) -> do
      uiLog $ "ERROR: IPC server failed to start: " <> displayException e
      error $ "IPC server failed: " <> displayException e
    Right s -> do
      uiLog "IPC server started successfully"
      pure s

  let stateWithServer = initialState {_uiServer = Just server}

  -- Create event channel
  eventChan <- newBChan 100
  uiLog "Event channel created"

  -- Build vty
  uiLog "Building Vty terminal interface"
  vtyResult <- try (mkVty V.defaultConfig)
  initialVty <- case vtyResult of
    Left (e :: SomeException) -> do
      uiLog $ "ERROR: Vty initialization failed: " <> displayException e
      stopServer server
      error $ "Vty failed: " <> displayException e
    Right vty -> do
      uiLog "Vty initialized successfully"
      pure vty

  -- Run the app
  uiLog "Starting Brick UI main loop"
  let buildVty = mkVty V.defaultConfig
  uiResult <- try $ customMain initialVty buildVty (Just eventChan) (slidesApp eventChan) stateWithServer
  case uiResult of
    Left (e :: SomeException) -> do
      uiLog $ "ERROR: Brick UI crashed: " <> displayException e
      stopServer server
      error $ "UI crashed: " <> displayException e
    Right _ -> do
      uiLog "Brick UI exited normally"

  -- Cleanup
  uiLog "Stopping IPC server"
  stopServer server
  uiLog "Cleanup complete"
 where
  handleClientMessage :: IPCMessage -> ClientHandle -> IO ()
  handleClientMessage msg _client = case msg of
    MsgRequestSlide _dir -> pure () -- Handle in event loop
    MsgPing -> pure () -- Could send pong
    _ -> pure ()

-- | The brick application
slidesApp :: BChan UIEvent -> App UIState UIEvent Name
slidesApp eventChan =
  App
    { appDraw = drawUI
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent eventChan
    , appStartEvent = pure ()
    , appAttrMap = const theAttrMap
    }

-- | Draw the UI
drawUI :: UIState -> [Widget Name]
drawUI st = [ui]
 where
  slideState = st ^. uiSlideState
  pres = slideState ^. ssPresentation
  currentIdx = slideState ^. ssCurrentSlide
  slides = pres ^. presSlides
  -- Safe indexing with ix - no more !! or bounds checks
  currentSlide = slides ^? ix currentIdx & maybe emptySlide id

  mode = slideState ^. ssMode
  output = slideState ^. ssOutputBuffer
  cmdIdx = slideState ^. ssCurrentCommand

  ui =
    withBorderStyle unicode $
      vBox
        [ headerWidget pres currentIdx (length slides)
        , commandWidget currentSlide cmdIdx
        , outputWidget output
        , modeWidget mode (st ^. uiGhciInput)
        , footerWidget' (st ^. uiRunning) mode
        ]

-- | Empty slide placeholder
emptySlide :: Slide
emptySlide = Slide "No slides" [] "" []

-- | Header showing presentation title and progress
headerWidget :: Presentation -> Int -> Int -> Widget Name
headerWidget pres currentIdx total =
  withAttr headerAttr $
    hBox
      [ padLeft (Pad 1) $ txt (pres ^. presName)
      , padRight (Pad 1) $ txt $ " " <> slideProgress currentIdx total
      ]

-- | Widget showing current commands (compact, fixed height)
commandWidget :: Slide -> Int -> Widget Name
commandWidget slide cmdIdx =
  borderWithLabel (txt $ " " <> (slide ^. slideTitle) <> " ") $
    vLimit 6 $
      vBox $
        zipWith renderCmd [0 ..] (slide ^. slideCommands)
 where
  renderCmd i cmd =
    let attr = if i == cmdIdx then currentCmdAttr else cmdAttr
        prefix = if i == cmdIdx then "> " else "  "
        cmdText = case cmd of
          SystemCmd t -> "$ " <> t
          GhciCmd e _ -> "λ " <> e
     in withAttr attr $ txt $ prefix <> cmdText

-- | Output display widget - THE QUEEN (expands to fill available space)
-- Note: Cannot use `fill` inside viewport - viewports require finite-height content
outputWidget :: Text -> Widget Name
outputWidget output =
  borderWithLabel (txt " Output ") $
    viewport OutputPane Vertical $
      padAll 1 $
        txt $ if T.null output then "(no output)" else output

-- | Mode indicator and GHCI input
modeWidget :: Mode -> Text -> Widget Name
modeWidget mode ghciInput =
  hBox
    [ padLeft (Pad 1) $
        withAttr modeAttr $
          txt $
            case mode of
              SystemMode -> "[SYSTEM]"
              GhciMode -> "[GHCI]"
    , case mode of
        GhciMode ->
          padLeft (Pad 2) $ txt $ "λ> " <> ghciInput <> "_"
        SystemMode ->
          padLeft (Pad 2) $ txt ""
    ]

-- | Footer with keybindings (mode-aware)
footerWidget' :: Bool -> Mode -> Widget Name
footerWidget' running mode =
  withAttr footerAttr $
    hCenter $
      txt $
        if running
          then "Running..."
          else case mode of
            SystemMode -> "Space:Run  n/→:Next  p/←:Prev  g:GHCI  C-q:Quit"
            GhciMode -> "Enter:Eval  s:System  C-q:Quit  (type expression)"

-- | Handle events
handleEvent ::
  BChan UIEvent ->
  BrickEvent Name UIEvent ->
  EventM Name UIState ()
handleEvent eventChan (VtyEvent (V.EvKey key mods)) = do
  st <- get
  let running = st ^. uiRunning
  let mode = st ^. uiSlideState . ssMode

  case (key, mods, running, mode) of
    -- Quit requires Ctrl-q (Emacs-style, harder to hit accidentally)
    (V.KChar 'q', [V.MCtrl], _, _) -> halt

    -- Mode switching
    (V.KChar 'g', [], False, SystemMode) -> setMode GhciMode
    (V.KChar 's', [], False, GhciMode) -> setMode SystemMode

    -- GHCI mode: capture ALL input (must come AFTER 's' mode switch)
    (V.KEnter, [], False, GhciMode) -> runGhciCommand eventChan
    (V.KBS, [], False, GhciMode) -> backspaceGhci
    (V.KChar c, [], False, GhciMode) -> appendGhci c

    -- Navigation only in SystemMode (when not running)
    (V.KChar 'n', [], False, SystemMode) -> nextSlide
    (V.KRight, [], False, SystemMode) -> nextSlide
    (V.KChar 'p', [], False, SystemMode) -> prevSlide
    (V.KLeft, [], False, SystemMode) -> prevSlide

    -- Run command (SystemMode only)
    (V.KChar ' ', [], False, SystemMode) -> runCurrentCommand eventChan

    _ -> pure ()
handleEvent _ (AppEvent (OutputReceived txt')) = do
  modify $ uiSlideState . ssOutputBuffer %~ (<> txt')
handleEvent _ (AppEvent (CommandDone _exitCode)) = do
  modify $ uiRunning .~ False
  advanceCommand
handleEvent _ _ = pure ()

-- | Navigate slides with bounds checking
navigateSlide :: (Int -> Int -> Int) -> EventM Name UIState ()
navigateSlide f = do
  modify $ uiSlideState %~ \ss ->
    let maxSlide = length (ss ^. ssPresentation . presSlides) - 1
        newIdx = f (ss ^. ssCurrentSlide) maxSlide
     in ss & ssCurrentSlide .~ newIdx & ssCurrentCommand .~ 0 & ssOutputBuffer .~ ""
  broadcastSlideChange

nextSlide, prevSlide :: EventM Name UIState ()
nextSlide = navigateSlide $ \i mx -> min mx (i + 1)
prevSlide = navigateSlide $ \i _  -> max 0 (i - 1)

setMode :: Mode -> EventM Name UIState ()
setMode = modify . (uiSlideState . ssMode .~)

-- | Run the current system command - uses safe indexing with ix
runCurrentCommand :: BChan UIEvent -> EventM Name UIState ()
runCurrentCommand chan = get >>= \st -> do
  let ss = st ^. uiSlideState
      cmd = ss ^? ssPresentation . presSlides . ix (ss ^. ssCurrentSlide)
                . slideCommands . ix (ss ^. ssCurrentCommand)
  case cmd of
    Just (SystemCmd cmdTxt) -> do
      modify $ (uiRunning .~ True) . (uiSlideState . ssOutputBuffer .~ "")
      liftIO $ void $ forkIO $ do
        r <- runSystemCommandStreaming (ss ^. ssVarStore) cmdTxt $ writeBChan chan . OutputReceived
        writeBChan chan (CommandDone (crExitCode r))
    _ -> pure ()

-- | Run GHCI command from input
runGhciCommand :: BChan UIEvent -> EventM Name UIState ()
runGhciCommand eventChan = do
  st <- get
  let input = st ^. uiGhciInput
  when (not $ T.null input) $ do
    modify $ uiRunning .~ True
    let store = st ^. uiSlideState . ssVarStore
    liftIO $ void $ forkIO $ do
      interp <- newInterpreter
      (result, _newStore) <- evalGhciExpr interp input store
      case (ghciResultDisplay result, ghciResultVarIndex result, ghciResultError result) of
        (Just display, Just varIdx, _) -> do
          writeBChan eventChan $
            OutputReceived $
              "$v" <> T.pack (show varIdx) <> " = " <> display <> "\n"
        (_, _, Just err) -> do
          writeBChan eventChan $ OutputReceived $ "Error: " <> err <> "\n"
        _ -> pure ()
      writeBChan eventChan (CommandDone 0)

    modify $ uiGhciInput .~ ""

backspaceGhci, advanceCommand :: EventM Name UIState ()
backspaceGhci = modify $ uiGhciInput %~ \t -> if T.null t then t else T.init t
advanceCommand = modify $ uiSlideState %~ \ss ->
  let idx = ss ^. ssCurrentSlide
      mx = ss ^? ssPresentation . presSlides . ix idx . slideCommands & maybe 0 (subtract 1 . length)
   in ss & ssCurrentCommand %~ min mx . (+1)

appendGhci :: Char -> EventM Name UIState ()
appendGhci = modify . (uiGhciInput %~) . flip T.snoc

broadcastSlideChange :: EventM Name UIState ()
broadcastSlideChange = get >>= \st ->
  let ss = st ^. uiSlideState
      idx = ss ^. ssCurrentSlide
   in case (st ^. uiServer, ss ^? ssPresentation . presSlides . ix idx) of
        (Just srv, Just sl) -> liftIO $ broadcast srv (MsgSlideChanged idx sl)
        _ -> pure ()

-- | Attribute names (some shared, some slides-specific)
cmdAttr, currentCmdAttr, modeAttr :: AttrName
cmdAttr = attrName "cmd"
currentCmdAttr = attrName "currentCmd"
modeAttr = attrName "mode"

-- | Attribute map
theAttrMap :: AttrMap
theAttrMap =
  attrMap
    V.defAttr
    [ (headerAttr, V.white `on` V.blue)
    , (footerAttr, V.black `on` V.white)
    , (cmdAttr, fg V.white)
    , (currentCmdAttr, V.black `on` V.yellow)
    , (modeAttr, fg V.cyan)
    ]
