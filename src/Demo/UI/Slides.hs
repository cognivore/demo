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
  , withAttr
  , withBorderStyle
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter)
import Control.Concurrent (forkIO)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
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

-- | Run the slides UI
runSlidesUI :: SlidesConfig -> IO ()
runSlidesUI config = do
  let initialState =
        UIState
          { _uiSlideState = initialSlideState (scPresentation config)
          , _uiServer = Nothing
          , _uiGhciInput = ""
          , _uiRunning = False
          }

  -- Start IPC server
  let sockPath = socketPathForPresentation (scPresPath config)
  server <- startServer sockPath handleClientMessage

  let stateWithServer = initialState {_uiServer = Just server}

  -- Create event channel
  eventChan <- newBChan 100

  -- Build vty
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  -- Run the app
  void $
    customMain initialVty buildVty (Just eventChan) (slidesApp eventChan) stateWithServer

  -- Cleanup
  stopServer server
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
        , footerWidget' (st ^. uiRunning)
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

-- | Widget showing current commands
commandWidget :: Slide -> Int -> Widget Name
commandWidget slide cmdIdx =
  borderWithLabel (txt $ " " <> (slide ^. slideTitle) <> " ") $
    vLimit 10 $
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

-- | Output display widget
outputWidget :: Text -> Widget Name
outputWidget output =
  borderWithLabel (txt " Output ") $
    vLimit 15 $
      padAll 1 $
        txt $
          if T.null output then "(no output)" else output

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

-- | Footer with keybindings
footerWidget' :: Bool -> Widget Name
footerWidget' running =
  withAttr footerAttr $
    hCenter $
      txt $
        if running
          then "Running..."
          else "Space:Run  n/→:Next  p/←:Prev  g:GHCI  s:System  q:Quit"

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
    -- Quit
    (V.KChar 'q', [], False, _) -> halt
    (V.KEsc, [], False, _) -> halt
    -- Navigation (when not running)
    (V.KChar 'n', [], False, _) -> nextSlide
    (V.KRight, [], False, _) -> nextSlide
    (V.KChar 'p', [], False, _) -> prevSlide
    (V.KLeft, [], False, _) -> prevSlide
    -- Mode switching
    (V.KChar 'g', [], False, _) -> setMode GhciMode
    (V.KChar 's', [], False, _) -> setMode SystemMode
    -- Run command
    (V.KChar ' ', [], False, SystemMode) -> runCurrentCommand eventChan
    -- GHCI input handling
    (V.KEnter, [], False, GhciMode) -> runGhciCommand eventChan
    (V.KBS, [], False, GhciMode) -> backspaceGhci
    (V.KChar c, [], False, GhciMode) -> appendGhci c
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
