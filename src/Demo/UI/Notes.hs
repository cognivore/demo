{-# LANGUAGE TemplateHaskell #-}

module Demo.UI.Notes
  ( -- * Running the UI
    runNotesUI
  , NotesConfig (..)
  , NotesMode (..)
  ) where

import Brick
  ( App (..)
  , BrickEvent (..)
  , EventM
  , Widget
  , customMain
  , get
  , halt
  , modify
  , padAll
  , txt
  , vBox
  , withAttr
  , withBorderStyle
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, hCenter)
import Data.Text (Text)
import Data.Text qualified as T
import Demo.Core.Types (Presentation, Slide, presSlides, slideNotes)
import Demo.IPC.Client
  ( IPCClient
  , connectToServer
  , disconnectFromServer
  , setMessageHandler
  )
import Demo.IPC.Protocol
  ( ClientType (..)
  , IPCMessage (..)
  , socketPathForPresentation
  )
import Demo.UI.Common (connectionStatus, defaultAttrMap, footerAttr, headerAttr, slideProgress)
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix (mkVty)
import Control.Lens ((&), (.~), (^.), (^?), ix)
import Lens.Micro.TH (makeLenses)

-- | Configuration for the notes UI
data NotesConfig = NotesConfig
  { ncPresPath :: FilePath
  , ncPresentation :: Presentation
  , ncProjectRoot :: FilePath
  , ncMode :: NotesMode
  }

-- | Mode for notes UI
data NotesMode
  = -- | Connected to slides server
    ConnectedMode
  | -- | Standalone recall mode
    RecallMode
  deriving stock (Show, Eq)

-- | Internal UI state
data UIState = UIState
  { _uiCurrentSlide :: Int
  , _uiCurrentNote :: Text
  , _uiPresentation :: Presentation
  , _uiConnected :: Bool
  , _uiClient :: Maybe IPCClient
  }

makeLenses ''UIState

-- | Custom events
data UIEvent
  = SlideChangedEvent Int Slide
  | ConnectionLost
  deriving stock (Show)

-- | Resource names
data Name = NotesPane
  deriving stock (Show, Eq, Ord)

-- | Run the notes UI
runNotesUI :: NotesConfig -> IO ()
runNotesUI config = do
  let pres = ncPresentation config
      firstNote = case pres ^. presSlides of
        [] -> ""
        (s : _) -> s ^. slideNotes

  let initialState =
        UIState
          { _uiCurrentSlide = 0
          , _uiCurrentNote = firstNote
          , _uiPresentation = pres
          , _uiConnected = False
          , _uiClient = Nothing
          }

  -- Create event channel
  eventChan <- newBChan 100

  -- Try to connect to slides server
  stateWithClient <- case ncMode config of
    RecallMode -> pure initialState
    ConnectedMode -> do
      let sockPath = socketPathForPresentation (ncPresPath config)
      result <- connectToServer sockPath NotesClient
      case result of
        Left _ -> pure initialState -- Fall back to recall mode
        Right client -> do
          setMessageHandler client $ \msg -> case msg of
            MsgSlideChanged idx slide ->
              writeBChan eventChan (SlideChangedEvent idx slide)
            _ -> pure ()
          pure $
            initialState
              { _uiConnected = True
              , _uiClient = Just client
              }

  -- Build vty
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty

  -- Run the app
  finalState <-
    customMain initialVty buildVty (Just eventChan) notesApp stateWithClient

  -- Cleanup
  case finalState ^. uiClient of
    Just client -> disconnectFromServer client
    Nothing -> pure ()

-- | The brick application
notesApp :: App UIState UIEvent Name
notesApp =
  App
    { appDraw = drawUI
    , appChooseCursor = \_ _ -> Nothing
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = const defaultAttrMap
    }

-- | Draw the UI
drawUI :: UIState -> [Widget Name]
drawUI st = [ui]
 where
  note = st ^. uiCurrentNote
  slideNum = st ^. uiCurrentSlide
  totalSlides = length $ (st ^. uiPresentation) ^. presSlides
  connected = st ^. uiConnected

  ui =
    withBorderStyle unicode $
      vBox
        [ headerWidget slideNum totalSlides connected
        , noteWidget note
        , footerWidget connected
        ]

-- | Header widget
headerWidget :: Int -> Int -> Bool -> Widget Name
headerWidget slideNum total connected =
  withAttr headerAttr $
    hCenter $
      txt $
        "Notes " <> slideProgress slideNum total <> connectionStatus connected

-- | Note display widget
noteWidget :: Text -> Widget Name
noteWidget note =
  borderWithLabel (txt " Speaker Notes ") $
    center $
      padAll 2 $
        txt $
          if T.null note then "(no notes for this slide)" else note

-- | Footer widget
footerWidget :: Bool -> Widget Name
footerWidget connected =
  withAttr footerAttr $
    hCenter $
      txt $
        if connected
          then "Synced with slides  |  C-q:Quit"
          else "←/→:Navigate  |  C-q:Quit"

-- | Handle events
handleEvent ::
  BrickEvent Name UIEvent ->
  EventM Name UIState ()
handleEvent (VtyEvent (V.EvKey key mods)) = do
  st <- get
  let connected = st ^. uiConnected

  case (key, mods) of
    -- Quit requires Ctrl-q
    (V.KChar 'q', [V.MCtrl]) -> halt
    -- Navigation (only in recall mode)
    (V.KRight, [])
      | not connected -> nextSlide
    (V.KChar 'n', [])
      | not connected -> nextSlide
    (V.KLeft, [])
      | not connected -> prevSlide
    (V.KChar 'p', [])
      | not connected -> prevSlide
    _ -> pure ()
handleEvent (AppEvent (SlideChangedEvent idx slide)) = do
  modify $ uiCurrentSlide .~ idx
  modify $ uiCurrentNote .~ (slide ^. slideNotes)
handleEvent (AppEvent ConnectionLost) = do
  modify $ uiConnected .~ False
  modify $ uiClient .~ Nothing
handleEvent _ = pure ()

-- | Move to next slide (recall mode) - uses safe indexing
nextSlide :: EventM Name UIState ()
nextSlide = modify $ \st ->
  let pres = st ^. uiPresentation
      maxIdx = length (pres ^. presSlides) - 1
      newIdx = min maxIdx (st ^. uiCurrentSlide + 1)
      -- Safe access with ix - no more bounds checks
      newNote = pres ^? presSlides . ix newIdx . slideNotes & maybe "" id
   in st & uiCurrentSlide .~ newIdx & uiCurrentNote .~ newNote

-- | Move to previous slide (recall mode) - uses safe indexing
prevSlide :: EventM Name UIState ()
prevSlide = modify $ \st ->
  let pres = st ^. uiPresentation
      newIdx = max 0 (st ^. uiCurrentSlide - 1)
      -- Safe access with ix
      newNote = pres ^? presSlides . ix newIdx . slideNotes & maybe "" id
   in st & uiCurrentSlide .~ newIdx & uiCurrentNote .~ newNote
