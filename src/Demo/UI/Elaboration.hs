{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Demo.UI.Elaboration
  ( -- * Running the UI
    runElaborationUI
  , ElaborationConfig (..)
  ) where

import Brick
  ( App (..)
  , BrickEvent (..)
  , EventM
  , Padding (..)
  , Widget
  , customMain
  , get
  , halt
  , hBox
  , modify
  , padAll
  , padLeft
  , txt
  , vBox
  , vLimit
  , withAttr
  , withBorderStyle
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (hCenter)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Demo.Core.Types (Elaboration (..), Presentation, Slide, elabCaption, elabEndLine, elabFile, elabStartLine, presSlides, slideElaborations)
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
import Control.Lens ((.~), (^.), (^?), ix)
import Lens.Micro.TH (makeLenses)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>), isAbsolute)
import System.IO.Error (catchIOError)

-- | Configuration for the elaboration UI
data ElaborationConfig = ElaborationConfig
  { ecPresPath :: FilePath
  , ecPresentation :: Presentation
  , ecProjectRoot :: FilePath
  , ecPresDir :: FilePath
  }

-- | Internal UI state
data UIState = UIState
  { _uiCurrentSlide :: Int
  , _uiElaborations :: [Elaboration]
  , _uiCurrentElab :: Int
  , _uiFileContent :: Text
  , _uiPresentation :: Presentation
  , _uiConnected :: Bool
  , _uiClient :: Maybe IPCClient
  , _uiProjectRoot :: FilePath
  , _uiPresDir :: FilePath
  }

makeLenses ''UIState

-- | Custom events
data UIEvent
  = SlideChangedEvent Int Slide
  | ElaborationsReceived [Elaboration]
  | ConnectionLost
  deriving stock (Show)

-- | Resource names
data Name = CodePane | CaptionPane
  deriving stock (Show, Eq, Ord)

-- | Run the elaboration UI
runElaborationUI :: ElaborationConfig -> IO ()
runElaborationUI config = do
  let pres = ecPresentation config
      projectRoot = ecProjectRoot config
      presDir = ecPresDir config
      firstElabs = case pres ^. presSlides of
        [] -> []
        (s : _) -> s ^. slideElaborations

  fileContent <- case firstElabs of
    [] -> pure ""
    (e : _) -> loadFileFragmentWithPaths projectRoot presDir e

  let initialState =
        UIState
          { _uiCurrentSlide = 0
          , _uiElaborations = firstElabs
          , _uiCurrentElab = 0
          , _uiFileContent = fileContent
          , _uiPresentation = pres
          , _uiConnected = False
          , _uiClient = Nothing
          , _uiProjectRoot = projectRoot
          , _uiPresDir = presDir
          }

  -- Create event channel
  eventChan <- newBChan 100

  -- Try to connect to slides server
  let sockPath = socketPathForPresentation (ecPresPath config)
  result <- connectToServer sockPath ElaborationClient
  stateWithClient <- case result of
    Left _ -> pure initialState
    Right client -> do
      setMessageHandler client $ \msg -> case msg of
        MsgSlideChanged idx slide ->
          writeBChan eventChan (SlideChangedEvent idx slide)
        MsgElaborations elabs ->
          writeBChan eventChan (ElaborationsReceived elabs)
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
    customMain initialVty buildVty (Just eventChan) elabApp stateWithClient

  -- Cleanup
  case finalState ^. uiClient of
    Just client -> disconnectFromServer client
    Nothing -> pure ()

-- | The brick application
elabApp :: App UIState UIEvent Name
elabApp =
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
  elabs = st ^. uiElaborations
  currentIdx = st ^. uiCurrentElab
  content = st ^. uiFileContent
  connected = st ^. uiConnected
  -- Safe indexing with ix
  currentElab = elabs ^? ix currentIdx

  ui =
    withBorderStyle unicode $
      vBox
        [ headerWidget currentElab (currentIdx + 1) (length elabs) connected
        , codeWidget content currentElab
        , footerWidget'
        ]

-- | Header widget
headerWidget :: Maybe Elaboration -> Int -> Int -> Bool -> Widget Name
headerWidget mElab idx total connected =
  withAttr headerAttr $
    hBox
      [ padLeft (Pad 1) $
          txt $
            case mElab of
              Just e -> T.pack (e ^. elabFile)
              Nothing -> "(no file)"
      , txt $ " " <> slideProgress (idx - 1) total <> connectionStatus connected
      ]

-- | Code display widget
codeWidget :: Text -> Maybe Elaboration -> Widget Name
codeWidget content mElab =
  borderWithLabel (txt $ " " <> caption <> " ") $
    vLimit 30 $
      padAll 1 $
        txt $
          if T.null content then "(no content)" else content
 where
  caption = case mElab of
    Just e -> e ^. elabCaption
    Nothing -> "Code"

-- | Footer widget
footerWidget' :: Widget Name
footerWidget' =
  withAttr footerAttr $
    hCenter $
      txt "←/→:Navigate fragments  |  C-q:Quit"

-- | Handle events
handleEvent ::
  BrickEvent Name UIEvent ->
  EventM Name UIState ()
handleEvent (VtyEvent (V.EvKey key mods)) = case (key, mods) of
  -- Quit requires Ctrl-q
  (V.KChar 'q', [V.MCtrl]) -> halt
  -- Navigation
  (V.KRight, []) -> nextElab
  (V.KChar 'n', []) -> nextElab
  (V.KLeft, []) -> prevElab
  (V.KChar 'p', []) -> prevElab
  _ -> pure ()
handleEvent (AppEvent (SlideChangedEvent idx slide)) = do
  modify $ uiCurrentSlide .~ idx
  modify $ uiElaborations .~ (slide ^. slideElaborations)
  modify $ uiCurrentElab .~ 0
  loadCurrentElab
handleEvent (AppEvent (ElaborationsReceived elabs)) = do
  modify $ uiElaborations .~ elabs
  modify $ uiCurrentElab .~ 0
  loadCurrentElab
handleEvent (AppEvent ConnectionLost) = do
  modify $ uiConnected .~ False
  modify $ uiClient .~ Nothing
handleEvent _ = pure ()

-- | Move to next elaboration
nextElab :: EventM Name UIState ()
nextElab = do
  st <- get
  let elabs = st ^. uiElaborations
      maxIdx = length elabs - 1
      newIdx = min maxIdx (st ^. uiCurrentElab + 1)
  modify $ uiCurrentElab .~ newIdx
  loadCurrentElab

-- | Move to previous elaboration
prevElab :: EventM Name UIState ()
prevElab = do
  st <- get
  let newIdx = max 0 (st ^. uiCurrentElab - 1)
  modify $ uiCurrentElab .~ newIdx
  loadCurrentElab

-- | Load content for current elaboration - uses safe indexing
loadCurrentElab :: EventM Name UIState ()
loadCurrentElab = do
  st <- get
  let elabs = st ^. uiElaborations
      idx = st ^. uiCurrentElab
      projectRoot = st ^. uiProjectRoot
      presDir = st ^. uiPresDir
  case elabs ^? ix idx of
    Just elab -> do
      content <- liftIO $ loadFileFragmentWithPaths projectRoot presDir elab
      modify $ uiFileContent .~ content
    Nothing -> pure ()

-- | Resolve a file path by trying multiple locations
-- Order of precedence:
--   1. Absolute paths are used as-is
--   2. Relative to project root
--   3. Relative to presentation directory
--   4. Relative to current working directory
resolveFilePath :: FilePath -> FilePath -> FilePath -> IO FilePath
resolveFilePath projectRoot presDir relPath
  | isAbsolute relPath = pure relPath
  | otherwise = do
      cwd <- getCurrentDirectory
      let candidates =
            [ projectRoot </> relPath       -- Relative to project root
            , presDir </> relPath           -- Relative to presentation dir
            , cwd </> relPath               -- Relative to cwd
            ]
      findFirst relPath candidates
 where
  findFirst :: FilePath -> [FilePath] -> IO FilePath
  findFirst fallback [] = pure fallback
  findFirst fallback (p:ps) = do
    exists <- doesFileExist p
    if exists then pure p else findFirst fallback ps

-- | Load a file fragment based on elaboration with path resolution
loadFileFragmentWithPaths :: FilePath -> FilePath -> Elaboration -> IO Text
loadFileFragmentWithPaths projectRoot presDir elab = do
  let file = elab ^. elabFile
      startLine = elab ^. elabStartLine
      endLine = elab ^. elabEndLine

  resolvedPath <- resolveFilePath projectRoot presDir file
  exists <- doesFileExist resolvedPath

  if not exists
    then pure $ "(file not found: " <> T.pack file <> ")"
    else do
      content <- TIO.readFile resolvedPath `catchIOError` const (pure "")
      let linesContent = T.lines content
          -- Extract lines (1-indexed)
          startIdx = max 0 (startLine - 1)
          endIdx = min (length linesContent) endLine
          selectedLines = take (endIdx - startIdx) (drop startIdx linesContent)
          -- Add line numbers
          numberedLines =
            zipWith
              (\n l -> T.pack (show n) <> " | " <> l)
              [startLine ..]
              selectedLines
      pure $ T.unlines numberedLines
