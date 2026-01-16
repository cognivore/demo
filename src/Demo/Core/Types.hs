{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.Core.Types
  ( -- * Presentation Types
    Presentation (..)
  , Slide (..)
  , Command (..)
  , Elaboration (..)
  , Direction (..)
  , SystemPrelude (..)
  , NixDevelopMode (..)
  , DirenvMode (..)
  , ExecMode (..)
  , defaultSystemPrelude

    -- * Lenses for Presentation
  , presName
  , presSlides
  , presPrelude

    -- * Lenses for Slide
  , slideTitle
  , slideCommands
  , slideNotes
  , slideElaborations

    -- * Lenses for System Prelude
  , spCwd
  , spProvision
  , spNixDevelop
  , spDirenv
  , spExecMode

    -- * Lenses for Elaboration
  , elabFile
  , elabStartLine
  , elabEndLine
  , elabCaption

    -- * Command Prisms
  , _SystemCmd
  , _GhciCmd

    -- * Runtime State
  , VarStore (..)
  , emptyVarStore
  , insertVar
  , lookupVar
  , nextVarIndex

    -- * Slide State
  , SlideState (..)
  , initialSlideState
  , ssCurrentSlide
  , ssCurrentCommand
  , ssVarStore
  , ssOutputBuffer
  , ssMode
  , ssPresentation
  , ssPreludeRan

    -- * Mode
  , Mode (..)

    -- * Uniplate instances
  , allCommands
  , allElaborations
  , transformCommands
  ) where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi, transformBi)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A complete presentation
data Presentation = Presentation
  { _presName :: Text
  , _presSlides :: [Slide]
  , _presPrelude :: SystemPrelude
  }
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | A single slide in the presentation
data Slide = Slide
  { _slideTitle :: Text
  , _slideCommands :: [Command]
  , _slideNotes :: Text
  , _slideElaborations :: [Elaboration]
  }
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Commands that can be executed on a slide
data Command
  = SystemCmd Text
  | GhciCmd Text (Maybe Text)
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Reference to a code fragment for elaboration
data Elaboration = Elaboration
  { _elabFile :: FilePath
  , _elabStartLine :: Int
  , _elabEndLine :: Int
  , _elabCaption :: Text
  }
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Navigation direction
data Direction = Prev | Next
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Whether to use nix develop for system commands
data NixDevelopMode
  = NixDevelopAuto (Maybe Text)
  | NixDevelopOn (Maybe Text)
  | NixShellAuto (Maybe Text)
  | NixShellOn (Maybe Text)
  | NixDevelopOff
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Whether to apply direnv for system commands
data DirenvMode = DirenvAuto | DirenvOn | DirenvOff
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | How to execute system commands
data ExecMode = ExecInline | ExecTempScript
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | System prelude for provisioning and execution context
data SystemPrelude = SystemPrelude
  { _spCwd :: Maybe FilePath
  , _spProvision :: [Text]
  , _spNixDevelop :: NixDevelopMode
  , _spDirenv :: DirenvMode
  , _spExecMode :: ExecMode
  }
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Default system prelude (auto nix develop if flake exists)
defaultSystemPrelude :: SystemPrelude
defaultSystemPrelude =
  SystemPrelude
    { _spCwd = Nothing
    , _spProvision = []
    , _spNixDevelop = NixDevelopAuto Nothing
    , _spDirenv = DirenvOff
    , _spExecMode = ExecInline
    }

-- Generate lenses
makeLenses ''Presentation
makeLenses ''Slide
makeLenses ''Elaboration
makeLenses ''SystemPrelude

-- Generate prisms for Command
makePrisms ''Command

-- | Store for variable values from GHCI evaluations
newtype VarStore = VarStore {unVarStore :: IntMap Value}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Create an empty variable store
emptyVarStore :: VarStore
emptyVarStore = VarStore IM.empty

-- | Insert a variable at a specific index
insertVar :: Int -> Value -> VarStore -> VarStore
insertVar idx val (VarStore m) = VarStore (IM.insert idx val m)

-- | Look up a variable by index
lookupVar :: Int -> VarStore -> Maybe Value
lookupVar idx (VarStore m) = IM.lookup idx m

-- | Get the next available variable index
nextVarIndex :: VarStore -> Int
nextVarIndex (VarStore m)
  | IM.null m = 1
  | otherwise = fst (IM.findMax m) + 1

-- | Current mode of the slides interface
data Mode = SystemMode | GhciMode
  deriving stock (Show, Eq, Generic, Data, Typeable)
  deriving anyclass (ToJSON, FromJSON)

-- | Runtime state for the slides process
data SlideState = SlideState
  { _ssCurrentSlide :: Int
  , _ssCurrentCommand :: Int
  , _ssVarStore :: VarStore
  , _ssOutputBuffer :: Text
  , _ssMode :: Mode
  , _ssPresentation :: Presentation
  , _ssPreludeRan :: Bool
  }
  deriving stock (Show, Eq)

makeLenses ''SlideState

-- | Create initial state from a presentation
initialSlideState :: Presentation -> SlideState
initialSlideState pres =
  SlideState
    { _ssCurrentSlide = 0
    , _ssCurrentCommand = 0
    , _ssVarStore = emptyVarStore
    , _ssOutputBuffer = ""
    , _ssMode = SystemMode
    , _ssPresentation = pres
    , _ssPreludeRan = False
    }

-- | Get all commands in a presentation using Uniplate
allCommands :: Presentation -> [Command]
allCommands = universeBi

-- | Get all elaborations in a presentation using Uniplate
allElaborations :: Presentation -> [Elaboration]
allElaborations = universeBi

-- | Transform all commands in a presentation using Uniplate
transformCommands :: (Command -> Command) -> Presentation -> Presentation
transformCommands = transformBi
