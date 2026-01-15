{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Demo.Core.Types
  ( -- * Presentation Types
    Presentation (..)
  , Slide (..)
  , Command (..)
  , Elaboration (..)
  , Direction (..)

    -- * Lenses for Presentation
  , presName
  , presSlides

    -- * Lenses for Slide
  , slideTitle
  , slideCommands
  , slideNotes
  , slideElaborations

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

    -- * Mode
  , Mode (..)

    -- * Uniplate instances
  , allCommands
  , allElaborations
  , transformCommands
  ) where

import Control.Lens (Lens', Prism', lens, makeLenses, makePrisms, prism')
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

-- Generate lenses
makeLenses ''Presentation
makeLenses ''Slide
makeLenses ''Elaboration

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
