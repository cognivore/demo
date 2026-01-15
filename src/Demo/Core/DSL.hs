{-# LANGUAGE DeriveFunctor #-}

module Demo.Core.DSL
  ( -- * Presentation Builder
    PresentationBuilder
  , SlideBuilder
  , mkPresentation

    -- * Slide Combinators
  , slide
  , note
  , system
  , ghci
  , ghciTyped
  , elaborate

    -- * Free Monad DSL (for extensibility)
  , SlideF (..)
  , PresentationF (..)
  , interpretSlide
  , interpretPresentation

    -- * Re-exports
  , Presentation (..)
  , Slide (..)
  , Command (..)
  , Elaboration (..)
  ) where

import Control.Lens ((&), (%~), (.~))
import Control.Monad.Free (Free (..))
import Control.Monad.State.Strict (State, execState, modify')
import Data.Text (Text)
import Demo.Core.Types

-- | Functor for slide building operations
data SlideF next
  = SetNote Text next
  | AddSystem Text next
  | AddGhci Text (Maybe Text) next
  | AddElaborate FilePath Int Int Text next
  deriving stock (Functor)

-- | Functor for presentation building operations
data PresentationF next
  = AddSlide Text (Free SlideF ()) next
  deriving stock (Functor)

-- | Interpret a free slide builder into a Slide
interpretSlide :: Text -> Free SlideF () -> Slide
interpretSlide title = finalize . go (emptySlide title)
 where
  go s (Pure ())  = s
  go s (Free op)  = case op of
    SetNote t k          -> go (s & slideNotes .~ t) k
    AddSystem c k        -> go (s & slideCommands %~ (SystemCmd c:)) k
    AddGhci e t k        -> go (s & slideCommands %~ (GhciCmd e t:)) k
    AddElaborate f a b c k -> go (s & slideElaborations %~ (Elaboration f a b c:)) k

-- | Empty slide with title
emptySlide :: Text -> Slide
emptySlide t = Slide t [] "" []

-- | Finalize slide by reversing accumulated lists
finalize :: Slide -> Slide
finalize s = s & slideCommands %~ reverse & slideElaborations %~ reverse

-- | Interpret a free presentation builder into a Presentation
interpretPresentation :: Text -> Free PresentationF () -> Presentation
interpretPresentation name = Presentation name . reverse . go []
 where
  go acc (Pure ()) = acc
  go acc (Free (AddSlide t b k)) = go (interpretSlide t b : acc) k

-- | State-based builder monad for constructing presentations (original API)
type PresentationBuilder = State [Slide]

-- | State-based builder monad for constructing slides (original API)
type SlideBuilder = State Slide

-- | Create a presentation from a builder
mkPresentation :: Text -> PresentationBuilder () -> Presentation
mkPresentation name = Presentation name . reverse . flip execState []

-- | Add a slide to the presentation
slide :: Text -> SlideBuilder () -> PresentationBuilder ()
slide title builder = modify' (finalize (execState builder (emptySlide title)) :)

-- | Set the note for the current slide
note :: Text -> SlideBuilder ()
note = modify' . (slideNotes .~)

-- | Add a system command to the current slide
system :: Text -> SlideBuilder ()
system = modify' . (slideCommands %~) . (:) . SystemCmd

-- | Add a GHCI command to the current slide
ghci :: Text -> SlideBuilder ()
ghci e = modify' $ slideCommands %~ (GhciCmd e Nothing :)

-- | Add a typed GHCI command to the current slide
ghciTyped :: Text -> Text -> SlideBuilder ()
ghciTyped e t = modify' $ slideCommands %~ (GhciCmd e (Just t) :)

-- | Add an elaboration (code fragment reference) to the current slide
elaborate :: FilePath -> (Int, Int) -> Text -> SlideBuilder ()
elaborate f (a, b) c = modify' $ slideElaborations %~ (Elaboration f a b c :)
