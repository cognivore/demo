{-# LANGUAGE DeriveFunctor #-}

module Demo.Core.DSL
  ( -- * Presentation Builder
    PresentationBuilder
  , SlideBuilder
  , PreludeBuilder
  , mkPresentation

    -- * Slide Combinators
  , slide
  , note
  , system
  , ghci
  , ghciTyped
  , elaborate

    -- * System Prelude
  , systemPrelude
  , preludeCwd
  , preludeSystem
  , preludeNixDevelop
  , preludeNixDevelopShell
  , preludeNixDevelopAuto
  , preludeNixDevelopAutoShell
  , preludeNoNixDevelop
  , preludeNixShell
  , preludeNixShellPackage
  , preludeNixShellAuto
  , preludeNixShellAutoPackage
  , preludeDirenv
  , preludeDirenvAuto
  , preludeNoDirenv
  , preludeExecInline
  , preludeExecTempScript

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
  | ModifyPrelude (SystemPrelude -> SystemPrelude) next
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
interpretPresentation name =
  finalizePresentation name . go defaultPresentationSpec
 where
  go acc (Pure ()) = acc
  go acc (Free (AddSlide t b k)) =
    let PresentationSpec slides prelude = acc
    in go (PresentationSpec (interpretSlide t b : slides) prelude) k
  go acc (Free (ModifyPrelude f k)) =
    let PresentationSpec slides prelude = acc
    in go (PresentationSpec slides (f prelude)) k

-- | State-based builder monad for constructing presentations (original API)
type PresentationBuilder = State PresentationSpec

-- | State-based builder monad for constructing slides (original API)
type SlideBuilder = State Slide

-- | State-based builder monad for configuring the system prelude
type PreludeBuilder = State SystemPrelude

-- | Internal presentation builder state
data PresentationSpec = PresentationSpec
  { psSlides :: [Slide]
  , psPrelude :: SystemPrelude
  }

-- | Empty presentation spec
defaultPresentationSpec :: PresentationSpec
defaultPresentationSpec =
  PresentationSpec
    { psSlides = []
    , psPrelude = defaultSystemPrelude
    }

-- | Finalize a presentation from spec
finalizePresentation :: Text -> PresentationSpec -> Presentation
finalizePresentation name (PresentationSpec slides prelude) =
  Presentation name (reverse slides) prelude

-- | Create a presentation from a builder
mkPresentation :: Text -> PresentationBuilder () -> Presentation
mkPresentation name = finalizePresentation name . flip execState defaultPresentationSpec

-- | Add a slide to the presentation
slide :: Text -> SlideBuilder () -> PresentationBuilder ()
slide title builder =
  modify' $
    \(PresentationSpec slides prelude) ->
      PresentationSpec
        (finalize (execState builder (emptySlide title)) : slides)
        prelude

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

--------------------------------------------------------------------------------
-- System Prelude (Provisioning + Execution Context)
--------------------------------------------------------------------------------

-- | Configure the system prelude for the presentation
systemPrelude :: PreludeBuilder () -> PresentationBuilder ()
systemPrelude builder =
  modify' $ \(PresentationSpec slides prelude) ->
    PresentationSpec slides (execState builder prelude)

-- | Set working directory for system commands
preludeCwd :: FilePath -> PreludeBuilder ()
preludeCwd path = modify' $ spCwd .~ Just path

-- | Add a provisioning command to run once before first system command
preludeSystem :: Text -> PreludeBuilder ()
preludeSystem cmd = modify' $ spProvision %~ (<> [cmd])

-- | Force nix develop usage (default shell)
preludeNixDevelop :: PreludeBuilder ()
preludeNixDevelop = modify' $ spNixDevelop .~ NixDevelopOn Nothing

-- | Force nix develop usage with a named dev shell
preludeNixDevelopShell :: Text -> PreludeBuilder ()
preludeNixDevelopShell shellName =
  modify' $ spNixDevelop .~ NixDevelopOn (Just shellName)

-- | Auto-detect nix develop based on flake presence (default shell)
preludeNixDevelopAuto :: PreludeBuilder ()
preludeNixDevelopAuto = modify' $ spNixDevelop .~ NixDevelopAuto Nothing

-- | Auto-detect nix develop with a named dev shell
preludeNixDevelopAutoShell :: Text -> PreludeBuilder ()
preludeNixDevelopAutoShell shellName =
  modify' $ spNixDevelop .~ NixDevelopAuto (Just shellName)

-- | Disable nix develop usage
preludeNoNixDevelop :: PreludeBuilder ()
preludeNoNixDevelop = modify' $ spNixDevelop .~ NixDevelopOff

-- | Force nix shell usage (default package)
preludeNixShell :: PreludeBuilder ()
preludeNixShell = modify' $ spNixDevelop .~ NixShellOn Nothing

-- | Force nix shell usage with a specific package
preludeNixShellPackage :: Text -> PreludeBuilder ()
preludeNixShellPackage packageName =
  modify' $ spNixDevelop .~ NixShellOn (Just packageName)

-- | Auto-detect nix shell usage (default package)
preludeNixShellAuto :: PreludeBuilder ()
preludeNixShellAuto = modify' $ spNixDevelop .~ NixShellAuto Nothing

-- | Auto-detect nix shell usage with a specific package
preludeNixShellAutoPackage :: Text -> PreludeBuilder ()
preludeNixShellAutoPackage packageName =
  modify' $ spNixDevelop .~ NixShellAuto (Just packageName)

-- | Force direnv usage
preludeDirenv :: PreludeBuilder ()
preludeDirenv = modify' $ spDirenv .~ DirenvOn

-- | Auto-detect direnv based on .envrc presence
preludeDirenvAuto :: PreludeBuilder ()
preludeDirenvAuto = modify' $ spDirenv .~ DirenvAuto

-- | Disable direnv usage
preludeNoDirenv :: PreludeBuilder ()
preludeNoDirenv = modify' $ spDirenv .~ DirenvOff

-- | Execute commands inline (default)
preludeExecInline :: PreludeBuilder ()
preludeExecInline = modify' $ spExecMode .~ ExecInline

-- | Execute commands via a temporary script file
preludeExecTempScript :: PreludeBuilder ()
preludeExecTempScript = modify' $ spExecMode .~ ExecTempScript
