{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

-- | Zipper-based navigation for presentations
--
-- Zippers are comonads that provide a "focused" view into a data structure.
-- The 'extract' operation gives the current focus, and 'extend' propagates
-- context-dependent computations across the structure.
module Demo.Core.Zipper
  ( -- * Generic List Zipper
    ListZipper (..)
  , mkListZipper
  , zipperToList
  , zipperLength
  , zipperIndex
  , moveLeft
  , moveRight
  , moveToIndex
  , updateFocus
  , focusedMaybe

    -- * Presentation Zipper
  , PresentationZipper (..)
  , mkPresentationZipper
  , currentSlide
  , currentCommand
  , slideZipper
  , commandZipper
  , nextSlide
  , prevSlide
  , nextCommand
  , prevCommand
  , goToSlide
  , goToCommand
  , modifyCurrentSlide
  , modifyCurrentCommand
  , presentationFromZipper
  , slideIndex
  , commandIndex
  , totalSlides
  , totalCommands
  ) where

import Control.Comonad (Comonad (..))
import Data.List (unfoldr)
import GHC.Generics (Generic)

import Demo.Core.Types (Command, Presentation (..), Slide (..), presSlides)
import Control.Lens ((^.))

-- | A generic list zipper - provides O(1) access to current element
-- and O(1) movement in both directions.
--
-- Invariant: The zipper represents the list (reverse before ++ [focus] ++ after)
data ListZipper a = ListZipper
  { lzBefore :: [a]   -- ^ Elements before focus (in reverse order)
  , lzFocus :: a      -- ^ Current focused element
  , lzAfter :: [a]    -- ^ Elements after focus
  }
  deriving (Show, Eq, Functor, Generic)

-- | Comonad instance for ListZipper
--
-- The key insight is that a zipper gives us a "context" at each position.
-- 'extract' gives the focus, 'duplicate' gives a zipper of zippers where
-- each position contains the view from that position.
instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract = lzFocus

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate z = ListZipper lefts z rights
    where
      -- Generate all positions to the left
      lefts = unfoldr (fmap (\z' -> (z', z')) . moveLeft) z
      -- Generate all positions to the right
      rights = unfoldr (fmap (\z' -> (z', z')) . moveRight) z

-- | Create a zipper from a non-empty list, focusing on the first element
mkListZipper :: [a] -> Maybe (ListZipper a)
mkListZipper [] = Nothing
mkListZipper (x:xs) = Just $ ListZipper [] x xs

-- | Convert a zipper back to a list
zipperToList :: ListZipper a -> [a]
zipperToList (ListZipper before focus after) =
  reverse before ++ [focus] ++ after

-- | Get the total length of the zipper
zipperLength :: ListZipper a -> Int
zipperLength (ListZipper before _ after) = length before + 1 + length after

-- | Get the current index (0-based)
zipperIndex :: ListZipper a -> Int
zipperIndex (ListZipper before _ _) = length before

-- | Move focus to the left (previous element)
moveLeft :: ListZipper a -> Maybe (ListZipper a)
moveLeft (ListZipper [] _ _) = Nothing
moveLeft (ListZipper (b:bs) f as) = Just $ ListZipper bs b (f:as)

-- | Move focus to the right (next element)
moveRight :: ListZipper a -> Maybe (ListZipper a)
moveRight (ListZipper _ _ []) = Nothing
moveRight (ListZipper bs f (a:as)) = Just $ ListZipper (f:bs) a as

-- | Move to a specific index
moveToIndex :: Int -> ListZipper a -> Maybe (ListZipper a)
moveToIndex targetIdx z
  | targetIdx < 0 = Nothing
  | targetIdx >= zipperLength z = Nothing
  | currentIdx == targetIdx = Just z
  | currentIdx < targetIdx = moveRight z >>= moveToIndex targetIdx
  | otherwise = moveLeft z >>= moveToIndex targetIdx
  where
    currentIdx = zipperIndex z

-- | Update the focused element
updateFocus :: (a -> a) -> ListZipper a -> ListZipper a
updateFocus f (ListZipper bs x as) = ListZipper bs (f x) as

-- | Safe access to focus as Maybe (always Just for valid zippers)
focusedMaybe :: ListZipper a -> Maybe a
focusedMaybe = Just . lzFocus

--------------------------------------------------------------------------------
-- Presentation Zipper - Nested zipper for slides and commands
--------------------------------------------------------------------------------

-- | A presentation zipper with nested command focus
data PresentationZipper = PresentationZipper
  { pzPresentation :: Presentation
  , pzSlides :: ListZipper Slide
  , pzCommands :: Maybe (ListZipper Command)  -- Nothing if slide has no commands
  }
  deriving (Show, Eq, Generic)

-- | Create a presentation zipper, focusing on the first slide
mkPresentationZipper :: Presentation -> Maybe PresentationZipper
mkPresentationZipper pres = do
  slides <- mkListZipper (pres ^. presSlides)
  let cmds = mkListZipper (_slideCommands (lzFocus slides))
  pure $ PresentationZipper pres slides cmds

-- | Get the currently focused slide
currentSlide :: PresentationZipper -> Slide
currentSlide = extract . pzSlides

-- | Get the currently focused command (if any)
currentCommand :: PresentationZipper -> Maybe Command
currentCommand pz = extract <$> pzCommands pz

-- | Get the slide zipper
slideZipper :: PresentationZipper -> ListZipper Slide
slideZipper = pzSlides

-- | Get the command zipper (if any)
commandZipper :: PresentationZipper -> Maybe (ListZipper Command)
commandZipper = pzCommands

-- | Move to the next slide
nextSlide :: PresentationZipper -> Maybe PresentationZipper
nextSlide pz = do
  newSlides <- moveRight (pzSlides pz)
  let newCmds = mkListZipper (_slideCommands (lzFocus newSlides))
  pure $ pz { pzSlides = newSlides, pzCommands = newCmds }

-- | Move to the previous slide
prevSlide :: PresentationZipper -> Maybe PresentationZipper
prevSlide pz = do
  newSlides <- moveLeft (pzSlides pz)
  let newCmds = mkListZipper (_slideCommands (lzFocus newSlides))
  pure $ pz { pzSlides = newSlides, pzCommands = newCmds }

-- | Move to the next command within the current slide
nextCommand :: PresentationZipper -> Maybe PresentationZipper
nextCommand pz = do
  cmds <- pzCommands pz
  newCmds <- moveRight cmds
  pure $ pz { pzCommands = Just newCmds }

-- | Move to the previous command within the current slide
prevCommand :: PresentationZipper -> Maybe PresentationZipper
prevCommand pz = do
  cmds <- pzCommands pz
  newCmds <- moveLeft cmds
  pure $ pz { pzCommands = Just newCmds }

-- | Go to a specific slide by index
goToSlide :: Int -> PresentationZipper -> Maybe PresentationZipper
goToSlide idx pz = do
  newSlides <- moveToIndex idx (pzSlides pz)
  let newCmds = mkListZipper (_slideCommands (lzFocus newSlides))
  pure $ pz { pzSlides = newSlides, pzCommands = newCmds }

-- | Go to a specific command by index within current slide
goToCommand :: Int -> PresentationZipper -> Maybe PresentationZipper
goToCommand idx pz = do
  cmds <- pzCommands pz
  newCmds <- moveToIndex idx cmds
  pure $ pz { pzCommands = Just newCmds }

-- | Modify the current slide
modifyCurrentSlide :: (Slide -> Slide) -> PresentationZipper -> PresentationZipper
modifyCurrentSlide f pz =
  let newSlides = updateFocus f (pzSlides pz)
      newCmds = mkListZipper (_slideCommands (lzFocus newSlides))
  in pz { pzSlides = newSlides, pzCommands = newCmds }

-- | Modify the current command
modifyCurrentCommand :: (Command -> Command) -> PresentationZipper -> PresentationZipper
modifyCurrentCommand f pz = case pzCommands pz of
  Nothing -> pz
  Just cmds ->
    let newCmds = updateFocus f cmds
        -- Also update the slide to reflect the command change
        newSlide = (currentSlide pz) { _slideCommands = zipperToList newCmds }
        newSlides = updateFocus (const newSlide) (pzSlides pz)
    in pz { pzSlides = newSlides, pzCommands = Just newCmds }

-- | Reconstruct presentation from zipper (reflects any modifications)
presentationFromZipper :: PresentationZipper -> Presentation
presentationFromZipper pz =
  (pzPresentation pz) { _presSlides = zipperToList (pzSlides pz) }

-- | Get current slide index
slideIndex :: PresentationZipper -> Int
slideIndex = zipperIndex . pzSlides

-- | Get current command index (0 if no commands)
commandIndex :: PresentationZipper -> Int
commandIndex pz = maybe 0 zipperIndex (pzCommands pz)

-- | Get total number of slides
totalSlides :: PresentationZipper -> Int
totalSlides = zipperLength . pzSlides

-- | Get total number of commands in current slide
totalCommands :: PresentationZipper -> Int
totalCommands pz = maybe 0 zipperLength (pzCommands pz)
