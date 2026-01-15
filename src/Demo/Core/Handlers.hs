{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}

-- | Contravariant event handlers
--
-- Event handlers are consumers of events - they receive data and produce effects.
-- This is the dual of producers/generators, which makes them contravariant functors.
--
-- Contramap lets us adapt a handler for type B into a handler for type A
-- by providing a function A -> B.
module Demo.Core.Handlers
  ( -- * Handler Types
    Handler (..)
  , EventSink (..)

    -- * Handler Combinators
  , sinkHandler
  , nullHandler
  , mappingHandler
  , filteringHandler
  , combineHandlers
  , fanout

    -- * Predicate-based Handlers
  , Predicate (..)
  , andP
  , orP
  , notP
  , constP

    -- * Common Handlers
  , outputHandler
  , commandDoneHandler
  , slideChangeHandler

    -- * Divisible/Decidable instances
  , divided
  , conquered
  , chosen
  , lost
  ) where

import Brick.BChan (BChan, writeBChan)
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))
import Data.Text (Text)
import Data.Void (Void, absurd)

--------------------------------------------------------------------------------
-- Handler Type
--------------------------------------------------------------------------------

-- | A handler is a contravariant functor that consumes events
--
-- The key insight is that if we have a handler for B and a way to convert A to B,
-- we can create a handler for A. This is 'contramap'.
newtype Handler a = Handler { runHandler :: a -> IO () }

instance Contravariant Handler where
  contramap f (Handler h) = Handler (h . f)

-- | Divisible lets us combine handlers
instance Divisible Handler where
  divide :: (a -> (b, c)) -> Handler b -> Handler c -> Handler a
  divide f (Handler hb) (Handler hc) = Handler $ \a ->
    let (b, c) = f a
    in hb b >> hc c

  conquer :: Handler a
  conquer = Handler $ const (pure ())

-- | Decidable lets us choose between handlers
instance Decidable Handler where
  lose :: (a -> Void) -> Handler a
  lose f = Handler (absurd . f)

  choose :: (a -> Either b c) -> Handler b -> Handler c -> Handler a
  choose f (Handler hb) (Handler hc) = Handler $ \a ->
    case f a of
      Left b -> hb b
      Right c -> hc c

-- | Semigroup instance combines handlers
instance Semigroup (Handler a) where
  Handler h1 <> Handler h2 = Handler $ \a -> h1 a >> h2 a

-- | Monoid instance with null handler
instance Monoid (Handler a) where
  mempty = Handler $ const (pure ())

--------------------------------------------------------------------------------
-- Event Sink (BChan wrapper)
--------------------------------------------------------------------------------

-- | An event sink wrapping a BChan
newtype EventSink e = EventSink { getSink :: BChan e }

-- | Create a handler from an event sink and a mapping
sinkHandler :: EventSink e -> (a -> e) -> Handler a
sinkHandler (EventSink chan) f = Handler $ writeBChan chan . f


--------------------------------------------------------------------------------
-- Handler Combinators
--------------------------------------------------------------------------------

-- | A handler that does nothing
nullHandler :: Handler a
nullHandler = mempty

-- | Create a handler that maps input before processing
mappingHandler :: (a -> b) -> Handler b -> Handler a
mappingHandler = contramap

-- | Create a handler that only processes inputs satisfying a predicate
filteringHandler :: (a -> Bool) -> Handler a -> Handler a
filteringHandler p (Handler h) = Handler $ \a ->
  if p a then h a else pure ()

-- | Combine multiple handlers into one
combineHandlers :: [Handler a] -> Handler a
combineHandlers = mconcat

-- | Fan out to multiple handlers
fanout :: Handler a -> Handler a -> Handler a
fanout = (<>)

-- | Shorthand for 'divide'
divided :: (a -> (b, c)) -> Handler b -> Handler c -> Handler a
divided = divide

-- | Shorthand for 'conquer'
conquered :: Handler a
conquered = conquer

-- | Shorthand for 'choose'
chosen :: (a -> Either b c) -> Handler b -> Handler c -> Handler a
chosen = choose

-- | Shorthand for 'lose'
lost :: (a -> Void) -> Handler a
lost = lose

--------------------------------------------------------------------------------
-- Predicate (also contravariant)
--------------------------------------------------------------------------------

-- | A predicate is a contravariant functor
newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)

-- | Combine predicates with AND
andP :: Predicate a -> Predicate a -> Predicate a
andP (Predicate p1) (Predicate p2) = Predicate (liftA2 (&&) p1 p2)

-- | Combine predicates with OR
orP :: Predicate a -> Predicate a -> Predicate a
orP (Predicate p1) (Predicate p2) = Predicate (liftA2 (||) p1 p2)

-- | Negate a predicate
notP :: Predicate a -> Predicate a
notP (Predicate p) = Predicate (not . p)

-- | Constant predicate
constP :: Bool -> Predicate a
constP b = Predicate (const b)

instance Semigroup (Predicate a) where
  (<>) = andP

instance Monoid (Predicate a) where
  mempty = constP True

--------------------------------------------------------------------------------
-- Common UI Handlers
--------------------------------------------------------------------------------

-- | UI event types for handlers
data UIEvent
  = OutputReceived Text
  | CommandDone Int
  | SlideChanged Int
  | Connected
  | Disconnected
  deriving stock (Show, Eq)

-- | Create a handler for output events
outputHandler :: BChan e -> (Text -> e) -> Handler Text
outputHandler chan mkEvent = Handler $ writeBChan chan . mkEvent

-- | Create a handler for command completion
commandDoneHandler :: BChan e -> (Int -> e) -> Handler Int
commandDoneHandler chan mkEvent = Handler $ writeBChan chan . mkEvent

-- | Create a handler for slide changes
slideChangeHandler :: BChan e -> (Int -> e) -> Handler Int
slideChangeHandler chan mkEvent = Handler $ writeBChan chan . mkEvent
