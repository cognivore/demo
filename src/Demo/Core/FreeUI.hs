{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Free monad DSL for declarative UI actions
--
-- This module separates the description of UI effects from their interpretation,
-- enabling pure testing and flexible execution strategies.
module Demo.Core.FreeUI
  ( -- * Free Monad DSL
    UIActionF (..)
  , UIAction

    -- * Smart Constructors
  , runSystem
  , runGhci
  , updateOutput
  , appendOutput
  , broadcastMsg
  , forkAction
  , getState
  , modifyState
  , setRunning

    -- * Interpreters
  , Interpreter (..)
  , interpretIO
  , interpretPure

    -- * Kleisli Arrows for Streaming
  , StreamingExec
  , mkStreamingExec
  , runStreamingExec
  ) where

import Control.Arrow (Kleisli (..))
import Control.Monad.Free (Free (..), liftF)
import Data.Text (Text)

import Demo.Core.Types (VarStore)
import Demo.IPC.Protocol (IPCMessage)
import Demo.Interpreter.System (CommandResult (..))
import Demo.Interpreter.Ghci (GhciResult (..))

-- | The functor describing UI actions
--
-- Each constructor represents a single, atomic UI effect.
-- The continuation style (with callbacks) makes this a proper functor.
data UIActionF s next
  = RunSystem VarStore Text (CommandResult -> next)
    -- ^ Run a system command with variable substitution
  | RunGhci Text (GhciResult -> next)
    -- ^ Run a GHCI expression
  | UpdateOutput Text next
    -- ^ Set the output buffer
  | AppendOutput Text next
    -- ^ Append to the output buffer
  | Broadcast IPCMessage next
    -- ^ Broadcast a message to IPC clients
  | Fork (IO ()) next
    -- ^ Fork an IO action
  | GetState (s -> next)
    -- ^ Get the current state
  | ModifyState (s -> s) next
    -- ^ Modify the state
  | SetRunning Bool next
    -- ^ Set the running flag
  deriving stock (Functor)

-- | The free monad over UIActionF
type UIAction s = Free (UIActionF s)

--------------------------------------------------------------------------------
-- Smart Constructors
--------------------------------------------------------------------------------

-- | Run a system command
runSystem :: VarStore -> Text -> UIAction s CommandResult
runSystem store cmd = liftF $ RunSystem store cmd id

-- | Run a GHCI expression
runGhci :: Text -> UIAction s GhciResult
runGhci expr = liftF $ RunGhci expr id

-- | Set the output buffer
updateOutput :: Text -> UIAction s ()
updateOutput txt = liftF $ UpdateOutput txt ()

-- | Append to the output buffer
appendOutput :: Text -> UIAction s ()
appendOutput txt = liftF $ AppendOutput txt ()

-- | Broadcast a message to IPC clients
broadcastMsg :: IPCMessage -> UIAction s ()
broadcastMsg msg = liftF $ Broadcast msg ()

-- | Fork an IO action
forkAction :: IO () -> UIAction s ()
forkAction action = liftF $ Fork action ()

-- | Get the current state
getState :: UIAction s s
getState = liftF $ GetState id

-- | Modify the state
modifyState :: (s -> s) -> UIAction s ()
modifyState f = liftF $ ModifyState f ()

-- | Set the running flag
setRunning :: Bool -> UIAction s ()
setRunning b = liftF $ SetRunning b ()

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------

-- | An interpreter provides implementations for each action
data Interpreter s m = Interpreter
  { interpRunSystem :: VarStore -> Text -> m CommandResult
  , interpRunGhci :: Text -> m GhciResult
  , interpUpdateOutput :: Text -> m ()
  , interpAppendOutput :: Text -> m ()
  , interpBroadcast :: IPCMessage -> m ()
  , interpFork :: IO () -> m ()
  , interpGetState :: m s
  , interpModifyState :: (s -> s) -> m ()
  , interpSetRunning :: Bool -> m ()
  }

-- | Interpret a free monad program using the given interpreter
interpretIO :: Monad m => Interpreter s m -> UIAction s a -> m a
interpretIO _ (Pure a) = pure a
interpretIO interp (Free action) = case action of
  RunSystem store cmd k -> do
    result <- interpRunSystem interp store cmd
    interpretIO interp (k result)
  RunGhci expr k -> do
    result <- interpRunGhci interp expr
    interpretIO interp (k result)
  UpdateOutput txt k -> do
    interpUpdateOutput interp txt
    interpretIO interp k
  AppendOutput txt k -> do
    interpAppendOutput interp txt
    interpretIO interp k
  Broadcast msg k -> do
    interpBroadcast interp msg
    interpretIO interp k
  Fork io k -> do
    interpFork interp io
    interpretIO interp k
  GetState k -> do
    s <- interpGetState interp
    interpretIO interp (k s)
  ModifyState f k -> do
    interpModifyState interp f
    interpretIO interp k
  SetRunning b k -> do
    interpSetRunning interp b
    interpretIO interp k

-- | Pure interpreter for testing (collects actions)
data PureAction s
  = PureRunSystem VarStore Text
  | PureRunGhci Text
  | PureUpdateOutput Text
  | PureAppendOutput Text
  | PureBroadcast IPCMessage
  | PureFork
  | PureModifyState
  | PureSetRunning Bool
  deriving stock (Show, Eq)

-- | Interpret a free monad program purely, collecting actions
interpretPure :: s -> UIAction s a -> ([PureAction s], a)
interpretPure _ (Pure a) = ([], a)
interpretPure s (Free action) = case action of
  RunSystem store cmd k ->
    let (rest, a) = interpretPure s (k (mockResult cmd))
    in (PureRunSystem store cmd : rest, a)
  RunGhci expr k ->
    let (rest, a) = interpretPure s (k (mockGhciResult expr))
    in (PureRunGhci expr : rest, a)
  UpdateOutput txt k ->
    let (rest, a) = interpretPure s k
    in (PureUpdateOutput txt : rest, a)
  AppendOutput txt k ->
    let (rest, a) = interpretPure s k
    in (PureAppendOutput txt : rest, a)
  Broadcast msg k ->
    let (rest, a) = interpretPure s k
    in (PureBroadcast msg : rest, a)
  Fork _ k ->
    let (rest, a) = interpretPure s k
    in (PureFork : rest, a)
  GetState k ->
    interpretPure s (k s)
  ModifyState _ k ->
    let (rest, a) = interpretPure s k
    in (PureModifyState : rest, a)
  SetRunning b k ->
    let (rest, a) = interpretPure s k
    in (PureSetRunning b : rest, a)
  where
    mockResult _ = CommandResult 0 ""
    mockGhciResult _ = GhciError "Mock GHCI"

--------------------------------------------------------------------------------
-- Kleisli Arrows for Streaming Execution
--------------------------------------------------------------------------------

-- | A streaming executor as a Kleisli arrow
--
-- This models the flow: command text -> streaming output -> final result
-- The callback is used for intermediate streaming output.
type StreamingExec = Kleisli IO (VarStore, Text, Text -> IO ()) CommandResult

-- | Create a streaming executor
mkStreamingExec :: (VarStore -> Text -> (Text -> IO ()) -> IO CommandResult) -> StreamingExec
mkStreamingExec f = Kleisli $ \(store, cmd, callback) -> f store cmd callback

-- | Run a streaming executor
runStreamingExec :: StreamingExec -> VarStore -> Text -> (Text -> IO ()) -> IO CommandResult
runStreamingExec (Kleisli f) store cmd callback = f (store, cmd, callback)
