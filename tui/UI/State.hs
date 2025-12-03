{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

-- | STM-based UI state for concurrent UI updates
--
-- This module defines the shared state between the effect interpreter stack
-- and the brick UI thread. Uses a zipper-based history and event queue for
-- clean separation between agent and UI threads.
module UI.State
  ( UIVars(..)
  , Name(..)
  , AgentEvent(..)
  , SomeInputWidget(..)
  , newUIVars
  , sendAgentEvent
  , readAgentEvents
  , waitForUserInput
  , provideUserInput
  , requestCancelFromUI
  , readCancellationFlag
  , clearCancellationFlag
  ) where

import Brick (Widget)
import Control.Concurrent.STM
import Data.Text (Text)
import UI.OutputHistory (LogLevel(..))
import UI.UserInput.InputWidget (InputWidget(..))

-- | Resource names for widgets (defined here to avoid circular dependency)
data Name = InputEditor | HistoryViewport | CompletedHistory
  deriving stock (Eq, Ord, Show)

-- | Existential wrapper for input widgets of any type
-- This allows us to store a typed widget without knowing its type
data SomeInputWidget where
  SomeInputWidget
    :: InputWidget a
    => Text              -- ^ Prompt text
    -> a                 -- ^ Current value
    -> (Maybe a -> IO ()) -- ^ Callback: Just value = confirmed, Nothing = cancelled
    -> SomeInputWidget

-- | Events sent from agent thread to UI thread
-- Note: Events carry typed messages when relevant
data AgentEvent msg
  = StreamChunkEvent Text         -- ^ New streaming chunk
  | StreamReasoningEvent Text     -- ^ New streaming reasoning chunk
  | AgentCompleteEvent [msg]      -- ^ Agent completed with new messages
  | AgentErrorEvent Text          -- ^ Agent encountered error
  | LogEvent LogLevel Text        -- ^ New log entry
  | ToolExecutionEvent Text       -- ^ Tool execution started
  | ShowInputWidgetEvent SomeInputWidget  -- ^ Show an input widget
  | ClearInputWidgetEvent         -- ^ Clear the current input widget

-- | Shared state variables for UI communication
-- Parametrized over message type to work with typed events
-- STM is ONLY used for communication queues
data UIVars msg = UIVars
  { agentEventQueue :: TQueue (AgentEvent msg)  -- ^ Events from agent to UI
  , userInputQueue :: TQueue Text         -- ^ User input queue (UI writes, agent reads)
  , refreshSignal :: IO ()                -- ^ Callback to trigger UI refresh
  , cancellationFlag :: TVar Bool         -- ^ Cancellation flag (UI writes, agent reads)
  }

-- Note: userResponseQueue is managed per-request in SomeInputWidget callback

-- | Create fresh UI state variables (communication queues only)
-- The refresh callback will be set later by the UI
newUIVars :: IO () -> IO (UIVars msg)
newUIVars refreshCallback = do
  eventQueue <- newTQueueIO
  inputQueue <- newTQueueIO
  cancelFlag <- newTVarIO False
  return $ UIVars eventQueue inputQueue refreshCallback cancelFlag

--------------------------------------------------------------------------------
-- New Event-Based API
--------------------------------------------------------------------------------

-- | Send an agent event to the UI thread
sendAgentEvent :: UIVars msg -> AgentEvent msg -> IO ()
sendAgentEvent vars event = do
  atomically $ writeTQueue (agentEventQueue vars) event
  refreshSignal vars

-- | Read all pending agent events (non-blocking)
readAgentEvents :: UIVars msg -> STM [AgentEvent msg]
readAgentEvents vars = go []
  where
    go acc = do
      isEmpty <- isEmptyTQueue (agentEventQueue vars)
      if isEmpty
        then return (reverse acc)
        else do
          event <- readTQueue (agentEventQueue vars)
          go (event:acc)

-- | Block until user provides input
waitForUserInput :: TQueue Text -> STM Text
waitForUserInput = readTQueue

-- | Provide user input (from UI thread)
provideUserInput :: TQueue Text -> Text -> STM ()
provideUserInput = writeTQueue

-- | Request cancellation from the UI thread (sets flag)
requestCancelFromUI :: UIVars msg -> STM ()
requestCancelFromUI vars = writeTVar (cancellationFlag vars) True

-- | Check the cancellation flag (non-blocking read)
readCancellationFlag :: UIVars msg -> STM Bool
readCancellationFlag vars = readTVar (cancellationFlag vars)

-- | Clear the cancellation flag after handling cancellation (so next request can proceed)
clearCancellationFlag :: UIVars msg -> IO ()
clearCancellationFlag vars = atomically $ writeTVar (cancellationFlag vars) False
