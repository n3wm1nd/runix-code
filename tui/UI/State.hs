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
  , waitForUserInput
  , provideUserInput
  , requestCancelFromUI
  , readCancellationFlag
  , clearCancellationFlag
  ) where

import Brick (Widget)
import qualified Brick.BChan
import Control.Concurrent.STM
import Data.Text (Text)
import Runix.Logging.Effects (Level(..))
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
  | UserMessageEvent msg          -- ^ User message received (before agent processes it)
  | AgentCompleteEvent [msg]      -- ^ Agent completed with new messages
  | AgentErrorEvent Text          -- ^ Agent encountered error
  | LogEvent Level Text           -- ^ New log entry
  | ToolExecutionEvent Text       -- ^ Tool execution started
  | ShowInputWidgetEvent SomeInputWidget  -- ^ Show an input widget
  | ClearInputWidgetEvent         -- ^ Clear the current input widget

-- Manual Eq instance (SomeInputWidget can't derive Eq due to callback function)
instance Eq msg => Eq (AgentEvent msg) where
  StreamChunkEvent t1 == StreamChunkEvent t2 = t1 == t2
  StreamReasoningEvent t1 == StreamReasoningEvent t2 = t1 == t2
  UserMessageEvent m1 == UserMessageEvent m2 = m1 == m2
  AgentCompleteEvent ms1 == AgentCompleteEvent ms2 = ms1 == ms2
  AgentErrorEvent t1 == AgentErrorEvent t2 = t1 == t2
  LogEvent l1 t1 == LogEvent l2 t2 = l1 == l2 && t1 == t2
  ToolExecutionEvent t1 == ToolExecutionEvent t2 = t1 == t2
  ShowInputWidgetEvent _ == ShowInputWidgetEvent _ = False  -- Can't compare functions
  ClearInputWidgetEvent == ClearInputWidgetEvent = True
  _ == _ = False

-- Manual Show instance
instance Show msg => Show (AgentEvent msg) where
  show (StreamChunkEvent t) = "StreamChunkEvent " ++ show t
  show (StreamReasoningEvent t) = "StreamReasoningEvent " ++ show t
  show (UserMessageEvent m) = "UserMessageEvent " ++ show m
  show (AgentCompleteEvent ms) = "AgentCompleteEvent " ++ show ms
  show (AgentErrorEvent t) = "AgentErrorEvent " ++ show t
  show (LogEvent l t) = "LogEvent " ++ show l ++ " " ++ show t
  show (ToolExecutionEvent t) = "ToolExecutionEvent " ++ show t
  show (ShowInputWidgetEvent _) = "ShowInputWidgetEvent <widget>"
  show ClearInputWidgetEvent = "ClearInputWidgetEvent"

-- | Shared state variables for UI communication
-- Parametrized over message type to work with typed events
data UIVars msg = UIVars
  { agentEventChan :: AgentEvent msg -> IO ()  -- ^ Callback to send events to UI
  , userInputQueue :: TQueue Text         -- ^ User input queue (UI writes, agent reads)
  , cancellationFlag :: TVar Bool         -- ^ Cancellation flag (UI writes, agent reads)
  }

-- Note: userResponseQueue is managed per-request in SomeInputWidget callback

-- | Create fresh UI state variables
-- Takes a callback to send agent events
newUIVars :: (AgentEvent msg -> IO ()) -> IO (UIVars msg)
newUIVars sendEventCallback = do
  inputQueue <- newTQueueIO
  cancelFlag <- newTVarIO False
  return $ UIVars sendEventCallback inputQueue cancelFlag

--------------------------------------------------------------------------------
-- Event API
--------------------------------------------------------------------------------

-- | Send an agent event to the UI thread
-- Uses the callback provided during initialization
sendAgentEvent :: UIVars msg -> AgentEvent msg -> IO ()
sendAgentEvent vars event =
  agentEventChan vars event

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
