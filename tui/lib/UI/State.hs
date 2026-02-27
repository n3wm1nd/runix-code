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
  , LLMSettings(..)
  , UserRequest(..)
  , newUIVars
  , sendAgentEvent
  , waitForUserInput
  , provideUserInput
  , requestCancelFromUI
  , readCancellationFlag
  , clearCancellationFlag
  ) where

import Control.Concurrent.STM
import Data.Text (Text)
import Runix.Logging (Level)
import UI.UserInput.InputWidget (InputWidget(..))
import UI.OutputHistory (Zipper, OutputItem)

-- | Resource names for widgets (defined here to avoid circular dependency)
data Name = InputEditor
          | HistoryViewport
          | CachedFront       -- Cache for front (older) messages in zipper
          | CachedCurrent     -- Cache for current (focused) message in zipper
          | CachedBack        -- Cache for back (newer) messages in zipper
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
  = ZipperUpdateEvent (Zipper (OutputItem msg))  -- ^ Full zipper update from agent
  | UserMessageEvent msg          -- ^ User message received (before agent processes it)
  | AgentCompleteEvent [msg]      -- ^ Agent completed with new messages
  | LogEvent Level Text           -- ^ Log event (for UI infrastructure, not agent output)
  | ShowInputWidgetEvent SomeInputWidget  -- ^ Show an input widget
  | ClearInputWidgetEvent         -- ^ Clear the current input widget
  | RunExternalCommandEvent (IO ())  -- ^ Run external command (suspends/resumes Vty)

-- Manual Eq instance (SomeInputWidget can't derive Eq due to callback function)
instance Eq msg => Eq (AgentEvent msg) where
  ZipperUpdateEvent z1 == ZipperUpdateEvent z2 = z1 == z2
  UserMessageEvent m1 == UserMessageEvent m2 = m1 == m2
  AgentCompleteEvent ms1 == AgentCompleteEvent ms2 = ms1 == ms2
  LogEvent l1 t1 == LogEvent l2 t2 = l1 == l2 && t1 == t2
  ShowInputWidgetEvent _ == ShowInputWidgetEvent _ = False  -- Can't compare functions
  ClearInputWidgetEvent == ClearInputWidgetEvent = True
  RunExternalCommandEvent _ == RunExternalCommandEvent _ = False  -- Can't compare IO actions
  _ == _ = False

-- Manual Show instance
instance Show msg => Show (AgentEvent msg) where
  show (ZipperUpdateEvent _) = "ZipperUpdateEvent <zipper>"
  show (UserMessageEvent m) = "UserMessageEvent " ++ show m
  show (AgentCompleteEvent ms) = "AgentCompleteEvent " ++ show ms
  show (LogEvent l t) = "LogEvent " ++ show l ++ " " ++ show t
  show (ShowInputWidgetEvent _) = "ShowInputWidgetEvent <widget>"
  show ClearInputWidgetEvent = "ClearInputWidgetEvent"
  show (RunExternalCommandEvent _) = "RunExternalCommandEvent <action>"

-- | Runtime LLM configuration settings
data LLMSettings = LLMSettings
  { llmStreaming :: Bool
  } deriving stock (Eq, Show)

-- | User request containing input text, history, and LLM settings
data UserRequest msg = UserRequest
  { userText :: Text
  , currentHistory :: [msg]
  , requestSettings :: LLMSettings
  } deriving stock (Eq, Show)

-- | Shared state variables for UI communication
-- Parametrized over message type to work with typed events
data UIVars msg = UIVars
  { agentEventChan :: AgentEvent msg -> IO ()  -- ^ Callback to send events to UI
  , userInputQueue :: TQueue (UserRequest msg)  -- ^ User input queue (UI writes, agent reads)
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
waitForUserInput :: TQueue (UserRequest msg) -> STM (UserRequest msg)
waitForUserInput = readTQueue

-- | Provide user input (from UI thread)
provideUserInput :: TQueue (UserRequest msg) -> UserRequest msg -> STM ()
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
