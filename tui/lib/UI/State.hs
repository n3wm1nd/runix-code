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
  , CompletionRequest(..)
  , newUIVars
  , sendAgentEvent
  , waitForUserInput
  , provideUserInput
  , requestCancelFromUI
  , readCancellationFlag
  , clearCancellationFlag
  , requestCompletion
  , waitForCompletion
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
  | RestoreSessionEvent (Zipper (OutputItem msg))  -- ^ Restore session (UI only, doesn't affect AgentWidgets)
  | LogEvent Level Text           -- ^ Log event (for UI infrastructure, not agent output)
  | ShowInputWidgetEvent SomeInputWidget  -- ^ Show an input widget
  | ClearInputWidgetEvent         -- ^ Clear the current input widget
  | RunExternalCommandEvent (IO ())  -- ^ Run external command (suspends/resumes Vty)
  | StreamStartEvent Int          -- ^ Stream started (stream ID)
  | StreamChunkEvent Int Int      -- ^ Chunk received (stream ID, total chunks for this stream)
  | StreamEndEvent Int            -- ^ Stream completed successfully (stream ID)
  | StreamErrorEvent Int          -- ^ Stream failed with error (stream ID)

-- Manual Eq instance (SomeInputWidget can't derive Eq due to callback function)
instance Eq msg => Eq (AgentEvent msg) where
  ZipperUpdateEvent z1 == ZipperUpdateEvent z2 = z1 == z2
  UserMessageEvent m1 == UserMessageEvent m2 = m1 == m2
  AgentCompleteEvent ms1 == AgentCompleteEvent ms2 = ms1 == ms2
  RestoreSessionEvent z1 == RestoreSessionEvent z2 = z1 == z2
  LogEvent l1 t1 == LogEvent l2 t2 = l1 == l2 && t1 == t2
  ShowInputWidgetEvent _ == ShowInputWidgetEvent _ = False  -- Can't compare functions
  ClearInputWidgetEvent == ClearInputWidgetEvent = True
  RunExternalCommandEvent _ == RunExternalCommandEvent _ = False  -- Can't compare IO actions
  StreamStartEvent id1 == StreamStartEvent id2 = id1 == id2
  StreamChunkEvent id1 c1 == StreamChunkEvent id2 c2 = id1 == id2 && c1 == c2
  StreamEndEvent id1 == StreamEndEvent id2 = id1 == id2
  StreamErrorEvent id1 == StreamErrorEvent id2 = id1 == id2
  _ == _ = False

-- Manual Show instance
instance Show msg => Show (AgentEvent msg) where
  show (ZipperUpdateEvent _) = "ZipperUpdateEvent <zipper>"
  show (UserMessageEvent m) = "UserMessageEvent " ++ show m
  show (AgentCompleteEvent ms) = "AgentCompleteEvent " ++ show ms
  show (RestoreSessionEvent _) = "RestoreSessionEvent <zipper>"
  show (LogEvent l t) = "LogEvent " ++ show l ++ " " ++ show t
  show (ShowInputWidgetEvent _) = "ShowInputWidgetEvent <widget>"
  show ClearInputWidgetEvent = "ClearInputWidgetEvent"
  show (RunExternalCommandEvent _) = "RunExternalCommandEvent <action>"
  show (StreamStartEvent sid) = "StreamStartEvent " ++ show sid
  show (StreamChunkEvent sid c) = "StreamChunkEvent " ++ show sid ++ " " ++ show c
  show (StreamEndEvent sid) = "StreamEndEvent " ++ show sid
  show (StreamErrorEvent sid) = "StreamErrorEvent " ++ show sid

-- | Runtime LLM configuration settings
-- Currently empty but can hold maxLength, reasoning effort, temperature, etc.
data LLMSettings = LLMSettings
  {
  } deriving stock (Eq, Show)

-- | User request containing input text, history zipper, and LLM settings
data UserRequest msg = UserRequest
  { userText :: Text
  , currentHistory :: Zipper (OutputItem msg)  -- ^ Current UI zipper (history structure)
  , requestSettings :: LLMSettings
  } deriving stock (Eq, Show)

-- | File completion request from UI
-- The TVar starts as Nothing, and the handler writes Just [results] when done
data CompletionRequest = CompletionRequest
  { completionPattern :: Text           -- ^ Pattern to match (e.g., "ReadMe", "src/mai")
  , completionResponse :: TVar (Maybe [Text])  -- ^ Response written by handler
  }

-- | Shared state variables for UI communication
-- Parametrized over message type to work with typed events
data UIVars msg = UIVars
  { agentEventChan :: AgentEvent msg -> IO ()  -- ^ Callback to send events to UI
  , userInputQueue :: TQueue (UserRequest msg)  -- ^ User input queue (UI writes, agent reads)
  , cancellationFlag :: TVar Bool         -- ^ Cancellation flag (UI writes, agent reads)
  , completionQueue :: TQueue CompletionRequest  -- ^ File completion requests
  }

-- Note: userResponseQueue is managed per-request in SomeInputWidget callback

-- | Create fresh UI state variables
-- Takes a callback to send agent events
newUIVars :: (AgentEvent msg -> IO ()) -> IO (UIVars msg)
newUIVars sendEventCallback = do
  inputQueue <- newTQueueIO
  cancelFlag <- newTVarIO False
  compQueue <- newTQueueIO
  return $ UIVars sendEventCallback inputQueue cancelFlag compQueue

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

-- | Request file completion and wait for response
-- Creates a TVar, sends request, waits for handler to fill it
requestCompletion :: UIVars msg -> Text -> IO [Text]
requestCompletion vars pattern = do
  responseVar <- newTVarIO Nothing
  let req = CompletionRequest pattern responseVar
  atomically $ writeTQueue (completionQueue vars) req
  -- Wait for response
  atomically $ do
    result <- readTVar responseVar
    case result of
      Nothing -> retry  -- Block until response is written
      Just files -> return files

-- | Wait for a completion request (for interpreter/handler)
waitForCompletion :: TQueue CompletionRequest -> STM CompletionRequest
waitForCompletion = readTQueue

-- | Check the cancellation flag (non-blocking read)
readCancellationFlag :: UIVars msg -> STM Bool
readCancellationFlag vars = readTVar (cancellationFlag vars)

-- | Clear the cancellation flag after handling cancellation (so next request can proceed)
clearCancellationFlag :: UIVars msg -> IO ()
clearCancellationFlag vars = atomically $ writeTVar (cancellationFlag vars) False
