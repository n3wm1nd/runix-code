{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

-- | STM-based UI state for concurrent UI updates
--
-- This module defines the shared state between the effect interpreter stack
-- and the brick UI thread. The effect interpreters write to this state,
-- and the UI reads from it.
module UI.State
  ( UIState(..)
  , UIVars(..)
  , Name(..)
  , SomeInputWidget(..)
  , emptyUIState
  , newUIVars
  , appendLog
  , appendStreamingChunk
  , appendStreamingReasoning
  , setStatus
  , patchMessages
  , setDisplayFilter
  , readUIState
  , waitForUserInput
  , provideUserInput
  , setPendingInput
  , clearPendingInput
  , uiStateVar
  , userInputQueue
  , userResponseQueue
  ) where

import Brick (Widget, EventM)
import Brick.Types (BrickEvent)
import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import UI.OutputHistory (OutputHistory, RenderedMessage, DisplayFilter, defaultFilter, LogLevel(..), addLog, addStreamingChunk, addStreamingReasoning)
import UI.UserInput.InputWidget (InputWidget(..))

-- | Resource names for widgets (defined here to avoid circular dependency)
data Name = InputEditor | HistoryViewport | CompletedHistory
  deriving stock (Eq, Ord, Show)

-- | Existential wrapper for input widgets of any type
-- This allows us to store a typed widget in UIState without knowing its type
data SomeInputWidget where
  SomeInputWidget
    :: InputWidget a
    => Text              -- ^ Prompt text
    -> a                 -- ^ Current value
    -> (a -> IO ())      -- ^ Callback to submit the value
    -> SomeInputWidget

-- | UI state shared between agent thread and UI thread
data UIState = UIState
  { uiOutputHistory :: OutputHistory Name  -- ^ Complete timeline with cached widgets (newest first)
  , uiStatus :: Text                        -- ^ Current status message
  , uiDisplayFilter :: DisplayFilter        -- ^ What to show in the UI
  , uiPendingInput :: Maybe SomeInputWidget -- ^ Active input widget, if any
  }

-- | Initial empty UI state
emptyUIState :: UIState
emptyUIState = UIState
  { uiOutputHistory = []
  , uiStatus = T.pack "Ready"
  , uiDisplayFilter = defaultFilter
  , uiPendingInput = Nothing
  }

-- | Shared state variables for UI communication
data UIVars = UIVars
  { uiStateVar :: TVar UIState      -- ^ Main UI state (agent writes, UI reads)
  , userInputQueue :: TQueue Text   -- ^ User input queue (UI writes, agent reads)
  , refreshSignal :: IO ()          -- ^ Callback to trigger UI refresh (non-blocking, coalesces multiple requests)
  }

-- Note: userResponseQueue is managed per-request in SomeInputWidget callback

-- | Create fresh UI state variables
-- The refresh callback will be set later by the UI
newUIVars :: IO () -> IO UIVars
newUIVars refreshCallback = do
  stateVar <- newTVarIO emptyUIState
  inputQueue <- newTQueueIO
  return $ UIVars stateVar inputQueue refreshCallback

-- | Append a log message to output history and trigger refresh
appendLog :: UIVars -> Text -> IO ()
appendLog vars msg = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiOutputHistory = addLog Info msg (uiOutputHistory st) }
  refreshSignal vars

-- | Append a streaming chunk to output history and trigger refresh
appendStreamingChunk :: UIVars -> Text -> IO ()
appendStreamingChunk vars chunk = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiOutputHistory = addStreamingChunk chunk (uiOutputHistory st) }
  refreshSignal vars

-- | Append a streaming reasoning chunk to output history and trigger refresh
appendStreamingReasoning :: UIVars -> Text -> IO ()
appendStreamingReasoning vars chunk = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiOutputHistory = addStreamingReasoning chunk (uiOutputHistory st) }
  refreshSignal vars

-- | Update the status line and trigger refresh
setStatus :: UIVars -> Text -> IO ()
setStatus vars status = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiStatus = status }
  refreshSignal vars

-- | Patch the output history with new rendered messages and trigger refresh
patchMessages :: UIVars -> OutputHistory Name -> IO ()
patchMessages vars newOutput = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiOutputHistory = newOutput }
  refreshSignal vars

-- | Update the display filter and trigger refresh
setDisplayFilter :: UIVars -> DisplayFilter -> IO ()
setDisplayFilter vars filt = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiDisplayFilter = filt }
  refreshSignal vars

-- | Read the current UI state (for UI rendering)
readUIState :: TVar UIState -> STM UIState
readUIState = readTVar

-- | Block until user provides input
waitForUserInput :: TQueue Text -> STM Text
waitForUserInput = readTQueue

-- | Provide user input (from UI thread)
provideUserInput :: TQueue Text -> Text -> STM ()
provideUserInput = writeTQueue

-- | Set a pending input widget (from interpreter)
setPendingInput :: UIVars -> SomeInputWidget -> IO ()
setPendingInput vars widget = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiPendingInput = Just widget }
  refreshSignal vars

-- | Clear pending input widget (when completed or cancelled)
clearPendingInput :: UIVars -> IO ()
clearPendingInput vars = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiPendingInput = Nothing }
  refreshSignal vars

-- | Dummy export for compatibility (not actually used with new approach)
userResponseQueue :: ()
userResponseQueue = ()
