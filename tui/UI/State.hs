{-# LANGUAGE RankNTypes #-}

-- | STM-based UI state for concurrent UI updates
--
-- This module defines the shared state between the effect interpreter stack
-- and the brick UI thread. The effect interpreters write to this state,
-- and the UI reads from it.
module UI.State where

import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import UI.OutputHistory (OutputMessage(..), DisplayFilter, defaultFilter, LogLevel(..), addLog, addStreamingChunk)

-- | UI state shared between agent thread and UI thread
data UIState = UIState
  { uiOutputHistory :: [OutputMessage]  -- ^ Complete timeline of all output
  , uiStatus :: Text                     -- ^ Current status message
  , uiDisplayFilter :: DisplayFilter     -- ^ What to show in the UI
  }

-- | Initial empty UI state
emptyUIState :: UIState
emptyUIState = UIState
  { uiOutputHistory = []
  , uiStatus = T.pack "Ready"
  , uiDisplayFilter = defaultFilter
  }

-- | Shared state variables for UI communication
data UIVars = UIVars
  { uiStateVar :: TVar UIState      -- ^ Main UI state (agent writes, UI reads)
  , userInputQueue :: TQueue Text   -- ^ User input queue (UI writes, agent reads)
  , refreshSignal :: IO ()          -- ^ Callback to trigger UI refresh
  }

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

-- | Update the status line and trigger refresh
setStatus :: UIVars -> Text -> IO ()
setStatus vars status = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiStatus = status }
  refreshSignal vars

-- | Patch the output history with new messages and trigger refresh
patchMessages :: UIVars -> [OutputMessage] -> IO ()
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
