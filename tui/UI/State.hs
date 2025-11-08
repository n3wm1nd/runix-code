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

-- | UI state shared between agent thread and UI thread
data UIState = UIState
  { uiLogs :: [Text]          -- ^ Log messages (most recent last)
  , uiStatus :: Text           -- ^ Current status message
  , uiDisplayMessages :: [Text] -- ^ Chat history as display text (most recent last)
  }

-- | Initial empty UI state
emptyUIState :: UIState
emptyUIState = UIState
  { uiLogs = []
  , uiStatus = T.pack "Ready"
  , uiDisplayMessages = []
  }

-- | Shared state variables for UI communication
data UIVars = UIVars
  { uiStateVar :: TVar UIState      -- ^ Main UI state (agent writes, UI reads)
  , userInputVar :: TMVar Text      -- ^ User input channel (UI writes, agent reads)
  , refreshSignal :: IO ()          -- ^ Callback to trigger UI refresh
  }

-- | Create fresh UI state variables
-- The refresh callback will be set later by the UI
newUIVars :: IO () -> IO UIVars
newUIVars refreshCallback = do
  stateVar <- newTVarIO emptyUIState
  inputVar <- newEmptyTMVarIO
  return $ UIVars stateVar inputVar refreshCallback

-- | Append a log message to the UI state and trigger refresh
appendLog :: UIVars -> Text -> IO ()
appendLog vars msg = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiLogs = uiLogs st ++ [msg] }
  refreshSignal vars

-- | Update the status line and trigger refresh
setStatus :: UIVars -> Text -> IO ()
setStatus vars status = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiStatus = status }
  refreshSignal vars

-- | Add a display message to the chat history and trigger refresh
appendDisplayMessage :: UIVars -> Text -> IO ()
appendDisplayMessage vars msg = do
  atomically $ modifyTVar' (uiStateVar vars) $ \st ->
    st { uiDisplayMessages = uiDisplayMessages st ++ [msg] }
  refreshSignal vars

-- | Read the current UI state (for UI rendering)
readUIState :: TVar UIState -> STM UIState
readUIState = readTVar

-- | Block until user provides input
waitForUserInput :: TMVar Text -> STM Text
waitForUserInput = takeTMVar

-- | Provide user input (from UI thread)
provideUserInput :: TMVar Text -> Text -> STM ()
provideUserInput = putTMVar
