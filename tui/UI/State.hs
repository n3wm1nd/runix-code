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
  }

-- | Create fresh UI state variables
newUIVars :: IO UIVars
newUIVars = do
  stateVar <- newTVarIO emptyUIState
  inputVar <- newEmptyTMVarIO
  return $ UIVars stateVar inputVar

-- | Append a log message to the UI state
appendLog :: TVar UIState -> Text -> STM ()
appendLog stateVar msg = modifyTVar' stateVar $ \st ->
  st { uiLogs = uiLogs st ++ [msg] }

-- | Update the status line
setStatus :: TVar UIState -> Text -> STM ()
setStatus stateVar status = modifyTVar' stateVar $ \st ->
  st { uiStatus = status }

-- | Add a display message to the chat history
appendDisplayMessage :: TVar UIState -> Text -> STM ()
appendDisplayMessage stateVar msg = modifyTVar' stateVar $ \st ->
  st { uiDisplayMessages = uiDisplayMessages st ++ [msg] }

-- | Read the current UI state (for UI rendering)
readUIState :: TVar UIState -> STM UIState
readUIState = readTVar

-- | Block until user provides input
waitForUserInput :: TMVar Text -> STM Text
waitForUserInput = takeTMVar

-- | Provide user input (from UI thread)
provideUserInput :: TMVar Text -> Text -> STM ()
provideUserInput = putTMVar
