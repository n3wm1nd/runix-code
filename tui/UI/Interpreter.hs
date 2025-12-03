{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

-- | Interpreter for UI effect using STM-based state
--
-- This interpreter bridges the Polysemy effect stack with the brick UI
-- by writing to shared STM variables.
module UI.Interpreter where

import Polysemy
import Polysemy.Embed (embed)
import Control.Concurrent.STM

import UI.Effects
import UI.State (UIVars, userInputQueue, waitForUserInput, sendAgentEvent)
import UI.OutputHistory (LogLevel(..))
import UI.State (AgentEvent(..))

-- | Interpret UI effect by sending events to the UI thread
--
-- This interpreter:
-- - Sends log messages and status updates as AgentEvents to the UI queue
-- - Triggers UI refresh after each event
-- - Blocks on user input queue when prompting for input
interpretUI :: Member (Embed IO) r
            => UIVars msg
            -> Sem (UI ': r) a
            -> Sem r a
interpretUI uiVars = interpret $ \case
  LogMessage msg -> embed $ sendAgentEvent uiVars (LogEvent Info msg)

  UpdateStatus status -> embed $ sendAgentEvent uiVars (LogEvent Info status)

  PromptUser prompt -> do
    -- First, send status update to show we're waiting for input
    embed $ sendAgentEvent uiVars (LogEvent Info prompt)
    -- Then block until user provides input
    embed $ atomically $ waitForUserInput (userInputQueue uiVars)
