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
import UI.State

-- | Interpret UI effect using STM-based state
--
-- This interpreter:
-- - Writes log messages, status updates, and display messages to TVar UIState
-- - Triggers UI refresh after each update
-- - Blocks on TMVar when prompting for user input
interpretUI :: Member (Embed IO) r
            => UIVars
            -> Sem (UI ': r) a
            -> Sem r a
interpretUI uiVars = interpret $ \case
  LogMessage msg -> embed $ appendLog uiVars msg

  UpdateStatus status -> embed $ setStatus uiVars status

  AddMessageDisplay msg -> embed $ appendDisplayMessage uiVars msg

  PromptUser prompt -> do
    -- First, update status to show we're waiting for input
    embed $ setStatus uiVars prompt
    -- Then block until user provides input
    embed $ atomically $ waitForUserInput (userInputVar uiVars)
