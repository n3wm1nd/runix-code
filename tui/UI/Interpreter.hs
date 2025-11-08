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
-- - Blocks on TMVar when prompting for user input
interpretUI :: Member (Embed IO) r
            => UIVars
            -> Sem (UI ': r) a
            -> Sem r a
interpretUI (UIVars stateVar inputVar) = interpret $ \case
  LogMessage msg -> embed $ atomically $ appendLog stateVar msg

  UpdateStatus status -> embed $ atomically $ setStatus stateVar status

  AddMessageDisplay msg -> embed $ atomically $ appendDisplayMessage stateVar msg

  PromptUser prompt -> do
    -- First, update status to show we're waiting for input
    embed $ atomically $ setStatus stateVar prompt
    -- Then block until user provides input
    embed $ atomically $ waitForUserInput inputVar
