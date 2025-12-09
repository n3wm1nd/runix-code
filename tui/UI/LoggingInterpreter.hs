{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Interpreter that routes Logging effect to UI effect
--
-- This allows logging messages to appear in the TUI automatically
-- without the logging code knowing about UI details.
module UI.LoggingInterpreter where

import Polysemy
import qualified Data.Text as T
import GHC.Stack (prettyCallStack)

import Runix.Logging.Effects (Logging(..))
import UI.Effects (UI, logMessage)

-- | Reinterpret Logging effect as UI effect
--
-- Logging messages are sent to the UI with their severity level preserved.
interpretLoggingToUI :: Member UI r => Sem (Logging ': r) a -> Sem r a
interpretLoggingToUI = interpret $ \case
  Log level _callStack msg ->
    logMessage level msg

-- | Reinterpret Logging with call stack information
--
-- Includes the call stack in the log message for better debugging.
interpretLoggingToUIWithStack :: Member UI r => Sem (Logging ': r) a -> Sem r a
interpretLoggingToUIWithStack = interpret $ \case
  Log level callStack msg ->
    logMessage level (msg <> T.pack "\n  " <> T.pack (prettyCallStack callStack))
