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
import GHC.Stack (CallStack, prettyCallStack)

import Runix.Logging.Effects (Logging(..), Level(..))
import UI.Effects (UI, logMessage)

-- | Reinterpret Logging effect as UI effect
--
-- Logging messages are formatted with their severity level and
-- sent to the UI via the logMessage operation.
interpretLoggingToUI :: Member UI r => Sem (Logging ': r) a -> Sem r a
interpretLoggingToUI = interpret $ \case
  Log level _callStack msg ->
    logMessage $ levelPrefix level <> msg
  where
    levelPrefix Info = T.pack "[INFO] "
    levelPrefix Warning = T.pack "[WARNING] "
    levelPrefix Error = T.pack "[ERROR] "

-- | Reinterpret Logging with call stack information
--
-- Includes the call stack in the log message for better debugging.
interpretLoggingToUIWithStack :: Member UI r => Sem (Logging ': r) a -> Sem r a
interpretLoggingToUIWithStack = interpret $ \case
  Log level callStack msg ->
    logMessage $ levelPrefix level <> msg <> T.pack "\n  " <> T.pack (prettyCallStack callStack)
  where
    levelPrefix Info = T.pack "[INFO] "
    levelPrefix Warning = T.pack "[WARNING] "
    levelPrefix Error = T.pack "[ERROR] "
