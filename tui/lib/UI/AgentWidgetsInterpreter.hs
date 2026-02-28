{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

-- | Interpreter for AgentWidgets effect
--
-- Maintains output history zipper, translating semantic AgentWidgets
-- operations into zipper updates. Handles sub-agent routing via recursion.
module UI.AgentWidgetsInterpreter
  ( interpretAgentWidgets
  ) where

import Polysemy
import Polysemy.State (State, put, modify, runState, gets, get)
import Control.Monad (when)

import UI.AgentWidgets (AgentWidgets(..))
import UI.OutputHistory (OutputHistoryZipper, OutputItem(..), emptyZipper, appendItem, updateCurrent, zipperCurrent)
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))

-- | State maintained by AgentWidgets interpreter
newtype AgentWidgetsState msg = AgentWidgetsState
  { awsZipper :: OutputHistoryZipper msg           -- ^ Current output zipper
  }

-- | Interpret AgentWidgets by maintaining zipper state and sending updates to UI
--
-- This interpreter maintains the output history zipper. Each semantic operation
-- updates the zipper, then sends a ZipperUpdateEvent to the UI for immediate rendering.
interpretAgentWidgets :: forall msg r a. Member (Embed IO) r
                      => UIVars msg
                      -> Sem (AgentWidgets msg : r) a
                      -> Sem r a
interpretAgentWidgets uiVars sem = do
  (_finalState, result) <- runState initialState $ interpret handler $ raiseUnder sem
  return result
  where
    initialState = AgentWidgetsState emptyZipper

    handler :: AgentWidgets msg (Sem rInitial) x -> Sem (State (AgentWidgetsState msg) : r) x
    handler = \case
      AddMessage msg -> do
        clearStatusIfPresent
        modifyZipper (appendItem (MessageItem msg))
        sendUpdate

      LogMessage level text -> do
        modifyZipper (appendItem (LogItem level text))
        sendUpdate

      SetStatus status -> do
        zipper <- gets @(AgentWidgetsState msg) awsZipper
        let newZipper = case zipperCurrent zipper of
              Just (StatusItem _) -> updateCurrent (StatusItem status) zipper
              _ -> appendItem (StatusItem status) zipper
        put $ AgentWidgetsState newZipper
        sendUpdate

      ReplaceHistory msgs -> do
        mapM_ (\msg -> modifyZipper (appendItem (MessageItem msg))) msgs
        sendUpdate

    modifyZipper :: (OutputHistoryZipper msg -> OutputHistoryZipper msg) -> Sem (State (AgentWidgetsState msg) : r) ()
    modifyZipper f = modify (\s -> s { awsZipper = f (awsZipper s) })

    clearStatusIfPresent :: Sem (State (AgentWidgetsState msg) : r) ()
    clearStatusIfPresent = do
      zipper <- gets @(AgentWidgetsState msg) awsZipper
      case zipperCurrent zipper of
        Just (StatusItem _) -> modifyZipper removeCurrentItem
        _ -> return ()

    sendUpdate :: Sem (State (AgentWidgetsState msg) : r) ()
    sendUpdate = do
      zipper <- gets awsZipper
      raise $ embed $ sendAgentEvent uiVars (ZipperUpdateEvent zipper)

-- | Remove the current item from the zipper
removeCurrentItem :: OutputHistoryZipper msg -> OutputHistoryZipper msg
removeCurrentItem z = z { zipperCurrent = Nothing }
