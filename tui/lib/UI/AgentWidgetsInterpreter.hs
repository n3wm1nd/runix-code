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

import UI.AgentWidgets (AgentWidgets(..), SubsectionAddr(..))
import UI.OutputHistory (OutputHistoryZipper, OutputItem(..), emptyZipper, appendItem, insertItem, moveNewer, updateCurrent, zipperCurrent, countSubtrees, atAddress, queryAtAddress)
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
      AddMessage addr msg -> do
        clearStatusIfPresent
        modifyZipperAt addr (appendItem (MessageItem msg))
        sendUpdate

      LogMessage addr level text -> do
        modifyZipperAt addr (appendItem (LogItem level text))
        sendUpdate

      SetStatus addr status -> do
        modifyZipperAt addr (\zipper ->
          case zipperCurrent zipper of
            Just (StatusItem _) -> updateCurrent (StatusItem status) zipper
            _ -> appendItem (StatusItem status) zipper
          )
        sendUpdate

      ReplaceHistory addr msgs -> do
        mapM_ (\msg -> modifyZipperAt addr (appendItem (MessageItem msg))) msgs
        sendUpdate

      StartSubsection addr -> do
        -- Count existing SectionItems at this address to get the new ID
        zipper <- gets awsZipper
        let subsectionId = countSectionsAt addr zipper
        -- Insert subsection as newer item (into back), then move focus to it
        modifyZipperAt addr (\z -> moveNewer (insertItem (SectionItem emptyZipper) z))
        -- Return the new nested address
        return $ Nested subsectionId addr

    -- Count how many SectionItems exist at a given address
    countSectionsAt :: SubsectionAddr -> OutputHistoryZipper msg -> Int
    countSectionsAt addr zipper = queryAtAddress addr countSubtrees zipper

    -- Modify zipper at address using proper zipper operations
    modifyZipperAt :: SubsectionAddr -> (OutputHistoryZipper msg -> OutputHistoryZipper msg) -> Sem (State (AgentWidgetsState msg) : r) ()
    modifyZipperAt addr f = modify (\s -> s { awsZipper = atAddress addr f (awsZipper s) })

    clearStatusIfPresent :: Sem (State (AgentWidgetsState msg) : r) ()
    clearStatusIfPresent = do
      zipper <- gets @(AgentWidgetsState msg) awsZipper
      case zipperCurrent zipper of
        Just (StatusItem _) -> modify @(AgentWidgetsState msg) (\s -> s { awsZipper = removeCurrentItem (awsZipper s) })
        _ -> return ()

    sendUpdate :: Sem (State (AgentWidgetsState msg) : r) ()
    sendUpdate = do
      zipper <- gets awsZipper
      raise $ embed $ sendAgentEvent uiVars (ZipperUpdateEvent zipper)

-- | Remove the current item from the zipper
removeCurrentItem :: OutputHistoryZipper msg -> OutputHistoryZipper msg
removeCurrentItem z = z { zipperCurrent = Nothing }
