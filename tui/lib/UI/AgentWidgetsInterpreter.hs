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
import UI.OutputHistory (OutputHistoryZipper, OutputItem(..), emptyZipper, appendItem, insertItem, moveNewer, updateCurrent, zipperCurrent, countSubtrees, atAddress, queryAtAddress, zipperToList, listToZipper, mergeOutputMessages)
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))

-- | State maintained by AgentWidgets interpreter
newtype AgentWidgetsState msg = AgentWidgetsState
  { awsZipper :: OutputHistoryZipper msg           -- ^ Current output zipper
  }

-- | Interpret AgentWidgets by maintaining zipper state and sending updates to UI
--
-- This interpreter maintains the output history zipper. Each semantic operation
-- updates the zipper, then sends a ZipperUpdateEvent to the UI for immediate rendering.
--
-- Takes the initial zipper from the UI to ensure state is in sync.
interpretAgentWidgets :: forall msg r a. (Member (Embed IO) r, Eq msg)
                      => UIVars msg
                      -> OutputHistoryZipper msg  -- ^ Initial zipper from UI
                      -> Sem (AgentWidgets msg : r) a
                      -> Sem r a
interpretAgentWidgets uiVars initialZipper sem = do
  (_finalState, result) <- runState (AgentWidgetsState initialZipper) $ interpret handler $ raiseUnder sem
  return result
  where

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
        -- Replace history by merging new messages with existing zipper
        -- Preserves logs and subsections from old zipper
        -- Note: CompletedToolItem entries are NOT added here - they're added at display time
        -- or when initially loading a session. This keeps the interpreter generic over msg type.
        modifyZipperAt addr $ \zipper ->
          let oldItems = zipperToList zipper  -- newest-first
              newItems = map MessageItem (reverse msgs)  -- msgs is oldest-first, convert to newest-first
              merged = mergeOutputMessages newItems oldItems  -- newest-first
          in listToZipper merged
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
