{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Interpreter for AgentWidgets effect
--
-- Maintains output history zipper and segment mappings, translating
-- semantic AgentWidgets operations into zipper updates.
module UI.AgentWidgetsInterpreter
  ( interpretAgentWidgets
  ) where

import Polysemy
import Polysemy.State (State, get, put, modify, runState, gets)

import UI.AgentWidgets (AgentWidgets(..), SegmentID(..))
import UI.OutputHistory (OutputHistoryZipper, OutputItem(..), emptyZipper, appendItem, updateCurrent, zipperCurrent, Zipper, focusOldest, moveNewer)
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))
import Runix.Streaming.SSE (StreamingContent(..))

-- | State maintained by AgentWidgets interpreter
newtype AgentWidgetsState msg = AgentWidgetsState
  { awsZipper :: OutputHistoryZipper msg           -- ^ Current output zipper
  }

-- | Interpret AgentWidgets by maintaining zipper state and sending updates to UI
--
-- This interpreter maintains the output history zipper and segment mappings.
-- Each semantic operation updates the zipper, then sends a ZipperUpdateEvent to the UI.
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
        -- Remove streaming preview if present before adding the real message
        zipper <- gets @(AgentWidgetsState msg) awsZipper
        let cleanedZipper = case zipperCurrent zipper of
              Just (StreamingChunkItem _) -> removeCurrentItem zipper
              Just (StreamingReasoningItem _) -> removeCurrentItem zipper
              _ -> zipper
        modify (\s -> s { awsZipper = appendItem (MessageItem msg) cleanedZipper })
        sendUpdate

      StreamChunk content -> case content of
        StreamingText text -> do
          zipper <- gets @(AgentWidgetsState msg) awsZipper
          let newItem = case zipperCurrent zipper of
                Just (StreamingChunkItem prev) -> StreamingChunkItem (prev <> text)
                _ -> StreamingChunkItem text
          modifyZipper (updateCurrent newItem)
          sendUpdate

        StreamingReasoning text -> do
          zipper <- gets @(AgentWidgetsState msg) awsZipper
          let newItem = case zipperCurrent zipper of
                Just (StreamingReasoningItem prev) -> StreamingReasoningItem (prev <> text)
                _ -> StreamingReasoningItem text
          modifyZipper (updateCurrent newItem)
          sendUpdate

      LogMessage level text -> do
        modifyZipper (appendItem (LogItem level text))
        sendUpdate

      AgentError text -> do
        modifyZipper (appendItem (SystemEventItem text))
        sendUpdate

      CreateSegment -> do
        zipper <- gets @(AgentWidgetsState msg) awsZipper
        let segId = SegmentID (countSections zipper)
        -- Insert empty SectionItem
        modifyZipper (appendItem (SectionItem emptyZipper))
        sendUpdate
        return segId

      UpdateSegment (SegmentID segIndex) f -> do
        state <- get
        -- Navigate to the subsection, apply f, navigate back
        let zipper = awsZipper state
            modifiedZipper = withSubsection segIndex f zipper
        put state { awsZipper = modifiedZipper }
        sendUpdate

      ReplaceHistory msgs -> do
        -- For now, just append messages (proper merging would preserve logs/subsections)
        mapM_ (\msg -> modifyZipper (appendItem (MessageItem msg))) msgs
        sendUpdate

    modifyZipper :: (OutputHistoryZipper msg -> OutputHistoryZipper msg) -> Sem (State (AgentWidgetsState msg) : r) ()
    modifyZipper f = modify (\s -> s { awsZipper = f (awsZipper s) })

    sendUpdate :: Sem (State (AgentWidgetsState msg) : r) ()
    sendUpdate = do
      zipper <- gets awsZipper
      raise $ embed $ sendAgentEvent uiVars (ZipperUpdateEvent zipper)

-- | Navigate to a subsection by index, apply a function, navigate back
withSubsection :: Int -> (Zipper (OutputItem msg) -> Zipper (OutputItem msg)) -> Zipper (OutputItem msg) -> Zipper (OutputItem msg)
withSubsection idx f zipper =
  let -- Move to oldest (start)
      atStart = focusOldest zipper
      -- Move forward to the idx-th SectionItem
      atSection = navigateToSection idx atStart
  in case zipperCurrent atSection of
       Just (SectionItem subZipper) ->
         let modified = SectionItem (f subZipper)
         in updateCurrent modified atSection
       _ -> zipper  -- Not a section, no change

-- | Navigate forward through the zipper until we find the idx-th SectionItem
navigateToSection :: Int -> Zipper (OutputItem msg) -> Zipper (OutputItem msg)
navigateToSection n z
  | n <= 0 = z
  | otherwise = case zipperCurrent z of
      Just (SectionItem _) -> navigateToSection (n - 1) (moveNewer z)
      Just _ -> navigateToSection n (moveNewer z)
      Nothing -> navigateToSection n (moveNewer z)

-- | Count how many SectionItems exist in the zipper
countSections :: Zipper (OutputItem msg) -> Int
countSections z = go 0 (focusOldest z)
  where
    go count zipper = case zipperCurrent zipper of
      Nothing -> count
      Just (SectionItem _) -> go (count + 1) (moveNewer zipper)
      Just _ -> go count (moveNewer zipper)

-- | Remove the current item from the zipper (set current to Nothing)
removeCurrentItem :: Zipper a -> Zipper a
removeCurrentItem z = z { zipperCurrent = Nothing }
