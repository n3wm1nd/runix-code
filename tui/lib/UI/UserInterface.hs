{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Widget isolation interpreter for TUI
--
-- 'interpretAsWidget' captures all output from the action it interprets
-- into a single section, using the AgentWidgets semantic operations.
--
-- It creates a segment for the action's output and routes all emissions
-- (Logging, LLM results, nested AgentWidgets) into that segment via UpdateSegment.
module UI.UserInterface
  ( interpretAsWidget
  ) where

import Polysemy
import Polysemy.Fail (Fail(..))
import qualified Data.Text as T

import Runix.LLM (LLM(..), LLMInfo(..))
import Runix.Logging (Logging(..))
import Runix.Streaming.SSE (StreamingContent(..))
import UniversalLLM (Message)
import UI.AgentWidgets (AgentWidgets(..), SegmentID, createSegment, updateSegment, streamChunk, logMessage, agentError, addMessage)
import UI.OutputHistory (OutputItem(..), appendItem, updateCurrent, zipperCurrent)

-- | Interpret an action's output into a section on the existing 'AgentWidgets'.
--
-- Creates a segment and routes all output (logs, LLM streaming, completion,
-- sub-agent emissions) into that segment.
interpretAsWidget
  :: forall model r a.
     ( Member (AgentWidgets (Message model)) r
     , Member (LLM model) r
     , Member Fail r
     )
  => Sem (AgentWidgets (Message model) : Logging : LLM model : r) a
  -> Sem r a
interpretAsWidget action = do
  -- Create a segment for this action's output
  segmentId <- createSegment @(Message model)

  -- Interpret the action with all effects routing to parent AgentWidgets
  -- Chain order (outermost to innermost): LLM → Logging → AgentWidgets (fresh, for sub-agents only)
  -- Logging and LLM call parent AgentWidgets operations directly (logMessage, streamChunk, etc)
  result <- interpretH @(LLM model) (\case
        QueryLLM configs msgs _callback -> do
          -- Replace callback to route streaming to parent AgentWidgets
          result <- raise (send (QueryLLM configs msgs streamToParent))
          case result of
            Right messages ->
              -- Add completion messages via parent AgentWidgets
              mapM_ (\m -> raise (addMessage @(Message model) m)) messages
            Left err ->
              -- Add error via parent AgentWidgets
              raise (agentError @(Message model) (T.pack ("LLM error: " <> err)))
          pureT result
      )
    . intercept (\case
        Fail msg -> do
          -- Capture errors via parent AgentWidgets
          raise $ agentError @(Message model) (T.pack msg)
          send (Fail msg)
      )
    . interpret (\case
        Log level _callstack msg ->
          -- Route logs to parent AgentWidgets
          raise $ logMessage @(Message model) level msg
      )
    . interpret (\case
        -- Sub-agents call AgentWidgets operations - route them into THIS segment via updateSegment
        AddMessage msg ->
          raise $ updateSegment @(Message model) segmentId (appendItem (MessageItem msg))

        StreamChunk content -> case content of
          StreamingText text ->
            raise $ updateSegment @(Message model) segmentId $ \subZipper ->
              case zipperCurrent subZipper of
                Just (StreamingChunkItem prev) ->
                  updateCurrent (StreamingChunkItem (prev <> text)) subZipper
                _ ->
                  appendItem (StreamingChunkItem text) subZipper

          StreamingReasoning text ->
            raise $ updateSegment @(Message model) segmentId $ \subZipper ->
              case zipperCurrent subZipper of
                Just (StreamingReasoningItem prev) ->
                  updateCurrent (StreamingReasoningItem (prev <> text)) subZipper
                _ ->
                  appendItem (StreamingReasoningItem text) subZipper

        LogMessage level text ->
          raise $ updateSegment @(Message model) segmentId (appendItem (LogItem level text))

        AgentError text ->
          raise $ updateSegment @(Message model) segmentId (appendItem (SystemEventItem text))

        -- Sub-agents create their own segments - forward to parent
        CreateSegment ->
          raise $ createSegment @(Message model)

        -- Sub-agents update their own segments - forward to parent
        UpdateSegment subSegId f ->
          raise $ updateSegment @(Message model) subSegId f

        -- ReplaceHistory from sub-agents - append messages to this segment
        ReplaceHistory msgs ->
          mapM_ (\m -> raise $ updateSegment @(Message model) segmentId (appendItem (MessageItem m))) msgs
      )
    $ action

  return result
  where
    -- Streaming callback: calls parent AgentWidgets streamChunk operation
    streamToParent :: Member (AgentWidgets (Message model)) r' => LLMInfo -> Sem r' ()
    streamToParent (LLMInfo content) = streamChunk @(Message model) content
