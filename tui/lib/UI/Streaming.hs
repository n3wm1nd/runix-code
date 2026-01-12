{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Streaming chunk processing for the TUI
--
-- This module handles the conversion and batching of streaming chunks:
-- 1. Extract content from SSE chunks (reinterpretSSEChunks)
-- 2. Batch rapid chunks to prevent UI overload (interpretStreamChunkToUI)
-- 3. Cancellation support for the TUI (interpretCancellation)
module UI.Streaming
  ( reinterpretSSEChunks
  , interpretStreamChunkToUI
  , interpretCancellation
  ) where

import qualified Data.ByteString as BS
import Control.Concurrent.STM
import Polysemy (Member, Sem, Embed, embed, interpret, reinterpret)
import Runix.Streaming (StreamChunk(..), emitChunk)
import Runix.Streaming.SSE (StreamingContent(..), extractContentFromChunk)
import Runix.Cancellation (Cancellation(..))
import UI.State (UIVars, AgentEvent(..), sendAgentEvent, readCancellationFlag)

--------------------------------------------------------------------------------
-- SSE Chunk Reinterpretation
--------------------------------------------------------------------------------

-- | Reinterpret StreamChunk BS.ByteString to StreamChunk StreamingContent
-- Extracts content from SSE chunks, filtering out metadata events
-- Emits ALL content chunks found in each HTTP chunk (may be multiple SSE events per chunk)
reinterpretSSEChunks :: Sem (StreamChunk BS.ByteString : r) a
                     -> Sem (StreamChunk StreamingContent : r) a
reinterpretSSEChunks = reinterpret $ \case
  EmitChunk chunk ->
    -- Extract all content chunks from this HTTP chunk and emit each one
    mapM_ emitChunk (extractContentFromChunk chunk)

--------------------------------------------------------------------------------
-- Stream Batching
--------------------------------------------------------------------------------

-- | Interpret StreamChunk StreamingContent by forwarding directly to UI
--
-- Simplified version: just forward events directly (blocking) to verify
-- where the actual problem is (sending vs receiving).
interpretStreamChunkToUI :: forall msg r a. Member (Embed IO) r
                         => UIVars msg
                         -> Sem (StreamChunk StreamingContent : r) a
                         -> Sem r a
interpretStreamChunkToUI uiVars = interpret $ \case
  EmitChunk content ->
    embed $ sendAgentEvent uiVars (toAgentEvent content)
  where
    -- Convert StreamingContent to AgentEvent
    toAgentEvent :: StreamingContent -> AgentEvent msg
    toAgentEvent (StreamingText text) = StreamChunkEvent text
    toAgentEvent (StreamingReasoning reasoning) = StreamReasoningEvent reasoning

--------------------------------------------------------------------------------
-- Cancellation
--------------------------------------------------------------------------------

-- | Interpret Cancellation effect for TUI
--
-- Reads the cancellation flag from UIVars (set by UI when user presses ESC).
-- The flag is checked at strategic points: before QueryLLM, between HTTP chunks.
interpretCancellation :: Member (Embed IO) r
                      => UIVars msg
                      -> Sem (Cancellation : r) a
                      -> Sem r a
interpretCancellation uiVars = interpret $ \case
  IsCanceled ->
    -- Check the STM flag atomically
    embed $ atomically $ readCancellationFlag uiVars
