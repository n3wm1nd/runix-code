{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Widget isolation interpreter for TUI
--
-- This module provides an interpreter that captures UI effects (Logging, StreamChunk)
-- and routes them to a widget for display, while observing message history.
module UI.UserInterface
  ( interpretAsWidget
  ) where

import Polysemy
import Polysemy.State (State(..), get)
import Polysemy.Fail (Fail(..))
import Runix.Logging (Logging(..))
import Runix.Streaming (StreamChunk(..))
import Runix.Streaming.SSE (StreamingContent(..))
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))
import UI.OutputHistory (OutputItem(..))
import qualified Data.Text as T

-- | Interpreter that routes UI effects to a widget
--
-- This interpreter:
-- - Observes State [msg] (message history) via intercept (passes through) to capture messages
-- - Swallows Logging and routes log messages to widget
-- - Swallows StreamChunk StreamingContent and routes stream chunks to widget
-- - Observes Fail (intercepts but passes through) to send error events
-- - Leaves UI effect global (for user input prompts)
-- - Return value passes through unchanged
interpretAsWidget :: forall msg r a. (Member (State [msg]) r, Member (Embed IO) r, Member Fail r)
                  => UIVars msg
                  -> Sem (Logging : StreamChunk StreamingContent : r) a
                  -> Sem r a
interpretAsWidget uiVars action = do
  -- Run action with intercepted effects
  result <- intercept (\case
              -- Observe Fail - send error event but pass through
              Fail msg -> do
                embed $ sendAgentEvent uiVars (AgentErrorEvent (T.pack msg))
                send (Fail msg)  -- Re-send to propagate error
            )
          . interpret (\case
              -- Swallow StreamChunk - send to widget instead
              EmitChunk content ->
                embed $ sendAgentEvent uiVars (toAgentEvent content)
            )
          . interpret (\case
              -- Swallow Logging - send to widget instead
              Log level _ msg ->
                embed $ sendAgentEvent uiVars (LogEvent level msg)
            )
          $ action

  -- Get final message history after action completes
  finalHistory <- get @[msg]

  -- Send completion event with final messages
  embed $ sendAgentEvent uiVars (AgentCompleteEvent finalHistory)

  return result
  where
    toAgentEvent :: StreamingContent -> AgentEvent msg
    toAgentEvent (StreamingText text) = StreamChunkEvent text
    toAgentEvent (StreamingReasoning reasoning) = StreamReasoningEvent reasoning
