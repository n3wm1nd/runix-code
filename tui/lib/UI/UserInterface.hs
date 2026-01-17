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
import UI.AgentWidgets (AgentWidgets, emitLog, emitStreamChunk, emitError, emitCompletion)
import qualified Data.Text as T

-- | Interpreter that routes UI effects to a widget
--
-- This interpreter:
-- - Observes State [msg] (message history) to capture messages
-- - Swallows Logging and routes to AgentWidgets
-- - Swallows StreamChunk and routes to AgentWidgets
-- - Observes Fail (intercepts but passes through) to send error events
-- - Uses AgentWidgets effect to output (no direct IO)
-- - Return value passes through unchanged
interpretAsWidget :: forall msg r a. (Member (State [msg]) r, Member (AgentWidgets msg) r, Member Fail r)
                  => Sem (Logging : StreamChunk StreamingContent : r) a
                  -> Sem r a
interpretAsWidget action = do
  -- Run action with intercepted effects
  result <- intercept (\case
              -- Observe Fail - send error event but pass through
              Fail msg -> do
                emitError @msg (T.pack msg)
                send (Fail msg)  -- Re-send to propagate error
            )
          . interpret (\case
              -- Swallow StreamChunk - route to widget
              EmitChunk content ->
                emitStreamChunk @msg content
            )
          . interpret (\case
              -- Swallow Logging - route to widget
              Log level _ msg ->
                emitLog @msg level msg
            )
          $ action

  -- Get final message history after action completes
  finalHistory <- get @[msg]

  -- Send completion event with final messages
  emitCompletion @msg finalHistory

  return result
