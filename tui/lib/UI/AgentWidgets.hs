{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | AgentWidgets effect for routing agent output to UI widgets
--
-- This effect provides a clean abstraction for agents to output to UI
-- without requiring direct IO access. Different UIs (TUI, CLI, API) can
-- interpret this effect differently.
module UI.AgentWidgets
  ( AgentWidgets(..)
  , emitLog
  , emitStreamChunk
  , emitError
  , emitCompletion
  ) where

import Polysemy
import Data.Kind (Type)
import Data.Text (Text)
import Runix.Logging (Level)
import Runix.Streaming.SSE (StreamingContent)

-- | Effect for outputting to agent widgets
--
-- Operations represent different types of output an agent might produce:
-- - Logs (with severity levels)
-- - Streaming chunks (real-time LLM responses)
-- - Errors
-- - Completion (with final message history)
data AgentWidgets msg (m :: Type -> Type) a where
  -- | Emit a log message with a severity level
  EmitLog :: Level -> Text -> AgentWidgets msg m ()

  -- | Emit a streaming chunk (real-time LLM output)
  EmitStreamChunk :: StreamingContent -> AgentWidgets msg m ()

  -- | Emit an error message
  EmitError :: Text -> AgentWidgets msg m ()

  -- | Signal completion with final message history
  EmitCompletion :: [msg] -> AgentWidgets msg m ()

-- | Smart constructors for AgentWidgets operations
emitLog :: forall msg r. Member (AgentWidgets msg) r => Level -> Text -> Sem r ()
emitLog level text = send @(AgentWidgets msg) (EmitLog level text)

emitStreamChunk :: forall msg r. Member (AgentWidgets msg) r => StreamingContent -> Sem r ()
emitStreamChunk content = send @(AgentWidgets msg) (EmitStreamChunk content)

emitError :: forall msg r. Member (AgentWidgets msg) r => Text -> Sem r ()
emitError text = send @(AgentWidgets msg) (EmitError text)

emitCompletion :: forall msg r. Member (AgentWidgets msg) r => [msg] -> Sem r ()
emitCompletion msgs = send @(AgentWidgets msg) (EmitCompletion msgs)
