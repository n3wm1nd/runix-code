{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | AgentWidgets effect for routing agent output to UI widgets
--
-- This effect provides a clean abstraction for agents to output to UI
-- without requiring direct knowledge of the storage format. The interpreter
-- maintains the actual OutputHistoryZipper and handles semantic operations.
module UI.AgentWidgets
  ( AgentWidgets(..)
  , AgentStatus(..)
  , StreamingState(..)
  , addMessage
  , logMessage
  , setStatus
  , replaceHistory
  ) where

import Polysemy
import Data.Kind (Type)
import Data.Text (Text)
import Runix.Logging (Level)

-- | Agent execution status (includes streaming state)
data AgentStatus
  = Idle
  | Working Text  -- ^ Working with description (e.g., "Reading files...")
  | Streaming StreamingState  -- ^ Actively streaming LLM response
  | WaitingForInput
  | WaitingForToolCall
  | Done
  | Failed Text  -- ^ Agent failed with error message
  deriving stock (Eq, Show, Ord)

-- | Streaming state for accumulating LLM output
data StreamingState = StreamingState
  { streamingThinking :: Maybe Text   -- ^ Accumulated thinking/reasoning text
  , streamingResponse :: Maybe Text   -- ^ Accumulated response text
  } deriving stock (Eq, Show, Ord)

-- | Effect for outputting to agent widgets
--
-- Semantic operations that abstract away the storage format:
-- - AddMessage: add a message to history (user or agent)
-- - LogMessage: add a log entry
-- - SetStatus: update agent status (includes streaming)
-- - ReplaceHistory: merge new message history (preserves logs, subsections)
data AgentWidgets msg (m :: Type -> Type) a where
  -- | Add a message to the history
  AddMessage :: msg -> AgentWidgets msg m ()

  -- | Add a log message with severity level
  LogMessage :: Level -> Text -> AgentWidgets msg m ()

  -- | Set agent status (includes streaming state)
  SetStatus :: AgentStatus -> AgentWidgets msg m ()

  -- | Replace message history (merges, preserving logs and subsections)
  ReplaceHistory :: [msg] -> AgentWidgets msg m ()

-- | Smart constructors for AgentWidgets operations
addMessage :: forall msg r. Member (AgentWidgets msg) r => msg -> Sem r ()
addMessage msg = send @(AgentWidgets msg) (AddMessage msg)

logMessage :: forall msg r. Member (AgentWidgets msg) r => Level -> Text -> Sem r ()
logMessage level text = send @(AgentWidgets msg) (LogMessage level text)

setStatus :: forall msg r. Member (AgentWidgets msg) r => AgentStatus -> Sem r ()
setStatus status = send @(AgentWidgets msg) (SetStatus status)

replaceHistory :: forall msg r. Member (AgentWidgets msg) r => [msg] -> Sem r ()
replaceHistory msgs = send @(AgentWidgets msg) (ReplaceHistory msgs)
