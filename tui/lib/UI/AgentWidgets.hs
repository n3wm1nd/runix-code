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
  , SubsectionAddr(..)
  , addMessage
  , logMessage
  , setStatus
  , replaceHistory
  , startSubsection
  ) where

import Polysemy
import Data.Kind (Type)
import Data.Text (Text)
import Runix.Logging (Level)

-- | Hierarchical address for subsections in the zipper tree
-- () = root, (n, addr) = nth subsection at addr
data SubsectionAddr = Root | Nested Int SubsectionAddr
  deriving stock (Eq, Show, Ord)

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
-- All operations take a SubsectionAddr to specify where in the zipper tree they operate.
data AgentWidgets msg (m :: Type -> Type) a where
  -- | Add a message to the history at the given address
  AddMessage :: SubsectionAddr -> msg -> AgentWidgets msg m ()

  -- | Add a log message with severity level at the given address
  LogMessage :: SubsectionAddr -> Level -> Text -> AgentWidgets msg m ()

  -- | Set agent status at the given address (includes streaming state)
  SetStatus :: SubsectionAddr -> AgentStatus -> AgentWidgets msg m ()

  -- | Replace message history at the given address (merges, preserving logs and subsections)
  ReplaceHistory :: SubsectionAddr -> [msg] -> AgentWidgets msg m ()

  -- | Start a new subsection at the given address, returns the new subsection's address
  StartSubsection :: SubsectionAddr -> AgentWidgets msg m SubsectionAddr

-- | Smart constructors for AgentWidgets operations
-- These default to Root address for backward compatibility
addMessage :: forall msg r. Member (AgentWidgets msg) r => msg -> Sem r ()
addMessage msg = send @(AgentWidgets msg) (AddMessage Root msg)

logMessage :: forall msg r. Member (AgentWidgets msg) r => Level -> Text -> Sem r ()
logMessage level text = send @(AgentWidgets msg) (LogMessage Root level text)

setStatus :: forall msg r. Member (AgentWidgets msg) r => AgentStatus -> Sem r ()
setStatus status = send @(AgentWidgets msg) (SetStatus Root status)

replaceHistory :: forall msg r. Member (AgentWidgets msg) r => [msg] -> Sem r ()
replaceHistory msgs = send @(AgentWidgets msg) (ReplaceHistory Root msgs)

startSubsection :: forall msg r. Member (AgentWidgets msg) r => SubsectionAddr -> Sem r SubsectionAddr
startSubsection addr = send @(AgentWidgets msg) (StartSubsection addr)
