{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | AgentWidgets effect for routing agent output to UI widgets
--
-- This effect provides a clean abstraction for agents to output to UI
-- without requiring direct knowledge of the storage format. The interpreter
-- maintains the actual OutputHistoryZipper and handles semantic operations.
module UI.AgentWidgets
  ( AgentWidgets(..)
  , SegmentID(..)
  , addMessage
  , streamChunk
  , logMessage
  , agentError
  , createSegment
  , updateSegment
  , replaceHistory
  ) where

import Polysemy
import Data.Kind (Type)
import Data.Text (Text)
import Runix.Logging (Level)
import Runix.Streaming.SSE (StreamingContent)
import UI.OutputHistory (Zipper, OutputItem)

-- | Opaque identifier for a segment (subsection) in the output history
newtype SegmentID = SegmentID Int
  deriving stock (Eq, Ord, Show)

-- | Effect for outputting to agent widgets
--
-- Semantic operations that abstract away the storage format:
-- - AddMessage: add a message to history (user or agent)
-- - StreamChunk: append streaming chunk (thinking or response)
-- - LogMessage: add a log entry
-- - AgentError: indicate an error occurred
-- - CreateSegment: create a new subsection for sub-agent output, returns ID
-- - UpdateSegment: modify a specific segment's zipper
-- - ReplaceHistory: merge new message history (preserves logs, subsections)
data AgentWidgets msg (m :: Type -> Type) a where
  -- | Add a message to the history
  AddMessage :: msg -> AgentWidgets msg m ()

  -- | Append a streaming chunk (type is in StreamingContent: Text or Reasoning)
  StreamChunk :: StreamingContent -> AgentWidgets msg m ()

  -- | Add a log message with severity level
  LogMessage :: Level -> Text -> AgentWidgets msg m ()

  -- | Indicate an error occurred
  AgentError :: Text -> AgentWidgets msg m ()

  -- | Create a new segment (subsection) for sub-agent output
  CreateSegment :: AgentWidgets msg m SegmentID

  -- | Update a specific segment's zipper
  UpdateSegment :: SegmentID -> (Zipper (OutputItem msg) -> Zipper (OutputItem msg)) -> AgentWidgets msg m ()

  -- | Replace message history (merges, preserving logs and subsections)
  ReplaceHistory :: [msg] -> AgentWidgets msg m ()

-- | Smart constructors for AgentWidgets operations
addMessage :: forall msg r. Member (AgentWidgets msg) r => msg -> Sem r ()
addMessage msg = send @(AgentWidgets msg) (AddMessage msg)

streamChunk :: forall msg r. Member (AgentWidgets msg) r => StreamingContent -> Sem r ()
streamChunk content = send @(AgentWidgets msg) (StreamChunk content)

logMessage :: forall msg r. Member (AgentWidgets msg) r => Level -> Text -> Sem r ()
logMessage level text = send @(AgentWidgets msg) (LogMessage level text)

agentError :: forall msg r. Member (AgentWidgets msg) r => Text -> Sem r ()
agentError text = send @(AgentWidgets msg) (AgentError text)

createSegment :: forall msg r. Member (AgentWidgets msg) r => Sem r SegmentID
createSegment = send @(AgentWidgets msg) CreateSegment

updateSegment :: forall msg r. Member (AgentWidgets msg) r => SegmentID -> (Zipper (OutputItem msg) -> Zipper (OutputItem msg)) -> Sem r ()
updateSegment segId f = send @(AgentWidgets msg) (UpdateSegment segId f)

replaceHistory :: forall msg r. Member (AgentWidgets msg) r => [msg] -> Sem r ()
replaceHistory msgs = send @(AgentWidgets msg) (ReplaceHistory msgs)
