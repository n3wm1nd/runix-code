{-# LANGUAGE GADTs #-}

-- | Unified output history for the TUI
--
-- This module provides a single timeline of all observable events:
-- messages, logs, streaming updates, etc. The timeline can be patched
-- when the underlying message history changes while preserving logs
-- and other events.
module UI.OutputHistory
  ( -- * Types
    OutputMessage(..)
  , RenderedMessage(..)
  , OutputHistory
  , LogLevel(..)
  , DisplayFilter(..)
    -- * Filters
  , defaultFilter
  , shouldDisplay
    -- * Smart Constructors
  , renderMessage
  , renderMessageList
    -- * Rendering (legacy)
  , renderOutputMessage
  , renderOutputMessageRaw
  , renderOutputMessages
    -- * History Management
  , patchOutputHistory
  , addLog
  , addSystemEvent
  , addStreamingChunk
  , addToolExecution
  , removeStreamingChunks
    -- * Performance Optimization
  , splitHistory
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import UniversalLLM.Core.Types (Message)
import UI.Rendering (markdownToWidgets, markdownToWidgetsWithIndent)
import Brick.Types (Widget)
import Brick.Widgets.Core (txt, padLeft, (<+>), vBox)
import Brick.Widgets.Core (Padding(..))

-- | A single entry in the output timeline
data OutputMessage where
  -- | A message from the conversation history
  -- Contains an index to track which message this represents
  ConversationMessage :: Int -> Text -> OutputMessage

  -- | A log entry (info, warning, error)
  LogEntry :: LogLevel -> Text -> OutputMessage

  -- | Streaming response (incomplete assistant message)
  StreamingChunk :: Text -> OutputMessage

  -- | System event (status change, etc.)
  SystemEvent :: Text -> OutputMessage

  -- | Tool execution indicator
  ToolExecution :: Text -> OutputMessage

data LogLevel = Info | Warning | Error
  deriving (Eq, Show)

-- | A rendered message with cached widgets
-- Stores both markdown and raw renderings to avoid re-parsing on mode switch
data RenderedMessage n = RenderedMessage
  { rmMessage :: OutputMessage           -- ^ The original message data
  , rmMarkdownWidgets :: [Widget n]      -- ^ Pre-rendered markdown widgets
  , rmRawWidgets :: [Widget n]          -- ^ Pre-rendered raw text widgets
  }

-- | Output history type - list with newest messages first for O(1) append
-- TODO: Replace with zipper in future for efficient scrolling
type OutputHistory n = [RenderedMessage n]

-- | Display filters - what to show in the UI
data DisplayFilter = DisplayFilter
  { showMessages :: Bool      -- ^ Show conversation messages
  , showLogs :: Bool          -- ^ Show log entries
  , showStreaming :: Bool     -- ^ Show streaming chunks
  , showSystemEvents :: Bool  -- ^ Show system events
  , showToolCalls :: Bool     -- ^ Show tool executions
  }

-- | Default filter - show everything
defaultFilter :: DisplayFilter
defaultFilter = DisplayFilter
  { showMessages = True
  , showLogs = True
  , showStreaming = True
  , showSystemEvents = False  -- System events off by default
  , showToolCalls = True
  }

-- | Check if an output message should be displayed
shouldDisplay :: DisplayFilter -> OutputMessage -> Bool
shouldDisplay filt (ConversationMessage _ _) = showMessages filt
shouldDisplay filt (LogEntry _ _) = showLogs filt
shouldDisplay filt (StreamingChunk _) = showStreaming filt
shouldDisplay filt (SystemEvent _) = showSystemEvents filt
shouldDisplay filt (ToolExecution _) = showToolCalls filt

--------------------------------------------------------------------------------
-- Smart Constructors - Create RenderedMessages with cached widgets
--------------------------------------------------------------------------------

-- | Create a RenderedMessage by rendering both markdown and raw versions
renderMessage :: forall n. OutputMessage -> RenderedMessage n
renderMessage msg = RenderedMessage
  { rmMessage = msg
  , rmMarkdownWidgets = renderOutputMessage msg
  , rmRawWidgets = renderOutputMessageRaw msg
  }

-- | Convert a list of OutputMessages to RenderedMessages (newest first)
-- Input list should have newest messages first
renderMessageList :: forall n. [OutputMessage] -> OutputHistory n
renderMessageList = map renderMessage

--------------------------------------------------------------------------------
-- Legacy Rendering Functions
--------------------------------------------------------------------------------

-- | Combine a marker with the first line of content, keeping rest as-is
-- Used for adding column 0 markers to messages
combineMarkerWithContent :: forall n. Text -> [Widget n] -> [Widget n]
combineMarkerWithContent marker [] = [txt marker]
combineMarkerWithContent marker (first:rest) = (txt marker <+> first) : rest

-- | Render an output message to Brick widgets with markdown formatting
-- Message content is indented by 1 space, with markers at column 0
-- Adds one blank line before and after conversation messages
renderOutputMessage :: forall n. OutputMessage -> [Widget n]
renderOutputMessage (ConversationMessage _ text) =
  -- Extract "You:" or "Agent:" prefix and render separately
  -- The content after the prefix has "  " indent that we need to strip
  let contentWidgets = case T.stripPrefix "You:\n  " text of
        Just content ->
          let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
          in combineMarkerWithContent "<" widgets
        Nothing -> case T.stripPrefix "Agent:\n  " text of
          Just content ->
            let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
            in combineMarkerWithContent ">" widgets
          Nothing -> markdownToWidgetsWithIndent 1 text  -- Fallback
  in txt " " : contentWidgets ++ [txt " "]  -- Blank line before and after

renderOutputMessage (LogEntry level msg) =
  let marker = case level of
                 Info -> "I "
                 Warning -> "W "
                 Error -> "E "
  in [txt marker <+> vBox (markdownToWidgets msg)]
renderOutputMessage (StreamingChunk text) =
  let contentWidgets = markdownToWidgetsWithIndent 1 text
  in combineMarkerWithContent "}" contentWidgets
renderOutputMessage (SystemEvent msg) =
  [txt "S " <+> vBox (markdownToWidgets msg)]
renderOutputMessage (ToolExecution name) =
  [txt "T " <+> vBox (markdownToWidgets name)]

-- | Render a list of output messages with appropriate spacing
-- Adds blank lines before and after conversation messages
renderOutputMessages :: forall n. (OutputMessage -> [Widget n]) -> [OutputMessage] -> [Widget n]
renderOutputMessages renderFunc messages = go messages
  where
    go :: [OutputMessage] -> [Widget n]
    go [] = []
    go (msg:rest) =
      case msg of
        ConversationMessage _ _ ->
          -- Add blank line before, render message, add blank line after
          -- Use txt " " instead of emptyWidget to ensure it takes vertical space
          txt " " : renderFunc msg ++ [txt " "] ++ go rest
        StreamingChunk _ ->
          -- Add blank line before streaming chunk (same as conversation message)
          txt " " : renderFunc msg ++ [txt " "] ++ go rest
        _ ->
          -- No spacing for logs, system events, tool executions
          renderFunc msg ++ go rest

-- | Render an output message as raw text (no markdown processing)
-- Note: Spacing is handled by renderOutputMessages, not here
renderOutputMessageRaw :: forall n. OutputMessage -> [Widget n]
renderOutputMessageRaw (ConversationMessage _ text) =
  case T.stripPrefix "You:\n  " text of
    Just content ->
      [txt "<" <+> padLeft (Pad 1) (txt (T.replace "\n  " "\n" content))]
    Nothing -> case T.stripPrefix "Agent:\n  " text of
      Just content ->
        [txt ">" <+> padLeft (Pad 1) (txt (T.replace "\n  " "\n" content))]
      Nothing -> [padLeft (Pad 1) (txt text)]
renderOutputMessageRaw (LogEntry level msg) =
  let marker = case level of
                 Info -> "I "
                 Warning -> "W "
                 Error -> "E "
  in [txt marker <+> txt msg]
renderOutputMessageRaw (StreamingChunk text) =
  [txt "}" <+> padLeft (Pad 1) (txt text)]
renderOutputMessageRaw (SystemEvent msg) = [txt "S " <+> txt msg]
renderOutputMessageRaw (ToolExecution name) = [txt "T " <+> txt name]

-- | Patch the output history with a new message list
--
-- Remove streaming, figure out which messages are NEW, prepend them
-- Logs stay in place chronologically
--
-- Note: Input message list is oldest-first (from agent), output history is newest-first
patchOutputHistory :: forall model provider n.
                      [Message model provider]  -- ^ New message list (oldest first)
                   -> (Message model provider -> Text)  -- ^ Message renderer
                   -> OutputHistory n  -- ^ Current output history (newest first)
                   -> OutputHistory n  -- ^ Patched output history (newest first)
patchOutputHistory newMessages renderMsg currentOutput =
  let
    -- Remove streaming chunks
    withoutStreaming = removeStreamingChunks currentOutput

    -- Count how many conversation messages are already in output
    existingCount = length [() | m <- withoutStreaming, isConvMsg (rmMessage m)]

    -- Only render NEW messages (messages beyond what's already there)
    newMsgsToRender = drop existingCount newMessages
    newRendered = [renderMessage (ConversationMessage idx (renderMsg msg))
                   | (idx, msg) <- zip [existingCount..] newMsgsToRender]

    -- Prepend new messages (reversed to newest-first) to existing output
  in reverse newRendered ++ withoutStreaming

  where
    isConvMsg :: OutputMessage -> Bool
    isConvMsg (ConversationMessage _ _) = True
    isConvMsg _ = False

-- | Check if an output message is a conversation message
isConversationMessage :: OutputMessage -> Bool
isConversationMessage (ConversationMessage _ _) = True
isConversationMessage _ = False

-- | Add a log entry to the output history (cons to front - newest first)
addLog :: forall n. LogLevel -> Text -> OutputHistory n -> OutputHistory n
addLog level msg output = renderMessage (LogEntry level msg) : output

-- | Add a system event to the output history (cons to front - newest first)
addSystemEvent :: forall n. Text -> OutputHistory n -> OutputHistory n
addSystemEvent msg output = renderMessage (SystemEvent msg) : output

-- | Add or update streaming chunk in the output history
-- If the first entry (newest) is a StreamingChunk, update it
-- Otherwise, create a new StreamingChunk entry at front
addStreamingChunk :: forall n. Text -> OutputHistory n -> OutputHistory n
addStreamingChunk chunk output =
  case output of
    (RenderedMessage (StreamingChunk existing) _ _ : rest) ->
      -- Update existing streaming chunk at front
      renderMessage (StreamingChunk (existing <> chunk)) : rest
    _ ->
      -- Create new streaming chunk at front
      renderMessage (StreamingChunk chunk) : output

-- | Remove all streaming chunks from output history
-- Used when the final response arrives to replace the preview
removeStreamingChunks :: forall n. OutputHistory n -> OutputHistory n
removeStreamingChunks = filter (not . isStreamingChunk . rmMessage)
  where
    isStreamingChunk (StreamingChunk _) = True
    isStreamingChunk _ = False

-- | Add a tool execution indicator (cons to front - newest first)
addToolExecution :: forall n. Text -> OutputHistory n -> OutputHistory n
addToolExecution name output = renderMessage (ToolExecution name) : output

--------------------------------------------------------------------------------
-- Performance Optimization
--------------------------------------------------------------------------------

-- | Split output history into stable/transient parts for rendering optimization
-- For performance optimization: stable messages can be cached, transient content re-rendered
--
-- Returns: (stable history to cache, transient content to re-render each frame)
--
-- Everything up to and including the most recent ASSISTANT message is stable (cached).
-- Everything after (logs during streaming, streaming chunks) is transient (re-rendered).
--
splitHistory :: forall n. OutputHistory n -> (OutputHistory n, OutputHistory n)
splitHistory history = (stableHistory, transientHistory)
  where
    -- Find the first assistant message (newest-first traversal)
    -- Everything BEFORE it is transient (logs, streaming since assistant's response)
    -- Everything FROM it onwards is stable (assistant msg + older history with logs)
    (transientHistory, stableHistory) = break isAssistantMessage history

    isAssistantMessage :: RenderedMessage n -> Bool
    isAssistantMessage m = case rmMessage m of
      ConversationMessage _ text -> "Agent:" `T.isPrefixOf` text
      _ -> False
