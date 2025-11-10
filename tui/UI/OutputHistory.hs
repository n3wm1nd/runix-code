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
  , LogLevel(..)
  , DisplayFilter(..)
    -- * Filters
  , defaultFilter
  , shouldDisplay
    -- * Rendering
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

-- | Combine a marker with the first line of content, keeping rest as-is
-- Used for adding column 0 markers to messages
combineMarkerWithContent :: forall n. Text -> [Widget n] -> [Widget n]
combineMarkerWithContent marker [] = [txt marker]
combineMarkerWithContent marker (first:rest) = (txt marker <+> first) : rest

-- | Render an output message to Brick widgets with markdown formatting
-- Message content is indented by 1 space, with markers at column 0
-- Note: Spacing is handled by renderOutputMessages, not here
renderOutputMessage :: forall n. OutputMessage -> [Widget n]
renderOutputMessage (ConversationMessage _ text) =
  -- Extract "You:" or "Agent:" prefix and render separately
  -- The content after the prefix has "  " indent that we need to strip
  case T.stripPrefix "You:\n  " text of
    Just content ->
      let contentWidgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
      in combineMarkerWithContent "<" contentWidgets
    Nothing -> case T.stripPrefix "Agent:\n  " text of
      Just content ->
        let contentWidgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
        in combineMarkerWithContent ">" contentWidgets
      Nothing -> markdownToWidgetsWithIndent 1 text  -- Fallback

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
-- Strategy:
-- 1. Remove all streaming chunks (they were just a preview)
-- 2. Extract current conversation messages and their indices
-- 3. Compare with new messages
-- 4. Keep matching prefix unchanged
-- 5. For diverging messages: replace old messages but keep non-message entries (logs, etc.)
-- 6. Preserve all non-message entries throughout the history
patchOutputHistory :: forall model provider.
                      [Message model provider]  -- ^ New message list
                   -> (Message model provider -> Text)  -- ^ Message renderer
                   -> [OutputMessage]  -- ^ Current output history
                   -> [OutputMessage]  -- ^ Patched output history
patchOutputHistory newMessages renderMsg currentOutput =
  let currentOutputClean = removeStreamingChunks currentOutput
  in patchOutputHistory' newMessages renderMsg currentOutputClean

patchOutputHistory' :: forall model provider.
                      [Message model provider]  -- ^ New message list
                   -> (Message model provider -> Text)  -- ^ Message renderer
                   -> [OutputMessage]  -- ^ Current output history
                   -> [OutputMessage]  -- ^ Patched output history
patchOutputHistory' newMessages renderMsg currentOutput =
  let
    -- Extract conversation messages with their positions in output list
    currentMsgIndices :: [(Int, Int, Text)]  -- (output index, msg index, text)
    currentMsgIndices =
      [(outIdx, msgIdx, msgText)
      | (outIdx, ConversationMessage msgIdx msgText) <- zip [0..] currentOutput]

    -- Render new messages
    newRendered :: [(Int, Text)]  -- (msg index, text)
    newRendered = zip [0..] (map renderMsg newMessages)

    -- Find the longest matching prefix
    matchingPrefix :: Int  -- How many messages match
    matchingPrefix = length $ takeWhile id
      [ msgText == newText
      | ((_, msgIdx, msgText), (msgIdx', newText)) <- zip currentMsgIndices newRendered
      , msgIdx == msgIdx'
      ]

    -- Find where in the output we need to start replacing
    -- Keep everything up to and including the last matching message
    (beforeDivergence, afterDivergence) = splitAtMessageIndex matchingPrefix currentOutput

    -- Extract non-message entries from after the divergence point (keep logs!)
    nonMessageEntries = [entry | entry <- afterDivergence, not (isConversationMessage entry)]

    -- Create new conversation messages
    newMsgOutput =
      [ConversationMessage idx msgText | (idx, msgText) <- drop matchingPrefix newRendered]

  in beforeDivergence ++ nonMessageEntries ++ newMsgOutput

-- | Check if an output message is a conversation message
isConversationMessage :: OutputMessage -> Bool
isConversationMessage (ConversationMessage _ _) = True
isConversationMessage _ = False

-- | Split output history at a message index
-- Returns (keep, drop) where keep contains everything up to and including
-- the last occurrence of the specified message index
splitAtMessageIndex :: Int -> [OutputMessage] -> ([OutputMessage], [OutputMessage])
splitAtMessageIndex targetIdx output =
  let
    -- Find position of last message with index < targetIdx
    lastKeepPos =
      case [i | (i, ConversationMessage idx _) <- zip [0..] output, idx < targetIdx] of
        [] -> 0
        positions -> maximum positions + 1
  in splitAt lastKeepPos output

-- | Add a log entry to the output history
addLog :: LogLevel -> Text -> [OutputMessage] -> [OutputMessage]
addLog level msg output = output ++ [LogEntry level msg]

-- | Add a system event to the output history
addSystemEvent :: Text -> [OutputMessage] -> [OutputMessage]
addSystemEvent msg output = output ++ [SystemEvent msg]

-- | Add or update streaming chunk in the output history
-- If the last entry is a StreamingChunk, append to it
-- Otherwise, create a new StreamingChunk entry
addStreamingChunk :: Text -> [OutputMessage] -> [OutputMessage]
addStreamingChunk chunk output =
  case reverse output of
    (StreamingChunk existing : rest) ->
      -- Append to existing streaming chunk
      reverse (StreamingChunk (existing <> chunk) : rest)
    _ ->
      -- Create new streaming chunk
      output ++ [StreamingChunk chunk]

-- | Remove all streaming chunks from output history
-- Used when the final response arrives to replace the preview
removeStreamingChunks :: [OutputMessage] -> [OutputMessage]
removeStreamingChunks = filter (not . isStreamingChunk)
  where
    isStreamingChunk (StreamingChunk _) = True
    isStreamingChunk _ = False

-- | Add a tool execution indicator
addToolExecution :: Text -> [OutputMessage] -> [OutputMessage]
addToolExecution name output = output ++ [ToolExecution name]
