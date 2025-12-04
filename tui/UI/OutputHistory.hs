{-# LANGUAGE GADTs #-}

-- | Unified output history for the TUI
--
-- This module provides a single timeline of all observable events:
-- messages, logs, streaming updates, etc. The timeline uses a zipper
-- structure to efficiently support navigation, editing, and rendering.
module UI.OutputHistory
  ( -- * Core Types
    OutputItem(..)
  , OutputHistoryZipper(..)
  , DisplayFilter(..)
    -- * Zipper Operations
  , emptyZipper
  , zipperToList
  , listToZipper
  , focusNewest
  , focusOldest
  , moveNewer
  , moveOlder
  , insertItem
  , updateCurrent
  , extractMessages
    -- * Filters
  , defaultFilter
  , shouldDisplay
    -- * Rendering
  , renderItem
  , renderItemMarkdown
  , renderItemRaw
    -- * Merge logic
  , mergeOutputMessages
    -- * Legacy compatibility (to be removed)
  , OutputMessage(..)
  , RenderedMessage(..)
  , OutputHistory
  , renderMessage
  , renderMessageList
  , renderOutputMessage
  , renderOutputMessageRaw
  , renderOutputMessages
  -- , patchOutputHistory  -- Removed: not used
  , addLog
  , addSystemEvent
  , addStreamingChunk
  , addStreamingReasoning
  , addToolExecution
  , removeStreamingChunks
  , splitHistory
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import UI.Rendering (markdownToWidgets, markdownToWidgetsWithIndent)
import UI.Attributes (logInfoAttr, logWarningAttr, logErrorAttr)
import Brick.Types (Widget)
import Brick.Widgets.Core (txt, padLeft, (<+>), vBox, withAttr)
import Brick.Widgets.Core (Padding(..))
import Runix.Logging.Effects (Level(..))

--------------------------------------------------------------------------------
-- New Zipper-Based Types
--------------------------------------------------------------------------------

-- | A single item in the output history timeline
-- Parametrized over message type to store actual typed messages
data OutputItem msg
  = MessageItem msg           -- ^ Conversation message (typed)
  | LogItem Level Text        -- ^ Log entry
  | StreamingChunkItem Text   -- ^ Streaming text chunk
  | StreamingReasoningItem Text  -- ^ Streaming reasoning chunk
  | SystemEventItem Text      -- ^ System event notification
  | ToolExecutionItem Text    -- ^ Tool execution indicator
  deriving (Eq, Show, Ord)

-- | Zipper structure for output history
-- Structure: back (newer) <- current -> front (older)
-- This allows efficient navigation and modification of focused elements
data OutputHistoryZipper msg = OutputHistoryZipper
  { zipperBack :: [OutputItem msg]      -- ^ Newer items (reverse chronological)
  , zipperCurrent :: Maybe (OutputItem msg)  -- ^ Focused/streaming item
  , zipperFront :: [OutputItem msg]     -- ^ Older items (reverse chronological)
  }

--------------------------------------------------------------------------------
-- Zipper Operations
--------------------------------------------------------------------------------

-- | Create an empty zipper
emptyZipper :: OutputHistoryZipper msg
emptyZipper = OutputHistoryZipper [] Nothing []

-- | Convert zipper to a list (newest first)
zipperToList :: OutputHistoryZipper msg -> [OutputItem msg]
zipperToList (OutputHistoryZipper back current front) =
  back ++ maybe [] (:[]) current ++ front

-- | Create a zipper from a list (items in newest-first order)
-- Focus will be on the last (newest) item
listToZipper :: [OutputItem msg] -> OutputHistoryZipper msg
listToZipper [] = emptyZipper
listToZipper items = OutputHistoryZipper items Nothing []

-- | Focus on the newest item (move to head of back)
focusNewest :: OutputHistoryZipper msg -> OutputHistoryZipper msg
focusNewest z@(OutputHistoryZipper [] Nothing []) = z  -- Empty zipper
focusNewest z@(OutputHistoryZipper [] Nothing _) = z   -- Already at newest (empty back)
focusNewest (OutputHistoryZipper [] (Just cur) front) =
  -- Current is newest, stay here
  OutputHistoryZipper [] (Just cur) front
focusNewest (OutputHistoryZipper (b:bs) current front) =
  -- Move newest from back to current, push old current to front
  let front' = maybe front (:front) current
  in OutputHistoryZipper bs (Just b) front'

-- | Focus on the oldest item
focusOldest :: OutputHistoryZipper msg -> OutputHistoryZipper msg
focusOldest z@(OutputHistoryZipper _ Nothing []) = z  -- Already at oldest (empty front)
focusOldest (OutputHistoryZipper back (Just cur) []) =
  -- Current is oldest, stay here
  OutputHistoryZipper back (Just cur) []
focusOldest (OutputHistoryZipper back current (f:fs)) =
  -- Move oldest from front to current, push old current to back
  let back' = maybe back (:back) current
  in OutputHistoryZipper back' (Just f) fs

-- | Move focus to newer item (toward back)
moveNewer :: OutputHistoryZipper msg -> OutputHistoryZipper msg
moveNewer z@(OutputHistoryZipper [] _ _) = z  -- No newer items
moveNewer (OutputHistoryZipper (b:bs) current front) =
  let front' = maybe front (:front) current
  in OutputHistoryZipper bs (Just b) front'

-- | Move focus to older item (toward front)
moveOlder :: OutputHistoryZipper msg -> OutputHistoryZipper msg
moveOlder z@(OutputHistoryZipper _ _ []) = z  -- No older items
moveOlder (OutputHistoryZipper back current (f:fs)) =
  let back' = maybe back (:back) current
  in OutputHistoryZipper back' (Just f) fs

-- | Insert a new item at the newest position (prepend to back)
insertItem :: OutputItem msg -> OutputHistoryZipper msg -> OutputHistoryZipper msg
insertItem item (OutputHistoryZipper back current front) =
  OutputHistoryZipper (item:back) current front

-- | Update the current focused item
updateCurrent :: OutputItem msg -> OutputHistoryZipper msg -> OutputHistoryZipper msg
updateCurrent item (OutputHistoryZipper back _ front) =
  OutputHistoryZipper back (Just item) front

-- | Extract typed messages from zipper (filters out logs, streaming, etc.)
-- Returns messages in oldest-first order (ready to pass to agent)
extractMessages :: OutputHistoryZipper msg -> [msg]
extractMessages zipper =
  let items = zipperToList zipper
      extractMsg (MessageItem msg) = Just msg
      extractMsg _ = Nothing
  in reverse $ foldr (\item acc -> maybe acc (:acc) (extractMsg item)) [] items

--------------------------------------------------------------------------------
-- Rendering Functions
--------------------------------------------------------------------------------

-- | Render an OutputItem with markdown formatting
-- Takes a function to render msg to Text
renderItemMarkdown :: forall msg n. (msg -> Text) -> OutputItem msg -> [Widget n]
renderItemMarkdown renderMsg item = case item of
  MessageItem msg ->
    let text = renderMsg msg
        contentWidgets = case T.stripPrefix "You:\n  " text of
          Just content ->
            let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
            in combineMarkerWithContent "<" widgets
          Nothing -> case T.stripPrefix "Agent:\n  " text of
            Just content ->
              let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
              in combineMarkerWithContent ">" widgets
            Nothing -> case T.stripPrefix "[Agent reasoning]:\n  " text of
              Just content ->
                let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
                in combineMarkerWithContent "?" widgets
              Nothing -> case T.stripPrefix "Agent (tool call):\n  " text of
                Just content ->
                  -- Render tool calls as plain text, not markdown
                  let plainText = T.replace "\n  " "\n" content
                  in combineMarkerWithContent "T" [padLeft (Pad 1) (txt plainText)]
                Nothing -> case T.stripPrefix "Tool result:\n  " text of
                  Just content ->
                    -- Render tool results as plain text, not markdown
                    let plainText = T.replace "\n  " "\n" content
                    in combineMarkerWithContent "R" [padLeft (Pad 1) (txt plainText)]
                  Nothing -> markdownToWidgetsWithIndent 1 text
    in txt " " : contentWidgets ++ [txt " "]

  LogItem level msg ->
    let (marker, attr) = case level of
          Info -> ("I ", logInfoAttr)
          Warning -> ("W ", logWarningAttr)
          Error -> ("E ", logErrorAttr)
    in [withAttr attr (txt marker) <+> txt msg]

  StreamingChunkItem text ->
    let contentWidgets = markdownToWidgetsWithIndent 1 text
    in combineMarkerWithContent "}" contentWidgets

  StreamingReasoningItem text ->
    let contentWidgets = markdownToWidgetsWithIndent 1 text
    in combineMarkerWithContent "~" contentWidgets

  SystemEventItem msg ->
    [txt "S " <+> txt msg]

  ToolExecutionItem name ->
    [txt "T " <+> txt name]
  where
    combineMarkerWithContent :: Text -> [Widget n] -> [Widget n]
    combineMarkerWithContent marker [] = [txt marker]
    combineMarkerWithContent marker (first:rest) = (txt marker <+> first) : rest

-- | Render an OutputItem as raw text (no markdown processing)
-- Takes a function to render msg to Text
renderItemRaw :: forall msg n. (msg -> Text) -> OutputItem msg -> [Widget n]
renderItemRaw renderMsg item = case item of
  MessageItem msg ->
    let text = renderMsg msg
    in case T.stripPrefix "You:\n  " text of
      Just content ->
        [txt "<" <+> padLeft (Pad 1) (txt (T.replace "\n  " "\n" content))]
      Nothing -> case T.stripPrefix "Agent:\n  " text of
        Just content ->
          [txt ">" <+> padLeft (Pad 1) (txt (T.replace "\n  " "\n" content))]
        Nothing -> case T.stripPrefix "[Agent reasoning]:\n  " text of
          Just content ->
            [txt "?" <+> padLeft (Pad 1) (txt (T.replace "\n  " "\n" content))]
          Nothing -> [padLeft (Pad 1) (txt text)]

  LogItem level msg ->
    let marker = case level of
          Info -> "I "
          Warning -> "W "
          Error -> "E "
    in [txt marker <+> txt msg]

  StreamingChunkItem text ->
    [txt "}" <+> padLeft (Pad 1) (txt text)]

  StreamingReasoningItem text ->
    [txt "~" <+> padLeft (Pad 1) (txt text)]

  SystemEventItem msg ->
    [txt "S " <+> txt msg]

  ToolExecutionItem name ->
    [txt "T " <+> txt name]

-- | Polymorphic render function - choose markdown or raw
renderItem :: forall msg n. Bool -> (msg -> Text) -> OutputItem msg -> [Widget n]
renderItem useMarkdown renderMsg item =
  if useMarkdown
    then renderItemMarkdown renderMsg item
    else renderItemRaw renderMsg item

--------------------------------------------------------------------------------
-- Legacy Types (for backward compatibility during migration)
--------------------------------------------------------------------------------

-- | A single entry in the output timeline
data OutputMessage
  = ConversationMessage Int Text
  | LogEntry Level Text
  | StreamingChunk Text
  | StreamingReasoning Text
  | SystemEvent Text
  | ToolExecution Text
  deriving stock (Eq, Show, Ord)

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
shouldDisplay filt (StreamingReasoning _) = showStreaming filt  -- Treat reasoning same as streaming
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
  -- Extract "You:", "Agent:", or "[Agent reasoning]:" prefix and render separately
  -- The content after the prefix has "  " indent that we need to strip
  let contentWidgets = case T.stripPrefix "You:\n  " text of
        Just content ->
          let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
          in combineMarkerWithContent "<" widgets
        Nothing -> case T.stripPrefix "Agent:\n  " text of
          Just content ->
            let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
            in combineMarkerWithContent ">" widgets
          Nothing -> case T.stripPrefix "[Agent reasoning]:\n  " text of
            Just content ->
              let widgets = markdownToWidgetsWithIndent 1 (T.replace "\n  " "\n" content)
              in combineMarkerWithContent "?" widgets  -- Use "?" marker for reasoning
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
renderOutputMessage (StreamingReasoning text) =
  let contentWidgets = markdownToWidgetsWithIndent 1 text
  in combineMarkerWithContent "~" contentWidgets  -- Use "~" marker for streaming reasoning
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
      Nothing -> case T.stripPrefix "[Agent reasoning]:\n  " text of
        Just content ->
          [txt "?" <+> padLeft (Pad 1) (txt (T.replace "\n  " "\n" content))]
        Nothing -> [padLeft (Pad 1) (txt text)]
renderOutputMessageRaw (LogEntry level msg) =
  let marker = case level of
                 Info -> "I "
                 Warning -> "W "
                 Error -> "E "
  in [txt marker <+> txt msg]
renderOutputMessageRaw (StreamingChunk text) =
  [txt "}" <+> padLeft (Pad 1) (txt text)]
renderOutputMessageRaw (StreamingReasoning text) =
  [txt "~" <+> padLeft (Pad 1) (txt text)]
renderOutputMessageRaw (SystemEvent msg) = [txt "S " <+> txt msg]
renderOutputMessageRaw (ToolExecution name) = [txt "T " <+> txt name]

-- | Check if an OutputMessage is a conversation message
isConversationMessage :: OutputMessage -> Bool
isConversationMessage (ConversationMessage _ _) = True
isConversationMessage _ = False

-- | Extract conversation message texts from a list
extractConvTexts :: [OutputMessage] -> [Text]
extractConvTexts = foldr (\msg acc -> case msg of
                            ConversationMessage _ t -> t : acc
                            _ -> acc) []

-- | Check if an OutputItem is a message item
isMessageItem :: OutputItem msg -> Bool
isMessageItem (MessageItem _) = True
isMessageItem _ = False

-- | Extract message values from a list of OutputItems (for merge comparison)
extractMessageItems :: Eq msg => [OutputItem msg] -> [msg]
extractMessageItems = foldr (\item acc -> case item of
                                MessageItem m -> m : acc
                                _ -> acc) []

-- | Merge two lists of OutputMessages
--
-- Contract:
--   newItems: newest-first list of conversation messages only
--   oldItems: newest-first list of all OutputMessages (conversations + logs + etc)
--   Returns: newest-first merged list
--
-- Behavior:
--   - All conversation messages from newItems replace those in oldItems
--   - All non-conversation messages from oldItems are preserved in their relative positions
--   - If newItems contains non-conversation messages (contract violation), they are prepended
--
-- Invariants:
--   - All new conversation messages appear in result
--   - All old non-conversation messages are preserved
--   - Relative order of non-conversation messages is maintained
mergeOutputMessages :: Eq msg => [OutputItem msg] -> [OutputItem msg] -> [OutputItem msg]
mergeOutputMessages [] oldItems =
  -- No new items: keep all non-message items, discard old messages
  filter (not . isMessageItem) oldItems

mergeOutputMessages newItems [] =
  -- No old items: just use new items
  newItems

mergeOutputMessages newItems@(newItem:restNew) (oldItem:restOld) =
  case (newItem, oldItem) of
    (MessageItem newMsg, MessageItem oldMsg)
      | newMsg == oldMsg ->
          -- Messages match: collect any logs after old message, keep them
          let (logsAfter, restAfterLogs) = span (not . isMessageItem) restOld
          in newItem : logsAfter ++ mergeOutputMessages restNew restAfterLogs
      | newMsg `elem` extractMessageItems restOld ->
          -- New message exists later in old: old message was deleted, skip it
          mergeOutputMessages newItems restOld
      | oldMsg `elem` extractMessageItems restNew ->
          -- Old message exists later in new: new message is insertion, add it
          newItem : mergeOutputMessages restNew (oldItem:restOld)
      | otherwise ->
          -- Neither exists later: new message is addition
          newItem : mergeOutputMessages restNew (oldItem:restOld)

    (MessageItem newMsg, _) ->
      -- Old item is not a message (log, etc)
      -- Collect all non-message items until next message
      let (nonMsgItems, rest) = span (not . isMessageItem) (oldItem:restOld)
      in case rest of
        [] ->
          -- No more messages: keep logs, add remaining new
          nonMsgItems ++ mergeOutputMessages newItems []
        (nextMsg:restAfter) ->
          -- Check if new message matches the message after the logs
          case (newItem, nextMsg) of
            (MessageItem newM, MessageItem oldM)
              | newM == oldM ->
                  -- Match: logs came before msg in old (newer), so keep them first
                  nonMsgItems ++ newItem : mergeOutputMessages restNew restAfter
              | oldM `elem` extractMessageItems restNew ->
                  -- Old message exists later in new: defer logs, process new items first
                  newItem : mergeOutputMessages restNew (nonMsgItems ++ [nextMsg] ++ restAfter)
              | newM `elem` extractMessageItems restAfter ->
                  -- New message exists later in old: skip old message with its logs
                  mergeOutputMessages newItems restAfter
              | otherwise ->
                  -- Neither exists later: new is insertion, keep logs with old
                  newItem : nonMsgItems ++ nextMsg : mergeOutputMessages restNew restAfter

    _ ->
      -- New item is not a message (shouldn't happen)
      newItem : mergeOutputMessages restNew (oldItem:restOld)

-- | Patch the output history with a new message list
-- LEGACY: Not used, kept for reference only
-- patchOutputHistory :: forall model provider n.
--                       [Message model provider]  -- ^ New messages from this turn (oldest first)
--                    -> (Message model provider -> Text)  -- ^ Message renderer
--                    -> OutputHistory n  -- ^ Current output history (newest first)
--                    -> OutputHistory n  -- ^ Patched output history (newest first)
-- patchOutputHistory newMessages renderMsg currentOutput =
--   let
--     -- Remove all streaming chunks first
--     withoutStreaming = removeStreamingChunks currentOutput
--
--     -- Extract OutputMessages from current output (already newest-first)
--     oldOutputMsgs = map rmMessage withoutStreaming
--
--     -- Reverse new messages to newest-first, then convert to OutputMessages
--     newMessagesNewestFirst = reverse newMessages
--     newOutputMsgs = map (\msg -> ConversationMessage 0 (renderMsg msg)) newMessagesNewestFirst
--
--     -- Merge at OutputMessage level (both are newest-first)
--     mergedMsgs = mergeOutputMessages newOutputMsgs oldOutputMsgs
--
--   in renderMessageList mergedMsgs

-- | Add a log entry to the output history (cons to front - newest first)
addLog :: forall n. Level -> Text -> OutputHistory n -> OutputHistory n
addLog level msg output = renderMessage (LogEntry level msg) : output

-- | Add a system event to the output history (cons to front - newest first)
addSystemEvent :: forall n. Text -> OutputHistory n -> OutputHistory n
addSystemEvent msg output = renderMessage (SystemEvent msg) : output

-- | Add or update streaming chunk in the output history
addStreamingChunk :: forall n. Text -> OutputHistory n -> OutputHistory n
addStreamingChunk chunk output =
  case output of
    (RenderedMessage (StreamingChunk existing) _ _ : rest) ->
      renderMessage (StreamingChunk (existing <> chunk)) : rest
    _ ->
      renderMessage (StreamingChunk chunk) : output

-- | Add or update streaming reasoning chunk in the output history
addStreamingReasoning :: forall n. Text -> OutputHistory n -> OutputHistory n
addStreamingReasoning chunk output =
  case output of
    (RenderedMessage (StreamingReasoning existing) _ _ : rest) ->
      renderMessage (StreamingReasoning (existing <> chunk)) : rest
    _ ->
      renderMessage (StreamingReasoning chunk) : output

-- | Remove all streaming chunks from output history
-- Stops at first ConversationMessage (can't be any streaming before that)
removeStreamingChunks :: forall n. OutputHistory n -> OutputHistory n
removeStreamingChunks = go []
  where
    go acc [] = reverse acc
    go acc (msg@(RenderedMessage (ConversationMessage _ _) _ _) : rest) =
      -- Hit conversation message, keep it and everything after
      reverse acc ++ (msg : rest)
    go acc (RenderedMessage (StreamingChunk _) _ _ : rest) =
      -- Remove streaming chunk, continue
      go acc rest
    go acc (RenderedMessage (StreamingReasoning _) _ _ : rest) =
      -- Remove streaming reasoning placeholder, continue
      go acc rest
    go acc (msg : rest) =
      -- Keep other messages (logs, system events, etc)
      go (msg : acc) rest

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
