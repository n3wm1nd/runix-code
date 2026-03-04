{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Unified output history for the TUI
--
-- This module provides a single timeline of all observable events:
-- messages, logs, streaming updates, etc. The timeline uses a zipper
-- structure to efficiently support navigation, editing, and rendering.
module UI.OutputHistory
  ( -- * Core Types
    OutputItem(..)
  , Zipper(..)
  , OutputHistoryZipper
  , DisplayFilter(..)
  , RenderOptions(..)
  , defaultRenderOptions
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
  , appendItem
  , appendItemAndFocus
  , extractMessages
  , onRewoundZipper
  , onForwardUntil
  , onNthSubtree
  , countSubtrees
  , atAddress
  , queryAtAddress
    -- * Filters
  , defaultFilter
  , shouldDisplay
    -- * Rendering
  , renderItem
  , renderZipper
    -- * Merge logic
  , mergeOutputMessages
  , mergeEditedMessages
  , addCompletedToolItems
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
  , addToolExecution
  , splitHistory
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import UI.Rendering (markdownToWidgets, markdownToWidgetsWithIndent)
import UI.Attributes (logInfoAttr, logWarningAttr, logErrorAttr, focusedItemAttr, transparentBgAttr)
import UI.AgentWidgets (AgentStatus(..), StreamingState(..), SubsectionAddr(..))
import Brick.Types (Widget)
import Brick.Widgets.Core (txt, txtWrap, padLeft, (<+>), vBox, withAttr, emptyWidget)
import Brick.Widgets.Core (Padding(..))
import Runix.Logging (Level(..))
import UniversalLLM (Message(..), ToolCall(..), ToolResult(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM

--------------------------------------------------------------------------------
-- New Zipper-Based Types
--------------------------------------------------------------------------------

-- | Rendering options for OutputItems
data RenderOptions = RenderOptions
  { useMarkdown :: Bool  -- ^ Render markdown in user/assistant text
  } deriving stock (Eq, Show)

-- | Default rendering options
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions
  { useMarkdown = True
  }

-- | A single item in the output history timeline
-- Parametrized over message type to store actual typed messages
data OutputItem msg
  = MessageItem msg           -- ^ Conversation message (typed)
  | LogItem Level Text        -- ^ Log entry
  | StatusItem AgentStatus    -- ^ Agent status
  | SystemEventItem Text      -- ^ System event notification
  | ToolExecutionItem Text    -- ^ Tool execution indicator
  | CompletedToolItem msg msg -- ^ Completed tool call: tool call + result (for display)
  | SectionItem (Zipper (OutputItem msg))  -- ^ Grouped output from a sub-agent invocation
  deriving stock (Eq, Show, Ord)

-- | Generic zipper structure for navigable lists
-- Structure: back (newer) <- current -> front (older)
-- This allows efficient navigation and modification of focused elements
data Zipper a = Zipper
  { zipperBack :: [a]      -- ^ Newer items (reverse chronological)
  , zipperCurrent :: Maybe a  -- ^ Focused/streaming item
  , zipperFront :: [a]     -- ^ Older items (reverse chronological)
  } deriving stock (Eq, Show, Ord)

-- | Type alias for output history (zipper of OutputItems)
type OutputHistoryZipper msg = Zipper (OutputItem msg)

--------------------------------------------------------------------------------
-- Zipper Operations
--------------------------------------------------------------------------------

-- | Create an empty zipper
emptyZipper :: Zipper a
emptyZipper = Zipper [] Nothing []

-- | Convert zipper to a list (newest first)
zipperToList :: Zipper a -> [a]
zipperToList (Zipper back current front) =
  back ++ maybe [] (:[]) current ++ front

-- | Create a zipper from a list (items in newest-first order)
-- Focus will be on the newest item (head of list), rest go to front
-- Maintains invariant: back is empty, current is newest, front contains older items
-- Note: front stores items in chronological order (second-newest to oldest)
listToZipper :: [a] -> Zipper a
listToZipper [] = emptyZipper
listToZipper (newest:older) = Zipper [] (Just newest) older

-- | Focus on the newest item (move to head of back)
focusNewest :: Zipper a -> Zipper a
focusNewest z@(Zipper [] Nothing []) = z  -- Empty zipper
focusNewest z@(Zipper [] Nothing _) = z   -- Already at newest (empty back)
focusNewest (Zipper [] (Just cur) front) =
  -- Current is newest, stay here
  Zipper [] (Just cur) front
focusNewest (Zipper (b:bs) current front) =
  -- Move newest from back to current, push old current to front
  let front' = maybe front (:front) current
  in Zipper bs (Just b) front'

-- | Focus on the oldest item
focusOldest :: Zipper a -> Zipper a
focusOldest z@(Zipper _ Nothing []) = z  -- Already at oldest (empty front)
focusOldest (Zipper back (Just cur) []) =
  -- Current is oldest, stay here
  Zipper back (Just cur) []
focusOldest (Zipper back current (f:fs)) =
  -- Move oldest from front to current, push old current to back
  let back' = maybe back (:back) current
  in Zipper back' (Just f) fs

-- | Move focus to newer item (toward back)
moveNewer :: Zipper a -> Zipper a
moveNewer z@(Zipper [] _ _) = z  -- No newer items
moveNewer (Zipper (b:bs) current front) =
  let front' = maybe front (:front) current
  in Zipper bs (Just b) front'

-- | Move focus to older item (toward front)
moveOlder :: Zipper a -> Zipper a
moveOlder z@(Zipper _ _ []) = z  -- No older items
moveOlder (Zipper back current (f:fs)) =
  let back' = maybe back (:back) current
  in Zipper back' (Just f) fs

-- | Insert a new item at the newest position (prepend to back)
insertItem :: a -> Zipper a -> Zipper a
insertItem item (Zipper back current front) =
  Zipper (item:back) current front

-- | Update the current focused item
updateCurrent :: a -> Zipper a -> Zipper a
updateCurrent item (Zipper back _ front) =
  Zipper back (Just item) front

-- | Add a new item as current, moving the old current to front
-- This maintains the invariant that back is always empty (newest item is current)
appendItem :: a -> Zipper a -> Zipper a
appendItem item (Zipper back current front) =
  let front' = case current of
        Nothing -> front
        Just c -> c : front
  in Zipper back (Just item) front'

-- | Add a new item as current, moving the old current to back (keeps focus on new item)
-- Use this when you want to immediately work with the newly added item
appendItemAndFocus :: a -> Zipper a -> Zipper a
appendItemAndFocus item (Zipper back current front) =
  let back' = case current of
        Nothing -> back
        Just c -> c : back
  in Zipper back' (Just item) front

-- | Extract typed messages from zipper (filters out logs, streaming, etc.)
-- Recurses into SectionItems to find nested messages.
-- Returns messages in oldest-first order (ready to pass to agent)
extractMessages :: OutputHistoryZipper msg -> [msg]
extractMessages zipper =
  let items = reverse (zipperToList zipper)  -- oldest-first
  in concatMap extractFromItem items
  where
    extractFromItem (MessageItem msg) = [msg]
    extractFromItem (SectionItem subZipper) = extractMessages subZipper
    extractFromItem _ = []

-- | Run an operation on a rewound zipper, then restore focus
-- Recursively rewinds to oldest, applies operation, then moves forward same number of steps
onRewoundZipper :: (Zipper a -> Zipper a) -> Zipper a -> Zipper a
onRewoundZipper f zipper =
  case zipperFront zipper of
    [] -> f zipper  -- Already at oldest, apply operation
    _ ->
      let rewound = moveOlder zipper           -- Step back one
          modified = onRewoundZipper f rewound -- Recurse (goes all the way back)
          restored = moveNewer modified        -- Step forward one (as stack unwinds)
      in restored

-- | Move forward until condition on element is met, apply operation, then restore position
-- Recursively moves forward, checking condition on current element, then restores on unwind
onForwardUntil :: (a -> Bool) -> (Zipper a -> Zipper a) -> Zipper a -> Zipper a
onForwardUntil condition f zipper =
  case zipperCurrent zipper of
    Nothing -> zipper  -- End of zipper, condition never met
    Just elem
      | condition elem -> f zipper  -- Condition met on current element
      | otherwise ->
          case zipperBack zipper of
            [] -> zipper  -- No newer items, can't move forward, give up
            _ ->
              let forward = moveNewer zipper
                  modified = onForwardUntil condition f forward
                  restored = moveOlder modified
              in restored

-- | Apply operation on the nth SectionItem's inner zipper (counting from oldest/front)
-- Must be called within onRewoundZipper to start from the beginning
onNthSubtree :: Int -> (Zipper (OutputItem msg) -> Zipper (OutputItem msg)) -> Zipper (OutputItem msg) -> Zipper (OutputItem msg)
onNthSubtree targetIdx f zipper
  | targetIdx < 0 = zipper  -- Invalid index, no-op
  | otherwise = case zipperCurrent zipper of
      Just (SectionItem subZipper)
        | targetIdx == 0 ->
            -- Found the target subsection, apply operation
            updateCurrent (SectionItem (f subZipper)) zipper
        | otherwise ->
            -- This is a subsection but not the target, keep searching
            onNthSubtree (targetIdx - 1) f (moveNewer zipper)
      Just _ ->
        -- Not a subsection, keep searching forward
        onNthSubtree targetIdx f (moveNewer zipper)
      Nothing ->
        -- End of zipper, subsection not found
        zipper

-- | Count SectionItems in entire zipper (all of back, current, and front)
countSubtrees :: Zipper (OutputItem msg) -> Int
countSubtrees (Zipper back current front) =
  let countInList = length . filter isSectionItem
      isSectionItem (SectionItem _) = True
      isSectionItem _ = False
      backCount = countInList back
      currentCount = case current of
        Just (SectionItem _) -> 1
        _ -> 0
      frontCount = countInList front
  in backCount + currentCount + frontCount

-- | Apply operation at a hierarchical address in the zipper tree (modify)
-- Preserves cursor position by rewinding, modifying, then restoring
atAddress :: SubsectionAddr -> (Zipper (OutputItem msg) -> Zipper (OutputItem msg)) -> Zipper (OutputItem msg) -> Zipper (OutputItem msg)
atAddress Root f zipper = f zipper
atAddress (Nested idx parent) f zipper =
  -- Recurse to parent address, then within parent navigate to idx'th subsection
  atAddress parent (\parentZipper ->
    onRewoundZipper (findAndModify idx) parentZipper
  ) zipper
  where
    -- Find the idx'th SectionItem (counting from oldest) and apply f to its inner zipper
    findAndModify n z = case zipperCurrent z of
      Just (SectionItem subZipper)
        | n == 0 ->
            -- Found the target! Apply f to its inner zipper
            updateCurrent (SectionItem (f subZipper)) z
        | otherwise ->
            -- This is a SectionItem but not the target, keep going
            case zipperBack z of
              [] -> z  -- Can't move forward, give up
              _ -> findAndModify (n - 1) (moveNewer z)
      Just _ ->
        -- Not a SectionItem, skip it
        case zipperBack z of
          [] -> z  -- Can't move forward, give up
          _ -> findAndModify n (moveNewer z)
      Nothing ->
        -- End of zipper, couldn't find it
        z

-- | Query at a hierarchical address in the zipper tree (read-only)
-- Returns a value extracted from the zipper at that address
queryAtAddress :: SubsectionAddr -> (Zipper (OutputItem msg) -> a) -> Zipper (OutputItem msg) -> a
queryAtAddress Root f zipper = f zipper
queryAtAddress (Nested idx parent) f zipper =
  -- Recurse to parent address, then within parent navigate to idx'th subsection
  queryAtAddress parent (\parentZipper ->
    skipAndQuery idx (focusOldest parentZipper)
  ) zipper
  where
    -- Skip to the idx'th SectionItem and query its inner zipper
    -- This doesn't modify the zipper, just extracts a value
    skipAndQuery n z = case zipperCurrent z of
      Just (SectionItem subZipper)
        | n == 0 -> f subZipper  -- Found it!
        | otherwise ->
            case zipperBack z of
              [] -> error "Invalid address: subsection not found (end of zipper)"
              _ -> skipAndQuery (n - 1) (moveNewer z)
      Just _ ->
        case zipperBack z of
          [] -> error "Invalid address: subsection not found (no more items)"
          _ -> skipAndQuery n (moveNewer z)
      Nothing -> error "Invalid address: subsection not found (empty)"

--------------------------------------------------------------------------------
-- Rendering Functions
--------------------------------------------------------------------------------

-- | Render an entire output history zipper with focus markers
-- This is the main rendering function that handles front/current/back and focus
renderZipper :: forall model n. RenderOptions -> OutputHistoryZipper (Message model) -> Widget n
renderZipper opts zipper =
  let -- Render front (older items, reversed for chronological order)
      frontWidgets = case reverse (zipperFront zipper) of
        [] -> emptyWidget
        items -> vBox $ map (renderItem opts False) items
      -- Render current (focused item with highlighted indicator)
      currentWidget = case zipperCurrent zipper of
        Nothing -> emptyWidget
        Just item -> renderItem opts True item
      -- Render back (newer items, reversed for chronological order)
      backWidgets = case reverse (zipperBack zipper) of
        [] -> emptyWidget
        items -> vBox $ map (renderItem opts False) items
  in vBox [frontWidgets, currentWidget, backWidgets]

-- | Render an OutputItem with optional markdown formatting
-- Pattern matches directly on Message constructors to determine rendering
-- Only UserText, AssistantText, AssistantReasoning, and streaming items use markdown when enabled
-- The isFocused parameter applies a dark gray background to the entire item area,
-- but the text content gets explicit transparent background
renderItem :: forall model n. RenderOptions -> Bool -> OutputItem (Message model) -> Widget n
renderItem opts isFocused item =
  let renderContent marker text = renderContentWithMarker isFocused marker text
  in vBox $ case item of
  -- User messages
  MessageItem (UserText text) ->
    [txt " ", renderContent (txt "<") text, txt " "]

  -- Assistant text
  MessageItem (AssistantText text) ->
    [txt " ", renderContent (txt ">") text, txt " "]

  -- Assistant reasoning
  MessageItem (AssistantReasoning text) ->
    [txt " ", renderContent (txt "?") text, txt " "]

  -- Tool calls: Don't render - they're shown via CompletedToolItem
  MessageItem (AssistantTool _toolCall) ->
    []

  -- Tool results: Don't render - they're shown via CompletedToolItem
  MessageItem (ToolResultMsg _toolResult) ->
    []

  -- User images: simple text representation
  MessageItem (UserImage desc _imageData) ->
    [txt " ", txt "<" <+> padLeft (Pad 1) (txt $ "[Image: " <> desc <> "]"), txt " "]

  -- JSON messages: show as formatted text
  MessageItem (UserRequestJSON query schema) ->
    let jsonText = "Query: " <> query <> "\nSchema: " <> T.pack (show schema)
    in [txt " ", txt "<" <+> padLeft (Pad 1) (txt jsonText), txt " "]

  MessageItem (AssistantJSON value) ->
    let jsonText = T.pack (show value)
    in [txt " ", txt ">" <+> padLeft (Pad 1) (txt jsonText), txt " "]

  -- System messages: plain text
  MessageItem (SystemText text) ->
    [txt " ", txt "S" <+> padLeft (Pad 1) (txt text), txt " "]

  -- Log items: always same rendering
  LogItem level msg ->
    let (marker, attr) = case level of
          Info -> ("I ", logInfoAttr)
          Warning -> ("W ", logWarningAttr)
          Error -> ("E ", logErrorAttr)
    in [withAttr attr (txt marker) <+> txt msg]

  -- Status items
  StatusItem status ->
    case status of
      Idle -> []
      Working desc -> [txt "⋯ " <+> txt desc]
      Streaming state ->
        let thinkingWidget = case streamingThinking state of
              Nothing -> []
              Just t -> [renderContent (txt "~") t]
            responseWidget = case streamingResponse state of
              Nothing -> []
              Just r -> [renderContent (txt "}") r]
        in thinkingWidget ++ responseWidget
      WaitingForInput -> [txt "⋯ Waiting for input"]
      WaitingForToolCall -> [txt "⋯ Waiting for tool call"]
      Done -> []
      Failed err -> [txt "✗ " <+> txt err]

  -- System events: always same rendering
  SystemEventItem msg ->
    [txt "S " <+> txt msg]

  -- Tool execution indicators: always same rendering
  ToolExecutionItem name ->
    [txt "T " <+> txt name]

  -- Completed tool: Merged display of tool call + result
  CompletedToolItem (AssistantTool toolCall) (ToolResultMsg toolResult) ->
    renderCompletedTool toolCall toolResult

  -- Fallback for other message types in CompletedToolItem (shouldn't happen)
  CompletedToolItem _ _ ->
    [txt "T [Invalid tool item]"]

  -- Section: render subzipper recursively (same as root zipper, but indented)
  SectionItem subZipper ->
    [padLeft (Pad 1) (renderZipper opts subZipper)]

  where
    useMd = useMarkdown opts

    -- Render content with a marker on the left edge
    -- When focused, wraps entire hBox in gray, then content gets transparent bg (creating left bar)
    renderContentWithMarker :: Bool -> Widget n -> Text -> Widget n
    renderContentWithMarker focused marker text =
      let content = if useMd
                    then vBox $ markdownToWidgetsWithIndent 0 text
                    else txtWrap text
          applyFocus w = if focused then withAttr focusedItemAttr w else w
      in applyFocus (marker <+> withAttr transparentBgAttr (padLeft (Pad 1) content))

    -- Render a completed tool call with nice formatting
    renderCompletedTool :: ToolCall -> ToolResult -> [Widget n]
    renderCompletedTool call result =
      let callLine = renderToolCallLine call
          resultLine = renderToolResultLine result
      in [txt " ", vBox [callLine, resultLine], txt " "]

    -- Render tool call as single line
    renderToolCallLine :: ToolCall -> Widget n
    renderToolCallLine (ToolCall _id name args) =
      txt "T " <+> txt name <+> renderArgs args
    renderToolCallLine (InvalidToolCall _id name _raw err) =
      txt "T " <+> txt name <+> txt " [invalid: " <+> txt err <+> txt "]"

    -- Render arguments: if single param, show inline like func(value)
    renderArgs :: Aeson.Value -> Widget n
    renderArgs (Aeson.Object obj) =
      case KM.toList obj of
        [(_key, val)] ->
          -- Single parameter: show as func("value") or func(123)
          txt "(" <+> renderSimpleValue val <+> txt ")"
        _ ->
          -- Multiple parameters: show truncated JSON
          txt " " <+> txt (truncateText 50 $ T.pack $ show $ Aeson.Object obj)
    renderArgs val = txt " " <+> txt (truncateText 50 $ T.pack $ show val)

    -- Render simple values without quotes where appropriate
    renderSimpleValue :: Aeson.Value -> Widget n
    renderSimpleValue (Aeson.String s) = txt "\"" <+> txt (truncateText 40 s) <+> txt "\""
    renderSimpleValue (Aeson.Number n) = txt (T.pack $ show n)
    renderSimpleValue (Aeson.Bool b) = txt (T.pack $ show b)
    renderSimpleValue Aeson.Null = txt "null"
    renderSimpleValue val = txt (truncateText 40 $ T.pack $ show val)

    -- Render tool result with formatting
    renderToolResultLine :: ToolResult -> Widget n
    renderToolResultLine (ToolResult _call (Left errMsg)) =
      txt "  → Error: " <+> txtWrap (truncateText 60 errMsg)
    renderToolResultLine (ToolResult _call (Right value)) =
      txt "  → " <+> renderResultValue value

    -- Render result value: try to show it nicely
    renderResultValue :: Aeson.Value -> Widget n
    renderResultValue (Aeson.String s) =
      -- For string results, show first few lines
      let lines' = T.lines s
          preview = T.unlines (take 3 lines')
          hasMore = length lines' > 3
      in vBox $ map txt (T.lines preview) ++ if hasMore then [txt "  ..."] else []
    renderResultValue val =
      txt (truncateText 60 $ T.pack $ show val)

    -- Truncate text to max length with ellipsis
    truncateText :: Int -> Text -> Text
    truncateText maxLen t =
      if T.length t > maxLen
      then T.take (maxLen - 3) t <> "..."
      else t

--------------------------------------------------------------------------------
-- Legacy Types (for backward compatibility during migration)
--------------------------------------------------------------------------------

-- | A single entry in the output timeline
data OutputMessage
  = ConversationMessage Int Text
  | LogEntry Level Text
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
  , showSystemEvents :: Bool  -- ^ Show system events
  , showToolCalls :: Bool     -- ^ Show tool executions
  }

-- | Default filter - show everything
defaultFilter :: DisplayFilter
defaultFilter = DisplayFilter
  { showMessages = True
  , showLogs = True
  , showSystemEvents = False  -- System events off by default
  , showToolCalls = True
  }

-- | Check if an output message should be displayed
shouldDisplay :: DisplayFilter -> OutputMessage -> Bool
shouldDisplay filt (ConversationMessage _ _) = showMessages filt
shouldDisplay filt (LogEntry _ _) = showLogs filt
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
renderOutputMessageRaw (SystemEvent msg) = [txt "S " <+> txt msg]
renderOutputMessageRaw (ToolExecution name) = [txt "T " <+> txt name]


-- | Check if an OutputItem is a message item
isMessageItem :: OutputItem msg -> Bool
isMessageItem (MessageItem _) = True
isMessageItem _ = False

-- | Extract message values from a list of OutputItems (for merge comparison)
extractMessageItems :: [OutputItem msg] -> [msg]
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

-- | Add CompletedToolItem entries for tool call/result pairs
-- Scans through OutputItems and inserts CompletedToolItem after each pair
-- Matches by tool call ID to handle multiple concurrent tool calls
-- Input/output are in newest-first order, so we see results before calls
addCompletedToolItems :: [OutputItem (Message m)] -> [OutputItem (Message m)]
addCompletedToolItems items = go [] items
  where
    -- processed holds items we've already seen (in reverse order)
    go :: [OutputItem (Message m)] -> [OutputItem (Message m)] -> [OutputItem (Message m)]
    go _ [] = []

    -- Found a tool result - just emit it and remember we saw it
    go processed (item@(MessageItem (ToolResultMsg _)) : rest) =
      item : go (item : processed) rest

    -- Found a tool call - look back in processed items for its result
    go processed (MessageItem callMsg@(AssistantTool toolCall) : rest) =
      let callId = case toolCall of
            ToolCall tid _ _ -> tid
            InvalidToolCall tid _ _ _ -> tid
          matchingResult = findMatchingResult callId processed
      in case matchingResult of
           Just resultMsg ->
             -- Found the result: emit call + CompletedToolItem
             MessageItem callMsg : CompletedToolItem callMsg resultMsg : go (MessageItem callMsg : processed) rest
           Nothing ->
             -- No result yet (call still executing), just emit call
             MessageItem callMsg : go (MessageItem callMsg : processed) rest

    -- Other items: pass through
    go processed (item : rest) = item : go (item : processed) rest

    -- Find matching result in already-processed items
    findMatchingResult :: Text -> [OutputItem (Message m)] -> Maybe (Message m)
    findMatchingResult _ [] = Nothing
    findMatchingResult targetId (MessageItem msg@(ToolResultMsg (ToolResult resultCall _)) : rest) =
      let resultCallId = case resultCall of
            ToolCall tid _ _ -> tid
            InvalidToolCall tid _ _ _ -> tid
      in if resultCallId == targetId
         then Just msg
         else findMatchingResult targetId rest
    findMatchingResult targetId (_ : rest) = findMatchingResult targetId rest

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

    (MessageItem _newMsg, _) ->
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
                  -- This shouldn't happen: nextMsg should be a MessageItem after span
                  -- But if it's not, treat it as a non-message item and continue
                  newItem : mergeOutputMessages restNew (nonMsgItems ++ [nextMsg] ++ restAfter)

    _ ->
      -- New item is not a message (shouldn't happen)
      newItem : mergeOutputMessages restNew (oldItem:restOld)

-- | Merge edited messages back into full history
-- Unlike mergeOutputMessages, this preserves non-editable messages from oldItems
-- that don't appear in editedMessages
--
-- Contract:
--   editedMessages: List of edited editable messages (UserText, AssistantText, AssistantReasoning)
--   oldItems: Full history including all message types and non-message items
--   Returns: Updated history with edited messages replaced, non-editable messages preserved
mergeEditedMessages :: Eq msg
                    => (msg -> Bool)  -- ^ Predicate: is this message editable?
                    -> [OutputItem msg]  -- ^ Edited messages (editable subset)
                    -> [OutputItem msg]  -- ^ Old full history
                    -> [OutputItem msg]  -- ^ Merged result
mergeEditedMessages isEditable editedItems oldItems =
  -- Strategy: Replace editable MessageItems in oldItems with editedItems,
  -- keeping non-editable MessageItems and all non-message items unchanged
  go editedItems oldItems
  where
    go [] remaining = filter (not . isEditableMessageItem) remaining
    go edited [] = edited  -- New edited messages at the end
    go edited@(e:restEdited) (o:restOld) =
      case (e, o) of
        (MessageItem editedMsg, MessageItem oldMsg)
          | isEditable oldMsg && isEditable editedMsg ->
              if editedMsg == oldMsg
              then -- Messages match: keep it and collect any logs after
                   let (logsAfter, restAfterLogs) = span (not . isMessageItem) restOld
                   in e : logsAfter ++ go restEdited restAfterLogs
              else if editedMsg `elem` extractEditableItems restOld
              then -- Edited message exists later: old message was deleted
                   go edited restOld
              else if oldMsg `elem` extractEditableItems restEdited
              then -- Old message exists later: edited message is insertion
                   e : go restEdited (o:restOld)
              else -- Neither exists later: replacement
                   e : go restEdited restOld

          | isEditable oldMsg ->
              -- Old is editable but new isn't (shouldn't happen): skip old
              go edited restOld

          | otherwise ->
              -- Old is non-editable: keep it, continue with same edited list
              o : go edited restOld

        (MessageItem _, _) ->
          -- Old item is not a message: keep it
          o : go edited restOld

        _ ->
          -- Edited item is not a message (shouldn't happen): pass through
          e : go restEdited (o:restOld)

    isEditableMessageItem (MessageItem msg) = isEditable msg
    isEditableMessageItem _ = False

    extractEditableItems = foldr extractEditable []
      where
        extractEditable (MessageItem m) acc
          | isEditable m = m : acc
        extractEditable _ acc = acc

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
