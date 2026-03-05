{-# LANGUAGE RankNTypes #-}

-- | Generic TUI interface for runix-code
--
-- This module provides a brick-based chat interface that works with any
-- message type and agent function. It handles:
-- - Input with multiple modes (Enter sends vs Enter newline)
-- - Backslash-enter for literal newlines
-- - Bracketed paste support
-- - Scrollable history
-- - Dynamic input sizing
-- - STM-based state for concurrent updates from effect interpreters
module UI.UI
  ( -- * UI Entry Point
    runUI
    -- * Types (re-export from UI.State)
  , Name(..)
  , InputMode(..)
  , AppState(..)
  ) where

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Main (invalidateCacheEntry)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Zipper (TextZipper, cursorPosition, breakLine, deletePrevChar, moveLeft, moveRight, currentLine, moveCursor)
import qualified Data.Text.Zipper.Generic as TZ
import Lens.Micro
import Lens.Micro.Mtl
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)
import Control.Concurrent.STM
import Data.List (dropWhileEnd)
import qualified Brick.BChan
import Brick.BChan (newBChan, writeBChan)

import UI.State (UIVars(..), Name(..), provideUserInput, requestCancelFromUI, SomeInputWidget(..), AgentEvent(..), LLMSettings(..), UserRequest(..))
import Runner (ModelEntry(..))
import UI.OutputHistory (Zipper(..), OutputHistoryZipper, OutputItem(..), emptyZipper, appendItem, renderItem, RenderOptions(..), defaultRenderOptions, zipperFront, zipperCurrent, zipperBack, extractMessages)
import UniversalLLM (Message(..))
import UI.UserInput.InputWidget (isWidgetComplete)
import qualified UI.Attributes as Attrs
import qualified UI.Widgets.MessageHistory as MH
import qualified UI.InputPanel as IP
import qualified Runix.LLM.Context as Context
import UI.RichInput (InputSegment(..), RefState(..), SegmentLine, segmentsToText, segmentsTake, segmentsDrop, segmentsLength, rotateMatches)
import UI.State (UIVars(..), Name(..), provideUserInput, requestCancelFromUI, SomeInputWidget(..), AgentEvent(..), LLMSettings(..), UserRequest(..), requestCompletion, CompletionRequest(..), sendAgentEvent)
import Runix.Logging (Level(..))

-- | Custom events for the TUI
data CustomEvent msg
  = AgentEvent (AgentEvent msg)  -- ^ Event from agent thread
  | UpdateViewport               -- ^ Update viewport state after render
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Input mode: what Enter key does
data InputMode = EnterSends | EnterNewline
  deriving stock (Eq, Show)

-- | Markdown rendering mode
data MarkdownMode = RenderMarkdown | ShowRaw
  deriving stock (Eq, Show)

-- | TUI application state
--
-- Parametrized over message type to store typed messages in zipper
-- The zipper lives here - UI owns the state, interpreters send events to update it
-- Uses parallel zippers: one for data, one for pre-rendered widgets
-- | Information about an active LLM stream
data StreamInfo = StreamInfo
  { streamId :: Int       -- ^ Stream identifier
  , chunkCount :: Int     -- ^ Number of chunks received
  } deriving stock (Eq, Show)

data AppState msg = AppState
  { _uiVars :: UIVars msg                  -- STM queues for communication
  , _outputZipper :: OutputHistoryZipper msg  -- History zipper with typed messages (data)
  , _widgetZipper :: Zipper (T.Widget Name)  -- Pre-rendered widgets (parallel structure)
  , _status :: Text                         -- Current status message
  , _pendingInput :: Maybe SomeInputWidget  -- Active input widget (cached from TVar)
  , _inputEditor :: Editor SegmentLine Name -- Input field with rich content
  , _inputMode :: InputMode                -- Current input mode
  , _markdownMode :: MarkdownMode          -- Whether to render markdown or show raw
  , _lastViewport :: Maybe T.Viewport      -- Last viewport state for scroll indicators
  , _eventChan :: Brick.BChan.BChan (CustomEvent msg)  -- Event channel for sending custom events
  , _llmSettings :: LLMSettings            -- Current LLM settings for display
  , _activeStreams :: [StreamInfo]         -- Active LLM streams (in order of creation)
  , _modelEntry :: ModelEntry              -- Active model entry (for name, config, etc.)
  }

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

uiVarsL :: Lens' (AppState msg) (UIVars msg)
uiVarsL = lens _uiVars (\st v -> st { _uiVars = v })

outputZipperL :: Lens' (AppState msg) (OutputHistoryZipper msg)
outputZipperL = lens _outputZipper (\st z -> st { _outputZipper = z })

widgetZipperL :: Lens' (AppState msg) (Zipper (T.Widget Name))
widgetZipperL = lens _widgetZipper (\st z -> st { _widgetZipper = z })

statusL :: Lens' (AppState msg) Text
statusL = lens _status (\st s -> st { _status = s })

pendingInputL :: Lens' (AppState msg) (Maybe SomeInputWidget)
pendingInputL = lens _pendingInput (\st p -> st { _pendingInput = p })

inputEditorL :: Lens' (AppState msg) (Editor SegmentLine Name)
inputEditorL = lens _inputEditor (\st e -> st { _inputEditor = e })

inputModeL :: Lens' (AppState msg) InputMode
inputModeL = lens _inputMode (\st m -> st { _inputMode = m })

markdownModeL :: Lens' (AppState msg) MarkdownMode
markdownModeL = lens _markdownMode (\st m -> st { _markdownMode = m })

lastViewportL :: Lens' (AppState msg) (Maybe T.Viewport)
lastViewportL = lens _lastViewport (\st v -> st { _lastViewport = v })

eventChanL :: Lens' (AppState msg) (Brick.BChan.BChan (CustomEvent msg))
eventChanL = lens _eventChan (\st c -> st { _eventChan = c })

llmSettingsL :: Lens' (AppState msg) LLMSettings
llmSettingsL = lens _llmSettings (\st s -> st { _llmSettings = s })

activeStreamsL :: Lens' (AppState msg) [StreamInfo]
activeStreamsL = lens _activeStreams (\st s -> st { _activeStreams = s })

modelEntryL :: Lens' (AppState msg) ModelEntry
modelEntryL = lens _modelEntry (\st m -> st { _modelEntry = m })

--------------------------------------------------------------------------------
-- Display Rendering
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- UI Entry Point
--------------------------------------------------------------------------------

-- | Run the TUI with STM-based state
--
-- The UI is parametrized over the Message type (specific model/provider instances)
-- Messages are rendered directly by pattern matching on Message constructors
--
-- Refreshes are triggered by the effect interpreters, not by polling.
runUI :: forall model. Eq (Message model) =>
         ModelEntry  -- ^ Model entry for display (name, config, etc.)
      -> ((AgentEvent (Message model) -> IO ()) -> IO (UIVars (Message model)))  -- ^ Function to create UIVars with send callback
      -> IO ()
runUI modelEntry mkUIVars = do
  -- Create event channel - agent events and UI events use same channel
  -- Small buffer since batching happens in interpretStreamChunkToUI
  eventChan <- newBChan 8

  -- Create UI vars with callback that writes AgentEvents wrapped in CustomEvent
  -- Brick will automatically batch renders when events come in fast
  let sendAgentEventCallback agentEvent = Brick.BChan.writeBChan eventChan (AgentEvent agentEvent)
  uiVars <- mkUIVars sendAgentEventCallback

  let initialState = AppState
        { _uiVars = uiVars
        , _outputZipper = emptyZipper
        , _widgetZipper = emptyZipper
        , _status = Text.pack "Ready"
        , _pendingInput = Nothing
        , _inputEditor = editor InputEditor Nothing []
        , _inputMode = EnterSends
        , _markdownMode = RenderMarkdown
        , _lastViewport = Nothing
        , _eventChan = eventChan
        , _llmSettings = LLMSettings  -- Default settings
        , _activeStreams = []
        , _modelEntry = modelEntry
        }

  -- Create vty with bracketed paste enabled
  let buildVty = do
        v <- mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.BracketedPaste True
        return v
  initialVty <- buildVty
  _finalState <- M.customMain initialVty buildVty (Just eventChan) app initialState
  return ()

--------------------------------------------------------------------------------
-- Brick App Definition
--------------------------------------------------------------------------------

app :: forall model. Eq (Message model) => M.App (AppState (Message model)) (CustomEvent (Message model)) Name
app = M.App
  { M.appDraw = drawUI
  , M.appHandleEvent = handleEvent
  , M.appStartEvent = return ()
  , M.appAttrMap = const Attrs.theMap
  , M.appChooseCursor = M.showFirstCursor
  }

--------------------------------------------------------------------------------
-- Editor Helper Functions
--------------------------------------------------------------------------------

-- | Delete the word before the cursor (Ctrl-W behavior)
deleteWordBackwards :: Editor SegmentLine Name -> Editor SegmentLine Name
deleteWordBackwards ed =
  let z = ed ^. editContentsL
      (row, col) = cursorPosition z
      currentLine = case drop row (getEditContents ed) of
                      (line:_) -> segmentsToText line
                      [] -> ""
      -- Find the start of the word (skip back over non-whitespace, then over whitespace)
      beforeCursor = Text.unpack $ Text.take col currentLine
      -- Skip trailing whitespace
      afterWhitespace = dropWhileEnd (== ' ') beforeCursor
      -- Skip the word
      afterWord = dropWhileEnd (/= ' ') afterWhitespace
      newCol = length afterWord
      charsToDelete = col - newCol
      -- Delete that many characters
      z' = iterate deletePrevChar z !! charsToDelete
  in ed & editContentsL .~ z'

-- | Move cursor backwards by one word (Ctrl-Left behavior)
-- Works on TextZipper, can be used with applyEdit
moveWordBackwards :: TextZipper SegmentLine -> TextZipper SegmentLine
moveWordBackwards z =
  let (_row, col) = cursorPosition z
      line = segmentsToText (currentLine z)
      beforeCursor = Text.unpack $ Text.take col line
      -- Skip trailing whitespace
      afterWhitespace = dropWhileEnd (== ' ') beforeCursor
      -- Skip the word
      afterWord = dropWhileEnd (/= ' ') afterWhitespace
      newCol = length afterWord
      charsToMove = col - newCol
  in applyN charsToMove moveLeft z
  where
    applyN n f x = iterate f x !! max 0 n

-- | Move cursor forwards by one word (Ctrl-Right behavior)
-- Works on TextZipper, can be used with applyEdit
moveWordForwards :: TextZipper SegmentLine -> TextZipper SegmentLine
moveWordForwards z =
  let (_row, col) = cursorPosition z
      line = segmentsToText (currentLine z)
      afterCursor = Text.unpack $ Text.drop col line
      -- Skip whitespace
      afterWhitespace = dropWhile (== ' ') afterCursor
      -- Skip the word
      afterWord = dropWhile (/= ' ') afterWhitespace
      charsToMove = length afterCursor - length afterWord
  in applyN charsToMove moveRight z
  where
    applyN n f x = iterate f x !! max 0 n

--------------------------------------------------------------------------------
-- Tab Completion
--------------------------------------------------------------------------------

-- | Get the current word at cursor position (returns word and its start/end positions)
getCurrentWord :: Editor SegmentLine Name -> Maybe (Text, Int, Int)
getCurrentWord ed =
  let z = ed ^. editContentsL
      (row, col) = cursorPosition z
      contentLines = getEditContents ed
  in if row < length contentLines
     then
       let line = segmentsToText (contentLines !! row)
           lineStr = Text.unpack line
           -- Find word boundaries
           beforeCursor = take col lineStr
           afterCursor = drop col lineStr
           -- Expand left to word start
           wordStart = length $ takeWhile (\c -> c /= ' ' && c /= '\n') $ reverse beforeCursor
           -- Expand right to word end
           wordEnd = length $ takeWhile (\c -> c /= ' ' && c /= '\n') afterCursor
           startPos = col - wordStart
           endPos = col + wordEnd
           word = take (wordStart + wordEnd) $ drop startPos lineStr
       in if null word
          then Nothing
          else Just (Text.pack word, startPos, endPos)
     else Nothing

-- | Replace the current word with a FileRefSegment containing all matches
replaceCurrentWord :: Int -> Int -> Text -> [FilePath] -> Editor SegmentLine Name -> Editor SegmentLine Name
replaceCurrentWord startPos endPos _typedText filePaths ed =
  let z = ed ^. editContentsL
      (row, col) = cursorPosition z
      contentLines = getEditContents ed
  in if row < length contentLines && not (null filePaths)
     then
       let line = contentLines !! row
           -- Strip "./" prefix from all file paths
           cleanPaths = map (\case '.':'/':rest -> rest; p -> p) filePaths
           -- Split line into before/replacement/after segments
           beforeSegs = segmentsTake startPos line
           afterSegs = segmentsDrop endPos line
           -- Create file reference segment with all matches (head is current)
           fileSegment = FileRefSegment _typedText cleanPaths RefAccepted
           newLine = beforeSegs ++ [fileSegment] ++ afterSegs
           -- Replace line in editor
           newLines = take row contentLines ++ [newLine] ++ drop (row + 1) contentLines
           -- Calculate new cursor position (after the file reference)
           firstPath = head cleanPaths
           newCol = Text.length (segmentsToText beforeSegs) + length ("@" ++ firstPath)
           -- Create new zipper with updated content and cursor position
           newZipper = moveCursor (row, newCol) $ TZ.textZipper newLines Nothing
       in ed & editContentsL .~ newZipper
     else ed

--------------------------------------------------------------------------------
-- UI Rendering
--------------------------------------------------------------------------------

-- Draw UI is specific to Message types since renderItem requires it
drawUI :: forall model. AppState (Message model) -> [T.Widget Name]
drawUI st = [indicatorLayer, baseLayer]
  where
    -- Read state directly from AppState
    status = _status st
    mPendingInput = _pendingInput st
    wzipper = _widgetZipper st
    _vars = _uiVars st
    ozipper = _outputZipper st

    -- Calculate token count from history
    historyMessages = extractMessages ozipper
    Context.TokenCount tokens = Context.tokenCount historyMessages

    -- Extract pre-rendered widgets from the widget zipper
    -- Zipper structure: front is reversed (newest at head), back is natural order (oldest at head)
    -- To display oldest→newest: reverse front, then current, then back
    frontWidgets = if null (zipperFront wzipper)
                   then []
                   else [cached CachedFront $ vBox $ reverse $ zipperFront wzipper]

    currentWidgets = case zipperCurrent wzipper of
                       Nothing -> []
                       Just widget -> [cached CachedCurrent widget]

    backWidgets = if null (zipperBack wzipper)
                  then []
                  else [cached CachedBack $ vBox $ zipperBack wzipper]

    -- Calculate dimensions
    availHeight = 100  -- This will be determined by context, placeholder for now
    maxInputHeight = max 1 (availHeight `div` 2)
    editorLines = getEditContents (_inputEditor st)
    contentHeight = max 1 (length editorLines)
    inputHeight = min contentHeight maxInputHeight

    modeStr = case _inputMode st of
              EnterSends -> "Enter: send"
              EnterNewline -> "Enter: newline (Ctrl-D: send)"

    markdownStr = case _markdownMode st of
                    RenderMarkdown -> "Markdown: rendered"
                    ShowRaw -> "Markdown: raw"


    -- Combine widgets: front (oldest) ++ current ++ back (newest at bottom)
    historyWidgets = frontWidgets ++ currentWidgets ++ backWidgets

    -- Active streams widget
    activeStreamsWidget = if null (_activeStreams st)
                          then emptyWidget
                          else vBox $ map renderStream (_activeStreams st)
      where
        renderStream si = txt $ "⋯ Stream #" <> Text.pack (show (streamId si))
                                <> ": " <> Text.pack (show (chunkCount si)) <> " chunks"

    statusText = Text.unpack status

    -- MessageHistory returns (base, indicators) for Brick's layer system
    (historyBase, historyIndicators) = MH.messageHistory HistoryViewport (_lastViewport st) historyWidgets

    -- Base layer: main UI
    baseLayer = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let availH = ctx ^. T.availHeightL

          -- Check if we have a pending input widget
          ui = case mPendingInput of
            -- Input widget is active - show it instead of normal input
            Just widget ->
              let (inputPanel, panelHeight) = IP.drawInputPanel widget
                  historyH = availH - panelHeight - 2  -- -2 for border and status
               in vBox
                    [ vLimit historyH historyBase
                    , inputPanel
                    , hBorder
                    , renderInputWidgetStatusBar
                    ]

            -- Normal input mode
            Nothing ->
              let streamHeight = length (_activeStreams st)
                  historyH = availH - inputHeight - streamHeight - 4  -- -4 for two borders, status
               in vBox
                    [ vLimit historyH historyBase
                    , activeStreamsWidget
                    , hBorder
                    , vLimit inputHeight $
                        renderPrompt status <+>
                          padLeft (Pad 1) (padRight (Pad 1) $
                            renderEditor renderSegmentLines True (_inputEditor st))
                    , hBorder
                    , renderStatusBar (_inputMode st) (_markdownMode st) tokens
                    ]
      T.render ui

    -- Indicator layer: rendered on top by Brick's renderFinal
    indicatorLayer = historyIndicators

    -- Render segment lines with highlighting
    renderSegmentLines :: [SegmentLine] -> T.Widget Name
    renderSegmentLines segLines =
      vBox $ map renderSegmentLine segLines

    renderSegmentLine :: SegmentLine -> T.Widget Name
    renderSegmentLine [] = str " "  -- Empty lines shown as space to preserve them
    renderSegmentLine segs = hBox $ map renderSegment segs

    renderSegment :: InputSegment -> T.Widget Name
    renderSegment (TextSegment t) = txt t
    renderSegment (FileRefSegment _ matches RefPending) =
      let path = case matches of (p:_) -> p; [] -> ""
      in withAttr Attrs.fileRefPendingAttr $ txt ("@" <> Text.pack path)
    renderSegment (FileRefSegment _ matches RefAccepted) =
      let path = case matches of (p:_) -> p; [] -> ""
      in withAttr Attrs.fileRefAcceptedAttr $ txt ("@" <> Text.pack path)
    renderSegment (FileRefSegment _ matches RefRejected) =
      let path = case matches of (p:_) -> p; [] -> ""
      in withAttr Attrs.fileRefRejectedAttr $ txt ("@" <> Text.pack path)

    -- Render input prompt with color based on status
    renderPrompt :: Text -> T.Widget Name
    renderPrompt status =
      let promptAttr = if status == "Ready" || status == ""
                       then Attrs.promptReadyAttr  -- green
                       else Attrs.promptBusyAttr   -- yellow for pending/working
      in withAttr promptAttr (txt ">")

    -- Render a structured status bar with colors
    renderStatusBar :: InputMode -> MarkdownMode -> Int -> T.Widget Name
    renderStatusBar inputMode mdMode tokenCount =
      let -- Mode indicators
          inputModeWidget = case inputMode of
            EnterSends -> txt "Enter: send"
            EnterNewline -> txt "Enter: newline"

          mdModeWidget = case mdMode of
            RenderMarkdown -> txt "markdown"
            ShowRaw -> txt "raw"

          -- Model name widget
          modelNameWidget = withAttr Attrs.boldAttr (txt $ meName (_modelEntry st))

          -- Token count widget
          tokenWidget = withAttr Attrs.codeAttr (txt $ Text.pack (show tokenCount) <> " tokens")

          modesWidget = modelNameWidget
                    <+> txt "  "
                    <+> withAttr Attrs.boldAttr inputModeWidget
                    <+> txt "  "
                    <+> withAttr Attrs.boldAttr mdModeWidget
                    <+> txt "  "
                    <+> tokenWidget

          -- Keybindings section (right side)
          keybinding key desc = withAttr Attrs.codeAttr (txt key) <+> txt (" " <> desc)
          keybindings = keybinding "\\Enter" "newline" <+> txt "  "
                    <+> keybinding "Ctrl-T" "input" <+> txt "  "
                    <+> keybinding "Ctrl-R" "toggle md" <+> txt "  "
                    <+> keybinding "Ctrl-C" "quit"

      in modesWidget <+> fill ' ' <+> keybindings

    renderInputWidgetStatusBar :: T.Widget Name
    renderInputWidgetStatusBar =
      let inputWidgetLabel = withAttr Attrs.inputPanelLabelAttr (txt "Input Widget Active")
          keybinding = withAttr Attrs.codeAttr (txt "Esc") <+> txt " cancel"
      in inputWidgetLabel <+> fill ' ' <+> keybinding

--------------------------------------------------------------------------------
-- Event Handling
--------------------------------------------------------------------------------

-- | Re-render the widget zipper from the output zipper
-- Call this after: (1) output zipper changes, (2) markdown mode changes
reRenderWidgetZipper :: T.EventM Name (AppState (Message model)) ()
reRenderWidgetZipper = do
  mode <- use markdownModeL
  ozipper <- use outputZipperL
  let opts = defaultRenderOptions { useMarkdown = case mode of
                                      RenderMarkdown -> True
                                      ShowRaw -> False }
      -- Render each OutputItem to Widget Name
      -- Current item gets isFocused=True for highlight, others get False
      renderZipper (Zipper back current front) =
        Zipper
          { zipperBack = map (renderItem opts False) back
          , zipperCurrent = fmap (renderItem opts True) current
          , zipperFront = map (renderItem opts False) front
          }
  widgetZipperL .= renderZipper ozipper

-- | Replace output zipper, re-render widgets, and invalidate caches
replaceOutputZipper :: Zipper (OutputItem (Message model)) -> T.EventM Name (AppState (Message model)) ()
replaceOutputZipper zipper = do
  outputZipperL .= zipper
  reRenderWidgetZipper
  invalidateCacheEntry CachedFront
  invalidateCacheEntry CachedCurrent
  invalidateCacheEntry CachedBack

handleEvent :: forall model. Eq (Message model) => T.BrickEvent Name (CustomEvent (Message model)) -> T.EventM Name (AppState (Message model)) ()
-- Check if input widget is active first
handleEvent ev = do
  -- Read pending input widget from AppState
  mWidget <- use pendingInputL

  case mWidget of
    Just widget -> handleInputWidgetEvent widget ev
    Nothing -> handleNormalEvent ev

-- Handle events when input widget is active
handleInputWidgetEvent :: SomeInputWidget -> T.BrickEvent Name (CustomEvent msg) -> T.EventM Name (AppState msg) ()
-- Esc cancels the input widget - signal cancellation
handleInputWidgetEvent (SomeInputWidget _ _currentValue submitCallback) (T.VtyEvent (V.EvKey V.KEsc [])) = do
  -- Submit Nothing to signal cancellation and fail the tool call
  liftIO $ submitCallback Nothing
  -- Clear the widget immediately so future events aren't intercepted
  pendingInputL .= Nothing

-- Enter confirms and submits the current value
handleInputWidgetEvent (SomeInputWidget _ currentValue submitCallback) (T.VtyEvent (V.EvKey V.KEnter [])) = do
  -- Check if value is complete
  if isWidgetComplete currentValue
    then do
      -- Submit Just value to signal successful confirmation
      liftIO $ submitCallback (Just currentValue)
      -- Clear the widget immediately so future events aren't intercepted
      pendingInputL .= Nothing
    else
      return ()  -- Don't submit if incomplete

-- All other events go to the widget
handleInputWidgetEvent widget ev = do
  -- Run the widget event handler which uses EventM Name () instead of AppState
  ((), mNewWidget) <- T.nestEventM () $ IP.handleInputPanelEvent widget ev
  -- Update the widget in AppState if it changed
  case mNewWidget of
    Just newWidget -> pendingInputL .= Just newWidget
    Nothing -> return ()

--------------------------------------------------------------------------------
-- Event Handling
--------------------------------------------------------------------------------

-- Normal event handling (when no input widget active)
handleNormalEvent :: forall model. Eq (Message model) => T.BrickEvent Name (CustomEvent (Message model)) -> T.EventM Name (AppState (Message model)) ()
-- ESC: Request cancellation of current operation
handleNormalEvent (T.VtyEvent (V.EvKey V.KEsc [])) = do
  vars <- use uiVarsL
  -- Set the cancellation flag to stop the agent
  liftIO $ atomically $ requestCancelFromUI vars
  -- Clear the flag again for the next request after agent stops
  -- (This happens when the agent main loop gets control back)
  return ()

-- Ctrl+C: Actually exit the application
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt

-- Handle agent events - process state updates, render is triggered by separate channel
handleNormalEvent (T.AppEvent (AgentEvent event)) = do
  case event of
      ZipperUpdateEvent newZipper -> do
        -- Replace zipper with agent's updated version
        replaceOutputZipper newZipper

      UserMessageEvent msg -> do
        -- Add user message as new current
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
            newItem = MessageItem msg
        outputZipperL %= appendItem newItem
        widgetZipperL %= appendItem (renderItem opts True newItem)
        invalidateCacheEntry CachedFront
        invalidateCacheEntry CachedCurrent

      AgentCompleteEvent _msgs -> do
        -- Agent completed, update status
        statusL .= Text.pack "Ready"

      RestoreSessionEvent zipper -> do
        -- Restore session: update UI zipper and re-render
        replaceOutputZipper zipper

      LogEvent level text -> do
        -- UI infrastructure logs (not agent output)
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
            newItem = LogItem level text
        outputZipperL %= appendItem newItem
        widgetZipperL %= appendItem (renderItem opts True newItem)
        invalidateCacheEntry CachedFront
        invalidateCacheEntry CachedCurrent

      ShowInputWidgetEvent widget ->
        pendingInputL .= Just widget

      ClearInputWidgetEvent ->
        pendingInputL .= Nothing

      RunExternalCommandEvent action -> do
        -- Suspend Vty, run the external command, then resume
        currentState <- T.get
        M.suspendAndResume $ do
          action
          return currentState  -- Return current state unchanged

      StreamStartEvent sid -> do
        -- Add new stream to active streams
        activeStreamsL %= (++ [StreamInfo sid 0])

      StreamChunkEvent sid _ -> do
        -- Increment chunk count for this stream
        activeStreamsL %= map (\si -> if streamId si == sid then si { chunkCount = chunkCount si + 1 } else si)

      StreamEndEvent sid -> do
        -- Remove stream from active streams
        activeStreamsL %= filter (\si -> streamId si /= sid)

  -- After processing the event, scroll viewport and request viewport update
  M.vScrollToEnd (M.viewportScroll HistoryViewport)
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

-- Update cached viewport state (called after scrolling/rendering)
handleNormalEvent (T.AppEvent UpdateViewport) = do
  mVp <- M.lookupViewport HistoryViewport
  lastViewportL .= mVp

-- Handle window resize - stick to bottom if we were already there
handleNormalEvent (T.VtyEvent (V.EvResize _ _)) = do
  -- Check if we're at the bottom before resize
  wasAtBottom <- use lastViewportL >>= \case
    Nothing -> return True  -- Default to bottom if no viewport yet
    Just vp -> return $ MH.isAtBottom vp

  -- Invalidate all caches since word wrapping changes on resize
  invalidateCacheEntry CachedFront
  invalidateCacheEntry CachedCurrent
  invalidateCacheEntry CachedBack

  -- If we were at bottom, scroll to bottom after resize
  when wasAtBottom $ do
    M.vScrollToEnd (M.viewportScroll HistoryViewport)

  -- Update viewport state after resize (non-blocking)
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

-- Page Up/Down for scrolling history
handleNormalEvent (T.VtyEvent (V.EvKey V.KPageUp [])) = do
  M.vScrollPage (M.viewportScroll HistoryViewport) T.Up
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

handleNormalEvent (T.VtyEvent (V.EvKey V.KPageDown [])) = do
  M.vScrollPage (M.viewportScroll HistoryViewport) T.Down
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

-- Ctrl-T toggles input mode
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl])) = do
  mode <- use inputModeL
  let newMode = case mode of
        EnterSends -> EnterNewline
        EnterNewline -> EnterSends
  inputModeL .= newMode

-- Ctrl-R toggles markdown rendering mode (R for "raw")
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = do
  mode <- use markdownModeL
  let newMode = case mode of
        RenderMarkdown -> ShowRaw
        ShowRaw -> RenderMarkdown
  markdownModeL .= newMode
  -- Re-render widget zipper with new markdown mode
  reRenderWidgetZipper
  -- Invalidate all caches since widgets changed
  invalidateCacheEntry CachedFront
  invalidateCacheEntry CachedCurrent
  invalidateCacheEntry CachedBack

  -- Check if we're at the bottom before switching rendering mode
  wasAtBottom <- use lastViewportL >>= \case
    Nothing -> return True  -- Default to bottom if no viewport yet
    Just vp -> return $ MH.isAtBottom vp

  -- If we were at bottom, scroll to bottom after switching (since rendered/raw have different heights)
  when wasAtBottom $ do
    M.vScrollToEnd (M.viewportScroll HistoryViewport)

  -- Update viewport state after mode switch (non-blocking)
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

-- Ctrl-D sends message (useful in EnterNewline mode)
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = sendMessage

-- Handle paste events - insert content without triggering send
-- Just pass the paste event to the editor's normal handler
handleNormalEvent ev@(T.VtyEvent (V.EvPaste _)) = do
  zoom inputEditorL $ handleEditorEvent ev

-- Enter key behavior depends on mode and backslash handling
handleNormalEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
  -- First check if cursor is right after a backslash
  ed <- use inputEditorL
  let contentLines = getEditContents ed
      zipper = ed ^. editContentsL
      (row, col) = cursorPosition zipper

  -- Check if we're after a backslash
  if row < length contentLines && col > 0
    then do
      let currentLineText = segmentsToText (contentLines !! row)
      if col <= Text.length currentLineText && col > 0 && Text.index currentLineText (col - 1) == '\\'
        then do
          -- Replace \<Enter> with actual newline
          let newZipper = breakLine $ deletePrevChar zipper
              newEditor = ed & editContentsL .~ newZipper
          inputEditorL .= newEditor
        else handleEnterByMode
    else handleEnterByMode
  where
    handleEnterByMode = do
      mode <- use inputModeL
      case mode of
        EnterSends -> sendMessage
        EnterNewline -> zoom inputEditorL $ handleEditorEvent (T.VtyEvent (V.EvKey V.KEnter []))

-- Shift-Enter also inserts newline for terminals that support it
handleNormalEvent (T.VtyEvent (V.EvKey V.KEnter [V.MShift])) = do
  zoom inputEditorL $ handleEditorEvent (T.VtyEvent (V.EvKey V.KEnter []))

-- Ctrl-W: Delete word backwards
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl])) = do
  inputEditorL %= deleteWordBackwards

-- Ctrl-Left: Jump word backwards (may not work in all terminals)
handleNormalEvent (T.VtyEvent (V.EvKey V.KLeft [V.MCtrl])) = do
  inputEditorL %= applyEdit moveWordBackwards

-- Ctrl-Right: Jump word forwards (may not work in all terminals)
handleNormalEvent (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = do
  inputEditorL %= applyEdit moveWordForwards

-- Alt-B: Jump word backwards (Emacs-style, fallback for Ctrl-Left)
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'b') [V.MMeta])) = do
  inputEditorL %= applyEdit moveWordBackwards

-- Alt-F: Jump word forwards (Emacs-style, fallback for Ctrl-Right)
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'f') [V.MMeta])) = do
  inputEditorL %= applyEdit moveWordForwards

-- Tab: File completion for @file references (or cycle through existing matches)
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  vars <- use uiVarsL
  ed <- use inputEditorL
  let z = ed ^. editContentsL
      (row, col) = cursorPosition z
      contentLines = getEditContents ed

  -- Check if cursor is right after a FileRefSegment - if so, rotate it
  if row < length contentLines && col > 0
    then do
      let line = contentLines !! row
      -- Find segment at cursor
      case findSegmentAtCursor col 0 line of
        Just idx ->
          case line !! idx of
            FileRefSegment{} -> do
              -- Rotate this segment's matches
              let rotated = rotateMatches (line !! idx)
                  newLine = take idx line ++ [rotated] ++ drop (idx + 1) line
                  newLines = take row contentLines ++ [newLine] ++ drop (row + 1) contentLines
                  newZipper = moveCursor (row, col) $ TZ.textZipper newLines Nothing
              inputEditorL .= (ed & editContentsL .~ newZipper)
            _ -> performCompletion vars  -- Not a FileRef, try completion
        Nothing -> performCompletion vars  -- Not at segment boundary, try completion
    else performCompletion vars  -- Not in valid position, try completion
  where
    performCompletion vars = do
      ed <- use inputEditorL
      case getCurrentWord ed of
        Nothing -> return ()  -- No word at cursor
        Just (word, startPos, endPos) ->
          if Text.isPrefixOf "@" word
            then do
              let pattern = Text.drop 1 word
              -- Request completion (blocks until response)
              matches <- liftIO $ requestCompletion vars pattern
              case matches of
                [] -> return ()  -- No matches
                _ -> do
                  -- Replace word with file reference segment containing all matches
                  let matchPaths = map Text.unpack matches
                  inputEditorL %= replaceCurrentWord startPos endPos word matchPaths
            else return ()  -- Not a @ reference

    -- Reuse the same helper from backspace handler
    findSegmentAtCursor :: Int -> Int -> [InputSegment] -> Maybe Int
    findSegmentAtCursor cursorCol = go 0
      where
        go _ _ [] = Nothing
        go idx pos (seg:rest) =
          let segLen = case seg of
                         TextSegment t -> Text.length t
                         FileRefSegment _ (p:_) _ -> length ("@" ++ p)
                         FileRefSegment _ [] _ -> 1
              newPos = pos + segLen
          in if newPos == cursorCol
             then Just idx  -- Return index of any segment type
             else if newPos > cursorCol
                  then Nothing  -- Cursor is inside this segment
                  else go (idx + 1) newPos rest

-- Backspace: Delete whole FileRefSegment if cursor is right after one
handleNormalEvent (T.VtyEvent (V.EvKey V.KBS [])) = do
  ed <- use inputEditorL
  let z = ed ^. editContentsL
      (row, col) = cursorPosition z
      contentLines = getEditContents ed

  -- Check if we're right after a FileRefSegment
  if row < length contentLines && col > 0
    then do
      let line = contentLines !! row
      -- Walk through segments and check if cursor is exactly at segment boundary
      case findSegmentAtCursor col 0 line of
        Just idx ->
          -- Cursor is right after a FileRefSegment at index idx - delete it
          let beforeSegs = take idx line
              afterSegs = drop (idx + 1) line
              newLine = beforeSegs ++ afterSegs
              newLines = take row contentLines ++ [newLine] ++ drop (row + 1) contentLines
              newCol = segmentsLength beforeSegs
              newZipper = moveCursor (row, newCol) $ TZ.textZipper newLines Nothing
          in inputEditorL .= (ed & editContentsL .~ newZipper)
        Nothing ->
          -- Not after a file ref, do normal backspace
          zoom inputEditorL $ handleEditorEvent (T.VtyEvent (V.EvKey V.KBS []))
    else
      zoom inputEditorL $ handleEditorEvent (T.VtyEvent (V.EvKey V.KBS []))
  where
    -- Find if cursor is positioned exactly after a FileRefSegment
    -- Returns Just index if so, Nothing otherwise
    findSegmentAtCursor :: Int -> Int -> [InputSegment] -> Maybe Int
    findSegmentAtCursor cursorCol = go 0
      where
        go _ _ [] = Nothing
        go idx pos (seg:rest) =
          let segLen = case seg of
                         TextSegment t -> Text.length t
                         FileRefSegment _ (p:_) _ -> length ("@" ++ p)
                         FileRefSegment _ [] _ -> 1
              newPos = pos + segLen
          in if newPos == cursorCol
             then case seg of
                    FileRefSegment{} -> Just idx
                    _ -> go (idx + 1) newPos rest
             else if newPos > cursorCol
                  then Nothing  -- Cursor is inside this segment
                  else go (idx + 1) newPos rest

handleNormalEvent ev = do
  -- Delegate other events to the editor
  zoom inputEditorL $ handleEditorEvent ev

--------------------------------------------------------------------------------
-- Message Sending
--------------------------------------------------------------------------------

sendMessage :: T.EventM Name (AppState msg) ()
sendMessage = do
  ed <- use inputEditorL
  let contentLines = getEditContents ed  -- :: [SegmentLine]
      content = Text.unpack $ Text.intercalate "\n" $ map segmentsToText contentLines
  if null (filter (/= ' ') content)
    then return ()  -- Don't send empty messages
    else do
      -- Get the UI vars, current settings, and history zipper
      vars <- use uiVarsL
      settings <- use llmSettingsL
      zipper <- use outputZipperL

      -- Set status to Processing immediately
      statusL .= Text.pack "Processing..."

      -- Send user request (text + settings + zipper) to the agent
      let request = UserRequest { userText = Text.pack content, currentHistory = zipper, requestSettings = settings }
      liftIO $ atomically $ provideUserInput (userInputQueue vars) request

      -- Clear input
      inputEditorL .= editor InputEditor Nothing []

      -- Scroll viewport to bottom
      M.vScrollToEnd (M.viewportScroll HistoryViewport)

      -- Send event to update viewport indicators after render (non-blocking)
      chan <- use eventChanL
      liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport
