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
import Brick.Main (invalidateCacheEntry)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Data.Text as Text
import Data.Text (Text)
import Lens.Micro
import Lens.Micro.Mtl
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void)
import Control.Concurrent.STM
import qualified Brick.BChan
import Brick.BChan (newBChan, writeBChan)

import UI.State (UIVars(..), Name(..), provideUserInput, requestCancelFromUI, SomeInputWidget(..), AgentEvent(..), LLMSettings(..), UserRequest(..), requestCompletion, CompletionRequest(..))
import Runner (ModelEntry(..))
import UI.OutputHistory (Zipper(..), OutputHistoryZipper, OutputItem(..), emptyZipper, appendItem, renderItem, RenderOptions(..), defaultRenderOptions, zipperFront, zipperCurrent, zipperBack, extractMessages)
import UniversalLLM (Message(..))
import UI.UserInput.InputWidget (isWidgetComplete)
import qualified UI.Attributes as Attrs
import qualified UI.Widgets.MessageHistory as MH
import qualified UI.InputPanel as IP
import qualified Runix.LLM.Context as Context
import qualified UI.SegmentEditor as SE

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
  , _inputEditor :: SE.SegmentEditor Name  -- Input field with rich content
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

inputEditorL :: Lens' (AppState msg) (SE.SegmentEditor Name)
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
        , _inputEditor = SE.emptyEditor SE.EditorConfig
            { SE.editorName = InputEditor
            , SE.lineLimit = Nothing
            , SE.newlineMode = SE.EnterSends
            }
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

    -- Calculate dimensions (will be recalculated with actual context in baseLayer)
    availHeight = 100  -- This will be determined by context, placeholder for now
    maxInputHeight = max 1 (availHeight `div` 2)

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
          availW = ctx ^. T.availWidthL

          -- Update editor wrap width based on available width
          -- Account for prompt, padding, and borders
          -- Calculate input height based on editor lines
          editorLines = SE.getEditorLines (_inputEditor st)
          contentHeight = max 1 (length editorLines)
          inputHeight = min contentHeight maxInputHeight

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
                            SE.renderEditor True (_inputEditor st))
                    , hBorder
                    , renderStatusBar (_inputMode st) (_markdownMode st) tokens
                    ]
      T.render ui

    -- Indicator layer: rendered on top by Brick's renderFinal
    indicatorLayer = historyIndicators

    -- Render segment lines with highlighting
    renderSegmentLines :: [SE.SegmentLine] -> T.Widget Name
    renderSegmentLines segLines =
      vBox $ map renderSegmentLine segLines

    renderSegmentLine :: SE.SegmentLine -> T.Widget Name
    renderSegmentLine [] = str " "  -- Empty lines shown as space to preserve them
    renderSegmentLine segs = hBox $ map renderSegment segs

    renderSegment :: SE.InputSegment -> T.Widget Name
    renderSegment (SE.CharSegment c) = txt (Text.singleton c)
    renderSegment (SE.FileRefSegment paths _ SE.RefPending) =
      let path = case paths of (p:_) -> p; [] -> ""
      in withAttr Attrs.fileRefPendingAttr $ txt ("@" <> Text.pack path)
    renderSegment (SE.FileRefSegment paths _ SE.RefAccepted) =
      let path = case paths of (p:_) -> p; [] -> ""
      in withAttr Attrs.fileRefAcceptedAttr $ txt ("@" <> Text.pack path)
    renderSegment (SE.FileRefSegment paths _ SE.RefRejected) =
      let path = case paths of (p:_) -> p; [] -> ""
      in withAttr Attrs.fileRefRejectedAttr $ txt ("@" <> Text.pack path)
    renderSegment (SE.PastedSegment t) =
      withAttr Attrs.pastedAttr $ txt t

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
      newEditorMode = case newMode of
        EnterSends -> SE.EnterSends
        EnterNewline -> SE.EnterNewline
  inputModeL .= newMode
  inputEditorL %= SE.setNewlineMode newEditorMode

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

-- Tab: File completion for @file references (or cycle through existing matches)
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) = do
  ed <- use inputEditorL
  -- Check if segment before cursor is a FileRef - if so, rotate it
  case SE.getSegmentBeforeCursor ed of
    Just (SE.FileRefSegment _ _ _) ->
      inputEditorL .= SE.rotateFileRefAtCursor ed
    _ -> do
      -- Not after a file ref - check if we're typing one
      case SE.getWordBeforeCursor ed of
        Just (word, segCount) | "@" `Text.isPrefixOf` word -> do
          vars <- use uiVarsL
          let pattern = Text.drop 1 word  -- Remove @
          matches <- liftIO $ requestCompletion vars pattern
          case matches of
            [] -> return ()  -- No matches
            _ -> do
              let ed' = SE.deleteNSegments segCount ed
                  cleanMatches = map stripDotSlash (map Text.unpack matches)
                  ed'' = SE.insertFileRef cleanMatches pattern SE.RefAccepted ed'
              inputEditorL .= ed''
        _ -> return ()  -- Not a @ word, ignore tab
  where
    stripDotSlash ('.':'/':rest) = rest
    stripDotSlash path = path

-- Handle all other events through the segment editor
-- The editor returns (shouldSubmit, newEditor)
handleNormalEvent ev = do
  ed <- use inputEditorL
  (shouldSubmit, newEd) <- SE.handleEditorEvent ev ed
  inputEditorL .= newEd
  when shouldSubmit sendMessage

--------------------------------------------------------------------------------
-- Message Sending
--------------------------------------------------------------------------------

sendMessage :: T.EventM Name (AppState msg) ()
sendMessage = do
  ed <- use inputEditorL
  let content = Text.unpack $ SE.getEditorContent ed
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
      inputEditorL %= SE.clearEditor

      -- Scroll viewport to bottom
      M.vScrollToEnd (M.viewportScroll HistoryViewport)

      -- Send event to update viewport indicators after render (non-blocking)
      chan <- use eventChanL
      liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport
