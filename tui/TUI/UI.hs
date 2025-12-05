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
module TUI.UI
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
import qualified Brick.AttrMap as A
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Zipper (cursorPosition, breakLine, deletePrevChar)
import Lens.Micro
import Lens.Micro.Mtl
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, void, forM_, unless)
import Control.Concurrent.STM
import qualified Brick.BChan
import Brick.BChan (newBChan, writeBChan)

import UI.State (UIVars(..), Name(..), provideUserInput, requestCancelFromUI, SomeInputWidget(..), AgentEvent(..))
import UI.OutputHistory (Zipper(..), OutputHistoryZipper, OutputItem(..), emptyZipper, appendItem, updateCurrent, renderItem, RenderOptions(..), defaultRenderOptions, zipperFront, zipperCurrent, zipperBack, zipperToList, listToZipper, mergeOutputMessages)
import Runix.Logging.Effects (Level(..))
import UniversalLLM.Core.Types (Message(..))
import UI.UserInput.InputWidget (isWidgetComplete)
import qualified UI.Attributes as Attrs
import qualified TUI.Widgets.MessageHistory as MH
import qualified TUI.InputPanel as IP
import Graphics.Vty (Key(..), Event(..))

-- | Custom events for the TUI
data CustomEvent msg
  = AgentEvent (AgentEvent msg)  -- ^ Event from agent thread
  | UpdateViewport               -- ^ Update viewport state after render
  deriving (Eq, Show)

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
data AppState msg = AppState
  { _uiVars :: UIVars msg                  -- STM queues for communication
  , _outputZipper :: OutputHistoryZipper msg  -- History zipper with typed messages (data)
  , _widgetZipper :: Zipper (T.Widget Name)  -- Pre-rendered widgets (parallel structure)
  , _status :: Text                         -- Current status message
  , _pendingInput :: Maybe SomeInputWidget  -- Active input widget (cached from TVar)
  , _inputEditor :: Editor String Name     -- Input field
  , _inputMode :: InputMode                -- Current input mode
  , _markdownMode :: MarkdownMode          -- Whether to render markdown or show raw
  , _lastViewport :: Maybe T.Viewport      -- Last viewport state for scroll indicators
  , _eventChan :: Brick.BChan.BChan (CustomEvent msg)  -- Event channel for sending custom events
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

inputEditorL :: Lens' (AppState msg) (Editor String Name)
inputEditorL = lens _inputEditor (\st e -> st { _inputEditor = e })

inputModeL :: Lens' (AppState msg) InputMode
inputModeL = lens _inputMode (\st m -> st { _inputMode = m })

markdownModeL :: Lens' (AppState msg) MarkdownMode
markdownModeL = lens _markdownMode (\st m -> st { _markdownMode = m })

lastViewportL :: Lens' (AppState msg) (Maybe T.Viewport)
lastViewportL = lens _lastViewport (\st v -> st { _lastViewport = v })

eventChanL :: Lens' (AppState msg) (Brick.BChan.BChan (CustomEvent msg))
eventChanL = lens _eventChan (\st c -> st { _eventChan = c })

--------------------------------------------------------------------------------
-- Display Rendering
--------------------------------------------------------------------------------

-- | Render display text to lines
--
-- Display messages come pre-formatted from the effect interpreters.
renderDisplayText :: Text -> [String]
renderDisplayText = lines . Text.unpack

--------------------------------------------------------------------------------
-- UI Entry Point
--------------------------------------------------------------------------------

-- | Run the TUI with STM-based state
--
-- The UI is parametrized over the Message type (specific model/provider instances)
-- Messages are rendered directly by pattern matching on Message constructors
--
-- Refreshes are triggered by the effect interpreters, not by polling.
runUI :: forall model provider. Eq (Message model provider) =>
         ((AgentEvent (Message model provider) -> IO ()) -> IO (UIVars (Message model provider)))  -- ^ Function to create UIVars with send callback
      -> IO ()
runUI mkUIVars = do
  -- Create event channel - agent events and UI events use same channel
  -- This ensures proper ordering and eliminates the separate refresh signal
  eventChan <- newBChan 10  -- Reasonable buffer size

  -- Create UI vars with callback that writes AgentEvents wrapped in CustomEvent
  let sendAgentEventCallback agentEvent = Brick.BChan.writeBChan eventChan (AgentEvent agentEvent)
  uiVars <- mkUIVars sendAgentEventCallback

  let initialState = AppState
        { _uiVars = uiVars
        , _outputZipper = emptyZipper
        , _widgetZipper = emptyZipper
        , _status = Text.pack "Ready"
        , _pendingInput = Nothing
        , _inputEditor = editor InputEditor Nothing ""
        , _inputMode = EnterSends
        , _markdownMode = RenderMarkdown
        , _lastViewport = Nothing
        , _eventChan = eventChan
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

app :: forall model provider. Eq (Message model provider) => M.App (AppState (Message model provider)) (CustomEvent (Message model provider)) Name
app = M.App
  { M.appDraw = drawUI
  , M.appHandleEvent = handleEvent
  , M.appStartEvent = return ()
  , M.appAttrMap = const Attrs.theMap
  , M.appChooseCursor = M.showFirstCursor
  }

--------------------------------------------------------------------------------
-- UI Rendering
--------------------------------------------------------------------------------

-- Draw UI is specific to Message types since renderItem requires it
drawUI :: forall model provider. AppState (Message model provider) -> [T.Widget Name]
drawUI st = [indicatorLayer, baseLayer]
  where
    -- Read state directly from AppState
    status = _status st
    mPendingInput = _pendingInput st
    wzipper = _widgetZipper st

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
                  historyH = availH - panelHeight - 1  -- -1 for status
               in vBox
                    [ vLimit historyH historyBase
                    , inputPanel
                    , strWrap $ "Status: " ++ statusText ++ " | Input widget active | Esc: cancel"
                    ]

            -- Normal input mode
            Nothing ->
              let historyH = availH - inputHeight - 3  -- -3 for border, status
               in vBox
                    [ vLimit historyH historyBase
                    , hBorder
                    , vLimit inputHeight $
                        renderEditor (vBox . map renderLine) True (_inputEditor st)
                    , strWrap $ "Status: " ++ statusText ++ " | " ++ modeStr ++ " | " ++ markdownStr ++ " | \\<Enter>: newline | Ctrl-T: toggle input | Ctrl-R: toggle markdown | Ctrl-C: quit"
                    ]
      T.render ui

    -- Indicator layer: rendered on top by Brick's renderFinal
    indicatorLayer = historyIndicators

    -- Render a line, showing empty lines as a space to preserve them
    renderLine :: String -> T.Widget Name
    renderLine "" = str " "
    renderLine s = str s

--------------------------------------------------------------------------------
-- Event Handling
--------------------------------------------------------------------------------

-- | Re-render the widget zipper from the output zipper
-- Call this after: (1) output zipper changes, (2) markdown mode changes
reRenderWidgetZipper :: T.EventM Name (AppState (Message model provider)) ()
reRenderWidgetZipper = do
  mode <- use markdownModeL
  ozipper <- use outputZipperL
  let opts = defaultRenderOptions { useMarkdown = case mode of
                                      RenderMarkdown -> True
                                      ShowRaw -> False }
      -- Render each OutputItem to Widget Name
      renderZipper (Zipper back current front) =
        Zipper
          { zipperBack = map (renderItem opts) back
          , zipperCurrent = fmap (renderItem opts) current
          , zipperFront = map (renderItem opts) front
          }
  widgetZipperL .= renderZipper ozipper

handleEvent :: forall model provider. Eq (Message model provider) => T.BrickEvent Name (CustomEvent (Message model provider)) -> T.EventM Name (AppState (Message model provider)) ()
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
handleInputWidgetEvent widget@(SomeInputWidget _ currentValue submitCallback) (T.VtyEvent (V.EvKey V.KEnter [])) = do
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

-- Normal event handling (when no input widget active)
handleNormalEvent :: forall model provider. Eq (Message model provider) => T.BrickEvent Name (CustomEvent (Message model provider)) -> T.EventM Name (AppState (Message model provider)) ()
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

-- Handle agent events directly from the event channel
handleNormalEvent (T.AppEvent (AgentEvent event)) = do
  -- Process the agent event
  case event of
      StreamChunkEvent text -> do
        -- Accumulate streaming text: if current is StreamingChunkItem, append to it
        zipper <- use outputZipperL
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
        case zipperCurrent zipper of
          Just (StreamingChunkItem prevText) -> do
            let newItem = StreamingChunkItem (prevText <> text)
            outputZipperL %= updateCurrent newItem
            -- Only re-render current, not entire zipper
            widgetZipperL %= updateCurrent (renderItem opts newItem)
            invalidateCacheEntry CachedCurrent
          _ -> do
            -- First chunk: create new streaming item
            let newItem = StreamingChunkItem text
            outputZipperL %= appendItem newItem
            widgetZipperL %= appendItem (renderItem opts newItem)
            invalidateCacheEntry CachedFront
            invalidateCacheEntry CachedCurrent

      StreamReasoningEvent text -> do
        -- Accumulate reasoning text similarly
        zipper <- use outputZipperL
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
        case zipperCurrent zipper of
          Just (StreamingReasoningItem prevText) -> do
            let newItem = StreamingReasoningItem (prevText <> text)
            outputZipperL %= updateCurrent newItem
            widgetZipperL %= updateCurrent (renderItem opts newItem)
            invalidateCacheEntry CachedCurrent
          _ -> do
            let newItem = StreamingReasoningItem text
            outputZipperL %= appendItem newItem
            widgetZipperL %= appendItem (renderItem opts newItem)
            invalidateCacheEntry CachedFront
            invalidateCacheEntry CachedCurrent

      UserMessageEvent msg -> do
        -- Add user message as new current
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
            newItem = MessageItem msg
        outputZipperL %= appendItem newItem
        widgetZipperL %= appendItem (renderItem opts newItem)
        invalidateCacheEntry CachedFront
        invalidateCacheEntry CachedCurrent

      AgentCompleteEvent msgs -> do
        -- Use mergeOutputMessages to properly merge messages with existing output
        zipper <- use outputZipperL
        let currentItems = zipperToList zipper  -- newest-first

            -- Remove streaming items first
            withoutStreaming = filter (\case
                StreamingChunkItem _ -> False
                StreamingReasoningItem _ -> False
                _ -> True) currentItems

            -- Convert messages to OutputItems (newest-first)
            newItems = map MessageItem (reverse msgs)

            -- Merge using the tested merge logic
            mergedItems = mergeOutputMessages newItems withoutStreaming

        -- Convert back to zipper (full rebuild)
        outputZipperL .= listToZipper mergedItems
        reRenderWidgetZipper
        -- Agent is done, update status
        statusL .= Text.pack "Ready"
        -- Invalidate all caches (full rebuild of zipper structure)
        invalidateCacheEntry CachedFront
        invalidateCacheEntry CachedCurrent
        invalidateCacheEntry CachedBack

      AgentErrorEvent text -> do
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
            newItem = SystemEventItem text
        outputZipperL %= appendItem newItem
        widgetZipperL %= appendItem (renderItem opts newItem)
        statusL .= Text.append (Text.pack "Error: ") text
        invalidateCacheEntry CachedFront
        invalidateCacheEntry CachedCurrent

      LogEvent level text -> do
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
            newItem = LogItem level text
        outputZipperL %= appendItem newItem
        widgetZipperL %= appendItem (renderItem opts newItem)
        invalidateCacheEntry CachedFront
        invalidateCacheEntry CachedCurrent

      ToolExecutionEvent text -> do
        mode <- use markdownModeL
        let opts = defaultRenderOptions { useMarkdown = case mode of RenderMarkdown -> True; ShowRaw -> False }
            newItem = ToolExecutionItem text
        outputZipperL %= appendItem newItem
        widgetZipperL %= appendItem (renderItem opts newItem)
        invalidateCacheEntry CachedFront
        invalidateCacheEntry CachedCurrent

      ShowInputWidgetEvent widget ->
        pendingInputL .= Just widget

      ClearInputWidgetEvent ->
        pendingInputL .= Nothing

  -- Scroll viewport to bottom to show new content
  M.vScrollToEnd (M.viewportScroll HistoryViewport)
  -- Send event to update viewport indicators after render (non-blocking)
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

  -- Invalidate cache since word wrapping changes on resize
  invalidateCacheEntry CompletedHistory

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
      let currentLine = contentLines !! row
      if col <= length currentLine && col > 0 && currentLine !! (col - 1) == '\\'
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

handleNormalEvent ev = do
  -- Delegate other events to the editor
  zoom inputEditorL $ handleEditorEvent ev

--------------------------------------------------------------------------------
-- Message Sending
--------------------------------------------------------------------------------

sendMessage :: T.EventM Name (AppState msg) ()
sendMessage = do
  ed <- use inputEditorL
  let contentLines = getEditContents ed
      content = unlines contentLines
  if null (filter (/= ' ') content)
    then return ()  -- Don't send empty messages
    else do
      -- Get the UI vars
      vars <- use uiVarsL

      -- Set status to Processing immediately
      statusL .= Text.pack "Processing..."

      -- Send user input to the agent via TMVar
      liftIO $ atomically $ provideUserInput (userInputQueue vars) (Text.pack content)

      -- Clear input
      inputEditorL .= editor InputEditor Nothing ""

      -- Scroll viewport to bottom
      M.vScrollToEnd (M.viewportScroll HistoryViewport)

      -- Send event to update viewport indicators after render (non-blocking)
      chan <- use eventChanL
      liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport
