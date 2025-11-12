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
import Control.Monad (when, void)
import Control.Concurrent.STM
import qualified Brick.BChan
import Brick.BChan (newBChan, writeBChan)

import UI.State (UIVars(..), UIState(..), Name(..), provideUserInput, readUIState, uiStateVar, clearPendingInput, SomeInputWidget(..))
import UI.OutputHistory (RenderedMessage(..), OutputMessage(..), shouldDisplay)
import UI.UserInput.InputWidget (isWidgetComplete)
import qualified UI.Attributes as Attrs
import qualified TUI.Widgets.MessageHistory as MH
import qualified TUI.InputPanel as IP
import Graphics.Vty (Key(..), Event(..))

-- | Custom events for the TUI
data CustomEvent = RefreshUI | UpdateViewport
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
-- Now uses STM-based state for concurrent updates from effect interpreters.
data AppState = AppState
  { _uiVars :: UIVars                      -- STM variables for UI state
  , _inputEditor :: Editor String Name     -- Input field
  , _inputMode :: InputMode                -- Current input mode
  , _markdownMode :: MarkdownMode          -- Whether to render markdown or show raw
  , _cachedUIState :: UIState              -- Cached copy updated on each refresh event
  , _lastViewport :: Maybe T.Viewport      -- Last viewport state for scroll indicators
  , _eventChan :: Brick.BChan.BChan CustomEvent  -- Event channel for sending custom events
  }

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

uiVarsL :: Lens' AppState UIVars
uiVarsL = lens _uiVars (\st v -> st { _uiVars = v })

inputEditorL :: Lens' AppState (Editor String Name)
inputEditorL = lens _inputEditor (\st e -> st { _inputEditor = e })

inputModeL :: Lens' AppState InputMode
inputModeL = lens _inputMode (\st m -> st { _inputMode = m })

markdownModeL :: Lens' AppState MarkdownMode
markdownModeL = lens _markdownMode (\st m -> st { _markdownMode = m })

cachedUIStateL :: Lens' AppState UIState
cachedUIStateL = lens _cachedUIState (\st s -> st { _cachedUIState = s })

lastViewportL :: Lens' AppState (Maybe T.Viewport)
lastViewportL = lens _lastViewport (\st v -> st { _lastViewport = v })

eventChanL :: Lens' AppState (Brick.BChan.BChan CustomEvent)
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
-- The UI reads from UIVars (written to by effect interpreters) and
-- writes user input to the TQueue when user sends a message.
-- Refreshes are triggered by the effect interpreters, not by polling.
runUI :: (IO () -> IO UIVars)  -- ^ Function to create UIVars with refresh callback
      -> IO ()
runUI mkUIVars = do
  -- Create event channel for UI refreshes
  -- Size 1 to coalesce multiple refresh requests - if channel has a pending refresh,
  -- we don't need to queue another one (writeBChanNonBlocking will return False)
  eventChan <- newBChan 1

  -- Create UI vars with refresh callback that coalesces requests
  uiVars <- mkUIVars $ do
    -- Non-blocking write - if channel is full (already has a pending refresh), returns False
    _ <- Brick.BChan.writeBChanNonBlocking eventChan RefreshUI
    return ()

  -- Read initial UI state
  initialUIState <- atomically $ readUIState (uiStateVar uiVars)

  let initialState = AppState
        { _uiVars = uiVars
        , _inputEditor = editor InputEditor Nothing ""
        , _inputMode = EnterSends
        , _markdownMode = RenderMarkdown
        , _cachedUIState = initialUIState
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

app :: M.App AppState CustomEvent Name
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

drawUI :: AppState -> [T.Widget Name]
drawUI st = [indicatorLayer, baseLayer]
  where
    uiState = _cachedUIState st

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

    -- Separate streaming chunks from completed output
    (streamingMsgs, completedMsgs) = span isStreaming (uiOutputHistory uiState)

    isStreaming m = case rmMessage m of
      StreamingChunk _ -> True
      StreamingReasoning _ -> True
      _ -> False

    -- Filter based on display filter
    filteredCompleted = filter (shouldDisplay (uiDisplayFilter uiState) . rmMessage) completedMsgs
    filteredStreaming = filter (shouldDisplay (uiDisplayFilter uiState) . rmMessage) streamingMsgs

    -- Extract completed widgets (cached, oldest-first for display)
    completedWidgets = case _markdownMode st of
      RenderMarkdown -> concatMap rmMarkdownWidgets (reverse filteredCompleted)
      ShowRaw -> concatMap rmRawWidgets (reverse filteredCompleted)

    -- Extract streaming widgets (not cached, oldest-first for display)
    streamingWidgets = case _markdownMode st of
      RenderMarkdown -> concatMap rmMarkdownWidgets (reverse filteredStreaming)
      ShowRaw -> concatMap rmRawWidgets (reverse filteredStreaming)

    -- Combine: cache completed (including logs), don't cache streaming
    historyWidgets =
      if null completedWidgets
      then streamingWidgets
      else if null streamingWidgets
        then [cached CompletedHistory (vBox completedWidgets)]
        else [cached CompletedHistory (vBox completedWidgets), hBorder] ++ streamingWidgets

    statusText = Text.unpack (uiStatus uiState)

    -- MessageHistory returns (base, indicators) for Brick's layer system
    (historyBase, historyIndicators) = MH.messageHistory HistoryViewport (_lastViewport st) historyWidgets

    -- Base layer: main UI
    baseLayer = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let availH = ctx ^. T.availHeightL

          -- Check if we have a pending input widget
          ui = case uiPendingInput uiState of
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

handleEvent :: T.BrickEvent Name CustomEvent -> T.EventM Name AppState ()
-- Check if input widget is active first
handleEvent ev = do
  vars <- use uiVarsL
  uiState <- use cachedUIStateL

  case uiPendingInput uiState of
    Just widget -> handleInputWidgetEvent widget ev
    Nothing -> handleNormalEvent ev

-- Handle events when input widget is active
handleInputWidgetEvent :: SomeInputWidget -> T.BrickEvent Name CustomEvent -> T.EventM Name AppState ()
-- Esc cancels the input widget
handleInputWidgetEvent widget (T.VtyEvent (V.EvKey V.KEsc [])) = do
  vars <- use uiVarsL
  liftIO $ clearPendingInput vars

-- Enter confirms and submits the current value
handleInputWidgetEvent widget@(SomeInputWidget _ currentValue submitCallback) (T.VtyEvent (V.EvKey V.KEnter [])) = do
  vars <- use uiVarsL
  -- Check if value is complete
  if isWidgetComplete currentValue
    then do
      -- Submit the value (triggers callback)
      liftIO $ submitCallback currentValue
      -- Clear the widget from UI (will be done by interpreter, but we can do it here too)
      liftIO $ clearPendingInput vars
    else
      return ()  -- Don't submit if incomplete

-- All other events go to the widget
handleInputWidgetEvent widget ev = do
  vars <- use uiVarsL
  -- Run the widget event handler which uses EventM Name () instead of AppState
  ((), mNewWidget) <- T.nestEventM () $ IP.handleInputPanelEvent widget ev
  -- Update the widget in UIState if it changed
  case mNewWidget of
    Just newWidget -> do
      uiState <- use cachedUIStateL
      let newUIState = uiState { uiPendingInput = Just newWidget }
      cachedUIStateL .= newUIState
      -- Also update in the shared state var
      liftIO $ atomically $ modifyTVar' (uiStateVar vars) $ \st ->
        st { uiPendingInput = Just newWidget }
    Nothing -> return ()

-- Normal event handling (when no input widget active)
handleNormalEvent :: T.BrickEvent Name CustomEvent -> T.EventM Name AppState ()
handleNormalEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt

-- Refresh UI state from STM and scroll to bottom
handleNormalEvent (T.AppEvent RefreshUI) = do
  vars <- use uiVarsL
  oldUIState <- use cachedUIStateL
  newUIState <- liftIO $ atomically $ readUIState (uiStateVar vars)

  -- Check if completed (non-streaming) output changed
  let oldCompleted = filter (not . isStreamingMsg . rmMessage) (uiOutputHistory oldUIState)
      newCompleted = filter (not . isStreamingMsg . rmMessage) (uiOutputHistory newUIState)
      isStreamingMsg (StreamingChunk _) = True
      isStreamingMsg (StreamingReasoning _) = True
      isStreamingMsg _ = False

  -- Invalidate cache if completed output changed
  when (length oldCompleted /= length newCompleted) $
    invalidateCacheEntry CompletedHistory

  cachedUIStateL .= newUIState
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
  -- Invalidate cache since we're switching between markdown/raw rendering
  invalidateCacheEntry CompletedHistory

-- Ctrl-D sends message (useful in EnterNewline mode)
handleNormalEvent (T.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = sendMessage

-- Handle paste events - insert content without triggering send
handleNormalEvent (T.VtyEvent (V.EvPaste pastedBytes)) = do
  let pastedText = Text.decodeUtf8 pastedBytes
  ed <- use inputEditorL
  let currentLines = getEditContents ed
      newContent = unlines currentLines ++ Text.unpack pastedText
  inputEditorL .= editor InputEditor Nothing newContent

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

sendMessage :: T.EventM Name AppState ()
sendMessage = do
  ed <- use inputEditorL
  let contentLines = getEditContents ed
      content = unlines contentLines
  if null (filter (/= ' ') content)
    then return ()  -- Don't send empty messages
    else do
      -- Get the UI vars
      vars <- use uiVarsL

      -- Send user input to the agent via TMVar
      liftIO $ atomically $ provideUserInput (userInputQueue vars) (Text.pack content)

      -- Clear input
      inputEditorL .= editor InputEditor Nothing ""

      -- Scroll viewport to bottom
      M.vScrollToEnd (M.viewportScroll HistoryViewport)

      -- Send event to update viewport indicators after render (non-blocking)
      chan <- use eventChanL
      liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport
