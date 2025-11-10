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

import UI.State (UIVars(..), UIState(..), Name(..), provideUserInput, readUIState, uiStateVar)
import UI.OutputHistory (RenderedMessage(..), shouldDisplay, renderOutputMessage, renderOutputMessageRaw)
import qualified UI.Attributes as Attrs
import qualified TUI.Widgets.MessageHistory as MH

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
    cached = _cachedUIState st

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

    -- Filter and extract cached widgets from output history
    filteredOutput = filter (shouldDisplay (uiDisplayFilter cached) . rmMessage) (uiOutputHistory cached)

    -- Extract cached widgets based on markdown mode (NO re-rendering!)
    historyWidgets = case _markdownMode st of
      RenderMarkdown ->
        -- Use cached markdown widgets, reverse to oldest-first for display, add spacing
        renderHistoryWithSpacing (map rmMarkdownWidgets (reverse filteredOutput))
      ShowRaw ->
        -- Use cached raw widgets, reverse to oldest-first for display, add spacing
        renderHistoryWithSpacing (map rmRawWidgets (reverse filteredOutput))

    -- Add spacing between messages (blank lines before/after)
    renderHistoryWithSpacing :: [[T.Widget Name]] -> [T.Widget Name]
    renderHistoryWithSpacing = concatMap (\widgets -> txt " " : widgets ++ [txt " "])

    statusText = Text.unpack (uiStatus cached)

    -- MessageHistory returns (base, indicators) for Brick's layer system
    (historyBase, historyIndicators) = MH.messageHistory HistoryViewport (_lastViewport st) historyWidgets

    -- Base layer: main UI
    baseLayer = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let availH = ctx ^. T.availHeightL
          historyH = availH - inputHeight - 3  -- -3 for border, status

          ui = vBox
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
handleEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt

-- Refresh UI state from STM and scroll to bottom
handleEvent (T.AppEvent RefreshUI) = do
  vars <- use uiVarsL
  newUIState <- liftIO $ atomically $ readUIState (uiStateVar vars)
  cachedUIStateL .= newUIState
  -- Scroll viewport to bottom to show new content
  M.vScrollToEnd (M.viewportScroll HistoryViewport)
  -- Send event to update viewport indicators after render (non-blocking)
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

-- Update cached viewport state (called after scrolling/rendering)
handleEvent (T.AppEvent UpdateViewport) = do
  mVp <- M.lookupViewport HistoryViewport
  lastViewportL .= mVp

-- Handle window resize - stick to bottom if we were already there
handleEvent (T.VtyEvent (V.EvResize _ _)) = do
  -- Check if we're at the bottom before resize
  wasAtBottom <- use lastViewportL >>= \case
    Nothing -> return True  -- Default to bottom if no viewport yet
    Just vp -> return $ MH.isAtBottom vp

  -- If we were at bottom, scroll to bottom after resize
  when wasAtBottom $ do
    M.vScrollToEnd (M.viewportScroll HistoryViewport)

  -- Update viewport state after resize (non-blocking)
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

-- Page Up/Down for scrolling history
handleEvent (T.VtyEvent (V.EvKey V.KPageUp [])) = do
  M.vScrollPage (M.viewportScroll HistoryViewport) T.Up
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

handleEvent (T.VtyEvent (V.EvKey V.KPageDown [])) = do
  M.vScrollPage (M.viewportScroll HistoryViewport) T.Down
  chan <- use eventChanL
  liftIO $ void $ Brick.BChan.writeBChanNonBlocking chan UpdateViewport

-- Ctrl-T toggles input mode
handleEvent (T.VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl])) = do
  mode <- use inputModeL
  let newMode = case mode of
        EnterSends -> EnterNewline
        EnterNewline -> EnterSends
  inputModeL .= newMode

-- Ctrl-R toggles markdown rendering mode (R for "raw")
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = do
  mode <- use markdownModeL
  let newMode = case mode of
        RenderMarkdown -> ShowRaw
        ShowRaw -> RenderMarkdown
  markdownModeL .= newMode

-- Ctrl-D sends message (useful in EnterNewline mode)
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl])) = sendMessage

-- Handle paste events - insert content without triggering send
handleEvent (T.VtyEvent (V.EvPaste pastedBytes)) = do
  let pastedText = Text.decodeUtf8 pastedBytes
  ed <- use inputEditorL
  let currentLines = getEditContents ed
      newContent = unlines currentLines ++ Text.unpack pastedText
  inputEditorL .= editor InputEditor Nothing newContent

-- Enter key behavior depends on mode and backslash handling
handleEvent (T.VtyEvent (V.EvKey V.KEnter [])) = do
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
handleEvent (T.VtyEvent (V.EvKey V.KEnter [V.MShift])) = do
  zoom inputEditorL $ handleEditorEvent (T.VtyEvent (V.EvKey V.KEnter []))

handleEvent ev = do
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
