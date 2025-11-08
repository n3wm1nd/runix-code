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
    -- * Types
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
import Control.Monad (when)
import Control.Concurrent.STM
import qualified Brick.BChan
import Brick.BChan (newBChan, writeBChan)

import UI.State (UIVars(..), UIState(..), provideUserInput, readUIState, uiStateVar)
import UI.OutputHistory (OutputMessage, shouldDisplay, renderOutputMessage)

-- | Custom events for the TUI
data CustomEvent = RefreshUI | UpdateViewport
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Resource names for widgets
data Name = InputEditor | HistoryViewport
  deriving stock (Eq, Ord, Show)

-- | Input mode: what Enter key does
data InputMode = EnterSends | EnterNewline
  deriving stock (Eq, Show)

-- | TUI application state
--
-- Now uses STM-based state for concurrent updates from effect interpreters.
data AppState = AppState
  { _uiVars :: UIVars                      -- STM variables for UI state
  , _inputEditor :: Editor String Name     -- Input field
  , _inputMode :: InputMode                -- Current input mode
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
  eventChan <- newBChan 10

  -- Create UI vars with refresh callback
  uiVars <- mkUIVars $ writeBChan eventChan RefreshUI

  -- Read initial UI state
  initialUIState <- atomically $ readUIState (uiStateVar uiVars)

  let initialState = AppState
        { _uiVars = uiVars
        , _inputEditor = editor InputEditor Nothing ""
        , _inputMode = EnterSends
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
  , M.appAttrMap = const $ A.attrMap V.defAttr []
  , M.appChooseCursor = M.showFirstCursor
  }

--------------------------------------------------------------------------------
-- UI Rendering
--------------------------------------------------------------------------------

drawUI :: AppState -> [T.Widget Name]
drawUI st = [indicatorLayer, baseLayer]  -- Try reversed order
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

    -- Filter and render output history
    filteredOutput = filter (shouldDisplay (uiDisplayFilter cached)) (uiOutputHistory cached)
    historyLines = concatMap (map Text.unpack . renderOutputMessage) filteredOutput

    statusText = Text.unpack (uiStatus cached)

    -- Base layer: main UI without indicators
    baseLayer = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let availH = ctx ^. T.availHeightL
          historyH = availH - inputHeight - 3  -- -3 for border, status

          mainUI = vBox
            [ vLimit historyH $ viewport HistoryViewport T.Vertical $
                vBox $ map strWrap historyLines
            , hBorder
            , vLimit inputHeight $
                renderEditor (vBox . map renderLine) True (_inputEditor st)
            , strWrap $ "Status: " ++ statusText ++ " | " ++ modeStr ++ " | \\<Enter>: newline | Ctrl-T: toggle | Ctrl-C: quit"
            ]
      T.render mainUI

    -- Indicator layer: use translateBy to position indicators without affecting layout
    indicatorLayer = T.Widget T.Fixed T.Fixed $ do
      ctx <- T.getContext
      let screenWidth = ctx ^. T.availWidthL

      case _lastViewport st of
        Nothing -> return T.emptyResult  -- No indicators if viewport not available
        Just vp -> do
          let scrollTop = vp ^. T.vpTop
              visibleHeight = snd (vp ^. T.vpSize)
              totalContentHeight = snd (vp ^. T.vpContentSize)
              scrollBottom = scrollTop + visibleHeight

              rowsAbove = scrollTop
              rowsBelow = max 0 (totalContentHeight - scrollBottom)

              topText = "↑" ++ show rowsAbove ++ "↑"
              bottomText = "↓" ++ show rowsBelow ++ "↓"

          -- Create Vty images at absolute positions - these overlay without affecting layout
          let topImg = if rowsAbove > 0
                      then V.translateX (screenWidth - length topText) $
                           V.translateY 0 $
                           V.string V.defAttr topText
                      else V.emptyImage

              bottomImg = if rowsBelow > 0
                         then V.translateX (screenWidth - length bottomText) $
                              V.translateY (visibleHeight - 1) $
                              V.string V.defAttr bottomText
                         else V.emptyImage

              combinedImg = topImg V.<-> bottomImg

          return $ T.emptyResult & T.imageL .~ combinedImg

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
  -- Send event to update viewport indicators after render
  chan <- use eventChanL
  liftIO $ writeBChan chan UpdateViewport

-- Update cached viewport state (called after scrolling/rendering)
handleEvent (T.AppEvent UpdateViewport) = do
  mVp <- M.lookupViewport HistoryViewport
  lastViewportL .= mVp

-- Handle window resize - stick to bottom if we were already there
handleEvent (T.VtyEvent (V.EvResize _ _)) = do
  -- Check if we're at the bottom before resize
  wasAtBottom <- use lastViewportL >>= \case
    Nothing -> return True  -- Default to bottom if no viewport yet
    Just vp -> do
      let scrollTop = vp ^. T.vpTop
          visibleHeight = snd (vp ^. T.vpSize)
          totalContentHeight = snd (vp ^. T.vpContentSize)
          scrollBottom = scrollTop + visibleHeight
          atBottom = scrollBottom >= totalContentHeight
      return atBottom

  -- If we were at bottom, scroll to bottom after resize
  when wasAtBottom $ do
    M.vScrollToEnd (M.viewportScroll HistoryViewport)

  -- Update viewport state after resize
  chan <- use eventChanL
  liftIO $ writeBChan chan UpdateViewport

-- Page Up/Down for scrolling history
handleEvent (T.VtyEvent (V.EvKey V.KPageUp [])) = do
  M.vScrollPage (M.viewportScroll HistoryViewport) T.Up
  chan <- use eventChanL
  liftIO $ writeBChan chan UpdateViewport

handleEvent (T.VtyEvent (V.EvKey V.KPageDown [])) = do
  M.vScrollPage (M.viewportScroll HistoryViewport) T.Down
  chan <- use eventChanL
  liftIO $ writeBChan chan UpdateViewport

-- Ctrl-T toggles input mode
handleEvent (T.VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl])) = do
  mode <- use inputModeL
  let newMode = case mode of
        EnterSends -> EnterNewline
        EnterNewline -> EnterSends
  inputModeL .= newMode

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

      -- Send event to update viewport indicators after render
      chan <- use eventChanL
      liftIO $ writeBChan chan UpdateViewport
