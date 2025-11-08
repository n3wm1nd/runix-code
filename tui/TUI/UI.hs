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
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Brick.BChan (newBChan, writeBChan)

import UI.State (UIVars(..), UIState(..), provideUserInput, readUIState)

-- | Custom events for the TUI
data CustomEvent = RefreshUI
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
  , _cachedUIState :: UIState              -- Cached copy of UI state for rendering
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
-- writes user input to the TMVar when user sends a message.
runUI :: UIVars  -- ^ STM variables for UI state
      -> IO ()
runUI uiVars = do
  -- Read initial UI state
  initialUIState <- atomically $ readUIState (uiStateVar uiVars)

  let initialState = AppState
        { _uiVars = uiVars
        , _inputEditor = editor InputEditor Nothing ""
        , _inputMode = EnterSends
        , _cachedUIState = initialUIState
        }

  -- Create event channel for periodic refreshes
  eventChan <- newBChan 10

  -- Fork a thread that sends periodic refresh events
  _ <- forkIO $ forever $ do
    threadDelay 100000  -- 100ms = 10 FPS
    writeBChan eventChan RefreshUI

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
drawUI st = [ui]
  where
    cached = _cachedUIState st
    ui = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let availHeight = ctx ^. T.availHeightL
          maxInputHeight = max 1 (availHeight `div` 2)  -- Max 50% of screen
          editorLines = getEditContents (_inputEditor st)
          contentHeight = max 1 (length editorLines)
          inputHeight = min contentHeight maxInputHeight

          historyHeight = availHeight - inputHeight - 3  -- -3 for border, status, logs

          modeStr = case _inputMode st of
                      EnterSends -> "Enter: send"
                      EnterNewline -> "Enter: newline (Ctrl-D: send)"

          -- Render all display messages
          historyLines = concatMap renderDisplayText (uiDisplayMessages cached)

          -- Render log messages (show last 5)
          logLines = map Text.unpack $ reverse $ take 5 $ reverse $ uiLogs cached

          mainUI = vBox
            [ vLimit historyHeight $ viewport HistoryViewport T.Vertical $
                vBox $ map strWrap historyLines
            , hBorder
            , vLimit inputHeight $
                renderEditor (vBox . map renderLine) True (_inputEditor st)
            , strWrap $ "Status: " ++ Text.unpack (uiStatus cached) ++ " | " ++ modeStr ++ " | \\<Enter>: newline | Ctrl-T: toggle | Ctrl-C: quit"
            , if null logLines then str "" else strWrap $ "Logs: " ++ unlines logLines
            ]
      T.render mainUI

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

-- Periodically refresh UI state from STM
handleEvent (T.AppEvent RefreshUI) = do
  vars <- use uiVarsL
  newUIState <- liftIO $ atomically $ readUIState (uiStateVar vars)
  cachedUIStateL .= newUIState

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
      liftIO $ atomically $ provideUserInput (userInputVar vars) (Text.pack content)

      -- Clear input
      inputEditorL .= editor InputEditor Nothing ""

      -- Scroll viewport to bottom
      M.vScrollToEnd (M.viewportScroll HistoryViewport)
