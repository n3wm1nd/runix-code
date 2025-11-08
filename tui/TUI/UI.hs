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
import qualified Data.Text.Encoding as Text
import Data.Text.Zipper (cursorPosition, breakLine, deletePrevChar)
import Lens.Micro
import Lens.Micro.Mtl
import UniversalLLM.Core.Types (Message(..))
import Control.Monad.IO.Class (liftIO)

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
-- Generic over provider/model so it can work with any LLM backend.
data AppState model provider = AppState
  { _messages :: [Message model provider]  -- Message history (most recent last)
  , _inputEditor :: Editor String Name     -- Input field
  , _status :: String                      -- Status message
  , _inputMode :: InputMode                -- Current input mode
  , _onSend :: String -> IO [Message model provider]  -- Callback when user sends message
  }

--------------------------------------------------------------------------------
-- Lenses
--------------------------------------------------------------------------------

inputEditorL :: Lens' (AppState model provider) (Editor String Name)
inputEditorL = lens _inputEditor (\st e -> st { _inputEditor = e })

messagesL :: Lens' (AppState model provider) [Message model provider]
messagesL = lens _messages (\st m -> st { _messages = m })

statusL :: Lens' (AppState model provider) String
statusL = lens _status (\st s -> st { _status = s })

inputModeL :: Lens' (AppState model provider) InputMode
inputModeL = lens _inputMode (\st m -> st { _inputMode = m })

onSendL :: Lens' (AppState model provider) (String -> IO [Message model provider])
onSendL = lens _onSend (\st f -> st { _onSend = f })

--------------------------------------------------------------------------------
-- Message Rendering
--------------------------------------------------------------------------------

-- | Render a message to display string
--
-- For now, simple text extraction. Can be enhanced later with
-- syntax highlighting, formatting, etc.
renderMessage :: Message model provider -> [String]
renderMessage (UserText t) =
  ["You:"] ++ map ("  " ++) (lines (Text.unpack t))
renderMessage (AssistantText t) =
  ["Agent:"] ++ map ("  " ++) (lines (Text.unpack t))
renderMessage (AssistantTool tc) =
  ["Agent (tool call):"] ++ ["  " ++ show tc]
renderMessage (ToolResultMsg tr) =
  ["Tool result:"] ++ ["  " ++ show tr]
renderMessage (UserImage _desc _img) =
  ["[User sent image]"]
renderMessage (UserRequestJSON query j) =
  ["[User sent JSON]:"] ++ ["  Query: " ++ Text.unpack query] ++ ["  " ++ show j]
renderMessage (AssistantReasoning r) =
  ["[Agent reasoning]:"] ++ map ("  " ++) (lines (Text.unpack r))
renderMessage (AssistantJSON j) =
  ["[Agent sent JSON]:"] ++ ["  " ++ show j]
renderMessage (SystemText t) =
  ["[System]:"] ++ map ("  " ++) (lines (Text.unpack t))

--------------------------------------------------------------------------------
-- UI Entry Point
--------------------------------------------------------------------------------

-- | Run the TUI with initial messages and an agent callback
--
-- The agent callback takes user input and returns updated message history.
-- This keeps the UI generic - it doesn't know about LLM details.
runUI :: [Message model provider]  -- ^ Initial message history
      -> (String -> IO [Message model provider])  -- ^ Agent callback
      -> IO ()
runUI initialMessages agentCallback = do
  let initialState = AppState
        { _messages = initialMessages
        , _inputEditor = editor InputEditor Nothing ""
        , _status = "Ready"
        , _inputMode = EnterSends
        , _onSend = agentCallback
        }

  -- Create vty with bracketed paste enabled
  let buildVty = do
        v <- mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.BracketedPaste True
        return v
  initialVty <- buildVty
  _finalState <- M.customMain initialVty buildVty Nothing app initialState
  return ()

--------------------------------------------------------------------------------
-- Brick App Definition
--------------------------------------------------------------------------------

app :: M.App (AppState model provider) e Name
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

drawUI :: AppState model provider -> [T.Widget Name]
drawUI st = [ui]
  where
    ui = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let availHeight = ctx ^. T.availHeightL
          maxInputHeight = max 1 (availHeight `div` 2)  -- Max 50% of screen
          editorLines = getEditContents (_inputEditor st)
          contentHeight = max 1 (length editorLines)
          inputHeight = min contentHeight maxInputHeight

          historyHeight = availHeight - inputHeight - 2  -- -2 for border and status

          modeStr = case _inputMode st of
                      EnterSends -> "Enter: send"
                      EnterNewline -> "Enter: newline (Ctrl-D: send)"

          -- Render all messages
          historyLines = concatMap renderMessage (_messages st)

          mainUI = vBox
            [ vLimit historyHeight $ viewport HistoryViewport T.Vertical $
                vBox $ map strWrap historyLines
            , hBorder
            , vLimit inputHeight $
                renderEditor (vBox . map renderLine) True (_inputEditor st)
            , strWrap $ "Status: " ++ _status st ++ " | " ++ modeStr ++ " | \\<Enter>: newline | Ctrl-T: toggle | Ctrl-C: quit"
            ]
      T.render mainUI

    -- Render a line, showing empty lines as a space to preserve them
    renderLine :: String -> T.Widget Name
    renderLine "" = str " "
    renderLine s = str s

--------------------------------------------------------------------------------
-- Event Handling
--------------------------------------------------------------------------------

handleEvent :: T.BrickEvent Name e -> T.EventM Name (AppState model provider) ()
handleEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt

-- Ctrl-T toggles input mode
handleEvent (T.VtyEvent (V.EvKey (V.KChar 't') [V.MCtrl])) = do
  mode <- use inputModeL
  let newMode = case mode of
        EnterSends -> EnterNewline
        EnterNewline -> EnterSends
  inputModeL .= newMode
  statusL .= "Mode: " ++ show newMode

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

sendMessage :: T.EventM Name (AppState model provider) ()
sendMessage = do
  ed <- use inputEditorL
  let contentLines = getEditContents ed
      content = unlines contentLines
  if null (filter (/= ' ') content)
    then return ()  -- Don't send empty messages
    else do
      -- Get the agent callback
      agentCallback <- use onSendL

      -- Call the agent in IO
      newMessages <- liftIO $ agentCallback content

      -- Update history with new messages
      messagesL .= newMessages

      -- Clear input and update status
      inputEditorL .= editor InputEditor Nothing ""
      statusL .= "Message sent"

      -- Scroll viewport to bottom
      M.vScrollToEnd (M.viewportScroll HistoryViewport)
