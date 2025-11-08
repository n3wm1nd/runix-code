-- | Brick-based TUI for runix-code
module Main (main) where

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
import Lens.Micro
import Lens.Micro.Mtl

-- | Resource names for widgets
data Name = InputEditor | HistoryViewport
  deriving stock (Eq, Ord, Show)

-- | Application state
data AppState = AppState
  { _messages :: [String]        -- Message history (most recent last)
  , _inputEditor :: Editor String Name  -- Input field
  , _status :: String             -- Status message
  }

-- Lenses for AppState
inputEditorL :: Lens' AppState (Editor String Name)
inputEditorL = lens _inputEditor (\st e -> st { _inputEditor = e })

messagesL :: Lens' AppState [String]
messagesL = lens _messages (\st m -> st { _messages = m })

statusL :: Lens' AppState String
statusL = lens _status (\st s -> st { _status = s })

-- | Initialize application
initialState :: AppState
initialState = AppState
  { _messages = ["Welcome to Runix Code TUI!", "Type your message (multiline supported).", "Press Enter to send, Shift-Enter for newline, Ctrl-C to quit."]
  , _inputEditor = editor InputEditor Nothing ""  -- Nothing = unlimited lines
  , _status = "Ready"
  }

-- | Draw the UI
drawUI :: AppState -> [T.Widget Name]
drawUI st = [ui]
  where
    ui = T.Widget T.Greedy T.Greedy $ do
      ctx <- T.getContext
      let availHeight = ctx ^. T.availHeightL
          maxInputHeight = max 1 (availHeight `div` 2)  -- Max 50% of screen
          editorLines = getEditContents (_inputEditor st)
          contentHeight = max 1 (length editorLines)  -- At least 1 line
          inputHeight = min contentHeight maxInputHeight

          historyHeight = availHeight - inputHeight - 2  -- -2 for border and status

          mainUI = vBox
            [ vLimit historyHeight $ viewport HistoryViewport T.Vertical $
                vBox $ map strWrap (_messages st)  -- Word wrap history
            , hBorder
            , vLimit inputHeight $
                renderEditor (vBox . map renderLine) True (_inputEditor st)
            , strWrap $ "Status: " ++ _status st ++ " | Enter: send, Shift-Enter: newline, Ctrl-C: quit"
            ]
      T.render mainUI

    -- Render a line, showing empty lines as a space to preserve them
    -- NOTE: Can't use strWrap here because Editor creates its own viewport
    renderLine :: String -> T.Widget Name
    renderLine "" = str " "  -- Empty line
    renderLine s = str s      -- No wrapping in editor (would conflict with viewport)

-- | Handle events
handleEvent :: T.BrickEvent Name e -> T.EventM Name AppState ()
handleEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
handleEvent (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = M.halt

-- Handle paste events - insert content without triggering send
handleEvent (T.VtyEvent (V.EvPaste pastedBytes)) = do
  -- Convert ByteString to String and insert into editor
  let pastedText = Text.decodeUtf8 pastedBytes
  ed <- use inputEditorL
  let currentLines = getEditContents ed
      newContent = unlines currentLines ++ Text.unpack pastedText
  inputEditorL .= editor InputEditor Nothing newContent

-- Regular Enter sends the message (only when typed, not pasted)
handleEvent (T.VtyEvent (V.EvKey V.KEnter [])) = sendMessage

-- Shift-Enter also inserts newline for terminals that support it
handleEvent (T.VtyEvent (V.EvKey V.KEnter [V.MShift])) = do
  zoom inputEditorL $ handleEditorEvent (T.VtyEvent (V.EvKey V.KEnter []))

handleEvent ev = do
  -- Delegate other events to the editor
  zoom inputEditorL $ handleEditorEvent ev

-- Helper to send message
sendMessage :: T.EventM Name AppState ()
sendMessage = do
  ed <- use inputEditorL
  let contentLines = getEditContents ed
      content = unlines contentLines
  if null (filter (/= ' ') content)
    then return ()  -- Don't send empty messages
    else do
      -- Add user message to history (show each line)
      messagesL %= (++ ["You:"] ++ map ("  " ++) contentLines)
      -- Simulate agent response (for now)
      let agentResponse = "Agent: Echo - " ++ show (length contentLines) ++ " lines"
      messagesL %= (++ [agentResponse])
      -- Clear input and update status
      inputEditorL %= const (editor InputEditor Nothing "")
      statusL %= const "Message sent"
      -- Scroll viewport to bottom to show new messages
      M.vScrollToEnd (M.viewportScroll HistoryViewport)

-- | Brick app definition
app :: M.App AppState e Name
app = M.App
  { M.appDraw = drawUI
  , M.appHandleEvent = handleEvent
  , M.appStartEvent = return ()
  , M.appAttrMap = const $ A.attrMap V.defAttr []
  , M.appChooseCursor = M.showFirstCursor
  }

-- | Main entry point
main :: IO ()
main = do
  -- Create vty with bracketed paste enabled
  let buildVty = do
        v <- mkVty V.defaultConfig
        V.setMode (V.outputIface v) V.BracketedPaste True
        return v
  initialVty <- buildVty
  _finalState <- M.customMain initialVty buildVty Nothing app initialState
  return ()
