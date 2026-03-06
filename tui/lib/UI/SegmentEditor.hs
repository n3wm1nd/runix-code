{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Rich segment-based editor for Runix Code TUI
--
-- This module provides a full-featured text editor that works with segments
-- instead of just characters. Segments can be:
-- - Plain text characters that can be edited normally
-- - File references that are deleted/navigated as atomic units
-- - Pasted content that can be deleted as a unit
--
-- The editor is built on top of a generic text zipper but with custom event
-- handling that understands segment boundaries.
module UI.SegmentEditor
  ( -- * Editor type
    SegmentEditor
  , EditorConfig(..)
  , NewlineMode(..)
  , InputSegment(..)
  , SegmentLine
  , RefState(..)
    -- * Construction
  , emptyEditor
  , editorFromText
  , editorFromSegments
    -- * Reading content
  , getEditorContent
  , getEditorLines
  , getEditorSegmentLines
  , getCursorPos
  , isEmpty
  , getSegmentAtCursor
  , getSegmentBeforeCursor
    -- * Event handling
  , handleEditorEvent
    -- * Editing operations
  , insertChar
  , insertText
  , insertSegment
  , insertFileRef
  , deleteBackward
  , deleteForward
  , deleteWordBackward
  , breakLine
  , replaceSegmentAtCursor
  , rotateFileRefAtCursor
  , moveCursorLeft
  , moveCursorRight
  , moveCursorUp
  , moveCursorDown
  , moveCursorToLineStart
  , moveCursorToLineEnd
  , moveCursorToStart
  , moveCursorToEnd
  , moveWordLeft
  , moveWordRight
  , clearEditor
  , setNewlineMode
  , getWordBeforeCursor
  , deleteNSegments
    -- * Rendering
  , renderEditor
  , renderEditorWithPrompt
  ) where

import Brick.Types (Widget, BrickEvent(..), EventM, Location(..), ViewportType(..))
import Brick.Widgets.Core
import Brick.AttrMap (AttrName, attrName)
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (isSpace, isAlphaNum)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A segment of input content
data InputSegment
  = CharSegment Char              -- ^ Single character (normal typing)
  | FileRefSegment
      { segRefPaths :: [FilePath]      -- ^ File paths (head is current selection)
      , segRefQuery :: Text            -- ^ What the user typed
      , segRefState :: RefState        -- ^ Reference state
      }
  | PastedSegment Text              -- ^ Pasted content (deleted as unit)
  deriving (Eq, Show)

-- | State of a file reference
data RefState = RefPending | RefAccepted | RefRejected
  deriving (Eq, Show)

-- | How the editor handles newline input
data NewlineMode
  = EnterSends        -- ^ Enter submits, Shift+Enter for literal newline
  | EnterNewline      -- ^ Enter adds newline, submission happens via other means
  | BackslashEscape   -- ^ \+Enter adds newline, plain Enter submits
  deriving (Eq, Show)

-- | Configuration for the editor
data EditorConfig n = EditorConfig
  { editorName :: n               -- ^ Widget name for focus/viewport
  , lineLimit :: Maybe Int        -- ^ Maximum number of lines (Nothing = unlimited)
  , newlineMode :: NewlineMode    -- ^ How to handle Enter key
  } deriving (Eq, Show)

-- | A line of segments
type SegmentLine = [InputSegment]

-- | The editor state
--
-- ZIPPER INVARIANTS (CRITICAL - READ THIS):
-- ==========================================
--
-- This is a two-level zipper: a zipper of lines, where the current line is itself a zipper of segments.
--
-- The "before" parts use CONS LIST ORDER: head = element closest to cursor
-- The "after" parts use READING ORDER: head = element closest to cursor
--
-- SEGMENT LEVEL (current line):
--   edLineBefore :: [InputSegment]
--     - Segments BEFORE cursor on current line
--     - Stored REVERSED: head = segment immediately before cursor
--     - Example: cursor after "ab" in "abcd" → ['b','a']
--
--   edLineAfter :: [InputSegment]
--     - Segments AFTER cursor on current line
--     - Stored NORMAL: head = segment immediately after cursor
--     - Example: cursor after "ab" in "abcd" → ['c','d']
--
-- LINE LEVEL (complete lines):
--   edLinesBefore :: [SegmentLine]
--     - Complete lines BEFORE current line
--     - List order: REVERSED (head = line immediately above current)
--     - Each line content: REVERSED (same format as edLineBefore would be at end of that line)
--     - Example: lines ["foo", "bar"] with cursor on 3rd line → [['r','a','b'], ['o','o','f']]
--
--   edLinesAfter :: [SegmentLine]
--     - Complete lines AFTER current line
--     - List order: NORMAL (head = line immediately below current)
--     - Each line content: NORMAL (same format as edLineAfter would be at start of that line)
--     - Example: next two lines are "foo", "bar" → [['f','o','o'], ['b','a','r']]
--
-- OPERATIONS MUST RESPECT THESE INVARIANTS:
--   - When storing current line in edLinesBefore: reverse edLineAfter ++ edLineBefore (= reversed)
--   - When storing current line in edLinesAfter: reverse edLineBefore ++ edLineAfter (= normal)
--   - When loading from edLinesBefore: line is already reversed, use directly or reverse to normal
--   - When loading from edLinesAfter: line is already normal, use directly or reverse to reversed
--   - splitLineAt takes NORMAL order, returns (REVERSED, NORMAL) for direct zipper use
--
-- DO NOT add extra reverses! The data is already in the correct format for cons list operations.
--
data SegmentEditor n = SegmentEditor
  { edConfig :: EditorConfig n
  , edLinesBefore :: [SegmentLine]  -- ^ Lines before current (reversed list, each line reversed)
  , edLineBefore :: SegmentLine     -- ^ Segments before cursor (reversed)
  , edLineAfter :: SegmentLine      -- ^ Segments after cursor (normal)
  , edLinesAfter :: [SegmentLine]   -- ^ Lines after current (normal list, each line normal)
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Create an empty editor
emptyEditor :: EditorConfig n -> SegmentEditor n
emptyEditor config = SegmentEditor
  { edConfig = config
  , edLinesBefore = []
  , edLineBefore = []
  , edLineAfter = []
  , edLinesAfter = []
  }

-- | Create an editor from plain text
editorFromText :: EditorConfig n -> Text -> SegmentEditor n
editorFromText cfg txt =
  let textLines = T.lines txt
      segLines = map (map CharSegment . T.unpack) textLines
  in editorFromSegments cfg segLines

-- | Create an editor from segment lines
editorFromSegments :: EditorConfig n -> [SegmentLine] -> SegmentEditor n
editorFromSegments config [] = emptyEditor config
editorFromSegments config (firstLine:rest) = SegmentEditor
  { edConfig = config
  , edLinesBefore = []
  , edLineBefore = []
  , edLineAfter = firstLine
  , edLinesAfter = rest
  }

--------------------------------------------------------------------------------
-- Reading content
--------------------------------------------------------------------------------

-- | Get all content as a single text string
getEditorContent :: SegmentEditor n -> Text
getEditorContent ed =
  T.intercalate "\n" (getEditorLines ed)

-- | Get content as list of lines (as Text)
getEditorLines :: SegmentEditor n -> [Text]
getEditorLines ed =
  -- edLinesBefore: each line is reversed, list is reversed
  -- Need to reverse list, then reverse each line
  let beforeLines = map reverse (reverse (edLinesBefore ed))
      currentLine = reverse (edLineBefore ed) ++ edLineAfter ed
      afterLines = edLinesAfter ed
      allLines = beforeLines ++ [currentLine] ++ afterLines
  in map segmentLineToText allLines

-- | Get content as list of segment lines (preserves structure)
getEditorSegmentLines :: SegmentEditor n -> [SegmentLine]
getEditorSegmentLines ed =
  map reverse (reverse (edLinesBefore ed))
  ++ [reverse (edLineBefore ed) ++ edLineAfter ed]
  ++ edLinesAfter ed

-- | Get cursor position (row, col) - 0-indexed
getCursorPos :: SegmentEditor n -> (Int, Int)
getCursorPos ed =
  let row = length (edLinesBefore ed)
      col = segmentLineLength (edLineBefore ed)
  in (row, col)

-- | Check if editor is empty
isEmpty :: SegmentEditor n -> Bool
isEmpty ed =
  null (edLinesBefore ed) &&
  null (edLineBefore ed) &&
  null (edLineAfter ed) &&
  null (edLinesAfter ed)

-- | Get the segment immediately after the cursor (if any)
getSegmentAtCursor :: SegmentEditor n -> Maybe InputSegment
getSegmentAtCursor ed = case edLineAfter ed of
  (seg:_) -> Just seg
  [] -> Nothing

-- | Get the segment immediately before the cursor (if any)
getSegmentBeforeCursor :: SegmentEditor n -> Maybe InputSegment
getSegmentBeforeCursor ed = case edLineBefore ed of
  (seg:_) -> Just seg
  [] -> Nothing

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Convert a segment line to text
segmentLineToText :: SegmentLine -> Text
segmentLineToText = mconcat . map segmentToText

-- | Generate display text for paste segment
-- Single-line: shows actual text, Multi-line: shows placeholder
pasteDisplayText :: Text -> Text
pasteDisplayText t
  | T.any (== '\n') t =
      let lineCount = length (T.lines t)
      in "[paste: " <> T.pack (show lineCount) <> " lines]"
  | otherwise = t

-- | Convert a single segment to text (actual content, not display)
segmentToText :: InputSegment -> Text
segmentToText (CharSegment c) = T.singleton c
segmentToText (FileRefSegment {segRefPaths = (path:_)}) = "@" <> T.pack path
segmentToText (FileRefSegment {segRefPaths = []}) = "@"  -- Shouldn't happen
segmentToText (PastedSegment t) = t  -- Always return actual content

-- | Get display length of a segment line
segmentLineLength :: SegmentLine -> Int
segmentLineLength = sum . map segmentLength

-- | Get display length of a segment
segmentLength :: InputSegment -> Int
segmentLength (CharSegment _) = 1
segmentLength (FileRefSegment {segRefPaths = (path:_)}) = 1 + length path
segmentLength (FileRefSegment {segRefPaths = []}) = 1
segmentLength (PastedSegment t) = T.length (pasteDisplayText t)

-- | Check if a segment is a word character (for word navigation)
isWordSegment :: InputSegment -> Bool
isWordSegment (CharSegment c) = isAlphaNum c || c == '_'
isWordSegment _ = True  -- File refs and pasted text count as words

-- | Check if a segment is whitespace
isSpaceSegment :: InputSegment -> Bool
isSpaceSegment (CharSegment c) = isSpace c
isSpaceSegment _ = False

--------------------------------------------------------------------------------
-- Event handling
--------------------------------------------------------------------------------

-- | Handle a Brick event, returning whether the editor wants to "submit"
-- Returns (shouldSubmit, newEditor)
handleEditorEvent :: BrickEvent n e -> SegmentEditor n -> EventM n s (Bool, SegmentEditor n)
handleEditorEvent (VtyEvent (EvKey KEnter [])) ed =
  case newlineMode (edConfig ed) of
    EnterSends ->
      -- Check if cursor is right after backslash
      case edLineBefore ed of
        (CharSegment '\\' : _) ->
          -- Remove backslash and insert newline
          let ed' = deleteBackward ed
              ed'' = breakLine ed'
          in return (False, ed'')
        _ -> return (True, ed)  -- Normal enter submits
    EnterNewline -> return (False, breakLine ed)
    BackslashEscape ->
      -- Check if cursor is right after backslash
      case edLineBefore ed of
        (CharSegment '\\' : _) ->
          -- Remove backslash and insert newline
          let ed' = deleteBackward ed
              ed'' = breakLine ed'
          in return (False, ed'')
        _ -> return (True, ed)  -- Normal enter submits

handleEditorEvent (VtyEvent (EvKey KEnter [MShift])) ed =
  -- Shift+Enter always inserts newline
  return (False, breakLine ed)

handleEditorEvent (VtyEvent (EvKey (KChar c) [])) ed
  | c /= '\t' = return (False, insertChar c ed)

handleEditorEvent (VtyEvent (EvKey KBS [])) ed =
  return (False, deleteBackward ed)

handleEditorEvent (VtyEvent (EvKey KDel [])) ed =
  return (False, deleteForward ed)

handleEditorEvent (VtyEvent (EvKey KLeft [])) ed =
  return (False, moveCursorLeft ed)

handleEditorEvent (VtyEvent (EvKey KRight [])) ed =
  return (False, moveCursorRight ed)

handleEditorEvent (VtyEvent (EvKey KUp [])) ed =
  return (False, moveCursorUp ed)

handleEditorEvent (VtyEvent (EvKey KDown [])) ed =
  return (False, moveCursorDown ed)

handleEditorEvent (VtyEvent (EvKey KHome [])) ed =
  return (False, moveCursorToLineStart ed)

handleEditorEvent (VtyEvent (EvKey KEnd [])) ed =
  return (False, moveCursorToLineEnd ed)

handleEditorEvent (VtyEvent (EvKey (KChar 'a') [MCtrl])) ed =
  return (False, moveCursorToLineStart ed)

handleEditorEvent (VtyEvent (EvKey (KChar 'e') [MCtrl])) ed =
  return (False, moveCursorToLineEnd ed)

handleEditorEvent (VtyEvent (EvKey (KChar 'w') [MCtrl])) ed =
  return (False, deleteWordBackward ed)

handleEditorEvent (VtyEvent (EvKey (KChar 'k') [MCtrl])) ed =
  -- Kill to end of line
  return (False, ed { edLineAfter = [] })

handleEditorEvent (VtyEvent (EvKey (KChar 'u') [MCtrl])) ed =
  -- Kill to start of line
  return (False, ed { edLineBefore = [] })

handleEditorEvent (VtyEvent (EvKey (KChar 'b') [MMeta])) ed =
  return (False, moveWordLeft ed)

handleEditorEvent (VtyEvent (EvKey (KChar 'f') [MMeta])) ed =
  return (False, moveWordRight ed)

handleEditorEvent (VtyEvent (EvKey KLeft [MCtrl])) ed =
  return (False, moveWordLeft ed)

handleEditorEvent (VtyEvent (EvKey KRight [MCtrl])) ed =
  return (False, moveWordRight ed)

handleEditorEvent (VtyEvent (EvKey (KChar '<') [MMeta])) ed =
  return (False, moveCursorToStart ed)

handleEditorEvent (VtyEvent (EvKey (KChar '>') [MMeta])) ed =
  return (False, moveCursorToEnd ed)

handleEditorEvent (VtyEvent (EvPaste bs)) ed =
  case TE.decodeUtf8' bs of
    Left _ -> return (False, ed)  -- Ignore invalid UTF-8
    Right pastedTxt -> return (False, insertSegment (PastedSegment pastedTxt) ed)

-- Tab and other keys would be handled by caller (for completion, etc.)
handleEditorEvent _ ed = return (False, ed)

--------------------------------------------------------------------------------
-- Editing operations
--------------------------------------------------------------------------------

-- | Insert a single character at cursor
insertChar :: Char -> SegmentEditor n -> SegmentEditor n
insertChar c ed = ed { edLineBefore = CharSegment c : edLineBefore ed }

-- | Insert text at cursor (as CharSegments)
insertText :: Text -> SegmentEditor n -> SegmentEditor n
insertText textToInsert ed =
  let chars = T.unpack textToInsert
      segments = map CharSegment chars
  in ed { edLineBefore = reverse segments ++ edLineBefore ed }

-- | Insert a segment at cursor
insertSegment :: InputSegment -> SegmentEditor n -> SegmentEditor n
insertSegment seg ed = ed { edLineBefore = seg : edLineBefore ed }

-- | Insert a file reference at cursor
insertFileRef :: [FilePath] -> Text -> RefState -> SegmentEditor n -> SegmentEditor n
insertFileRef paths query state ed =
  insertSegment (FileRefSegment
    { segRefPaths = paths
    , segRefQuery = query
    , segRefState = state
    }) ed

-- | Replace the segment after the cursor with a new one
-- Useful for updating file refs after tab completion
replaceSegmentAtCursor :: InputSegment -> SegmentEditor n -> SegmentEditor n
replaceSegmentAtCursor newSeg ed =
  case edLineAfter ed of
    (_:rest) -> ed { edLineAfter = newSeg : rest }
    [] -> ed  -- Nothing to replace

-- | Rotate file reference alternatives (looks at segment BEFORE cursor)
rotateFileRefAtCursor :: SegmentEditor n -> SegmentEditor n
rotateFileRefAtCursor ed =
  case edLineBefore ed of
    (FileRefSegment {segRefPaths = (current:rest), segRefQuery = query, segRefState = state} : beforeSegs) ->
      case rest of
        [] -> ed  -- Only one path, nothing to rotate
        (next:others) ->
          let rotated = FileRefSegment
                { segRefPaths = next : (others ++ [current])
                , segRefQuery = query
                , segRefState = state
                }
          in ed { edLineBefore = rotated : beforeSegs }
    _ -> ed  -- Not a file ref or empty paths

-- | Clear the entire editor
clearEditor :: SegmentEditor n -> SegmentEditor n
clearEditor ed = emptyEditor (edConfig ed)

-- | Update the newline mode
setNewlineMode :: NewlineMode -> SegmentEditor n -> SegmentEditor n
setNewlineMode mode ed =
  let cfg = edConfig ed
  in ed { edConfig = cfg { newlineMode = mode } }

-- | Get the word before cursor (for completion)
-- Returns the word text and the number of segments it spans
getWordBeforeCursor :: SegmentEditor n -> Maybe (Text, Int)
getWordBeforeCursor ed = go (edLineBefore ed) [] 0
  where
    go [] acc count
      | null acc = Nothing
      | otherwise = Just (T.pack acc, count)
    go (CharSegment c : rest) acc count
      | c == ' ' || c == '\n' =
          if null acc
          then Nothing
          else Just (T.pack acc, count)
      | otherwise = go rest (c : acc) (count + 1)
    go (_otherSegment:_rest) acc count
      -- Hit a non-CharSegment (FileRef, Pasted, etc)
      | null acc = Nothing
      | otherwise = Just (T.pack acc, count)

-- | Delete N segments backward
deleteNSegments :: Int -> SegmentEditor n -> SegmentEditor n
deleteNSegments 0 ed = ed
deleteNSegments n ed = deleteNSegments (n - 1) (deleteBackward ed)

-- | Delete backward (delete segment before cursor)
deleteBackward :: SegmentEditor n -> SegmentEditor n
deleteBackward ed =
  case edLineBefore ed of
    (_:rest) -> ed { edLineBefore = rest }
    [] -> case edLinesBefore ed of
      [] -> ed  -- At start of document
      (prevLine:prevLines) ->
        -- Join with previous line - move to end of previous line
        -- edLineAfter stays as is (normal order), prevLine is already reversed
        ed { edLinesBefore = prevLines
           , edLineBefore = prevLine
           -- edLineAfter stays the same (current line content after cursor)
           }

-- | Delete forward (delete segment after cursor)
deleteForward :: SegmentEditor n -> SegmentEditor n
deleteForward ed =
  case edLineAfter ed of
    (_:rest) -> ed { edLineAfter = rest }
    [] -> case edLinesAfter ed of
      [] -> ed  -- At end of document
      (nextLine:nextLines) ->
        -- Join with next line
        ed { edLinesAfter = nextLines
           , edLineAfter = nextLine
           }

-- | Delete word backward
deleteWordBackward :: SegmentEditor n -> SegmentEditor n
deleteWordBackward ed = goSpaces ed
  where
    goSpaces edCurrent = case edLineBefore edCurrent of
      [] -> edCurrent  -- Nothing to delete
      (seg:_)
        | isSpaceSegment seg ->
            -- Delete spaces, then continue to word
            goSpaces (deleteBackward edCurrent)
        | isWordSegment seg ->
            -- Delete word segments
            deleteWord edCurrent
        | otherwise ->
            -- Delete single non-word segment (like file ref)
            deleteBackward edCurrent

    deleteWord edCurrent = case edLineBefore edCurrent of
      [] -> edCurrent
      (seg:_)
        | isWordSegment seg -> deleteWord (deleteBackward edCurrent)
        | otherwise -> edCurrent  -- Stop at non-word

-- | Break line at cursor
breakLine :: SegmentEditor n -> SegmentEditor n
breakLine ed =
  case lineLimit (edConfig ed) of
    Just lim ->
      let totalLines = length (edLinesBefore ed) + 1 + length (edLinesAfter ed)
      in if totalLines >= lim
         then ed  -- Don't exceed line limit
         else doBreak ed
    Nothing -> doBreak ed
  where
    doBreak ed' =
      -- Line break at cursor: edLineBefore becomes a complete line
      -- It's already in reversed format, perfect for edLinesBefore
      ed' { edLinesBefore = edLineBefore ed' : edLinesBefore ed'
          , edLineBefore = []
          -- edLineAfter stays as is (becomes the new line)
          }

--------------------------------------------------------------------------------
-- Cursor movement
--------------------------------------------------------------------------------

-- | Move cursor left (by one segment)
moveCursorLeft :: SegmentEditor n -> SegmentEditor n
moveCursorLeft ed =
  case edLineBefore ed of
    (seg:rest) ->
      ed { edLineBefore = rest
         , edLineAfter = seg : edLineAfter ed
         }
    [] -> case edLinesBefore ed of
      [] -> ed  -- At start of document
      (prevLine:prevLines) ->
        -- Move to end of previous line, store current line in edLinesAfter
        let currentLine = edLineAfter ed  -- current line is just edLineAfter (edLineBefore is [])
        in ed { edLinesBefore = prevLines
              , edLineBefore = prevLine  -- cursor at end of prevLine
              , edLineAfter = []
              , edLinesAfter = currentLine : edLinesAfter ed
              }

-- | Move cursor right (by one segment)
moveCursorRight :: SegmentEditor n -> SegmentEditor n
moveCursorRight ed =
  case edLineAfter ed of
    (seg:rest) ->
      ed { edLineBefore = seg : edLineBefore ed
         , edLineAfter = rest
         }
    [] -> case edLinesAfter ed of
      [] -> ed  -- At end of document
      (nextLine:nextLines) ->
        -- Move to start of next line, store current line in edLinesBefore
        let currentLine = edLineBefore ed  -- already reversed, perfect for edLinesBefore
        in ed { edLinesBefore = currentLine : edLinesBefore ed
              , edLineBefore = []
              , edLineAfter = nextLine
              , edLinesAfter = nextLines
              }

-- | Move cursor up one line
moveCursorUp :: SegmentEditor n -> SegmentEditor n
moveCursorUp ed =
  case edLinesBefore ed of
    [] -> ed  -- Already at first line
    (prevLine:prevLines) ->
      let col = segmentLineLength (edLineBefore ed)
          -- prevLine is reversed (from edLinesBefore), reverse to normal for splitLineAt
          (newBefore, newAfter) = splitLineAt col (reverse prevLine)
      in ed { edLinesAfter = (reverse (edLineBefore ed) ++ edLineAfter ed) : edLinesAfter ed
            , edLineAfter = newAfter
            , edLineBefore = newBefore
            , edLinesBefore = prevLines
            }

-- | Move cursor down one line
moveCursorDown :: SegmentEditor n -> SegmentEditor n
moveCursorDown ed =
  case edLinesAfter ed of
    [] -> ed  -- Already at last line
    (nextLine:nextLines) ->
      let col = segmentLineLength (edLineBefore ed)
          -- nextLine is normal (from edLinesAfter), use directly for splitLineAt
          (newBefore, newAfter) = splitLineAt col nextLine
      in ed { edLinesBefore = (reverse (edLineAfter ed) ++ edLineBefore ed) : edLinesBefore ed
            , edLineBefore = newBefore
            , edLineAfter = newAfter
            , edLinesAfter = nextLines
            }

-- | Split a line at a given column position (tries to get as close as possible)
-- Takes a line in normal order, returns (before reversed, after normal) for zipper
splitLineAt :: Int -> SegmentLine -> (SegmentLine, SegmentLine)
splitLineAt targetCol line = go 0 [] line
  where
    go _ acc [] = (acc, [])
    go col acc (seg:rest)
      | col >= targetCol = (acc, seg:rest)
      | otherwise = go (col + segmentLength seg) (seg:acc) rest

-- | Move cursor to start of current line
moveCursorToLineStart :: SegmentEditor n -> SegmentEditor n
moveCursorToLineStart ed =
  ed { edLineAfter = reverse (edLineBefore ed) ++ edLineAfter ed
     , edLineBefore = []
     }

-- | Move cursor to end of current line
moveCursorToLineEnd :: SegmentEditor n -> SegmentEditor n
moveCursorToLineEnd ed =
  ed { edLineBefore = reverse (edLineAfter ed) ++ edLineBefore ed
     , edLineAfter = []
     }

-- | Move cursor to start of document
moveCursorToStart :: SegmentEditor n -> SegmentEditor n
moveCursorToStart ed =
  case reverse (edLinesBefore ed) of
    [] -> moveCursorToLineStart ed  -- Already on first line
    (firstLine:restLines) ->
      let allLines = restLines ++ [reverse (edLineBefore ed) ++ edLineAfter ed] ++ edLinesAfter ed
      in ed { edLinesBefore = []
            , edLineBefore = []
            , edLineAfter = firstLine
            , edLinesAfter = allLines
            }

-- | Move cursor to end of document
moveCursorToEnd :: SegmentEditor n -> SegmentEditor n
moveCursorToEnd ed =
  case reverse (edLinesAfter ed) of
    [] -> moveCursorToLineEnd ed  -- Already on last line
    (lastLine:restLines) ->
      let allLines = edLinesBefore ed ++ [reverse (edLineBefore ed) ++ edLineAfter ed] ++ reverse restLines
      in ed { edLinesBefore = allLines
            , edLineBefore = lastLine
            , edLineAfter = []
            , edLinesAfter = []
            }

-- | Move cursor one word to the left
moveWordLeft :: SegmentEditor n -> SegmentEditor n
moveWordLeft ed = goSpaces ed
  where
    goSpaces edCurrent = case edLineBefore edCurrent of
      [] -> edCurrent  -- At start of line
      (seg:_)
        | isSpaceSegment seg ->
            -- Skip over spaces
            goSpaces (moveCursorLeft edCurrent)
        | isWordSegment seg ->
            -- Skip over word segments
            skipWord edCurrent
        | otherwise ->
            -- Stop at non-word segment
            moveCursorLeft edCurrent

    skipWord edCurrent = case edLineBefore edCurrent of
      [] -> edCurrent
      (seg:_)
        | isWordSegment seg -> skipWord (moveCursorLeft edCurrent)
        | otherwise -> edCurrent  -- Stop before non-word

-- | Move cursor one word to the right
moveWordRight :: SegmentEditor n -> SegmentEditor n
moveWordRight ed = goSpaces ed
  where
    goSpaces edCurrent = case edLineAfter edCurrent of
      [] -> edCurrent  -- At end of line
      (seg:_)
        | isSpaceSegment seg ->
            -- Skip over spaces
            goSpaces (moveCursorRight edCurrent)
        | isWordSegment seg ->
            -- Skip over word segments
            skipWord edCurrent
        | otherwise ->
            -- Stop before non-word segment
            moveCursorRight edCurrent

    skipWord edCurrent = case edLineAfter edCurrent of
      [] -> edCurrent
      (seg:_)
        | isWordSegment seg -> skipWord (moveCursorRight edCurrent)
        | otherwise -> edCurrent  -- Stop at non-word

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render the editor with optional focus
-- This version renders segments with special styling
renderEditor :: (Ord n, Show n)
             => Bool  -- ^ Has focus
             -> SegmentEditor n
             -> Widget n
renderEditor hasFocus ed =
  let allLines = getEditorSegmentLines ed
      lineWidgets = map renderSegmentLine allLines
      content = vBox lineWidgets
      (row, col) = getCursorPos ed
      cursorLoc = Location (col, row)
      name = editorName (edConfig ed)
  in withAttr (if hasFocus then editFocusedAttr else editAttr) $
     viewport name Both $
     (if hasFocus then showCursor name cursorLoc else id) $
     content

-- | Render the editor with a prompt prefix
renderEditorWithPrompt :: (Ord n, Show n)
                       => Text      -- ^ Prompt text
                       -> Bool      -- ^ Has focus
                       -> SegmentEditor n
                       -> Widget n
renderEditorWithPrompt prompt hasFocus ed =
  let allLines = getEditorSegmentLines ed
      renderedLines = case allLines of
        [] -> [txt prompt <+> txt " "]
        (firstLine:restLines) ->
          (txt prompt <+> txt " " <+> renderSegmentLine firstLine)
          : map (\line -> txt (T.replicate (T.length prompt + 1) " ") <+> renderSegmentLine line) restLines
      content = vBox renderedLines
      (row, col) = getCursorPos ed
      -- Adjust cursor location for prompt
      cursorLoc = Location (col + T.length prompt + 1, row)
      name = editorName (edConfig ed)
  in withAttr (if hasFocus then editFocusedAttr else editAttr) $
     viewport name Both $
     (if hasFocus then showCursor name cursorLoc else id) $
     content

-- | Render a line of segments with appropriate styling
renderSegmentLine :: SegmentLine -> Widget n
renderSegmentLine [] = txt " "  -- Empty line must render something to maintain height
renderSegmentLine segs = hBox (map renderSegment segs)

-- | Render a single segment with appropriate styling
renderSegment :: InputSegment -> Widget n
renderSegment (CharSegment c) = txt (T.singleton c)
renderSegment (FileRefSegment {segRefPaths = (path:_), segRefState = RefPending}) =
  withAttr fileRefPendingAttr $ txt $ "@" <> T.pack path
renderSegment (FileRefSegment {segRefPaths = (path:_), segRefState = RefAccepted}) =
  withAttr fileRefAcceptedAttr $ txt $ "@" <> T.pack path
renderSegment (FileRefSegment {segRefPaths = (path:_), segRefState = RefRejected}) =
  withAttr fileRefRejectedAttr $ txt $ "@" <> T.pack path
renderSegment (FileRefSegment {segRefPaths = []}) =
  txt "@"  -- Shouldn't happen
renderSegment (PastedSegment t) =
  let displayText = pasteDisplayText t
      attr = if T.any (== '\n') t then pastedMarkerAttr else pastedAttr
  in withAttr attr $ txt displayText

-- | Attribute for unfocused editor
editAttr :: AttrName
editAttr = attrName "edit"

-- | Attribute for focused editor
editFocusedAttr :: AttrName
editFocusedAttr = editAttr <> attrName "focused"

-- | Attribute for pending file references
fileRefPendingAttr :: AttrName
fileRefPendingAttr = attrName "fileRefPending"

-- | Attribute for accepted file references
fileRefAcceptedAttr :: AttrName
fileRefAcceptedAttr = attrName "fileRefAccepted"

-- | Attribute for rejected file references
fileRefRejectedAttr :: AttrName
fileRefRejectedAttr = attrName "fileRefRejected"

-- | Attribute for pasted content
pastedAttr :: AttrName
pastedAttr = attrName "pasted"

-- | Attribute for multi-line paste marker/placeholder
pastedMarkerAttr :: AttrName
pastedMarkerAttr = attrName "pastedMarker"
