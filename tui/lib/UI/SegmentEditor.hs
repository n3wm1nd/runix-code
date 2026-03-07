{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Rich segment-based editor for Runix Code TUI
--
-- This module provides a full-featured text editor that works with segments
-- instead of just characters. Segments can be:
-- - Plain text characters that can be edited normally
-- - Newline characters (stored explicitly, not implied by structure)
-- - File references that are deleted/navigated as atomic units
-- - Pasted content that can be deleted as a unit
--
-- ARCHITECTURE:
-- =============
-- The editor is a LINEAR ZIPPER of segments with a cursor position.
-- Logically, it's just [before segments] ++ cursor ++ [after segments].
--
-- Movement:
--   - forward/back: move through the linear segment list
--   - left/right/up/down: cursor positioning (derived from forward/back + newline awareness)
--
-- Editing:
--   - Insert operations just cons onto the before list
--   - Newlines are stored as NewlineSegment, not structural
--   - Display logic converts linear zipper to 2D lines for rendering
--
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
import Brick.Widgets.Core (viewport, vBox, hBox, txt, withAttr, showCursor, (<+>))
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
  = CharSegment Char              -- ^ Single character (normal typing, including '\n')
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

-- | A line is a 1D zipper of segments
-- (segments before cursor reversed, segments after cursor normal)
type SegmentLine = ([InputSegment], [InputSegment])

-- | The editor state - A 2D ZIPPER
--
-- ARCHITECTURE:
-- =============
-- The editor is a 2D zipper: a zipper of lines, where the current line is itself a 1D zipper.
-- This structure provides efficient local edits while maintaining a logical 1D view.
--
-- INVARIANTS:
-- ===========
-- edLinesBefore :: [SegmentLine]
--   - Complete lines BEFORE the current line
--   - Stored as REVERSED CONS LIST: head = line immediately above cursor
--   - Each line stored REVERSED: as if cursor is at end of that line
--   - Example: two lines above ["foo", "bar"] → [(['r','a','b'],[]), (['o','o','f'],[])]
--
-- line :: SegmentLine
--   - The current line being edited (1D zipper)
--   - (before, after) where before is reversed, after is normal
--   - Example: "ab|cd" → (['b','a'], ['c','d'])
--
-- edLinesAfter :: [SegmentLine]
--   - Complete lines AFTER the current line
--   - Stored as NORMAL LIST: head = line immediately below cursor
--   - Each line stored NORMAL: as if cursor is at start of that line
--   - Example: two lines below ["foo", "bar"] → [([], ['f','o','o']), ([], ['b','a','r'])]
--
-- OPERATIONS:
-- ===========
-- Data operations work on the 1D zipper (the current line).
-- Structural fixups maintain the 2D projection (moving between lines, splitting/joining).
--
data SegmentEditor n = SegmentEditor
  { edConfig :: EditorConfig n
  , edLinesBefore :: [SegmentLine]  -- ^ Lines before current (reversed cons list)
  , line :: SegmentLine              -- ^ Current line (1D zipper)
  , edLinesAfter :: [SegmentLine]    -- ^ Lines after current (normal list)
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Create an empty editor
emptyEditor :: EditorConfig n -> SegmentEditor n
emptyEditor config = SegmentEditor
  { edConfig = config
  , edLinesBefore = []
  , line = ([], [])
  , edLinesAfter = []
  }

-- | Create an editor from plain text
editorFromText :: EditorConfig n -> Text -> SegmentEditor n
editorFromText cfg txt =
  let textLines = T.lines txt
      segmentLines = map (\t -> ([], map CharSegment (T.unpack t))) textLines
  in case segmentLines of
       [] -> emptyEditor cfg
       (firstLine:restLines) -> SegmentEditor
         { edConfig = cfg
         , edLinesBefore = []
         , line = firstLine
         , edLinesAfter = restLines
         }

-- | Create an editor from segment list (cursor at start)
editorFromSegments :: EditorConfig n -> [SegmentLine] -> SegmentEditor n
editorFromSegments config [] = emptyEditor config
editorFromSegments config (firstLine:restLines) = SegmentEditor
  { edConfig = config
  , edLinesBefore = []
  , line = firstLine
  , edLinesAfter = restLines
  }

--------------------------------------------------------------------------------
-- Reading content
--------------------------------------------------------------------------------

-- | Get all content as a single text string
getEditorContent :: SegmentEditor n -> Text
getEditorContent ed =
  let beforeLines = reverse (edLinesBefore ed)
      currentLine = line ed
      afterLines = edLinesAfter ed
      allLines = beforeLines ++ [currentLine] ++ afterLines
      lineTexts = map segmentLineToText allLines
  in T.intercalate "\n" lineTexts

-- | Get content as list of lines (as Text)
getEditorLines :: SegmentEditor n -> [Text]
getEditorLines ed =
  let beforeLines = reverse (edLinesBefore ed)
      currentLine = line ed
      afterLines = edLinesAfter ed
      allLines = beforeLines ++ [currentLine] ++ afterLines
  in map segmentLineToText allLines

-- | Get content as list of segment lines (preserves structure)
getEditorSegmentLines :: SegmentEditor n -> [SegmentLine]
getEditorSegmentLines ed =
  reverse (edLinesBefore ed) ++ [line ed] ++ edLinesAfter ed

-- | Get cursor position (row, col) - 0-indexed
getCursorPos :: SegmentEditor n -> (Int, Int)
getCursorPos ed =
  let row = length (edLinesBefore ed)
      (before, _) = line ed
      col = sum (map segmentLength before)
  in (row, col)

-- | Check if editor is empty
isEmpty :: SegmentEditor n -> Bool
isEmpty ed =
  let (before, after) = line ed
  in null (edLinesBefore ed) && null before && null after && null (edLinesAfter ed)

-- | Get the segment immediately after the cursor (if any)
getSegmentAtCursor :: SegmentEditor n -> Maybe InputSegment
getSegmentAtCursor ed =
  let (_, after) = line ed
  in case after of
       (seg:_) -> Just seg
       [] -> Nothing

-- | Get the segment immediately before the cursor (if any)
getSegmentBeforeCursor :: SegmentEditor n -> Maybe InputSegment
getSegmentBeforeCursor ed =
  let (before, _) = line ed
  in case before of
       (seg:_) -> Just seg
       [] -> Nothing

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Convert a segment line to text
segmentLineToText :: SegmentLine -> Text
segmentLineToText (before, after) =
  let allSegs = reverse before ++ after
  in mconcat (map segmentToText allSegs)

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

-- | Get display length of a segment
segmentLength :: InputSegment -> Int
segmentLength (CharSegment '\n') = 0  -- Doesn't contribute to line length
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
-- Core Movement Primitives
--------------------------------------------------------------------------------

-- | Move forward one segment (crosses line boundaries)
forward :: SegmentEditor n -> SegmentEditor n
forward ed =
  let (before, after) = line ed
  in case after of
       (seg:rest) ->
         -- DATA: Move within current line
         ed { line = (seg : before, rest) }
       [] ->
         -- FIXUP: At end of line, move to next line
         case edLinesAfter ed of
           [] -> ed  -- At end of document
           (nextLine:nextLines) ->
             let completedLine = (before, [])  -- Current line completed (cursor at end)
             in ed { edLinesBefore = completedLine : edLinesBefore ed
                   , line = nextLine
                   , edLinesAfter = nextLines
                   }

-- | Move backward one segment (crosses line boundaries)
back :: SegmentEditor n -> SegmentEditor n
back ed =
  let (before, after) = line ed
  in case before of
       (seg:rest) ->
         -- DATA: Move within current line
         ed { line = (rest, seg : after) }
       [] ->
         -- FIXUP: At start of line, move to previous line
         case edLinesBefore ed of
           [] -> ed  -- At start of document
           (prevLine:prevLines) ->
             let completedLine = ([], after)  -- Current line completed (cursor at start)
             in ed { edLinesBefore = prevLines
                   , line = prevLine
                   , edLinesAfter = completedLine : edLinesAfter ed
                   }


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
      let (before, _) = line ed
      in case before of
           (CharSegment '\\' : _) ->
             -- Remove backslash and insert newline
             let ed' = deleteBackward ed
                 ed'' = breakLine ed'
             in return (False, ed'')
           _ -> return (True, ed)  -- Normal enter submits
    EnterNewline -> return (False, breakLine ed)
    BackslashEscape ->
      -- Check if cursor is right after backslash
      let (before, _) = line ed
      in case before of
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
  let (before, _after) = line ed
  in return (False, ed { line = (before, []) })

handleEditorEvent (VtyEvent (EvKey (KChar 'u') [MCtrl])) ed =
  -- Kill to start of line
  let (_before, after) = line ed
  in return (False, ed { line = ([], after) })

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
insertChar c ed =
  -- DATA: Insert character into current line (1D zipper operation)
  let (before, after) = line ed
  in ed { line = (CharSegment c : before, after) }

-- | Insert text at cursor (as CharSegments)
insertText :: Text -> SegmentEditor n -> SegmentEditor n
insertText textToInsert ed =
  -- DATA: Insert characters into current line (1D zipper operation)
  let chars = T.unpack textToInsert
      segments = map CharSegment chars
      (before, after) = line ed
  in ed { line = (reverse segments ++ before, after) }

-- | Insert a segment at cursor
insertSegment :: InputSegment -> SegmentEditor n -> SegmentEditor n
insertSegment seg ed =
  -- DATA: Insert segment into current line (1D zipper operation)
  let (before, after) = line ed
  in ed { line = (seg : before, after) }

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
  let (before, after) = line ed
  in case after of
       (_:rest) -> ed { line = (before, newSeg : rest) }
       [] -> ed  -- Nothing to replace

-- | Rotate file reference alternatives (looks at segment BEFORE cursor)
rotateFileRefAtCursor :: SegmentEditor n -> SegmentEditor n
rotateFileRefAtCursor ed =
  let (before, after) = line ed
  in case before of
       (FileRefSegment {segRefPaths = (current:rest), segRefQuery = query, segRefState = state} : beforeSegs) ->
         case rest of
           [] -> ed  -- Only one path, nothing to rotate
           (next:others) ->
             let rotated = FileRefSegment
                   { segRefPaths = next : (others ++ [current])
                   , segRefQuery = query
                   , segRefState = state
                   }
             in ed { line = (rotated : beforeSegs, after) }
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
getWordBeforeCursor ed =
  let (before, _) = line ed
  in go before [] 0
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
  let (before, after) = line ed
  in case before of
    (_:rest) ->
      -- DATA: Delete one segment from current line
      ed { line = (rest, after) }
    [] ->
      -- FIXUP: At start of line, join with previous line first
      case edLinesBefore ed of
        [] -> ed  -- At start of document, nothing to do
        (prevLine:prevLines) ->
          -- Join: move to end of previous line, append current line's after
          let (prevBefore, prevAfter) = prevLine
              newLine = (prevBefore, prevAfter ++ after)
          in ed { edLinesBefore = prevLines, line = newLine }

-- | Delete forward (delete segment after cursor)
deleteForward :: SegmentEditor n -> SegmentEditor n
deleteForward ed =
  let (before, after) = line ed
  in case after of
    (_:rest) ->
      -- DATA: Delete one segment from current line
      ed { line = (before, rest) }
    [] ->
      -- FIXUP: At end of line, join with next line first
      case edLinesAfter ed of
        [] -> ed  -- At end of document, nothing to do
        (nextLine:nextLines) ->
          -- Join: append next line's content to current line
          let (nextBefore, nextAfter) = nextLine
              newLine = (before, reverse nextBefore ++ nextAfter)
          in ed { edLinesAfter = nextLines, line = newLine }

-- | Delete word backward
deleteWordBackward :: SegmentEditor n -> SegmentEditor n
deleteWordBackward ed = goSpaces ed
  where
    goSpaces edCurrent =
      let (before, _after) = line edCurrent
      in case before of
        [] ->
          -- At start of current line, check if there are lines before
          case edLinesBefore edCurrent of
            [] -> edCurrent  -- Nothing to delete
            _ -> goSpaces (deleteBackward edCurrent)  -- Try previous line
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

    deleteWord edCurrent =
      let (before, _after) = line edCurrent
      in case before of
        [] -> edCurrent
        (seg:_)
          | isWordSegment seg -> deleteWord (deleteBackward edCurrent)
          | otherwise -> edCurrent  -- Stop at non-word

-- | Break line at cursor (insert newline)
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
      -- DATA: Conceptually inserting '\n' (but we don't store it as a segment)
      -- FIXUP: Split current line at cursor, push before part to edLinesBefore
      let (before, after) = line ed'
          completedLine = (before, [])  -- Line before cursor, cursor at end
      in ed' { edLinesBefore = completedLine : edLinesBefore ed'
             , line = ([], after)  -- New line starts with cursor at beginning
             }

--------------------------------------------------------------------------------
-- Cursor movement
--------------------------------------------------------------------------------

-- | Move cursor left (by one segment)
moveCursorLeft :: SegmentEditor n -> SegmentEditor n
moveCursorLeft = back

-- | Move cursor right (by one segment)
moveCursorRight :: SegmentEditor n -> SegmentEditor n
moveCursorRight = forward

-- | Check if at the front of the line zipper (cursor at start)
isInFrontZ :: SegmentLine -> Bool
isInFrontZ (before, _) = null before

-- | Check if at the back of the line zipper (cursor at end)
isInBackZ :: SegmentLine -> Bool
isInBackZ (_, after) = null after

-- | Move line zipper backward one segment
backZ :: SegmentLine -> SegmentLine
backZ ([], after) = ([], after)  -- Already at start
backZ (b:bs, after) = (bs, b:after)

-- | Move line zipper forward one segment
forwardZ :: SegmentLine -> SegmentLine
forwardZ (before, []) = (before, [])  -- Already at end
forwardZ (before, a:as) = (a:before, as)

-- | Move cursor up one line
moveCursorUp :: SegmentEditor n -> SegmentEditor n
moveCursorUp ed = rewindAndUp ed
  where
    rewindAndUp e
      | isInFrontZ (line e) =
          -- At start of line, move to previous line
          case edLinesBefore e of
            [] -> e  -- No previous line
            (prevLine:rest) ->
              let (_, after) = line e
              in e { edLinesBefore = rest
                   , line = prevLine
                   , edLinesAfter = ([], after) : edLinesAfter e
                   }
      | otherwise =
          -- Recurse back, then forward on return
          let e' = rewindAndUp (back e)
          in if isInBackZ (line e')
             then e'
             else forward e'

-- | Move cursor down one line
moveCursorDown :: SegmentEditor n -> SegmentEditor n
moveCursorDown ed = forwardAndDown ed
  where
    forwardAndDown e
      | isInBackZ (line e) =
          -- At end of line, move to next line
          case edLinesAfter e of
            [] -> e  -- No next line
            (nextLine:rest) ->
              let (before, _) = line e
              in e { edLinesBefore = (before, []) : edLinesBefore e
                   , line = nextLine
                   , edLinesAfter = rest
                   }
      | otherwise =
          -- Recurse forward, then back on return
          let e' = forwardAndDown (forward e)
          in if isInFrontZ (line e')
             then e'
             else back e'

-- | Move cursor to start of current line
moveCursorToLineStart :: SegmentEditor n -> SegmentEditor n
moveCursorToLineStart ed =
  let (before, after) = line ed
  in ed { line = ([], reverse before ++ after) }

-- | Move cursor to end of current line
moveCursorToLineEnd :: SegmentEditor n -> SegmentEditor n
moveCursorToLineEnd ed =
  let (before, after) = line ed
  in ed { line = (reverse after ++ before, []) }

-- | Move cursor to start of document
moveCursorToStart :: SegmentEditor n -> SegmentEditor n
moveCursorToStart ed =
  let (before, after) = line ed
      allLinesAfter = ([], reverse before ++ after) : edLinesAfter ed
      allLinesReversed = reverse (edLinesBefore ed) ++ allLinesAfter
  in case allLinesReversed of
      [] -> ed  -- Empty editor
      (firstLine:restLines) ->
        ed { edLinesBefore = []
           , line = firstLine
           , edLinesAfter = restLines
           }

-- | Move cursor to end of document
moveCursorToEnd :: SegmentEditor n -> SegmentEditor n
moveCursorToEnd ed =
  let (before, after) = line ed
      allLinesBefore = (reverse after ++ before, []) : edLinesBefore ed
      allLines = reverse (edLinesAfter ed) ++ allLinesBefore
  in case allLines of
      [] -> ed  -- Empty editor
      (lastLine:restLinesReversed) ->
        ed { edLinesBefore = restLinesReversed
           , line = lastLine
           , edLinesAfter = []
           }

-- | Move cursor one word to the left
moveWordLeft :: SegmentEditor n -> SegmentEditor n
moveWordLeft ed = goSpaces ed
  where
    goSpaces edCurrent =
      let (before, _after) = line edCurrent
      in case before of
        [] ->
          -- At start of current line, check if there are lines before
          case edLinesBefore edCurrent of
            [] -> edCurrent  -- At start of document
            _ -> goSpaces (back edCurrent)  -- Move to previous line
        (seg:_)
          | isSpaceSegment seg -> goSpaces (back edCurrent)
          | isWordSegment seg -> skipWord edCurrent
          | otherwise -> back edCurrent

    skipWord edCurrent =
      let (before, _after) = line edCurrent
      in case before of
        [] -> edCurrent
        (seg:_)
          | isWordSegment seg -> skipWord (back edCurrent)
          | otherwise -> edCurrent  -- Stop before non-word

-- | Move cursor one word to the right
moveWordRight :: SegmentEditor n -> SegmentEditor n
moveWordRight ed = goSpaces ed
  where
    goSpaces edCurrent =
      let (_before, after) = line edCurrent
      in case after of
        [] ->
          -- At end of current line, check if there are lines after
          case edLinesAfter edCurrent of
            [] -> edCurrent  -- At end of document
            _ -> goSpaces (forward edCurrent)  -- Move to next line
        (seg:_)
          | isSpaceSegment seg -> goSpaces (forward edCurrent)
          | isWordSegment seg -> skipWord edCurrent
          | otherwise -> forward edCurrent

    skipWord edCurrent =
      let (_before, after) = line edCurrent
      in case after of
        [] -> edCurrent
        (seg:_)
          | isWordSegment seg -> skipWord (forward edCurrent)
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
          : map (\ln -> txt (T.replicate (T.length prompt + 1) " ") <+> renderSegmentLine ln) restLines
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
renderSegmentLine (before, after) =
  let allSegs = reverse before ++ after
  in if null allSegs
     then txt " "  -- Empty line must render something to maintain height
     else hBox (map renderSegment allSegs)

-- | Render a single segment with appropriate styling
renderSegment :: InputSegment -> Widget n
renderSegment (CharSegment '\n') = txt ""  -- Newlines don't render (handled by vBox)
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
