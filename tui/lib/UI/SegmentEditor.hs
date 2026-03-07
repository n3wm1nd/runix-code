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
-- The editor is a 2D ZIPPER: a Zipper of lines (GapZippers of segments).
-- Each line is a Z.GapZipper (cursor between segments), and the editor
-- is a Zipper focusing on the current line.
--
-- Movement:
--   - Within line: forward/back on the Z.GapZipper (moves cursor)
--   - Between lines: forward/back on the outer Zipper (changes focused line)
--
-- Editing:
--   - Insert operations use insertAtGap on the current line
--   - Line breaks split the current GapZipper and create a new line
--   - All operations go through the Zippable typeclass when possible
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
import UI.Zipper (Zipper, GapZipper, Zippable(..))
import qualified UI.Zipper as Z

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

-- | A line is a gap zipper of segments (cursor between segments)
type SegmentLine a = GapZipper a

-- | The editor state - A 2D ZIPPER
--
-- ARCHITECTURE:
-- =============
-- The editor is a Zipper of SegmentLines (GapZippers).
-- - Outer Zipper: focuses on current line among all lines
-- - Inner GapZipper: cursor position within current line
--
-- This provides efficient local edits while maintaining 2D structure.
--
-- OPERATIONS:
-- ===========
-- - Within-line operations: work on the current GapZipper via Zippable
-- - Between-line operations: work on the outer Zipper via Zippable
-- - All raw zipper manipulation goes through UI.Zipper module
--
-- The type parameter 'a' is the segment type (typically InputSegment)
-- The 'n' parameter is for the Brick widget name
data SegmentEditor n a = SegmentEditor
  { edConfig :: EditorConfig n
  , edLines :: Zipper (SegmentLine a)   -- ^ Zipper of lines (current line is focused)
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Create an empty editor (single empty line with cursor at end)
emptyEditor :: EditorConfig n -> SegmentEditor n a
emptyEditor config = SegmentEditor
  { edConfig = config
  , edLines = Z.singletonZipper Z.emptyGap
  }

-- | Create an editor from plain text
editorFromText :: EditorConfig n -> Text -> SegmentEditor n InputSegment
editorFromText cfg txt =
  let textLines = T.lines txt
      -- Convert each line to a GapZipper with cursor at end
      segmentLines = map (\t -> Z.listToGap (map CharSegment (T.unpack t))) textLines
  in case segmentLines of
       [] -> emptyEditor cfg
       lines' -> case Z.listToZipper lines' of
         Nothing -> emptyEditor cfg  -- Shouldn't happen
         Just zipper -> SegmentEditor
           { edConfig = cfg
           , edLines = zipper
           }

-- | Create an editor from segment lines (cursor at start of first line)
editorFromSegments :: EditorConfig n -> [SegmentLine a] -> SegmentEditor n a
editorFromSegments config [] = emptyEditor config
editorFromSegments config lines' = case Z.listToZipper lines' of
  Nothing -> emptyEditor config  -- Shouldn't happen
  Just zipper -> SegmentEditor
    { edConfig = config
    , edLines = zipper
    }

--------------------------------------------------------------------------------
-- Reading content
--------------------------------------------------------------------------------

-- | Get all content as a single text string
getEditorContent :: SegmentEditor n InputSegment -> Text
getEditorContent ed =
  let allLines = toList (edLines ed)
      lineTexts = map segmentLineToText allLines
  in T.intercalate "\n" lineTexts

-- | Get content as list of lines (as Text)
getEditorLines :: SegmentEditor n InputSegment -> [Text]
getEditorLines ed =
  let allLines = toList (edLines ed)
  in map segmentLineToText allLines

-- | Get content as list of segment lines (preserves structure)
getEditorSegmentLines :: SegmentEditor n a -> [SegmentLine a]
getEditorSegmentLines ed = toList (edLines ed)

-- | Get cursor position (row, col) - 0-indexed
getCursorPos :: SegmentEditor n InputSegment -> (Int, Int)
getCursorPos ed =
  let allLines = toList (edLines ed)
      currentLine = Z.getCurrent (edLines ed)
      -- Row is how many lines come before current
      row = length $ takeWhile (/= currentLine) allLines
      -- Col is sum of segment lengths before cursor in current line
      col = sum (map segmentLength (Z.gapBefore currentLine))
  in (row, col)

-- | Check if editor is empty (single line with no segments)
isEmpty :: SegmentEditor n a -> Bool
isEmpty ed =
  let allLines = toList (edLines ed)
  in case allLines of
       [singleLine] -> null (toList singleLine)
       _ -> False

-- | Get the segment immediately after the cursor (if any)
getSegmentAtCursor :: SegmentEditor n a -> Maybe a
getSegmentAtCursor ed =
  let currentLine = Z.getCurrent (edLines ed)
  in Z.getAfterGap currentLine

-- | Get the segment immediately before the cursor (if any)
getSegmentBeforeCursor :: SegmentEditor n a -> Maybe a
getSegmentBeforeCursor ed =
  let currentLine = Z.getCurrent (edLines ed)
  in Z.getBeforeGap currentLine

--------------------------------------------------------------------------------
-- Zippable instance for SegmentEditor
--------------------------------------------------------------------------------

-- | SegmentEditor is a Zippable that navigates through segments
-- Forward/back move within current line; at boundaries, move between lines
instance Zippable (SegmentEditor n) where
  -- Move forward one segment (within line or to next line)
  forward ed =
    let currentLine = Z.getCurrent (edLines ed)
    in if not (atEnd currentLine)
       then
         -- Move forward within current line
         let newLine = Z.forward currentLine
         in ed { edLines = Z.updateCurrent newLine (edLines ed) }
       else
         -- At end of line, move to start of next line
         if atEnd (edLines ed)
         then ed  -- At end of document
         else
           let newLines = Z.forward (edLines ed)
               nextLine = Z.getCurrent newLines
               newLine = start nextLine
           in ed { edLines = Z.updateCurrent newLine newLines }

  -- Move backward one segment (within line or to previous line)
  back ed =
    let currentLine = Z.getCurrent (edLines ed)
    in if not (atStart currentLine)
       then
         -- Move back within current line
         let newLine = Z.back currentLine
         in ed { edLines = Z.updateCurrent newLine (edLines ed) }
       else
         -- At start of line, move to end of previous line
         if atStart (edLines ed)
         then ed  -- At start of document
         else
           let newLines = Z.back (edLines ed)
               prevLine = Z.getCurrent newLines
               newLine = end prevLine
           in ed { edLines = Z.updateCurrent newLine newLines }

  -- Move to start of document (first line, first position)
  start ed =
    let newLines = start (edLines ed)
        firstLine = Z.getCurrent newLines
        newLine = start firstLine
    in ed { edLines = Z.updateCurrent newLine newLines }

  -- Move to end of document (last line, last position)
  end ed =
    let newLines = end (edLines ed)
        lastLine = Z.getCurrent newLines
        newLine = end lastLine
    in ed { edLines = Z.updateCurrent newLine newLines }

  -- Check if at start of document
  atStart ed =
    let currentLine = Z.getCurrent (edLines ed)
    in atStart (edLines ed) && atStart currentLine

  -- Check if at end of document
  atEnd ed =
    let currentLine = Z.getCurrent (edLines ed)
    in atEnd (edLines ed) && atEnd currentLine

  -- Convert to list of all segments (across all lines)
  toList ed =
    let allLines = toList (edLines ed)
        allSegments = concatMap toList allLines
    in allSegments

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Convert a segment line to text
segmentLineToText :: SegmentLine InputSegment -> Text
segmentLineToText line' =
  let allSegs = toList line'
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
-- Event handling
--------------------------------------------------------------------------------

-- | Handle a Brick event, returning whether the editor wants to "submit"
-- Returns (shouldSubmit, newEditor)
handleEditorEvent :: BrickEvent n e -> SegmentEditor n InputSegment -> EventM n s (Bool, SegmentEditor n InputSegment)
handleEditorEvent (VtyEvent (EvKey KEnter [])) ed =
  case newlineMode (edConfig ed) of
    EnterSends ->
      -- Check if cursor is right after backslash
      case getSegmentBeforeCursor ed of
           Just (CharSegment '\\') ->
             -- Remove backslash and insert newline
             let ed' = deleteBackward ed
                 ed'' = breakLine ed'
             in return (False, ed'')
           _ -> return (True, ed)  -- Normal enter submits
    EnterNewline -> return (False, breakLine ed)
    BackslashEscape ->
      -- Check if cursor is right after backslash
      case getSegmentBeforeCursor ed of
           Just (CharSegment '\\') ->
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
  let currentLine = Z.getCurrent (edLines ed)
      newLine = Z.GapZipper (Z.gapBefore currentLine) []
  in return (False, ed { edLines = Z.updateCurrent newLine (edLines ed) })

handleEditorEvent (VtyEvent (EvKey (KChar 'u') [MCtrl])) ed =
  -- Kill to start of line
  let currentLine = Z.getCurrent (edLines ed)
      newLine = Z.GapZipper [] (Z.gapAfter currentLine)
  in return (False, ed { edLines = Z.updateCurrent newLine (edLines ed) })

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
insertChar :: Char -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertChar c ed =
  let currentLine = Z.getCurrent (edLines ed)
      newLine = Z.insertAtGap (CharSegment c) currentLine
  in ed { edLines = Z.updateCurrent newLine (edLines ed) }

-- | Insert text at cursor (as CharSegments)
insertText :: Text -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertText textToInsert ed =
  let chars = T.unpack textToInsert
      segments = map CharSegment chars
      currentLine = Z.getCurrent (edLines ed)
      -- Insert segments in order (foldr to maintain order)
      newLine = foldr Z.insertAtGap currentLine segments
  in ed { edLines = Z.updateCurrent newLine (edLines ed) }

-- | Insert a segment at cursor
insertSegment :: InputSegment -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertSegment seg ed =
  let currentLine = Z.getCurrent (edLines ed)
      newLine = Z.insertAtGap seg currentLine
  in ed { edLines = Z.updateCurrent newLine (edLines ed) }

-- | Insert a file reference at cursor
insertFileRef :: [FilePath] -> Text -> RefState -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertFileRef paths query state ed =
  insertSegment (FileRefSegment
    { segRefPaths = paths
    , segRefQuery = query
    , segRefState = state
    }) ed

-- | Replace the segment after the cursor with a new one
-- Useful for updating file refs after tab completion
replaceSegmentAtCursor :: InputSegment -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
replaceSegmentAtCursor newSeg ed =
  let currentLine = Z.getCurrent (edLines ed)
  in case Z.getAfterGap currentLine of
       Nothing -> ed  -- Nothing to replace
       Just _ ->
         -- Delete after, then insert
         let line' = Z.deleteAfterGap currentLine
             line'' = Z.insertAtGap newSeg line'
             line''' = forward line''  -- Move cursor past new segment
         in ed { edLines = Z.updateCurrent line''' (edLines ed) }

-- | Rotate file reference alternatives (looks at segment BEFORE cursor)
rotateFileRefAtCursor :: SegmentEditor n InputSegment -> SegmentEditor n InputSegment
rotateFileRefAtCursor ed =
  let currentLine = Z.getCurrent (edLines ed)
  in case Z.getBeforeGap currentLine of
       Just (FileRefSegment {segRefPaths = (current:rest), segRefQuery = query, segRefState = state}) ->
         case rest of
           [] -> ed  -- Only one path, nothing to rotate
           (next:others) ->
             let rotated = FileRefSegment
                   { segRefPaths = next : (others ++ [current])
                   , segRefQuery = query
                   , segRefState = state
                   }
                 -- Delete old ref, insert rotated
                 line' = Z.deleteBeforeGap currentLine
                 line'' = Z.insertAtGap rotated line'
             in ed { edLines = Z.updateCurrent line'' (edLines ed) }
       _ -> ed  -- Not a file ref or empty paths

-- | Clear the entire editor
clearEditor :: SegmentEditor n a -> SegmentEditor n a
clearEditor ed = emptyEditor (edConfig ed)

-- | Update the newline mode
setNewlineMode :: NewlineMode -> SegmentEditor n a -> SegmentEditor n a
setNewlineMode mode ed =
  let cfg = edConfig ed
  in ed { edConfig = cfg { newlineMode = mode } }

-- | Get the word before cursor (for completion)
-- Returns the word text and the number of segments it spans
getWordBeforeCursor :: SegmentEditor n InputSegment -> Maybe (Text, Int)
getWordBeforeCursor ed =
  let currentLine = Z.getCurrent (edLines ed)
      before = Z.gapBefore currentLine  -- Already reversed
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
deleteNSegments :: Int -> SegmentEditor n a -> SegmentEditor n a
deleteNSegments 0 ed = ed
deleteNSegments n ed = deleteNSegments (n - 1) (deleteBackward ed)

-- | Delete backward (delete segment before cursor)
deleteBackward :: SegmentEditor n a -> SegmentEditor n a
deleteBackward ed =
  let currentLine = Z.getCurrent (edLines ed)
  in if not (atStart currentLine)
     then
       -- Delete within current line
       let newLine = Z.deleteBeforeGap currentLine
       in ed { edLines = Z.updateCurrent newLine (edLines ed) }
     else
       -- At start of line, join with previous line
       if atStart (edLines ed)
       then ed  -- At start of document
       else
         -- Move to previous line, append current line's after to it
         let prevLines = back (edLines ed)
             prevLine = Z.getCurrent prevLines
             currentAfter = Z.gapAfter currentLine
             -- Join: append current line's after segments to previous line
             joinedLine = foldr (\seg ln -> forward (Z.insertAtGap seg ln)) prevLine (reverse currentAfter)
             -- Remove current line by updating with joined line and removing it
             newLines = Z.updateCurrent joinedLine prevLines
         in ed { edLines = newLines }

-- | Delete forward (delete segment after cursor)
deleteForward :: SegmentEditor n a -> SegmentEditor n a
deleteForward ed =
  let currentLine = Z.getCurrent (edLines ed)
  in if not (atEnd currentLine)
     then
       -- Delete within current line
       let newLine = Z.deleteAfterGap currentLine
       in ed { edLines = Z.updateCurrent newLine (edLines ed) }
     else
       -- At end of line, join with next line
       if atEnd (edLines ed)
       then ed  -- At end of document
       else
         -- Append next line to current line
         let nextLines = forward (edLines ed)
             nextLine = Z.getCurrent nextLines
             nextSegments = toList nextLine
             -- Append all segments from next line
             joinedLine = foldr (\seg ln -> forward (Z.insertAtGap seg ln)) currentLine (reverse nextSegments)
             -- Update current line and remove next line
             newLines = Z.updateCurrent joinedLine (edLines ed)
         in ed { edLines = newLines }

-- | Delete word backward
deleteWordBackward :: SegmentEditor n InputSegment -> SegmentEditor n InputSegment
deleteWordBackward ed = goSpaces ed
  where
    goSpaces edCurrent =
      let currentLine = Z.getCurrent (edLines edCurrent)
      in case Z.getBeforeGap currentLine of
        Nothing ->
          -- At start of current line, check if at start of document
          if atStart edCurrent
          then edCurrent  -- Nothing to delete
          else goSpaces (deleteBackward edCurrent)  -- Try previous line
        Just seg
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
      let currentLine = Z.getCurrent (edLines edCurrent)
      in case Z.getBeforeGap currentLine of
        Nothing -> edCurrent
        Just seg
          | isWordSegment seg -> deleteWord (deleteBackward edCurrent)
          | otherwise -> edCurrent  -- Stop at non-word

-- | Break line at cursor (insert newline)
breakLine :: SegmentEditor n a -> SegmentEditor n a
breakLine ed =
  case lineLimit (edConfig ed) of
    Just lim ->
      let totalLines = length (toList (edLines ed))
      in if totalLines >= lim
         then ed  -- Don't exceed line limit
         else doBreak ed
    Nothing -> doBreak ed
  where
    doBreak ed' =
      -- Split current line at cursor
      let currentLine = Z.getCurrent (edLines ed')
          afterSegments = Z.gapAfter currentLine
          -- Complete current line (move cursor to end)
          completedLine = end currentLine
          -- Create new line with after segments (cursor at start)
          newLine = Z.listToGap afterSegments
          -- Insert new line after current
          newLines = Z.updateCurrent completedLine (edLines ed')
          newLines' = Z.appendItemAndFocus newLine newLines
      in ed' { edLines = newLines' }

--------------------------------------------------------------------------------
-- Cursor movement
--------------------------------------------------------------------------------

-- | Move cursor left (by one segment within current line)
moveCursorLeft :: SegmentEditor n a -> SegmentEditor n a
moveCursorLeft ed =
  let currentLine = Z.getCurrent (edLines ed)
      newLine = back currentLine
  in ed { edLines = Z.updateCurrent newLine (edLines ed) }

-- | Move cursor right (by one segment within current line)
moveCursorRight :: SegmentEditor n a -> SegmentEditor n a
moveCursorRight ed =
  let currentLine = Z.getCurrent (edLines ed)
      newLine = forward currentLine
  in ed { edLines = Z.updateCurrent newLine (edLines ed) }

-- | Move cursor up one line (maintaining column position)
moveCursorUp :: SegmentEditor n a -> SegmentEditor n a
moveCursorUp ed = rewindAndUp ed
  where
    rewindAndUp e
      | atStart (Z.getCurrent (edLines e)) =
          -- At start of line, move to previous line
          if atStart (edLines e)
          then e  -- No previous line
          else
            let currentLine = Z.getCurrent (edLines e)
                afterSegments = Z.gapAfter currentLine
                prevLines = back (edLines e)
                -- Keep after segments for potential re-positioning
                newLines = prevLines  -- TODO: reposition cursor
            in e { edLines = newLines }
      | otherwise =
          -- Recurse back within line, then forward on return
          let e' = rewindAndUp (moveCursorLeft e)
              currentLine = Z.getCurrent (edLines e')
          in if atEnd currentLine
             then e'
             else moveCursorRight e'

-- | Move cursor down one line (maintaining column position)
moveCursorDown :: SegmentEditor n a -> SegmentEditor n a
moveCursorDown ed = forwardAndDown ed
  where
    forwardAndDown e
      | atEnd (Z.getCurrent (edLines e)) =
          -- At end of line, move to next line
          if atEnd (edLines e)
          then e  -- No next line
          else
            let currentLine = Z.getCurrent (edLines e)
                beforeSegments = Z.gapBefore currentLine
                nextLines = forward (edLines e)
                -- Keep before segments for potential re-positioning
                newLines = nextLines  -- TODO: reposition cursor
            in e { edLines = newLines }
      | otherwise =
          -- Recurse forward within line, then back on return
          let e' = forwardAndDown (moveCursorRight e)
              currentLine = Z.getCurrent (edLines e')
          in if atStart currentLine
             then e'
             else moveCursorLeft e'

-- | Move cursor to start of current line
moveCursorToLineStart :: SegmentEditor n a -> SegmentEditor n a
moveCursorToLineStart ed =
  let currentLine = Z.getCurrent (edLines ed)
      newLine = start currentLine
  in ed { edLines = Z.updateCurrent newLine (edLines ed) }

-- | Move cursor to end of current line
moveCursorToLineEnd :: SegmentEditor n a -> SegmentEditor n a
moveCursorToLineEnd ed =
  let currentLine = Z.getCurrent (edLines ed)
      newLine = end currentLine
  in ed { edLines = Z.updateCurrent newLine (edLines ed) }

-- | Move cursor to start of document
moveCursorToStart :: SegmentEditor n a -> SegmentEditor n a
moveCursorToStart ed =
  let newLines = start (edLines ed)
      currentLine = Z.getCurrent newLines
      newLine = start currentLine
  in ed { edLines = Z.updateCurrent newLine newLines }

-- | Move cursor to end of document
moveCursorToEnd :: SegmentEditor n a -> SegmentEditor n a
moveCursorToEnd ed =
  let newLines = end (edLines ed)
      currentLine = Z.getCurrent newLines
      newLine = end currentLine
  in ed { edLines = Z.updateCurrent newLine newLines }

-- | Move cursor one word to the left
moveWordLeft :: SegmentEditor n InputSegment -> SegmentEditor n InputSegment
moveWordLeft ed = goSpaces ed
  where
    goSpaces edCurrent =
      let currentLine = Z.getCurrent (edLines edCurrent)
      in case Z.getBeforeGap currentLine of
        Nothing ->
          -- At start of current line, check if at start of document
          if atStart edCurrent
          then edCurrent  -- At start of document
          else goSpaces (moveCursorLeft edCurrent)  -- Move to previous line
        Just seg
          | isSpaceSegment seg -> goSpaces (moveCursorLeft edCurrent)
          | isWordSegment seg -> skipWord edCurrent
          | otherwise -> moveCursorLeft edCurrent

    skipWord edCurrent =
      let currentLine = Z.getCurrent (edLines edCurrent)
      in case Z.getBeforeGap currentLine of
        Nothing -> edCurrent
        Just seg
          | isWordSegment seg -> skipWord (moveCursorLeft edCurrent)
          | otherwise -> edCurrent  -- Stop before non-word

-- | Move cursor one word to the right
moveWordRight :: SegmentEditor n InputSegment -> SegmentEditor n InputSegment
moveWordRight ed = goSpaces ed
  where
    goSpaces edCurrent =
      let currentLine = Z.getCurrent (edLines edCurrent)
      in case Z.getAfterGap currentLine of
        Nothing ->
          -- At end of current line, check if at end of document
          if atEnd edCurrent
          then edCurrent  -- At end of document
          else goSpaces (moveCursorRight edCurrent)  -- Move to next line
        Just seg
          | isSpaceSegment seg -> goSpaces (moveCursorRight edCurrent)
          | isWordSegment seg -> skipWord edCurrent
          | otherwise -> moveCursorRight edCurrent

    skipWord edCurrent =
      let currentLine = Z.getCurrent (edLines edCurrent)
      in case Z.getAfterGap currentLine of
        Nothing -> edCurrent
        Just seg
          | isWordSegment seg -> skipWord (moveCursorRight edCurrent)
          | otherwise -> edCurrent  -- Stop at non-word

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render the editor with optional focus
-- This version renders segments with special styling
renderEditor :: (Ord n, Show n)
             => Bool  -- ^ Has focus
             -> SegmentEditor n InputSegment
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
                       -> SegmentEditor n InputSegment
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
renderSegmentLine :: SegmentLine InputSegment -> Widget n
renderSegmentLine line' =
  let allSegs = toList line'
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
