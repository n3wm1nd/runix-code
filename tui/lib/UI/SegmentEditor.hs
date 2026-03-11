{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

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
    SegmentEditor(..)
  , EditorConfig(..)
  , NewlineMode(..)
  , InputSegment(..)
  , GapZipper
  , RefState(..)
  , HasLinebreak(..)
    -- * Construction
  , emptyEditor
  , editorFromText
  , editorFromSegments
    -- * Reading content
  , getEditorContent
  , getEditorLines
  , getCursorPos
  , isEmpty
  , getSegmentAtCursor
  , getSegmentBeforeCursor
    -- * Event handling
  , handleEditorEvent
    -- * Editing operations
  , insertChar
  , typeChar
  , insertText
  , insertSegment
  , insertFileRef
  , delBackward
  , delForward
  , delWordBackward
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
    -- * Word wrapping
  , rewrapEditor
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
import UI.Zipper (Zipper, GapZipper, Zippable(..))
import qualified UI.Zipper as Z
import UI.SegmentEditor.Types
  ( InputSegment(..)
  , RefState(..)
  , HasLinebreak(..)
  , editorTxt
  , segmentLength
  , segmentToText
  , renderLineSegments
  , renderLineLength
  , pasteDisplayText
  , isWordSegment
  , isSpaceSegment
  )
import qualified UI.SegmentEditor.WordWrap as WW

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

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

-- | The editor state - A 2D structure with cursor
--
-- ARCHITECTURE:
-- =============
-- The editor stores lines in three parts:
-- - Lines above current: list of segment lists (reversed: head is closest to current)
-- - Current line: GapZipper positioning the cursor
-- - Lines below current: list of segment lists (forward: head is closest to current)
--
-- INVARIANTS:
-- ===========
-- 1. Lines in edLinesAbove have segments in reverse order (head is last segment)
-- 2. Lines in edLinesBelow have segments in forward order (head is first segment)
-- 3. The list of lines above is in reverse order (head is line just above current)
-- 4. The list of lines below is in forward order (head is line just below current)
-- 5. Summary: heads always point toward current cursor position at all levels
--
-- This provides efficient local edits while maintaining 2D structure.
--
-- The type parameter 'a' is the segment type (typically InputSegment)
-- The 'n' parameter is for the Brick widget name
data SegmentEditor n a = SegmentEditor
  { edConfig :: EditorConfig n
  , edLinesAbove :: [[a]]          -- ^ Lines above (reversed segments, reversed list)
  , edCurrentLine :: GapZipper a   -- ^ Current line with cursor
  , edLinesBelow :: [[a]]          -- ^ Lines below (forward segments, forward list)
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

-- | Create an empty editor (single empty line with cursor at end)
emptyEditor :: EditorConfig n -> SegmentEditor n a
emptyEditor config = SegmentEditor
  { edConfig = config
  , edLinesAbove = []
  , edCurrentLine = Z.emptyGap
  , edLinesBelow = []
  }

-- | Create an editor from plain text
editorFromText :: EditorConfig n -> Text -> SegmentEditor n InputSegment
editorFromText cfg text =
  -- Just use insertText which handles newlines correctly via typeChar
  insertText text (emptyEditor cfg)

-- | Create an editor from segment lines (cursor at start of first line)
editorFromSegments :: EditorConfig n -> [Z.GapZipper a] -> SegmentEditor n a
editorFromSegments config [] = emptyEditor config
editorFromSegments config (firstLine:restLines) =
  let linesBelow = map toList restLines  -- Just convert to lists
  in SegmentEditor
    { edConfig = config
    , edLinesAbove = []
    , edCurrentLine = firstLine
    , edLinesBelow = linesBelow
    }

--------------------------------------------------------------------------------
-- Reading content
--------------------------------------------------------------------------------

-- | Get all content as a single text string
getEditorContent :: SegmentEditor n InputSegment -> Text
getEditorContent ed =
  let aboveTexts = map (segmentLineToText . listToGap . reverse) (reverse (edLinesAbove ed))
      currentText = segmentLineToText (edCurrentLine ed)
      belowTexts = map (segmentLineToText . listToGap) (edLinesBelow ed)
      allTexts = aboveTexts ++ [currentText] ++ belowTexts
  in T.concat allTexts  -- Just concatenate - newlines come from CharSegment '\n'
  where
    listToGap segs = Z.GapZipper [] segs

-- | Get content as list of lines (as Text)
getEditorLines :: SegmentEditor n InputSegment -> [Text]
getEditorLines ed =
  let aboveTexts = map (segmentLineToText . listToGap . reverse) (reverse (edLinesAbove ed))
      currentText = segmentLineToText (edCurrentLine ed)
      belowTexts = map (segmentLineToText . listToGap) (edLinesBelow ed)
  in aboveTexts ++ [currentText] ++ belowTexts
  where
    listToGap segs = Z.GapZipper [] segs


-- | Get cursor position (row, col) - 0-indexed
getCursorPos :: SegmentEditor n InputSegment -> (Int, Int)
getCursorPos ed =
  let row = length (edLinesAbove ed)
      col = sum (map segmentLength (Z.gapBefore (edCurrentLine ed)))
  in (row, col)

-- | Check if editor is empty (single line with no segments)
isEmpty :: SegmentEditor n a -> Bool
isEmpty ed =
  null (edLinesAbove ed) && null (edLinesBelow ed) && null (toList (edCurrentLine ed))

-- | Get the segment immediately after the cursor (if any)
getSegmentAtCursor :: SegmentEditor n a -> Maybe a
getSegmentAtCursor ed =
  let currentLine = edCurrentLine ed
  in Z.getAfterGap currentLine

-- | Get the segment immediately before the cursor (if any)
getSegmentBeforeCursor :: SegmentEditor n a -> Maybe a
getSegmentBeforeCursor ed =
  let currentLine = edCurrentLine ed
  in Z.getBeforeGap currentLine

--------------------------------------------------------------------------------
-- Zippable instance for SegmentEditor
--------------------------------------------------------------------------------

-- | SegmentEditor is a Zippable that navigates through segments
-- Forward/back move within current line; at boundaries, move between lines
-- Invariant: edLinesAbove has reversed segments, edLinesBelow has forward segments
instance Zippable (SegmentEditor n) where
  -- Move forward one segment (right, or to next line if at end of line)
  -- ALWAYS move forward first, THEN check if we need to transition
  forward ed =
    let -- Move forward within current line (if already at end, this is a no-op)
        ed' = ed { edCurrentLine = Z.forward (edCurrentLine ed) }
    in -- Now check: are we at end of line with lines below? If so, transition
       if atEnd (edCurrentLine ed') && not (null (edLinesBelow ed'))
       then case edLinesBelow ed' of
              (nextLine : rest) ->
                ed' { edLinesAbove = Z.gapBefore (edCurrentLine ed') : edLinesAbove ed'
                    , edCurrentLine = Z.GapZipper [] nextLine
                    , edLinesBelow = rest }
              [] -> error "Invariant violated"
       else ed'

  -- Move backward one segment (left, or to previous line if at start of line)
  -- First check if we need to transition, then move back
  back ed =
    -- If at start with lines above, transition to end of previous line first
    let ed' = if atStart (edCurrentLine ed) && not (null (edLinesAbove ed))
              then case edLinesAbove ed of
                     (prevLine : rest) ->
                       ed { edLinesAbove = rest
                          , edCurrentLine = Z.GapZipper prevLine []
                          , edLinesBelow = Z.gapAfter (edCurrentLine ed) : edLinesBelow ed }
                     [] -> error "Invariant violated"
              else ed
    in -- Now move back one position
       if atStart (edCurrentLine ed')
       then ed'  -- Can't move back
       else ed' { edCurrentLine = Z.back (edCurrentLine ed') }

  -- Move to start of document (first line, first position)
  start ed = if atStart ed then ed else start (back ed)

  -- Move to end of document (last line, last position)
  end ed = if atEnd ed then ed else end (forward ed)

  -- Check if at start of document (first line, start of line)
  atStart ed = null (edLinesAbove ed) && atStart (edCurrentLine ed)

  -- Check if at end of document (last line, end of line)
  atEnd ed = null (edLinesBelow ed) && atEnd (edCurrentLine ed)

  -- Convert to list of all segments (across all lines)
  toList ed =
    let linesAboveSegs = concatMap reverse (reverse (edLinesAbove ed))
        currentSegs = toList (edCurrentLine ed)
        linesBelowSegs = concat (edLinesBelow ed)
    in linesAboveSegs ++ currentSegs ++ linesBelowSegs

  -- Insert segment before cursor
  insertBackward seg ed =
    ed { edCurrentLine = insertBackward seg (edCurrentLine ed) }

  -- Insert segment after cursor
  insertForward seg ed =
    ed { edCurrentLine = insertForward seg (edCurrentLine ed) }

  -- Delete segment before cursor (just navigate and delete, ignore line structure)
  deleteBackward ed =
    case deleteBackward (edCurrentLine ed) of
      Just newLine ->
        -- Deleted within current line
        Just (ed { edCurrentLine = newLine })
      Nothing ->
        -- At start of current line, move back to previous line and delete there
        if null (edLinesAbove ed)
        then Nothing  -- At start of document
        else deleteBackward (back ed)  -- Move back and try again

  -- Delete segment after cursor (just navigate and delete, ignore line structure)
  deleteForward ed =
    case deleteForward (edCurrentLine ed) of
      Just newLine ->
        -- Deleted within current line
        Just (ed { edCurrentLine = newLine })
      Nothing ->
        -- At end of current line, move forward to next line and delete there
        if null (edLinesBelow ed)
        then Nothing  -- At end of document
        else deleteForward (forward ed)  -- Move forward and try again

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Convert a segment line to text
segmentLineToText :: GapZipper InputSegment -> Text
segmentLineToText line' =
  let allSegs = toList line'
  in mconcat (map segmentToText allSegs)

-- Helper functions are now imported from UI.SegmentEditor.Types

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
             let ed' = delBackward ed
                 ed'' = typeChar '\n' ed'
             in return (False, ed'')
           _ -> return (True, ed)  -- Normal enter submits
    EnterNewline -> return (False, typeChar '\n' ed)
    BackslashEscape ->
      -- Check if cursor is right after backslash
      case getSegmentBeforeCursor ed of
           Just (CharSegment '\\') ->
             -- Remove backslash and insert newline
             let ed' = delBackward ed
                 ed'' = typeChar '\n' ed'
             in return (False, ed'')
           _ -> return (True, ed)  -- Normal enter submits

handleEditorEvent (VtyEvent (EvKey KEnter [MShift])) ed =
  -- Shift+Enter always inserts newline
  return (False, typeChar '\n' ed)

handleEditorEvent (VtyEvent (EvKey (KChar c) [])) ed
  | c /= '\t' = return (False, insertChar c ed)

handleEditorEvent (VtyEvent (EvKey KBS [])) ed =
  return (False, delBackward ed)

handleEditorEvent (VtyEvent (EvKey KDel [])) ed =
  return (False, delForward ed)

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
  return (False, delWordBackward ed)

handleEditorEvent (VtyEvent (EvKey (KChar 'k') [MCtrl])) ed =
  -- Kill to end of line
  let currentLine = edCurrentLine ed
      newLine = Z.GapZipper (Z.gapBefore currentLine) []
  in return (False, ed { edCurrentLine = newLine })

handleEditorEvent (VtyEvent (EvKey (KChar 'u') [MCtrl])) ed =
  -- Kill to start of line
  let currentLine = edCurrentLine ed
      newLine = Z.GapZipper [] (Z.gapAfter currentLine)
  in return (False, ed { edCurrentLine = newLine })

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
insertChar c ed = insertBackward (CharSegment c) ed

-- | Type a character - handles linebreaks by calling breakLine after inserting
typeChar :: Char -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
typeChar c ed =
  if isHardBreak (CharSegment c)
  then breakLine (insertChar c ed)  -- Insert newline, then break line
  else insertChar c ed

-- | Insert text at cursor (as if typing each character)
insertText :: Text -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertText text ed = foldl (\e c -> typeChar c e) ed (T.unpack text)

-- | Insert a segment at cursor
insertSegment :: InputSegment -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertSegment seg ed = insertBackward seg ed

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
  let currentLine = edCurrentLine ed
  in case Z.getAfterGap currentLine of
       Nothing -> ed  -- Nothing to replace
       Just _ ->
         -- Delete after, then insert
         let line' = Z.deleteAfterGap currentLine
             line'' = Z.insertAtGap newSeg line'
         in ed { edCurrentLine = line'' }

-- | Rotate file reference alternatives (looks at segment BEFORE cursor)
rotateFileRefAtCursor :: SegmentEditor n InputSegment -> SegmentEditor n InputSegment
rotateFileRefAtCursor ed =
  let currentLine = edCurrentLine ed
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
             in ed { edCurrentLine = line'' }
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
  let currentLine = edCurrentLine ed
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
deleteNSegments n ed = deleteNSegments (n - 1) (delBackward ed)

-- | Delete backward (delete segment before cursor, join lines if at line start)
delBackward :: SegmentEditor n a -> SegmentEditor n a
delBackward ed =
  let currentLine = edCurrentLine ed
  in if atStart currentLine
     then
       -- At start of line - join with previous line
       case edLinesAbove ed of
         [] -> ed  -- At start of document (no previous line)
         (prevLine : rest) ->
           -- prevLine is in reverse order (last segment is head)
           -- The last char should be '\n', drop it and join lines
           case prevLine of
             [] -> error "Empty line in edLinesAbove"
             (_lastChar : prevRest) ->
               -- Join: prevRest (reversed) + current line after cursor
               let joinedLine = Z.GapZipper prevRest (Z.gapAfter currentLine)
               in ed { edLinesAbove = rest, edCurrentLine = joinedLine }
     else
       -- Not at line start, just delete the segment before cursor
       case deleteBackward ed of
         Just ed' -> ed'
         Nothing -> ed

-- | Delete forward (delete segment after cursor, join lines if at line end)
delForward :: SegmentEditor n a -> SegmentEditor n a
delForward ed =
  let currentLine = edCurrentLine ed
  in if atEnd currentLine
     then
       -- At end of line - join with next line
       case edLinesBelow ed of
         [] -> ed  -- At end of document (no next line)
         (nextLine : rest) ->
           -- nextLine is in forward order
           -- Join: current line's before + nextLine
           let joinedLine = Z.GapZipper (Z.gapBefore currentLine) nextLine
           in ed { edLinesBelow = rest, edCurrentLine = joinedLine }
     else
       -- Not at line end, just delete the segment
       case deleteForward ed of
         Just ed' -> ed'
         Nothing -> ed

-- | Delete word backward
delWordBackward :: SegmentEditor n InputSegment -> SegmentEditor n InputSegment
delWordBackward ed = goSpaces ed
  where
    goSpaces edCurrent =
      let currentLine = edCurrentLine edCurrent
      in case Z.getBeforeGap currentLine of
        Nothing ->
          -- At start of current line, check if at start of document
          if atStart edCurrent
          then edCurrent  -- Nothing to delete
          else goSpaces (delBackward edCurrent)  -- Try previous line
        Just seg
          | isSpaceSegment seg ->
              -- Delete spaces, then continue to word
              goSpaces (delBackward edCurrent)
          | isWordSegment seg ->
              -- Delete word segments
              deleteWord edCurrent
          | otherwise ->
              -- Delete single non-word segment (like file ref)
              delBackward edCurrent

    deleteWord edCurrent =
      let currentLine = edCurrentLine edCurrent
      in case Z.getBeforeGap currentLine of
        Nothing -> edCurrent
        Just seg
          | isWordSegment seg -> deleteWord (delBackward edCurrent)
          | otherwise -> edCurrent  -- Stop at non-word

-- | Break line at cursor (insert newline)
breakLine :: SegmentEditor n a -> SegmentEditor n a
breakLine ed =
  case lineLimit (edConfig ed) of
    Just lim ->
      let totalLines = 1 + length (edLinesAbove ed) + length (edLinesBelow ed)
      in if totalLines >= lim
         then ed  -- Don't exceed line limit
         else doBreak ed
    Nothing -> doBreak ed
  where
    doBreak ed' =
      -- Split current line at cursor
      let currentLine = edCurrentLine ed'
          afterSegments = Z.gapAfter currentLine  -- normal order
          beforeSegments = Z.gapBefore currentLine  -- reversed order: [3,2,1] means segments 1,2,3
          -- New line gets after segments (at START)
          newLine = Z.listToGap afterSegments
      in case beforeSegments of
        [] ->
          -- Current line before cursor is empty - nothing to move to edLinesAbove
          -- Just move to new line (afterSegments becomes current line)
          ed' { edCurrentLine = newLine }
        _ ->
          -- Move completed line (beforeSegments) to edLinesAbove
          -- beforeSegments is already in reversed order, perfect for edLinesAbove
          ed' { edLinesAbove = beforeSegments : edLinesAbove ed'
              , edCurrentLine = newLine
              }

--------------------------------------------------------------------------------
-- Cursor movement
--------------------------------------------------------------------------------

-- | Move cursor left (by one segment, crossing line boundaries)
moveCursorLeft :: HasLinebreak a => SegmentEditor n a -> SegmentEditor n a
moveCursorLeft ed =
  let ed' = back ed
      -- Check if we moved onto a hard linebreak - if so, skip over it
      currentLine = edCurrentLine ed'
  in case Z.gapBefore currentLine of
       (seg:_) | isHardBreak seg -> back ed'  -- Skip over the linebreak
       _ -> ed'

-- | Move cursor right (by one segment, crossing line boundaries)
moveCursorRight :: HasLinebreak a => SegmentEditor n a -> SegmentEditor n a
moveCursorRight ed =
  -- Check if next segment is a hard linebreak - if so, skip over it
  let currentLine = edCurrentLine ed
  in case Z.gapAfter currentLine of
       (seg:_) | isHardBreak seg ->
         -- Skip over the linebreak
         let ed' = forward ed
         in forward ed'
       _ -> forward ed

-- | Move cursor up one line (maintaining column position)
-- TODO: Preserve column position when moving between lines
-- For now, just move to end of previous line
moveCursorUp :: HasLinebreak a => SegmentEditor n a -> SegmentEditor n a
moveCursorUp ed =
  case edLinesAbove ed of
    [] -> ed  -- No previous line
    _ ->
      if atStart (edCurrentLine ed)
      then
        -- At start of line - move back to previous line (which is at END)
        -- Then rewind to START so we can move forward to restore column
        let ed' = back ed
            -- Rewind current line to START
            rewindToStart e =
              if atStart (edCurrentLine e)
              then e
              else rewindToStart (e { edCurrentLine = Z.back (edCurrentLine e) })
        in rewindToStart ed'
      else
        -- Not at start - move back one segment, recurse, then move forward
        let currentLine = edCurrentLine ed
            ed' = ed { edCurrentLine = Z.back currentLine }
            ed'' = moveCursorUp ed'
            -- Move forward on current line if not at end or at a hard linebreak
            newLine = edCurrentLine ed''
        in if atEnd newLine
           then ed''  -- At end of line, stop here
           else
             -- Check if next segment is a hard linebreak - if so, stop here
             case Z.gapAfter newLine of
               (seg:_) | isHardBreak seg -> ed''  -- Stop before hard linebreak
               _ -> ed'' { edCurrentLine = Z.forward newLine }

-- | Move cursor down one line (maintaining column position)
moveCursorDown :: HasLinebreak a => SegmentEditor n a -> SegmentEditor n a
moveCursorDown ed =
  case edLinesBelow ed of
    [] -> ed  -- No next line
    _ ->
      if atStart (edCurrentLine ed)
      then
        -- At start of line - move current line to end, then forward to next line
        let moveToEndOfLine e =
              if atEnd (edCurrentLine e)
              then forward e  -- At end, move to next line
              else moveToEndOfLine (e { edCurrentLine = Z.forward (edCurrentLine e) })
        in moveToEndOfLine ed
      else
        -- Not at start - move back one segment, recurse, then move forward
        let currentLine = edCurrentLine ed
            ed' = ed { edCurrentLine = Z.back currentLine }
            ed'' = moveCursorDown ed'
            -- Move forward on current line if not at end or at a hard linebreak
            newLine = edCurrentLine ed''
        in if atEnd newLine
           then ed''  -- At end of line, stop here
           else
             -- Check if next segment is a hard linebreak - if so, stop here
             case Z.gapAfter newLine of
               (seg:_) | isHardBreak seg -> ed''  -- Stop before hard linebreak
               _ -> ed'' { edCurrentLine = Z.forward newLine }

-- | Move cursor to start of current line
moveCursorToLineStart :: SegmentEditor n a -> SegmentEditor n a
moveCursorToLineStart ed =
  let currentLine = edCurrentLine ed
      newLine = start currentLine
  in ed { edCurrentLine = newLine }

-- | Move cursor to end of current line
moveCursorToLineEnd :: SegmentEditor n a -> SegmentEditor n a
moveCursorToLineEnd ed =
  let currentLine = edCurrentLine ed
      newLine = end currentLine
  in ed { edCurrentLine = newLine }

-- | Move cursor to start of document
moveCursorToStart :: SegmentEditor n a -> SegmentEditor n a
moveCursorToStart = start

-- | Move cursor to end of document
moveCursorToEnd :: SegmentEditor n a -> SegmentEditor n a
moveCursorToEnd = end

-- | Move cursor one word to the left
moveWordLeft :: SegmentEditor n InputSegment -> SegmentEditor n InputSegment
moveWordLeft ed = goSpaces ed
  where
    goSpaces edCurrent =
      let currentLine = edCurrentLine edCurrent
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
      let currentLine = edCurrentLine edCurrent
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
      let currentLine = edCurrentLine edCurrent
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
      let currentLine = edCurrentLine edCurrent
      in case Z.getAfterGap currentLine of
        Nothing -> edCurrent
        Just seg
          | isWordSegment seg -> skipWord (moveCursorRight edCurrent)
          | otherwise -> edCurrent  -- Stop at non-word

--------------------------------------------------------------------------------
-- Word Wrapping
--------------------------------------------------------------------------------

-- | Rewrap the entire editor at the given width, preserving cursor position
-- Recursively backs to start, rewraps, then forwards back to cursor position
rewrapEditor :: Int -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
rewrapEditor width ed
  | atStart ed = doRewrap ed
  | otherwise = forward (rewrapEditor width (back ed))
  where
    doRewrap e =
      let -- Get all segments as one flat list
          allSegments = toList e
          -- Rewrap at the given width
          wrappedLines = WW.wrapLine width allSegments
      in case wrappedLines of
           [] -> e { edLinesAbove = [], edCurrentLine = Z.emptyGap, edLinesBelow = [] }
           (firstLine:restLines) ->
             -- Cursor at start of first line: GapZipper [] firstLine
             -- Lines below are just lists (in forward order)
             e { edLinesAbove = []
               , edCurrentLine = Z.GapZipper [] firstLine
               , edLinesBelow = restLines
               }

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
  let aboveWidgets = map (renderGapZipper . listToGap . reverse) (reverse (edLinesAbove ed))
      currentWidget = renderGapZipper (edCurrentLine ed)
      belowWidgets = map (renderGapZipper . listToGap) (edLinesBelow ed)
      lineWidgets = aboveWidgets ++ [currentWidget] ++ belowWidgets
      content = vBox lineWidgets
      (row, col) = getCursorPos ed
      cursorLoc = Location (col, row)
      name = editorName (edConfig ed)
  in withAttr (if hasFocus then editFocusedAttr else editAttr) $
     viewport name Both $
     (if hasFocus then showCursor name cursorLoc else id) $
     content
  where
    listToGap segs = Z.GapZipper [] segs

-- | Render the editor with a prompt prefix
renderEditorWithPrompt :: (Ord n, Show n)
                       => Text      -- ^ Prompt text
                       -> Bool      -- ^ Has focus
                       -> SegmentEditor n InputSegment
                       -> Widget n
renderEditorWithPrompt prompt hasFocus ed =
  let aboveLines = map (renderGapZipper . listToGap . reverse) (reverse (edLinesAbove ed))
      currentLine = renderGapZipper (edCurrentLine ed)
      belowLines = map (renderGapZipper . listToGap) (edLinesBelow ed)
      allLines = aboveLines ++ [currentLine] ++ belowLines
      renderedLines = case allLines of
        [] -> [txt prompt <+> txt " "]
        (firstLine:restLines) ->
          (txt prompt <+> txt " " <+> firstLine)
          : map (\ln -> txt (T.replicate (T.length prompt + 1) " ") <+> ln) restLines
      content = vBox renderedLines
      (row, col) = getCursorPos ed
      -- Adjust cursor location for prompt
      cursorLoc = Location (col + T.length prompt + 1, row)
      name = editorName (edConfig ed)
  in withAttr (if hasFocus then editFocusedAttr else editAttr) $
     viewport name Both $
     (if hasFocus then showCursor name cursorLoc else id) $
     content
  where
    listToGap segs = Z.GapZipper [] segs

-- | Render a line of segments with appropriate styling
renderGapZipper :: GapZipper InputSegment -> Widget n
renderGapZipper line' =
  let allSegs = toList line'
      -- Check if line contains only newline characters
      onlyNewlines = all isHardBreak allSegs
  in if null allSegs || onlyNewlines
     then txt " "  -- Empty line or line with only newlines must render something to maintain height
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
