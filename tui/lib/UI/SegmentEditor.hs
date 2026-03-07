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
  deriving (Eq, Show, Ord)

-- | State of a file reference
data RefState = RefPending | RefAccepted | RefRejected
  deriving (Eq, Show, Ord)

-- | Typeclass for segments that can represent linebreaks
class HasLinebreak a where
  -- | Check if a segment is any kind of linebreak (hard or soft)
  isLinebreak :: a -> Bool

  -- | Check if a segment is specifically a hard linebreak (explicit newline)
  isHardBreak :: a -> Bool

  -- | Check if a segment is specifically a soft linebreak (word-wrapping)
  isSoftBreak :: a -> Bool

-- | Instance for InputSegment
instance HasLinebreak InputSegment where
  isLinebreak (CharSegment '\n') = True
  isLinebreak _ = False

  isHardBreak (CharSegment '\n') = True
  isHardBreak _ = False

  isSoftBreak _ = False  -- Not implemented yet for InputSegment

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
-- - Lines above current: stored as Zippers at END (non-empty, reversed order)
-- - Current line: stored as GapZipper (can be empty only if no other lines)
-- - Lines below current: stored as Zippers at START (non-empty, forward order)
--
-- INVARIANTS:
-- ===========
-- 1. Lines in edLinesAbove are non-empty Zippers positioned at END
-- 2. Lines in edLinesBelow are non-empty Zippers positioned at START
-- 3. edCurrentLine can be empty, but in the context of how we're using it,
--    this only happens when starting a new line after a newline
--
-- This provides efficient local edits while maintaining 2D structure.
--
-- The type parameter 'a' is the segment type (typically InputSegment)
-- The 'n' parameter is for the Brick widget name
data SegmentEditor n a = SegmentEditor
  { edConfig :: EditorConfig n
  , edLinesAbove :: [Zipper a]     -- ^ Lines above cursor (at END, reversed)
  , edCurrentLine :: GapZipper a   -- ^ Current line with cursor
  , edLinesBelow :: [Zipper a]     -- ^ Lines below cursor (at START, forward)
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
editorFromText cfg txt =
  -- Just use insertText which handles newlines correctly via typeChar
  insertText txt (emptyEditor cfg)

-- | Create an editor from segment lines (cursor at start of first line)
editorFromSegments :: EditorConfig n -> [Z.GapZipper a] -> SegmentEditor n a
editorFromSegments config [] = emptyEditor config
editorFromSegments config (firstLine:restLines) =
  let linesBelow = map (\gap -> case Z.listToZipper (toList gap) of
                           Nothing -> error "Empty line in editorFromSegments"
                           Just z -> z) restLines
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
  let aboveTexts = map (segmentLineToText . toZipperAsGap) (reverse (edLinesAbove ed))
      currentText = segmentLineToText (edCurrentLine ed)
      belowTexts = map (segmentLineToText . toZipperAsGap) (edLinesBelow ed)
      allTexts = aboveTexts ++ [currentText] ++ belowTexts
  in T.concat allTexts  -- Just concatenate - newlines come from CharSegment '\n'
  where
    toZipperAsGap z = Z.GapZipper [] (toList z)

-- | Get content as list of lines (as Text)
getEditorLines :: SegmentEditor n InputSegment -> [Text]
getEditorLines ed =
  let aboveTexts = map (segmentLineToText . toZipperAsGap) (reverse (edLinesAbove ed))
      currentText = segmentLineToText (edCurrentLine ed)
      belowTexts = map (segmentLineToText . toZipperAsGap) (edLinesBelow ed)
  in aboveTexts ++ [currentText] ++ belowTexts
  where
    toZipperAsGap z = Z.GapZipper [] (toList z)


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
-- Invariant: lines in edLinesAbove/Below are Zippers (non-empty) stored at END/START
instance Zippable (SegmentEditor n) where
  -- Move forward one segment (right, or to next line if at end of line)
  forward ed =
    if not (atEnd (edCurrentLine ed))
    then
      -- Move forward within current line
      ed { edCurrentLine = Z.forward (edCurrentLine ed) }
    else
      -- At end of line, move to next line (which is stored at START)
      case edLinesBelow ed of
        [] -> ed  -- No next line
        (Z.Zipper [] cur front : rest) ->
          -- nextLine is at START: Zipper [] cur front
          -- Convert to GapZipper at START: GapZipper [] (cur:front)
          let nextAsGap = Z.GapZipper [] (cur : front)
          in case edCurrentLine ed of
            Z.GapZipper [] [] ->
              -- Current line is empty, don't store it
              ed { edCurrentLine = nextAsGap, edLinesBelow = rest }
            Z.GapZipper before [] ->
              -- Current line at END, convert to Zipper at END
              let (b:bs) = before
                  currentAsZipper = Z.Zipper bs b []
              in ed { edLinesAbove = currentAsZipper : edLinesAbove ed
                    , edCurrentLine = nextAsGap
                    , edLinesBelow = rest }
        _ -> error "Invariant violated: line in edLinesBelow not at START"

  -- Move backward one segment (left, or to previous line if at start of line)
  back ed =
    if not (atStart (edCurrentLine ed))
    then
      -- Move back within current line
      ed { edCurrentLine = Z.back (edCurrentLine ed) }
    else
      -- At start of line, move to previous line (which is stored at END)
      case edLinesAbove ed of
        [] -> ed  -- No previous line
        (Z.Zipper back cur [] : rest) ->
          -- prevLine is at END: Zipper back cur []
          -- Convert to GapZipper at END: GapZipper (cur:back) []
          let prevAsGap = Z.GapZipper (cur : back) []
          in case edCurrentLine ed of
            Z.GapZipper [] [] ->
              -- Current line is empty, don't store it
              ed { edLinesAbove = rest, edCurrentLine = prevAsGap }
            Z.GapZipper [] after ->
              -- Current line at START, convert to Zipper at START
              let (a:as) = after
                  currentAsZipper = Z.Zipper [] a as
              in ed { edLinesAbove = rest
                    , edCurrentLine = prevAsGap
                    , edLinesBelow = currentAsZipper : edLinesBelow ed }
        _ -> error "Invariant violated: line in edLinesAbove not at END"

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
    let linesAboveSegs = concatMap toList (reverse (edLinesAbove ed))
        currentSegs = toList (edCurrentLine ed)
        linesBelowSegs = concatMap toList (edLinesBelow ed)
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
insertChar c ed = forward $ insertBackward (CharSegment c) ed

-- | Type a character - handles linebreaks by calling breakLine after inserting
typeChar :: HasLinebreak InputSegment => Char -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
typeChar c ed =
  if isHardBreak (CharSegment c)
  then breakLine (insertChar c ed)  -- Insert newline, then break line
  else insertChar c ed

-- | Insert text at cursor (as if typing each character)
insertText :: Text -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertText txt ed = foldl (\e c -> typeChar c e) ed (T.unpack txt)

-- | Insert a segment at cursor
insertSegment :: InputSegment -> SegmentEditor n InputSegment -> SegmentEditor n InputSegment
insertSegment seg ed = forward $ insertBackward seg ed

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
             line''' = forward line''  -- Move cursor past new segment
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
       -- At start of line - move to previous line and delete last character there
       case edLinesAbove ed of
         [] -> ed  -- At start of document (no previous line)
         (Z.Zipper back prevCur [] : rest) ->
           -- Previous line is at END: Zipper back prevCur []
           -- prevCur is the last character (should be '\n')
           -- Delete it by not including it in the joined line
           -- NOTE: When word-wrapping is implemented, this is where we would trigger rewrapping
           let joinedLine = Z.GapZipper back (Z.gapAfter currentLine)
           in ed { edLinesAbove = rest, edCurrentLine = joinedLine }
         _ -> error "Invariant violated: line in edLinesAbove not at END"
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
         (Z.Zipper [] nextCur front : rest) ->
           -- Next line is at START: Zipper [] nextCur front
           -- Join: current line's before + (nextCur:front)
           let joinedLine = Z.GapZipper (Z.gapBefore currentLine) (nextCur : front)
           in ed { edLinesBelow = rest, edCurrentLine = joinedLine }
         _ -> error "Invariant violated: line in edLinesBelow not at START"
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
        (b:bs) ->
          -- Move completed line (beforeSegments) to edLinesAbove
          -- Create Zipper at END directly: Zipper [2,1] 3 [] for beforeSegments = [3,2,1]
          let completedAsZipper = Z.Zipper bs b []
          in ed' { edLinesAbove = completedAsZipper : edLinesAbove ed'
                 , edCurrentLine = newLine
                 }

--------------------------------------------------------------------------------
-- Cursor movement
--------------------------------------------------------------------------------

-- | Move cursor left (by one segment, crossing line boundaries)
moveCursorLeft :: SegmentEditor n a -> SegmentEditor n a
moveCursorLeft = back

-- | Move cursor right (by one segment, crossing line boundaries)
moveCursorRight :: SegmentEditor n a -> SegmentEditor n a
moveCursorRight = forward

-- | Move cursor up one line (maintaining column position)
-- TODO: Preserve column position when moving between lines
-- For now, just move to end of previous line
moveCursorUp :: SegmentEditor n a -> SegmentEditor n a
moveCursorUp ed =
  case edLinesAbove ed of
    [] -> ed  -- No previous line
    _ -> let ed' = start ed  -- Move to start of current line
         in back ed'  -- Move to previous line (which is at END)

-- | Move cursor down one line (maintaining column position)
-- TODO: Preserve column position when moving between lines
-- For now, just move to start of next line
moveCursorDown :: SegmentEditor n a -> SegmentEditor n a
moveCursorDown ed =
  case edLinesBelow ed of
    [] -> ed  -- No next line
    _ -> let ed' = end ed  -- Move to end of current line
         in forward ed'  -- Move to next line (which is at START)

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
-- Rendering
--------------------------------------------------------------------------------

-- | Render the editor with optional focus
-- This version renders segments with special styling
renderEditor :: (Ord n, Show n)
             => Bool  -- ^ Has focus
             -> SegmentEditor n InputSegment
             -> Widget n
renderEditor hasFocus ed =
  let aboveWidgets = map renderZipperLine (reverse (edLinesAbove ed))
      currentWidget = renderGapZipper (edCurrentLine ed)
      belowWidgets = map renderZipperLine (edLinesBelow ed)
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
    renderZipperLine z = renderGapZipper (Z.GapZipper [] (toList z))

-- | Render the editor with a prompt prefix
renderEditorWithPrompt :: (Ord n, Show n)
                       => Text      -- ^ Prompt text
                       -> Bool      -- ^ Has focus
                       -> SegmentEditor n InputSegment
                       -> Widget n
renderEditorWithPrompt prompt hasFocus ed =
  let aboveLines = map renderZipperLine (reverse (edLinesAbove ed))
      currentLine = renderGapZipper (edCurrentLine ed)
      belowLines = map renderZipperLine (edLinesBelow ed)
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
    renderZipperLine z = renderGapZipper (Z.GapZipper [] (toList z))

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
