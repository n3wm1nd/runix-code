{-# LANGUAGE OverloadedStrings #-}

-- | Core types for the SegmentEditor
module UI.SegmentEditor.Types
  ( InputSegment(..)
  , RefState(..)
  , HasLinebreak(..)
  , editorTxt
  , segmentToText
  , segmentLength
  , renderLineSegments
  , renderLineLength
  , pasteDisplayText
  , isWordSegment
  , isSpaceSegment
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)

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
data RefState = RefLoading | RefPending | RefAccepted | RefRejected
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

-- | Get the editor text representation of a segment
-- This is what is displayed to the user and used for measuring width.
-- For PastedSegments, multi-line content shows a placeholder.
-- Newlines render as empty (they're line terminators, not visible content).
editorTxt :: InputSegment -> Text
editorTxt (CharSegment '\n') = ""
editorTxt (CharSegment c) = T.singleton c
editorTxt (FileRefSegment {segRefPaths = (path:_)}) = "@" <> T.pack path
editorTxt (FileRefSegment {segRefPaths = []}) = "@"  -- Shouldn't happen
editorTxt (PastedSegment t) = pasteDisplayText t

-- | Get the actual content text of a segment (used when extracting final text)
-- For PastedSegments, always returns the actual pasted content.
segmentToText :: InputSegment -> Text
segmentToText (CharSegment c) = T.singleton c
segmentToText (FileRefSegment {segRefPaths = (path:_)}) = "@" <> T.pack path
segmentToText (FileRefSegment {segRefPaths = []}) = "@"  -- Shouldn't happen
segmentToText (PastedSegment t) = t  -- Always return actual content

-- | Generate display text for paste segment
-- Single-line: shows actual text, Multi-line: shows placeholder
pasteDisplayText :: Text -> Text
pasteDisplayText t
  | T.any (== '\n') t =
      let lineCount = length (T.lines t)
      in "[paste: " <> T.pack (show lineCount) <> " lines]"
  | otherwise = t

-- | Get display length of a segment
-- Consistently derived from editorTxt (which handles newlines correctly)
segmentLength :: InputSegment -> Int
segmentLength = T.length . editorTxt

-- | Render a line of segments as it would be displayed
-- This applies display rules for SOFT-wrapped lines:
-- - Leading spaces from soft wraps are not displayed (stripped)
-- - But spaces after hard breaks (\n) ARE preserved
-- For now, we strip leading spaces (assuming soft wrap context)
-- TODO: This should take a parameter indicating if this is after a hard break
renderLineSegments :: [InputSegment] -> Text
renderLineSegments segs = T.stripStart (T.concat (map editorTxt segs))

-- | Get the display length of a line of segments
-- Uses the rendered representation (with leading spaces stripped)
renderLineLength :: [InputSegment] -> Int
renderLineLength = T.length . renderLineSegments

-- | Check if a segment is a word character (for word navigation)
isWordSegment :: InputSegment -> Bool
isWordSegment (CharSegment c) = isAlphaNum c || c == '_'
isWordSegment _ = True  -- File refs and pasted text count as words

-- | Check if a segment is whitespace
isSpaceSegment :: InputSegment -> Bool
isSpaceSegment (CharSegment c) = isSpace c
isSpaceSegment _ = False
