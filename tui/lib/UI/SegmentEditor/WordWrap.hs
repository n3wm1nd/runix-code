{-# LANGUAGE OverloadedStrings #-}

-- | Pure word-wrapping functions for InputSegments
--
-- This module provides clean, testable functions for wrapping lines of
-- InputSegments at word boundaries. The design is based on grouping segments
-- into atomic "word units" that should not be broken across lines.
module UI.SegmentEditor.WordWrap
  ( WordUnit(..)
  , segmentsToWordUnits
  , wrapLine
  , wrapLineWithUnits
  , rewrap
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import UI.SegmentEditor.Types (InputSegment(..), segmentLength, renderLineLength)

-- | A word unit is an atomic sequence of segments that should not be broken
-- across lines. This represents the natural breaking points in text.
data WordUnit
  = Word [InputSegment]      -- ^ Sequence of word characters (or single FileRef/Paste)
  | Space [InputSegment]     -- ^ Sequence of spaces
  | HardBreak InputSegment   -- ^ Hard line break ('\n')
  deriving (Eq, Show)

-- | Get the display width of a word unit
wordUnitWidth :: WordUnit -> Int
wordUnitWidth (Word segs) = sum (map segmentLength segs)
wordUnitWidth (Space segs) = sum (map segmentLength segs)
wordUnitWidth (HardBreak seg) = segmentLength seg

-- | Get the segments from a word unit
wordUnitSegments :: WordUnit -> [InputSegment]
wordUnitSegments (Word segs) = segs
wordUnitSegments (Space segs) = segs
wordUnitSegments (HardBreak seg) = [seg]

-- | Convert a list of InputSegments into WordUnits
-- This groups segments by their breaking characteristics:
-- - Consecutive word chars become one Word unit
-- - Consecutive spaces become one Space unit
-- - Hard breaks are isolated as HardBreak units
-- - FileRefs and Pastes are treated as atomic Word units
segmentsToWordUnits :: [InputSegment] -> [WordUnit]
segmentsToWordUnits = reverse . map reverseUnit . go []
  where
    -- Reverse the segments within Word and Space units
    reverseUnit (Word segs) = Word (reverse segs)
    reverseUnit (Space segs) = Space (reverse segs)
    reverseUnit (HardBreak seg) = HardBreak seg

    go acc [] = acc
    go acc (seg:rest) = case seg of
      CharSegment '\n' ->
        -- Hard break - emit accumulated word/space first, then the break
        go (HardBreak seg : acc) rest

      CharSegment c | isSpace c ->
        -- Start or continue a space unit
        case acc of
          Space segs : prevAcc -> go (Space (seg:segs) : prevAcc) rest
          _ -> go (Space [seg] : acc) rest

      CharSegment c | isWordChar c ->
        -- Start or continue a word unit
        case acc of
          Word segs : prevAcc -> go (Word (seg:segs) : prevAcc) rest
          _ -> go (Word [seg] : acc) rest

      CharSegment _ ->
        -- Other punctuation - treat as a word unit by itself
        go (Word [seg] : acc) rest

      FileRefSegment {} ->
        -- File refs are atomic - always their own word unit
        go (Word [seg] : acc) rest

      PastedSegment _ ->
        -- Pasted segments are atomic word units (treated as single words)
        -- Multi-line pastes display as placeholders (e.g., "[paste: 3 lines]")
        -- so they should be wrapped based on the placeholder display width, not content
        go (Word [seg] : acc) rest

    isSpace ' ' = True
    isSpace '\t' = True
    isSpace _ = False

    isWordChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])

-- | Wrap a line of InputSegments at the given width
-- Returns multiple lines, each fitting within the width limit
-- Respects word boundaries and hard breaks
--
-- Rules:
-- 1. Hard breaks ('\n') always force a new line
-- 2. Words are not broken across lines
-- 3. If a single word exceeds width, it goes on its own line (overflow)
-- 4. Trailing spaces stay with the previous line (prevents lines starting with spaces)
-- 5. Lines can only start with spaces if:
--    - It's the first line, OR
--    - The previous line ended with a hard break ('\n')
wrapLine :: Int -> [InputSegment] -> [[InputSegment]]
wrapLine width segs = wrapLineWithUnits width (segmentsToWordUnits segs)

-- | Wrap using pre-computed WordUnits (useful for testing)
wrapLineWithUnits :: Int -> [WordUnit] -> [[InputSegment]]
wrapLineWithUnits width units = reverse $ map reverse $ go [] [] units
  where
    -- go :: [[InputSegment]] -> [InputSegment] -> [WordUnit] -> [[InputSegment]]
    -- Accumulates completed lines and current line segments
    -- We use renderLineLength to measure display width (strips leading spaces)
    go completedLines currentLine [] =
      -- End of input - emit current line if non-empty
      if null currentLine
      then completedLines
      else currentLine : completedLines

    go completedLines currentLine (unit:rest) = case unit of
      HardBreak seg ->
        -- Hard break - complete current line and start fresh
        let lineWithBreak = seg : currentLine
            newCompleted = lineWithBreak : completedLines
        in go newCompleted [] rest

      Space spaceSegs ->
        let candidateLine = reverse spaceSegs ++ currentLine
            candidateWidth = renderLineLength candidateLine
        in if candidateWidth <= width
           then
             -- Space fits on current line (or is leading space with 0 display width)
             go completedLines candidateLine rest
           else
             -- Space would overflow - attach it to current line as trailing space
             -- This prevents creating lines that are only spaces (which render as empty)
             let lineWithSpace = reverse spaceSegs ++ currentLine
             in go (lineWithSpace : completedLines) [] rest

      Word wordSegs ->
        let candidateLine = reverse wordSegs ++ currentLine
            candidateWidth = renderLineLength candidateLine
        in if null currentLine
           then
             -- First word on line - always fits (even if it overflows width)
             go completedLines candidateLine rest
           else if candidateWidth <= width
           then
             -- Word fits on current line
             go completedLines candidateLine rest
           else
             -- Word doesn't fit - complete line and start fresh with this word
             go (currentLine : completedLines) (reverse wordSegs) rest

-- | Rewrap lines at a new width
-- Takes a list of lines (each line is a list of segments), joins them,
-- and wraps them at the specified width.
--
-- This is the core operation for dynamic rewrapping when the terminal width changes.
-- It removes all soft line breaks (joins lines) and creates new soft breaks at word boundaries.
--
-- Note: Hard breaks (CharSegment '\n') are preserved and force line breaks.
rewrap :: Int -> [[InputSegment]] -> [[InputSegment]]
rewrap width lines' =
  let -- Join all lines into a single list of segments
      allSegments = concat lines'
  in wrapLine width allSegments
