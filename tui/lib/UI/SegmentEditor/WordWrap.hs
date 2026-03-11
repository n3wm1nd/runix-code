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
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import UI.SegmentEditor.Types (InputSegment(..), segmentLength)

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

      PastedSegment t ->
        -- Single-line pastes are atomic word units
        -- Multi-line pastes should have been expanded before wrapping
        if T.any (== '\n') t
        then error "wrapLine: multi-line PastedSegment should be expanded before wrapping"
        else go (Word [seg] : acc) rest

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
-- 4. Spaces at line breaks are trimmed
wrapLine :: Int -> [InputSegment] -> [[InputSegment]]
wrapLine width segs = wrapLineWithUnits width (segmentsToWordUnits segs)

-- | Wrap using pre-computed WordUnits (useful for testing)
wrapLineWithUnits :: Int -> [WordUnit] -> [[InputSegment]]
wrapLineWithUnits width units = reverse $ map reverse $ go [] 0 [] units
  where
    -- go :: [[InputSegment]] -> Int -> [InputSegment] -> [WordUnit] -> [[InputSegment]]
    -- Accumulates completed lines, current line width, current line segments
    go completedLines _currentWidth currentLine [] =
      -- End of input - emit current line if non-empty
      if null currentLine
      then completedLines
      else currentLine : completedLines

    go completedLines currentWidth currentLine (unit:rest) = case unit of
      HardBreak seg ->
        -- Hard break - complete current line and start fresh
        let lineWithBreak = seg : currentLine
            newCompleted = lineWithBreak : completedLines
        in go newCompleted 0 [] rest

      Space spaceSegs ->
        let spaceWidth = sum (map segmentLength spaceSegs)
        in if null currentLine
           then
             -- Space at start of line - skip it
             go completedLines 0 currentLine rest
           else if currentWidth + spaceWidth <= width
           then
             -- Space fits on current line
             go completedLines (currentWidth + spaceWidth) (reverse spaceSegs ++ currentLine) rest
           else
             -- Space would overflow - complete line and start fresh (skip the space)
             go (currentLine : completedLines) 0 [] rest

      Word wordSegs ->
        let wordWidth = sum (map segmentLength wordSegs)
        in if null currentLine
           then
             -- First word on line - always fits (even if it overflows width)
             go completedLines wordWidth (reverse wordSegs) rest
           else if currentWidth + wordWidth <= width
           then
             -- Word fits on current line
             go completedLines (currentWidth + wordWidth) (reverse wordSegs ++ currentLine) rest
           else
             -- Word doesn't fit - complete line and start fresh with this word
             go (currentLine : completedLines) wordWidth (reverse wordSegs) rest
