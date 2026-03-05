{-# LANGUAGE OverloadedStrings #-}

-- | Rich input content model for @file support
--
-- This module defines a segment-based content model that can represent both
-- plain text and file references with state. It implements GenericTextZipper
-- to work seamlessly with Brick's Editor widget.
module UI.RichInput
  ( -- * Input segments
    InputSegment(..)
  , RefState(..)
  , SegmentLine
    -- * Operations
  , segmentsToText
  , segmentsLength
  , segmentsTake
  , segmentsDrop
  , segmentsLast
  , segmentsInit
  , segmentsLines
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.String (IsString(..))
import qualified Data.Text.Zipper.Generic as Z
import Brick.Widgets.Core (TextWidth(..))
import Brick.Widgets.Edit (DecodeUtf8(..))

-- | A segment of input content
data InputSegment
  = TextSegment Text
  | FileRefSegment
      { refTypedText :: Text      -- ^ What user typed (e.g., "ReadMe")
      , refFilePath :: FilePath    -- ^ Resolved file path
      , refState :: RefState       -- ^ Reference state
      }
  deriving stock (Eq, Show)

-- | State of a file reference
data RefState = RefPending | RefAccepted | RefRejected
  deriving stock (Eq, Show)

-- | Type alias for clarity: a line is a list of segments
type SegmentLine = [InputSegment]

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Flatten segments to text (for rendering/submission)
segmentsToText :: [InputSegment] -> Text
segmentsToText = mconcat . map segToText
  where
    segToText (TextSegment t) = t
    segToText (FileRefSegment _ path _) = "@" <> T.pack path

-- | Get character length of segments
segmentsLength :: [InputSegment] -> Int
segmentsLength = T.length . segmentsToText

-- | Take n characters from segments, preserving structure
segmentsTake :: Int -> [InputSegment] -> [InputSegment]
segmentsTake n segs = go n segs
  where
    go 0 _ = []
    go _ [] = []
    go remaining (seg:rest) =
      let segText = case seg of
                      TextSegment t -> t
                      FileRefSegment _ p _ -> "@" <> T.pack p
          segLen = T.length segText
      in if remaining >= segLen
         then seg : go (remaining - segLen) rest
         else case seg of
                TextSegment t -> [TextSegment (T.take remaining t)]
                FileRefSegment{} -> [seg]  -- Don't split file refs

-- | Drop n characters from segments, preserving structure
segmentsDrop :: Int -> [InputSegment] -> [InputSegment]
segmentsDrop n segs = go n segs
  where
    go 0 rest = rest
    go _ [] = []
    go remaining (seg:rest) =
      let segText = case seg of
                      TextSegment t -> t
                      FileRefSegment _ p _ -> "@" <> T.pack p
          segLen = T.length segText
      in if remaining >= segLen
         then go (remaining - segLen) rest
         else case seg of
                TextSegment t -> TextSegment (T.drop remaining t) : rest
                FileRefSegment{} -> seg : rest  -- Don't split file refs

-- | Get last character
segmentsLast :: [InputSegment] -> Char
segmentsLast segs = T.last (segmentsToText segs)

-- | Remove last character, preserving structure
segmentsInit :: [InputSegment] -> [InputSegment]
segmentsInit [] = []
segmentsInit segs =
  let totalLen = segmentsLength segs
  in segmentsTake (totalLen - 1) segs

-- | Split on newlines
segmentsLines :: [InputSegment] -> [[InputSegment]]
segmentsLines segs = go segs []
  where
    go [] acc = [reverse acc]
    go (seg:rest) acc = case seg of
      TextSegment t ->
        let parts = T.splitOn "\n" t
        in case parts of
             [single] -> go rest (TextSegment single : acc)
             (first:others) ->
               let finishLine = reverse (TextSegment first : acc)
                   middleLines = map (\txt -> [TextSegment txt]) (init others)
                   lastPart = TextSegment (last others)
               in [finishLine] ++ middleLines ++ go rest [lastPart]
             [] -> go rest acc
      FileRefSegment{} -> go rest (seg : acc)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- Note: SegmentLine is a type alias for [InputSegment], so it already has
-- Semigroup and Monoid instances from the list type. We don't need to define them.

instance Z.GenericTextZipper SegmentLine where
  singleton c = [TextSegment (T.singleton c)]
  take = segmentsTake
  drop = segmentsDrop
  length = segmentsLength
  last = segmentsLast
  init = segmentsInit
  null = Prelude.null
  lines = segmentsLines
  toList = T.unpack . segmentsToText

instance IsString SegmentLine where
  fromString s = [TextSegment (T.pack s)]

-- | TextWidth instance for calculating display width
instance TextWidth SegmentLine where
  textWidth segs = T.length (segmentsToText segs)

-- | DecodeUtf8 instance for UTF-8 handling in paste events
instance DecodeUtf8 SegmentLine where
  decodeUtf8 bs = Right [TextSegment (TE.decodeUtf8 bs)]
