{-# LANGUAGE OverloadedStrings #-}

module WordWrapSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import UI.SegmentEditor.Types (InputSegment(..), RefState(..), editorTxt)
import UI.SegmentEditor.WordWrap

-- Helper to create segments from text
textToSegs :: Text -> [InputSegment]
textToSegs = map CharSegment . T.unpack

-- Helper to convert wrapped lines back to text for comparison
segsToText :: [InputSegment] -> Text
segsToText = T.concat . map editorTxt

spec :: Spec
spec = describe "WordWrap" $ do
  describe "segmentsToWordUnits" $ do
    it "groups consecutive word characters" $ do
      let segs = textToSegs "hello"
          units = segmentsToWordUnits segs
      units `shouldBe` [Word segs]

    it "separates words and spaces" $ do
      let segs = textToSegs "hello world"
          units = segmentsToWordUnits segs
      length units `shouldBe` 3  -- word, space, word

    it "treats hard breaks as separate units" $ do
      let segs = textToSegs "hello\nworld"
          units = segmentsToWordUnits segs
      length units `shouldBe` 3  -- word, hardbreak, word

    it "treats FileRefSegment as atomic word" $ do
      let segs = [FileRefSegment ["foo.txt"] "foo" RefPending]
          units = segmentsToWordUnits segs
      units `shouldBe` [Word [FileRefSegment ["foo.txt"] "foo" RefPending]]

  describe "wrapLine" $ do
    it "single short line fits within width" $ do
      let segs = textToSegs "hello"
          wrapped = wrapLine 10 segs
      length wrapped `shouldBe` 1
      segsToText (head wrapped) `shouldBe` "hello"

    it "wraps long line at word boundary" $ do
      let segs = textToSegs "hello world"
          wrapped = wrapLine 7 segs
      length wrapped `shouldBe` 2
      map segsToText wrapped `shouldBe` ["hello ", "world"]

    it "handles hard breaks" $ do
      let segs = textToSegs "hello\nworld"
          wrapped = wrapLine 20 segs
      length wrapped `shouldBe` 2

    it "word longer than width goes on its own line" $ do
      let segs = textToSegs "superlongword"
          wrapped = wrapLine 5 segs
      length wrapped `shouldBe` 1
      segsToText (head wrapped) `shouldBe` "superlongword"

    it "trims leading spaces on wrapped lines" $ do
      let segs = textToSegs "hello world foo"
          wrapped = wrapLine 7 segs
      map segsToText wrapped `shouldBe` ["hello ", "world ", "foo"]

    it "handles multiple words on same line" $ do
      let segs = textToSegs "a b c d"
          wrapped = wrapLine 10 segs
      length wrapped `shouldBe` 1
      segsToText (head wrapped) `shouldBe` "a b c d"

    it "wraps at exact width boundary" $ do
      let segs = textToSegs "hello world"
          wrapped = wrapLine 11 segs  -- "hello world" is exactly 11
      length wrapped `shouldBe` 1
      segsToText (head wrapped) `shouldBe` "hello world"
