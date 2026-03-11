{-# LANGUAGE OverloadedStrings #-}

module WordWrapSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Text.Wrap (WrapSettings(..), defaultWrapSettings, wrapTextToLines)
import UI.SegmentEditor.Types (InputSegment(..), RefState(..), editorTxt)
import UI.SegmentEditor.WordWrap

-- Helper to create segments from text
textToSegs :: Text -> [InputSegment]
textToSegs = map CharSegment . T.unpack

-- Helper to convert wrapped lines back to text for comparison
segsToText :: [InputSegment] -> Text
segsToText = T.concat . map editorTxt

-- QuickCheck generators
-- Generate simple text without special segments (for comparison with word-wrap)
genSimpleText :: Gen Text
genSimpleText = do
  -- Generate text with words and spaces, no newlines
  words' <- listOf1 genWord
  return $ T.intercalate " " words'
  where
    genWord = do
      len <- choose (1, 15)
      T.pack <$> vectorOf len (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

genWidth :: Gen Int
genWidth = choose (10, 80)

-- Property: Our wrapping should match Text.Wrap for simple text
-- (text with only word characters and spaces, no special segments)
prop_matchesTextWrap :: Property
prop_matchesTextWrap = forAll genSimpleText $ \txt ->
  forAll genWidth $ \width ->
    let ourResult = wrapLine width (textToSegs txt)
        ourLines = map segsToText ourResult

        -- word-wrap uses WrapSettings and works with strict Text
        settings = defaultWrapSettings { preserveIndentation = False, breakLongWords = False }
        theirResult = wrapTextToLines settings width txt
        theirLines = theirResult

    in counterexample ("Input: " ++ show txt) $
       counterexample ("Width: " ++ show width) $
       counterexample ("Our result: " ++ show ourLines) $
       counterexample ("Their result: " ++ show theirLines) $
       -- Compare ignoring trailing spaces (our implementation preserves them, word-wrap may trim)
       map T.stripEnd ourLines === map T.stripEnd theirLines

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

  describe "wrapLine (properties vs word-wrap library)" $ do
    it "matches Text.Wrap.wrapTextToLines for simple text" $
      property prop_matchesTextWrap

  describe "wrapLine (invariant properties)" $ do
    it "all wrapped lines fit within width (except overflow words)" $
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width ->
          let wrapped = wrapLine width (textToSegs txt)
              lineLengths = map (T.length . segsToText) wrapped
          in all (<= width) lineLengths || any (> width) (map T.length (T.words txt))

    it "concatenating wrapped lines equals original (ignoring spacing)" $
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width ->
          let wrapped = wrapLine width (textToSegs txt)
              rejoined = T.unwords (map (T.strip . segsToText) wrapped)
              original = T.unwords (T.words txt)
          in rejoined === original

    it "number of wrapped lines is at least 1 for non-empty input" $
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width ->
          not (T.null txt) ==>
            let wrapped = wrapLine width (textToSegs txt)
            in length wrapped >= 1
