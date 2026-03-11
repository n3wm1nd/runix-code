{-# LANGUAGE OverloadedStrings #-}

module WordWrapSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)
import qualified Data.Text as T
import Text.Wrap (WrapSettings(..), defaultWrapSettings, wrapTextToLines)
import UI.SegmentEditor.Types (InputSegment(..), RefState(..), editorTxt, renderLineSegments)
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
-- We preserve data (including leading spaces), but renderLineSegments shows display
prop_matchesTextWrap :: Property
prop_matchesTextWrap = forAll genSimpleText $ \txt ->
  forAll genWidth $ \width ->
    let ourResult = wrapLine width (textToSegs txt)
        ourRendered = map renderLineSegments ourResult

        -- word-wrap uses WrapSettings and works with strict Text
        settings = defaultWrapSettings { preserveIndentation = False, breakLongWords = False }
        theirResult = wrapTextToLines settings width txt
        theirLines = theirResult

    in counterexample ("Input: " ++ show txt) $
       counterexample ("Width: " ++ show width) $
       counterexample ("Our rendered: " ++ show ourRendered) $
       counterexample ("Their result: " ++ show theirLines) $
       -- Compare rendered output (which strips leading spaces like word-wrap does)
       map T.stripEnd ourRendered === map T.stripEnd theirLines

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

    it "preserves leading spaces (data) but trims for display" $ do
      let segs = textToSegs "hello world foo"
          wrapped = wrapLine 7 segs
      -- First verify concat preserves everything
      concat wrapped `shouldBe` segs
      -- Rendered output trims leading spaces
      map renderLineSegments wrapped `shouldBe` ["hello ", "world ", "foo"]

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
    it "matches Text.Wrap.wrapTextToLines for simple text (ignoring leading spaces)" $
      property prop_matchesTextWrap

  describe "wrapLine (invariant properties)" $ do
    it "all wrapped lines fit within width (using render length, trimming trailing spaces)" $
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width ->
          let wrapped = wrapLine width (textToSegs txt)
              -- Render and trim both leading AND trailing spaces for width check
              -- (trailing spaces can overflow when attached to prevent empty lines)
              lineLengths = map (T.length . T.strip . renderLineSegments) wrapped
          in all (<= width) lineLengths || any (> width) (map T.length (T.words txt))

    it "concatenating wrapped lines equals original exactly" $
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width ->
          let segs = textToSegs txt
              wrapped = wrapLine width segs
              rejoined = concat wrapped
          in rejoined === segs

    it "number of wrapped lines is at least 1 for non-empty input" $
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width ->
          not (T.null txt) ==>
            let wrapped = wrapLine width (textToSegs txt)
            in length wrapped >= 1

  describe "rewrap" $ do
    it "rewrapping already-wrapped lines produces same result" $ do
      let text = "hello world this is a test"
          segs = textToSegs text
          wrapped1 = wrapLine 10 segs
          rewrapped = rewrap 10 wrapped1
      rewrapped `shouldBe` wrapped1

    it "rewrapping at wider width uses fewer lines" $ do
      let text = "hello world this is a long test sentence"
          segs = textToSegs text
          narrow = wrapLine 10 segs
          wide = rewrap 40 narrow
      length wide `shouldSatisfy` (<= length narrow)

    it "rewrapping at narrower width uses more lines" $ do
      let text = "hello world this is a test"
          segs = textToSegs text
          wide = wrapLine 40 segs
          narrow = rewrap 10 wide
      length narrow `shouldSatisfy` (>= length wide)

    it "rewrapping preserves all segments exactly" $
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width1 ->
          forAll genWidth $ \width2 ->
            let segs = textToSegs txt
                wrapped1 = wrapLine width1 segs
                wrapped2 = rewrap width2 wrapped1
            in concat wrapped1 === concat wrapped2

    it "rewrapping preserves hard breaks" $ do
      let text = "hello\nworld\ntest"
          segs = textToSegs text
          wrapped = wrapLine 20 segs
          rewrapped = rewrap 30 wrapped
      -- Should still have 3 lines due to hard breaks
      length rewrapped `shouldBe` 3

  describe "wrapLine (leading space invariants)" $ do
    it "no line starts with space except first line or after hard break" $ do
      let startsWithSpace [] = False
          startsWithSpace (CharSegment ' ':_) = True
          startsWithSpace (CharSegment '\t':_) = True
          startsWithSpace _ = False
      property $ forAll genSimpleText $ \txt ->
        forAll genWidth $ \width ->
          let wrapped = wrapLine width (textToSegs txt)
          in all (not . startsWithSpace) (drop 1 wrapped) -- All lines after first

    it "lines after hard breaks CAN start with spaces" $ do
      let text = "hello\n  world"  -- Hard break followed by spaces
          segs = textToSegs text
          wrapped = wrapLine 20 segs
      length wrapped `shouldBe` 2
      -- Second line should preserve the leading spaces after \n
      case wrapped of
        [line1, line2] -> do
          -- Line 1 ends with \n
          last line1 `shouldBe` CharSegment '\n'
          -- Line 2 starts with spaces (preserved after hard break)
          take 2 line2 `shouldBe` [CharSegment ' ', CharSegment ' ']
        _ -> expectationFailure "Should have exactly 2 lines"

    it "soft-wrapped lines never start with spaces" $ do
      let text = "hello world foo bar"  -- No hard breaks
          segs = textToSegs text
          wrapped = wrapLine 7 segs
      -- All lines after the first should not start with space
      all (not . startsWithSpace) (drop 1 wrapped) `shouldBe` True
      where
        startsWithSpace [] = False
        startsWithSpace (CharSegment ' ':_) = True
        startsWithSpace _ = False
