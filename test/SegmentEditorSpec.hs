{-# LANGUAGE OverloadedStrings #-}

module SegmentEditorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import UI.SegmentEditor
import UI.Zipper (Zipper, toList)

-- Test configuration
testConfig :: EditorConfig ()
testConfig = EditorConfig
  { editorName = ()
  , lineLimit = Nothing
  , newlineMode = EnterSends
  }

-- Alias for backwards compatibility
noWrapConfig :: EditorConfig ()
noWrapConfig = testConfig

spec :: Spec
spec = do
  describe "Basic Operations" $ do
    it "inserts single character" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertChar 'a' ed
      getEditorContent ed' `shouldBe` "a"

    it "inserts multiple characters" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertChar 'a' ed
          ed'' = insertChar 'b' ed'
          ed''' = insertChar 'c' ed''
      getEditorContent ed''' `shouldBe` "abc"

    it "deletes character backward" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "abc" ed
          ed'' = delBackward ed'
      getEditorContent ed'' `shouldBe` "ab"

  describe "Newline Handling" $ do
    it "preserves newlines in getEditorContent" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "line1\nline2\nline3" ed
      getEditorContent ed' `shouldBe` "line1\nline2\nline3"

    it "preserves newlines inserted via typeChar" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "line1" ed
          ed'' = typeChar '\n' ed'
          ed''' = insertText "line2" ed''
      getEditorContent ed''' `shouldBe` "line1\nline2"

    it "preserves multiple newlines in a row" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "a\n\n\nb" ed
      getEditorContent ed' `shouldBe` "a\n\n\nb"

    it "preserves newlines at start and end" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "\ntext\n" ed
      getEditorContent ed' `shouldBe` "\ntext\n"

    it "preserves newlines after editor operations and clear" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "line1\nline2" ed
          content1 = getEditorContent ed'
          ed'' = clearEditor ed'
          ed''' = insertText "line1\nline2" ed''
          content2 = getEditorContent ed'''
      content1 `shouldBe` "line1\nline2"
      content2 `shouldBe` "line1\nline2"

    it "preserves newlines when typing character by character" $ do
      let ed = emptyEditor noWrapConfig
          ed1 = insertChar 'h' ed
          ed2 = insertChar 'e' ed1
          ed3 = insertChar 'l' ed2
          ed4 = insertChar 'l' ed3
          ed5 = insertChar 'o' ed4
          ed6 = typeChar '\n' ed5  -- Insert newline via typeChar
          ed7 = insertChar 'w' ed6
          ed8 = insertChar 'o' ed7
          ed9 = insertChar 'r' ed8
          ed10 = insertChar 'l' ed9
          ed11 = insertChar 'd' ed10
      getEditorContent ed11 `shouldBe` "hello\nworld"
      -- Also verify we have 2 lines
      let allLines = getEditorLines ed11
      length allLines `shouldBe` 2  -- Should be 2 lines

    it "typeChar newline actually inserts CharSegment newline" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "hello" ed
          ed'' = typeChar '\n' ed'
          ed''' = insertText "world" ed''
      -- Verify content has newline
      getEditorContent ed''' `shouldBe` "hello\nworld"
      -- Verify we have 2 lines when split
      length (getEditorLines ed''') `shouldBe` 2

    it "editorFromText preserves newlines" $ do
      let ed = editorFromText noWrapConfig "hello\nworld"
      getEditorContent ed `shouldBe` "hello\nworld"
      length (getEditorLines ed) `shouldBe` 2

    it "simulates UI flow: type hello, backslash-enter, world" $ do
      -- Start with empty editor in EnterSends mode
      let config = EditorConfig
            { editorName = ()
            , lineLimit = Nothing
            , newlineMode = EnterSends
            }
          ed0 = emptyEditor config
      -- Type "hello\"
      let ed1 = insertChar 'h' ed0
          ed2 = insertChar 'e' ed1
          ed3 = insertChar 'l' ed2
          ed4 = insertChar 'l' ed3
          ed5 = insertChar 'o' ed4
          ed6 = insertChar '\\' ed5
      -- Now press Enter (should trigger backslash+Enter logic)
      -- Simulate VtyEvent (EvKey KEnter [])
      -- This should: delete backslash, type newline
      let ed7 = delBackward ed6  -- Remove backslash
          ed8 = typeChar '\n' ed7  -- Type newline
      -- Type "world"
      let ed9 = insertChar 'w' ed8
          ed10 = insertChar 'o' ed9
          ed11 = insertChar 'r' ed10
          ed12 = insertChar 'l' ed11
          ed13 = insertChar 'd' ed12
      -- Check content
      getEditorContent ed13 `shouldBe` "hello\nworld"
      length (getEditorLines ed13) `shouldBe` 2

  describe "Empty Line Handling" $ do
    it "handles empty line in the middle via typeChar" $ do
      let ed = emptyEditor noWrapConfig
          ed1 = insertText "line1" ed
          ed2 = typeChar '\n' ed1
          ed3 = typeChar '\n' ed2  -- Empty line
          ed4 = insertText "line3" ed3
      getEditorContent ed4 `shouldBe` "line1\n\nline3"
      length (getEditorLines ed4) `shouldBe` 3

    it "handles empty line via insertText" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "a\n\nb" ed
      getEditorContent ed' `shouldBe` "a\n\nb"
      length (getEditorLines ed') `shouldBe` 3

    it "handles empty line at start via typeChar" $ do
      let ed = emptyEditor noWrapConfig
          ed1 = typeChar '\n' ed
          ed2 = insertText "text" ed1
      getEditorContent ed2 `shouldBe` "\ntext"
      length (getEditorLines ed2) `shouldBe` 2

    it "handles multiple consecutive empty lines via typeChar" $ do
      let ed = emptyEditor noWrapConfig
          ed1 = insertText "a" ed
          ed2 = typeChar '\n' ed1
          ed3 = typeChar '\n' ed2
          ed4 = typeChar '\n' ed3
          ed5 = insertText "b" ed4
      getEditorContent ed5 `shouldBe` "a\n\n\nb"
      length (getEditorLines ed5) `shouldBe` 4

    it "typing characters then newlines creates empty lines" $ do
      let ed = emptyEditor noWrapConfig
          ed1 = typeChar 'a' ed
          ed2 = typeChar '\n' ed1
          ed3 = typeChar '\n' ed2  -- Empty line
          ed4 = typeChar 'b' ed3
      getEditorContent ed4 `shouldBe` "a\n\nb"
      length (getEditorLines ed4) `shouldBe` 3

  describe "Property: typing preserves content" $ do
    it "typing string char-by-char equals insertText" $ property $
      \(str :: String) ->
        let edTyped = foldr (flip (.)) id [insertChar c | c <- str] (emptyEditor noWrapConfig)
            edInserted = insertText (T.pack str) (emptyEditor noWrapConfig)
        in getEditorContent edTyped == getEditorContent edInserted

    it "typing with typeChar for newlines preserves content" $ property $
      \(str :: String) ->
        let edTyped = foldl (\e c -> typeChar c e) (emptyEditor noWrapConfig) str
            edInserted = insertText (T.pack str) (emptyEditor noWrapConfig)
        in getEditorContent edTyped == getEditorContent edInserted

  describe "Property: editing with backspace" $ do
    it "sequence of chars, newlines, and backspaces produces correct content" $ property $
      \(actions :: [EditAction]) ->
        let ed = foldl applyAction (emptyEditor noWrapConfig) actions
            expectedContent = simulateActions actions
        in getEditorContent ed == T.pack expectedContent

    it "all non-final lines end with newline" $ property $
      \(actions :: [EditAction]) ->
        let ed = foldl applyAction (emptyEditor noWrapConfig) actions
            linesAbove = edLinesAbove ed
            -- All lines in linesAbove should end with '\n'
            aboveValid = all lineEndsWithNewline linesAbove
        in aboveValid

-- | Actions that can be performed on the editor
data EditAction
  = TypeChar Char
  | TypeNewline
  | Backspace
  deriving (Show, Eq)

instance Arbitrary EditAction where
  arbitrary = frequency
    [ (10, TypeChar <$> arbitraryPrintableChar)
    , (3, pure TypeNewline)
    , (2, pure Backspace)
    ]

-- | Apply an action to the editor
applyAction :: SegmentEditor () InputSegment -> EditAction -> SegmentEditor () InputSegment
applyAction ed (TypeChar c) = typeChar c ed
applyAction ed TypeNewline = typeChar '\n' ed
applyAction ed Backspace = delBackward ed

-- | Simulate actions on a plain string to get expected content
simulateActions :: [EditAction] -> String
simulateActions = go ""
  where
    go acc [] = reverse acc
    go acc (TypeChar c : rest) = go (c : acc) rest
    go acc (TypeNewline : rest) = go ('\n' : acc) rest
    go acc (Backspace : rest) = case acc of
      [] -> go acc rest  -- Backspace at start does nothing
      (_:xs) -> go xs rest

-- | Check if a line (Zipper) ends with a newline
lineEndsWithNewline :: Zipper InputSegment -> Bool
lineEndsWithNewline z =
  let segments = toList z
  in case reverse segments of
    [] -> False  -- Empty line (shouldn't happen for non-current lines)
    (CharSegment '\n' : _) -> True
    _ -> False
