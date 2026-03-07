{-# LANGUAGE OverloadedStrings #-}

module SegmentEditorSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import UI.SegmentEditor

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
          ed'' = deleteBackward ed'
      getEditorContent ed'' `shouldBe` "ab"

  describe "Newline Handling" $ do
    it "preserves newlines in getEditorContent" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "line1\nline2\nline3" ed
      getEditorContent ed' `shouldBe` "line1\nline2\nline3"

    it "preserves newlines inserted via breakLine" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "line1" ed
          ed'' = breakLine ed'
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
          ed6 = breakLine ed5  -- Insert newline via breakLine
          ed7 = insertChar 'w' ed6
          ed8 = insertChar 'o' ed7
          ed9 = insertChar 'r' ed8
          ed10 = insertChar 'l' ed9
          ed11 = insertChar 'd' ed10
      getEditorContent ed11 `shouldBe` "hello\nworld"
      -- Also verify the segments contain an actual newline CharSegment
      let allSegs = getEditorSegmentLines ed11
      length allSegs `shouldBe` 2  -- Should be 2 lines

    it "breakLine actually inserts CharSegment newline" $ do
      let ed = emptyEditor noWrapConfig
          ed' = insertText "hello" ed
          ed'' = breakLine ed'
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
      -- This should: delete backslash, insert newline
      let ed7 = deleteBackward ed6  -- Remove backslash
          ed8 = breakLine ed7        -- Insert newline
      -- Type "world"
      let ed9 = insertChar 'w' ed8
          ed10 = insertChar 'o' ed9
          ed11 = insertChar 'r' ed10
          ed12 = insertChar 'l' ed11
          ed13 = insertChar 'd' ed12
      -- Check content
      getEditorContent ed13 `shouldBe` "hello\nworld"
      length (getEditorLines ed13) `shouldBe` 2
