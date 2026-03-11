{-# LANGUAGE OverloadedStrings #-}

module SegmentEditorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import UI.SegmentEditor
import UI.Zipper (Zipper, Zippable(..), toList)

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

  describe "Cursor Left/Right Linebreak Skipping" $ do
    it "skips over newline when moving right" $ do
      -- "hello\nworld" - cursor at 'o' in hello, move right should skip \n and land on 'w'
      let ed = insertText "hello" (emptyEditor noWrapConfig)
          ed1 = typeChar '\n' ed
          ed2 = insertText "world" ed1
          -- Move to end of "hello" (before the \n)
          ed3 = moveCursorToStart ed2
          ed4 = moveCursorRight ed3  -- h
          ed5 = moveCursorRight ed4  -- e
          ed6 = moveCursorRight ed5  -- l
          ed7 = moveCursorRight ed6  -- l
          ed8 = moveCursorRight ed7  -- o
          -- Move right should skip the \n and go to 'w' in "world"
          ed9 = moveCursorRight ed8
      getCursorPos ed8 `shouldBe` (0, 5)  -- At 'o' in "hello"
      getCursorPos ed9 `shouldBe` (1, 0)  -- At 'w' in "world" (skipped \n)

    it "skips over newline when moving left" $ do
      -- "hello\nworld" - cursor at 'w' in world, move left should skip \n and land on 'o'
      let ed = insertText "hello" (emptyEditor noWrapConfig)
          ed1 = typeChar '\n' ed
          ed2 = insertText "world" ed1
          -- Move to start of "world"
          ed3 = moveCursorToLineStart ed2
          -- Move left should skip the \n and go to 'o' in "hello"
          ed4 = moveCursorLeft ed3
      getCursorPos ed3 `shouldBe` (1, 0)  -- At 'w' in "world"
      getCursorPos ed4 `shouldBe` (0, 5)  -- At 'o' in "hello" (skipped \n)

    it "handles consecutive linebreaks when moving right multiple times" $ do
      -- "a\n\nb" - need multiple moveRight calls to skip both linebreaks
      let ed = insertText "a" (emptyEditor noWrapConfig)
          ed1 = typeChar '\n' ed
          ed2 = typeChar '\n' ed1
          ed3 = insertText "b" ed2
          -- Move to start (before 'a')
          ed4 = moveCursorToStart ed3
          -- Move right - now after 'a', before first \n
          ed5 = moveCursorRight ed4
          -- Move right again - skips first \n, lands in empty line
          ed6 = moveCursorRight ed5
          -- Move right again - skips second \n, lands before 'b'
          ed7 = moveCursorRight ed6
      getCursorPos ed4 `shouldBe` (0, 0)  -- Before 'a'
      getCursorPos ed5 `shouldBe` (0, 1)  -- After 'a', before first \n
      getCursorPos ed6 `shouldBe` (1, 0)  -- In empty line (after skipping first \n)
      getCursorPos ed7 `shouldBe` (2, 0)  -- Before 'b' (after skipping second \n)

  describe "Cursor Up/Down Column Preservation" $ do
    it "preserves column when moving up with lines longer than column" $ do
      -- Create editor with three lines
      -- Lines have content: "hello\n", "world\n", "12345"
      -- Note: newlines count as segments/columns
      let ed = insertText "hello" (emptyEditor noWrapConfig)
          ed1 = typeChar '\n' ed
          ed2 = insertText "world" ed1
          ed3 = typeChar '\n' ed2
          ed4 = insertText "12345" ed3
          -- Now at end of "12345", move to column 3
          ed5 = moveCursorToLineStart ed4
          ed6 = moveCursorRight ed5  -- column 1 (character '1')
          ed7 = moveCursorRight ed6  -- column 2 (character '2')
          ed8 = moveCursorRight ed7  -- column 3 (character '3')
          ed9 = moveCursorUp ed8     -- Should be at column 3 in "world"
          ed10 = moveCursorUp ed9    -- Should be at column 3 in "hello"
      snd (getCursorPos ed8) `shouldBe` 3
      snd (getCursorPos ed9) `shouldBe` 3
      snd (getCursorPos ed10) `shouldBe` 3

    it "preserves column when moving down with lines longer than column" $ do
      -- Create editor starting at column 3 in first line, move down
      let ed = insertText "hello" (emptyEditor noWrapConfig)
          ed1 = typeChar '\n' ed
          ed2 = insertText "world" ed1
          ed3 = typeChar '\n' ed2
          ed4 = insertText "12345" ed3
          -- Move to start, then to column 3 in "hello"
          ed5 = moveCursorToStart ed4
          ed6 = moveCursorRight ed5  -- column 1 ('h')
          ed7 = moveCursorRight ed6  -- column 2 ('e')
          ed8 = moveCursorRight ed7  -- column 3 ('l')
          ed9 = moveCursorDown ed8   -- Should be at column 3 in "world" ('r')
          ed10 = moveCursorDown ed9  -- Should be at column 3 in "12345" ('3')
      snd (getCursorPos ed8) `shouldBe` 3
      snd (getCursorPos ed9) `shouldBe` 3
      snd (getCursorPos ed10) `shouldBe` 3

    it "stops at end of shorter line when moving up" $ do
      -- Lines: "hello", "hi", "world"
      -- Move to column 4 in "world", then up to "hi" (which is shorter)
      let ed = insertText "hello\nhi\nworld" (emptyEditor noWrapConfig)
          ed1 = moveCursorToLineStart ed
          ed2 = moveCursorRight ed1  -- column 1
          ed3 = moveCursorRight ed2  -- column 2
          ed4 = moveCursorRight ed3  -- column 3
          ed5 = moveCursorRight ed4  -- column 4 in "world"
          ed6 = moveCursorUp ed5     -- Should be at end of "hi" (column 2)
          ed7 = moveCursorUp ed6     -- Should be at column 2 in "hello"
      snd (getCursorPos ed5) `shouldBe` 4
      snd (getCursorPos ed6) `shouldBe` 2  -- Stops at end of "hi"
      snd (getCursorPos ed7) `shouldBe` 2  -- Remembers column 4, but "hello" long enough for column 2

    it "stops at end of shorter line when moving down" $ do
      -- Lines: "hello", "hi", "world"
      -- Start at column 4 in "hello", move down to "hi" (which is shorter)
      let ed = insertText "hello\nhi\nworld" (emptyEditor noWrapConfig)
          ed1 = moveCursorToStart ed
          ed2 = moveCursorRight ed1  -- column 1
          ed3 = moveCursorRight ed2  -- column 2
          ed4 = moveCursorRight ed3  -- column 3
          ed5 = moveCursorRight ed4  -- column 4 in "hello"
          ed6 = moveCursorDown ed5   -- Should be at end of "hi" (column 2)
          ed7 = moveCursorDown ed6   -- Should be at column 2 in "world"
      snd (getCursorPos ed5) `shouldBe` 4
      snd (getCursorPos ed6) `shouldBe` 2  -- Stops at end of "hi"
      snd (getCursorPos ed7) `shouldBe` 2  -- Remembers column 4, but moves to column 2

    it "preserves column when moving up then down" $ do
      -- Lines: "abcdef", "xyz", "123456"
      -- Start at column 4, move up to short line, then back down
      let ed = insertText "abcdef\nxyz\n123456" (emptyEditor noWrapConfig)
          ed1 = moveCursorToLineStart ed
          ed2 = foldr (const moveCursorRight) ed1 [1..4]  -- column 4 in "123456"
          ed3 = moveCursorUp ed2     -- column 3 in "xyz" (end of line)
          ed4 = moveCursorDown ed3   -- Should return to column 3 in "123456"
      snd (getCursorPos ed2) `shouldBe` 4
      snd (getCursorPos ed3) `shouldBe` 3  -- End of "xyz"
      snd (getCursorPos ed4) `shouldBe` 3  -- Not back to column 4, stays at column 3

  describe "Cursor and Insert Interaction" $ do
    it "debug: show gap state during insert" $ do
      let ed = insertText "1234" (emptyEditor noWrapConfig)
          segBefore = getSegmentBeforeCursor ed
          segAt = getSegmentAtCursor ed
          ed1 = moveCursorLeft ed
          segBefore1 = getSegmentBeforeCursor ed1
          segAt1 = getSegmentAtCursor ed1
          ed2 = insertChar 'X' ed1
          segBefore2 = getSegmentBeforeCursor ed2
          segAt2 = getSegmentAtCursor ed2
          content = getEditorContent ed2
      -- After insertText "1234", cursor is at end
      segBefore `shouldBe` Just (CharSegment '4')
      segAt `shouldBe` Nothing
      -- After moveCursorLeft, cursor is between '3' and '4'
      segBefore1 `shouldBe` Just (CharSegment '3')
      segAt1 `shouldBe` Just (CharSegment '4')
      -- After insertChar 'X', cursor should be after 'X', before '4'
      segBefore2 `shouldBe` Just (CharSegment 'X')
      segAt2 `shouldBe` Just (CharSegment '4')
      content `shouldBe` "123X4"

    it "inserting character after moving left keeps cursor position" $ do
      -- Reproduce bug: "1234" -> move left -> insert 'X' should give "123X4"
      let ed = insertText "1234" (emptyEditor noWrapConfig)
          -- ed is at end: "1234_"
          ed1 = moveCursorLeft ed
          -- ed1 should be: "123_4"
          col1 = snd (getCursorPos ed1)
          ed2 = insertChar 'X' ed1
          -- ed2 should be: "123X_4" (cursor after X)
          col2 = snd (getCursorPos ed2)
          content = getEditorContent ed2
      -- Verify positions
      col1 `shouldBe` 3  -- After moving left from end
      col2 `shouldBe` 4  -- After inserting 'X' (should be right after X)
      content `shouldBe` "123X4"

    it "multiple left moves and insert maintains position" $ do
      -- "12345" -> left left -> insert 'X' should give "123X45"
      let ed = insertText "12345" (emptyEditor noWrapConfig)
          ed1 = moveCursorLeft ed
          ed2 = moveCursorLeft ed1
          col_before = snd (getCursorPos ed2)
          ed3 = insertChar 'X' ed2
          col_after = snd (getCursorPos ed3)
          content = getEditorContent ed3
      col_before `shouldBe` 3  -- After two left moves
      col_after `shouldBe` 4   -- After inserting 'X'
      content `shouldBe` "123X45"

  describe "Forward/Back are inverses" $ do
    it "back then forward returns to same position" $ do
      let ed = insertText "hello world" (emptyEditor noWrapConfig)
          ed' = forward (back ed)
      getSegmentAtCursor ed `shouldBe` getSegmentAtCursor ed'

    it "forward then back returns to same position" $ do
      let ed = insertText "hello world" (emptyEditor noWrapConfig)
          edMoved = moveCursorLeft ed
          ed' = back (forward edMoved)
      getSegmentAtCursor edMoved `shouldBe` getSegmentAtCursor ed'

    it "5 backs then 5 forwards returns to same position" $ do
      let ed = insertText "hello world" (emptyEditor noWrapConfig)
          ed1 = back (back (back (back (back ed))))
          ed2 = forward (forward (forward (forward (forward ed1))))
      getSegmentAtCursor ed `shouldBe` getSegmentAtCursor ed2

    it "forward/back work across line boundaries" $ do
      let ed = insertText "hello\nworld" (emptyEditor noWrapConfig)
          -- At end: after 'd'
          ed1 = back ed  -- before 'd'
          ed2 = back ed1 -- before 'l'
          ed3 = back ed2 -- before 'l'
          ed4 = back ed3 -- before 'r'
          ed5 = back ed4 -- before 'o'
          ed6 = back ed5 -- before 'w'
          ed7 = back ed6 -- before '\n'
          ed8 = back ed7 -- before 'o'
      -- Now forward back
          ed9 = forward (forward (forward (forward (forward (forward (forward (forward ed8)))))))
      atEnd ed `shouldBe` True
      atEnd ed9 `shouldBe` True
      getSegmentAtCursor ed `shouldBe` getSegmentAtCursor ed9

  describe "Word Wrapping" $ do
    it "simple rewrap test with no actual wrapping" $ do
      let ed = insertText "abc" (emptyEditor noWrapConfig)
          -- Move to middle: "a|bc"
          edMid = back (back ed)
          -- Rewrap at large width (no actual wrapping should happen)
          rewrapped = rewrapEditor 100 edMid
      getSegmentAtCursor edMid `shouldBe` Just (CharSegment 'b')
      getSegmentAtCursor rewrapped `shouldBe` Just (CharSegment 'b')
      getEditorContent rewrapped `shouldBe` "abc"

    it "rewrapEditor preserves cursor position at start" $ do
      let ed = insertText "hello world" (emptyEditor noWrapConfig)
          edAtStart = moveCursorToStart ed
          rewrapped = rewrapEditor 5 edAtStart
      getCursorPos rewrapped `shouldBe` (0, 0)
      getEditorContent rewrapped `shouldBe` "hello world"

    it "rewrapEditor preserves cursor position in middle" $ do
      let ed = insertText "hello world" (emptyEditor noWrapConfig)
          edMoved = moveCursorLeft ed  -- One left from end: "hello worl|d"
          rewrapped = rewrapEditor 5 edMoved
      -- Content should be preserved
      getEditorContent rewrapped `shouldBe` "hello world"
      toList edMoved `shouldBe` toList rewrapped
      -- Cursor should still point to same segment
      getSegmentAtCursor rewrapped `shouldBe` Just (CharSegment 'd')

    it "rewrapEditor preserves cursor position at end" $ do
      let ed = insertText "hello world" (emptyEditor noWrapConfig)
          rewrapped = rewrapEditor 5 ed
      -- Cursor at end should remain at end
      atEnd rewrapped `shouldBe` True
      getEditorContent rewrapped `shouldBe` "hello world"

    it "rewrapEditor actually wraps text" $ do
      let ed = insertText "hello world foo" (emptyEditor noWrapConfig)
          rewrapped = rewrapEditor 7 ed
          lines' = getEditorLines rewrapped
      -- Should be multiple lines after wrapping at width 7
      length lines' `shouldSatisfy` (> 1)
      -- Content preserved
      getEditorContent rewrapped `shouldBe` "hello world foo"

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

-- | Check if a line (list) ends with a newline
lineEndsWithNewline :: [InputSegment] -> Bool
lineEndsWithNewline segments =
  case reverse segments of
    [] -> False  -- Empty line (shouldn't happen for non-current lines)
    (CharSegment '\n' : _) -> True
    _ -> False
