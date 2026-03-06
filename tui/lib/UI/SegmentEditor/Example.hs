{-# LANGUAGE OverloadedStrings #-}

-- | Example usage of SegmentEditor
--
-- This module demonstrates the key features and usage patterns.
module UI.SegmentEditor.Example where

import UI.SegmentEditor
import Brick.Types (BrickEvent(..))
import Graphics.Vty (Event(..), Key(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Example: Basic text editing
example1 :: IO ()
example1 = do
  let config = EditorConfig
        { editorName = ()
        , lineLimit = Nothing
        , newlineMode = EnterSends
        }
      ed0 = emptyEditor config

      -- Type "Hello"
      ed1 = insertChar 'H' ed0
      ed2 = insertChar 'e' ed1
      ed3 = insertChar 'l' ed2
      ed4 = insertChar 'l' ed3
      ed5 = insertChar 'o' ed4

  putStrLn $ "Content: " ++ show (getEditorContent ed5)
  putStrLn $ "Cursor: " ++ show (getCursorPos ed5)

  -- Move left, insert space, type "World"
  let ed6 = moveCursorToLineStart ed5
      ed7 = insertChar 'W' ed6
      ed8 = insertText "orld " ed7

  putStrLn $ "Content: " ++ show (getEditorContent ed8)
  -- Should be: "World Hello"

-- | Example: File reference handling
example2 :: IO ()
example2 = do
  let config = EditorConfig
        { editorName = ()
        , lineLimit = Nothing
        , newlineMode = EnterSends
        }
      ed0 = emptyEditor config

      -- Type "Check "
      ed1 = insertText "Check " ed0

      -- Insert file reference
      ed2 = insertFileRef "src/Main.hs" ["src/MainTest.hs", "app/Main.hs"] "Main" RefPending ed1

      -- Type " for details"
      ed3 = insertText " for details" ed2

  putStrLn $ "Content: " ++ show (getEditorContent ed3)
  -- Should be: "Check @src/Main.hs for details"

  putStrLn $ "Segment count: " ++ show (length $ head $ getEditorSegmentLines ed3)
  -- Should show multiple segments

  -- Navigate back and try to delete file ref
  let ed4 = moveCursorToLineEnd ed3
      ed5 = moveWordLeft ed4  -- Jump over "details"
      ed6 = moveWordLeft ed5  -- Jump over space and file ref
      ed7 = deleteForward ed6  -- Delete file ref as a unit

  putStrLn $ "After delete: " ++ show (getEditorContent ed7)
  -- Should be: "Check  for details" (file ref deleted)

-- | Example: Paste handling
example3 :: IO ()
example3 = do
  let config = EditorConfig
        { editorName = ()
        , lineLimit = Nothing
        , newlineMode = EnterSends
        }
      ed0 = emptyEditor config

      -- Type some text
      ed1 = insertText "Start: " ed0

      -- Simulate paste
      ed2 = insertSegment (PastedSegment "pasted content here") ed1

      -- Type more
      ed3 = insertText " :end" ed2

  putStrLn $ "Content: " ++ show (getEditorContent ed3)
  -- Should be: "Start: pasted content here :end"

  -- Delete the pasted segment as a unit
  let ed4 = moveCursorLeft ed3  -- Move before ":end"
      ed5 = moveCursorLeft ed4  -- Move before pasted segment
      ed6 = deleteForward ed5   -- Delete entire pasted segment

  putStrLn $ "After delete: " ++ show (getEditorContent ed6)
  -- Should be: "Start:  :end"

-- | Example: Multi-line editing
example4 :: IO ()
example4 = do
  let config = EditorConfig
        { editorName = ()
        , lineLimit = Just 10
        , newlineMode = EnterNewline  -- Enter creates new line
        }
      ed0 = emptyEditor config

      -- Type first line
      ed1 = insertText "Line 1" ed0
      ed2 = breakLine ed1

      -- Type second line
      ed3 = insertText "Line 2" ed2
      ed4 = breakLine ed3

      -- Type third line
      ed5 = insertText "Line 3" ed4

  putStrLn $ "Lines: " ++ show (getEditorLines ed5)
  putStrLn $ "Cursor: " ++ show (getCursorPos ed5)
  -- Cursor should be at (2, 6) - third line, after "Line 3"

  -- Move up and insert text
  let ed6 = moveCursorUp ed5
      ed7 = moveCursorToLineEnd ed6
      ed8 = insertText " [modified]" ed7

  putStrLn $ "Lines after edit: " ++ show (getEditorLines ed8)
  -- Should show "Line 2 [modified]" on second line

-- | Example: Word navigation
example5 :: IO ()
example5 = do
  let config = EditorConfig
        { editorName = ()
        , lineLimit = Nothing
        , newlineMode = EnterSends
        }
      ed0 = emptyEditor config

      -- Type sentence with file ref in middle
      ed1 = insertText "Please check " ed0
      ed2 = insertFileRef "README.md" [] "README" RefAccepted ed1
      ed3 = insertText " and " ed2
      ed4 = insertFileRef "CONTRIBUTING.md" [] "CONTRIB" RefAccepted ed3
      ed5 = insertText " files" ed4

  putStrLn $ "Content: " ++ show (getEditorContent ed5)

  -- Navigate by words from end
  let positions = iterate moveWordLeft ed5
      cursors = map getCursorPos (take 10 positions)

  putStrLn "Word navigation positions:"
  mapM_ (putStrLn . ("  " ++) . show) cursors

  -- Should jump over words and file refs as units

-- | Example: Backspace over different segment types
example6 :: IO ()
example6 = do
  let config = EditorConfig
        { editorName = ()
        , lineLimit = Nothing
        , newlineMode = EnterSends
        }
      ed0 = emptyEditor config

      -- Create content: char, file ref, pasted, char
      ed1 = insertChar 'X' ed0
      ed2 = insertFileRef "test.txt" [] "test" RefPending ed1
      ed3 = insertSegment (PastedSegment " pasted ") ed2
      ed4 = insertChar 'Y' ed3

  putStrLn $ "Initial: " ++ show (getEditorContent ed4)
  putStrLn $ "Segments: " ++ show (head $ getEditorSegmentLines ed4)

  -- Backspace deletes each segment atomically
  let ed5 = deleteBackward ed4  -- Delete 'Y'
      ed6 = deleteBackward ed5  -- Delete entire " pasted "
      ed7 = deleteBackward ed6  -- Delete entire file ref
      ed8 = deleteBackward ed7  -- Delete 'X'

  putStrLn $ "After 4 backspaces: " ++ show (getEditorContent ed8)
  putStrLn $ "Is empty: " ++ show (isEmpty ed8)
  -- Should be empty

-- | Example: Configurable newline modes
example7 :: IO ()
example7 = do
  -- Mode 1: Enter sends (Shift+Enter for newline)
  let config1 = EditorConfig () Nothing EnterSends
      ed1 = editorFromText config1 "test"
  -- In real UI, Enter key would signal submission

  -- Mode 2: Enter adds newline (submission via other means)
  let config2 = EditorConfig () Nothing EnterNewline
      ed2 = editorFromText config2 "test"
      ed3 = breakLine ed2
  putStrLn $ "EnterNewline mode: " ++ show (getEditorLines ed3)

  -- Mode 3: Backslash-escape mode
  let config3 = EditorConfig () Nothing BackslashEscape
      ed4 = editorFromText config3 "test\\"
      -- In real UI, Enter after backslash would delete it and insert newline
      ed5 = deleteBackward ed4  -- Simulate removing backslash
      ed6 = breakLine ed5       -- Insert newline
  putStrLn $ "BackslashEscape mode: " ++ show (getEditorLines ed6)

-- Run all examples
main :: IO ()
main = do
  putStrLn "=== Example 1: Basic text editing ==="
  example1
  putStrLn ""

  putStrLn "=== Example 2: File references ==="
  example2
  putStrLn ""

  putStrLn "=== Example 3: Paste handling ==="
  example3
  putStrLn ""

  putStrLn "=== Example 4: Multi-line editing ==="
  example4
  putStrLn ""

  putStrLn "=== Example 5: Word navigation ==="
  example5
  putStrLn ""

  putStrLn "=== Example 6: Backspace over segments ==="
  example6
  putStrLn ""

  putStrLn "=== Example 7: Newline modes ==="
  example7
