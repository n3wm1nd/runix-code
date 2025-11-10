{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark suite for runix-code TUI performance
--
-- This benchmarks the markdown rendering and UI update path to identify
-- performance bottlenecks when streaming with large conversation histories.
module Main (main) where

import Criterion.Main
import qualified Data.Text as T
import Data.Text (Text)
import Control.DeepSeq (NFData(..), deepseq)

import UI.Rendering (markdownToWidgets, markdownToWidgetsWithIndent)
import UI.OutputHistory
  ( OutputMessage(..)
  , renderOutputMessage
  , renderOutputMessages
  , patchOutputHistory
  , addStreamingChunk
  , removeStreamingChunks
  )
import UniversalLLM.Core.Types (Message(..))
import Brick.Types (Widget)
import qualified Graphics.Vty as V

-- | NFData instance for OutputMessage (for benchmarking)
instance NFData OutputMessage where
  rnf (ConversationMessage idx txt) = idx `deepseq` txt `deepseq` ()
  rnf (LogEntry level txt) = level `seq` txt `deepseq` ()
  rnf (StreamingChunk txt) = txt `deepseq` ()
  rnf (SystemEvent txt) = txt `deepseq` ()
  rnf (ToolExecution txt) = txt `deepseq` ()

--------------------------------------------------------------------------------
-- Sample Data Generators
--------------------------------------------------------------------------------

-- | Generate a simple message history of a given size
generateHistory :: Int -> [Message model provider]
generateHistory n =
  concat [[UserText ("User message " <> T.pack (show i)),
           AssistantText ("Assistant response " <> T.pack (show i) <> ". This is a longer response with multiple sentences. " <>
                         "It includes some technical details and explanations. " <>
                         "The response continues for several lines to simulate real usage.")]
         | i <- [1..n]]

-- | Generate a markdown-rich message history
generateMarkdownHistory :: Int -> [Message model provider]
generateMarkdownHistory n =
  concat [[UserText ("User question " <> T.pack (show i)),
           AssistantText (markdownMessage i)]
         | i <- [1..n]]
  where
    markdownMessage i = T.unlines
      [ "Agent: Here's the response to question " <> T.pack (show i)
      , ""
      , "# Section 1"
      , ""
      , "This is a **bold** statement with some *italic* text."
      , ""
      , "## Subsection 1.1"
      , ""
      , "Here's a code example:"
      , ""
      , "```haskell"
      , "fibonacci :: Int -> Int"
      , "fibonacci 0 = 0"
      , "fibonacci 1 = 1"
      , "fibonacci n = fibonacci (n-1) + fibonacci (n-2)"
      , "```"
      , ""
      , "### Details"
      , ""
      , "Some more details with `inline code` and [links](https://example.com)."
      , ""
      , "- Bullet point 1"
      , "- Bullet point 2"
      , "- Bullet point 3"
      ]

-- | Generate a conversation history with code blocks
generateCodeHeavyHistory :: Int -> [Message model provider]
generateCodeHeavyHistory n =
  concat [[UserText ("How do I implement feature " <> T.pack (show i) <> "?"),
           AssistantText (codeMessage i)]
         | i <- [1..n]]
  where
    codeMessage i = T.unlines
      [ "Agent: Here's how to implement feature " <> T.pack (show i)
      , ""
      , "```python"
      , "def feature_" <> T.pack (show i) <> "(data):"
      , "    # Process the input data"
      , "    result = []"
      , "    for item in data:"
      , "        if item.is_valid():"
      , "            processed = process_item(item)"
      , "            result.append(processed)"
      , "    return result"
      , ""
      , "def process_item(item):"
      , "    # Transform the item"
      , "    return {"
      , "        'id': item.id,"
      , "        'value': item.value * 2,"
      , "        'timestamp': time.now()"
      , "    }"
      , "```"
      , ""
      , "Make sure to handle edge cases!"
      ]

-- | Simple message to display converter (for benchmarking patchOutputHistory)
messageToDisplay :: Message model provider -> Text
messageToDisplay (UserText txt) = "You:\n  " <> txt
messageToDisplay (AssistantText txt) = "Agent:\n  " <> txt
messageToDisplay _ = "Other message type"

--------------------------------------------------------------------------------
-- Benchmark Suite
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "quick-smoke-test"
      -- Just the essential benchmarks for quick feedback during optimization
      [ bench "markdown-simple" $ whnf markdownToWidgets simpleTextMedium
      , bench "markdown-with-code" $ whnf markdownToWidgets codeBlockHaskell
      , bench "patch-history-50" $ nf (patchHistoryBench 50) 51
      , bench "full-cycle-50" $ whnf fullUpdateCycleMarkdownBench 50
      ]
  ]

-- | Full comprehensive benchmark suite (takes ~2 minutes)
-- Run with: cabal bench runix-code-bench --benchmark-options="-m pattern full"
fullBenchmarks :: IO ()
fullBenchmarks = defaultMain
  [ bgroup "markdown-rendering"
      [ bench "simple-text-small" $ whnf markdownToWidgets simpleTextSmall
      , bench "simple-text-medium" $ whnf markdownToWidgets simpleTextMedium
      , bench "simple-text-large" $ whnf markdownToWidgets simpleTextLarge
      , bench "formatted-text" $ whnf markdownToWidgets formattedText
      , bench "code-block-python" $ whnf markdownToWidgets codeBlockPython
      , bench "code-block-haskell" $ whnf markdownToWidgets codeBlockHaskell
      , bench "nested-sections" $ whnf markdownToWidgets nestedSections
      , bench "with-indent-0" $ whnf (markdownToWidgetsWithIndent 0) formattedText
      , bench "with-indent-1" $ whnf (markdownToWidgetsWithIndent 1) formattedText
      , bench "with-indent-4" $ whnf (markdownToWidgetsWithIndent 4) formattedText
      ]
  , bgroup "output-history"
      [ bench "render-10-messages" $ whnf (renderOutputMessages renderOutputMessage) (toOutputMessages 10)
      , bench "render-50-messages" $ whnf (renderOutputMessages renderOutputMessage) (toOutputMessages 50)
      , bench "render-100-messages" $ whnf (renderOutputMessages renderOutputMessage) (toOutputMessages 100)
      , bench "render-10-markdown-rich" $ whnf (renderOutputMessages renderOutputMessage) (toOutputMessagesMarkdown 10)
      , bench "render-50-markdown-rich" $ whnf (renderOutputMessages renderOutputMessage) (toOutputMessagesMarkdown 50)
      , bench "render-100-markdown-rich" $ whnf (renderOutputMessages renderOutputMessage) (toOutputMessagesMarkdown 100)
      ]
  , bgroup "history-patching"
      [ bench "patch-no-change-10" $ nf (patchHistoryBench 10) 10
      , bench "patch-no-change-50" $ nf (patchHistoryBench 50) 50
      , bench "patch-no-change-100" $ nf (patchHistoryBench 100) 100
      , bench "patch-add-one-10" $ nf (patchHistoryBench 10) 11
      , bench "patch-add-one-50" $ nf (patchHistoryBench 50) 51
      , bench "patch-add-one-100" $ nf (patchHistoryBench 100) 101
      , bench "patch-rebuild-all-10" $ nf (patchHistoryBench 0) 10
      , bench "patch-rebuild-all-50" $ nf (patchHistoryBench 0) 50
      , bench "patch-rebuild-all-100" $ nf (patchHistoryBench 0) 100
      ]
  , bgroup "streaming-updates"
      [ bench "add-chunk-to-empty" $ nf (addStreamingChunk "chunk") []
      , bench "add-chunk-to-small-history" $ nf (addStreamingChunk "chunk") (toOutputMessages 10)
      , bench "add-chunk-to-existing-chunk" $ nf (addStreamingChunk "more") [StreamingChunk "initial"]
      , bench "remove-chunks-small" $ nf removeStreamingChunks (toOutputMessagesWithStreaming 10 5)
      , bench "remove-chunks-large" $ nf removeStreamingChunks (toOutputMessagesWithStreaming 100 50)
      ]
  , bgroup "full-update-cycle"
      [ bench "full-cycle-10-messages" $ whnf fullUpdateCycleBench 10
      , bench "full-cycle-50-messages" $ whnf fullUpdateCycleBench 50
      , bench "full-cycle-100-messages" $ whnf fullUpdateCycleBench 100
      , bench "full-cycle-markdown-10" $ whnf fullUpdateCycleMarkdownBench 10
      , bench "full-cycle-markdown-50" $ whnf fullUpdateCycleMarkdownBench 50
      , bench "full-cycle-markdown-100" $ whnf fullUpdateCycleMarkdownBench 100
      ]
  ]

--------------------------------------------------------------------------------
-- Sample Markdown Content
--------------------------------------------------------------------------------

simpleTextSmall :: Text
simpleTextSmall = "This is a simple paragraph with no formatting."

simpleTextMedium :: Text
simpleTextMedium = T.unlines
  [ "This is a longer paragraph that spans multiple lines."
  , "It contains several sentences to test wrapping behavior."
  , "The text should flow naturally when rendered."
  ]

simpleTextLarge :: Text
simpleTextLarge = T.unlines
  [ "This is an even longer block of text that simulates a typical response."
  , "It includes multiple paragraphs and several sentences per paragraph."
  , "The goal is to test how well the rendering handles larger amounts of text."
  , ""
  , "This second paragraph continues the response with more details."
  , "We want to see how the performance scales with content size."
  , "Each additional line adds to the complexity of the rendering task."
  ]

formattedText :: Text
formattedText = T.unlines
  [ "This paragraph contains **bold text**, *italic text*, and `inline code`."
  , "It also has [links](https://example.com) and ~~strikethrough~~ text."
  , "The formatting requires additional processing during rendering."
  ]

codeBlockPython :: Text
codeBlockPython = T.unlines
  [ "Here's a Python function:"
  , ""
  , "```python"
  , "def hello_world():"
  , "    print('Hello, world!')"
  , "    return 42"
  , "```"
  ]

codeBlockHaskell :: Text
codeBlockHaskell = T.unlines
  [ "Here's a Haskell function:"
  , ""
  , "```haskell"
  , "fibonacci :: Int -> Int"
  , "fibonacci 0 = 0"
  , "fibonacci 1 = 1"
  , "fibonacci n = fibonacci (n-1) + fibonacci (n-2)"
  , "```"
  ]

nestedSections :: Text
nestedSections = T.unlines
  [ "# Main Section"
  , ""
  , "Introduction paragraph."
  , ""
  , "## Subsection A"
  , ""
  , "Details about subsection A."
  , ""
  , "### Sub-subsection A.1"
  , ""
  , "Even more nested content."
  , ""
  , "## Subsection B"
  , ""
  , "Details about subsection B."
  ]

--------------------------------------------------------------------------------
-- Helper Functions for Benchmarks
--------------------------------------------------------------------------------

-- | Convert message history to output messages
toOutputMessages :: Int -> [OutputMessage]
toOutputMessages n =
  let history = generateHistory (n `div` 2)
  in patchOutputHistory history messageToDisplay []

-- | Convert markdown-rich message history to output messages
toOutputMessagesMarkdown :: Int -> [OutputMessage]
toOutputMessagesMarkdown n =
  let history = generateMarkdownHistory (n `div` 2)
  in patchOutputHistory history messageToDisplay []

-- | Create output messages with streaming chunks interspersed
toOutputMessagesWithStreaming :: Int -> Int -> [OutputMessage]
toOutputMessagesWithStreaming nMessages nChunks =
  let messages = toOutputMessages nMessages
      chunks = replicate nChunks (StreamingChunk "streaming text chunk...")
  in messages ++ chunks

-- | Benchmark patching with different history sizes
patchHistoryBench :: Int -> Int -> [OutputMessage]
patchHistoryBench oldSize newSize =
  let oldHistory = generateHistory (oldSize `div` 2)
      oldOutput = patchOutputHistory oldHistory messageToDisplay []
      newHistory = generateHistory (newSize `div` 2)
  in patchOutputHistory newHistory messageToDisplay oldOutput

-- | Benchmark a full update cycle: patch history + render all messages
fullUpdateCycleBench :: Int -> [Widget ()]
fullUpdateCycleBench n =
  let history = generateHistory (n `div` 2)
      output = patchOutputHistory history messageToDisplay []
  in renderOutputMessages renderOutputMessage output

-- | Full update cycle with markdown-rich content
fullUpdateCycleMarkdownBench :: Int -> [Widget ()]
fullUpdateCycleMarkdownBench n =
  let history = generateMarkdownHistory (n `div` 2)
      output = patchOutputHistory history messageToDisplay []
  in renderOutputMessages renderOutputMessage output
