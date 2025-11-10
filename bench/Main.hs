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
  , RenderedMessage(..)
  , OutputHistory
  , renderMessage
  , patchOutputHistory
  , addStreamingChunk
  )
import UI.State (Name(..))
import UniversalLLM.Core.Types (Message(..))
import Brick.Types (Widget)

-- | NFData instance for OutputMessage (for benchmarking)
instance NFData OutputMessage where
  rnf (ConversationMessage idx txt) = idx `deepseq` txt `deepseq` ()
  rnf (LogEntry level txt) = level `seq` txt `deepseq` ()
  rnf (StreamingChunk txt) = txt `deepseq` ()
  rnf (SystemEvent txt) = txt `deepseq` ()
  rnf (ToolExecution txt) = txt `deepseq` ()

-- | NFData instance for RenderedMessage (for benchmarking)
instance NFData (RenderedMessage n) where
  rnf (RenderedMessage msg _ _) = rnf msg  -- Only force the message, widgets are lazy

--------------------------------------------------------------------------------
-- Benchmark Suite
--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  [ bgroup "quick-smoke-test"
      -- Just the essential benchmarks for quick feedback during optimization
      [ bench "markdown-simple" $ whnf markdownToWidgets simpleTextMedium
      , bench "markdown-with-code" $ whnf markdownToWidgets codeBlockHaskell
      , bench "render-message" $ nf renderMessage (ConversationMessage 0 simpleTextMedium)
      , bench "add-streaming-chunk" $ nf (addStreamingChunk "chunk") []
      , bench "add-to-history-50" $ nf (addStreamingChunk "chunk") (generateHistory 50)
      , bench "patch-history-50" $ nf (patchHistoryBench 50) 51
      ]
  ]

--------------------------------------------------------------------------------
-- Benchmark Helpers
--------------------------------------------------------------------------------

-- | Generate a rendered history of N messages
generateHistory :: Int -> OutputHistory Name
generateHistory n = map renderMessage
  [ ConversationMessage i ("Message " <> T.pack (show i))
  | i <- [n-1, n-2 .. 0]  -- Newest first
  ]

-- | Benchmark patching with different history sizes
patchHistoryBench :: Int -> Int -> OutputHistory Name
patchHistoryBench oldSize newSize =
  let oldMessages = generateRawMessages (oldSize `div` 2)
      oldOutput = patchOutputHistory oldMessages messageToDisplay []
      newMessages = generateRawMessages (newSize `div` 2)
  in patchOutputHistory newMessages messageToDisplay oldOutput

-- | Generate raw message list for patching
generateRawMessages :: Int -> [Message model provider]
generateRawMessages n =
  concat [[UserText ("User " <> T.pack (show i)),
           AssistantText ("Agent " <> T.pack (show i))]
         | i <- [1..n]]


--------------------------------------------------------------------------------
-- Sample Markdown Content
--------------------------------------------------------------------------------

simpleTextMedium :: Text
simpleTextMedium = T.unlines
  [ "This is a longer paragraph that spans multiple lines."
  , "It contains several sentences to test wrapping behavior."
  , "The text should flow naturally when rendered."
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

-- | Simple message to display converter (for benchmarking patchOutputHistory)
messageToDisplay :: Message model provider -> Text
messageToDisplay (UserText txt) = "You:\n  " <> txt
messageToDisplay (AssistantText txt) = "Agent:\n  " <> txt
messageToDisplay _ = "Other message type"
