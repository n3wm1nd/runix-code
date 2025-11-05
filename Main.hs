module Main (main) where

import qualified Data.Text.IO as TIO

-- | Main entry point for runix-code
--
-- Currently a minimal scaffold that builds successfully.
-- Will be expanded to implement the agent loop in Phase 1.
main :: IO ()
main = do
  TIO.putStrLn "Runix Code v0.1.0"
  TIO.putStrLn "Agent scaffold - not yet functional"

  -- TODO Phase 1:
  -- - Load system prompt from prompts/system.md
  -- - Parse command-line arguments
  -- - Run agent loop
  -- - Display response
