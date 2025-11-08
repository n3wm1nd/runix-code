-- | TUI entry point for runix-code
--
-- This module handles:
-- - Configuration loading
-- - Model/interpreter setup
-- - Wiring the agent to the UI
module Main (main) where

import qualified Data.Text as T
import Data.IORef

import UniversalLLM.Core.Types (Message(..))
import UniversalLLM.Providers.Anthropic (Anthropic)
import Config
import TUI.UI (runUI)
import Models

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code TUI
main :: IO ()
main = do
  -- Load configuration (not used yet in echo mode)
  _cfg <- loadConfig

  -- For now, skip session loading
  let initialHistory = []

  -- Run with placeholder echo agent (model-agnostic for testing)
  runWithEchoAgent initialHistory

--------------------------------------------------------------------------------
-- Placeholder Echo Agent
--------------------------------------------------------------------------------

-- | Run TUI with a placeholder echo agent
--
-- The echo agent doesn't actually call the LLM - it just echoes the user input
-- as an AssistantText message. This is useful for testing the UI without
-- needing API keys or running models.
--
-- We use a specific model type here (ClaudeSonnet45/Anthropic) just for type
-- checking, but the agent doesn't actually use it.
runWithEchoAgent :: [Message ClaudeSonnet45 Anthropic] -> IO ()
runWithEchoAgent initialHistory = do
  -- Create a mutable reference to hold the message history
  historyRef <- newIORef initialHistory

  -- Define the echo agent callback
  let echoAgent :: String -> IO [Message ClaudeSonnet45 Anthropic]
      echoAgent userInput = do
        -- Get current history
        currentHistory <- readIORef historyRef

        -- Create new messages: user input + agent echo
        let userMsg = UserText (T.pack userInput)
            agentMsg = AssistantText (T.pack $ "Echo: " ++ userInput)
            newHistory = currentHistory ++ [userMsg, agentMsg]

        -- Update history reference
        writeIORef historyRef newHistory

        -- Return updated history
        return newHistory

  -- Start the UI
  runUI initialHistory echoAgent
