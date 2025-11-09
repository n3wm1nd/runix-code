{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStr)
import qualified System.IO as IO

import Polysemy
import Polysemy.Fail

import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter hiding (SystemPrompt)
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.HTTP.Effects ()
import Runix.Logging.Effects (Logging)
import Runix.Secret.Effects (runSecret)

import Agent (SystemPrompt(..), UserPrompt(..), runRunixCode, responseText)
import Models
import Config
import Runner

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code
--
-- Usage: echo "prompt" | runix-code [session-file]
--
-- Environment variables:
-- - RUNIX_MODEL: Model selection (claude-sonnet-45, glm-45-air, qwen3-coder)
-- - ANTHROPIC_OAUTH_TOKEN: Required for Claude models
-- - LLAMACPP_ENDPOINT: LlamaCpp server endpoint (default: http://localhost:8080/v1)
--
-- Reads prompt from stdin, runs the agent, displays response
-- If session-file is provided, loads history from it and saves updated history back
main :: IO ()
main = do
  -- Load configuration
  cfg <- loadConfig

  -- Read user input from stdin
  userInput <- TIO.getContents

  -- Check if we got any input
  if T.null (T.strip userInput)
    then do
      hPutStr IO.stderr "Error: No input provided. Usage: echo \"prompt\" | runix-code [session-file]\n"
      exitFailure
    else do
      -- Run with selected model
      result <- case cfgModelSelection cfg of
        UseClaudeSonnet45 -> runWithClaudeSonnet45 cfg userInput
        UseGLM45Air -> runWithGLM45Air cfg userInput
        UseQwen3Coder -> runWithQwen3Coder cfg userInput

      case result of
        Right response -> TIO.putStrLn response
        Left errMsg -> hPutStr IO.stderr errMsg >> exitFailure

--------------------------------------------------------------------------------
-- Model-Specific Runners (Just Interpreter Setup)
--------------------------------------------------------------------------------

-- | Run with Claude Sonnet 4.5
runWithClaudeSonnet45 :: Config -> Text -> IO (Either String Text)
runWithClaudeSonnet45 cfg userInput = do
  -- Get OAuth token
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case maybeToken of
    Nothing -> do
      hPutStr IO.stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      return $ Left "Missing ANTHROPIC_OAUTH_TOKEN"
    Just tokenStr ->
      runWithEffects $
        runSecret (pure tokenStr) $
          interpretAnthropicOAuth Anthropic ClaudeSonnet45 $
            runAgent @Anthropic @ClaudeSonnet45 cfg userInput

-- | Run with GLM4.5-air
runWithGLM45Air :: Config -> Text -> IO (Either String Text)
runWithGLM45Air cfg userInput =
  runWithEffects $
    interpretLlamaCpp (cfgLlamaCppEndpoint cfg) LlamaCpp GLM45Air $
      runAgent @LlamaCpp @GLM45Air cfg userInput

-- | Run with Qwen3-Coder
runWithQwen3Coder :: Config -> Text -> IO (Either String Text)
runWithQwen3Coder cfg userInput =
  runWithEffects $
    interpretLlamaCpp (cfgLlamaCppEndpoint cfg) LlamaCpp Qwen3Coder $
      runAgent @LlamaCpp @Qwen3Coder cfg userInput

--------------------------------------------------------------------------------
-- Generic Agent Runner (Model-Agnostic)
--------------------------------------------------------------------------------

-- | Run the agent with any model/provider
--
-- This is completely generic - no model-specific code here.
-- It just loads config, runs the agent, and saves the session.
runAgent :: forall provider model r.
            ( Member (LLM provider model) r
            , Members [FileSystemRead, FileSystemWrite] r
            , Member Grep r
            , Member Bash r
            , Member Logging r
            , Member Fail r
            , HasTools model provider
            , SupportsSystemPrompt provider
            , ProviderImplementation provider model
            )
         => Config
         -> Text
         -> Sem r Text
runAgent cfg userInput = do
  -- Load system prompt
  sysPromptText <- loadSystemPrompt
    "prompt/runix-code.md"
    "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."

  let sysPrompt = SystemPrompt sysPromptText
      userPrompt = UserPrompt userInput

  -- Load session (if specified)
  initialHistory <- case cfgSessionFile cfg of
    Nothing -> return []
    Just sessionFile -> loadSession @provider @model sessionFile

  -- Run the agent
  (result, finalHistory) <- runRunixCode @provider @model sysPrompt initialHistory userPrompt

  -- Save session (if specified)
  case cfgSessionFile cfg of
    Nothing -> return ()
    Just sessionFile -> saveSession @provider @model sessionFile finalHistory

  return $ responseText result
