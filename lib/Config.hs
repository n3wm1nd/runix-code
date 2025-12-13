{-# LANGUAGE GADTs #-}

-- | Configuration and model selection for runix-code
--
-- This module handles:
-- - CLI argument parsing
-- - Environment variable reading
-- - Model selection logic
-- - Configuration data types
--
-- The configuration is designed to be reusable across different interfaces
-- (CLI, TUI, API server) - each interface just needs to provide a Config.
module Config
  ( -- * Configuration Types
    Config(..)
  , ModelSelection(..)
    -- * Configuration Loading
  , loadConfig
  , getModelSelection
    -- * Helper Functions
  , getLlamaCppEndpoint
  , getOpenRouterApiKey
  , getOpenRouterModel
  , getZAIApiKey
  , getZAIEndpoint
  ) where

import System.Environment (getArgs, lookupEnv)
import qualified Data.Text as T
import System.IO (hPutStr)
import qualified System.IO as IO

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Model selection
data ModelSelection
  = UseClaudeSonnet45
  | UseGLM45Air
  | UseQwen3Coder
  | UseOpenRouter
  | UseGLM45AirZAI
  | UseGLM46ZAI
  deriving stock (Show, Eq)

-- | Application configuration
data Config = Config
  { cfgModelSelection :: ModelSelection
  , cfgSessionFile :: Maybe FilePath
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load configuration from CLI args and environment variables
--
-- Environment variables:
-- - RUNIX_MODEL: Model selection ("claude-sonnet-45", "glm-45-air", "qwen3-coder", "glm-45-air-zai", "glm-46-zai")
--
-- CLI arguments:
-- - First positional argument: session file path (optional)
loadConfig :: IO Config
loadConfig = do
  -- Get CLI arguments
  args <- getArgs
  let maybeSessionFile = case args of
        (file:_) -> Just file
        [] -> Nothing

  -- Get model selection from environment
  modelSelection <- getModelSelection

  return Config
    { cfgModelSelection = modelSelection
    , cfgSessionFile = maybeSessionFile
    }

-- | Get model selection from RUNIX_MODEL environment variable
--
-- Defaults to ClaudeSonnet45 if not set or invalid
getModelSelection :: IO ModelSelection
getModelSelection = do
  maybeModel <- lookupEnv "RUNIX_MODEL"
  case maybeModel of
    Nothing -> do
      hPutStr IO.stderr "info: RUNIX_MODEL not set, using claude-sonnet-45\n"
      return UseClaudeSonnet45
    Just modelStr -> case T.toLower (T.pack modelStr) of
      "claude-sonnet-45" -> return UseClaudeSonnet45
      "claude-sonnet-4-5" -> return UseClaudeSonnet45
      "claude" -> return UseClaudeSonnet45
      "glm-45-air" -> return UseGLM45Air
      "glm-4.5-air" -> return UseGLM45Air
      "glm45air" -> return UseGLM45Air
      "glm" -> return UseGLM45Air
      "qwen3-coder" -> return UseQwen3Coder
      "qwen3coder" -> return UseQwen3Coder
      "qwen" -> return UseQwen3Coder
      "openrouter" -> return UseOpenRouter
      "openrouter-universal" -> return UseOpenRouter
      "glm-45-air-zai" -> return UseGLM45AirZAI
      "glm-4.5-air-zai" -> return UseGLM45AirZAI
      "glm45airzai" -> return UseGLM45AirZAI
      "zai-glm45" -> return UseGLM45AirZAI
      "glm-46-zai" -> return UseGLM46ZAI
      "glm-4.6-zai" -> return UseGLM46ZAI
      "glm46zai" -> return UseGLM46ZAI
      "zai-glm46" -> return UseGLM46ZAI
      "zai" -> return UseGLM46ZAI  -- Default to latest GLM
      unknown -> do
        hPutStr IO.stderr $ "warn: Unknown model '" <> T.unpack unknown <> "', using claude-sonnet-45\n"
        return UseClaudeSonnet45

-- | Get LlamaCpp endpoint from environment
--
-- Defaults to http://localhost:8080/v1 if not set
getLlamaCppEndpoint :: IO String
getLlamaCppEndpoint = do
  maybeEndpoint <- lookupEnv "LLAMACPP_ENDPOINT"
  case maybeEndpoint of
    Nothing -> return "http://localhost:8080/v1"
    Just endpoint -> return endpoint

-- | Get OpenRouter API key from environment
--
-- Required when using OpenRouter models. Returns the OPENROUTER_API_KEY value
-- or throws an error if not set.
getOpenRouterApiKey :: IO String
getOpenRouterApiKey = do
  maybeKey <- lookupEnv "OPENROUTER_API_KEY"
  case maybeKey of
    Nothing -> do
      hPutStr IO.stderr "error: OPENROUTER_API_KEY environment variable not set\n"
      error "OPENROUTER_API_KEY is required for OpenRouter models"
    Just key -> return key

-- | Get OpenRouter model name from environment
--
-- Returns the OPENROUTER_MODEL value. Used to specify which model to use
-- when RUNIX_MODEL is set to 'openrouter'.
getOpenRouterModel :: IO String
getOpenRouterModel = do
  maybeModel <- lookupEnv "OPENROUTER_MODEL"
  case maybeModel of
    Nothing -> do
      hPutStr IO.stderr "error: OPENROUTER_MODEL environment variable not set\n"
      error "OPENROUTER_MODEL is required when using OpenRouter"
    Just model -> return model

-- | Get ZAI API key from environment
--
-- Required when using ZAI models. Returns the ZAI_API_KEY value
-- or throws an error if not set.
getZAIApiKey :: IO String
getZAIApiKey = do
  maybeKey <- lookupEnv "ZAI_API_KEY"
  case maybeKey of
    Nothing -> do
      hPutStr IO.stderr "error: ZAI_API_KEY environment variable not set\n"
      error "ZAI_API_KEY is required for ZAI models"
    Just key -> return key

-- | Get ZAI endpoint from environment
--
-- Defaults to https://api.z.ai/api/coding/paas/v4 if not set
getZAIEndpoint :: IO String
getZAIEndpoint = do
  maybeEndpoint <- lookupEnv "ZAI_ENDPOINT"
  case maybeEndpoint of
    Nothing -> return "https://api.z.ai/api/coding/paas/v4"
    Just endpoint -> return endpoint
