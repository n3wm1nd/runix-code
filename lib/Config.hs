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
  , RunixDataDir(..)
    -- * Filesystem Types (re-exported from Tools.Config)
  , ProjectFS(..)
  , ClaudeConfigFS(..)
  , RunixToolsFS(..)
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
import Runix.Tools.Config (ProjectFS(..), ClaudeConfigFS(..), RunixToolsFS(..))

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Model selection
data ModelSelection
  = UseClaudeSonnet45
  | UseClaudeHaiku45
  | UseClaudeOpus46
  | UseGLM45Air
  | UseMinimaxM25
  | UseQwen35
  | UseQwen3Coder
  | UseOpenRouter
  | UseGLM45AirZAI
  | UseGLM46ZAI
  | UseGLM47ZAI
  | UseGLM5ZAI
  deriving stock (Show, Eq)

-- | Path to the runix-code data directory (apps/runix-code during development)
-- This is where source files live (lib/GeneratedTools.hs, etc.), not the Cabal installation data directory
newtype RunixDataDir = RunixDataDir FilePath
  deriving stock (Show, Eq)

-- | Application configuration
data Config = Config
  { cfgModelSelection :: ModelSelection
  , cfgSessionFile :: Maybe FilePath
  , cfgResumeSession :: Maybe FilePath
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load configuration from CLI args and environment variables
--
-- Environment variables:
-- - RUNIX_MODEL: Model selection ("claude-sonnet-45", "claude-haiku-45", "claude-opus-46", "glm-45-air", "minimax-m25", "qwen3-coder", "glm-45-air-zai", "glm-46-zai", "glm-47-zai", "glm-5-zai")
--
-- CLI arguments:
-- - First positional argument: session file path (optional, legacy)
-- - --resume-session <path>: resume from saved session (for code reloading)
loadConfig :: IO Config
loadConfig = do
  -- Get CLI arguments
  args <- getArgs
  let (maybeSessionFile, maybeResumeSession) = parseArgs args

  -- Get model selection from environment
  modelSelection <- getModelSelection

  return Config
    { cfgModelSelection = modelSelection
    , cfgSessionFile = maybeSessionFile
    , cfgResumeSession = maybeResumeSession
    }

-- | Parse command-line arguments
parseArgs :: [String] -> (Maybe FilePath, Maybe FilePath)
parseArgs ("--resume-session":path:_) = (Nothing, Just path)
parseArgs (file:_) = (Just file, Nothing)  -- Backward compatibility
parseArgs [] = (Nothing, Nothing)

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
      "claude-haiku-45" -> return UseClaudeHaiku45
      "claude-haiku-4-5" -> return UseClaudeHaiku45
      "haiku" -> return UseClaudeHaiku45
      "claude-opus-46" -> return UseClaudeOpus46
      "claude-opus-4-6" -> return UseClaudeOpus46
      "opus" -> return UseClaudeOpus46
      "glm-45-air" -> return UseGLM45Air
      "glm-4.5-air" -> return UseGLM45Air
      "glm45air" -> return UseGLM45Air
      "glm" -> return UseGLM45Air
      "minimax-m25" -> return UseMinimaxM25
      "minimax-m2.5" -> return UseMinimaxM25
      "minimaxm25" -> return UseMinimaxM25
      "minimax" -> return UseMinimaxM25
      "qwen3.5" -> return UseQwen35
      "qwen3.5-122b" -> return UseQwen35
      "qwen35" -> return UseQwen35
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
      "glm-47-zai" -> return UseGLM47ZAI
      "glm-4.7-zai" -> return UseGLM47ZAI
      "glm47zai" -> return UseGLM47ZAI
      "zai-glm47" -> return UseGLM47ZAI
      "glm-5-zai" -> return UseGLM5ZAI
      "glm5-zai" -> return UseGLM5ZAI
      "glm5zai" -> return UseGLM5ZAI
      "zai-glm5" -> return UseGLM5ZAI
      "glm-5" -> return UseGLM5ZAI
      "glm5" -> return UseGLM5ZAI
      "zai" -> return UseGLM47ZAI  -- Default to latest generally-available GLM
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
