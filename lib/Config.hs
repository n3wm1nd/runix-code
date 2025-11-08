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
  ) where

import System.Environment (getArgs, lookupEnv)
import Data.Text (Text)
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
  deriving (Show, Eq)

-- | Application configuration
data Config = Config
  { cfgModelSelection :: ModelSelection
  , cfgSessionFile :: Maybe FilePath
  , cfgLlamaCppEndpoint :: String
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load configuration from CLI args and environment variables
--
-- Environment variables:
-- - RUNIX_MODEL: Model selection ("claude-sonnet-45", "glm-45-air", "qwen3-coder")
-- - LLAMACPP_ENDPOINT: LlamaCpp server endpoint (default: http://localhost:8080/v1)
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

  -- Get LlamaCpp endpoint from environment
  llamacppEndpoint <- getLlamaCppEndpoint

  return Config
    { cfgModelSelection = modelSelection
    , cfgSessionFile = maybeSessionFile
    , cfgLlamaCppEndpoint = llamacppEndpoint
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
