{-# LANGUAGE LambdaCase #-}

-- | Configuration and model selection for runix-code
--
-- This module handles:
-- - CLI argument parsing
-- - Environment variable reading
-- - Model identity resolution
-- - Configuration data types
--
-- The configuration is designed to be reusable across different interfaces
-- (CLI, TUI, API server) - each interface just needs to provide a Config.
module Config
  ( -- * Configuration Types
    Config(..)
  , ModelId(..)
  , RunixDataDir(..)
    -- * Filesystem Types (re-exported from Tools.Config)
  , ProjectFS(..)
  , ClaudeConfigFS(..)
  , RunixToolsFS(..)
  , RequestLogFS(..)
    -- * Configuration Loading
  , loadConfig
    -- * Model Resolution
  , resolveModelId
  , modelDisplayName
  ) where

import System.Environment (getArgs, lookupEnv)
import qualified Data.Text as T
import System.IO (hPutStr)
import qualified System.IO as IO
import Runix.Tools.Config (ProjectFS(..), ClaudeConfigFS(..), RunixToolsFS(..))
import Runix.FileSystem (HasProjectPath(..))

-- | Request logging filesystem - writes HTTP request/response logs
newtype RequestLogFS = RequestLogFS FilePath
  deriving stock (Show, Eq)

instance HasProjectPath RequestLogFS where
  getProjectPath (RequestLogFS path) = path

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Identifies a specific model + provider combination.
-- Each constructor corresponds to a concrete entry in 'buildAvailableModels'.
data ModelId
  -- Anthropic (OAuth)
  = ClaudeSonnet45
  | ClaudeHaiku45
  | ClaudeOpus46
  -- LlamaCpp
  | GLM45AirLlamaCpp
  | MinimaxM25LlamaCpp
  | Qwen35LlamaCpp
  | Qwen3CoderLlamaCpp
  -- ZAI
  | GLM45AirZAI
  | GLM46ZAI
  | GLM47ZAI
  | GLM5ZAI
  | GLM51ZAI
  | GLM5TurboZAI
  -- AlibabaCloud
  | MinimaxM25AlibabaCloud
  | KimiK25AlibabaCloud
  | Qwen35PlusAlibabaCloud
  | GLM5AlibabaCloud
  -- OpenRouter
  | OpenRouterModel
  deriving stock (Show, Eq)

-- | Display name for a ModelId (used in UI and error messages)
modelDisplayName :: ModelId -> T.Text
modelDisplayName = \case
  ClaudeSonnet45           -> "Claude Sonnet 4.5"
  ClaudeHaiku45            -> "Claude Haiku 4.5"
  ClaudeOpus46             -> "Claude Opus 4.6"
  GLM45AirLlamaCpp         -> "GLM 4.5 Air (LlamaCpp)"
  MinimaxM25LlamaCpp       -> "MiniMax M2.5 (LlamaCpp)"
  Qwen35LlamaCpp           -> "Qwen 3.5 122B (LlamaCpp)"
  Qwen3CoderLlamaCpp       -> "Qwen3 Coder (LlamaCpp)"
  GLM45AirZAI              -> "GLM 4.5 Air (ZAI)"
  GLM46ZAI                 -> "GLM 4.6 (ZAI)"
  GLM47ZAI                 -> "GLM 4.7 (ZAI)"
  GLM5ZAI                  -> "GLM 5 (ZAI)"
  GLM51ZAI                 -> "GLM 5.1 (ZAI)"
  GLM5TurboZAI             -> "GLM 5 Turbo (ZAI)"
  MinimaxM25AlibabaCloud   -> "MiniMax M2.5 (AlibabaCloud)"
  KimiK25AlibabaCloud      -> "Kimi K2.5 (AlibabaCloud)"
  Qwen35PlusAlibabaCloud   -> "Qwen 3.5 Plus (AlibabaCloud)"
  GLM5AlibabaCloud         -> "GLM 5 (AlibabaCloud)"
  OpenRouterModel          -> "OpenRouter"

-- | Path to the runix-code data directory (apps/runix-code during development)
-- This is where source files live (lib/GeneratedTools.hs, etc.), not the Cabal installation data directory
newtype RunixDataDir = RunixDataDir FilePath
  deriving stock (Show, Eq)

-- | Application configuration
data Config = Config
  { cfgModelId :: Maybe ModelId      -- ^ Selected model (Nothing = use first available)
  , cfgSessionFile :: Maybe FilePath
  , cfgResumeSession :: Maybe FilePath
  } deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Configuration Loading
--------------------------------------------------------------------------------

-- | Load configuration from CLI args and environment variables
--
-- Environment variables:
-- - RUNIX_MODEL: Model selection (format: provider/model or provider/variant/model)
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
  modelId <- getModelId

  return Config
    { cfgModelId = modelId
    , cfgSessionFile = maybeSessionFile
    , cfgResumeSession = maybeResumeSession
    }

-- | Parse command-line arguments
parseArgs :: [String] -> (Maybe FilePath, Maybe FilePath)
parseArgs ("--resume-session":path:_) = (Nothing, Just path)
parseArgs (file:_) = (Just file, Nothing)  -- Backward compatibility
parseArgs [] = (Nothing, Nothing)

-- | Resolve a user-provided model string to a 'ModelId'.
-- Returns 'Nothing' for unrecognized strings.
--
-- New format: "provider/model" (e.g., "llamacpp/glm45-air", "zai/glm-4.7", "anthropic/sonnet")
-- Legacy formats still supported for backwards compatibility
resolveModelId :: T.Text -> Maybe ModelId
resolveModelId input =
  let normalized = T.toLower . T.strip $ input
  in case T.split (=='/') normalized of
    -- New provider/model format
    ["anthropic", model] -> resolveAnthropicModel model
    ["llamacpp", model] -> resolveLlamaCppModel model
    ["zai", model] -> resolveZAIModel model
    ["alibabacloud", model] -> resolveAlibabaCloudModel model
    ["openrouter"] -> Just OpenRouterModel
    ["openrouter", _] -> Just OpenRouterModel  -- provider/variant/model for future

    -- Legacy single-word aliases (backwards compatibility)
    [single] -> resolveLegacyAlias single

    _ -> Nothing

-- Provider-specific model resolvers
resolveAnthropicModel :: T.Text -> Maybe ModelId
resolveAnthropicModel = \case
  "sonnet" -> Just ClaudeSonnet45
  "sonnet-4.5" -> Just ClaudeSonnet45
  "claude-sonnet-4.5" -> Just ClaudeSonnet45
  "haiku" -> Just ClaudeHaiku45
  "haiku-4.5" -> Just ClaudeHaiku45
  "claude-haiku-4.5" -> Just ClaudeHaiku45
  "opus" -> Just ClaudeOpus46
  "opus-4.6" -> Just ClaudeOpus46
  "claude-opus-4.6" -> Just ClaudeOpus46
  _ -> Nothing

resolveLlamaCppModel :: T.Text -> Maybe ModelId
resolveLlamaCppModel = \case
  "glm45-air" -> Just GLM45AirLlamaCpp
  "glm-45-air" -> Just GLM45AirLlamaCpp
  "glm-4.5-air" -> Just GLM45AirLlamaCpp
  "minimax" -> Just MinimaxM25LlamaCpp
  "minimax-m25" -> Just MinimaxM25LlamaCpp
  "minimax-m2.5" -> Just MinimaxM25LlamaCpp
  "qwen35" -> Just Qwen35LlamaCpp
  "qwen-3.5" -> Just Qwen35LlamaCpp
  "qwen3.5-122b" -> Just Qwen35LlamaCpp
  "qwen3-coder" -> Just Qwen3CoderLlamaCpp
  "qwen-3-coder" -> Just Qwen3CoderLlamaCpp
  _ -> Nothing

resolveZAIModel :: T.Text -> Maybe ModelId
resolveZAIModel = \case
  "glm45-air" -> Just GLM45AirZAI
  "glm-45-air" -> Just GLM45AirZAI
  "glm-4.5-air" -> Just GLM45AirZAI
  "glm46" -> Just GLM46ZAI
  "glm-46" -> Just GLM46ZAI
  "glm-4.6" -> Just GLM46ZAI
  "glm47" -> Just GLM47ZAI
  "glm-47" -> Just GLM47ZAI
  "glm-4.7" -> Just GLM47ZAI
  "glm5" -> Just GLM5ZAI
  "glm-5" -> Just GLM5ZAI
  "glm51" -> Just GLM51ZAI
  "glm-51" -> Just GLM51ZAI
  "glm-5.1" -> Just GLM51ZAI
  "glm5turbo" -> Just GLM5TurboZAI
  "glm-5-turbo" -> Just GLM5TurboZAI
  "glm-5turbo" -> Just GLM5TurboZAI
  _ -> Nothing

resolveAlibabaCloudModel :: T.Text -> Maybe ModelId
resolveAlibabaCloudModel = \case
  "minimax" -> Just MinimaxM25AlibabaCloud
  "minimax-m25" -> Just MinimaxM25AlibabaCloud
  "minimax-m2.5" -> Just MinimaxM25AlibabaCloud
  "kimi" -> Just KimiK25AlibabaCloud
  "kimi-k25" -> Just KimiK25AlibabaCloud
  "kimi-k2.5" -> Just KimiK25AlibabaCloud
  "qwen35-plus" -> Just Qwen35PlusAlibabaCloud
  "qwen-3.5-plus" -> Just Qwen35PlusAlibabaCloud
  "glm5" -> Just GLM5AlibabaCloud
  "glm-5" -> Just GLM5AlibabaCloud
  _ -> Nothing

-- Legacy single-word aliases for backwards compatibility
resolveLegacyAlias :: T.Text -> Maybe ModelId
resolveLegacyAlias = \case
  -- Anthropic
  "claude-sonnet-45"  -> Just ClaudeSonnet45
  "claude-sonnet-4-5" -> Just ClaudeSonnet45
  "claude"            -> Just ClaudeSonnet45
  "claude-haiku-45"   -> Just ClaudeHaiku45
  "claude-haiku-4-5"  -> Just ClaudeHaiku45
  "haiku"             -> Just ClaudeHaiku45
  "claude-opus-46"    -> Just ClaudeOpus46
  "claude-opus-4-6"   -> Just ClaudeOpus46
  "opus"              -> Just ClaudeOpus46
  -- LlamaCpp
  "glm-45-air"   -> Just GLM45AirLlamaCpp
  "glm-4.5-air"  -> Just GLM45AirLlamaCpp
  "glm45air"     -> Just GLM45AirLlamaCpp
  "glm"          -> Just GLM45AirLlamaCpp
  "minimax-m25"  -> Just MinimaxM25LlamaCpp
  "minimax-m2.5" -> Just MinimaxM25LlamaCpp
  "minimaxm25"   -> Just MinimaxM25LlamaCpp
  "minimax"      -> Just MinimaxM25LlamaCpp
  "qwen3.5"      -> Just Qwen35LlamaCpp
  "qwen3.5-122b" -> Just Qwen35LlamaCpp
  "qwen35"       -> Just Qwen35LlamaCpp
  "qwen3-coder"  -> Just Qwen3CoderLlamaCpp
  "qwen3coder"   -> Just Qwen3CoderLlamaCpp
  "qwen"         -> Just Qwen3CoderLlamaCpp
  -- ZAI
  "glm-45-air-zai"  -> Just GLM45AirZAI
  "glm-4.5-air-zai" -> Just GLM45AirZAI
  "glm45airzai"     -> Just GLM45AirZAI
  "zai-glm45"       -> Just GLM45AirZAI
  "glm-46-zai"      -> Just GLM46ZAI
  "glm-4.6-zai"     -> Just GLM46ZAI
  "glm46zai"        -> Just GLM46ZAI
  "zai-glm46"       -> Just GLM46ZAI
  "glm-47-zai"      -> Just GLM47ZAI
  "glm-4.7-zai"     -> Just GLM47ZAI
  "glm47zai"        -> Just GLM47ZAI
  "zai-glm47"       -> Just GLM47ZAI
  "glm-5-zai"       -> Just GLM5ZAI
  "glm5-zai"        -> Just GLM5ZAI
  "glm5zai"         -> Just GLM5ZAI
  "zai-glm5"        -> Just GLM5ZAI
  "glm-5"           -> Just GLM5ZAI
  "glm5"            -> Just GLM5ZAI
  "glm-5.1-zai"     -> Just GLM51ZAI
  "glm51-zai"       -> Just GLM51ZAI
  "glm51zai"        -> Just GLM51ZAI
  "zai-glm51"       -> Just GLM51ZAI
  "glm-5.1"         -> Just GLM51ZAI
  "glm51"           -> Just GLM51ZAI
  "glm-5-turbo-zai" -> Just GLM5TurboZAI
  "glm5turbo-zai"   -> Just GLM5TurboZAI
  "glm5turbozai"    -> Just GLM5TurboZAI
  "zai-glm5turbo"   -> Just GLM5TurboZAI
  "glm-5-turbo"     -> Just GLM5TurboZAI
  "glm5turbo"       -> Just GLM5TurboZAI
  "zai"             -> Just GLM51ZAI
  -- AlibabaCloud
  "minimax-m25-alibabacloud"   -> Just MinimaxM25AlibabaCloud
  "minimax-m2.5-alibabacloud"  -> Just MinimaxM25AlibabaCloud
  "minimaxm25-alibabacloud"    -> Just MinimaxM25AlibabaCloud
  "alibabacloud-minimax"       -> Just MinimaxM25AlibabaCloud
  "kimi-k25-alibabacloud"      -> Just KimiK25AlibabaCloud
  "kimi-k2.5-alibabacloud"     -> Just KimiK25AlibabaCloud
  "kimik25-alibabacloud"       -> Just KimiK25AlibabaCloud
  "alibabacloud-kimi"          -> Just KimiK25AlibabaCloud
  "qwen3.5-plus-alibabacloud"  -> Just Qwen35PlusAlibabaCloud
  "qwen35plus-alibabacloud"    -> Just Qwen35PlusAlibabaCloud
  "alibabacloud-qwen35plus"    -> Just Qwen35PlusAlibabaCloud
  "qwen3.5-plus"               -> Just Qwen35PlusAlibabaCloud
  "glm-5-alibabacloud"         -> Just GLM5AlibabaCloud
  "glm5-alibabacloud"          -> Just GLM5AlibabaCloud
  "alibabacloud-glm5"          -> Just GLM5AlibabaCloud
  "alibabacloud"               -> Just Qwen35PlusAlibabaCloud
  -- OpenRouter
  "openrouter"           -> Just OpenRouterModel
  "openrouter-universal" -> Just OpenRouterModel
  -- Unknown
  _ -> Nothing

-- | Read RUNIX_MODEL environment variable and resolve to a ModelId.
getModelId :: IO (Maybe ModelId)
getModelId = do
  maybeModel <- lookupEnv "RUNIX_MODEL"
  case maybeModel of
    Nothing -> do
      hPutStr IO.stderr "info: RUNIX_MODEL not set, defaulting to first available model\n"
      return Nothing
    Just modelStr -> case resolveModelId (T.pack modelStr) of
      Just mid -> return (Just mid)
      Nothing -> do
        hPutStr IO.stderr $ "warn: Unknown model '" ++ modelStr ++ "', defaulting to first available model\n"
        return Nothing
