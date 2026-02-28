{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Model definitions for runix-code
--
-- This module re-exports tested models from UniversalLLM.Models and adds
-- runix-code-specific configuration defaults.
module Models
  ( -- * Re-exported from UniversalLLM.Models
    ClaudeSonnet45(..)
  , ClaudeHaiku45(..)
  , ClaudeOpus46(..)
  , GLM45Air(..)
  , GLM46(..)
  , GLM47(..)
  , GLM5(..)
  , ZAI(..)
  , MinimaxM25(..)
  , Qwen35_122B(..)
  , Qwen3CoderNext(..)
  , Universal(..)
    -- * Tested providers from universal-llm
  , claudeSonnet45OAuth
  , claudeHaiku45OAuth
  , claudeOpus46OAuth
  , minimaxM25LlamaCpp
  , qwen35_122B
  , glm45AirLlamaCpp
  , glm45AirZAI
  , glm46
  , glm47
  , glm5
  , qwen3Coder
  , universal
    -- * Runix-code specific models
  , UniversalWithTools(..)
  , universalWithTools
    -- * Runix-code specific
  , ModelDefaults(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import UniversalLLM
import UniversalLLM.Settings
import UniversalLLM.Providers.Anthropic (AnthropicOAuth(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI

-- Import production models from universal-llm
import UniversalLLM.Models.Anthropic.Claude (ClaudeSonnet45(..), ClaudeHaiku45(..), ClaudeOpus46(..), claudeSonnet45OAuth, claudeHaiku45OAuth, claudeOpus46OAuth)
import UniversalLLM.Models.ZhipuAI.GLM (GLM45Air(..), GLM46(..), GLM47(..), GLM5(..), ZAI(..), glm45AirLlamaCpp, glm45AirZAI, glm46, glm47, glm5)
import UniversalLLM.Models.Minimax.M (MinimaxM25(..), minimaxM25LlamaCpp)
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_122B(..), Qwen3CoderNext(..), qwen35_122B, qwen3Coder)
import UniversalLLM.Models.OpenRouter (Universal(..))

--------------------------------------------------------------------------------
-- Runix-Code Specific Providers
--------------------------------------------------------------------------------

-- | Universal OpenRouter provider - text-only base provider
--
-- Universal is intentionally text-only for safety since not all OpenRouter
-- models support tools/reasoning/JSON.
universal :: ComposableProvider (Model Universal OpenRouter) ()
universal = OpenAI.baseComposableProvider @(Model Universal OpenRouter)

-- | UniversalWithTools - OpenRouter model with tools support
--
-- This is the runix-code-specific variant that assumes the OpenRouter model
-- supports tools (required for a coding assistant). Use this instead of
-- Universal when you need tool calling support.
data UniversalWithTools = UniversalWithTools Text deriving (Show, Eq)

instance ModelName (Model UniversalWithTools OpenRouter) where
  modelName (Model (UniversalWithTools name) _) = name

instance HasTools (Model UniversalWithTools OpenRouter) where
  withTools = OpenAI.openAITools

-- | Composable provider for UniversalWithTools
universalWithTools :: ComposableProvider (Model UniversalWithTools OpenRouter) ((), ())
universalWithTools = withTools `chainProviders` OpenAI.baseComposableProvider @(Model UniversalWithTools OpenRouter)

--------------------------------------------------------------------------------
-- Default Configuration Class
--------------------------------------------------------------------------------

-- | Models can define their default configuration (streaming, reasoning, etc.)
class ModelDefaults model where
  defaultConfigs :: [ModelConfig model]

--------------------------------------------------------------------------------
-- Runix-Code Specific Default Configurations
--------------------------------------------------------------------------------

instance ModelDefaults (Model ClaudeSonnet45 AnthropicOAuth) where
  defaultConfigs :: [ModelConfig (Model ClaudeSonnet45 AnthropicOAuth)]
  defaultConfigs =
    [ Reasoning True    -- Enable extended thinking
    ]

instance ModelDefaults (Model ClaudeHaiku45 AnthropicOAuth) where
  defaultConfigs :: [ModelConfig (Model ClaudeHaiku45 AnthropicOAuth)]
  defaultConfigs =
    [ Reasoning True    -- Enable extended thinking
    ]

instance ModelDefaults (Model ClaudeOpus46 AnthropicOAuth) where
  defaultConfigs :: [ModelConfig (Model ClaudeOpus46 AnthropicOAuth)]
  defaultConfigs =
    [ Reasoning True    -- Enable adaptive reasoning
    ]

instance ModelDefaults (Model GLM45Air LlamaCpp) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Model MinimaxM25 LlamaCpp) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Model Qwen35_122B LlamaCpp) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Model Qwen3CoderNext LlamaCpp) where
  defaultConfigs = []

instance ModelDefaults (Model Universal OpenRouter) where
  defaultConfigs = []

instance ModelDefaults (Model UniversalWithTools OpenRouter) where
  defaultConfigs = []

instance ModelDefaults (Model GLM45Air ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Model GLM46 ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Model GLM47 ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Model GLM5 ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

--------------------------------------------------------------------------------
-- Model Configuration Types
--------------------------------------------------------------------------------

-- | Type instances for ConfigFor - each model's canonical configuration
type instance ConfigFor (Model ClaudeSonnet45 AnthropicOAuth) = ClaudeSonnet45Config
type instance ConfigFor (Model ClaudeHaiku45 AnthropicOAuth) = ClaudeHaiku45Config
type instance ConfigFor (Model ClaudeOpus46 AnthropicOAuth) = ClaudeOpus46Config
type instance ConfigFor (Model GLM45Air LlamaCpp) = GLM45AirConfig
type instance ConfigFor (Model MinimaxM25 LlamaCpp) = MinimaxM25Config
type instance ConfigFor (Model Qwen35_122B LlamaCpp) = Qwen35Config
type instance ConfigFor (Model Qwen3CoderNext LlamaCpp) = Qwen3CoderConfig
type instance ConfigFor (Model Universal OpenRouter) = UniversalConfig
type instance ConfigFor (Model UniversalWithTools OpenRouter) = UniversalWithToolsConfig
type instance ConfigFor (Model GLM45Air ZAI) = GLM45AirZAIConfig
type instance ConfigFor (Model GLM46 ZAI) = GLM46Config
type instance ConfigFor (Model GLM47 ZAI) = GLM47Config
type instance ConfigFor (Model GLM5 ZAI) = GLM5Config

-- Claude Sonnet 4.5 configuration
data ClaudeSonnet45Config = ClaudeSonnet45Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Claude Haiku 4.5 configuration
data ClaudeHaiku45Config = ClaudeHaiku45Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Claude Opus 4.6 configuration
data ClaudeOpus46Config = ClaudeOpus46Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.5-Air (LlamaCpp) configuration
data GLM45AirConfig = GLM45AirConfig
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- MiniMax M2.5 (LlamaCpp) configuration
data MinimaxM25Config = MinimaxM25Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Qwen 3.5 122B configuration
data Qwen35Config = Qwen35Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Qwen3-Coder configuration (no reasoning)
data Qwen3CoderConfig = Qwen3CoderConfig
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Universal (OpenRouter) configuration - text-only, no reasoning
data UniversalConfig = UniversalConfig
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- UniversalWithTools (OpenRouter) configuration - tools support
data UniversalWithToolsConfig = UniversalWithToolsConfig
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.5-Air (ZAI) configuration
data GLM45AirZAIConfig = GLM45AirZAIConfig
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.6 configuration
data GLM46Config = GLM46Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.7 configuration
data GLM47Config = GLM47Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-5 configuration
data GLM5Config = GLM5Config
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)
