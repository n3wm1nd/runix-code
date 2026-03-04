{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
  ( -- * Core re-exports
    route
  , via
    -- * Re-exported from UniversalLLM.Models
  , ClaudeSonnet45(..)
  , ClaudeHaiku45(..)
  , ClaudeOpus46(..)
  , GLM45Air(..)
  , GLM46(..)
  , GLM47(..)
  , GLM5(..)
  , ZAI(..)
  , AlibabaCloud(..)
  , MinimaxM25(..)
  , Qwen35_122B(..)
  , Qwen3CoderNext(..)
  , Qwen35Plus(..)
  , KimiK25(..)
    -- * Runix-code specific models
  , UniversalWithTools(..)
  , OpenRouter(..)
  , AnthropicOAuth(..)
  , LlamaCpp(..)
    -- * Runix-code specific
  , ModelDefaults(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import UniversalLLM (route, via, Via, Model(..), ModelName(..), HasTools(..), Routing(..), ModelConfig(..), chainProviders)
import UniversalLLM.Settings
import UniversalLLM.Providers.Anthropic (AnthropicOAuth(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..), AlibabaCloud(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI

-- Import production models from universal-llm
import UniversalLLM.Models.Anthropic.Claude (ClaudeSonnet45(..), ClaudeHaiku45(..), ClaudeOpus46(..))
import UniversalLLM.Models.ZhipuAI.GLM (GLM45Air(..), GLM46(..), GLM47(..), GLM5(..), ZAI(..))
import UniversalLLM.Models.Minimax.M (MinimaxM25(..))
import UniversalLLM.Models.Alibaba.Qwen (Qwen35_122B(..), Qwen3CoderNext(..), Qwen35Plus(..))
import UniversalLLM.Models.Moonshot.Kimi (KimiK25(..))

--------------------------------------------------------------------------------
-- Runix-Code Specific Models
--------------------------------------------------------------------------------

-- | UniversalWithTools - OpenRouter model with tools support
--
-- This is the runix-code-specific variant that assumes the OpenRouter model
-- supports tools (required for a coding assistant).
data UniversalWithTools = UniversalWithTools Text deriving (Show, Eq)

instance ModelName (UniversalWithTools `Via` OpenRouter) where
  modelName (Model (UniversalWithTools name) _) = name

instance HasTools (UniversalWithTools `Via` OpenRouter) where
  withTools = OpenAI.openAITools

instance Routing (UniversalWithTools `Via` OpenRouter) where
  type RoutingState (UniversalWithTools `Via` OpenRouter) = ((), ())
  route = withTools `chainProviders` OpenAI.baseComposableProvider @(UniversalWithTools `Via` OpenRouter)

--------------------------------------------------------------------------------
-- Default Configuration Class
--------------------------------------------------------------------------------

-- | Models can define their default configuration (streaming, reasoning, etc.)
class ModelDefaults model where
  defaultConfigs :: [ModelConfig model]

--------------------------------------------------------------------------------
-- Runix-Code Specific Default Configurations
--------------------------------------------------------------------------------

instance ModelDefaults (ClaudeSonnet45 `Via` AnthropicOAuth) where
  defaultConfigs :: [ModelConfig (ClaudeSonnet45 `Via` AnthropicOAuth)]
  defaultConfigs =
    [ Reasoning True    -- Enable extended thinking
    ]

instance ModelDefaults (ClaudeHaiku45 `Via` AnthropicOAuth) where
  defaultConfigs :: [ModelConfig (ClaudeHaiku45 `Via` AnthropicOAuth)]
  defaultConfigs =
    [ Reasoning True    -- Enable extended thinking
    ]

instance ModelDefaults (ClaudeOpus46 `Via` AnthropicOAuth) where
  defaultConfigs :: [ModelConfig (ClaudeOpus46 `Via` AnthropicOAuth)]
  defaultConfigs =
    [ Reasoning True    -- Enable adaptive reasoning
    ]

instance ModelDefaults (GLM45Air `Via` LlamaCpp) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (MinimaxM25 `Via` LlamaCpp) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Qwen35_122B `Via` LlamaCpp) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (Qwen3CoderNext `Via` LlamaCpp) where
  defaultConfigs = []

instance ModelDefaults (UniversalWithTools `Via` OpenRouter) where
  defaultConfigs = []

instance ModelDefaults (GLM45Air `Via` ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (GLM46 `Via` ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (GLM47 `Via` ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (GLM5 `Via` ZAI) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (MinimaxM25 `Via` AlibabaCloud) where
  defaultConfigs = []

instance ModelDefaults (KimiK25 `Via` AlibabaCloud) where
  defaultConfigs = []

instance ModelDefaults (Qwen35Plus `Via` AlibabaCloud) where
  defaultConfigs = []

instance ModelDefaults (GLM5 `Via` AlibabaCloud) where
  defaultConfigs = []

--------------------------------------------------------------------------------
-- Model Configuration Types
--------------------------------------------------------------------------------

-- | Type instances for ConfigFor - each model's canonical configuration
type instance ConfigFor (ClaudeSonnet45 `Via` AnthropicOAuth) = ClaudeSonnet45Config
type instance ConfigFor (ClaudeHaiku45 `Via` AnthropicOAuth) = ClaudeHaiku45Config
type instance ConfigFor (ClaudeOpus46 `Via` AnthropicOAuth) = ClaudeOpus46Config
type instance ConfigFor (GLM45Air `Via` LlamaCpp) = GLM45AirConfig
type instance ConfigFor (MinimaxM25 `Via` LlamaCpp) = MinimaxM25Config
type instance ConfigFor (Qwen35_122B `Via` LlamaCpp) = Qwen35Config
type instance ConfigFor (Qwen3CoderNext `Via` LlamaCpp) = Qwen3CoderConfig
type instance ConfigFor (UniversalWithTools `Via` OpenRouter) = UniversalWithToolsConfig
type instance ConfigFor (GLM45Air `Via` ZAI) = GLM45AirZAIConfig
type instance ConfigFor (GLM46 `Via` ZAI) = GLM46Config
type instance ConfigFor (GLM47 `Via` ZAI) = GLM47Config
type instance ConfigFor (GLM5 `Via` ZAI) = GLM5Config
type instance ConfigFor (MinimaxM25 `Via` AlibabaCloud) = MinimaxM25AlibabaCloudConfig
type instance ConfigFor (KimiK25 `Via` AlibabaCloud) = KimiK25AlibabaCloudConfig
type instance ConfigFor (Qwen35Plus `Via` AlibabaCloud) = Qwen35PlusConfig
type instance ConfigFor (GLM5 `Via` AlibabaCloud) = GLM5AlibabaCloudConfig

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

-- MiniMax M2.5 (AlibabaCloud) configuration
data MinimaxM25AlibabaCloudConfig = MinimaxM25AlibabaCloudConfig
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Kimi K2.5 (AlibabaCloud) configuration
data KimiK25AlibabaCloudConfig = KimiK25AlibabaCloudConfig
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Qwen 3.5 Plus (AlibabaCloud) configuration
data Qwen35PlusConfig = Qwen35PlusConfig
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-5 (AlibabaCloud) configuration
data GLM5AlibabaCloudConfig = GLM5AlibabaCloudConfig
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)
