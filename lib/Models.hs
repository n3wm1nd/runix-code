{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (KimiK25 `Via` AlibabaCloud) where
  defaultConfigs =
    [ Reasoning True    -- Enable Deep Thinking (reasoning content not returned)
    ]

instance ModelDefaults (Qwen35Plus `Via` AlibabaCloud) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

instance ModelDefaults (GLM5 `Via` AlibabaCloud) where
  defaultConfigs =
    [ Reasoning True    -- Enable reasoning extraction
    ]

--------------------------------------------------------------------------------
-- Model Configuration Types
--------------------------------------------------------------------------------

-- | Configuration for models with reasoning support
data ConfigWithReasoning = ConfigWithReasoning
  { reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- | Configuration for models without reasoning support
data ConfigBasic = ConfigBasic
  { temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- | Type instances for ConfigFor - each model's canonical configuration
-- Models with reasoning support
type instance ConfigFor (ClaudeSonnet45 `Via` AnthropicOAuth) = ConfigWithReasoning
type instance ConfigFor (ClaudeHaiku45 `Via` AnthropicOAuth) = ConfigWithReasoning
type instance ConfigFor (ClaudeOpus46 `Via` AnthropicOAuth) = ConfigWithReasoning
type instance ConfigFor (GLM45Air `Via` LlamaCpp) = ConfigWithReasoning
type instance ConfigFor (MinimaxM25 `Via` LlamaCpp) = ConfigWithReasoning
type instance ConfigFor (Qwen35_122B `Via` LlamaCpp) = ConfigWithReasoning
type instance ConfigFor (GLM45Air `Via` ZAI) = ConfigWithReasoning
type instance ConfigFor (GLM46 `Via` ZAI) = ConfigWithReasoning
type instance ConfigFor (GLM47 `Via` ZAI) = ConfigWithReasoning
type instance ConfigFor (GLM5 `Via` ZAI) = ConfigWithReasoning
type instance ConfigFor (MinimaxM25 `Via` AlibabaCloud) = ConfigWithReasoning
type instance ConfigFor (KimiK25 `Via` AlibabaCloud) = ConfigWithReasoning  -- Hidden reasoning (parameter accepted, content not returned)
type instance ConfigFor (Qwen35Plus `Via` AlibabaCloud) = ConfigWithReasoning
type instance ConfigFor (GLM5 `Via` AlibabaCloud) = ConfigWithReasoning

-- Models without reasoning support
type instance ConfigFor (Qwen3CoderNext `Via` LlamaCpp) = ConfigBasic
type instance ConfigFor (UniversalWithTools `Via` OpenRouter) = ConfigBasic
