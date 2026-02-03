{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Model definitions for runix-code
--
-- This module defines all the LLM models that can be used with runix-code.
-- Each model is a separate type with instances for ModelName, HasTools, and
-- ProviderImplementation.
module Models
  ( -- * Anthropic Models
    ClaudeSonnet45(..)
    -- * LlamaCpp Models
  , GLM45Air(..)
  , Qwen3Coder(..)
    -- * OpenRouter Models
  , Universal(..)
    -- * ZAI Models
  , ZAI(..)
  , GLM45Air_ZAI(..)
  , GLM46(..)
  , GLM47(..)
    -- * Default Configurations
  , ModelDefaults(..)
    -- * Composable Providers
  , claudeSonnet45ComposableProvider
  , claudeSonnet45OAuthComposableProvider
  , glm45AirComposableProvider
  , qwen3CoderComposableProvider
  , universalComposableProvider
  , glm45AirZAIComposableProvider
  , glm46ComposableProvider
  , glm47ComposableProvider
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import UniversalLLM
import UniversalLLM.Settings
import qualified UniversalLLM.Providers.Anthropic as AnthropicProvider
import UniversalLLM.Providers.Anthropic (Anthropic(..), AnthropicOAuth(..), OAuthToolsState)
import qualified UniversalLLM.Providers.OpenAI as OpenAI
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))
import UniversalLLM.Providers.XMLToolCalls (xmlResponseParser)
import UniversalLLM.Protocols.OpenAI (OpenAIRequest(..), OpenAIMessage(..), OpenAIResponse)
import qualified UniversalLLM.Providers.OpenAI as Openai

--------------------------------------------------------------------------------
-- Default Configuration Class
--------------------------------------------------------------------------------

-- | Models can define their default configuration (streaming, reasoning, etc.)
class ModelDefaults model where
  defaultConfigs :: [ModelConfig model]

--------------------------------------------------------------------------------
-- GLM-Specific Workarounds
--------------------------------------------------------------------------------

-- | GLM-specific minimum token limit enforcer
--
-- GLM's Jinja2 template crashes if it runs out of tokens while generating a <think> block,
-- because it tries to parse incomplete reasoning content and hits null values.
--
-- This combinator ensures a minimum max_tokens is set to reduce the chance of mid-block cutoff,
-- but only if reasoning is enabled (checked via Reasoning config).
glmEnsureMinTokens :: forall model s.
                      (ProviderRequest model ~ OpenAIRequest)
                   => Int  -- Minimum max_tokens
                   -> ComposableProvider model s
glmEnsureMinTokens minTokens _m configs _s = 
    noopHandler
    { cpConfigHandler = \req ->
        if reasoningDisabled configs
          then req
          else case max_tokens req of
            Nothing -> req { max_tokens = Just minTokens }
            Just current | current < minTokens -> req { max_tokens = Just minTokens }
            Just _ -> req
    }
  where
    -- Check if reasoning is explicitly disabled in config
    reasoningDisabled :: [ModelConfig model] -> Bool
    reasoningDisabled cfg = any isReasoningFalse cfg
      where
        isReasoningFalse (Reasoning False) = True
        isReasoningFalse _ = False

-- | GLM-specific null content fixer
--
-- GLM's Jinja2 template can't handle null content in messages. Two issues:
-- 1. Assistant messages with tool calls have null content (per OpenAI spec)
-- 2. Tool results that return Aeson.Null get JSON-encoded to the string "null"
--
-- GLM's template tries to do string operations on content without checking if it's null,
-- causing "Value is not callable: null" errors.
--
-- This combinator fixes both by replacing null/problematic content with empty strings.
--
-- Apply this AFTER openAIWithTools in the provider chain.
glmFixNullContent :: forall model s.
                     (ProviderRequest model ~ OpenAIRequest)
                  => ComposableProvider model s
glmFixNullContent _model _configs _s = 
    noopHandler
    { cpToRequest = \_msg req -> (fixAllNullContent req)
    }
  where
    -- Fix all messages in the request that have null content
    fixAllNullContent :: OpenAIRequest -> OpenAIRequest
    fixAllNullContent req = req { messages = map fixNullContent (messages req) }

    -- Replace null/problematic content with empty string and strip malformed think tags
    fixNullContent :: OpenAIMessage -> OpenAIMessage
    -- Assistant messages with null content (when they have tool calls)
    fixNullContent msg@OpenAIMessage{ role = "assistant", content = Nothing } =
      msg { content = Just "" }
    -- Tool result messages with the string "null"
    fixNullContent msg@OpenAIMessage{ role = "tool", content = Just "null" } =
      msg { content = Just "" }
    -- Assistant messages with content - strip any <think> tags that might be malformed
    fixNullContent msg@OpenAIMessage{ role = "assistant", content = Just contentTxt } =
      msg { content = Just (stripThinkTags contentTxt) }
    -- Everything else passes through unchanged
    fixNullContent msg = msg

    -- Strip all <think>...</think> blocks and any orphaned tags
    stripThinkTags :: Text -> Text
    stripThinkTags txt =
      let withoutBlocks = T.replace "</think>" "" $ T.replace "<think>" "" txt
      in withoutBlocks

--------------------------------------------------------------------------------
-- Anthropic Models
--------------------------------------------------------------------------------

-- | Claude Sonnet 4.5 model
data ClaudeSonnet45 = ClaudeSonnet45 deriving stock (Show, Eq)

instance ModelName (Model ClaudeSonnet45 Anthropic) where
  modelName (Model _ _) = "claude-sonnet-4-5-20250929"

instance HasTools (Model ClaudeSonnet45 Anthropic) where
  withTools = AnthropicProvider.anthropicTools

instance HasReasoning (Model ClaudeSonnet45 Anthropic) where
  type ReasoningState (Model ClaudeSonnet45 Anthropic) = AnthropicProvider.AnthropicReasoningState
  withReasoning = AnthropicProvider.anthropicReasoning

-- Composable provider for ClaudeSonnet45
claudeSonnet45ComposableProvider ::
  (HasTools model,
  BaseComposableProvider model) =>
 ComposableProvider model (ToolState model, BaseState model)
claudeSonnet45ComposableProvider = withTools `chainProviders` baseProvider

instance BaseComposableProvider (Model ClaudeSonnet45 Anthropic) where
  baseProvider = AnthropicProvider.baseComposableProvider

instance ModelDefaults (Model ClaudeSonnet45 Anthropic) where
  defaultConfigs :: [ModelConfig (Model ClaudeSonnet45 Anthropic)]
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable extended thinking
    ]

-- OAuth version with tool name workarounds
instance ModelName (Model ClaudeSonnet45 AnthropicOAuth) where
  modelName (Model _ _) = "claude-sonnet-4-5-20250929"

instance HasTools (Model ClaudeSonnet45 AnthropicOAuth) where
  type ToolState (Model ClaudeSonnet45 AnthropicOAuth) = OAuthToolsState
  withTools = AnthropicProvider.anthropicOAuthTools

instance HasReasoning (Model ClaudeSonnet45 AnthropicOAuth) where
  type ReasoningState (Model ClaudeSonnet45 AnthropicOAuth) = AnthropicProvider.AnthropicReasoningState
  withReasoning = AnthropicProvider.anthropicReasoning

instance BaseComposableProvider (Model ClaudeSonnet45 AnthropicOAuth) where
  baseProvider = AnthropicProvider.baseComposableProvider

instance ModelDefaults (Model ClaudeSonnet45 AnthropicOAuth) where
  defaultConfigs :: [ModelConfig (Model ClaudeSonnet45 AnthropicOAuth)]
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable extended thinking
    ]

-- OAuth composable provider (with tool name workarounds)
claudeSonnet45OAuthComposableProvider ::
  ComposableProvider (Model ClaudeSonnet45 AnthropicOAuth) (AnthropicProvider.AnthropicReasoningState, (OAuthToolsState, ()))
claudeSonnet45OAuthComposableProvider = withReasoning `chainProviders` withTools `chainProviders` baseProvider

--------------------------------------------------------------------------------
-- LlamaCpp Models
--------------------------------------------------------------------------------

-- | GLM4.5-air model (via llama.cpp)
--
-- This model uses a hybrid approach:
-- - Request: Uses native OpenAI tool format (llamacpp understands it)
-- - Response: Model outputs XML, we parse it (llamacpp doesn't recognize it)
--
-- GLM also has a Jinja2 template bug that can't handle null content in tool results,
-- so we apply glmFixNullToolResults to work around it.
data GLM45Air = GLM45Air deriving stock (Show, Eq)

instance ModelName (Model GLM45Air LlamaCpp) where
  modelName (Model _ _) = "glm-4.5-air"

instance HasTools (Model GLM45Air LlamaCpp) where
  type ToolState (Model GLM45Air LlamaCpp) = ((), ())
  withTools = xmlResponseParser `chainProviders` OpenAI.openAITools

instance HasReasoning (Model GLM45Air LlamaCpp) where
  withReasoning = OpenAI.openAIReasoning

-- Composable provider for GLM45Air
glm45AirComposableProvider ::
  ( HasTools model, HasReasoning model,
  BaseComposableProvider model ) =>
  ComposableProvider model
  (ToolState model, (ReasoningState model, BaseState model))
glm45AirComposableProvider = withTools `chainProviders` withReasoning `chainProviders` baseProvider

instance BaseComposableProvider (Model GLM45Air LlamaCpp) where
  type BaseState (Model GLM45Air LlamaCpp) = ((), ((), ()))
  baseProvider = glmFixNullContent `chainProviders` glmEnsureMinTokens 2048 `chainProviders` OpenAI.baseComposableProvider

instance ModelDefaults (Model GLM45Air LlamaCpp) where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable reasoning extraction
    ]

-- | Qwen3-Coder model (via llama.cpp)
--
-- This model generates XML tool calls natively, but llama.cpp's chat template
-- automatically converts them to OpenAI format for us. We receive standard
-- OpenAI-style tool calls in both streaming and non-streaming responses.
data Qwen3Coder = Qwen3Coder deriving stock (Show, Eq)

instance ModelName (Model Qwen3Coder LlamaCpp) where
  modelName (Model _ _) = "qwen3-coder"

instance HasTools (Model Qwen3Coder LlamaCpp) where
  withTools = OpenAI.openAITools

-- Composable provider for Qwen3Coder
qwen3CoderComposableProvider :: ComposableProvider (Model Qwen3Coder LlamaCpp) (ToolState (Model Qwen3Coder LlamaCpp), ())
qwen3CoderComposableProvider = withTools `chainProviders` baseProvider

instance BaseComposableProvider (Model Qwen3Coder LlamaCpp) where
  baseProvider = Openai.baseComposableProvider

instance ModelDefaults (Model Qwen3Coder LlamaCpp) where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    -- No reasoning for Qwen3Coder
    ]

--------------------------------------------------------------------------------
-- OpenRouter Models
--------------------------------------------------------------------------------

-- | Universal model for OpenRouter
--
-- This model allows specifying any OpenRouter-compatible model by storing the
-- model name as a value. The model name is read from the OPENROUTER_MODEL
-- environment variable.
data Universal = Universal Text deriving stock (Show, Eq)

instance ModelName (Model Universal OpenRouter) where
  modelName (Model (Universal name) _) = name

instance HasTools (Model Universal OpenRouter) where
  withTools = OpenAI.openAITools

instance HasReasoning (Model Universal OpenRouter) where
  type ReasoningState (Model Universal OpenRouter) = OpenAI.OpenRouterReasoningState
  withReasoning = OpenAI.openRouterReasoning

-- Composable provider for Universal
universalComposableProvider ::
  (HasTools model, HasReasoning model,
  BaseComposableProvider model) =>
  ComposableProvider model
  (ToolState model, (ReasoningState model, BaseState model))
universalComposableProvider = withTools `chainProviders` withReasoning `chainProviders` baseProvider

instance BaseComposableProvider (Model Universal OpenRouter) where
  baseProvider = OpenAI.baseComposableProvider

instance ModelDefaults (Model Universal OpenRouter) where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable reasoning/extended thinking
    ]

--------------------------------------------------------------------------------
-- ZAI Models
--------------------------------------------------------------------------------

-- | ZAI provider for GLM models (https://api.z.ai)
-- Uses OpenAI-compatible protocol
data ZAI = ZAI deriving stock (Show, Eq)

-- Supporting capability instances for ZAI
instance SupportsTemperature ZAI
instance SupportsMaxTokens ZAI
instance SupportsSeed ZAI
instance SupportsSystemPrompt ZAI
instance SupportsStop ZAI
instance SupportsStreaming ZAI

-- | GLM-4.5-Air model via ZAI
data GLM45Air_ZAI = GLM45Air_ZAI deriving stock (Show, Eq)

instance Provider (Model GLM45Air_ZAI ZAI) where
  type ProviderRequest (Model GLM45Air_ZAI ZAI) = OpenAIRequest
  type ProviderResponse (Model GLM45Air_ZAI ZAI) = OpenAIResponse

instance ModelName (Model GLM45Air_ZAI ZAI) where
  modelName (Model _ _) = "glm-4.5-air"

instance HasTools (Model GLM45Air_ZAI ZAI) where
  withTools = OpenAI.openAITools

instance HasReasoning (Model GLM45Air_ZAI ZAI) where
  withReasoning = OpenAI.openAIReasoning

instance HasJSON (Model GLM45Air_ZAI ZAI) where
  type JSONState (Model GLM45Air_ZAI ZAI) = ()
  withJSON = OpenAI.openAIJSON

-- Composable provider for GLM45Air_ZAI
glm45AirZAIComposableProvider ::
  ( HasTools model, HasReasoning model, HasJSON model,
  BaseComposableProvider model ) =>
  ComposableProvider model
  (JSONState model, (ReasoningState model, (ToolState model, BaseState model)))
glm45AirZAIComposableProvider = withJSON `chainProviders` withReasoning `chainProviders` withTools `chainProviders` baseProvider

instance BaseComposableProvider (Model GLM45Air_ZAI ZAI) where
  baseProvider = OpenAI.baseComposableProvider

instance ModelDefaults (Model GLM45Air_ZAI ZAI) where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable reasoning extraction
    ]

-- | GLM-4.6 model via ZAI
data GLM46 = GLM46 deriving stock (Show, Eq)

instance Provider (Model GLM46 ZAI) where
  type ProviderRequest (Model GLM46 ZAI) = OpenAIRequest
  type ProviderResponse (Model GLM46 ZAI) = OpenAIResponse

instance ModelName (Model GLM46 ZAI) where
  modelName (Model _ _) = "glm-4.6"

instance HasTools (Model GLM46 ZAI) where
  withTools = OpenAI.openAITools

instance HasReasoning (Model GLM46 ZAI) where
  withReasoning = OpenAI.openAIReasoning

instance HasJSON (Model GLM46 ZAI) where
  withJSON = OpenAI.openAIJSON

-- Composable provider for GLM46
glm46ComposableProvider ::
  ( HasTools model, HasReasoning model, HasJSON model,
  BaseComposableProvider model ) =>
  ComposableProvider model
  (JSONState model, (ReasoningState model, (ToolState model, BaseState model)))
glm46ComposableProvider = withJSON `chainProviders` withReasoning `chainProviders` withTools `chainProviders` baseProvider

instance BaseComposableProvider (Model GLM46 ZAI) where
  baseProvider = OpenAI.baseComposableProvider

instance ModelDefaults (Model GLM46 ZAI) where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable reasoning extraction
    ]

-- | GLM-4.7 model via ZAI
data GLM47 = GLM47 deriving stock (Show, Eq)

instance Provider (Model GLM47 ZAI) where
  type ProviderRequest (Model GLM47 ZAI) = OpenAIRequest
  type ProviderResponse (Model GLM47 ZAI) = OpenAIResponse

instance ModelName (Model GLM47 ZAI) where
  modelName (Model _ _) = "glm-4.7"

instance HasTools (Model GLM47 ZAI) where
  withTools = OpenAI.openAITools

instance HasReasoning (Model GLM47 ZAI) where
  withReasoning = OpenAI.openAIReasoning

instance HasJSON (Model GLM47 ZAI) where
  withJSON = OpenAI.openAIJSON

-- Composable provider for GLM47
glm47ComposableProvider ::
  ( HasTools model, HasReasoning model, HasJSON model,
  BaseComposableProvider model ) =>
  ComposableProvider model
  (JSONState model, (ReasoningState model, (ToolState model, BaseState model)))
glm47ComposableProvider = withJSON `chainProviders` withReasoning `chainProviders` withTools `chainProviders` baseProvider

instance BaseComposableProvider (Model GLM47 ZAI) where
  baseProvider = OpenAI.baseComposableProvider

instance ModelDefaults (Model GLM47 ZAI) where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable reasoning extraction
    ]

--------------------------------------------------------------------------------
-- Model Configuration Types
--------------------------------------------------------------------------------

-- | Type instances for ConfigFor - each model's canonical configuration
type instance ConfigFor (Model ClaudeSonnet45 Anthropic) = ClaudeSonnet45Config
type instance ConfigFor (Model GLM45Air LlamaCpp) = GLM45AirConfig
type instance ConfigFor (Model Qwen3Coder LlamaCpp) = Qwen3CoderConfig
type instance ConfigFor (Model Universal OpenRouter) = UniversalConfig
type instance ConfigFor (Model GLM45Air_ZAI ZAI) = GLM45AirZAIConfig
type instance ConfigFor (Model GLM46 ZAI) = GLM46Config
type instance ConfigFor (Model GLM47 ZAI) = GLM47Config

-- Claude Sonnet 4.5 configuration
data ClaudeSonnet45Config = ClaudeSonnet45Config
  { streaming :: StreamingSetting
  , reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.5-Air (LlamaCpp) configuration
data GLM45AirConfig = GLM45AirConfig
  { streaming :: StreamingSetting
  , reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Qwen3-Coder configuration (no reasoning)
data Qwen3CoderConfig = Qwen3CoderConfig
  { streaming :: StreamingSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- Universal (OpenRouter) configuration
data UniversalConfig = UniversalConfig
  { streaming :: StreamingSetting
  , reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.5-Air (ZAI) configuration
data GLM45AirZAIConfig = GLM45AirZAIConfig
  { streaming :: StreamingSetting
  , reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.6 configuration
data GLM46Config = GLM46Config
  { streaming :: StreamingSetting
  , reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)

-- GLM-4.7 configuration
data GLM47Config = GLM47Config
  { streaming :: StreamingSetting
  , reasoning :: ReasoningSetting
  , temperature :: Maybe TemperatureSetting
  , maxTokens :: Maybe MaxTokensSetting
  } deriving stock (Show, Eq, Generic)
