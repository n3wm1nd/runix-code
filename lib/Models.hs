{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

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
    -- * Default Configurations
  , ModelDefaults(..)
    -- * Composable Providers
  , claudeSonnet45ComposableProvider
  , glm45AirComposableProvider
  , qwen3CoderComposableProvider
  , universalComposableProvider
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import UniversalLLM
import qualified UniversalLLM.Providers.Anthropic as AnthropicProvider
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenAI(..), OpenRouter(..))
import UniversalLLM.Providers.XMLToolCalls (xmlResponseParser)
import UniversalLLM.Protocols.OpenAI (OpenAIRequest(..), OpenAIMessage(..))
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
claudeSonnet45ComposableProvider = providerTools

instance BaseComposableProvider (Model ClaudeSonnet45 Anthropic) where
  baseProvider = AnthropicProvider.baseComposableProvider

instance ModelDefaults (Model ClaudeSonnet45 Anthropic) where
  defaultConfigs :: [ModelConfig (Model ClaudeSonnet45 Anthropic)]
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable extended thinking
    ]

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
glm45AirComposableProvider = providerReasoningTools

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
qwen3CoderComposableProvider = providerTools

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
universalComposableProvider = providerReasoningTools

instance BaseComposableProvider (Model Universal OpenRouter) where
  baseProvider = OpenAI.baseComposableProvider

instance ModelDefaults (Model Universal OpenRouter) where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    , Reasoning True    -- Enable reasoning/extended thinking
    ]
