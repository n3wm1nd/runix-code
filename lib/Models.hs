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
    -- * Default Configurations
  , ModelDefaults(..)
    -- * Composable Providers
  , claudeSonnet45ComposableProvider
  , glm45AirComposableProvider
  , qwen3CoderComposableProvider
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import UniversalLLM
import qualified UniversalLLM.Providers.Anthropic as AnthropicProvider
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI
import UniversalLLM.Providers.OpenAI (LlamaCpp(..))
import UniversalLLM.Providers.XMLToolCalls (withXMLResponseParsing, xmlResponseParser)
import UniversalLLM.Protocols.OpenAI (OpenAIRequest(..), OpenAIMessage(..), OpenAIResponse)

--------------------------------------------------------------------------------
-- Default Configuration Class
--------------------------------------------------------------------------------

-- | Models can define their default configuration (streaming, reasoning, etc.)
class ModelDefaults provider model where
  defaultConfigs :: [ModelConfig provider model]

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
glmEnsureMinTokens :: forall provider model s.
                      (ProviderRequest provider ~ OpenAIRequest)
                   => Int  -- Minimum max_tokens
                   -> ComposableProvider provider model s
glmEnsureMinTokens minTokens _p _m configs _s = 
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
    reasoningDisabled :: [ModelConfig provider model] -> Bool
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
glmFixNullContent :: forall provider model s.
                     (ProviderRequest provider ~ OpenAIRequest)
                  => ComposableProvider provider model s
glmFixNullContent _provider _model _configs _s = 
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
    fixNullContent (OpenAIMessage "assistant" Nothing reasoning toolCalls toolCallId) =
      OpenAIMessage "assistant" (Just "") reasoning toolCalls toolCallId
    -- Tool result messages with the string "null"
    fixNullContent (OpenAIMessage "tool" (Just "null") reasoning toolCalls toolCallId) =
      OpenAIMessage "tool" (Just "") reasoning toolCalls toolCallId
    -- Assistant messages with content - strip any <think> tags that might be malformed
    fixNullContent (OpenAIMessage "assistant" (Just contentTxt) reasoning toolCalls toolCallId) =
      OpenAIMessage "assistant" (Just (stripThinkTags contentTxt)) reasoning toolCalls toolCallId
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

instance ModelName Anthropic ClaudeSonnet45 where
  modelName _ = "claude-sonnet-4-5-20250929"

instance HasTools ClaudeSonnet45 Anthropic where
  withTools = AnthropicProvider.anthropicTools

instance HasReasoning ClaudeSonnet45 Anthropic where
  withReasoning = AnthropicProvider.anthropicReasoning

-- Composable provider for ClaudeSonnet45
claudeSonnet45ComposableProvider :: ComposableProvider Anthropic ClaudeSonnet45 (ReasoningState ClaudeSonnet45 Anthropic, (ToolState ClaudeSonnet45 Anthropic, ()))
claudeSonnet45ComposableProvider = withReasoning `chainProviders` withTools `chainProviders` AnthropicProvider.baseComposableProvider

instance ModelDefaults Anthropic ClaudeSonnet45 where
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

instance ModelName LlamaCpp GLM45Air where
  modelName _ = "glm-4.5-air"

instance HasTools GLM45Air LlamaCpp where
  withTools = xmlResponseParser

instance HasReasoning GLM45Air LlamaCpp where
  withReasoning = OpenAI.openAIReasoning

-- Composable provider for GLM45Air
glm45AirComposableProvider :: 
  ( HasTools model LlamaCpp, HasReasoning model LlamaCpp, ModelName LlamaCpp model ) =>
  ComposableProvider LlamaCpp model 
  (ToolState model LlamaCpp, ((), ((), (ReasoningState model LlamaCpp, ((), ())))))
glm45AirComposableProvider = withTools 
  `chainProviders` glmFixNullContent 
  `chainProviders` glmEnsureMinTokens 2048 
  `chainProviders` withReasoning 
  `chainProviders` OpenAI.openAITools
  `chainProviders` OpenAI.baseComposableProvider

instance ModelDefaults LlamaCpp GLM45Air where
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

instance ModelName LlamaCpp Qwen3Coder where
  modelName _ = "qwen3-coder"

instance HasTools Qwen3Coder LlamaCpp where
  withTools = OpenAI.openAITools

-- Composable provider for Qwen3Coder
qwen3CoderComposableProvider :: ComposableProvider LlamaCpp Qwen3Coder (ToolState Qwen3Coder LlamaCpp, ())
qwen3CoderComposableProvider = withTools `chainProviders` OpenAI.baseComposableProvider

instance ModelDefaults LlamaCpp Qwen3Coder where
  defaultConfigs =
    [ Streaming True    -- Enable streaming for real-time feedback
    -- No reasoning for Qwen3Coder
    ]
