{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
  ) where

import UniversalLLM
import UniversalLLM.Core.Types (ComposableProvider)
import qualified UniversalLLM.Providers.Anthropic as AnthropicProvider
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import qualified UniversalLLM.Providers.OpenAI as OpenAI
import UniversalLLM.Providers.OpenAI (LlamaCpp(..))
import UniversalLLM.Providers.XMLToolCalls (withXMLResponseParsing)

--------------------------------------------------------------------------------
-- Anthropic Models
--------------------------------------------------------------------------------

-- | Claude Sonnet 4.5 model
data ClaudeSonnet45 = ClaudeSonnet45 deriving (Show, Eq)

instance ModelName Anthropic ClaudeSonnet45 where
  modelName _ = "claude-sonnet-4-5-20250929"

instance HasTools ClaudeSonnet45 Anthropic where
  withTools = AnthropicProvider.anthropicWithTools

instance ProviderImplementation Anthropic ClaudeSonnet45 where
  getComposableProvider = AnthropicProvider.ensureUserFirst . withTools $ AnthropicProvider.baseComposableProvider

--------------------------------------------------------------------------------
-- LlamaCpp Models
--------------------------------------------------------------------------------

-- | GLM4.5-air model (via llama.cpp)
--
-- This model uses a hybrid approach:
-- - Request: Uses native OpenAI tool format (llamacpp understands it)
-- - Response: Model outputs XML, we parse it (llamacpp doesn't recognize it)
data GLM45Air = GLM45Air deriving (Show, Eq)

instance ModelName LlamaCpp GLM45Air where
  modelName _ = "glm-4.5-air"

instance HasTools GLM45Air LlamaCpp where
  withTools = withXMLResponseParsing

instance ProviderImplementation LlamaCpp GLM45Air where
  -- Chain: base -> openAIWithTools (native tool format) -> withXMLResponseParsing (parse XML in responses)
  getComposableProvider = withTools $ OpenAI.openAIWithTools OpenAI.baseComposableProvider

-- | Qwen3-Coder model (via llama.cpp)
--
-- This model uses a hybrid approach:
-- - Request: Uses native OpenAI tool format (llamacpp understands it)
-- - Response: Model outputs XML, we parse it (llamacpp doesn't recognize it)
data Qwen3Coder = Qwen3Coder deriving (Show, Eq)

instance ModelName LlamaCpp Qwen3Coder where
  modelName _ = "qwen3-coder"

instance HasTools Qwen3Coder LlamaCpp where
  withTools = withXMLResponseParsing

instance ProviderImplementation LlamaCpp Qwen3Coder where
  -- Chain: base -> openAIWithTools (native tool format) -> withXMLResponseParsing (parse XML in responses)
  getComposableProvider = withTools $ OpenAI.openAIWithTools OpenAI.baseComposableProvider
