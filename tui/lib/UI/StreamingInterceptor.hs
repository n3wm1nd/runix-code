{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- | High-level LLM streaming interceptor for chunk visualization
--
-- This module provides an interceptor that sits between interpretLLMStream and interpretLLMViaStreaming.
-- Stack: interpretLLMStream . interceptStreamChunksToUI . interpretLLMViaStreaming
module UI.StreamingInterceptor
  ( interceptStreamChunksToUI
  ) where

import Polysemy
import Polysemy.State

import Runix.LLMStream (LLMStreaming(..), StreamEvent)
import Runix.Streaming (Streaming(..), fetchNext)
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))
import UniversalLLM (Message, ModelConfig)

-- | Intercept LLMStreaming to send chunk events to UI
--
-- This INTERCEPTS the LLMStreaming effect (doesn't add or remove it from the stack).
-- It counts chunks and sends visualization events, then forwards to the real LLMStreaming.
-- Type: Sem r a -> Sem r a (where LLMStreaming is Member of r)
interceptStreamChunksToUI
  :: forall model r a.
     ( Member (LLMStreaming model) r
     , Member (Embed IO) r
     )
  => UIVars (Message model)
  -> Sem r a
  -> Sem r a
interceptStreamChunksToUI uiVars = intercept handler
  where
    handler :: LLMStreaming model (Sem rInitial) x -> Sem r x
    handler (StartStream config) = do
      -- For now use a simple counter (TODO: proper stream ID tracking)
      -- Send start event
      embed $ sendAgentEvent uiVars (StreamStartEvent 0)

      -- Forward to underlying LLMStreaming with type application
      send @(Streaming StreamEvent (Either String [Message model]) ([ModelConfig model], [Message model])) (StartStream config)

    handler (FetchItem sid) = do
      -- Forward fetch with type application
      mItem <- send @(Streaming StreamEvent (Either String [Message model]) ([ModelConfig model], [Message model])) (FetchItem sid)

      -- On each chunk, send event
      case mItem of
        Just _event -> do
          -- Send chunk event (TODO: proper counting)
          embed $ sendAgentEvent uiVars (StreamChunkEvent 0 1)
        Nothing -> return ()

      return mItem

    handler (CloseStream sid) = do
      -- Send end event
      embed $ sendAgentEvent uiVars (StreamEndEvent 0)

      -- Forward close with type application
      send @(Streaming StreamEvent (Either String [Message model]) ([ModelConfig model], [Message model])) (CloseStream sid)
