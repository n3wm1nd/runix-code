{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- | StreamChunk interpreter for chunk visualization in the TUI
--
-- Translates 'StreamChunk StreamEvent' into UI events, eliminating the effect
-- from the stack. Sits between 'interpretModel' and 'interpretTUIEffects'.
module UI.StreamingInterceptor
  ( interpretStreamChunksToUI
  ) where

import Polysemy

import Runix.LLM.Streaming (StreamEvent(..))
import Runix.StreamChunk (StreamChunk(..))
import UI.State (UIVars, sendAgentEvent, AgentEvent(..))
import UniversalLLM (Message)

-- | Eliminate 'StreamChunk StreamEvent' by forwarding each chunk to the UI.
interpretStreamChunksToUI
  :: forall model r a.
     Member (Embed IO) r
  => UIVars (Message model)
  -> Sem (StreamChunk StreamEvent : r) a
  -> Sem r a
interpretStreamChunksToUI uiVars = interpret $ \case
  EmitChunk event -> embed $ case event of
    StreamStarted -> sendAgentEvent uiVars (StreamStartEvent 0)
    StreamDone    -> sendAgentEvent uiVars (StreamEndEvent 0)
    _             -> sendAgentEvent uiVars (StreamChunkEvent 0 1)
