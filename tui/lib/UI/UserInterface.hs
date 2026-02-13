{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Widget isolation interpreter for TUI
--
-- 'interpretAsWidget' acts as a viewport: it locally provides a streaming LLM
-- interpreter, intercepts 'LLMInfo' events for display, and routes all output
-- to 'AgentWidgets'. Agent code only needs @Member (LLM model) r@,
-- @Member LLMInfo r@, and @Member Logging r@ — the streaming plumbing is
-- entirely contained within this interpreter.
module UI.UserInterface
  ( interpretAsWidget
  ) where

import Polysemy
import Polysemy.State (State(..), get)
import Polysemy.Fail (Fail(..))
import qualified Data.Text as T

import Runix.LLM (LLM, LLMInfo(..))
import Runix.Logging (Logging(..))
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.Cancellation (Cancellation)
import UniversalLLM (Message)
import UI.AgentWidgets (AgentWidgets, emitLog, emitStreamChunk, emitError, emitCompletion)

-- | Widget viewport interpreter.
--
-- Takes a pre-built streaming LLM interpreter and wraps agent code so that:
--
-- 1. @Logging@ is routed to 'AgentWidgets'
-- 2. @LLM model@ is interpreted via the streaming path
-- 3. @LLMInfo@ events are intercepted and routed to 'AgentWidgets'
-- 4. @Fail@ is intercepted (observed + re-raised) to send error events
-- 5. On completion, final history is read from @State@ and emitted
--
-- The streaming interpreter is pre-applied at construction time (see 'ModelInterpreter'),
-- so call sites just pass this function directly without knowing about streaming details.
interpretAsWidget
  :: forall model r a.
     ( Member (AgentWidgets (Message model)) r
     , Member (State [Message model]) r
     , Member Fail r
     , Member HTTP r
     , Member HTTPStreaming r
     , Member Cancellation r
     , Member (Embed IO) r
     )
  => (forall r' a'. Members '[Fail, Embed IO, HTTP, HTTPStreaming, LLMInfo, Cancellation] r'
      => Sem (LLM model : r') a' -> Sem r' a')
  -> Sem (Logging : LLM model : LLMInfo : r) a
  -> Sem r a
interpretAsWidget streamingInterp action = do
  result <- intercept (\case
              Fail msg -> do
                emitError @(Message model) (T.pack msg)
                send (Fail msg)
            )
          . interpret (\case
              EmitLLMInfo content ->
                emitStreamChunk @(Message model) content
            )
          . streamingInterp
          . interpret (\case
              Log level _ msg ->
                emitLog @(Message model) level msg
            )
          $ action

  finalHistory <- get @[Message model]
  emitCompletion @(Message model) finalHistory
  return result
