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
-- interpreter, captures chunks for display, and routes all output to 'AgentWidgets'.
-- Agent code only needs @Member (LLM model) r@ and @Member Logging r@ —
-- the streaming plumbing is entirely contained within this interpreter.
module UI.UserInterface
  ( interpretAsWidget
  ) where

import Polysemy
import Polysemy.State (State(..), get)
import Polysemy.Fail (Fail(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Runix.LLM (LLM)
import Runix.Logging (Logging(..))
import Runix.Streaming (StreamChunk(..))
import Runix.Streaming.SSE (extractContentFromChunk)
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.Cancellation (Cancellation)
import UniversalLLM (Message)
import UI.AgentWidgets (AgentWidgets, emitLog, emitStreamChunk, emitError, emitCompletion)

-- | Widget viewport interpreter.
--
-- Takes a pre-built streaming LLM interpreter and wraps agent code so that:
--
-- 1. @Logging@ is routed to 'AgentWidgets'
-- 2. @LLM model@ is interpreted via the streaming path (emitting raw SSE chunks)
-- 3. Raw @StreamChunk BS.ByteString@ chunks are parsed and routed to 'AgentWidgets'
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
  => (forall r' a'. Members '[Fail, Embed IO, HTTP, HTTPStreaming, StreamChunk BS.ByteString, Cancellation] r'
      => Sem (LLM model : r') a' -> Sem r' a')
  -> Sem (Logging : LLM model : r) a
  -> Sem r a
interpretAsWidget streamingInterp action = do
  result <- intercept (\case
              Fail msg -> do
                emitError @(Message model) (T.pack msg)
                send (Fail msg)
            )
          . captureSSEToWidgets @model
          . streamingInterp
          . raiseUnder @(StreamChunk BS.ByteString)
          . interpret (\case
              Log level _ msg ->
                emitLog @(Message model) level msg
            )
          $ action

  finalHistory <- get @[Message model]
  emitCompletion @(Message model) finalHistory
  return result

-- | Capture raw SSE byte chunks and route parsed content to AgentWidgets
captureSSEToWidgets
  :: forall model r a.
     Member (AgentWidgets (Message model)) r
  => Sem (StreamChunk BS.ByteString : r) a
  -> Sem r a
captureSSEToWidgets = interpret $ \case
  EmitChunk chunk ->
    mapM_ (emitStreamChunk @(Message model)) (extractContentFromChunk chunk)
