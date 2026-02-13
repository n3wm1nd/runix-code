{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Widget isolation interpreter for TUI
--
-- 'interpretAsWidget' acts as a viewport: it intercepts 'LLM' to replace
-- the streaming callback so chunks route to 'AgentWidgets', and interprets
-- 'Logging' and observes 'Fail' for error reporting.
module UI.UserInterface
  ( interpretAsWidget
  ) where

import Polysemy
import Polysemy.State (State(..), get)
import Polysemy.Fail (Fail(..))
import qualified Data.Text as T

import Runix.LLM (LLM(..), LLMInfo(..))
import Runix.Logging (Logging(..))
import UniversalLLM (Message)
import UI.AgentWidgets (AgentWidgets, emitLog, emitStreamChunk, emitError, emitCompletion)

-- | Widget viewport interpreter.
--
-- Intercepts 'LLM' to replace the 'QueryLLM' callback: instead of the
-- caller's callback (which would re-enter the interpreter), we substitute
-- one that calls 'emitStreamChunk' directly into 'AgentWidgets'.
-- This works because 'AgentWidgets' is in @r@ (below 'LLM model'),
-- so 'runTSimple' inside the base interpreter can reach it.
--
-- Also routes 'Logging' to 'AgentWidgets' and observes 'Fail' for error
-- reporting.
interpretAsWidget
  :: forall model r a.
     ( Member (AgentWidgets (Message model)) r
     , Member (LLM model) r
     , Member (State [Message model]) r
     , Member Fail r
     )
  => Sem (Logging : LLM model : r) a
  -> Sem r a
interpretAsWidget action = do
  result <- intercept (\case
              Fail msg -> do
                emitError @(Message model) (T.pack msg)
                send (Fail msg)
            )
          . interpretH @(LLM model) (\case
              QueryLLM configs msgs _callback -> do
                result <- raise (send (QueryLLM configs msgs streamCallback))
                case result of
                  Right messages -> raise (emitCompletion @(Message model) messages)
                  Left err -> raise (emitError @(Message model) (T.pack err))
                pureT result
            )
          . interpret (\case
              Log level _ msg ->
                emitLog @(Message model) level msg
            )
          $ action

  finalHistory <- get @[Message model]
  emitCompletion @(Message model) finalHistory
  return result
  where
    -- | Callback that routes streaming chunks directly to AgentWidgets.
    -- This bypasses the LLM effect entirely — emitStreamChunk targets
    -- AgentWidgets which is in r, so it passes through the LLM interpreter
    -- without being consumed.
    streamCallback :: LLMInfo -> Sem r ()
    streamCallback (LLMInfo content) = emitStreamChunk @(Message model) content
