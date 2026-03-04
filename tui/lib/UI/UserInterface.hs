{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | Widget isolation interpreter for TUI
--
-- CRITICAL DESIGN:
-- ================
-- interpretAsWidget takes an action with these effects on the stack:
--   AgentWidgets (INNER - for sub-agents only)
--   Logging
--   LLM
--   r (contains PARENT AgentWidgets)
--
-- OUTPUT ROUTING:
-- - LLM messages → PARENT AgentWidgets (on r)
-- - Logging → PARENT AgentWidgets (on r)
-- - Fail → PARENT AgentWidgets (on r)
-- - Sub-agent operations → INNER AgentWidgets → wrapped with withSubAgent → PARENT AgentWidgets
--
module UI.UserInterface
  ( interpretAsWidget
  ) where

import Polysemy
import Polysemy.Fail (Fail(..))
import qualified Data.Text as T

import Runix.LLM (LLM(..))
import Runix.Logging (Logging(..))
import UniversalLLM (Message)
import UI.AgentWidgets (AgentWidgets(..), AgentStatus(..), SubsectionAddr(..), addMessage, logMessage, setStatus, startSubsection)

-- | Interpret LLM, Logging, and inner AgentWidgets effects as AgentWidgets operations
--
-- PARENT AgentWidgets is on r - this is where ALL output goes
-- INNER AgentWidgets is ONLY for sub-agents to use
interpretAsWidget
  :: forall model r a.
     ( Member (AgentWidgets (Message model)) r  -- PARENT - receives all output
     , Member (LLM model) r
     , Member Fail r
     )
  => Sem (Logging : LLM model : AgentWidgets (Message model) : r) a
  -> Sem r a
interpretAsWidget =
  -- Composition: each interpreter removes one effect from the stack
  -- IMPORTANT: interpretAgentWidgetsAsSubsection must come FIRST (closest to action)
  -- so that it intercepts operations from interceptLLMToWidgets and interpretLoggingToAgentWidgets
  interpretFail @model
  . interpretAgentWidgetsAsSubsection @model
  . interceptLLMToWidgets @model
  . interpretLoggingToAgentWidgets @model

-- | Intercept AgentWidgets operations and route them into a subsection
-- Creates a subsection, runs action with operations routed to that subsection address
interpretAgentWidgetsAsSubsection
  :: forall model r a.
     Member (AgentWidgets (Message model)) r
  => Sem (AgentWidgets (Message model) : r) a
  -> Sem r a
interpretAgentWidgetsAsSubsection action = do
  -- Start a new subsection at root, get its address
  subsectionAddr <- startSubsection @(Message model) Root

  -- Intercept all AgentWidgets operations and redirect them to the subsection address
  interpret (\case
      AddMessage _oldAddr msg ->
        send @(AgentWidgets (Message model)) (AddMessage subsectionAddr msg)
      LogMessage _oldAddr level text ->
        send @(AgentWidgets (Message model)) (LogMessage subsectionAddr level text)
      SetStatus _oldAddr status ->
        send @(AgentWidgets (Message model)) (SetStatus subsectionAddr status)
      ReplaceHistory _oldAddr msgs ->
        send @(AgentWidgets (Message model)) (ReplaceHistory subsectionAddr msgs)
      StartSubsection _oldAddr ->
        -- Nested subsection: start it relative to our subsection
        send @(AgentWidgets (Message model)) (StartSubsection subsectionAddr)
    ) action

-- | Interpret Logging as AgentWidgets operations
interpretLoggingToAgentWidgets
  :: forall model r a.
     Member (AgentWidgets (Message model)) r
  => Sem (Logging : LLM model : r) a
  -> Sem (LLM model : r) a
interpretLoggingToAgentWidgets = interpret $ \case
  Log level _callstack msg ->
    raise $ logMessage @(Message model) level msg

-- | Intercept LLM queries and send results to AgentWidgets
interceptLLMToWidgets
  :: forall model r a.
     ( Member (AgentWidgets (Message model)) r
     , Member (LLM model) r
     )
  => Sem (LLM model : r) a
  -> Sem r a
interceptLLMToWidgets = interpret $ \case
  QueryLLM configs msgs -> do
    -- Send to parent LLM (already on r after interpret removed the inner LLM)
    result <- send (QueryLLM @model configs msgs)

    case result of
      Right messages -> do
        setStatus @(Message model) Done
        mapM_ (addMessage @(Message model)) messages
      Left err ->
        setStatus @(Message model) (Failed (T.pack err))

    return result

-- | Intercept Fail and report to AgentWidgets
interpretFail
  :: forall model r a.
     ( Member (AgentWidgets (Message model)) r
     , Member Fail r
     )
  => Sem r a
  -> Sem r a
interpretFail = intercept $ \case
  Fail msg -> do
    setStatus @(Message model) (Failed (T.pack msg))
    send (Fail msg)
