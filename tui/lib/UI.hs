{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | UI effect for displaying information and prompting user input
--
-- This effect allows other effects (like Logging, LLM, etc.) to interact
-- with the UI without knowing about the concrete UI implementation (brick, CLI, web, etc.)
module UI where

import Polysemy
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import UniversalLLM (Message(..))
import Runix.Logging (Level(..))

-- | UI effect for interacting with the user interface
--
-- This effect provides operations for:
-- - Displaying log messages
-- - Updating status
-- - Showing messages in chat history
-- - Prompting the user for input (blocking)
data UI (m :: Type -> Type) a where
  -- | Display a log message in the UI with severity level
  LogMessage :: Level -> Text -> UI m ()

  -- | Update the status line
  UpdateStatus :: Text -> UI m ()

  -- | Prompt the user for input (blocks until user responds)
  PromptUser :: Text -> UI m Text

-- | Smart constructors for UI effect
logMessage :: Member UI r => Level -> Text -> Sem r ()
logMessage level msg = send (LogMessage level msg)

updateStatus :: Member UI r => Text -> Sem r ()
updateStatus status = send (UpdateStatus status)

promptUser :: Member UI r => Text -> Sem r Text
promptUser prompt = send (PromptUser prompt)

-- | Helper: Convert a Message to display text
messageToDisplay :: forall model. Message model -> Text
messageToDisplay (UserText t) = T.pack "You:\n  " <> T.replace "\n" "\n  " t
messageToDisplay (AssistantText t) = T.pack "Agent:\n  " <> T.replace "\n" "\n  " t
messageToDisplay (AssistantTool tc) = T.pack $ "Agent (tool call):\n  " ++ show tc
messageToDisplay (ToolResultMsg tr) = T.pack $ "Tool result:\n  " ++ show tr
messageToDisplay (UserImage desc _img) = T.pack $ "[User sent image: " ++ T.unpack desc ++ "]"
messageToDisplay (UserRequestJSON query j) =
  T.pack $ "[User sent JSON]:\n  Query: " ++ T.unpack query ++ "\n  " ++ show j
messageToDisplay (AssistantReasoning r) =
  T.pack "[Agent reasoning]:\n  " <> T.replace "\n" "\n  " r
messageToDisplay (AssistantJSON j) = T.pack $ "[Agent sent JSON]:\n  " ++ show j
messageToDisplay (SystemText t) = T.pack "[System]:\n  " <> T.replace "\n" "\n  " t
