-- | TUI entry point for runix-code
--
-- This module handles:
-- - Configuration loading
-- - Model/interpreter setup (runner selection)
-- - Wiring the agent to the UI
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Text as T
import Data.IORef
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as Exception

import Polysemy
import Polysemy.Error (runError, Error)

import UniversalLLM.Core.Types (Message(..))
import UniversalLLM (ProviderOf)

import Config
import Models
import Runner (loadSystemPrompt, createModelInterpreter, ModelInterpreter(..), runConfig, runHistory )
import Runix.Runner (filesystemIO, grepIO, bashIO, cmdIO, failLog)
import TUI.UI (runUI)
import Agent (runixCode, UserPrompt (UserPrompt), SystemPrompt (SystemPrompt))
import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter (withLLMCancellation)
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.Cmd.Effects (Cmd)
import Runix.HTTP.Effects (HTTP, HTTPStreaming, httpIO, httpIOStreaming, withRequestTimeout)
import Runix.Logging.Effects (Logging(..))
import Runix.Cancellation.Effects (Cancellation(..))
import Runix.Streaming.Effects (StreamChunk(..), emitChunk)
import Runix.Streaming.SSE (StreamingContent(..), extractContentFromChunk)
import UI.State (newUIVars, UIVars, waitForUserInput, userInputQueue, sendAgentEvent, readCancellationFlag, clearCancellationFlag, AgentEvent(..))
import UI.Interpreter (interpretUI)
import UI.LoggingInterpreter (interpretLoggingToUI)
import UI.UserInput (UserInput)
import UI.UserInput.Interpreter (interpretUserInput)
import UI.UserInput.InputWidget (TUIWidget)
import Control.Monad (forever)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, SupportsSystemPrompt)
import qualified Data.ByteString as BS
import qualified UI.Effects


--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code TUI
main :: IO ()
main = do
  -- Load configuration
  cfg <- loadConfig

  -- Create model interpreter
  ModelInterpreter{interpretModel} <- createModelInterpreter (cfgModelSelection cfg)

  -- Run UI with the interpreter
  -- The interpreter is now just a function we can pass around
  runUI (\refreshCallback -> buildUIRunner interpretModel refreshCallback)

--------------------------------------------------------------------------------
-- Agent Loop
--------------------------------------------------------------------------------

-- | Agent loop that processes user input from the UI
-- | Update history by sending AgentCompleteEvent
updateHistory :: forall model.
                 UIVars (Message model)
              -> IORef [Message model]
              -> [Message model]
              -> IO ()
updateHistory uiVars historyRef newHistory = do
  -- Update historyRef (source of truth)
  writeIORef historyRef newHistory
  -- Send event with new messages
  sendAgentEvent uiVars (AgentCompleteEvent newHistory)

agentLoop :: forall model.
             ( HasTools model
             , SupportsSystemPrompt (ProviderOf model)
             , ModelDefaults model
             )
          => UIVars (Message model)
          -> IORef [Message model]
          -> SystemPrompt
          -> (forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming] r => Sem (LLM model : r) a -> Sem r a)  -- Model interpreter
          -> IO ()
agentLoop uiVars historyRef sysPrompt modelInterpreter = forever $
  Exception.catch runOneIteration handleException
  where
    runOneIteration = do
      -- Wait for user input
      userInput <- atomically $ waitForUserInput (userInputQueue uiVars)

      -- Clear any previous cancellation flag before starting new request
      clearCancellationFlag uiVars

      -- Get current history
      currentHistory <- readIORef historyRef

      -- Send user message to UI immediately
      sendAgentEvent uiVars (UserMessageEvent (UserText userInput))

      -- Run the agent with model-specific default configs
      let configs = defaultConfigs @model
          runToIO' :: Sem (LLM model : Grep : FileSystemRead : FileSystemWrite : Bash : Cmd : HTTP : HTTPStreaming : StreamChunk BS.ByteString : Cancellation : Fail : Logging : UserInput TUIWidget : UI.Effects.UI : Error String : Embed IO : '[]) a -> IO (Either String a)
          runToIO' = runM . runError . interpretTUIEffects uiVars . modelInterpreter

      result <- runToIO' . withLLMCancellation . runConfig configs . runHistory currentHistory $
          (runixCode @model @TUIWidget sysPrompt (UserPrompt userInput))

      -- Always clear cancellation flag after request completes (whether success or error)
      clearCancellationFlag uiVars

      case result of
        Left err -> do
          -- Show error in UI
          sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Agent error: " ++ err))
          -- History already has user message from above, nothing more to do

        Right (_result, newHistory) -> do
          -- Update history with agent's response
          updateHistory uiVars historyRef newHistory

    handleException :: Exception.SomeException -> IO ()
    handleException e = do
      -- Catch any IO exception in this iteration and show in UI
      clearCancellationFlag uiVars
      sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Uncaught exception: " ++ Exception.displayException e))

--------------------------------------------------------------------------------
-- UI Runner Builder
--------------------------------------------------------------------------------

-- | Build a UI runner by composing model interpreter with UI effects
--
-- This combines:
-- 1. Model interpreter (LLM effect -> base effects)
-- 2. UI effects interpreter (base effects -> IO)
-- 3. Agent loop
buildUIRunner :: forall model.
                 ( HasTools model
                 , SupportsSystemPrompt (ProviderOf model)
                 , ModelDefaults model
                 )
              => (forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming] r => Sem (LLM model : r) a -> Sem r a)  -- Model interpreter
              -> (AgentEvent (Message model) -> IO ())  -- Refresh callback
              -> IO (UIVars (Message model))
buildUIRunner modelInterpreter refreshCallback = do
  uiVars <- newUIVars @(Message model) refreshCallback
  historyRef <- newIORef ([] :: [Message model])

  -- Load system prompt using the composed interpreter stack
  let runToIO' :: Sem (LLM model : Grep : FileSystemRead : FileSystemWrite : Bash : Cmd : HTTP : HTTPStreaming : StreamChunk BS.ByteString : Cancellation : Fail : Logging : UserInput TUIWidget : UI.Effects.UI : Error String : Embed IO : '[]) a -> IO (Either String a)
      runToIO' = runM . runError . interpretTUIEffects uiVars . modelInterpreter

  result <- runToIO' $ loadSystemPrompt "prompt/runix-code.md" "You are a helpful AI coding assistant."
  let sysPrompt = case result of
        Right txt -> SystemPrompt txt
        Left _ -> SystemPrompt "You are a helpful AI coding assistant."

  _ <- forkIO $ agentLoop uiVars historyRef sysPrompt modelInterpreter
  return uiVars

--------------------------------------------------------------------------------
-- Runner Creation
--------------------------------------------------------------------------------

-- | Reinterpret StreamChunk BS.ByteString to StreamChunk StreamingContent by extracting SSE content
reinterpretSSEChunks :: Sem (StreamChunk BS.ByteString : r) a
                     -> Sem (StreamChunk StreamingContent : r) a
reinterpretSSEChunks = reinterpret $ \case
  EmitChunk chunk ->
    case extractContentFromChunk chunk of
      Just content -> emitChunk content
      Nothing -> return ()  -- Ignore non-content chunks (like message_start events)

-- | Interpret StreamChunk StreamingContent by sending to UI
interpretStreamChunkToUI :: Member (Embed IO) r
                         => UIVars msg
                         -> Sem (StreamChunk StreamingContent : r) a
                         -> Sem r a
interpretStreamChunkToUI uiVars = interpret $ \case
  EmitChunk (StreamingText text) ->
    embed $ sendAgentEvent uiVars (StreamChunkEvent text)
  EmitChunk (StreamingReasoning reasoning) ->
    embed $ sendAgentEvent uiVars (StreamReasoningEvent reasoning)

-- | Interpret Cancellation effect for TUI
-- Reads the cancellation flag from UIVars (set by UI when user presses ESC)
-- The flag is checked at strategic points: before QueryLLM, between HTTP chunks
interpretCancellation :: Member (Embed IO) r
                      => UIVars msg
                      -> Sem (Cancellation : r) a
                      -> Sem r a
interpretCancellation uiVars = interpret $ \case
  IsCanceled -> do
    -- Check the STM flag atomically
    embed $ atomically $ readCancellationFlag uiVars

-- | Interpret all base effects for TUI agents
--
-- This builds the effect interpretation stack from the bottom up:
-- - File system, bash, cmd, grep (basic IO effects)
-- - HTTP (both streaming and non-streaming)
-- - SSE chunk extraction and streaming to UI
-- - Cancellation, logging, user input
-- - UI effects and error handling

interpretTUIEffects :: forall msg a.
                       UIVars msg
                    -> Sem (Grep
                         : FileSystemRead
                         : FileSystemWrite
                         : Bash
                         : Cmd
                         : HTTP
                         : HTTPStreaming
                         : StreamChunk BS.ByteString
                         : Cancellation
                         : Fail
                         : Logging
                         : UserInput TUIWidget
                         : UI.Effects.UI
                         : '[Error String, Embed IO]) a
                    -> Sem '[Error String, Embed IO] a
interpretTUIEffects uiVars =
  interpretUI uiVars
    . interpretUserInput uiVars        -- UserInput effect
    . interpretLoggingToUI
    . failLog
    . interpretCancellation uiVars     -- Handle Cancellation effect
    . interpretStreamChunkToUI uiVars  -- Handle StreamChunk Text
    . reinterpretSSEChunks              -- Convert StreamChunk BS -> StreamChunk Text
    . httpIOStreaming (withRequestTimeout 300)  -- Emit StreamChunk BS
    . httpIO (withRequestTimeout 300)           -- Handle non-streaming HTTP
    . cmdIO
    . bashIO
    . filesystemIO
    . grepIO


--------------------------------------------------------------------------------
-- Echo Agent (Placeholder)
--------------------------------------------------------------------------------

-- | Echo agent - placeholder that doesn't use LLM
--
-- Pure function: takes history and user input, returns new history.
-- For the real agent, this would call runRunixCode.
_echoAgent :: forall model r.
             [Message model]
          -> String
          -> Sem r [Message model]
_echoAgent currentHistory userInput =
  let userMsg = UserText (T.pack userInput)
      agentMsg = AssistantText (T.pack $ "Echo: " ++ userInput)
      newHistory = currentHistory ++ [userMsg, agentMsg]
  in return newHistory
