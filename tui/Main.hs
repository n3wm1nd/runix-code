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
import Control.Monad (forever)
import qualified System.Directory as Dir

import Polysemy
import Polysemy.Error (runError, Error, catch)

import UniversalLLM.Core.Types (Message(..), ModelConfig(Streaming))
import UniversalLLM (ProviderOf)

import Config
import Models
import Runner (loadSystemPrompt, createModelInterpreter, ModelInterpreter(..), runConfig, runHistory )
import Runix.Runner (filesystemIO, grepIO, bashIO, cmdIO, failLog, filesystemReadIO, loggingIO)
import TUI.UI (runUI)
import Agent (runixCode, UserPrompt (UserPrompt), SystemPrompt (SystemPrompt))
import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter (withLLMCancellation)
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite, FileWatcher, fileWatcherIO, limitSubpathRead, limitSubpathWrite, filesystemReadIO, filesystemWriteIO)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.Cmd.Effects (Cmd)
import Runix.HTTP.Effects (HTTP, HTTPStreaming, httpIO, httpIOStreaming, withRequestTimeout)
import Runix.Logging.Effects (Logging(..))
import Runix.Cancellation.Effects (Cancellation(..))
import Runix.Streaming.Effects (StreamChunk)
import UI.State (newUIVars, UIVars, waitForUserInput, userInputQueue, readCancellationFlag, clearCancellationFlag, sendAgentEvent, AgentEvent(..), UserRequest(..), LLMSettings(..))
import UI.Interpreter (interpretUI)
import UI.LoggingInterpreter (interpretLoggingToUI)
import UI.UserInput (UserInput)
import UI.UserInput.Interpreter (interpretUserInput)
import UI.UserInput.InputWidget (TUIWidget)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, SupportsSystemPrompt, SupportsStreaming)
import qualified Data.ByteString as BS
import qualified UI.Effects
import UI.Streaming (reinterpretSSEChunks, interpretStreamChunkToUI, interpretCancellation)


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
             , SupportsStreaming (ProviderOf model)
             )
          => FilePath  -- CWD for security restrictions
          -> UIVars (Message model)
          -> IORef [Message model]
          -> SystemPrompt
          -> (forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming] r => Sem (LLM model : r) a -> Sem r a)  -- Model interpreter
          -> IO ()
agentLoop cwd uiVars historyRef sysPrompt modelInterpreter = do
  -- Run the entire agent loop inside Sem so FileWatcher state persists
  let runToIO' = runM . runError . interpretTUIEffects cwd uiVars . modelInterpreter

  result <- runToIO' $ forever $ runOneIteration

  -- Handle top-level errors (though forever should never return)
  case result of
    Left err -> do
      clearCancellationFlag uiVars
      sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Fatal error: " ++ err))
    Right _ -> return ()

  where
    runOneIteration = do
      -- Wait for user request (includes text + settings)
      UserRequest{userText, requestSettings} <- embed $ atomically $ waitForUserInput (userInputQueue uiVars)

      -- Clear any previous cancellation flag before starting new request
      embed $ clearCancellationFlag uiVars

      -- Get current history
      currentHistory <- embed $ readIORef historyRef

      -- Send user message to UI immediately
      embed $ sendAgentEvent uiVars (UserMessageEvent (UserText userText))

      -- Build configs using settings from the request
      let isStreaming (Streaming _) = True
          isStreaming _ = False

          baseConfigs = defaultConfigs @model
          -- Filter out Streaming from defaults, replace with request setting
          configsWithoutStreaming = filter (not . isStreaming) baseConfigs
          runtimeConfigs = configsWithoutStreaming ++ [Streaming (llmStreaming requestSettings)]

      -- Run agent with error catching
      catch
        (do (_result, newHistory) <- withLLMCancellation . runConfig runtimeConfigs . runHistory currentHistory $
                runixCode @model @TUIWidget sysPrompt (UserPrompt userText)
            embed $ updateHistory uiVars historyRef newHistory)
        (\err -> do
          -- Show error in UI
          embed $ sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Agent error: " ++ err)))

      -- Always clear cancellation flag after request completes (whether success or error)
      embed $ clearCancellationFlag uiVars

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
                 , SupportsStreaming (ProviderOf model)
                 )
              => (forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming] r => Sem (LLM model : r) a -> Sem r a)  -- Model interpreter
              -> (AgentEvent (Message model) -> IO ())  -- Refresh callback
              -> IO (UIVars (Message model))
buildUIRunner modelInterpreter refreshCallback = do
  -- Get current working directory for security restrictions
  cwd <- Dir.getCurrentDirectory

  uiVars <- newUIVars @(Message model) refreshCallback
  historyRef <- newIORef ([] :: [Message model])

  -- Load system prompt using the composed interpreter stack
  let runToIO' = runM . runError @String . loggingIO . failLog . filesystemReadIO

  result <- runToIO' $ loadSystemPrompt "prompt/runix-code.md" "You are a helpful AI coding assistant."
  let sysPrompt = case result of
        Right txt -> SystemPrompt txt
        Left _ -> SystemPrompt "You are a helpful AI coding assistant."

  _ <- forkIO $ agentLoop cwd uiVars historyRef sysPrompt modelInterpreter
  return uiVars

--------------------------------------------------------------------------------
-- Runner Creation
--------------------------------------------------------------------------------

-- | Interpret all base effects for TUI agents
--
-- This builds the effect interpretation stack from the bottom up:
-- - File system, bash, cmd, grep (basic IO effects)
-- - Security restrictions (limit to CWD)
-- - HTTP (both streaming and non-streaming)
-- - SSE chunk extraction and streaming to UI
-- - Cancellation, logging, user input
-- - UI effects and error handling


interpretTUIEffects :: (Member (Error String) r, Member (Embed IO) r)
                    => FilePath  -- CWD for security restrictions
                    -> UIVars msg
                    -> Sem (Grep
                         : Bash
                         : Cmd
                         : FileWatcher
                         : HTTP
                         : HTTPStreaming
                         : StreamChunk BS.ByteString
                         : Cancellation
                         : FileSystemWrite
                         : FileSystemRead
                         : Fail
                         : Logging
                         : UserInput TUIWidget
                         : UI.Effects.UI
                         : r) a
                    -> Sem r a
interpretTUIEffects cwd uiVars =
  interpretUI uiVars
    . interpretUserInput uiVars        -- UserInput effect
    . interpretLoggingToUI
    . failLog
    . filesystemReadIO                  -- Interpret read operations to IO (removes FileSystemRead)
    . limitSubpathRead cwd             -- SECURITY: Restrict reads to CWD (intercepts FileSystemRead)
    . filesystemWriteIO                 -- Interpret write operations to IO (removes FileSystemWrite)
    . limitSubpathWrite cwd            -- SECURITY: Restrict writes to CWD (intercepts FileSystemWrite)
    . interpretCancellation uiVars     -- Handle Cancellation effect
    . interpretStreamChunkToUI uiVars  -- Handle StreamChunk Text
    . reinterpretSSEChunks              -- Convert StreamChunk BS -> StreamChunk Text
    . httpIOStreaming (withRequestTimeout 300)  -- Emit StreamChunk BS
    . httpIO (withRequestTimeout 300)           -- Handle non-streaming HTTP
    . fileWatcherIO                     -- Interpret FileWatcher effect
    . cmdIO
    . bashIO
    . grepIO                            -- Interpret grep effect

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
