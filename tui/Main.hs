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
module Main (main) where

import qualified Data.Text as T
import Data.Text (pack)
import Data.IORef
import System.Environment (lookupEnv)
import System.IO (hPutStr)
import qualified System.IO as IO
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as Exception

import Polysemy
import Polysemy.Error (runError, Error)

import UniversalLLM.Core.Types (Message(..))
import UniversalLLM (ProviderOf, Model(..))
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))
import Runix.LLM.Interpreter (interpretAnthropicOAuth, interpretLlamaCpp, interpretOpenRouter, withLLMCancellation)
import Runix.Secret.Effects (runSecret)

import Config
import Models
import Runner (loadSystemPrompt)
import Runix.Runner (filesystemIO, grepIO, bashIO, cmdIO, failLog)
import TUI.UI (runUI)
import Agent (runRunixCode, UserPrompt (UserPrompt), SystemPrompt (SystemPrompt))
import Runix.LLM.Effects (LLM)
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

-- | Wrapper for a function that builds everything and returns UIVars
-- Each model has its own concrete types, wrapped existentially
data AgentRunnerBuilder where
  AgentRunnerBuilder :: forall model. Eq (Message model) =>
    ((AgentEvent (Message model) -> IO ()) -> IO (UIVars (Message model))) -> AgentRunnerBuilder

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

-- | Common effects used by all agents
type AgentEffects model =
  '[ LLM model
   , FileSystemRead
   , FileSystemWrite
   , Grep
   , Bash
   , Cmd
   , HTTP
   , HTTPStreaming
   , UserInput TUIWidget
   , Logging
   , Fail
   ]

-- | Runner type: interprets agent effects to IO with possible error
-- Newtype wrapper to properly handle rank-2 polymorphism
newtype AgentRunner model = AgentRunner
  { runAgent :: forall a. (forall r. Members (AgentEffects model) r => Sem r a)
              -> IO (Either String a)
  }

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code TUI
main :: IO ()
main = do
  -- Load configuration
  cfg <- loadConfig

  -- Create runner builder (returns a function that takes UIVars)
  let runnerBuilder = createRunnerBuilder (cfgModelSelection cfg) cfg

  -- Apply the builder: it contains the mkRunner function
  case runnerBuilder of
    AgentRunnerBuilder mkRunner -> do
      -- The builder hides the model/provider types
      runUI mkRunner

--------------------------------------------------------------------------------
-- System Prompt Loading
--------------------------------------------------------------------------------

-- | Load system prompt with fallback to default
loadSystemPromptWithFallback :: AgentRunner model -> IO SystemPrompt
loadSystemPromptWithFallback (AgentRunner runner) = do
  result <- runner $ loadSystemPrompt
              "prompt/runix-code.md"
              "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
  case result of
    Right promptText -> return $ SystemPrompt promptText
    Left err -> do
      hPutStr IO.stderr $ "Warning: Failed to load system prompt: " ++ err ++ "\n"
      return $ SystemPrompt "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."

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
          -> AgentRunner model
          -> IO ()
agentLoop uiVars historyRef sysPrompt (AgentRunner runner) = forever $
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
      result <- runner $ runRunixCode @model @TUIWidget
                           sysPrompt
                           configs
                           currentHistory
                           (UserPrompt userInput)

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
-- Generic Runner Builder
--------------------------------------------------------------------------------

-- | Generic runner builder that handles the common pattern for all models
--
-- This function captures the repetitive structure:
-- 1. Create UIVars and history
-- 2. Build the effect interpreter stack
-- 3. Load system prompt
-- 4. Fork the agent loop
-- 5. Return UIVars
buildModelRunner :: forall model.
                    ( HasTools model
                    , SupportsSystemPrompt (ProviderOf model)
                    , ModelDefaults model
                    )
                 => (UIVars (Message model) -> AgentRunner model)
                 -- ^ Function that creates the model-specific runner given UIVars
                 -> (AgentEvent (Message model) -> IO ())
                 -- ^ Refresh callback from UI
                 -> IO (UIVars (Message model))
buildModelRunner mkRunner refreshCallback = do
  uiVars <- newUIVars @(Message model) refreshCallback
  historyRef <- newIORef ([] :: [Message model])
  let runner = mkRunner uiVars
  sysPrompt <- loadSystemPromptWithFallback runner
  _ <- forkIO $ agentLoop uiVars historyRef sysPrompt runner
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

interpretTUIEffects :: (Member (Error String) r, Member (Embed IO) r)
                    => UIVars msg
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
                         : r) a
                    -> Sem r a
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

-- | Run the final effect stack to IO
runToIO :: Sem '[Error String, Embed IO] a -> IO (Either String a)
runToIO = runM . runError

-- | Create an AgentRunner builder based on the selected model
createRunnerBuilder :: ModelSelection -> Config -> AgentRunnerBuilder
createRunnerBuilder UseClaudeSonnet45 _cfg = AgentRunnerBuilder $ \refreshCallback -> do
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case maybeToken of
    Nothing -> do
      hPutStr IO.stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      error "Missing ANTHROPIC_OAUTH_TOKEN"
    Just tokenStr ->
      buildModelRunner @(Model ClaudeSonnet45 Anthropic)
        (\uiVars -> AgentRunner $ \agent ->
            runToIO
              . interpretTUIEffects uiVars
              . runSecret (pure tokenStr)
              . interpretAnthropicOAuth claudeSonnet45ComposableProvider (Model ClaudeSonnet45 Anthropic)
              $ withLLMCancellation agent)
        refreshCallback

createRunnerBuilder UseGLM45Air cfg = AgentRunnerBuilder $ \refreshCallback ->
  buildModelRunner @(Model GLM45Air LlamaCpp)
    (\uiVars -> AgentRunner $ \agent ->
        runToIO
          . interpretTUIEffects uiVars
          . interpretLlamaCpp glm45AirComposableProvider (cfgLlamaCppEndpoint cfg) (Model GLM45Air LlamaCpp)
          $ withLLMCancellation agent)
    refreshCallback

createRunnerBuilder UseQwen3Coder cfg = AgentRunnerBuilder $ \refreshCallback ->
  buildModelRunner @(Model Qwen3Coder LlamaCpp)
    (\uiVars -> AgentRunner $ \agent ->
        runToIO
          . interpretTUIEffects uiVars
          . interpretLlamaCpp qwen3CoderComposableProvider (cfgLlamaCppEndpoint cfg) (Model Qwen3Coder LlamaCpp)
          $ withLLMCancellation agent)
    refreshCallback

createRunnerBuilder UseOpenRouter _cfg = AgentRunnerBuilder $ \refreshCallback -> do
  apiKey <- getOpenRouterApiKey
  modelName <- getOpenRouterModel
  buildModelRunner @(Model Universal OpenRouter)
    (\uiVars -> AgentRunner $ \agent ->
        runToIO
          . interpretTUIEffects uiVars
          . runSecret (pure apiKey)
          . interpretOpenRouter universalComposableProvider (Model (Universal (pack modelName)) OpenRouter)
          $ withLLMCancellation agent)
    refreshCallback

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
