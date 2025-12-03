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
import Data.Text (Text)
import Data.Text (pack)
import Data.IORef
import System.Environment (lookupEnv)
import System.IO (hPutStr)
import qualified System.IO as IO
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Polysemy
import Polysemy.Error (runError, Error)
import Polysemy (interpret, embed)

import UniversalLLM.Core.Types (Message(..))
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
import Runix.HTTP.Effects (HTTP, httpIOStreaming, withRequestTimeout)
import Runix.Logging.Effects (Logging(..))
import Runix.Cancellation.Effects (Cancellation(..), isCanceled)
import Runix.Streaming.Effects (StreamChunk(..), emitChunk, ignoreChunks)
import Runix.Streaming.SSE (StreamingContent(..), extractContentFromChunk)
import UI.Effects (UI, messageToDisplay)
import UI.State (newUIVars, UIVars, waitForUserInput, userInputQueue, sendAgentEvent, readCancellationFlag, clearCancellationFlag, requestCancelFromUI, AgentEvent(..))
import UI.OutputHistory (LogLevel(..))
import UI.Interpreter (interpretUI)
import UI.LoggingInterpreter (interpretLoggingToUI)
import UI.UserInput (UserInput)
import UI.UserInput.Interpreter (interpretUserInput)
import UI.UserInput.InputWidget (TUIWidget)
import Control.Monad (forever)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, SupportsSystemPrompt, SupportsStreaming)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

--------------------------------------------------------------------------------
-- Existential wrapper for history + runner
--------------------------------------------------------------------------------

-- | Existentially hides the model/provider types while providing a way to run agents
-- The runner wraps a specific effect interpreter stack for this model/provider
data AgentRunner where
  AgentRunner :: forall model provider.
    ( HasTools model provider
    , SupportsSystemPrompt provider
    , SupportsStreaming provider
    , ModelDefaults provider model
    )
    => IORef [Message model provider]  -- History storage
    -> Runner model provider  -- Wrapped runner for this model/provider
    -> AgentRunner

-- | Type alias for a runner that can execute computations and return IO results
-- This is intentionally monomorphic - each runner works with its specific effect stack
-- Logging effect is reinterpreted as UI effect in the TUI
newtype Runner model provider = Runner
  { runWith :: forall a. (forall r. (Member (LLM provider model) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, Cmd, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a)
  }

-- | Wrapper for a function that builds everything and returns UIVars
-- Each model has its own concrete types, wrapped existentially
data AgentRunnerBuilder where
  AgentRunnerBuilder :: forall msg. Eq msg => (msg -> Text) -> (IO () -> IO (UIVars msg)) -> AgentRunnerBuilder

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

  -- Apply the builder: it contains render function and mkRunner
  case runnerBuilder of
    AgentRunnerBuilder renderMsg mkRunner -> do
      -- The builder hides the model/provider types
      runUI renderMsg mkRunner

--------------------------------------------------------------------------------
-- Agent Loop
--------------------------------------------------------------------------------

-- | Agent loop that processes user input from the UI
-- | Update history by sending AgentCompleteEvent
updateHistory :: forall model provider.
                 UIVars (Message model provider)
              -> IORef [Message model provider]
              -> [Message model provider]
              -> IO ()
updateHistory uiVars historyRef newHistory = do
  -- Update historyRef (source of truth)
  writeIORef historyRef newHistory
  -- Send event with new messages
  sendAgentEvent uiVars (AgentCompleteEvent newHistory)

agentLoop :: forall model provider.
             ( HasTools model provider
             , SupportsSystemPrompt provider
             , SupportsStreaming provider
             , ModelDefaults provider model
             )
          => UIVars (Message model provider)
          -> IORef [Message model provider]
          -> SystemPrompt
          -> (forall a. (forall r. (Member (LLM provider model) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, Cmd, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a))
          -> IO ()
agentLoop uiVars historyRef sysPrompt runner = forever $ do
  -- Wait for user input
  userInput <- atomically $ waitForUserInput (userInputQueue uiVars)

  -- Clear any previous cancellation flag before starting new request
  clearCancellationFlag uiVars

  -- Get current history
  currentHistory <- readIORef historyRef

  -- Immediately add user message to history and show in UI
  let historyWithUser = currentHistory ++ [UserText userInput]
  updateHistory uiVars historyRef historyWithUser

  -- Run the agent with model-specific default configs
  -- runixCode will add the user message to the history internally (so we pass the old history)
  let configs = defaultConfigs @provider @model
  result <- runner $ runRunixCode @provider @model @TUIWidget
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

-- | Effect interpretation stack for TUI
-- Logging effect is reinterpreted as UI effect for display
-- HTTP emits StreamChunk BS -> reinterpret to StreamingContent -> interpret to UI
runBaseEffects uiVars =
  runM
    . runError
    . interpretUI uiVars
    . interpretUserInput uiVars        -- UserInput effect
    . interpretLoggingToUI
    . failLog
    . interpretCancellation uiVars     -- Handle Cancellation effect
    . interpretStreamChunkToUI uiVars  -- Handle StreamChunk Text
    . reinterpretSSEChunks              -- Convert StreamChunk BS -> StreamChunk Text
    . httpIOStreaming (withRequestTimeout 300)  -- Emit StreamChunk BS
    . cmdIO
    . bashIO
    . filesystemIO
    . grepIO

-- | Create an AgentRunner builder based on the selected model
createRunnerBuilder :: ModelSelection -> Config -> AgentRunnerBuilder
createRunnerBuilder UseClaudeSonnet45 _cfg = AgentRunnerBuilder (messageToDisplay @ClaudeSonnet45 @Anthropic) $ \refreshCallback -> do
  uiVars <- newUIVars @(Message ClaudeSonnet45 Anthropic) refreshCallback
  historyRef <- newIORef ([] :: [Message ClaudeSonnet45 Anthropic])
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case maybeToken of
    Nothing -> do
      hPutStr IO.stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      error "Missing ANTHROPIC_OAUTH_TOKEN"
    Just tokenStr -> do
      let runAgent :: forall a. (forall r. (Member (LLM Anthropic ClaudeSonnet45) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, Cmd, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a)
          runAgent agent =
            runBaseEffects uiVars
              . runSecret (pure tokenStr)
              . interpretAnthropicOAuth claudeSonnet45ComposableProvider Anthropic ClaudeSonnet45
              $ withLLMCancellation agent
      -- Load system prompt (once at startup)
      sysPromptText <- runAgent $ loadSystemPrompt "prompt/runix-code.md" "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
      let sysPrompt = case sysPromptText of
            Right promptText -> SystemPrompt promptText
            Left err ->
              -- If loading fails, log error and use default
              (hPutStr IO.stderr $ "Warning: Failed to load system prompt: " ++ err ++ "\n") `seq`
              SystemPrompt "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
      -- Fork agent thread
      _ <- forkIO $ agentLoop uiVars historyRef sysPrompt runAgent
      return uiVars

createRunnerBuilder UseGLM45Air cfg = AgentRunnerBuilder (messageToDisplay @GLM45Air @LlamaCpp) $ \refreshCallback -> do
  uiVars <- newUIVars @(Message GLM45Air LlamaCpp) refreshCallback
  historyRef <- newIORef ([] :: [Message GLM45Air LlamaCpp])
  let runAgent :: forall a. (forall r. (Member (LLM LlamaCpp GLM45Air) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, Cmd, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a)
      runAgent agent =
        runBaseEffects uiVars
          . interpretLlamaCpp glm45AirComposableProvider (cfgLlamaCppEndpoint cfg) LlamaCpp GLM45Air
          $ withLLMCancellation agent
  -- Load system prompt
  sysPromptText <- runAgent $ loadSystemPrompt "prompt/runix-code.md" "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
  let sysPrompt = case sysPromptText of
        Right promptText -> SystemPrompt promptText
        Left err ->
          (hPutStr IO.stderr $ "Warning: Failed to load system prompt: " ++ err ++ "\n") `seq`
          SystemPrompt "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
  -- Fork agent thread
  _ <- forkIO $ agentLoop uiVars historyRef sysPrompt runAgent
  return uiVars

createRunnerBuilder UseQwen3Coder cfg = AgentRunnerBuilder (messageToDisplay @Qwen3Coder @LlamaCpp) $ \refreshCallback -> do
  uiVars <- newUIVars @(Message Qwen3Coder LlamaCpp) refreshCallback
  historyRef <- newIORef ([] :: [Message Qwen3Coder LlamaCpp])
  let runAgent :: forall a. (forall r. (Member (LLM LlamaCpp Qwen3Coder) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, Cmd, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a)
      runAgent agent =
        runBaseEffects uiVars
          . interpretLlamaCpp qwen3CoderComposableProvider (cfgLlamaCppEndpoint cfg) LlamaCpp Qwen3Coder
          $ withLLMCancellation agent
  -- Load system prompt
  sysPromptText <- runAgent $ loadSystemPrompt "prompt/runix-code.md" "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
  let sysPrompt = case sysPromptText of
        Right promptText -> SystemPrompt promptText
        Left err ->
          (hPutStr IO.stderr $ "Warning: Failed to load system prompt: " ++ err ++ "\n") `seq`
          SystemPrompt "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
  -- Fork agent thread
  _ <- forkIO $ agentLoop uiVars historyRef sysPrompt runAgent
  return uiVars

createRunnerBuilder UseOpenRouter _cfg = AgentRunnerBuilder (messageToDisplay @Universal @OpenRouter) $ \refreshCallback -> do
  uiVars <- newUIVars @(Message Universal OpenRouter) refreshCallback
  apiKey <- getOpenRouterApiKey
  modelName <- getOpenRouterModel
  historyRef <- newIORef ([] :: [Message Universal OpenRouter])
  let runAgent :: forall a. (forall r. (Member (LLM OpenRouter Universal) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, Cmd, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a)
      runAgent agent =
        runBaseEffects uiVars
          . runSecret (pure apiKey)
          . interpretOpenRouter universalComposableProvider OpenRouter (Universal (pack modelName))
          $ withLLMCancellation agent
  -- Load system prompt
  sysPromptText <- runAgent $ loadSystemPrompt "prompt/runix-code.md" "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
  let sysPrompt = case sysPromptText of
        Right promptText -> SystemPrompt promptText
        Left err ->
          (hPutStr IO.stderr $ "Warning: Failed to load system prompt: " ++ err ++ "\n") `seq`
          SystemPrompt "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."
  -- Fork agent thread
  _ <- forkIO $ agentLoop uiVars historyRef sysPrompt runAgent
  return uiVars

--------------------------------------------------------------------------------
-- Echo Agent (Placeholder)
--------------------------------------------------------------------------------

-- | Echo agent - placeholder that doesn't use LLM
--
-- Pure function: takes history and user input, returns new history.
-- For the real agent, this would call runRunixCode.
echoAgent :: forall model provider r.
             [Message model provider]
          -> String
          -> Sem r [Message model provider]
echoAgent currentHistory userInput =
  let userMsg = UserText (T.pack userInput)
      agentMsg = AssistantText (T.pack $ "Echo: " ++ userInput)
      newHistory = currentHistory ++ [userMsg, agentMsg]
  in return newHistory
