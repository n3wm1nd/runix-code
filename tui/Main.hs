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
import UniversalLLM.Providers.OpenAI (LlamaCpp(..))
import Runix.LLM.Interpreter (interpretAnthropicOAuth, interpretLlamaCpp)
import Runix.Secret.Effects (runSecret)

import Config
import Models
import Runix.Runner (filesystemIO, grepIO, bashIO, cmdIO, failLog)
import TUI.UI (runUI)
import Agent (runRunixCode, UserPrompt (UserPrompt), SystemPrompt (SystemPrompt))
import Runix.LLM.Effects (LLM)
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.HTTP.Effects (HTTP, httpIOStreaming, withRequestTimeout)
import Runix.Logging.Effects (Logging(..))
import Runix.Streaming.Effects (StreamChunk(..), emitChunk, ignoreChunks)
import Runix.Streaming.SSE (StreamingContent(..), extractContentFromChunk)
import UI.Effects (UI, messageToDisplay)
import UI.State (newUIVars, UIVars, waitForUserInput, userInputQueue, patchMessages, appendLog, setStatus, uiStateVar, uiOutputHistory, appendStreamingChunk, appendStreamingReasoning)
import UI.OutputHistory (patchOutputHistory, RenderedMessage(..), OutputMessage(ConversationMessage))
import UI.Interpreter (interpretUI)
import UI.LoggingInterpreter (interpretLoggingToUI)
import UI.UserInput (UserInput)
import UI.UserInput.Interpreter (interpretUserInput)
import UI.UserInput.InputWidget (TUIWidget)
import Control.Monad (forever)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, SupportsSystemPrompt, SupportsStreaming, ProviderImplementation)
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
    , ProviderImplementation provider model
    , ModelDefaults provider model
    )
    => IORef [Message model provider]  -- History storage
    -> Runner model provider  -- Wrapped runner for this model/provider
    -> AgentRunner

-- | Type alias for a runner that can execute computations and return IO results
-- This is intentionally monomorphic - each runner works with its specific effect stack
-- Logging effect is reinterpreted as UI effect in the TUI
newtype Runner model provider = Runner
  { runWith :: forall a. (forall r. (Member (LLM provider model) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a)
  }

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code TUI
main :: IO ()
main = do
  -- Load configuration
  cfg <- loadConfig

  -- Run UI with a function that creates UIVars after getting the refresh callback
  runUI $ \refreshCallback -> do
    -- Create UI state variables with the refresh callback
    uiVars <- newUIVars refreshCallback

    -- Create the appropriate runner
    agentRunner <- createRunner (cfgModelSelection cfg) cfg uiVars

    -- Start agent thread that processes user input
    case agentRunner of
      AgentRunner (historyRef :: IORef [Message model provider]) (Runner runner) -> do
        -- Fork agent thread
        _ <- forkIO $ agentLoop uiVars historyRef runner
        return uiVars

--------------------------------------------------------------------------------
-- Agent Loop
--------------------------------------------------------------------------------

-- | Agent loop that processes user input from the UI
-- | Update history and sync outputHistory - single source of truth
updateHistory :: forall model provider.
                 ProviderImplementation provider model
              => UIVars
              -> IORef [Message model provider]
              -> [Message model provider]
              -> (Message model provider -> Text)
              -> IO ()
updateHistory uiVars historyRef newHistory renderMsg = do
  -- Update historyRef (source of truth)
  writeIORef historyRef newHistory
  -- Patch outputHistory from the new history
  currentOutput <- atomically $ uiOutputHistory <$> readTVar (uiStateVar uiVars)
  let patchedOutput = patchOutputHistory newHistory renderMsg currentOutput
  patchMessages uiVars patchedOutput

agentLoop :: forall model provider.
             ( HasTools model provider
             , SupportsSystemPrompt provider
             , SupportsStreaming provider
             , ProviderImplementation provider model
             , ModelDefaults provider model
             )
          => UIVars
          -> IORef [Message model provider]
          -> (forall a. (forall r. (Member (LLM provider model) r, Members '[FileSystemRead, FileSystemWrite, Grep, Bash, HTTP, UserInput TUIWidget, Logging, Fail] r) => Sem r a) -> IO (Either String a))
          -> IO ()
agentLoop uiVars historyRef runner = forever $ do
  -- Wait for user input
  userInput <- atomically $ waitForUserInput (userInputQueue uiVars)

  -- Get current history
  currentHistory <- readIORef historyRef

  -- Immediately add user message to history and show in UI
  let historyWithUser = currentHistory ++ [UserText userInput]
  updateHistory uiVars historyRef historyWithUser (messageToDisplay @model @provider)

  -- Run the agent with model-specific default configs
  -- runixCode will add the user message to the history internally (so we pass the old history)
  let configs = defaultConfigs @provider @model
  result <- runner $ runRunixCode @provider @model
                       (SystemPrompt "you are a helpful agent")
                       configs
                       currentHistory
                       (UserPrompt userInput)

  case result of
    Left err -> do
      -- Show error in UI
      appendLog uiVars (T.pack $ "Agent error: " ++ err)
      setStatus uiVars (T.pack "Error occurred")
      -- History already has user message from above, nothing more to do

    Right (_result, newHistory) -> do
      -- Update history with agent's response
      updateHistory uiVars historyRef newHistory (messageToDisplay @model @provider)
      setStatus uiVars (T.pack "Ready")

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
                         => UIVars
                         -> Sem (StreamChunk StreamingContent : r) a
                         -> Sem r a
interpretStreamChunkToUI uiVars = interpret $ \case
  EmitChunk (StreamingText text) ->
    embed $ appendStreamingChunk uiVars text
  EmitChunk (StreamingReasoning reasoning) ->
    embed $ appendStreamingReasoning uiVars reasoning

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
    . interpretStreamChunkToUI uiVars  -- Handle StreamChunk Text
    . reinterpretSSEChunks              -- Convert StreamChunk BS -> StreamChunk Text
    . httpIOStreaming (withRequestTimeout 300)  -- Emit StreamChunk BS
    . cmdIO
    . bashIO
    . filesystemIO
    . grepIO

-- | Create an AgentRunner based on the selected model
createRunner :: ModelSelection -> Config -> UIVars -> IO AgentRunner
createRunner UseClaudeSonnet45 _cfg uiVars = do
  historyRef <- newIORef ([] :: [Message ClaudeSonnet45 Anthropic])
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case maybeToken of
    Nothing -> do
      hPutStr IO.stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      error "Missing ANTHROPIC_OAUTH_TOKEN"
    Just tokenStr -> do
      let runner = Runner $ \agent ->
            runBaseEffects uiVars
              . runSecret (pure tokenStr)
              . interpretAnthropicOAuth Anthropic ClaudeSonnet45
              $ agent
      return $ AgentRunner historyRef runner

createRunner UseGLM45Air cfg uiVars = do
  historyRef <- newIORef ([] :: [Message GLM45Air LlamaCpp])
  let runner = Runner $ \agent ->
        runBaseEffects uiVars
          . interpretLlamaCpp (cfgLlamaCppEndpoint cfg) LlamaCpp GLM45Air
          $ agent
  return $ AgentRunner historyRef runner

createRunner UseQwen3Coder cfg uiVars = do
  historyRef <- newIORef ([] :: [Message Qwen3Coder LlamaCpp])
  let runner = Runner $ \agent ->
        runBaseEffects uiVars
          . interpretLlamaCpp (cfgLlamaCppEndpoint cfg) LlamaCpp Qwen3Coder
          $ agent
  return $ AgentRunner historyRef runner

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
