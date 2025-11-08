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
import Runix.Runner (filesystemIO, grepIO, bashIO, httpIO, withRequestTimeout, failLog)
import TUI.UI (runUI)
import Agent (runRunixCode, UserPrompt (UserPrompt), SystemPrompt (SystemPrompt))
import Runix.LLM.Effects (LLM)
import Runix.FileSystem.Effects (FileSystem)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.HTTP.Effects (HTTP)
import Runix.Logging.Effects (Logging(..))
import UI.Effects (UI, messageToDisplay)
import UI.State (newUIVars, UIVars, waitForUserInput, userInputQueue, patchMessages, appendLog, setStatus, uiStateVar, uiOutputHistory)
import UI.OutputHistory (patchOutputHistory)
import UI.Interpreter (interpretUI)
import UI.LoggingInterpreter (interpretLoggingToUI)
import Control.Monad (forever)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, SupportsSystemPrompt, ProviderImplementation)

--------------------------------------------------------------------------------
-- Existential wrapper for history + runner
--------------------------------------------------------------------------------

-- | Existentially hides the model/provider types while providing a way to run agents
-- The runner wraps a specific effect interpreter stack for this model/provider
data AgentRunner where
  AgentRunner :: forall model provider.
    ( HasTools model provider
    , SupportsSystemPrompt provider
    , ProviderImplementation provider model
    )
    => IORef [Message model provider]  -- History storage
    -> Runner model provider  -- Wrapped runner for this model/provider
    -> AgentRunner

-- | Type alias for a runner that can execute computations and return IO results
-- This is intentionally monomorphic - each runner works with its specific effect stack
-- Logging effect is reinterpreted as UI effect in the TUI
newtype Runner model provider = Runner
  { runWith :: forall a. (forall r. (Member (LLM provider model) r, Members '[FileSystem, Grep, Bash, HTTP, Logging, Fail] r) => Sem r a) -> IO (Either String a)
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
agentLoop :: forall model provider.
             ( HasTools model provider
             , SupportsSystemPrompt provider
             , ProviderImplementation provider model
             )
          => UIVars
          -> IORef [Message model provider]
          -> (forall a. (forall r. (Member (LLM provider model) r, Members '[FileSystem, Grep, Bash, HTTP, Logging, Fail] r) => Sem r a) -> IO (Either String a))
          -> IO ()
agentLoop uiVars historyRef runner = forever $ do
  -- Wait for user input
  userInput <- atomically $ waitForUserInput (userInputQueue uiVars)

  -- Get current history
  currentHistory <- readIORef historyRef

  -- Immediately show user message in UI before running agent
  let historyWithUser = currentHistory ++ [UserText userInput]
  currentOutput <- atomically $ uiOutputHistory <$> readTVar (uiStateVar uiVars)
  let patchedWithUser = patchOutputHistory historyWithUser (messageToDisplay @model @provider) currentOutput
  patchMessages uiVars patchedWithUser

  -- Run the agent
  result <- runner $ runRunixCode @provider @model
                       (SystemPrompt "you are a helpful agent")
                       currentHistory
                       (UserPrompt userInput)

  case result of
    Left err -> do
      -- Show error in UI
      appendLog uiVars (T.pack $ "Agent error: " ++ err)
      setStatus uiVars (T.pack "Error occurred")
    Right (_result, newHistory) -> do
      -- Update history
      writeIORef historyRef newHistory

      -- Patch output history with complete response (preserving logs)
      currentOutput2 <- atomically $ uiOutputHistory <$> readTVar (uiStateVar uiVars)
      let patchedOutput = patchOutputHistory newHistory (messageToDisplay @model @provider) currentOutput2
      patchMessages uiVars patchedOutput
      setStatus uiVars (T.pack "Ready")

--------------------------------------------------------------------------------
-- Runner Creation
--------------------------------------------------------------------------------

-- | Effect interpretation stack for TUI
-- Logging effect is reinterpreted as UI effect for display
runBaseEffects uiVars =
  runM
    . runError
    . interpretUI uiVars
    . interpretLoggingToUI
    . failLog
    . httpIO (withRequestTimeout 300)
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
