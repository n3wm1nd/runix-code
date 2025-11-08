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

  -- Create the appropriate runner based on config, and start UI in the same scope
  -- where the concrete types are known
  agentRunner <- createRunner (cfgModelSelection cfg) cfg
  case agentRunner of
    AgentRunner (historyRef :: IORef [Message model provider]) (Runner runner) -> do
      -- Now we're in a scope where model and provider are concrete and in scope
      -- (via ScopedTypeVariables from the pattern match)

      let handleInput userInput = do
            currentHistory <- readIORef historyRef
            -- Pass a polymorphic computation to the runner
            -- The runner will apply it to its specific effect stack
            result <- runner $ runRunixCode @provider @model
                                 (SystemPrompt "you are a helpful agent")
                                 currentHistory
                                 (UserPrompt (T.pack userInput))
            case result of
              Left err -> do
                hPutStr IO.stderr $ "Agent error: " ++ err ++ "\n"
                return currentHistory  -- Return current history on error
              Right (_result, newHistory) -> do
                writeIORef historyRef newHistory
                return newHistory

      initialHistory <- readIORef historyRef
      runUI initialHistory handleInput

--------------------------------------------------------------------------------
-- Runner Creation
--------------------------------------------------------------------------------

-- | Common effect interpretation stack
-- Takes a Sem computation with the base effects and runs it to IO
runBaseEffects =
  runM
    . runError
    . interpretLoggingCustom
    . failLog
    . httpIO (withRequestTimeout 300)
    . bashIO
    . filesystemIO
    . grepIO

-- | Custom logging interpreter that discards logs
-- TODO: Collect logs to pass to brick UI
interpretLoggingCustom :: Sem (Logging : r) a -> Sem r a
interpretLoggingCustom = interpret $ \case
  Info _callStack _msg   -> pure ()
  Warning _callStack _msg -> pure ()
  Error _callStack _msg  -> pure ()

-- | Create an AgentRunner based on the selected model
createRunner :: ModelSelection -> Config -> IO AgentRunner
createRunner UseClaudeSonnet45 _cfg = do
  historyRef <- newIORef ([] :: [Message ClaudeSonnet45 Anthropic])
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case maybeToken of
    Nothing -> do
      hPutStr IO.stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      error "Missing ANTHROPIC_OAUTH_TOKEN"
    Just tokenStr -> do
      let runner = Runner $ \agent ->
            runBaseEffects
              . runSecret (pure tokenStr)
              . interpretAnthropicOAuth Anthropic ClaudeSonnet45
              $ agent
      return $ AgentRunner historyRef runner

createRunner UseGLM45Air cfg = do
  historyRef <- newIORef ([] :: [Message GLM45Air LlamaCpp])
  let runner = Runner $ \agent ->
        runBaseEffects
          . interpretLlamaCpp (cfgLlamaCppEndpoint cfg) LlamaCpp GLM45Air
          $ agent
  return $ AgentRunner historyRef runner

createRunner UseQwen3Coder cfg = do
  historyRef <- newIORef ([] :: [Message Qwen3Coder LlamaCpp])
  let runner = Runner $ \agent ->
        runBaseEffects
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
