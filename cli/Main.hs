{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (main) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStr)
import qualified System.IO as IO
import Data.Default (Default)

import Polysemy
import Polysemy.Fail

import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter hiding (SystemPrompt)
import UniversalLLM.Providers.OpenAI (OpenRouter(..))
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.HTTP.Effects ()
import Runix.Cmd.Effects (Cmd)
import Runix.Logging.Effects (Logging)
import Runix.Secret.Effects (runSecret)

import Agent (SystemPrompt(..), UserPrompt(..), runRunixCode, responseText)
import Models (ModelDefaults(..), ClaudeSonnet45(..), GLM45Air(..), Qwen3Coder(..), Universal(..), claudeSonnet45ComposableProvider, glm45AirComposableProvider, qwen3CoderComposableProvider, universalComposableProvider)
import Config
import Data.Text (pack)
import Runner
import UI.UserInput (UserInput, ImplementsWidget(..), RenderRequest)

--------------------------------------------------------------------------------
-- CLI Widget Type
--------------------------------------------------------------------------------

-- | Phantom type for CLI widget system (non-interactive)
-- This is used to tag the UserInput effect as being for CLI use
data CLIWidget

-- | RenderRequest for CLI (never actually used since we always fail)
data instance RenderRequest CLIWidget a = CLIRenderRequest Text a

-- | ImplementsWidget instance for Text - the minimal instance we need
instance ImplementsWidget CLIWidget Text where
  askWidget prompt defaultValue = CLIRenderRequest prompt defaultValue

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code
--
-- Usage: echo "prompt" | runix-code [session-file]
--
-- Environment variables:
-- - RUNIX_MODEL: Model selection (claude-sonnet-45, glm-45-air, qwen3-coder)
-- - ANTHROPIC_OAUTH_TOKEN: Required for Claude models
-- - LLAMACPP_ENDPOINT: LlamaCpp server endpoint (default: http://localhost:8080/v1)
--
-- Reads prompt from stdin, runs the agent, displays response
-- If session-file is provided, loads history from it and saves updated history back
main :: IO ()
main = do
  -- Load configuration
  cfg <- loadConfig

  -- Read user input from stdin
  userInput <- TIO.getContents

  -- Check if we got any input
  if T.null (T.strip userInput)
    then do
      hPutStr IO.stderr "Error: No input provided. Usage: echo \"prompt\" | runix-code [session-file]\n"
      exitFailure
    else do
      -- Run with selected model
      result <- case cfgModelSelection cfg of
        UseClaudeSonnet45 -> runWithClaudeSonnet45 cfg userInput
        UseGLM45Air -> runWithGLM45Air cfg userInput
        UseQwen3Coder -> runWithQwen3Coder cfg userInput
        UseOpenRouter -> runWithOpenRouter cfg userInput

      case result of
        Right response -> TIO.putStrLn response
        Left errMsg -> hPutStr IO.stderr errMsg >> exitFailure

--------------------------------------------------------------------------------
-- Model-Specific Runners (Just Interpreter Setup)
--------------------------------------------------------------------------------

-- | Run with Claude Sonnet 4.5
runWithClaudeSonnet45 :: Config -> Text -> IO (Either String Text)
runWithClaudeSonnet45 cfg userInput = do
  -- Get OAuth token
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case maybeToken of
    Nothing -> do
      hPutStr IO.stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      return $ Left "Missing ANTHROPIC_OAUTH_TOKEN"
    Just tokenStr ->
      runWithEffects @CLIWidget $
        runSecret (pure tokenStr) $
          interpretAnthropicOAuth claudeSonnet45ComposableProvider Anthropic ClaudeSonnet45 $
            runAgent @Anthropic @ClaudeSonnet45 claudeSonnet45ComposableProvider cfg userInput

-- | Run with GLM4.5-air
runWithGLM45Air :: Config -> Text -> IO (Either String Text)
runWithGLM45Air cfg userInput =
  runWithEffects @CLIWidget $
    interpretLlamaCpp glm45AirComposableProvider (cfgLlamaCppEndpoint cfg) LlamaCpp GLM45Air $
      runAgent @LlamaCpp @GLM45Air glm45AirComposableProvider cfg userInput

-- | Run with Qwen3-Coder
runWithQwen3Coder :: Config -> Text -> IO (Either String Text)
runWithQwen3Coder cfg userInput =
  runWithEffects @CLIWidget $
    interpretLlamaCpp qwen3CoderComposableProvider (cfgLlamaCppEndpoint cfg) LlamaCpp Qwen3Coder $
      runAgent @LlamaCpp @Qwen3Coder qwen3CoderComposableProvider cfg userInput

-- | Run with OpenRouter
runWithOpenRouter :: Config -> Text -> IO (Either String Text)
runWithOpenRouter cfg userInput = do
  apiKey <- getOpenRouterApiKey
  modelName <- getOpenRouterModel
  runWithEffects @CLIWidget $
    runSecret (pure apiKey) $
      interpretOpenRouter universalComposableProvider OpenRouter (Universal (pack modelName)) $
        runAgent @OpenRouter @Universal universalComposableProvider cfg userInput

--------------------------------------------------------------------------------
-- Generic Agent Runner (Model-Agnostic)
--------------------------------------------------------------------------------

-- | Run the agent with any model/provider
--
-- This is completely generic - no model-specific code here.
-- It just loads config, runs the agent, and saves the session.
runAgent :: forall provider model s r.
            ( Member (LLM provider model) r
            , Members [FileSystemRead, FileSystemWrite] r
            , Member Grep r
            , Member Bash r
            , Member Logging r
            , Member Cmd r
            , Member Fail r
            , Member (UserInput CLIWidget) r
            , HasTools model provider
            , SupportsSystemPrompt provider
            , SupportsStreaming provider
            , ModelDefaults provider model
            , Monoid (ProviderRequest provider)
            , Default s
            )
         => ComposableProvider provider model s
         -> Config
         -> Text
         -> Sem r Text
runAgent composableProvider cfg userInput = do
  -- Load system prompt
  sysPromptText <- loadSystemPrompt
    "prompt/runix-code.md"
    "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."

  let sysPrompt = SystemPrompt sysPromptText
      userPrompt = UserPrompt userInput
      configs = defaultConfigs @provider @model

  -- Load session (if specified)
  initialHistory <- case cfgSessionFile cfg of
    Nothing -> return []
    Just sessionFile -> loadSession composableProvider sessionFile

  -- Run the agent
  (result, finalHistory) <- runRunixCode @provider @model @CLIWidget sysPrompt configs initialHistory userPrompt

  -- Save session (if specified)
  case cfgSessionFile cfg of
    Nothing -> return ()
    Just sessionFile -> saveSession composableProvider sessionFile finalHistory

  return $ responseText result
