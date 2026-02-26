{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | Runtime execution helpers for runix-code
--
-- This module provides generic helpers for:
-- - Loading/saving sessions (using FileSystem effect)
-- - Loading system prompts (using FileSystem effect)
-- - Running interpreter stacks
-- - Model-specific runner builders
-- - Generic agent runner for stateful agents
--
-- Everything is generic over model/provider and the actual action to run.
module Runner
  ( -- * Session Management
    loadSession
  , saveSession
    -- * System Prompt Loading
  , loadSystemPrompt
    -- * Interpreter Stack Helpers
  , runWithEffects
    -- * Model Interpreter
  , createModelInterpreter
  , ModelInterpreter(..)
    -- * Generic Agent Runner
  , runConfigHistory
  , runConfig
  , runHistory
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import GHC.Stack
import System.Environment (lookupEnv)
import System.IO (hPutStr)
import qualified System.IO as IO

import Polysemy
import Polysemy.Fail
import Polysemy.Error
import Polysemy.State (State, runState)
import Polysemy.Reader (Reader, runReader)

import Runix.Runner (grepIO, bashIO, cmdsIO, httpIO, httpIOStreaming, withRequestTimeout, loggingIO, failLog)
import Runix.FileSystem.Simple (FileSystem, FileSystemRead, FileSystemWrite, filesystemIO, readFile, writeFile, fileExists)
import Runix.Grep (GrepSystem)
import Runix.Bash (Bash)
import Runix.Cmd (Cmds)
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.Logging (Logging)
import Runix.Cancellation (Cancellation, cancelNoop)
import Runix.PromptStore (PromptStore, promptStoreIO)
import qualified Runix.Logging as Log
import Data.Default (Default, def)

import UniversalLLM (Message, ComposableProvider, cpSerializeMessage, cpDeserializeMessage, ModelConfig)
import UniversalLLM (ProviderOf, Model(..), HasTools, SupportsSystemPrompt, SupportsStreaming)
import UniversalLLM.Providers.Anthropic (AnthropicOAuth(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretLLMStreaming, interpretLLM, AnthropicOAuthAuth(..), LlamaCppAuth(..), OpenRouterAuth(..), ZAIAuth(..))
import Runix.RestAPI (restapiHTTP)
import UI.UserInput (UserInput, interpretUserInputFail)
import Models (ClaudeSonnet45(..), ClaudeHaiku45(..), ClaudeOpus46(..), GLM45Air(..), MinimaxM25(..), Qwen35_122B(..), Qwen3CoderNext(..), UniversalWithTools(..), GLM46(..), GLM47(..), GLM5(..), ZAI(..), ModelDefaults, claudeSonnet45OAuth, claudeHaiku45OAuth, claudeOpus46OAuth, glm45AirLlamaCpp, minimaxM25LlamaCpp, qwen35_122B, qwen3Coder, universalWithTools, glm45AirZAI, glm46, glm47, glm5)
import Config (ModelSelection(..), getLlamaCppEndpoint, getOpenRouterApiKey, getOpenRouterModel, getZAIApiKey)
import qualified Runix.FileSystem.System as System.Effects

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

requireEnv :: String -> IO String
requireEnv name = do
  val <- lookupEnv name
  case val of
    Nothing -> do
      hPutStr IO.stderr $ "Error: " <> name <> " environment variable is not set\n"
      error $ "Missing " <> name
    Just v -> return v

--------------------------------------------------------------------------------
-- Session Management (Effect-Based)
--------------------------------------------------------------------------------

-- | Load session from file
--
-- Sessions are model-agnostic - Message type parameters are phantom types.
-- We deserialize with a specific model type for type safety, but
-- the messages can be used with any compatible model.
loadSession :: forall model s r.
               ( Members [FileSystem, FileSystemRead, FileSystemWrite] r
               , Member Logging r
               , Member Fail r
               , Default s
               )
            => ComposableProvider model s
            -> FilePath
            -> Sem r [Message model]
loadSession composableProvider path = do
  exists <- fileExists path
  if not exists
    then do
      Log.warning $ T.pack $ "Session file does not exist: " <> path
      return []
    else do
      contents <- readFile path  -- readFile returns ByteString (strict)
      case Aeson.decode (BSL.fromStrict contents) of
        Nothing -> do
          Log.warning "Failed to parse session JSON, starting with empty history"
          return []
        Just val -> case deserializeMessages composableProvider val of
          Left err -> do
            Log.warning $ T.pack $ "Failed to deserialize session: " <> err
            return []
          Right msgs -> do
            Log.info $ T.pack $ "Loaded " <> show (length msgs) <> " messages from session"
            return msgs

-- | Save session to file
saveSession :: forall model s r.
               ( Members [FileSystemRead, FileSystemWrite, Fail] r
               , Member Logging r
               , Default s
               )
            => ComposableProvider model s
            -> FilePath
            -> [Message model]
            -> Sem r ()
saveSession composableProvider path msgs = do
  let json = serializeMessages composableProvider msgs
      encoded = BSL.toStrict $ Aeson.encode json  -- Convert to strict
  writeFile path encoded
  Log.info $ T.pack $ "Saved " <> show (length msgs) <> " messages to session"

-- | Serialize messages to JSON (internal helper)
serializeMessages :: forall model s.
                     (Default s)
                  => ComposableProvider model s
                  -> [Message model]
                  -> Aeson.Value
serializeMessages composableProvider msgs =
  let -- Apply with undefined values and default state since cpSerializeMessage is type-driven
      handlers = composableProvider (undefined :: model) ([] :: [ModelConfig model]) def
      serialized = [(i, v) | (i, Just v) <- zip [0::Integer ..] (map (cpSerializeMessage handlers) msgs)]
      failed = length msgs - length serialized
  in if failed > 0
     then Prelude.error $ "Failed to serialize " <> show failed <> " out of " <> show (length msgs) <> " messages"
     else Aeson.toJSON (map snd serialized)

-- | Deserialize messages from JSON (internal helper)
deserializeMessages :: forall model s.
                       ( Default s)
                    =>ComposableProvider model s
                    -> Aeson.Value
                    -> Either String [Message model]
deserializeMessages composableProvider val = case val of
  Aeson.Array arr -> do
    -- Get handlers using undefined for model/configs and default state since deserialization doesn't depend on them
    let -- We apply with undefined values and default state since cpDeserializeMessage is type-driven and doesn't use the runtime values
        handlers = composableProvider (undefined :: model) ([] :: [ModelConfig model]) def
        arrList = Vector.toList arr
        results = [(i, v, cpDeserializeMessage handlers v) | (i, v) <- zip [0::Integer ..] arrList]
        messages = [msg | (_, _, Just msg) <- results]
        failed = [(i, v) | (i, v, Nothing) <- results]
    if null failed
      then Right messages
      else Left $ "Failed to deserialize " <> show (length failed) <> " messages"
  _ -> Left "Expected JSON array"

--------------------------------------------------------------------------------
-- System Prompt Loading
--------------------------------------------------------------------------------

-- | Load system prompt from file or use default
loadSystemPrompt :: (Members [FileSystem, FileSystemRead, Fail] r, Member Logging r)
                 => FilePath  -- ^ Path to system prompt file
                 -> Text      -- ^ Default prompt if file doesn't exist
                 -> Sem r Text
loadSystemPrompt promptFile defaultPrompt = do
  exists <- fileExists promptFile
  if exists
    then do
      Log.info $ T.pack $ "Using system prompt from " <> promptFile
      contents <- readFile promptFile
      return $ TE.decodeUtf8 contents
    else do
      Log.warning $ T.pack $ promptFile <> " not found, using default system prompt"
      return defaultPrompt

--------------------------------------------------------------------------------
-- Interpreter Stack Runner
--------------------------------------------------------------------------------

-- | Run an action with the standard runix-code effect stack
--
-- This is a generic helper that interprets all the effects needed for
-- runix-code. The action itself is provided by the caller.
runWithEffects :: forall widget a. HasCallStack
               => (forall r. Members '[UserInput widget, FileSystem, FileSystemRead, FileSystemWrite, Runix.Grep.GrepSystem, Bash, Cmds, HTTP, HTTPStreaming, Logging, Fail, Embed IO, Cancellation, PromptStore] r
                   => Sem r a)
               -> IO (Either String a)
runWithEffects action =
  runM
    . runError
    . loggingIO
    . failLog
    . cancelNoop
    . interpretUserInputFail @widget
    . httpIO (withRequestTimeout 300)
    . httpIOStreaming (withRequestTimeout 300)
    . cmdsIO
    . bashIO
    . promptStoreIO
    . filesystemIO
    . System.Effects.filesystemIO . grepIO -- TODO: update as grep gets updated, temporary only
    $ action

--------------------------------------------------------------------------------
-- Model Interpreter
--------------------------------------------------------------------------------

-- | Wrapper for model-specific interpreter
-- Provides both streaming and non-streaming LLM interpreters, plus session serialization.
-- The streaming interpreter is used by 'interpretAsWidget' in the TUI (viewport-scoped).
-- The non-streaming interpreter is used by the CLI.
data ModelInterpreter where
  ModelInterpreter :: forall model.
    ( Eq (Message model)
    , HasTools model
    , SupportsSystemPrompt (ProviderOf model)
    , ModelDefaults model
    , SupportsStreaming (ProviderOf model)
    ) =>
    { interpretModelStreaming :: forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming, Cancellation] r => Sem (LLM model : r) a -> Sem r a
    , interpretModelNonStreaming :: forall r a. Members [Fail, Embed IO, HTTP] r => Sem (LLM model : r) a -> Sem r a
    , miLoadSession :: forall r. (Members [FileSystem, FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> Sem r [Message model]
    , miSaveSession :: forall r. (Members [FileSystem, FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ()
    } -> ModelInterpreter

-- | Create a model-specific interpreter based on configuration
createModelInterpreter :: ModelSelection -> IO ModelInterpreter
createModelInterpreter UseClaudeSonnet45 = do
  token <- requireEnv "ANTHROPIC_OAUTH_TOKEN"
  let auth = AnthropicOAuthAuth token
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth claudeSonnet45OAuth (Model ClaudeSonnet45 AnthropicOAuth) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @AnthropicOAuthAuth claudeSonnet45OAuth (Model ClaudeSonnet45 AnthropicOAuth) . raiseUnder
    , miLoadSession = loadSession claudeSonnet45OAuth
    , miSaveSession = saveSession claudeSonnet45OAuth
    }

createModelInterpreter UseClaudeHaiku45 = do
  token <- requireEnv "ANTHROPIC_OAUTH_TOKEN"
  let auth = AnthropicOAuthAuth token
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth claudeHaiku45OAuth (Model ClaudeHaiku45 AnthropicOAuth) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @AnthropicOAuthAuth claudeHaiku45OAuth (Model ClaudeHaiku45 AnthropicOAuth) . raiseUnder
    , miLoadSession = loadSession claudeHaiku45OAuth
    , miSaveSession = saveSession claudeHaiku45OAuth
    }

createModelInterpreter UseClaudeOpus46 = do
  token <- requireEnv "ANTHROPIC_OAUTH_TOKEN"
  let auth = AnthropicOAuthAuth token
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth claudeOpus46OAuth (Model ClaudeOpus46 AnthropicOAuth) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @AnthropicOAuthAuth claudeOpus46OAuth (Model ClaudeOpus46 AnthropicOAuth) . raiseUnder
    , miLoadSession = loadSession claudeOpus46OAuth
    , miSaveSession = saveSession claudeOpus46OAuth
    }

createModelInterpreter UseGLM45Air = do
  endpoint <- getLlamaCppEndpoint
  let auth = LlamaCppAuth endpoint
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth glm45AirLlamaCpp (Model GLM45Air LlamaCpp) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @LlamaCppAuth glm45AirLlamaCpp (Model GLM45Air LlamaCpp) . raiseUnder
    , miLoadSession = loadSession glm45AirLlamaCpp
    , miSaveSession = saveSession glm45AirLlamaCpp
    }

createModelInterpreter UseMinimaxM25 = do
  endpoint <- getLlamaCppEndpoint
  let auth = LlamaCppAuth endpoint
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth minimaxM25LlamaCpp (Model MinimaxM25 LlamaCpp) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @LlamaCppAuth minimaxM25LlamaCpp (Model MinimaxM25 LlamaCpp) . raiseUnder
    , miLoadSession = loadSession minimaxM25LlamaCpp
    , miSaveSession = saveSession minimaxM25LlamaCpp
    }

createModelInterpreter UseQwen35 = do
  endpoint <- getLlamaCppEndpoint
  let auth = LlamaCppAuth endpoint
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth qwen35_122B (Model Qwen35_122B LlamaCpp) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @LlamaCppAuth qwen35_122B (Model Qwen35_122B LlamaCpp) . raiseUnder
    , miLoadSession = loadSession qwen35_122B
    , miSaveSession = saveSession qwen35_122B
    }

createModelInterpreter UseQwen3Coder = do
  endpoint <- getLlamaCppEndpoint
  let auth = LlamaCppAuth endpoint
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth qwen3Coder (Model Qwen3CoderNext LlamaCpp) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @LlamaCppAuth qwen3Coder (Model Qwen3CoderNext LlamaCpp) . raiseUnder
    , miLoadSession = loadSession qwen3Coder
    , miSaveSession = saveSession qwen3Coder
    }

createModelInterpreter UseOpenRouter = do
  apiKey <- getOpenRouterApiKey
  modelName <- getOpenRouterModel
  let auth = OpenRouterAuth apiKey
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth universalWithTools (Model (UniversalWithTools (T.pack modelName)) OpenRouter) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @OpenRouterAuth universalWithTools (Model (UniversalWithTools (T.pack modelName)) OpenRouter) . raiseUnder
    , miLoadSession = loadSession universalWithTools
    , miSaveSession = saveSession universalWithTools
    }

createModelInterpreter UseGLM45AirZAI = do
  apiKey <- getZAIApiKey
  let auth = ZAIAuth apiKey
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth glm45AirZAI (Model GLM45Air ZAI) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @ZAIAuth glm45AirZAI (Model GLM45Air ZAI) . raiseUnder
    , miLoadSession = loadSession glm45AirZAI
    , miSaveSession = saveSession glm45AirZAI
    }

createModelInterpreter UseGLM46ZAI = do
  apiKey <- getZAIApiKey
  let auth = ZAIAuth apiKey
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth glm46 (Model GLM46 ZAI) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @ZAIAuth glm46 (Model GLM46 ZAI) . raiseUnder
    , miLoadSession = loadSession glm46
    , miSaveSession = saveSession glm46
    }

createModelInterpreter UseGLM47ZAI = do
  apiKey <- getZAIApiKey
  let auth = ZAIAuth apiKey
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth glm47 (Model GLM47 ZAI) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @ZAIAuth glm47 (Model GLM47 ZAI) . raiseUnder
    , miLoadSession = loadSession glm47
    , miSaveSession = saveSession glm47
    }

createModelInterpreter UseGLM5ZAI = do
  apiKey <- getZAIApiKey
  let auth = ZAIAuth apiKey
  return $ ModelInterpreter
    { interpretModelStreaming =
        restapiHTTP auth
          . interpretLLMStreaming auth glm5 (Model GLM5 ZAI) . raiseUnder
    , interpretModelNonStreaming =
        restapiHTTP auth
          . interpretLLM @ZAIAuth glm5 (Model GLM5 ZAI) . raiseUnder
    , miLoadSession = loadSession glm5
    , miSaveSession = saveSession glm5
    }

--------------------------------------------------------------------------------
-- Generic Agent Runner
--------------------------------------------------------------------------------

-- | Run an agent-like function with State and Reader effects
--
-- This is a generalized version of what runRunixCode does:
-- - Provides State for message history
-- - Provides Reader for model configs
-- - Runs the agent action and returns both the result and final history
runConfigHistory
  :: forall model result r.
     [ModelConfig model]                                 -- ^ Model configuration
  -> [Message model]                                -- ^ Initial message history
  -> Sem (State [Message model] : Reader [ModelConfig model] : r) result  -- ^ Agent action
  -> Sem r (result, [Message model])
runConfigHistory configs initialHistory agentAction = 
  runConfig configs . runHistory initialHistory  $ agentAction

runHistory
  :: forall model result r.
    [Message model]                                -- ^ Initial message history
  -> Sem (State [Message model] : r) result  -- ^ Agent action
  -> Sem r (result, [Message model])
runHistory initialHistory agentAction = do
  (finalHistory, result) <- runState initialHistory $ agentAction
  return (result, finalHistory)

runConfig
  :: forall model result r.
    [ModelConfig model]                                -- ^ Model configuration
  -> Sem (Reader [ModelConfig model] : r) result  -- ^ Agent action
  -> Sem r result
runConfig configs agentAction = do
  runReader configs $ agentAction

