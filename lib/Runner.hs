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
import qualified Data.ByteString as BS
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
import Runix.Grep (Grep)
import Runix.Bash (Bash)
import Runix.Cmd (Cmds)
import Runix.HTTP (HTTP, HTTPStreaming)
import Runix.Logging (Logging)
import Runix.Cancellation (Cancellation, cancelNoop)
import Runix.PromptStore (PromptStore, promptStoreIO)
import Runix.Config (Config)
import qualified Runix.Config as ConfigEffect
import qualified Config as AppConfig
import qualified Runix.Logging as Log
import Data.Default (Default, def)

import UniversalLLM (Message, ComposableProvider, cpSerializeMessage, cpDeserializeMessage, ModelConfig)
import UniversalLLM (ProviderOf, Model(..), HasTools, SupportsSystemPrompt, SupportsStreaming)
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..))
import Runix.LLM (LLM)
import Runix.LLM.Interpreter (interpretAnthropicOAuth, interpretLlamaCpp, interpretOpenRouter, interpretZAI)
import Runix.Secret (runSecret)
import Runix.Streaming (ignoreChunks)
import UI.UserInput (UserInput, interpretUserInputFail)
import Models (ClaudeSonnet45(..), GLM45Air(..), Qwen3Coder(..), Universal(..), GLM45Air_ZAI(..), GLM46(..), GLM47(..), ZAI(..), ModelDefaults, claudeSonnet45ComposableProvider, glm45AirComposableProvider, qwen3CoderComposableProvider, universalComposableProvider, glm45AirZAIComposableProvider, glm46ComposableProvider, glm47ComposableProvider)
import Config (ModelSelection(..), getLlamaCppEndpoint, getOpenRouterApiKey, getOpenRouterModel, getZAIApiKey)
import qualified Runix.FileSystem.System as System.Effects

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
               => (forall r. Members '[UserInput widget, FileSystem, FileSystemRead, FileSystemWrite, Grep, Bash, Cmds, HTTP, HTTPStreaming, Logging, Fail, Embed IO, Cancellation, PromptStore] r
                   => Sem r a)
               -> IO (Either String a)
runWithEffects action =
  runM
    . runError
    . loggingIO
    . failLog
    . cancelNoop
    . interpretUserInputFail @widget
    . ignoreChunks @BS.ByteString
    . httpIOStreaming (withRequestTimeout 300)
    . httpIO (withRequestTimeout 300)
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
-- Interprets LLM effect and provides session serialization
data ModelInterpreter where
  ModelInterpreter :: forall model.
    ( Eq (Message model)
    , HasTools model
    , SupportsSystemPrompt (ProviderOf model)
    , ModelDefaults model
    , SupportsStreaming (ProviderOf model)
    ) =>
    { interpretModel :: forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming] r => Sem (LLM model : r) a -> Sem r a
    , miLoadSession :: forall r. (Members [FileSystem, FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> Sem r [Message model]
    , miSaveSession :: forall r. (Members [FileSystem, FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ()
    } -> ModelInterpreter

-- | Create a model-specific interpreter based on configuration
createModelInterpreter :: ModelSelection -> IO ModelInterpreter
createModelInterpreter UseClaudeSonnet45 = do
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case maybeToken of
    Nothing -> do
      hPutStr IO.stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      error "Missing ANTHROPIC_OAUTH_TOKEN"
    Just tokenStr ->
      return $ ModelInterpreter
        { interpretModel =
            runSecret (pure tokenStr)
              . interpretAnthropicOAuth claudeSonnet45ComposableProvider (Model ClaudeSonnet45 Anthropic) . raiseUnder
        , miLoadSession = loadSession claudeSonnet45ComposableProvider
        , miSaveSession = saveSession claudeSonnet45ComposableProvider
        }

createModelInterpreter UseGLM45Air = do
  endpoint <- getLlamaCppEndpoint
  return $ ModelInterpreter
    { interpretModel =
        interpretLlamaCpp glm45AirComposableProvider endpoint (Model GLM45Air LlamaCpp)
    , miLoadSession = loadSession glm45AirComposableProvider
    , miSaveSession = saveSession glm45AirComposableProvider
    }

createModelInterpreter UseQwen3Coder = do
  endpoint <- getLlamaCppEndpoint
  return $ ModelInterpreter
    { interpretModel =
        interpretLlamaCpp qwen3CoderComposableProvider endpoint (Model Qwen3Coder LlamaCpp)
    , miLoadSession = loadSession qwen3CoderComposableProvider
    , miSaveSession = saveSession qwen3CoderComposableProvider
    }

createModelInterpreter UseOpenRouter = do
  apiKey <- getOpenRouterApiKey
  modelName <- getOpenRouterModel
  return $ ModelInterpreter
    { interpretModel =
        runSecret (pure apiKey)
          . interpretOpenRouter universalComposableProvider (Model (Universal (T.pack modelName)) OpenRouter) . raiseUnder
    , miLoadSession = loadSession universalComposableProvider
    , miSaveSession = saveSession universalComposableProvider
    }

createModelInterpreter UseGLM45AirZAI = do
  apiKey <- getZAIApiKey
  return $ ModelInterpreter
    { interpretModel =
        runSecret (pure apiKey)
          . interpretZAI glm45AirZAIComposableProvider (Model GLM45Air_ZAI ZAI) . raiseUnder
    , miLoadSession = loadSession glm45AirZAIComposableProvider
    , miSaveSession = saveSession glm45AirZAIComposableProvider
    }

createModelInterpreter UseGLM46ZAI = do
  apiKey <- getZAIApiKey
  return $ ModelInterpreter
    { interpretModel =
        runSecret (pure apiKey)
          . interpretZAI glm46ComposableProvider (Model GLM46 ZAI) . raiseUnder
    , miLoadSession = loadSession glm46ComposableProvider
    , miSaveSession = saveSession glm46ComposableProvider
    }

createModelInterpreter UseGLM47ZAI = do
  apiKey <- getZAIApiKey
  return $ ModelInterpreter
    { interpretModel =
        runSecret (pure apiKey)
          . interpretZAI glm47ComposableProvider (Model GLM47 ZAI) . raiseUnder
    , miLoadSession = loadSession glm47ComposableProvider
    , miSaveSession = saveSession glm47ComposableProvider
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

