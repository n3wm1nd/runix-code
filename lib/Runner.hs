{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

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
  , ModelInterpreter(..)
    -- * Model Registry
  , ModelEntry(..)
  , entryFields
  , entrySetField
  , entryToggleField
  , entryResetConfig
  , entryInterpreter
  , buildAvailableModels
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
import Data.Maybe (catMaybes)
import GHC.Generics (Generic, Rep)
import GHC.Stack
import System.Environment (lookupEnv)

import Polysemy
import Polysemy.Fail
import Polysemy.Error
import Polysemy.State (State, runState)
import Polysemy.Reader (Reader, runReader)

import Runix.Runner (grepIO, bashIO, cmdsIO, httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.FileSystem.Simple (FileSystem, FileSystemRead, FileSystemWrite, filesystemIO, readFile, writeFile, fileExists)
import Runix.Grep (GrepSystem)
import Runix.Bash (Bash)
import Runix.Cmd (Cmds)
import Runix.HTTP (HTTP)
import Runix.Logging (Logging)
import Runix.PromptStore (PromptStore, promptStoreIO)
import qualified Runix.Logging as Log
import Data.Default (Default, def)

import UniversalLLM (Message, ComposableProvider, cpSerializeMessage, cpDeserializeMessage, ModelConfig, ModelName, HasStreaming, ProviderRequest, ProviderResponse)
import UniversalLLM (ProviderOf, Model(..), HasTools, SupportsSystemPrompt)
import UniversalLLM.Settings (SettingField, SettingValue, ConfigFor, GSettingFields, GSetField, GToggleField, GDefault, ModelSettings, settingFields, setField, toggleField, defaultConfig, toModelConfigs)
import UniversalLLM.Providers.Anthropic (AnthropicOAuth(..))
import UniversalLLM.Providers.OpenAI (LlamaCpp(..), OpenRouter(..), AlibabaCloud(..))
import Runix.LLM (LLM)
import Runix.LLMStream (LLMStreaming)
import Runix.HTTP (HTTPStreaming)
import Runix.LLM.Interpreter (interpretLLMWith, interpretLLMStream, AnthropicOAuthAuth(..), LlamaCppAuth(..), OpenRouterAuth(..), ZAIAuth(..), AlibabaCloudAuth(..), ProviderProtocol, EnableStreaming, ProtocolRequest)
import Runix.RestAPI (RestEndpoint)
import Autodocodec (HasCodec)
import UI.UserInput (UserInput, interpretUserInputFail)
import Config (ModelId)
import qualified Config
import Models (ClaudeSonnet45(..), ClaudeHaiku45(..), ClaudeOpus46(..), GLM45Air(..), MinimaxM25(..), Qwen35_122B(..), Qwen3CoderNext(..), Qwen35Plus(..), KimiK25(..), UniversalWithTools(..), GLM46(..), GLM47(..), GLM5(..), ZAI(..), AlibabaCloud(..), ModelDefaults(..), claudeSonnet45OAuth, claudeHaiku45OAuth, claudeOpus46OAuth, glm45AirLlamaCpp, minimaxM25LlamaCpp, minimaxM25AlibabaCloud, qwen35_122B, qwen35Plus, qwen3Coder, kimiK25AlibabaCloud, universalWithTools, glm45AirZAI, glm46, glm47, glm5, glm5AlibabaCloud)
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
               => (forall r. Members '[UserInput widget, FileSystem, FileSystemRead, FileSystemWrite, Runix.Grep.GrepSystem, Bash, Cmds, HTTP, Logging, Fail, Embed IO, PromptStore] r
                   => Sem r a)
               -> IO (Either String a)
runWithEffects action =
  runM
    . runError
    . loggingIO
    . failLog
    . interpretUserInputFail @widget
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
-- Provides LLM interpreter plus session serialization.
data ModelInterpreter where
  ModelInterpreter :: forall model.
    ( Eq (Message model)
    , HasTools model
    , SupportsSystemPrompt (ProviderOf model)
    , ModelDefaults model
    ) =>
    { interpretModelNonStreaming :: forall r a. Members [Fail, Embed IO, HTTP] r => Sem (LLM model : r) a -> Sem r a
    , interpretModelStreaming :: forall r a. Members [Fail, HTTPStreaming] r => Sem (LLMStreaming model : r) a -> Sem r a
    , miLoadSession :: forall r. (Members [FileSystem, FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> Sem r [Message model]
    , miSaveSession :: forall r. (Members [FileSystem, FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ()
    } -> ModelInterpreter

--------------------------------------------------------------------------------
-- Model Registry
--------------------------------------------------------------------------------

-- | Constraint alias for config records supporting generic field introspection
type RegistryConfig cfg model =
  ( Generic cfg
  , GSettingFields (Rep cfg)
  , GSetField (Rep cfg)
  , GToggleField (Rep cfg)
  , GDefault (Rep cfg)
  , ModelSettings cfg model
  , Show cfg, Eq cfg
  )

-- | A model entry with pre-resolved auth, ready to produce interpreters.
-- Auth is resolved once during IO; interpreter creation is pure.
data ModelEntry where
  ModelEntry :: forall model cfg p s.
    ( cfg ~ ConfigFor model
    , HasTools model, SupportsSystemPrompt (ProviderOf model)
    , Eq (Message model), ModelDefaults model
    , RegistryConfig cfg model
    , RestEndpoint p, Default s
    -- Interpreter constraints
    , ModelName model, HasCodec (ProviderRequest model)
    , Monoid (ProviderRequest model)
    , EnableStreaming (ProviderResponse model)
    , ProtocolRequest (ProviderResponse model) ~ ProviderRequest model
    , HasStreaming model
    , HasCodec (ProviderResponse model)
    , ProviderProtocol (ProviderResponse model)
    ) =>
    { meId       :: ModelId                     -- ^ Model identity (for selection)
    , meName     :: Text                       -- ^ Display name (for UI)
    , meAuth     :: p                          -- ^ Already-resolved auth
    , meProvider :: ComposableProvider model s  -- ^ Provider handlers
    , meModel    :: model                      -- ^ Model value
    , meConfig   :: cfg                        -- ^ Current config (modifiable)
    } -> ModelEntry

-- | Extract all setting fields from a ModelEntry's config
entryFields :: ModelEntry -> [SettingField]
entryFields (ModelEntry _ _ _ _ _ cfg) = settingFields cfg

-- | Set a field by name on a ModelEntry's config
entrySetField :: Text -> SettingValue -> ModelEntry -> Maybe ModelEntry
entrySetField name val (ModelEntry i n a p m cfg) =
  case setField name val cfg of
    Just cfg' -> Just (ModelEntry i n a p m cfg')
    Nothing   -> Nothing

-- | Toggle an optional field on a ModelEntry's config
entryToggleField :: Text -> Bool -> ModelEntry -> Maybe ModelEntry
entryToggleField name enabled (ModelEntry i n a p m cfg) =
  case toggleField name enabled cfg of
    Just cfg' -> Just (ModelEntry i n a p m cfg')
    Nothing   -> Nothing

-- | Reset a ModelEntry's config to defaults
entryResetConfig :: ModelEntry -> ModelEntry
entryResetConfig (ModelEntry i n a p m (_ :: cfg)) =
  ModelEntry i n a p m (defaultConfig :: cfg)

-- | Create a ModelInterpreter from a ModelEntry (pure, auth already resolved)
entryInterpreter :: ModelEntry -> ModelInterpreter
entryInterpreter (ModelEntry _ _ auth provider model cfg) =
  let configs = toModelConfigs cfg
  in ModelInterpreter
    { interpretModelNonStreaming =
        interpretLLMWith auth provider model configs
    , interpretModelStreaming =
        interpretLLMStream auth provider model configs
    , miLoadSession = loadSession provider
    , miSaveSession = saveSession provider
    }

-- | Convenience constructor using defaultConfig for initial config
mkEntry :: forall model cfg p s.
  ( cfg ~ ConfigFor model
  , HasTools model, SupportsSystemPrompt (ProviderOf model)
  , ModelDefaults model
  , RegistryConfig cfg model
  , RestEndpoint p, Default s
  , ModelName model, HasCodec (ProviderRequest model)
  , Monoid (ProviderRequest model)
  , EnableStreaming (ProviderResponse model)
  , ProtocolRequest (ProviderResponse model) ~ ProviderRequest model
  , HasStreaming model
  , HasCodec (ProviderResponse model)
  ) => ModelId -> Text -> p -> ComposableProvider model s -> model -> ModelEntry
mkEntry mid name auth provider model = ModelEntry
  { meId = mid, meName = name, meAuth = auth, meProvider = provider, meModel = model, meConfig = defaultConfig }

-- | Build list of available models by probing environment for auth credentials.
-- Models whose auth is unavailable are silently skipped.
buildAvailableModels :: IO [ModelEntry]
buildAvailableModels = catMaybes . concat <$> sequence
  [ probeAnthropic, probeLlamaCpp, probeZAI, probeAlibabaCloud, probeOpenRouter ]

probeAnthropic :: IO [Maybe ModelEntry]
probeAnthropic = do
  mToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  case mToken of
    Nothing -> return [Nothing, Nothing, Nothing]
    Just token ->
      let auth = AnthropicOAuthAuth token
      in return $ map Just
        [ mkEntry Config.ClaudeSonnet45 "Claude Sonnet 4.5" auth claudeSonnet45OAuth (Model ClaudeSonnet45 AnthropicOAuth)
        , mkEntry Config.ClaudeHaiku45  "Claude Haiku 4.5"  auth claudeHaiku45OAuth  (Model ClaudeHaiku45 AnthropicOAuth)
        , mkEntry Config.ClaudeOpus46   "Claude Opus 4.6"   auth claudeOpus46OAuth   (Model ClaudeOpus46 AnthropicOAuth)
        ]

probeLlamaCpp :: IO [Maybe ModelEntry]
probeLlamaCpp = do
  mEndpoint <- lookupEnv "LLAMACPP_ENDPOINT"
  let endpoint = maybe "http://localhost:8080/v1" id mEndpoint
      auth = LlamaCppAuth endpoint
  -- LlamaCpp is always available (defaults to localhost)
  return $ map Just
    [ mkEntry Config.GLM45AirLlamaCpp   "GLM 4.5 Air (LlamaCpp)"  auth glm45AirLlamaCpp   (Model GLM45Air LlamaCpp)
    , mkEntry Config.MinimaxM25LlamaCpp "MiniMax M2.5 (LlamaCpp)" auth minimaxM25LlamaCpp (Model MinimaxM25 LlamaCpp)
    , mkEntry Config.Qwen35LlamaCpp     "Qwen 3.5 122B (LlamaCpp)" auth qwen35_122B       (Model Qwen35_122B LlamaCpp)
    , mkEntry Config.Qwen3CoderLlamaCpp "Qwen3 Coder (LlamaCpp)"  auth qwen3Coder         (Model Qwen3CoderNext LlamaCpp)
    ]

probeZAI :: IO [Maybe ModelEntry]
probeZAI = do
  mKey <- lookupEnv "ZAI_API_KEY"
  case mKey of
    Nothing -> return [Nothing, Nothing, Nothing, Nothing]
    Just key ->
      let auth = ZAIAuth key
      in return $ map Just
        [ mkEntry Config.GLM45AirZAI "GLM 4.5 Air (ZAI)" auth glm45AirZAI (Model GLM45Air ZAI)
        , mkEntry Config.GLM46ZAI    "GLM 4.6 (ZAI)"     auth glm46       (Model GLM46 ZAI)
        , mkEntry Config.GLM47ZAI    "GLM 4.7 (ZAI)"     auth glm47       (Model GLM47 ZAI)
        , mkEntry Config.GLM5ZAI     "GLM 5 (ZAI)"       auth glm5        (Model GLM5 ZAI)
        ]

probeAlibabaCloud :: IO [Maybe ModelEntry]
probeAlibabaCloud = do
  mKey <- lookupEnv "ALIBABACLOUD_API_KEY"
  case mKey of
    Nothing -> return [Nothing, Nothing, Nothing, Nothing]
    Just key ->
      let auth = AlibabaCloudAuth key
      in return $ map Just
        [ mkEntry Config.MinimaxM25AlibabaCloud "MiniMax M2.5 (AlibabaCloud)" auth minimaxM25AlibabaCloud (Model MinimaxM25 AlibabaCloud)
        , mkEntry Config.KimiK25AlibabaCloud    "Kimi K2.5 (AlibabaCloud)"    auth kimiK25AlibabaCloud    (Model KimiK25 AlibabaCloud)
        , mkEntry Config.Qwen35PlusAlibabaCloud "Qwen 3.5 Plus (AlibabaCloud)" auth qwen35Plus             (Model Qwen35Plus AlibabaCloud)
        , mkEntry Config.GLM5AlibabaCloud       "GLM 5 (AlibabaCloud)"        auth glm5AlibabaCloud       (Model GLM5 AlibabaCloud)
        ]

probeOpenRouter :: IO [Maybe ModelEntry]
probeOpenRouter = do
  mKey <- lookupEnv "OPENROUTER_API_KEY"
  mModel <- lookupEnv "OPENROUTER_MODEL"
  case (mKey, mModel) of
    (Just key, Just modelName) ->
      let auth = OpenRouterAuth key
      in return [Just $ mkEntry Config.OpenRouterModel (T.pack $ "OpenRouter: " ++ modelName) auth universalWithTools (Model (UniversalWithTools (T.pack modelName)) OpenRouter)]
    _ -> return [Nothing]

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

