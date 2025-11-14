{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Runtime execution helpers for runix-code
--
-- This module provides generic helpers for:
-- - Loading/saving sessions (using FileSystem effect)
-- - Loading system prompts (using FileSystem effect)
-- - Running interpreter stacks
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
  ) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import GHC.Stack

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Runner (filesystemIO, grepIO, bashIO, cmdIO, httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite, readFile, writeFile, fileExists)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.Cmd.Effects (Cmd)
import Runix.HTTP.Effects (HTTP)
import Runix.Logging.Effects (Logging)
import Runix.Cancellation.Effects (Cancellation, cancelNoop)
import qualified Runix.Logging.Effects as Log

import UniversalLLM.Core.Types (Message, ComposableProvider, cpSerializeMessage, cpDeserializeMessage, ProviderImplementation, getComposableProvider)
import UI.UserInput (UserInput, interpretUserInputFail)

--------------------------------------------------------------------------------
-- Session Management (Effect-Based)
--------------------------------------------------------------------------------

-- | Load session from file
--
-- Sessions are model-agnostic - Message type parameters are phantom types.
-- We deserialize with a specific model/provider type for type safety, but
-- the messages can be used with any compatible model.
loadSession :: forall provider model r.
               ( Members [FileSystemRead, FileSystemWrite] r
               , Member Logging r
               , Member Fail r
               , ProviderImplementation provider model
               )
            => FilePath
            -> Sem r [Message model provider]
loadSession path = do
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
        Just val -> case deserializeMessages @provider @model val of
          Left err -> do
            Log.warning $ T.pack $ "Failed to deserialize session: " <> err
            return []
          Right msgs -> do
            Log.info $ T.pack $ "Loaded " <> show (length msgs) <> " messages from session"
            return msgs

-- | Save session to file
saveSession :: forall provider model r.
               ( Members [FileSystemRead, FileSystemWrite] r
               , Member Logging r
               , ProviderImplementation provider model
               )
            => FilePath
            -> [Message model provider]
            -> Sem r ()
saveSession path msgs = do
  let json = serializeMessages @provider @model msgs
      encoded = BSL.toStrict $ Aeson.encode json  -- Convert to strict
  writeFile path encoded
  Log.info $ T.pack $ "Saved " <> show (length msgs) <> " messages to session"

-- | Serialize messages to JSON (internal helper)
serializeMessages :: forall provider model.
                     ProviderImplementation provider model
                  => [Message model provider]
                  -> Aeson.Value
serializeMessages msgs =
  let provider = getComposableProvider @provider @model
      serialized = [(i, v) | (i, Just v) <- zip [0..] (map (cpSerializeMessage provider) msgs)]
      failed = length msgs - length serialized
  in if failed > 0
     then Prelude.error $ "Failed to serialize " <> show failed <> " out of " <> show (length msgs) <> " messages"
     else Aeson.toJSON (map snd serialized)

-- | Deserialize messages from JSON (internal helper)
deserializeMessages :: forall provider model.
                       ProviderImplementation provider model
                    => Aeson.Value
                    -> Either String [Message model provider]
deserializeMessages val = case val of
  Aeson.Array arr -> do
    let provider = getComposableProvider @provider @model
        arrList = Vector.toList arr
        results = [(i, v, cpDeserializeMessage provider v) | (i, v) <- zip [0..] arrList]
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
loadSystemPrompt :: (Members [FileSystemRead, FileSystemWrite] r, Member Logging r)
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
               => (forall r. Members '[UserInput widget, FileSystemRead, FileSystemWrite, Grep, Bash, Cmd, HTTP, Logging, Fail, Embed IO, Cancellation] r
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
    . cmdIO
    . bashIO
    . filesystemIO
    . grepIO
    $ action
