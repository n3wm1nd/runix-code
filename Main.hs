module Main (main) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStr)
import GHC.Stack

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Runner (filesystemIO, httpIO, withRequestTimeout, loggingIO, failLog)
import Runix.LLM.Effects
import Runix.LLM.Interpreter hiding (SystemPrompt)
import Runix.HTTP.Effects
import Runix.FileSystem.Effects
import Runix.Logging.Effects
import Runix.Secret.Effects (runSecret)

import UniversalLLM hiding (SystemPrompt)
import UniversalLLM.Core.Types (Message(..), ComposableProvider, cpSerializeMessage, cpDeserializeMessage)
import qualified UniversalLLM.Providers.Anthropic as AnthropicProvider
import UniversalLLM.Providers.Anthropic (Anthropic(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import System.Environment (getArgs)
import System.Directory (doesFileExist)

import Agent (SystemPrompt(..), UserPrompt(..), RunixCodeResult(..), runRunixCode)

--------------------------------------------------------------------------------
-- Model Definition
--------------------------------------------------------------------------------

-- Claude Sonnet 4.5 model
data ClaudeSonnet45 = ClaudeSonnet45 deriving (Show, Eq)

instance ModelName Anthropic ClaudeSonnet45 where
  modelName _ = "claude-sonnet-4-5-20250929"

instance HasTools ClaudeSonnet45 Anthropic where
  withTools = AnthropicProvider.anthropicWithTools

instance ProviderImplementation Anthropic ClaudeSonnet45 where
  getComposableProvider = AnthropicProvider.ensureUserFirst . withTools $ AnthropicProvider.baseComposableProvider

--------------------------------------------------------------------------------
-- Session Serialization
--------------------------------------------------------------------------------

-- | Serialize a list of messages to JSON using the model's provider
serializeMessages :: [Message ClaudeSonnet45 Anthropic] -> Aeson.Value
serializeMessages msgs =
  let provider = getComposableProvider @Anthropic @ClaudeSonnet45
      -- Collect all successfully serialized messages
      serialized = [(i, v) | (i, Just v) <- zip [0..] (map (cpSerializeMessage provider) msgs)]
      failed = length msgs - length serialized
  in if failed > 0
     then Prelude.error $ "Failed to serialize " <> show failed <> " out of " <> show (length msgs) <> " messages"
     else Aeson.toJSON (map snd serialized)

-- | Deserialize a list of messages from JSON using the model's provider
deserializeMessages :: Aeson.Value -> Either String [Message ClaudeSonnet45 Anthropic]
deserializeMessages val = case val of
  Aeson.Array arr -> do
    let provider = getComposableProvider @Anthropic @ClaudeSonnet45
        arrList = Vector.toList arr
        results = [(i, v, cpDeserializeMessage provider v) | (i, v) <- zip [0..] arrList]
        messages = [msg | (_, _, Just msg) <- results]
        failed = [(i, v) | (i, v, Nothing) <- results]
    if null failed
      then Right messages
      else Left $ "Failed to deserialize " <> show (length failed) <> " messages at indices: "
                  <> show (map fst failed)
  _ -> Left "Expected JSON array"

-- | Save session to file
saveSession :: FilePath -> [Message ClaudeSonnet45 Anthropic] -> IO ()
saveSession path msgs = do
  let json = serializeMessages msgs
  BSL.writeFile path (Aeson.encode json)

-- | Load session from file
loadSession :: FilePath -> IO (Either String [Message ClaudeSonnet45 Anthropic])
loadSession path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left "Session file does not exist"
    else do
      contents <- BSL.readFile path
      case Aeson.decode contents of
        Nothing -> return $ Left "Failed to parse JSON"
        Just val -> return $ deserializeMessages val

--------------------------------------------------------------------------------
-- Interpreter Stack
--------------------------------------------------------------------------------

-- | Run the entire runix-code stack
runRunixCodeStack :: HasCallStack
                  => Text  -- ^ OAuth token
                  -> (forall r. Members '[FileSystem, HTTP, Logging, LLM Anthropic ClaudeSonnet45, Fail, Embed IO] r => Sem r a)
                  -> IO (Either String a)
runRunixCodeStack token action =
  runM
    . runError
    . loggingIO
    . failLog
    . httpIO (withRequestTimeout 300)
    . filesystemIO
    . runSecret (pure $ T.unpack token)
    . interpretAnthropicOAuth Anthropic ClaudeSonnet45
    $ action

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code
--
-- Usage: echo "prompt" | runix-code [session-file]
--
-- Reads prompt from stdin, runs the agent, displays response
-- If session-file is provided, loads history from it and saves updated history back
main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs
  let maybeSessionFile = case args of
        (file:_) -> Just file
        [] -> Nothing

  -- Get OAuth token from environment
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  token <- case maybeToken of
    Nothing -> do
      hPutStr stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      exitFailure
    Just t -> pure $ T.pack t

  -- Load existing session if session file provided
  initialHistory <- case maybeSessionFile of
    Nothing -> return []
    Just sessionFile -> do
      loaded <- loadSession sessionFile
      case loaded of
        Left err -> do
          hPutStr stderr $ "Warning: Failed to load session from " <> sessionFile <> ": " <> err <> "\n"
          hPutStr stderr "Starting with empty history.\n"
          return []
        Right msgs -> do
          hPutStr stderr $ "Loaded " <> show (length msgs) <> " messages from session.\n"
          return msgs

  -- Read user input from stdin
  userInput <- TIO.getContents

  -- Check if we got any input
  if T.null (T.strip userInput)
    then do
      hPutStr stderr "Error: No input provided. Usage: echo \"prompt\" | runix-code [session-file]\n"
      exitFailure
    else do
      -- Run the agent with session
      result <- runRunixCodeStack token $ runAgentWithHistory initialHistory userInput
      case result of
        Right (response, finalHistory) -> do
          -- Save updated session if session file provided
          case maybeSessionFile of
            Nothing -> return ()
            Just sessionFile -> saveSession sessionFile finalHistory
          TIO.putStrLn response
        Left errMsg -> hPutStr stderr errMsg >> exitFailure

-- | Run the agent with history
runAgentWithHistory :: Members '[FileSystem, HTTP, Logging, LLM Anthropic ClaudeSonnet45, Fail, Embed IO] r
                    => [Message ClaudeSonnet45 Anthropic]
                    -> Text
                    -> Sem r (Text, [Message ClaudeSonnet45 Anthropic])
runAgentWithHistory initialHistory userInput = do
  -- Use a simple system prompt for now
  let sysPrompt = SystemPrompt "You are a helpful AI coding assistant."
      userPrompt = UserPrompt userInput

  -- Run the agent
  (result, finalHistory) <- runRunixCode @Anthropic @ClaudeSonnet45 sysPrompt initialHistory userPrompt

  return (responseText result, finalHistory)
