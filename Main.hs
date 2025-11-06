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
import UniversalLLM.Core.Types (Message(..))
import qualified UniversalLLM.Providers.Anthropic as AnthropicProvider
import UniversalLLM.Providers.Anthropic (Anthropic(..))

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
-- Usage: echo "prompt" | runix-code
--
-- Reads prompt from stdin, runs the agent, displays response
main :: IO ()
main = do
  -- Get OAuth token from environment
  maybeToken <- lookupEnv "ANTHROPIC_OAUTH_TOKEN"
  token <- case maybeToken of
    Nothing -> do
      hPutStr stderr "Error: ANTHROPIC_OAUTH_TOKEN environment variable is not set\n"
      exitFailure
    Just t -> pure $ T.pack t

  -- Read user input from stdin
  userInput <- TIO.getContents

  -- Check if we got any input
  if T.null (T.strip userInput)
    then do
      hPutStr stderr "Error: No input provided. Usage: echo \"prompt\" | runix-code\n"
      exitFailure
    else do
      -- Run the agent
      result <- runRunixCodeStack token $ runAgent userInput
      case result of
        Right response -> TIO.putStrLn response
        Left errMsg -> hPutStr stderr errMsg >> exitFailure

-- | Run the agent with the user prompt
runAgent :: Members '[FileSystem, HTTP, Logging, LLM Anthropic ClaudeSonnet45, Fail, Embed IO] r
         => Text
         -> Sem r Text
runAgent userInput = do
  -- Use a simple system prompt for now
  let sysPrompt = SystemPrompt "You are a helpful AI coding assistant."
      userPrompt = UserPrompt userInput
      initialHistory = [] :: [Message ClaudeSonnet45 Anthropic]

  -- Run the agent
  (result, _finalHistory) <- runRunixCode @Anthropic @ClaudeSonnet45 sysPrompt initialHistory userPrompt

  return $ responseText result
