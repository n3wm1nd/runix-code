{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeAbstractions #-}

module Main (main) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.IO (hPutStr)
import qualified System.IO as IO

import Polysemy
import Polysemy.Error (runError)

import Runix.LLM.Interpreter hiding (SystemPrompt)
import Runix.Runner (filesystemIO, grepIO, bashIO, cmdIO, httpIO, httpIOStreaming, withRequestTimeout, loggingIO, failLog)
import Runix.FileSystem.Effects (fileWatcherNoop)
import Runix.PromptStore.Effects (promptStoreIO)
import Runix.Config.Effects (Config)
import qualified Runix.Config.Effects as ConfigEffect
import Runix.Cancellation.Effects (cancelNoop)
import Runix.Streaming.Effects (ignoreChunks)
import qualified Data.ByteString as BS

import Agent (SystemPrompt(..), UserPrompt(..), runixCode, responseText)
import qualified Config
import Config (RunixDataDir(..), loadConfig, cfgModelSelection, cfgSessionFile)
import Runner (loadSystemPrompt, createModelInterpreter, ModelInterpreter(..), runConfigHistory)
import UI.UserInput (ImplementsWidget(..), RenderRequest, interpretUserInputFail)
import qualified Paths_runix_code
import Paths_runix_code (getDataFileName)

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
      -- Create model interpreter (same as TUI!)
      modelInterp <- createModelInterpreter (cfgModelSelection cfg)

      -- Run the agent
      result <- runAgent modelInterp cfg userInput

      case result of
        Right response -> TIO.putStrLn response
        Left errMsg -> hPutStr IO.stderr errMsg >> exitFailure

--------------------------------------------------------------------------------
-- Generic Agent Runner
--------------------------------------------------------------------------------

-- | Run the agent with the model interpreter
--
-- This composes the model interpreter with CLI effects (no streaming, simple I/O)
runAgent :: ModelInterpreter -> Config.Config -> Text -> IO (Either String Text)
runAgent (ModelInterpreter @model (interpretModel) miLoadSess miSaveSess) cfg userInput = do
  -- Get data file path for the system prompt
  promptPath <- getDataFileName "prompt/runix-code.md"

  -- Get the data directory from Cabal - this is where source files are during development
  -- and where they're installed during installation
  runixDataDir <- RunixDataDir <$> Paths_runix_code.getDataDir

  -- Compose: model interpreter + CLI base effects + run to IO
  let runToIO' = runM
               . runError
               . loggingIO
               . failLog
               . interpretUserInputFail @CLIWidget
               . cancelNoop
               . ignoreChunks @BS.ByteString
               . httpIOStreaming (withRequestTimeout 300)
               . httpIO (withRequestTimeout 300)
               . cmdIO
               . bashIO
               . fileWatcherNoop         -- No-op file watcher for CLI
               . ConfigEffect.runConfig runixDataDir
               . promptStoreIO
               . filesystemIO
               . grepIO
               . interpretModel

  runToIO' $ do
    -- Load system prompt
    sysPromptText <- loadSystemPrompt
      promptPath
      "You are a helpful AI coding assistant. You can answer the user's queries, or use tools."

    let sysPrompt = SystemPrompt sysPromptText
        userPrompt = UserPrompt userInput

    -- Load session (if specified)
    initialHistory <- case cfgSessionFile cfg of
      Nothing -> return []
      Just sessionFile -> miLoadSess sessionFile

    -- Run the agent (configs are inferred from model's defaultConfigs with streaming disabled)
    (result, finalHistory) <- withLLMCancellation . runConfigHistory ([]) initialHistory $
      runixCode @model @CLIWidget sysPrompt userPrompt

    -- Save session (if specified)
    case cfgSessionFile cfg of
      Nothing -> return ()
      Just sessionFile -> miSaveSess sessionFile finalHistory

    return $ responseText result
