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
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Data.Text as T
import Data.IORef
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, when)
import qualified System.Directory as Dir
import System.Environment (getExecutablePath)
import System.Posix.Process (executeFile)
import System.Posix.Files (getFileStatus, modificationTime)

import Polysemy
import Polysemy.Error (runError, Error, catch)

import UniversalLLM.Core.Types (Message(..), ModelConfig(Streaming), ToolCall(..), ToolResult(..))
import UniversalLLM (ProviderOf)

import Config
import Models
import Runner (loadSystemPrompt, createModelInterpreter, ModelInterpreter(..), runConfig, runHistory )
import Runix.Runner (grepIO, bashIO, cmdIO, failLog, loggingIO)
import qualified UI.Commands.View as ViewCmd
import qualified UI.Commands.History as HistoryCmd
import UI.UI (runUI)
import Agent (runixCode, UserPrompt (UserPrompt), SystemPrompt (SystemPrompt))
import Runix.LLM.Effects (LLM)
import Runix.LLM.Interpreter (withLLMCancellation)
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite, FileWatcher, fileWatcherIO, limitSubpathRead, limitSubpathWrite, filesystemReadIO, filesystemWriteIO)
import Runix.Grep.Effects (Grep)
import Runix.Bash.Effects (Bash)
import Runix.Cmd.Effects (Cmd)
import Runix.HTTP.Effects (HTTP, HTTPStreaming, httpIO, httpIOStreaming, withRequestTimeout)
import Runix.Logging.Effects (Logging(..), info, Level(..))
import Runix.PromptStore.Effects (PromptStore, promptStoreIO)
import Runix.Config.Effects (Config)
import qualified Runix.Config.Effects as ConfigEffect
import Runix.Cancellation.Effects (Cancellation(..))
import Runix.Streaming.Effects (StreamChunk)
import UI.State (newUIVars, UIVars, waitForUserInput, userInputQueue, clearCancellationFlag, sendAgentEvent, AgentEvent(..), UserRequest(..), LLMSettings(..))
import UI.Interpreter (interpretUI)
import UI.LoggingInterpreter (interpretLoggingToUI)
import UI.UserInput (UserInput)
import UI.UserInput.Interpreter (interpretUserInput)
import UI.UserInput.InputWidget (TUIWidget)
import qualified UI.ForegroundCmd
import UI.ForegroundCmdInterpreter (interpretForegroundCmd)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, SupportsSystemPrompt, SupportsStreaming)
import qualified Data.ByteString as BS
import qualified UI.Effects
import UI.Streaming (reinterpretSSEChunks, interpretStreamChunkToUI, interpretCancellation)
import qualified Paths_runix_code
import Paths_runix_code (getDataFileName)


--------------------------------------------------------------------------------
-- Command Infrastructure
--------------------------------------------------------------------------------

-- | A command is simply a function that takes user input text
type Command r = T.Text -> Sem r ()

-- | A named slash command (e.g., "/help", "/clear")
data SlashCommand r = SlashCommand
  { commandName :: T.Text       -- ^ Name without the slash (e.g., "help")
  , commandFn   :: Command r    -- ^ The command implementation
  }

-- | A complete command set with slash commands and a default fallback
data CommandSet r = CommandSet
  { slashCommands :: [SlashCommand r]  -- ^ Named slash commands
  , defaultCommand :: Command r         -- ^ Command to run when no slash prefix matches
  }

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code TUI
main :: IO ()
main = do
  -- Load configuration
  cfg <- loadConfig

  -- Create model interpreter
  ModelInterpreter{interpretModel, miLoadSession, miSaveSession} <- createModelInterpreter (cfgModelSelection cfg)

  -- Run UI with the interpreter
  -- The interpreter is now just a function we can pass around
  runUI (\refreshCallback -> buildUIRunner interpretModel miLoadSession miSaveSession (cfgResumeSession cfg) refreshCallback)

--------------------------------------------------------------------------------
-- Agent Loop
--------------------------------------------------------------------------------

-- | Agent loop that processes user input from the UI
-- | Update history ref (completion event is sent by runOneIteration)
updateHistory :: forall model.
                 UIVars (Message model)
              -> IORef [Message model]
              -> [Message model]
              -> IO ()
updateHistory _uiVars historyRef newHistory = do
  -- Update historyRef (source of truth)
  -- Note: AgentCompleteEvent is sent by runOneIteration after command completes
  writeIORef historyRef newHistory

agentLoop :: forall model.
             ( HasTools model
             , SupportsSystemPrompt (ProviderOf model)
             , ModelDefaults model
             , SupportsStreaming (ProviderOf model)
             )
          => FilePath  -- CWD for security restrictions
          -> RunixDataDir  -- Data directory path
          -> UIVars (Message model)
          -> IORef [Message model]
          -> SystemPrompt
          -> (forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming] r => Sem (LLM model : r) a -> Sem r a)  -- Model interpreter
          -> (forall r. (Members [FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ())  -- Save session function
          -> FilePath  -- Executable path
          -> Integer  -- Initial executable mtime
          -> IO ()
agentLoop cwd dataDir uiVars historyRef sysPrompt modelInterpreter miSaveSession exePath initialMTime = do
  -- Run the entire agent loop inside Sem so FileWatcher state persists
  let runToIO' = runM . runError . interpretTUIEffects cwd dataDir uiVars . modelInterpreter

  result <- runToIO' $ forever $ runOneIteration

  -- Handle top-level errors (though forever should never return)
  case result of
    Left err -> do
      clearCancellationFlag uiVars
      sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Fatal error: " ++ err))
    Right _ -> return ()

  where
    runOneIteration = do
      -- Wait for user request (includes text + settings)
      UserRequest{userText, requestSettings} <- embed $ atomically $ waitForUserInput (userInputQueue uiVars)

      -- Clear any previous cancellation flag before starting new request
      embed $ clearCancellationFlag uiVars

      -- Build command set with current settings
      let cmdSet = CommandSet
            { slashCommands = [echoCommand, viewCommand, historyCommand]
            , defaultCommand = runDefaultAgentCommand requestSettings
            }

      -- Dispatch to appropriate command (commands are responsible for adding to history)
      dispatchCommand cmdSet userText

      -- After any command completes, send completion event with current history
      -- (This clears "processing" status in the UI)
      currentHistory <- embed $ readIORef historyRef
      embed $ sendAgentEvent uiVars (AgentCompleteEvent currentHistory)

      -- Always clear cancellation flag after request completes (whether success or error)
      embed $ clearCancellationFlag uiVars

      -- Check if executable has been modified and reload if necessary
      embed $ checkAndReloadOnChange

    -- | Check if executable has changed and reload if so
    checkAndReloadOnChange :: IO ()
    checkAndReloadOnChange = do
      -- Get current mtime of executable
      stat <- getFileStatus exePath
      let currentMTime = fromIntegral $ fromEnum $ modificationTime stat

      -- Check if executable has been modified (compare to initial mtime from startup)
      when (currentMTime /= initialMTime) $ do
        -- Save current session
        currentHistory <- readIORef historyRef
        let sessionFile = "/tmp/runix-code-session.json"

        -- Use the effect stack to save session
        let runSave = runM . runError @String . loggingIO . failLog . filesystemWriteIO . filesystemReadIO
        result <- runSave $ miSaveSession sessionFile currentHistory

        case result of
          Left err -> do
            -- Failed to save session - log error but don't reload
            sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Failed to save session for reload: " ++ err))
          Right () -> do
            -- Successfully saved - exec new binary via UI event (so Brick can suspend first)
            sendAgentEvent uiVars (RunExternalCommandEvent $ do
              executeFile exePath False ["--resume-session", sessionFile] Nothing
              -- Never returns!
              )

    -- | Dispatch user input to the appropriate command
    -- | Checks if input starts with "/" and matches a slash command,
    -- | otherwise falls back to the default command
    dispatchCommand :: CommandSet r -> T.Text -> Sem r ()
    dispatchCommand CommandSet{slashCommands, defaultCommand} userText =
      case T.stripPrefix "/" userText of
        Nothing -> defaultCommand userText  -- No slash prefix, use default
        Just rest ->
          -- Extract command name (everything before first space, or entire rest if no space)
          let (cmdName, remainder) = T.break (== ' ') rest
              cmdArg = T.stripStart remainder
          in case lookup cmdName [(commandName cmd, commandFn cmd) | cmd <- slashCommands] of
               Just cmd -> cmd cmdArg  -- Found matching slash command
               Nothing -> defaultCommand userText  -- No match, use default

    -- | Wrapper that converts a history-manipulating function into a command
    -- | Takes a function that receives current history and returns new history,
    -- | and handles:
    -- | - Sending user message to UI/history
    -- | - Reading current history
    -- | - Running the function
    -- | - Updating history with the result
    -- | - Error handling
    withHistoryUpdate :: Members '[Error String, Embed IO] r
                      => ([Message model] -> T.Text -> Sem r [Message model])
                      -> T.Text
                      -> Sem r ()
    withHistoryUpdate fn userText = do
      -- Send user message to UI immediately (adds to history)
      embed $ sendAgentEvent uiVars (UserMessageEvent (UserText userText))

      currentHistory <- embed $ readIORef historyRef
      catch
        (do newHistory <- fn currentHistory userText
            embed $ updateHistory uiVars historyRef newHistory)
        (\err -> embed $ sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Command error: " ++ err)))

    -- | Echo command: logs the input text
    echoCommand :: Member Logging r => SlashCommand r
    echoCommand = SlashCommand
      { commandName = "echo"
      , commandFn = \text -> info $ "Echo: " <> text
      }

    -- | View command: open conversation history in $PAGER
    viewCommand :: Members '[Embed IO, Logging] r => SlashCommand r
    viewCommand =
      let (name, fn) = ViewCmd.viewCommand historyRef uiVars
      in SlashCommand { commandName = name, commandFn = fn }

    -- | History command: edit conversation history in $EDITOR
    historyCommand :: Members '[Embed IO, Logging] r => SlashCommand r
    historyCommand =
      let (name, fn) = HistoryCmd.historyCommand historyRef uiVars
      in SlashCommand { commandName = name, commandFn = fn }

    -- | The default agent command: run runixCode with the user's input
    runDefaultAgentCommand settings userText =
      withHistoryUpdate (\currentHistory userTxt -> do
        -- Build configs using settings from the request
        let isStreaming (Streaming _) = True
            isStreaming _ = False
            baseConfigs = defaultConfigs @model
            configsWithoutStreaming = filter (not . isStreaming) baseConfigs
            runtimeConfigs = configsWithoutStreaming ++ [Streaming (llmStreaming settings)]

        -- Run agent and return new history
        (_result, newHistory) <- withLLMCancellation
                               . runConfig runtimeConfigs
                               . runHistory currentHistory
                               $ runixCode @model @TUIWidget sysPrompt (UserPrompt userTxt)
        return newHistory
      ) userText

--------------------------------------------------------------------------------
-- UI Runner Builder
--------------------------------------------------------------------------------

-- | Build a UI runner by composing model interpreter with UI effects
--
-- This combines:
-- 1. Model interpreter (LLM effect -> base effects)
-- 2. UI effects interpreter (base effects -> IO)
-- 3. Agent loop
buildUIRunner :: forall model.
                 ( HasTools model
                 , SupportsSystemPrompt (ProviderOf model)
                 , ModelDefaults model
                 , SupportsStreaming (ProviderOf model)
                 )
              => (forall r a. Members [Fail, Embed IO, HTTP, HTTPStreaming] r => Sem (LLM model : r) a -> Sem r a)  -- Model interpreter
              -> (forall r. (Members [FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> Sem r [Message model])  -- Load session
              -> (forall r. (Members [FileSystemRead, FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ())  -- Save session
              -> Maybe FilePath  -- Resume session path
              -> (AgentEvent (Message model) -> IO ())  -- Refresh callback
              -> IO (UIVars (Message model))
buildUIRunner modelInterpreter miLoadSession miSaveSession maybeSessionPath refreshCallback = do
  -- Get current working directory for security restrictions
  cwd <- Dir.getCurrentDirectory

  uiVars <- newUIVars @(Message model) refreshCallback
  historyRef <- newIORef ([] :: [Message model])

  -- Load session if resuming
  initialHistory <- case maybeSessionPath of
    Just path -> do
      let runToIO' = runM . runError @String . loggingIO . failLog . filesystemWriteIO . filesystemReadIO
      result <- runToIO' $ miLoadSession path
      case result of
        Right msgs -> do
          -- Log successful reload
          sendAgentEvent uiVars (LogEvent Info (T.pack $ "Code reloaded - session restored with " ++ show (length msgs) ++ " messages"))
          return msgs
        Left err -> do
          sendAgentEvent uiVars (AgentErrorEvent (T.pack $ "Failed to load session: " ++ err))
          return []
    Nothing -> return []

  -- Initialize historyRef with loaded or empty history
  writeIORef historyRef initialHistory

  -- Send loaded history to UI so it displays the messages
  when (not $ null initialHistory) $
    sendAgentEvent uiVars (AgentCompleteEvent initialHistory)

  -- Get data file path for the system prompt
  promptPath <- getDataFileName "prompt/runix-code.md"

  -- Get data directory for RunixDataDir config
  dataDir <- RunixDataDir <$> Paths_runix_code.getDataDir

  -- Load system prompt using the composed interpreter stack
  let runToIO' = runM . runError @String . loggingIO . failLog . filesystemReadIO

  result <- runToIO' $ loadSystemPrompt promptPath "You are a helpful AI coding assistant."
  let sysPrompt = case result of
        Right txt -> SystemPrompt txt
        Left _ -> SystemPrompt "You are a helpful AI coding assistant."

  -- Get executable path and initial modification time for code reloading
  exePath <- getExecutablePath
  stat <- getFileStatus exePath
  let initialMTime = fromIntegral $ fromEnum $ modificationTime stat

  _ <- forkIO $ agentLoop cwd dataDir uiVars historyRef sysPrompt modelInterpreter miSaveSession exePath initialMTime
  return uiVars

--------------------------------------------------------------------------------
-- Runner Creation
--------------------------------------------------------------------------------

-- | Interpret all base effects for TUI agents
--
-- This builds the effect interpretation stack from the bottom up:
-- - File system, bash, cmd, grep (basic IO effects)
-- - Security restrictions (limit to CWD)
-- - HTTP (both streaming and non-streaming)
-- - SSE chunk extraction and streaming to UI
-- - Cancellation, logging, user input
-- - UI effects and error handling


interpretTUIEffects :: (Member (Error String) r, Member (Embed IO) r)
                    => FilePath  -- CWD for security restrictions
                    -> RunixDataDir  -- Data directory path
                    -> UIVars msg
                    -> Sem (Grep
                         : Bash
                         : Cmd
                         : PromptStore
                         : Runix.Config.Effects.Config RunixDataDir
                         : FileWatcher
                         : HTTP
                         : HTTPStreaming
                         : StreamChunk BS.ByteString
                         : Cancellation
                         : FileSystemWrite
                         : FileSystemRead
                         : Fail
                         : Logging
                         : UserInput TUIWidget
                         : UI.ForegroundCmd.ForegroundCmd
                         : UI.Effects.UI
                         : r) a
                    -> Sem r a
interpretTUIEffects cwd dataDir uiVars =
  interpretUI uiVars
    . interpretForegroundCmd uiVars    -- ForegroundCmd effect
    . interpretUserInput uiVars        -- UserInput effect
    . interpretLoggingToUI
    . failLog
    . filesystemReadIO                  -- Interpret read operations to IO (removes FileSystemRead)
    . limitSubpathRead cwd             -- SECURITY: Restrict reads to CWD (intercepts FileSystemRead)
    . filesystemWriteIO                 -- Interpret write operations to IO (removes FileSystemWrite)
    . limitSubpathWrite cwd            -- SECURITY: Restrict writes to CWD (intercepts FileSystemWrite)
    . interpretCancellation uiVars     -- Handle Cancellation effect
    . interpretStreamChunkToUI uiVars  -- Handle StreamChunk Text
    . reinterpretSSEChunks              -- Convert StreamChunk BS -> StreamChunk Text
    . httpIOStreaming (withRequestTimeout 300)  -- Emit StreamChunk BS
    . httpIO (withRequestTimeout 300)           -- Handle non-streaming HTTP
    . fileWatcherIO                     -- Interpret FileWatcher effect
    . ConfigEffect.runConfig dataDir    -- Provide data directory
    . promptStoreIO                     -- Interpret PromptStore effect
    . cmdIO
    . bashIO
    . grepIO                            -- Interpret grep effect

--------------------------------------------------------------------------------
-- Echo Agent (Placeholder)
--------------------------------------------------------------------------------

-- | Echo agent - placeholder that doesn't use LLM
--
-- Pure function: takes history and user input, returns new history.
-- For the real agent, this would call runRunixCode.
_echoAgent :: forall model r.
             [Message model]
          -> String
          -> Sem r [Message model]
_echoAgent currentHistory userInput =
  let userMsg = UserText (T.pack userInput)
      agentMsg = AssistantText (T.pack $ "Echo: " ++ userInput)
      newHistory = currentHistory ++ [userMsg, agentMsg]
  in return newHistory
