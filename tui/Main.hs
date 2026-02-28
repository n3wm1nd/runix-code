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
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, when)
import qualified System.Directory as Dir
import System.Environment (getExecutablePath)
import System.Posix.Process (executeFile)
import System.Posix.Files (getFileStatus, modificationTime)

import Polysemy
import Polysemy.Error (runError, Error, catch)

import UniversalLLM (Message(..))
import UniversalLLM (ProviderOf)

import Config
import Models
import Runner (loadSystemPrompt, createModelInterpreter, ModelInterpreter(..), runConfig, runHistory )
import Runix.Runner (bashIO, cmdsIO, failLog, loggingIO)
import Runix.Grep (grepForFilesystem)
import qualified UI.Commands.View as ViewCmd
import qualified UI.Commands.History as HistoryCmd
import UI.UI (runUI)
import Agent (runixCode, UserPrompt (UserPrompt), SystemPrompt (SystemPrompt))
import Runix.LLM (LLM)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, FileWatcher)
import qualified Runix.FileSystem.Simple
import qualified Runix.FileSystem.System
import Runix.Grep (Grep)
import Runix.Bash (Bash)
import Runix.Cmd (Cmds)
import Runix.HTTP (HTTP, httpIO, withRequestTimeout)
import Runix.Logging (Logging(..), info, Level(..), loggingNull)
import Runix.PromptStore (PromptStore, promptStoreIO)
import qualified Runix.Config as ConfigEffect
import UI.State (newUIVars, UIVars, waitForUserInput, userInputQueue, clearCancellationFlag, sendAgentEvent, AgentEvent(..), UserRequest(..))
import UI.Interpreter (interpretUI)
import UI.LoggingInterpreter (interpretLoggingToUI)
import UI.UserInput (UserInput)
import UI.UserInput.Interpreter (interpretUserInput)
import UI.UserInput.InputWidget (TUIWidget)
import qualified UI.ForegroundCmd
import UI.ForegroundCmdInterpreter (interpretForegroundCmd)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, SupportsSystemPrompt)
import qualified UI
import UI.UserInterface (interpretAsWidget)
import UI.AgentWidgets (AgentWidgets, addMessage)
import UI.AgentWidgetsInterpreter (interpretAgentWidgets)
import qualified Paths_runix_code
import Paths_runix_code (getDataFileName)
import Runix.FileSystem (loggingWrite, filterRead, filterWrite, hideGit, hideClaude, filterFileSystem, fileSystemLocal, fileWatcherINotify, interceptFileAccessRead, interceptFileAccessWrite, onlyClaude)


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
  ModelInterpreter{interpretModelNonStreaming = interpretModel, miLoadSession, miSaveSession} <- createModelInterpreter (cfgModelSelection cfg)

  -- Run UI with the interpreter
  runUI (\refreshCallback -> buildUIRunner interpretModel miLoadSession miSaveSession (cfgResumeSession cfg) refreshCallback)

--------------------------------------------------------------------------------
-- Agent Loop
--------------------------------------------------------------------------------

agentLoop :: forall model.
             ( HasTools model
             , SupportsSystemPrompt (ProviderOf model)
             , ModelDefaults model
             )
          => FilePath  -- CWD for security restrictions
          -> RunixDataDir  -- Data directory path
          -> UIVars (Message model)
          -> SystemPrompt
          -> (forall r a. Members [Fail, Embed IO, HTTP] r => Sem (LLM model : r) a -> Sem r a)  -- LLM interpreter
          -> (forall r. (Members [Runix.FileSystem.Simple.FileSystem, Runix.FileSystem.Simple.FileSystemRead, Runix.FileSystem.Simple.FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ())  -- Save session function
          -> FilePath  -- Executable path
          -> Integer  -- Initial executable mtime
          -> IO ()
agentLoop cwd dataDir uiVars sysPrompt interpretModel miSaveSession exePath initialMTime = do
  -- Run the entire agent loop inside Sem so FileWatcher state persists
  let runToIO' = runM . runError . interpretTUIEffects cwd dataDir uiVars . interpretModel

  result <- runToIO' $ forever runOneIteration

  -- Handle top-level errors (though forever should never return)
  case result of
    Left err -> do
      clearCancellationFlag uiVars
      sendAgentEvent uiVars (LogEvent Error (T.pack $ "Fatal error: " ++ err))
    Right _ -> return ()

  where
    runOneIteration = do
      -- Wait for user request (includes text + settings + history)
      UserRequest{userText, currentHistory, requestSettings} <- embed $ atomically $ waitForUserInput (userInputQueue uiVars)

      -- Clear any previous cancellation flag before starting new request
      embed $ clearCancellationFlag uiVars

      -- Build command set with current settings and history
      let cmdSet = CommandSet
            { slashCommands = [ echoCommand
                              , let (name, fn) = ViewCmd.viewCommand currentHistory uiVars
                                in SlashCommand { commandName = name, commandFn = fn }
                              , let (name, fn) = HistoryCmd.historyCommand currentHistory uiVars
                                in SlashCommand { commandName = name, commandFn = fn }
                              ]
            , defaultCommand = runDefaultAgentCommand currentHistory requestSettings
            }

      -- Dispatch to appropriate command
      -- Note: commands send their own completion events (e.g., interpretAsWidget sends AgentCompleteEvent)
      dispatchCommand cmdSet userText

      -- Always clear cancellation flag after request completes (whether success or error)
      embed $ clearCancellationFlag uiVars

      -- Check if executable has been modified and reload if necessary
      embed $ checkAndReloadOnChange currentHistory

    -- | Check if executable has changed and reload if so
    checkAndReloadOnChange :: [Message model] -> IO ()
    checkAndReloadOnChange history = do
      -- Get current mtime of executable
      stat <- getFileStatus exePath
      let currentMTime = fromIntegral $ fromEnum $ modificationTime stat

      -- Check if executable has been modified (compare to initial mtime from startup)
      when (currentMTime /= initialMTime) $ do
        -- Save current session
        -- TODO: This is lossy - should save full OutputHistory (with tool calls, streaming state, etc)
        -- not just Message history. For now, reload only preserves conversation messages.
        let sessionFile = "/tmp/runix-code-session.json"

        -- Use the effect stack to save session (loggingNull: Brick owns stdout)
        let runSave = runM . runError @String . loggingNull . failLog
                    . Runix.FileSystem.Simple.filesystemIO
        result <- runSave $ miSaveSession sessionFile history

        case result of
          Left err -> do
            -- Failed to save session - log error but don't reload
            sendAgentEvent uiVars (LogEvent Error (T.pack $ "Failed to save session for reload: " ++ err))
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

    -- | Echo command: logs the input text
    echoCommand :: Member Logging r => SlashCommand r
    echoCommand = SlashCommand
      { commandName = "echo"
      , commandFn = \text -> info $ "Echo: " <> text
      }

    -- | The default agent command: run runixCode with the user's input
    runDefaultAgentCommand history _settings userText = do
      catch
        (do -- Use default configs (no streaming)
            let baseConfigs = defaultConfigs @model

            -- Run agent (with widget isolation via interpretAsWidget)
            _result <- runConfig baseConfigs
                     . runHistory history
                     . interpretAsWidget @model
                     $ do
                        -- Add conversation history to output
                        mapM_ (addMessage @(Message model)) history
                        -- Add new user message
                        addMessage @(Message model) (UserText userText)
                        -- Run the agent
                        runixCode @model @TUIWidget sysPrompt (UserPrompt userText)
            -- Signal completion so UI updates status
            embed $ sendAgentEvent uiVars (AgentCompleteEvent [])
        )
        (\err -> embed $ sendAgentEvent uiVars (LogEvent Error (T.pack $ "Command error: " ++ err)))

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
                 )
              => (forall r a. Members [Fail, Embed IO, HTTP] r => Sem (LLM model : r) a -> Sem r a)  -- LLM interpreter
              -> (forall r. (Members [Runix.FileSystem.Simple.FileSystem, Runix.FileSystem.Simple.FileSystemRead, Runix.FileSystem.Simple.FileSystemWrite, Logging, Fail] r) => FilePath -> Sem r [Message model])  -- Load session
              -> (forall r. (Members [Runix.FileSystem.Simple.FileSystem, Runix.FileSystem.Simple.FileSystemRead, Runix.FileSystem.Simple.FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ())  -- Save session
              -> Maybe FilePath  -- Resume session path
              -> (AgentEvent (Message model) -> IO ())  -- Refresh callback
              -> IO (UIVars (Message model))
buildUIRunner interpretModel miLoadSession miSaveSession maybeSessionPath refreshCallback = do
  -- Get current working directory for security restrictions
  cwd <- Dir.getCurrentDirectory

  uiVars <- newUIVars @(Message model) refreshCallback

  -- Load session if resuming and send to UI
  case maybeSessionPath of
    Just path -> do
      let runToIO' = runM . runError @String . loggingIO . failLog
                   . Runix.FileSystem.Simple.filesystemIO
      result <- runToIO' $ miLoadSession path
      case result of
        Right msgs -> do
          -- Log successful reload
          sendAgentEvent uiVars (LogEvent Info (T.pack $ "Code reloaded - session restored with " ++ show (length msgs) ++ " messages"))
          -- Send loaded history to UI so it displays the messages
          when (not $ null msgs) $
            sendAgentEvent uiVars (AgentCompleteEvent msgs)
        Left err -> do
          sendAgentEvent uiVars (LogEvent Error (T.pack $ "Failed to load session: " ++ err))
    Nothing -> return ()

  -- Get data file path for the system prompt
  promptPath <- getDataFileName "prompt/runix-code.md"

  -- Get data directory for RunixDataDir config
  dataDir <- RunixDataDir <$> Paths_runix_code.getDataDir

  -- Load system prompt using the composed interpreter stack
  let runToIO' = runM . runError @String . loggingIO . failLog
               . Runix.FileSystem.Simple.filesystemIO

  result <- runToIO' $ loadSystemPrompt promptPath "You are a helpful AI coding assistant."
  let sysPrompt = case result of
        Right txt -> SystemPrompt txt
        Left _ -> SystemPrompt "You are a helpful AI coding assistant."

  -- Get executable path and initial modification time for code reloading
  exePath <- getExecutablePath
  stat <- getFileStatus exePath
  let initialMTime = fromIntegral $ fromEnum $ modificationTime stat

  _ <- forkIO $ agentLoop cwd dataDir uiVars sysPrompt interpretModel miSaveSession exePath initialMTime
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



interpretTUIEffects ::
  (Member (Error String) r, Member (Embed IO) r) =>
  FilePath ->
  RunixDataDir ->
  UIVars msg ->
  Sem
    ( Runix.Grep.Grep ProjectFS
        : Runix.Grep.Grep RunixToolsFS
        : Bash
        : Cmds
        : PromptStore
        : ConfigEffect.Config RunixDataDir
        : HTTP
        : FileSystemWrite RunixToolsFS
        : FileSystemRead RunixToolsFS
        : FileSystem RunixToolsFS
        : FileSystemWrite ClaudeConfigFS
        : FileSystemRead ClaudeConfigFS
        : FileSystem ClaudeConfigFS
        : FileWatcher ProjectFS
        : FileSystemWrite ProjectFS
        : FileSystemRead ProjectFS
        : FileSystem ProjectFS
        : Runix.FileSystem.System.FileSystemRead
        : Runix.FileSystem.System.FileSystemWrite
        : Fail
        : Logging
        : UserInput TUIWidget
        : UI.ForegroundCmd.ForegroundCmd
        : AgentWidgets msg
        : UI.UI
        : r
    )
    a ->
  Sem r a
interpretTUIEffects cwd (RunixDataDir runixCodeDir) uiVars =
  interpretUI uiVars
    . interpretAgentWidgets uiVars
    . interpretForegroundCmd uiVars
    . interpretUserInput uiVars
    . interpretLoggingToUI
    . failLog
    -- Base System filesystem
    . Runix.FileSystem.System.filesystemIO
    -- ProjectFS: user's project with chroot, filters, logging, and file watching
    . fileSystemLocal (ProjectFS cwd)
    . fileWatcherINotify @ProjectFS
    . interceptFileAccessWrite @ProjectFS
    . interceptFileAccessRead @ProjectFS
    . loggingWrite @ProjectFS "project"
    . filterWrite @ProjectFS (hideClaude <> hideGit)
    . filterRead @ProjectFS (hideGit)
    . filterFileSystem @ProjectFS (hideGit)
    -- ClaudeConfigFS: access to .claude directories (read-only)
    . fileSystemLocal (ClaudeConfigFS cwd)
    . filterFileSystem @ClaudeConfigFS (onlyClaude)
    . filterRead @ClaudeConfigFS (onlyClaude)
    -- RunixToolsFS: runix-code source directory
    . fileSystemLocal (RunixToolsFS runixCodeDir)
    . loggingWrite @RunixToolsFS "runix-tools"
    . httpIO (withRequestTimeout 300)
    . ConfigEffect.runConfig (RunixDataDir runixCodeDir)
    . promptStoreIO
    . cmdsIO
    . bashIO
    . grepForFilesystem @RunixToolsFS
    . grepForFilesystem @ProjectFS

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
