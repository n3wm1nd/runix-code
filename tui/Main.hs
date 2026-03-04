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
import Data.List (find)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, when)
import qualified System.Directory as Dir
import System.Environment (getExecutablePath)
import System.Posix.Process (executeFile)
import System.Posix.Files (getFileStatus, modificationTime)
import qualified System.IO as IO

import Polysemy
import Polysemy.Error (runError, Error, catch)
import Polysemy.State (get)

import UniversalLLM (Message(..))
import UniversalLLM (ProviderOf)

import Config
import Models
import Runner (loadSystemPrompt, ModelInterpreter(..), runConfig, runHistory, buildAvailableModels, ModelEntry(..), entryInterpreter)
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
import Runix.HTTP (HTTP, HTTPStreaming, httpIO, httpIOStreaming, withRequestTimeout)
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
import UI.AgentWidgets (AgentWidgets, addMessage, replaceHistory)
import UI.AgentWidgetsInterpreter (interpretAgentWidgets)
import UI.StreamingInterceptor (interceptStreamChunksToUI)
import Runix.LLMStream (LLMStreaming)
import Runix.LLM.Interpreter (interpretLLMViaStreaming)
import qualified Paths_runix_code
import Paths_runix_code (getDataFileName)
import Runix.FileSystem (loggingWrite, filterRead, filterWrite, hideGit, hideClaude, filterFileSystem, fileSystemLocal, fileWatcherINotify, interceptFileAccessRead, interceptFileAccessWrite, onlyClaude)
import UI.OutputHistory (OutputItem(..), OutputHistoryZipper, listToZipper, addCompletedToolItems, extractMessages, emptyZipper, zipperToList)
import qualified Runix.LLM.Context


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

  -- Build available models by probing environment
  availableModels <- buildAvailableModels

  -- Select model by ID from available list
  let selectedEntry = case cfgModelId cfg of
        Nothing -> case availableModels of
          (first:_) -> first
          []        -> error "No models available - set ANTHROPIC_OAUTH_TOKEN, ZAI_API_KEY, etc."
        Just mid -> case find (\e -> meId e == mid) availableModels of
          Just entry -> entry
          Nothing -> error $ "Model '" ++ T.unpack (modelDisplayName mid)
            ++ "' not available. Available: "
            ++ T.unpack (T.intercalate ", " [meName e | e <- availableModels])

  IO.hPutStr IO.stderr $ "info: Using model: " ++ T.unpack (meName selectedEntry) ++ "\n"

  -- Unpack existential and run UI with full model list for interactive model switching
  case entryInterpreter selectedEntry of
    ModelInterpreter{interpretModelStreaming, miLoadSession, miSaveSession} ->
      runUI (\refreshCallback -> buildUIRunner availableModels interpretModelStreaming miLoadSession miSaveSession (cfgResumeSession cfg) refreshCallback)

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
          -> (forall r a. Members [Fail, HTTPStreaming] r => Sem (LLMStreaming model : r) a -> Sem r a)  -- LLMStreaming interpreter (internal)
          -> (forall r. (Members [Runix.FileSystem.Simple.FileSystem, Runix.FileSystem.Simple.FileSystemRead, Runix.FileSystem.Simple.FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ())  -- Save session function
          -> FilePath  -- Executable path
          -> Integer  -- Initial executable mtime
          -> IO ()
agentLoop cwd dataDir uiVars sysPrompt interpretModelStreaming miSaveSession exePath initialMTime = do
  -- Run the entire agent loop inside Sem so FileWatcher state persists
  -- Stack: interpretModelStreaming . interceptStreamChunksToUI . interpretLLMViaStreaming
  -- interpretModelStreaming: HTTPStreaming -> LLMStreaming
  -- interceptStreamChunksToUI: intercepts LLMStreaming (Sem r a -> Sem r a)
  -- interpretLLMViaStreaming: LLMStreaming -> LLM
  let runToIO' = runM . runError . interpretTUIEffects cwd dataDir uiVars
                   . interpretModelStreaming
                   . interceptStreamChunksToUI uiVars
                   . interpretLLMViaStreaming @model

  result <- runToIO' $ forever runOneIteration

  -- Handle top-level errors (though forever should never return)
  case result of
    Left err -> do
      clearCancellationFlag uiVars
      sendAgentEvent uiVars (LogEvent Error (T.pack $ "Fatal error: " ++ err))
    Right _ -> return ()

  where
    runOneIteration = do
      -- Wait for user request (includes text + settings + history zipper)
      UserRequest{userText, currentHistory, requestSettings} <- embed $ atomically $ waitForUserInput (userInputQueue uiVars)

      -- Clear any previous cancellation flag before starting new request
      embed $ clearCancellationFlag uiVars

      -- Extract messages from zipper for commands that need it
      let historyMessages = extractMessages currentHistory

      -- Build command set with current settings and history
      let cmdSet = CommandSet
            { slashCommands = [ echoCommand
                              , reloadCommand historyMessages
                              , clearCommand
                              , compactQuickCommand currentHistory
                              , showZipperCommand currentHistory
                              , let (name, fn) = ViewCmd.viewCommand historyMessages uiVars
                                in SlashCommand { commandName = name, commandFn = fn }
                              , let (name, fn) = HistoryCmd.historyCommand historyMessages uiVars
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
      embed $ checkAndReloadOnChange historyMessages

    -- | Perform reload: save session and exec new binary
    -- TODO: This is lossy - should save full OutputHistory (with tool calls, streaming state, etc)
    -- not just Message history. For now, reload only preserves conversation messages.
    performReload :: [Message model] -> IO ()
    performReload history = do
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

    -- | Check if executable has changed and reload if so
    checkAndReloadOnChange :: [Message model] -> IO ()
    checkAndReloadOnChange history = do
      -- Get current mtime of executable
      stat <- getFileStatus exePath
      let currentMTime = fromIntegral $ fromEnum $ modificationTime stat

      -- Check if executable has been modified (compare to initial mtime from startup)
      when (currentMTime /= initialMTime) $
        performReload history

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

    -- | Reload command: save session and restart with new binary
    reloadCommand :: forall r. Member (Embed IO) r => [Message model] -> SlashCommand r
    reloadCommand history = SlashCommand
      { commandName = "reload"
      , commandFn = \_ -> embed $ performReload history
      }

    -- | Clear command: reset history to empty
    clearCommand :: forall r. Member (Embed IO) r => SlashCommand r
    clearCommand = SlashCommand
      { commandName = "clear"
      , commandFn = \_ -> embed $ sendAgentEvent uiVars (RestoreSessionEvent emptyZipper)
      }

    -- | CompactQuick command: compress history by stripping tool results
    compactQuickCommand :: forall r. (Member (Embed IO) r, Member Logging r) => OutputHistoryZipper (Message model) -> SlashCommand r
    compactQuickCommand zipper = SlashCommand
      { commandName = "compactquick"
      , commandFn = \_ -> do
          -- Extract messages from zipper
          let historyMessages = extractMessages zipper
              beforeCount = length historyMessages

          -- Apply quick compression
          let compressedMessages = Runix.LLM.Context.compactQuick historyMessages
              afterCount = length compressedMessages

          -- Convert back to zipper (reverse for newest-first)
          let items = map MessageItem (reverse compressedMessages)
              itemsWithTools = addCompletedToolItems items
              newZipper = listToZipper itemsWithTools

          -- Send event to update UI (this replaces the zipper)
          embed $ sendAgentEvent uiVars (RestoreSessionEvent newZipper)

          -- Log the compression stats AFTER the restore, so the log message persists
          info $ T.pack $ "Compressed history: " ++ show beforeCount ++ " → " ++ show afterCount ++ " messages"
      }

    -- | ShowZipper command: debug output of current zipper contents
    showZipperCommand :: forall r. Member Logging r => OutputHistoryZipper (Message model) -> SlashCommand r
    showZipperCommand zipper = SlashCommand
      { commandName = "showzipper"
      , commandFn = \_ -> do
          let items = zipperToList zipper
          info $ T.pack $ "=== Zipper Contents (" ++ show (length items) ++ " items) ==="
          mapM_ (info . T.pack . show) items
          info $ T.pack "=== End Zipper ==="
      }

    -- | The default agent command: run runixCode with the user's input
    -- historyZipper is the current UI zipper, extract messages for the agent
    runDefaultAgentCommand historyZipper _settings userText = do
      catch
        (do -- Use default configs (no streaming)
            let baseConfigs = defaultConfigs @model
                historyMessages = extractMessages historyZipper

            -- Initialize AgentWidgets with current zipper, then run agent
            raiseUnder $ interpretAgentWidgets uiVars historyZipper $ do
              -- Add user message BEFORE entering subsection (so it's at root level)
              addMessage @(Message model) (UserText userText)

              -- Run agent (with widget isolation via interpretAsWidget)
              -- The do-block runs inside interpretAsWidget's subsection context
              _result <- interpretAsWidget @model $ do
                let runWithState = runConfig baseConfigs . runHistory historyMessages
                (result, finalHistory) <- runWithState $ do
                  -- Run the agent (user message already added above)
                  runixCode @model @TUIWidget sysPrompt (UserPrompt userText)

                -- After agent completes, reconcile final State with subsection zipper.
                -- The subsection should only contain messages from the agent's RESPONSE,
                -- not the user query (which is at root level) or previous history.
                -- Extract new messages and skip the user query.
                -- FIXME: This is fragile! We're just counting messages and assuming:
                --   1. The user query is always the first new message
                --   2. History wasn't compacted/reordered by the agent
                -- We need a proper way to identify which messages belong to the subsection,
                -- perhaps by marking them or using a different approach entirely.
                let allNewMessages = drop (length historyMessages) finalHistory
                    -- Skip the user query (first new message) - it's already at root level
                    subsectionMessages = drop 1 allNewMessages
                replaceHistory @(Message model) subsectionMessages
                return result

              -- Signal completion so UI updates status
              embed $ sendAgentEvent uiVars (AgentCompleteEvent [])
        )
        (\err -> embed $ sendAgentEvent uiVars (LogEvent Error (T.pack $ "Command error: " ++ err)))

--------------------------------------------------------------------------------
-- UI Runner Builder
--------------------------------------------------------------------------------

-- | Build a UI runner by composing model interpreter with UI effects
--
-- Takes the full list of available models and the selected entry.
-- Unpacks the existential ModelEntry, creates interpreter, and wires
-- everything into the UI. The available models list is threaded through
-- for future interactive model switching.
buildUIRunner :: forall model.
                 ( HasTools model
                 , SupportsSystemPrompt (ProviderOf model)
                 , ModelDefaults model
                 )
              => [ModelEntry]   -- All available models (for runtime switching)
              -> (forall r a. Members [Fail, HTTPStreaming] r => Sem (LLMStreaming model : r) a -> Sem r a)  -- LLMStreaming interpreter
              -> (forall r. (Members [Runix.FileSystem.Simple.FileSystem, Runix.FileSystem.Simple.FileSystemRead, Runix.FileSystem.Simple.FileSystemWrite, Logging, Fail] r) => FilePath -> Sem r [Message model])  -- Load session
              -> (forall r. (Members [Runix.FileSystem.Simple.FileSystem, Runix.FileSystem.Simple.FileSystemRead, Runix.FileSystem.Simple.FileSystemWrite, Logging, Fail] r) => FilePath -> [Message model] -> Sem r ())  -- Save session
              -> Maybe FilePath  -- Resume session path
              -> (AgentEvent (Message model) -> IO ())  -- Refresh callback
              -> IO (UIVars (Message model))
buildUIRunner _availableModels interpretModelStreaming miLoadSession miSaveSession maybeSessionPath refreshCallback = do
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
          -- Convert messages to OutputItems and create zipper
          -- Send RestoreSessionEvent (not ZipperUpdateEvent) so AgentWidgets state isn't affected
          when (not $ null msgs) $ do
            let items = map MessageItem (reverse msgs)  -- msgs is oldest-first, zipper needs newest-first
                itemsWithTools = addCompletedToolItems items
                zipper = listToZipper itemsWithTools
            sendAgentEvent uiVars (RestoreSessionEvent zipper)
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

  _ <- forkIO $ agentLoop cwd dataDir uiVars sysPrompt interpretModelStreaming miSaveSession exePath initialMTime
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



interpretTUIEffects :: forall msg r a.
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
        : HTTPStreaming
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
        : UI.UI
        : r
    )
    a ->
  Sem r a
interpretTUIEffects cwd (RunixDataDir runixCodeDir) uiVars =
  interpretUI uiVars
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
    . httpIOStreaming (withRequestTimeout 300)
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
