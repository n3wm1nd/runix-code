-- | Runix Code - AI coding assistant
--
-- This module provides a single elegant function: runixCode
--
-- Design principles:
-- - Just a function, not a special "agent runner"
-- - Can be called from other Runix tasks
-- - Can be used as a tool via ToolFunction instance
-- - Newtypes prevent mixing up semantically different Text values
-- - No unnecessary wrapper types
module Agent
  ( -- * Core Functions
    runixCode       -- Stateful version (for composition)
  , runixCodeAgentLoop

    -- * Types
  , SystemPrompt (..)
  , UserPrompt (..)
  , RunixCodeResult (..)

    -- * Serialization (for CLI convenience)
  , AgentSession (..)
  , AgentConfig (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Control.Monad
import Polysemy 
import Polysemy.State 
import Polysemy.Reader 
import Polysemy.Fail 
import UniversalLLM
import qualified UniversalLLM as ULL
import UniversalLLM.Tools
import Runix.LLM.WithInfo
import qualified Tools
import qualified Tools.Claude
import qualified Tools.ToolBuilder.Agent as ToolBuilder
import qualified GeneratedTools
import Runix.Grep
import Runix.Cmd
import Runix.Logging
import Runix.PromptStore
import Runix.Config
import Runix.FileSystem
import Config (ProjectFS, ClaudeConfigFS, RunixToolsFS, RunixDataDir)
import UI.UserInput
import Autodocodec
import Runix.LLM.ToolExecution (executeTool)
import qualified Runix.FileSystem.Simple as Simple

--------------------------------------------------------------------------------
-- Semantic Newtypes
--------------------------------------------------------------------------------

-- | System prompt - defines agent behavior
newtype SystemPrompt = SystemPrompt Text

-- | User prompt - what the user wants
newtype UserPrompt = UserPrompt Text
  deriving stock (Show, Eq)

instance HasCodec UserPrompt where
  codec = Autodocodec.dimapCodec UserPrompt (\(UserPrompt t) -> t) codec

instance ToolParameter UserPrompt where
  paramName _ _ = "prompt"
  paramDescription _ = "the user's request or question"

--------------------------------------------------------------------------------
-- Result Type
--------------------------------------------------------------------------------

-- | Result from runixCode
--
-- This is a unique type so it can have a ToolFunction instance,
-- making runixCode callable as a tool by other agents.
data RunixCodeResult model = RunixCodeResult
  { updatedHistory :: [Message model]
  , responseText :: Text
  }
  deriving stock (Show)

-- For codec, we only encode/decode the response text (history is internal)
instance HasCodec (RunixCodeResult model) where
  codec = Autodocodec.dimapCodec
    (\txt -> RunixCodeResult [] txt)
    responseText
    codec

instance ToolParameter (RunixCodeResult model) where
  paramName _ _ = "result"
  paramDescription _ = "result from the runix code agent"


instance ToolFunction (RunixCodeResult model) where
  toolFunctionName _ = "runix_code"
  toolFunctionDescription _ = "AI coding assistant that can read/write files, run shell commands, and help with code"

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Format file changes with diffs as a system message
-- Diffs old content (via stdin) against current file
formatFileChanges :: Members '[FileSystem ProjectFS, FileSystemRead ProjectFS, Cmd "diff", Fail] r
                  => [(String, ByteString, ByteString)]
                  -> Sem r Text
formatFileChanges changes = do
  let header = T.pack $ "SYSTEM NOTIFICATION: " ++ show (length changes) ++ " file(s) changed externally:\n\n"

  -- Process each changed file
  diffs <- forM changes $ \(path, oldContent, _newContent) -> do
    -- Run diff with old content via stdin, label it as path.old
    Tools.DiffResult diffOutput <- Tools.diffContentVsFile @ProjectFS (path ++ ".old") oldContent (Tools.FilePath $ T.pack path)
    return diffOutput

  return $ header <> T.intercalate "\n---\n\n" diffs

--------------------------------------------------------------------------------
-- (State effect for todo tracking is run locally in agent loop)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Core Function
--------------------------------------------------------------------------------

-- | Runix Code - AI coding assistant (stateful version for composition)
-- Uses State for message history and Reader for configs
runixCode
  :: forall model widget r.
     ( Member (LLM model) r
     , Member Fail r
     , Member (Grep ProjectFS) r
     , Member (Grep RunixToolsFS) r
     , Member Logging r
     , Member (UserInput widget) r
     , Member Cmds r
     , Member (FileWatcher ProjectFS) r
     , Member PromptStore r
     , Member (Config RunixDataDir) r
     , Members [FileSystem ProjectFS, FileSystemRead ProjectFS, FileSystemWrite ProjectFS] r
     , Members [FileSystem ClaudeConfigFS, FileSystemRead ClaudeConfigFS] r
     , Members [FileSystem RunixToolsFS, FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , ImplementsWidget widget Text
     , Member (State [Message model]) r
     , Member (Reader [ModelConfig model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => SystemPrompt
  -> UserPrompt
  -> Sem r (RunixCodeResult model)
runixCode (Agent.SystemPrompt sysPrompt) (UserPrompt userPrompt) = do
  baseConfigs <- ask @[ModelConfig model]
  currentHistory <- get @[Message model]

  -- Load CLAUDE.md files and add as system prompts if they exist
  claudeInstructions <- Tools.Claude.loadClaudeMdConfigs

  let claudeMdConfigs = map (\(Tools.Claude.ClaudeInstructions txt) -> ULL.SystemPrompt txt) claudeInstructions
      configsWithSystem = ULL.SystemPrompt sysPrompt : claudeMdConfigs ++ baseConfigs
      newHistory = currentHistory ++ [UserText userPrompt]

  -- Add user prompt to history
  put @[Message model] newHistory

  -- Run agent loop with Reader for configs and State for todos locally
  -- Build tools once inside the effect stack, before entering the loop
  (_finalTodos, result) <-
    runState ([] :: [Tools.Todo]) . interpretCmd @"cabal" . interpretCmd @"diff"
      -- as the default filesystem
      . Simple.withDefaultFileSystem @ProjectFS
      . Simple.withDefaultFileSystemRead @ProjectFS
      . Simple.withDefaultFileSystemWrite @ProjectFS
      . runReader configsWithSystem $ do
        -- Load Claude Code integrations (subagents and skills) - once, not per iteration
        subagents <- Tools.Claude.loadSubagents
        skills <- Tools.Claude.loadSkills

        let
            baseTools =
              [ LLMTool (Tools.grep @ProjectFS)
              , LLMTool (Tools.glob @ProjectFS)
              , LLMTool (Tools.readFile @ProjectFS)
              , LLMTool (Tools.getCwd @ProjectFS)
              , LLMTool (Tools.ask @widget)
              , LLMTool Tools.todoWrite
              , LLMTool Tools.todoRead
              , LLMTool Tools.todoCheck
              , LLMTool Tools.todoDelete
              , LLMTool Tools.cabalBuild
              ]

            -- Add tool-builder to baseTools
            allBaseTools = baseTools ++ [LLMTool (ToolBuilder.buildTool @model)]

        -- Convert subagents to tools
        subagentTools <- return $ map (Tools.Claude.claudeSubagentToTool @model allBaseTools) subagents

        -- Convert skills to tools
        skillTools <- mapM (Tools.Claude.claudeSkillToTool @model) skills

        -- Combine all tools once
        let tools = allBaseTools ++ subagentTools ++ skillTools ++ GeneratedTools.generatedTools

        
        runixCodeAgentLoop @model @widget tools
  return result

-- | Update config with new tool list
setTools :: HasTools model => [LLMTool (Sem r)] -> [ModelConfig model] -> [ModelConfig model]
setTools tools configs =
  let withoutTools = filter (not . isToolsConfig) configs
      toolDefs = map llmToolToDefinition tools
  in withoutTools ++ [Tools toolDefs]
  where
    isToolsConfig (Tools _) = True
    isToolsConfig _ = False

-- | Agent loop - receives pre-built tools (loaded once, reused across iterations)
runixCodeAgentLoop
  :: forall model widget r.
     ( Member (LLM model) r
     , Member Fail r
     , Member (Grep ProjectFS) r
     , Member Logging r
     , Member (UserInput widget) r
     , Members '[Cmd "cabal", Cmd "diff"] r
     , Member (FileWatcher ProjectFS) r
     , Member PromptStore r
     , Member (Config RunixDataDir) r
     , Members '[FileSystem ProjectFS, FileSystemRead ProjectFS, FileSystemWrite ProjectFS] r
     , Members '[FileSystem ClaudeConfigFS, FileSystemRead ClaudeConfigFS] r
     , Members '[FileSystem RunixToolsFS, FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , ImplementsWidget widget Text
     , Member (Reader [ModelConfig model]) r
     , Member (State [Message model]) r
     , Member (State [Tools.Todo]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => [LLMTool (Sem (Fail ': r))]  -- ^ Pre-built tools with Fail effect (loaded once, reused)
  -> Sem r (RunixCodeResult model)
runixCodeAgentLoop tools = do
  baseConfigs <- ask @[ModelConfig model]
  let configs = setTools tools baseConfigs

  -- Check for file changes and inject as system messages
  currentHistory <- get @[Message model]

  changedFiles <- getChangedFiles @ProjectFS

  historyWithChanges <- if null changedFiles
        then return currentHistory
        else do
          -- Log detected changes
          info $ T.pack $ "Detected " ++ show (length changedFiles) ++ " file change(s): " ++
                          show (map (\(path, _, _) -> path) changedFiles)

          -- Run formatFileChanges with runFail - if it fails, just skip the notification
          diffResult <- runFail $ formatFileChanges changedFiles
          case diffResult of
            Right diffText -> return $ currentHistory ++ [SystemText diffText]
            Left _err -> return currentHistory  -- Skip notification if diff fails

  -- Update history with change notifications before querying LLM
  put @[Message model] historyWithChanges

  responseMsgs <- queryLLM configs historyWithChanges

  let historyWithResponse = historyWithChanges ++ responseMsgs
      toolCalls = [tc | AssistantTool tc <- responseMsgs]

  -- Update history state
  put @[Message model] historyWithResponse

  case toolCalls of
    [] -> do
      let assistantResponse = case [txt | AssistantText txt <- responseMsgs] of
            (txt:_) -> txt
            [] -> ""
      return $ RunixCodeResult historyWithResponse assistantResponse

    calls -> do
      -- Execute all tool calls with logging - tools mutate State [Todo] directly
      results <- mapM (interceptFileAccessRead @ProjectFS . interceptFileAccessWrite @ProjectFS . executeTool tools) calls
      let historyWithResults = historyWithResponse ++ map ToolResultMsg results

      -- Update history again with tool results
      put @[Message model] historyWithResults

      -- Recurse with same tools
      runixCodeAgentLoop @model @widget tools

--------------------------------------------------------------------------------
-- Serialization Types (CLI convenience only)
--------------------------------------------------------------------------------

-- | Agent session - for saving/loading sessions
data AgentSession model = AgentSession
  { history :: [Message model]
  }

-- | Agent config - for loading system prompt from file
data AgentConfig = AgentConfig
  { systemPrompt :: SystemPrompt
  }
