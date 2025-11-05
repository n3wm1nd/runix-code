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
  ( -- * Core Function
    runixCode
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
import Polysemy (Member, Sem)
import Polysemy.State (State, runState)
import UniversalLLM.Core.Types (Message(..))
import UniversalLLM.Core.Tools (LLMTool(..), llmToolToDefinition, executeToolCallFromList)
import UniversalLLM (HasTools, SupportsTemperature, SupportsSystemPrompt)
import qualified UniversalLLM as ULL
import Runix.LLM.Effects (LLM, queryLLM)
import Runix.LLM.ToolInstances ()
import qualified Tools
import Runix.FileSystem.Effects (FileSystem)

--------------------------------------------------------------------------------
-- Semantic Newtypes
--------------------------------------------------------------------------------

-- | System prompt - defines agent behavior
newtype SystemPrompt = SystemPrompt Text

-- | User prompt - what the user wants
newtype UserPrompt = UserPrompt Text

--------------------------------------------------------------------------------
-- Result Type
--------------------------------------------------------------------------------

-- | Result from runixCode
--
-- This is a unique type so it can have a ToolFunction instance,
-- making runixCode callable as a tool by other agents.
data RunixCodeResult provider model = RunixCodeResult
  { updatedHistory :: [Message model provider]
  , responseText :: Text
  }

-- TODO: Add ToolFunction instance when we implement tools
-- instance ToolFunction (RunixCodeResult provider model) where
--   toolFunctionName _ = "runix_code"
--   toolFunctionDescription _ = "AI coding assistant that can read/write files, run shell commands, and help with code"

--------------------------------------------------------------------------------
-- (State effect for todo tracking is run locally in agent loop)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Core Function
--------------------------------------------------------------------------------

-- | Runix Code - AI coding assistant
runixCode
  :: forall provider model r.
     ( Member (LLM provider model) r
     , Member FileSystem r
     , HasTools model provider
     , SupportsTemperature provider
     , SupportsSystemPrompt provider
     )
  => SystemPrompt
  -> [Message model provider]
  -> UserPrompt
  -> Sem r (RunixCodeResult provider model)
runixCode (SystemPrompt sysPrompt) messageHistory (UserPrompt userPrompt) = do
  let baseConfigs = [ ULL.SystemPrompt sysPrompt
                    , ULL.Temperature 0.7
                    ]
      newHistory = messageHistory ++ [UserText userPrompt]
      initialTodos = [] :: [Tools.Todo]  -- Start with empty todo list

  -- Agent loop handles State [Tools.Todo] internally, returns result and final todos
  (result, _finalTodos) <- runixCodeAgentLoop baseConfigs initialTodos newHistory
  return result

-- | Build tool list - tools can use State [Tools.Todo] effect
buildTools :: forall r. (Member FileSystem r, Member (State [Tools.Todo]) r) => [LLMTool (Sem r)]
buildTools =
  [ LLMTool (Tools.readFile @r)
  , LLMTool (Tools.writeFile @r)
  , LLMTool (Tools.editFile @r)
  , LLMTool (Tools.glob @r)
  , LLMTool (Tools.grep @r)
  , LLMTool (Tools.bash @r)
  , LLMTool (Tools.todoWrite @r)  -- Uses State [Tools.Todo] directly
  ]

-- | Update config with new tool list
setTools :: HasTools model provider => [LLMTool (Sem r)] -> [ULL.ModelConfig provider model] -> [ULL.ModelConfig provider model]
setTools tools configs =
  let withoutTools = filter (not . isToolsConfig) configs
      toolDefs = map llmToolToDefinition tools
  in withoutTools ++ [ULL.Tools toolDefs]
  where
    isToolsConfig (ULL.Tools _) = True
    isToolsConfig _ = False

-- | Agent loop - handles tool calling recursion
-- This is exposed so others can build upon it (e.g., custom tool loops)
-- Runs State [Tools.Todo] internally, returns result and final todos
runixCodeAgentLoop
  :: forall provider model r.
     ( Member (LLM provider model) r
     , Member FileSystem r
     , HasTools model provider
     )
  => [ULL.ModelConfig provider model]
  -> [Tools.Todo]  -- Initial todos
  -> [Message model provider]
  -> Sem r (RunixCodeResult provider model, [Tools.Todo])  -- Returns result and final todos
runixCodeAgentLoop baseConfigs initialTodos currentHistory = do
  (finalTodos, result) <- runState initialTodos (runixCodeAgentLoopWithState baseConfigs currentHistory)
  return (result, finalTodos)

-- | Internal loop with State [Tools.Todo] in effect stack
runixCodeAgentLoopWithState
  :: forall provider model r.
     ( Member (LLM provider model) r
     , Member FileSystem r
     , Member (State [Tools.Todo]) r
     , HasTools model provider
     )
  => [ULL.ModelConfig provider model]
  -> [Message model provider]
  -> Sem r (RunixCodeResult provider model)
runixCodeAgentLoopWithState baseConfigs currentHistory = do
  let tools = buildTools @r
      configs = setTools tools baseConfigs

  responseMsgs <- queryLLM configs currentHistory

  let historyWithResponse = currentHistory ++ responseMsgs
      toolCalls = [tc | AssistantTool tc <- responseMsgs]

  case toolCalls of
    [] -> do
      let assistantResponse = case [txt | AssistantText txt <- responseMsgs] of
            (txt:_) -> txt
            [] -> ""
      return $ RunixCodeResult historyWithResponse assistantResponse

    calls -> do
      -- Execute all tool calls - tools mutate State [Todo] directly
      results <- mapM (executeToolCallFromList tools) calls
      let historyWithResults = historyWithResponse ++ map ToolResultMsg results
      runixCodeAgentLoopWithState baseConfigs historyWithResults

--------------------------------------------------------------------------------
-- Serialization Types (CLI convenience only)
--------------------------------------------------------------------------------

-- | Agent session - for saving/loading sessions
data AgentSession provider model = AgentSession
  { history :: [Message model provider]
  }

-- | Agent config - for loading system prompt from file
data AgentConfig = AgentConfig
  { systemPrompt :: SystemPrompt
  }
