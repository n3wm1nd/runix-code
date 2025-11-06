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
  , runRunixCode    -- Pure version (for standalone use)
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
import Polysemy.State (State, runState, get, put)
import Polysemy.Reader (Reader, runReader, ask)
import UniversalLLM.Core.Types (Message(..))
import UniversalLLM.Core.Tools (LLMTool(..), llmToolToDefinition, executeToolCallFromList, ToolFunction(..), ToolParameter(..))
import UniversalLLM (HasTools, SupportsSystemPrompt)
import qualified UniversalLLM as ULL
import Runix.LLM.Effects (LLM, queryLLM)
import Runix.LLM.ToolInstances ()
import qualified Tools
import Runix.FileSystem.Effects (FileSystem)
import Autodocodec (HasCodec(..))
import qualified Autodocodec

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
data RunixCodeResult provider model = RunixCodeResult
  { updatedHistory :: [Message model provider]
  , responseText :: Text
  }
  deriving stock (Show)

-- For codec, we only encode/decode the response text (history is internal)
instance HasCodec (RunixCodeResult provider model) where
  codec = Autodocodec.dimapCodec
    (\txt -> RunixCodeResult [] txt)
    responseText
    codec

instance ToolParameter (RunixCodeResult provider model) where
  paramName _ _ = "result"
  paramDescription _ = "result from the runix code agent"

instance ToolFunction (RunixCodeResult provider model) where
  toolFunctionName _ = "runix_code"
  toolFunctionDescription _ = "AI coding assistant that can read/write files, run shell commands, and help with code"

--------------------------------------------------------------------------------
-- (State effect for todo tracking is run locally in agent loop)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Core Function
--------------------------------------------------------------------------------

-- | Runix Code - AI coding assistant (stateful version for composition)
-- Uses State for message history and Reader for system prompt
runixCode
  :: forall provider model r.
     ( Member (LLM provider model) r
     , Member FileSystem r
     , Member (State [Message model provider]) r
     , Member (Reader SystemPrompt) r
     , HasTools model provider
     , SupportsSystemPrompt provider
     )
  => UserPrompt
  -> Sem r (RunixCodeResult provider model)
runixCode (UserPrompt userPrompt) = do
  SystemPrompt sysPrompt <- ask
  currentHistory <- get @[Message model provider]

  let baseConfigs :: [ULL.ModelConfig provider model]
      baseConfigs = [ ULL.SystemPrompt sysPrompt
                    ]
      newHistory = currentHistory ++ [UserText userPrompt]

  -- Add user prompt to history
  put @[Message model provider] newHistory

  -- Run agent loop with Reader for configs and State for todos locally
  (_finalTodos, result) <-
    runState ([] :: [Tools.Todo]) $
      runReader baseConfigs $
        runixCodeAgentLoop
  return result

-- | Pure wrapper for runixCode (for standalone use)
-- Interprets State and Reader effects, returns explicit values
runRunixCode
  :: forall provider model r.
     ( Member (LLM provider model) r
     , Member FileSystem r
     , HasTools model provider
     , SupportsSystemPrompt provider
     )
  => SystemPrompt
  -> [Message model provider]
  -> UserPrompt
  -> Sem r (RunixCodeResult provider model, [Message model provider])
runRunixCode sysPrompt initialHistory userPrompt = do
  (finalHistory, result) <- runReader sysPrompt $
                              runState initialHistory $
                                runixCode userPrompt
  return (result, finalHistory)

-- | Update config with new tool list
setTools :: HasTools model provider => [LLMTool (Sem r)] -> [ULL.ModelConfig provider model] -> [ULL.ModelConfig provider model]
setTools tools configs =
  let withoutTools = filter (not . isToolsConfig) configs
      toolDefs = map llmToolToDefinition tools
  in withoutTools ++ [ULL.Tools toolDefs]
  where
    isToolsConfig (ULL.Tools _) = True
    isToolsConfig _ = False

-- | Agent loop - reads base configs from Reader, builds tools each iteration
runixCodeAgentLoop
  :: forall provider model r.
     ( Member (LLM provider model) r
     , Member FileSystem r
     , Member (Reader [ULL.ModelConfig provider model]) r
     , Member (Reader SystemPrompt) r
     , Member (State [Message model provider]) r
     , Member (State [Tools.Todo]) r
     , HasTools model provider
     , SupportsSystemPrompt provider
     )
  => Sem r (RunixCodeResult provider model)
runixCodeAgentLoop = do
  baseConfigs <- ask @[ULL.ModelConfig provider model]

  let tools :: [LLMTool (Sem r)]
      tools =
        [ LLMTool Tools.readFile
        , LLMTool Tools.writeFile
        , LLMTool Tools.editFile
        , LLMTool Tools.glob
        , LLMTool Tools.grep
        , LLMTool Tools.bash
        , LLMTool Tools.todoWrite
        , LLMTool Tools.todoRead
        , LLMTool Tools.todoCheck
        , LLMTool Tools.todoDelete
        -- Recursive agent starts with fresh history, shares SystemPrompt Reader
        , LLMTool (\prompt -> fmap snd $ runState @[Message model provider] [] $ runixCode @provider @model prompt)
        ]
      configs = setTools tools baseConfigs

  currentHistory <- get @[Message model provider]
  responseMsgs <- queryLLM configs currentHistory

  let historyWithResponse = currentHistory ++ responseMsgs
      toolCalls = [tc | AssistantTool tc <- responseMsgs]

  -- Update history state
  put @[Message model provider] historyWithResponse

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

      -- Update history again with tool results
      put @[Message model provider] historyWithResults

      -- Recurse
      runixCodeAgentLoop

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
