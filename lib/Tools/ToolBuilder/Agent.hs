{-# LANGUAGE UndecidableInstances #-}

-- | Tool-builder agent with agent loop
--
-- The tool-builder agent creates and modifies tools through an iterative process:
-- 1. Inherits full conversation history (context from caller)
-- 2. Runs restricted agent loop with limited tools
-- 3. Validates compilation with retry logic
-- 4. Discards iteration messages, returns only summary
module Tools.ToolBuilder.Agent
  ( buildTool
  , toolBuilderLoop
  , runToolBuilderSession
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Polysemy (Member, Members, Sem, raise)
import Polysemy.State (State, get, put)
import Polysemy.Fail (Fail)
import UniversalLLM.Core.Types (Message(..))
import UniversalLLM.Core.Tools (LLMTool(..), llmToolToDefinition)
import UniversalLLM (HasTools, SupportsSystemPrompt, ProviderOf)
import qualified UniversalLLM as ULL
import Runix.LLM.Effects (LLM, queryLLM)
import Runix.LLM.ToolExecution (executeTool)
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite)
import Runix.Cmd.Effects (Cmd)
import Runix.Logging.Effects (Logging, info)
import Tools.ToolBuilder.Types
import Tools.ToolBuilder.Prompt (loadToolBuilderPrompt)
import qualified Tools  -- Import base tools

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Build or modify a tool using the tool-builder agent
--
-- This is the main entry point called by the runix-code agent.
-- It captures the calling agent's history for context, runs the tool-builder
-- in a restricted environment, and returns only the final summary.
buildTool
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member Cmd r
     , Members '[FileSystemRead, FileSystemWrite] r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => [LLMTool (Sem (Fail ': r))]  -- ^ Builder tools (passed from caller, just like Claude agents)
  -> ToolName
  -> ToolDescription
  -> BuildMode
  -> Sem r BuildToolResult
buildTool builderTools toolName desc mode = do
  -- Step 1: Capture calling agent's history (this is the context inheritance)
  callerHistory <- get @[Message model]

  -- Step 2: Load tool-builder system prompt
  toolBuilderPrompt <- loadToolBuilderPrompt

  -- Step 3: Construct task message for tool-builder
  let taskText = formatToolBuildTask toolName desc mode
      toolBuilderHistory = callerHistory ++ [UserText taskText]

  info $ "Starting tool-builder for: " <> getToolName toolName

  -- Step 4: Run tool-builder loop with restricted tools
  put @[Message model] toolBuilderHistory
  (summary, _finalHistory) <- runToolBuilderSession @model builderTools toolBuilderPrompt

  -- Step 5: Restore caller's history (unchanged)
  -- The tool-builder's iterations are discarded
  put @[Message model] callerHistory

  info $ "Tool-builder finished with: " <> buildMessage summary

  return summary

-- | Extract tool name from ToolName newtype
getToolName :: ToolName -> Text
getToolName (ToolName name) = name

--------------------------------------------------------------------------------
-- Task Formatting
--------------------------------------------------------------------------------

-- | Format the task description for the tool-builder
formatToolBuildTask :: ToolName -> ToolDescription -> BuildMode -> Text
formatToolBuildTask (ToolName name) (ToolDescription desc) mode =
  case mode of
    CreateNew ->
      T.unlines
        [ "BUILD NEW TOOL: " <> name
        , ""
        , "Description: " <> desc
        , ""
        , "Requirements:"
        , "- Read GeneratedTools.hs to understand existing patterns"
        , "- Create complete implementation with type signature and instances"
        , "- Append to GeneratedTools.hs"
        , "- Add the tool to the generatedTools list"
        , "- Add exports to the module export list"
        , "- Run cabal_build to validate"
        , "- If compilation fails, fix errors and retry"
        , "- When successful, report what you built"
        ]

    ModifyExisting (ToolName existingName) ->
      T.unlines
        [ "MODIFY EXISTING TOOL: " <> existingName
        , ""
        , "New specification: " <> desc
        , ""
        , "Requirements:"
        , "- Read GeneratedTools.hs and locate the existing tool"
        , "- Replace ONLY that tool's implementation"
        , "- Preserve all other tools"
        , "- Update the generatedTools list if needed"
        , "- Run cabal_build to validate"
        , "- If compilation fails, fix errors and retry"
        , "- When successful, report what changed"
        ]

--------------------------------------------------------------------------------
-- Tool-Builder Session
--------------------------------------------------------------------------------

-- | Run a single tool-builder session
-- Returns summary and final history (history will be discarded by caller)
runToolBuilderSession
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member Cmd r
     , Members '[FileSystemRead, FileSystemWrite] r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => [LLMTool (Sem (Fail ': r))]  -- ^ Builder tools
  -> Text  -- ^ System prompt
  -> Sem r (BuildToolResult, [Message model])
runToolBuilderSession builderTools systemPrompt = do
  let configs = [ULL.SystemPrompt systemPrompt]
      configsWithTools = setTools builderTools configs

  -- Run the loop
  result <- toolBuilderLoop @model configsWithTools builderTools

  finalHistory <- get @[Message model]
  return (result, finalHistory)

--------------------------------------------------------------------------------
-- Agent Loop
--------------------------------------------------------------------------------

-- | Tool-builder agent loop
--
-- Similar to runixCodeAgentLoop but with:
-- - Custom restricted tool set
-- - Compilation validation and retry logic
-- - Safety limit to prevent infinite loops
toolBuilderLoop
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member (State [Message model]) r
     , HasTools model
     )
  => [ULL.ModelConfig model]
  -> [LLMTool (Sem (Fail ': r))]
  -> Sem r BuildToolResult
toolBuilderLoop configs tools = do
  history <- get @[Message model]
  responseMsgs <- queryLLM configs history

  let updatedHistory = history ++ responseMsgs
      toolCalls = [tc | AssistantTool tc <- responseMsgs]

  put @[Message model] updatedHistory

  case toolCalls of
    [] -> do
      -- No more tool calls - extract final summary
      let responseText = case [txt | AssistantText txt <- responseMsgs] of
            (txt:_) -> txt
            [] -> "Tool build completed"

      -- Parse the response to determine success
      let success = detectSuccess responseText
          errors = if success then Nothing else Just responseText

      return $ BuildToolResult success responseText errors

    calls -> do
      -- Execute tools and continue
      results <- mapM (executeTool tools) calls
      let historyWithResults = updatedHistory ++ map ToolResultMsg results
      put @[Message model] historyWithResults

      -- Check if we hit max retries (safety limit)
      if countToolCalls updatedHistory > 20
        then return $ BuildToolResult
               False
               "Tool builder exceeded maximum iterations (20)"
               (Just "Too many retries - possible infinite loop")
        else toolBuilderLoop @model configs tools

-- | Count tool calls in history
countToolCalls :: [Message model] -> Int
countToolCalls = length . filter isToolCall
  where
    isToolCall (AssistantTool _) = True
    isToolCall _ = False

-- | Detect success from response text
detectSuccess :: Text -> Bool
detectSuccess txt =
  let lower = T.toLower txt
      hasSuccess = "success" `T.isInfixOf` lower
      hasFail = "fail" `T.isInfixOf` lower || "error" `T.isInfixOf` lower
  in hasSuccess && not hasFail

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Update config with new tool list
setTools :: HasTools model => [LLMTool (Sem r)] -> [ULL.ModelConfig model] -> [ULL.ModelConfig model]
setTools tools configs =
  let withoutTools = filter (not . isToolsConfig) configs
      toolDefs = map llmToolToDefinition tools
  in withoutTools ++ [ULL.Tools toolDefs]
  where
    isToolsConfig (ULL.Tools _) = True
    isToolsConfig _ = False
