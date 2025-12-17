{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


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
import Runix.Grep.Effects (Grep)
import Tools.ToolBuilder.Types
import Tools.ToolBuilder.Prompt (loadToolBuilderPrompt)
import qualified Tools  -- Import base tools
import Runix.LLM.ToolInstances ()
import qualified Autodocodec
import qualified UniversalLLM.Core.Tools

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
     , Member Fail r
     , Member Grep r
     , Members '[FileSystemRead, FileSystemWrite] r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => ToolName
  -> ToolDescription
  -> BuildMode
  -> Sem r BuildToolResult
buildTool toolName desc mode = do
  -- PRECONDITION: Verify current source tree compiles, abort otherwise
  -- The agent can't fix a broken codebase it didn't create
  info "Verifying codebase compiles before starting tool-builder..."
  baselineCompile <- Tools.cabalBuild (Tools.WorkingDirectory ".")
  case baselineCompile of
    Tools.CabalBuildResult False _stdout stderr -> do
      fail $ "Codebase does not compile. Fix these errors first:\n" <> T.unpack stderr
    Tools.CabalBuildResult True _ _ -> do
      info "Baseline compilation successful, proceeding with tool-builder"

  -- Step 1: Capture calling agent's history (this is the context inheritance)
  callerHistory <- get @[Message model]

  -- Step 2: Load tool-builder system prompt
  toolBuilderPrompt <- loadToolBuilderPrompt

  -- Step 3: Construct task message for tool-builder
  let taskText = formatToolBuildTask toolName desc mode
      toolBuilderHistory = callerHistory ++ [UserText taskText]

  info $ "Starting tool-builder for: " <> getToolName toolName

  -- Step 4: Run tool-builder loop
  put @[Message model] toolBuilderHistory
  summary <- toolBuilderLoop @model toolBuilderPrompt

  -- Step 5: Restore caller's history (unchanged)
  -- The tool-builder's iterations are discarded
  put @[Message model] callerHistory

  info $ "Tool-builder finished with: " <> buildMessage summary

  return summary

--------------------------------------------------------------------------------
-- Tool for Agent to Write Code
--------------------------------------------------------------------------------

-- | Atomically write tool code to GeneratedTools.hs
-- Appends the code, attempts compilation, and rolls back if it fails
writeToolcodeAtomic
  :: forall r.
     ( Member Cmd r
     , Member Fail r
     , Members '[FileSystemRead, FileSystemWrite] r
     , Member Logging r
     )
  => ToolName
  -> ToolImplementation
  -> Sem r WriteToolcodeResult
writeToolcodeAtomic (ToolName name) (ToolImplementation code) = do
  -- Read current content
  currentContent <- Tools.readFile (Tools.FilePath $ T.pack generatedToolsPath)
  let Tools.ReadFileResult currentText = currentContent

  -- Append new tool with marker
  let marker = "-- Generated tool: " <> name
      newContent = currentText <> "\n\n" <> marker <> "\n" <> code

  -- Write it out
  _ <- Tools.writeFile (Tools.FilePath $ T.pack generatedToolsPath) (Tools.FileContent newContent)

  -- Try to compile
  buildResult <- Tools.cabalBuild (Tools.WorkingDirectory ".")

  case buildResult of
    Tools.CabalBuildResult True _stdout _stderr -> do
      info $ "Successfully added tool: " <> name
      return $ WriteToolcodeResult ("Tool '" <> name <> "' added and compiled successfully")

    Tools.CabalBuildResult False _stdout stderr -> do
      -- Compilation failed - roll back
      info $ "Compilation failed for tool: " <> name <> ", rolling back"
      _ <- Tools.writeFile (Tools.FilePath $ T.pack generatedToolsPath) (Tools.FileContent currentText)
      fail $ "Compilation failed:\n" <> T.unpack stderr

-- Result type
newtype WriteToolcodeResult = WriteToolcodeResult Text
  deriving stock (Show, Eq)
  deriving (Autodocodec.HasCodec) via Text

instance UniversalLLM.Core.Tools.ToolParameter WriteToolcodeResult where
  paramName _ _ = "result"
  paramDescription _ = "result of writing tool code"

instance UniversalLLM.Core.Tools.ToolFunction WriteToolcodeResult where
  toolFunctionName _ = "write_toolcode_atomic"
  toolFunctionDescription _ = "Atomically write tool code to GeneratedTools.hs. Code is appended, compiled, and rolled back if compilation fails."

-- | Path to GeneratedTools.hs (relative from project root where cabal build runs)
generatedToolsPath :: String
generatedToolsPath = "apps/runix-code/lib/GeneratedTools.hs"

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
     , Member Cmd r
     , Member Fail r
     , Member Grep r
     , Members '[FileSystemRead, FileSystemWrite] r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => Text  -- ^ System prompt
  -> Sem r BuildToolResult
toolBuilderLoop systemPrompt = do
  -- CRITICAL: Explicit type signature with ScopedTypeVariables to bind 'r'
  let
      builderTools =
        [ LLMTool Tools.readFile
        , LLMTool Tools.glob
        , LLMTool Tools.grep
        , LLMTool writeToolcodeAtomic
        ]
      configs = [ULL.SystemPrompt systemPrompt]
      configsWithTools = setTools builderTools configs

  -- FORCED CONTEXT: Always inject GeneratedTools.hs content at start of loop
  -- The agent doesn't decide whether to read it - we force-feed the current state
  generatedToolsContent <- Tools.readFile (Tools.FilePath $ T.pack generatedToolsPath)
  let Tools.ReadFileResult generatedToolsText = generatedToolsContent
      contextMessage = SystemText $ T.unlines
        [ "=== CURRENT STATE OF GeneratedTools.hs ==="
        , generatedToolsText
        , "=== END OF GeneratedTools.hs ==="
        ]

  history <- get @[Message model]
  let historyWithContext = history ++ [contextMessage]
  put @[Message model] historyWithContext
  responseMsgs <- queryLLM configsWithTools historyWithContext

  let updatedHistory = historyWithContext ++ responseMsgs
      toolCalls = [tc | AssistantTool tc <- responseMsgs]

  put @[Message model] updatedHistory

  case toolCalls of
    [] -> do
      -- No more tool calls - agent thinks it's done
      -- ENFORCE: Verify compilation before allowing completion
      info "Agent claims completion, verifying compilation..."
      finalCompile <- Tools.cabalBuild (Tools.WorkingDirectory ".")

      case finalCompile of
        Tools.CabalBuildResult True _ _ -> do
          -- Success! Actually done
          info "Compilation verified, tool-builder succeeded"
          let responseText = case [txt | AssistantText txt <- responseMsgs] of
                (txt:_) -> txt
                [] -> "Tool build completed"
          return $ BuildToolResult responseText

        Tools.CabalBuildResult False _stdout stderr -> do
          -- Compilation failed - force agent to continue and fix it
          info "Compilation failed after agent claimed completion, forcing retry"
          let errorMessage = SystemText $ T.unlines
                [ "COMPILATION FAILED - you are not done yet!"
                , "Fix these errors before claiming success:"
                , stderr
                ]
              historyWithError = updatedHistory ++ [errorMessage]
          put @[Message model] historyWithError
          toolBuilderLoop @model systemPrompt

    calls -> do
      -- Execute tools and continue
      results <- mapM (executeTool builderTools) calls
      let historyWithResults = updatedHistory ++ map ToolResultMsg results
      put @[Message model] historyWithResults

      -- Check if we hit max retries (safety limit)
      if countToolCalls updatedHistory > 20
        then fail $ "Tool builder exceeded maximum iterations (20)"

        else toolBuilderLoop @model systemPrompt

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
