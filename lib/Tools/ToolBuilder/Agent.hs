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
import qualified Data.Char
import Polysemy (Member, Members, Sem, raise)
import Polysemy.State (State, get, put)
import Polysemy.Fail (Fail)
import UniversalLLM.Core.Types (Message(..))
import UniversalLLM.Core.Tools (LLMTool(..), llmToolToDefinition)
import UniversalLLM (HasTools, SupportsSystemPrompt, ProviderOf)
import qualified UniversalLLM as ULL
import Runix.LLM.Effects (LLM, queryLLM)
import Runix.LLM.ToolExecution (executeTool)
import Runix.FileSystem.Effects (FileSystem, FileSystemRead, FileSystemWrite)
import Config (RunixToolsFS)
import Runix.Cmd.Effects (Cmd)
import Runix.Logging.Effects (Logging, info)
import Runix.Grep.Effects (Grep)
import Runix.PromptStore.Effects (PromptStore)
import Runix.Config.Effects (Config, getConfig)
import System.FilePath ((</>))
import Tools.ToolBuilder.Types
import Tools.ToolBuilder.Prompt (loadToolBuilderPrompt)
import qualified Config as AppConfig
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
     , Member PromptStore r
     , Member (Config AppConfig.RunixDataDir) r
     , Members '[FileSystem RunixToolsFS, FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => ToolName
  -> ToolDescription
  -> BuildMode
  -> Sem r BuildToolResult
buildTool toolName desc mode = do
  -- Get data directory and create build function for all operations
  AppConfig.RunixDataDir dataDir <- getConfig
  let cabalFilePath = dataDir </> "runix-code.cabal"
      registryFilePath = dataDir </> "generated-tools/GeneratedTools.hs"
      toolModulesDir = dataDir </> "generated-tools/GeneratedTools"
      build = Tools.cabalBuild (Tools.WorkingDirectory $ T.pack dataDir)

  -- PRECONDITION: Verify current source tree compiles, abort otherwise
  -- The agent can't fix a broken codebase it didn't create
  info "Verifying codebase compiles before starting tool-builder..."
  baselineCompile <- build
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

  -- Step 4: Run tool-builder loop (passing build function and paths)
  put @[Message model] toolBuilderHistory
  summary <- toolBuilderLoop @model toolBuilderPrompt cabalFilePath registryFilePath toolModulesDir build

  -- Step 5: Restore caller's history (unchanged)
  -- The tool-builder's iterations are discarded
  put @[Message model] callerHistory

  info $ "Tool-builder finished with: " <> buildMessage summary

  return summary

--------------------------------------------------------------------------------
-- Tool for Agent to Write Code
--------------------------------------------------------------------------------

-- | Atomically write tool code as a new module
-- Creates individual module file, updates registry and cabal file
-- If compilation fails, rolls everything back
writeToolcodeAtomic
  :: forall r.
     ( Member Fail r
     , Members '[FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member Logging r
     )
  => FilePath  -- ^ Path to cabal file
  -> FilePath  -- ^ Path to GeneratedTools.hs (registry)
  -> FilePath  -- ^ Path to GeneratedTools/ directory
  -> Sem r Tools.CabalBuildResult  -- ^ Build function
  -> ToolName
  -> ToolImplementation
  -> Sem r WriteToolcodeResult
writeToolcodeAtomic cabalPath registryPath modulesDir build (ToolName name) (ToolImplementation code) = do
  -- Derive module name from tool name (e.g., "echo" -> "Echo")
  let moduleName = toModuleName name
      moduleFilePath = modulesDir </> T.unpack moduleName <> ".hs"
      qualifiedModuleName = "GeneratedTools." <> moduleName

  info $ "Creating new tool module: " <> qualifiedModuleName

  -- Step 1: Read current state of all files (for rollback)
  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent

  cabalContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack cabalPath)
  let Tools.ReadFileResult cabalText = cabalContent

  -- Step 2: Create module file with proper module header
  let moduleContent = T.unlines
        [ "-- | Generated tool: " <> name
        , "module " <> qualifiedModuleName <> " where"
        , ""
        , code
        ]

  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack moduleFilePath) (Tools.FileContent moduleContent)
  info $ "Created module file: " <> T.pack moduleFilePath

  -- Step 3: Update cabal file to add new exposed-module
  let updatedCabal = addModuleToCabal cabalText qualifiedModuleName
  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack cabalPath) (Tools.FileContent updatedCabal)
  info "Updated cabal file with new module"

  -- Step 4: Update registry file
  let functionName = extractFunctionName code
      updatedRegistry = updateRegistry registryText moduleName qualifiedModuleName functionName
  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent updatedRegistry)
  info "Updated registry file with new tool"

  -- Step 5: Try to compile
  info "Attempting compilation..."
  buildResult <- build

  case buildResult of
    Tools.CabalBuildResult True _stdout _stderr -> do
      info $ "Tool successfully created and registered: " <> name
      return $ WriteToolcodeResult ("SUCCESS: Tool '" <> name <> "' has been created as module " <> qualifiedModuleName <> " and registered. You are DONE!")

    Tools.CabalBuildResult False _stdout stderr -> do
      -- Compilation failed - roll back all changes
      info $ "Compilation failed for tool: " <> name <> ", rolling back all changes"

      -- Restore registry file (this removes the import/export/registration)
      _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent registryText)

      -- Restore cabal file (this removes the module from exposed-modules)
      _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack cabalPath) (Tools.FileContent cabalText)

      -- Note: The module file remains but is orphaned (not referenced by cabal)
      -- This is harmless and avoids requiring file deletion privileges

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

-- | Extract tool name from ToolName newtype
getToolName :: ToolName -> Text
getToolName (ToolName name) = name

-- | Convert tool name to PascalCase module name
-- Examples: "echo" -> "Echo", "hello-world" -> "HelloWorld"
toModuleName :: Text -> Text
toModuleName name =
  let parts = T.split (\c -> c == '-' || c == '_') name
      capitalize t = case T.uncons t of
        Just (c, rest) -> T.cons (Data.Char.toUpper c) rest
        Nothing -> t
  in T.concat (map capitalize parts)

-- | Extract function name from tool code
-- Looks for the main function definition (e.g., "echoTool :: ...")
-- Skips record field definitions and data type constructors
extractFunctionName :: Text -> Text
extractFunctionName code =
  let codeLines = T.lines code
      -- Find lines with " :: " (type signature)
      sigLines = filter isTopLevelFunctionSig codeLines
      -- Take first signature, extract function name (everything before ::)
      firstSig = case sigLines of
        (sig:_) -> T.strip $ T.takeWhile (/= ':') sig
        [] -> "unknownTool"  -- fallback
  in firstSig
  where
    -- Check if line is a top-level function signature (not a record field)
    isTopLevelFunctionSig line =
      let trimmed = T.strip line
      in " :: " `T.isInfixOf` line &&  -- has type signature
         not (T.isPrefixOf "{" trimmed) &&  -- not record syntax start
         not ("}" `T.isInfixOf` line) &&  -- not in record block
         not (T.isPrefixOf "," trimmed) &&  -- not a record field continuation
         -- Function names start with lowercase or are operators
         case T.uncons trimmed of
           Just (c, _) -> Data.Char.isLower c || c == '('
           Nothing -> False

-- | Add module to cabal file between markers
addModuleToCabal :: Text -> Text -> Text
addModuleToCabal cabalContent moduleName =
  let contentLines = T.lines cabalContent
      -- Find the marker line
      (beforeMarker, afterMarker) = break (T.isInfixOf "GENERATED_TOOLS_MODULES_START") contentLines
      -- Insert new module after the marker
      updated = case afterMarker of
        (markerLine:rest) ->
          beforeMarker ++ [markerLine, "                    , " <> moduleName] ++ rest
        [] -> contentLines  -- marker not found, return unchanged
  in T.unlines updated

-- | Update registry file with import, export, and tool registration
updateRegistry :: Text -> Text -> Text -> Text -> Text
updateRegistry registryContent moduleName qualifiedModuleName functionName =
  let contentLines = T.lines registryContent

      -- Step 1: Add import
      (beforeImports, afterImports) = break (T.isInfixOf "GENERATED_TOOL_IMPORTS_START") contentLines
      withImport = case afterImports of
        (markerLine:rest) ->
          beforeImports ++ [markerLine, "import qualified " <> qualifiedModuleName <> " as " <> moduleName] ++ rest
        [] -> contentLines

      -- Step 2: Add export
      (beforeExports, afterExports) = break (T.isInfixOf "GENERATED_TOOL_EXPORTS_START") withImport
      withExport = case afterExports of
        (markerLine:rest) ->
          beforeExports ++ [markerLine, "  , " <> moduleName <> "." <> functionName] ++ rest
        [] -> withImport

      -- Step 3: Add to generatedTools list
      (beforeTools, afterTools) = break (T.isInfixOf "GENERATED_TOOLS_LIST_START") withExport
      withTool = case afterTools of
        (markerLine:rest) ->
          beforeTools ++ [markerLine, "    LLMTool " <> moduleName <> "." <> functionName] ++ rest
        [] -> withExport

  in T.unlines withTool

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
        , "- Read GeneratedTools.hs registry to understand the structure"
        , "- Create complete implementation with type signature and instances"
        , "- Use write_toolcode_atomic to create the new tool module"
        , "  (This will create GeneratedTools/{ModuleName}.hs, update the registry, and update the cabal file)"
        , "- The tool will be automatically registered in the generatedTools list"
        , "- Compilation is validated automatically"
        , "- If compilation fails, fix errors and retry with write_toolcode_atomic"
        , "- When successful, report what you built"
        ]

    ModifyExisting (ToolName existingName) ->
      T.unlines
        [ "MODIFY EXISTING TOOL: " <> existingName
        , ""
        , "New specification: " <> desc
        , ""
        , "Requirements:"
        , "- Read the existing tool module file (GeneratedTools/{ModuleName}.hs)"
        , "- Modify ONLY that tool's implementation"
        , "- Preserve the module structure"
        , "- Use write_file to update the module"
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
     , Member PromptStore r
     , Members '[FileSystem RunixToolsFS, FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member (State [Message model]) r
     , HasTools model
     , SupportsSystemPrompt (ProviderOf model)
     )
  => Text      -- ^ System prompt
  -> FilePath  -- ^ Path to cabal file
  -> FilePath  -- ^ Path to GeneratedTools.hs (registry)
  -> FilePath  -- ^ Path to GeneratedTools/ directory
  -> Sem r Tools.CabalBuildResult  -- ^ Build function
  -> Sem r BuildToolResult
toolBuilderLoop systemPrompt cabalPath registryPath modulesDir build = do
  -- CRITICAL: Explicit type signature with ScopedTypeVariables to bind 'r'
  let
      builderTools =
        [ LLMTool (Tools.readFile @RunixToolsFS)
        , LLMTool (Tools.glob @RunixToolsFS)
        , LLMTool Tools.grep
        , LLMTool (writeToolcodeAtomic cabalPath registryPath modulesDir (raise build))
        , LLMTool (raise build)
        ]
      configs = [ULL.SystemPrompt systemPrompt]
      configsWithTools = setTools builderTools configs

  -- FORCED CONTEXT: Always inject GeneratedTools.hs registry content at start of loop
  -- The agent doesn't decide whether to read it - we force-feed the current state
  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent

  -- Read cabal file and extract generated-tools sublibrary configuration
  cabalContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack cabalPath)
  let Tools.ReadFileResult cabalText = cabalContent
      cabalSublibraryExcerpt = extractGeneratedToolsSublibrary cabalText

  -- Get list of existing generated tool files
  generatedToolFiles <- Tools.glob @RunixToolsFS (Tools.Pattern "generated-tools/**/*.hs")
  let Tools.GlobResult fileList = generatedToolFiles
      fileTree = T.unlines $ map (\f -> "  - " <> f) fileList

  let contextMessage = SystemText $ T.unlines
        [ "=== CURRENT STATE OF GeneratedTools.hs (registry) ==="
        , registryText
        , "=== END OF GeneratedTools.hs ==="
        , ""
        , "=== CABAL SUBLIBRARY CONFIGURATION (for reference) ==="
        , cabalSublibraryExcerpt
        , "=== END OF CABAL CONFIGURATION ==="
        , ""
        , "=== EXISTING GENERATED TOOL FILES ==="
        , fileTree
        , "=== END OF FILE TREE ==="
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
      finalCompile <- build

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
          toolBuilderLoop @model systemPrompt cabalPath registryPath modulesDir build

    calls -> do
      -- Execute tools and continue
      results <- mapM (executeTool builderTools) calls
      let historyWithResults = updatedHistory ++ map ToolResultMsg results
      put @[Message model] historyWithResults

      -- Check if we hit max retries (safety limit)
      if countToolCalls updatedHistory > 20
        then fail $ "Tool builder exceeded maximum iterations (20)"

        else toolBuilderLoop @model systemPrompt cabalPath registryPath modulesDir build

-- | Count tool calls in history
countToolCalls :: [Message model] -> Int
countToolCalls = length . filter isToolCall
  where
    isToolCall (AssistantTool _) = True
    isToolCall _ = False


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Extract the 'library generated-tools' sublibrary section from cabal file
-- Reads from "library generated-tools" until the next unindented non-empty line
extractGeneratedToolsSublibrary :: Text -> Text
extractGeneratedToolsSublibrary cabalText =
  let contentLines = T.lines cabalText
      -- Find the start of generated-tools sublibrary
      (_, afterStart) = break (T.isPrefixOf "library generated-tools") contentLines
  in case afterStart of
    [] -> "-- library generated-tools section not found"
    (start:rest) ->
      let -- Take lines while they're either empty or indented
          sublibraryLines = takeWhile isPartOfBlock rest
          isPartOfBlock line =
            T.null (T.strip line) ||  -- empty line
            (not (T.null line) && T.head line `elem` [' ', '\t'])  -- indented line
      in T.unlines (start : sublibraryLines)

-- | Update config with new tool list
setTools :: HasTools model => [LLMTool (Sem r)] -> [ULL.ModelConfig model] -> [ULL.ModelConfig model]
setTools tools configs =
  let withoutTools = filter (not . isToolsConfig) configs
      toolDefs = map llmToolToDefinition tools
  in withoutTools ++ [ULL.Tools toolDefs]
  where
    isToolsConfig (ULL.Tools _) = True
    isToolsConfig _ = False
