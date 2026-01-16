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
  -- * Exported for testing
  , ToolDef(..)
  , parseToolsList
  , renderToolsList
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char
import Data.Maybe (mapMaybe)
import Polysemy (Member, Members, Sem, raise)
import Polysemy.State (State, get, put)
import Polysemy.Fail (Fail)
import UniversalLLM (Message(..))
import UniversalLLM.Tools (LLMTool(..), llmToolToDefinition)
import UniversalLLM (HasTools, SupportsSystemPrompt, ProviderOf)
import qualified UniversalLLM as ULL
import Runix.LLM (LLM, queryLLM)
import Runix.LLM.ToolExecution (executeTool)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Config (RunixToolsFS(..))
import Runix.Cmd (Cmds, interpretCmd)
import Runix.Logging (Logging, info)
import Runix.Grep (Grep)
import Runix.PromptStore (PromptStore)
import System.FilePath ((</>))
import Tools.ToolBuilder.Types
import Runix.FileSystem (getFileSystem)
import Tools.ToolBuilder.Prompt (loadToolBuilderPrompt)
import qualified Tools  -- Import base tools
import Runix.LLM.ToolInstances ()
import qualified Autodocodec
import qualified UniversalLLM.Tools

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
     , Member Cmds r
     , Member Fail r
     , Member (Grep RunixToolsFS) r
     , Member PromptStore r
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
  -- Get data directory from filesystem parameter
  RunixToolsFS dataDir <- getFileSystem @RunixToolsFS
  info $ "Tool builder using data directory: " <> T.pack dataDir

  -- Use relative paths for file operations (filesystem is chrooted)
  let cabalFilePath = "/runix-code.cabal"
      registryFilePath = "/generated-tools/GeneratedTools.hs"
      toolModulesDir = "/generated-tools/GeneratedTools"
      -- Use absolute path for cabal build working directory (external command)
      build = interpretCmd @"cabal" $ Tools.cabalBuild (Tools.WorkingDirectory $ T.pack dataDir)

  -- PRECONDITION: Verify current source tree compiles, abort otherwise
  -- The agent can't fix a broken codebase it didn't create
  info "Verifying codebase compiles before starting tool-builder..."
  info $ "Running cabal build in directory: " <> T.pack dataDir
  baselineCompile <- build
  info "Cabal build completed"
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
     , Member (FileSystem RunixToolsFS) r
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

  -- Step 4: Extract function name (with rollback on failure)
  functionName <- case extractFunctionName code of
    Just fname -> return fname
    Nothing -> do
      -- Rollback cabal file
      _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack cabalPath) (Tools.FileContent cabalText)
      info "Rolled back cabal file due to function extraction failure"
      fail $ T.unpack $ T.unlines
        [ "ERROR: Could not extract function name from generated code."
        , "The code must contain a top-level function with a type signature (e.g., 'myTool :: ...')."
        , ""
        , "Generated code was:"
        , code
        ]

  -- Step 5: Add to registry (if this fails, Fail effect will propagate, but we need to rollback cabal)
  -- We can't easily catch Fail, so we'll accept that addGeneratedTool failing leaves cabal modified
  -- The user can retry and it should work since addToolToRegistry handles duplicates gracefully
  AddToolResult _ <- addGeneratedTool (AddToolParams moduleName qualifiedModuleName functionName)
  info $ "Updated registry file with function: " <> functionName

  -- Step 6: Try to compile
  info "Attempting compilation..."
  buildResult <- build

  case buildResult of
    Tools.CabalBuildResult True _stdout _stderr -> do
      info $ "Tool successfully created and registered: " <> name
      return $ WriteToolcodeResult ("SUCCESS: Tool '" <> name <> "' has been created as module " <> qualifiedModuleName <> " and registered. You are DONE!")

    Tools.CabalBuildResult False _stdout stderr -> do
      -- Compilation failed - roll back all changes
      info $ "Compilation failed for tool: " <> name <> ", rolling back all changes"

      -- Restore registry file from backup
      _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent registryText)
      info "Restored registry file from backup"

      -- Restore cabal file from backup
      _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack cabalPath) (Tools.FileContent cabalText)
      info "Restored cabal file from backup"

      -- Remove the module file
      _ <- Tools.remove @RunixToolsFS (Tools.FilePath $ T.pack moduleFilePath) (Tools.Recursive False)
      info $ "Removed module file: " <> T.pack moduleFilePath

      fail $ "Compilation failed:\n" <> T.unpack stderr

-- Result type
newtype WriteToolcodeResult = WriteToolcodeResult Text
  deriving stock (Show, Eq)
  deriving (Autodocodec.HasCodec) via Text

instance UniversalLLM.Tools.ToolParameter WriteToolcodeResult where
  paramName _ _ = "result"
  paramDescription _ = "result of writing tool code"

instance UniversalLLM.Tools.ToolFunction WriteToolcodeResult where
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
-- Handles multi-line type signatures by joining continuation lines
-- Skips record field definitions and data type constructors
-- Returns Nothing if no function signature found
extractFunctionName :: Text -> Maybe Text
extractFunctionName code =
  let codeLines = T.lines code
      -- Reconstruct potential multi-line signatures by looking for "::" anywhere
      -- and extracting the identifier before it
      signatures = mapMaybe findSignature (joinContinuations codeLines)
  in case signatures of
       (name:_) -> Just name
       [] -> Nothing
  where
    -- Join continuation lines (lines that don't start at column 0)
    joinContinuations :: [Text] -> [Text]
    joinContinuations [] = []
    joinContinuations (l:ls) =
      let (cont, rest) = span isContinuation ls
      in (T.unwords (l:cont)) : joinContinuations rest

    isContinuation line =
      not (T.null line) && T.head line `elem` [' ', '\t']

    -- Find function name in a line containing "::"
    findSignature :: Text -> Maybe Text
    findSignature line
      | " :: " `T.isInfixOf` line =
          let trimmed = T.strip line
              beforeSig = T.takeWhile (/= ':') line
              candidate = T.strip $ last $ T.words beforeSig
          in if isValidFunctionName candidate && not (isRecordField trimmed)
             then Just candidate
             else Nothing
      | otherwise = Nothing

    -- Check if this looks like a valid function name
    isValidFunctionName name =
      not (T.null name) &&
      case T.uncons name of
        Just (c, _) -> Data.Char.isLower c || c == '('
        Nothing -> False

    -- Check if this is a record field (starts with { or ,)
    isRecordField line =
      let trimmed = T.strip line
      in T.isPrefixOf "{" trimmed || T.isPrefixOf "," trimmed || "}" `T.isInfixOf` line

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

--------------------------------------------------------------------------------
-- Registry Management Tools (user-facing, callable independently)
--------------------------------------------------------------------------------

-- | Enable a tool in the registry (uncomment it)
enableGeneratedTool
  :: forall r.
     ( Member Fail r
     , Members '[FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member Logging r
     )
  => EnableToolParams
  -> Sem r EnableToolResult
enableGeneratedTool (EnableToolParams moduleName functionName) = do
  -- Use path relative to filesystem root (which is chrooted)
  let registryPath = "/generated-tools/GeneratedTools.hs"

  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent
      updated = enableToolInRegistry moduleName functionName registryText
  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent updated)
  info $ "Enabled tool: " <> moduleName <> "." <> functionName
  return $ EnableToolResult ("Enabled tool: " <> moduleName <> "." <> functionName)

data EnableToolParams = EnableToolParams
  { enableModuleName :: Text
  , enableFunctionName :: Text
  } deriving (Show, Eq)

instance Autodocodec.HasCodec EnableToolParams where
  codec = Autodocodec.object "EnableToolParams" $
    EnableToolParams
      <$> Autodocodec.requiredField "module_name" "Module name (e.g., 'Echo')" Autodocodec..= enableModuleName
      <*> Autodocodec.requiredField "function_name" "Function name (e.g., 'echoTool')" Autodocodec..= enableFunctionName

instance UniversalLLM.Tools.ToolParameter EnableToolParams where
  paramName _ _ = "enable_tool_params"
  paramDescription _ = "parameters to enable a tool"

newtype EnableToolResult = EnableToolResult Text
  deriving stock (Show, Eq)
  deriving (Autodocodec.HasCodec) via Text

instance UniversalLLM.Tools.ToolParameter EnableToolResult where
  paramName _ _ = "result"
  paramDescription _ = "result of enabling tool"

instance UniversalLLM.Tools.ToolFunction EnableToolResult where
  toolFunctionName _ = "enable_generated_tool"
  toolFunctionDescription _ = "Enable a generated tool by uncommenting it in the registry"

-- | Disable a tool in the registry (comment it out)
disableGeneratedTool
  :: forall r.
     ( Member Fail r
     , Members '[FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member Logging r
     )
  => DisableToolParams
  -> Sem r DisableToolResult
disableGeneratedTool (DisableToolParams moduleName functionName) = do
  -- Use path relative to filesystem root (which is chrooted)
  let registryPath = "/generated-tools/GeneratedTools.hs"

  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent
      updated = disableToolInRegistry moduleName functionName registryText
  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent updated)
  info $ "Disabled tool: " <> moduleName <> "." <> functionName
  return $ DisableToolResult ("Disabled tool: " <> moduleName <> "." <> functionName)

data DisableToolParams = DisableToolParams
  { disableModuleName :: Text
  , disableFunctionName :: Text
  } deriving (Show, Eq)

instance Autodocodec.HasCodec DisableToolParams where
  codec = Autodocodec.object "DisableToolParams" $
    DisableToolParams
      <$> Autodocodec.requiredField "module_name" "Module name (e.g., 'Echo')" Autodocodec..= disableModuleName
      <*> Autodocodec.requiredField "function_name" "Function name (e.g., 'echoTool')" Autodocodec..= disableFunctionName

instance UniversalLLM.Tools.ToolParameter DisableToolParams where
  paramName _ _ = "disable_tool_params"
  paramDescription _ = "parameters to disable a tool"

newtype DisableToolResult = DisableToolResult Text
  deriving stock (Show, Eq)
  deriving (Autodocodec.HasCodec) via Text

instance UniversalLLM.Tools.ToolParameter DisableToolResult where
  paramName _ _ = "result"
  paramDescription _ = "result of disabling tool"

instance UniversalLLM.Tools.ToolFunction DisableToolResult where
  toolFunctionName _ = "disable_generated_tool"
  toolFunctionDescription _ = "Disable a generated tool by commenting it out in the registry"

-- | Remove a tool from the registry completely
removeGeneratedTool
  :: forall r.
     ( Member Fail r
     , Members '[FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member Logging r
     )
  => RemoveToolParams
  -> Sem r RemoveToolResult
removeGeneratedTool (RemoveToolParams moduleName functionName) = do
  -- Use path relative to filesystem root (which is chrooted)
  let registryPath = "/generated-tools/GeneratedTools.hs"

  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent
      updated = removeToolFromRegistry moduleName functionName registryText
  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent updated)
  info $ "Removed tool from registry: " <> moduleName <> "." <> functionName
  return $ RemoveToolResult ("Removed tool from registry: " <> moduleName <> "." <> functionName)

data RemoveToolParams = RemoveToolParams
  { removeModuleName :: Text
  , removeFunctionName :: Text
  } deriving (Show, Eq)

instance Autodocodec.HasCodec RemoveToolParams where
  codec = Autodocodec.object "RemoveToolParams" $
    RemoveToolParams
      <$> Autodocodec.requiredField "module_name" "Module name (e.g., 'Echo')" Autodocodec..= removeModuleName
      <*> Autodocodec.requiredField "function_name" "Function name (e.g., 'echoTool')" Autodocodec..= removeFunctionName

instance UniversalLLM.Tools.ToolParameter RemoveToolParams where
  paramName _ _ = "remove_tool_params"
  paramDescription _ = "parameters to remove a tool"

newtype RemoveToolResult = RemoveToolResult Text
  deriving stock (Show, Eq)
  deriving (Autodocodec.HasCodec) via Text

instance UniversalLLM.Tools.ToolParameter RemoveToolResult where
  paramName _ _ = "result"
  paramDescription _ = "result of removing tool"

instance UniversalLLM.Tools.ToolFunction RemoveToolResult where
  toolFunctionName _ = "remove_generated_tool"
  toolFunctionDescription _ = "Remove a generated tool from the registry (import, export, and list)"

-- | Add a new tool to the registry
addGeneratedTool
  :: forall r.
     ( Member Fail r
     , Members '[FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member Logging r
     )
  => AddToolParams
  -> Sem r AddToolResult
addGeneratedTool (AddToolParams moduleName qualifiedModuleName functionName) = do
  -- Use path relative to filesystem root (which is chrooted)
  let registryPath = "/generated-tools/GeneratedTools.hs"

  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent
      updated = addToolToRegistry moduleName qualifiedModuleName functionName registryText
  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent updated)
  info $ "Added tool to registry: " <> moduleName <> "." <> functionName
  return $ AddToolResult ("Added tool to registry: " <> moduleName <> "." <> functionName)

data AddToolParams = AddToolParams
  { addModuleName :: Text
  , addQualifiedModuleName :: Text
  , addFunctionName :: Text
  } deriving (Show, Eq)

instance Autodocodec.HasCodec AddToolParams where
  codec = Autodocodec.object "AddToolParams" $
    AddToolParams
      <$> Autodocodec.requiredField "module_name" "Module name (e.g., 'Echo')" Autodocodec..= addModuleName
      <*> Autodocodec.requiredField "qualified_module_name" "Qualified module name (e.g., 'GeneratedTools.Echo')" Autodocodec..= addQualifiedModuleName
      <*> Autodocodec.requiredField "function_name" "Function name (e.g., 'echoTool')" Autodocodec..= addFunctionName

instance UniversalLLM.Tools.ToolParameter AddToolParams where
  paramName _ _ = "add_tool_params"
  paramDescription _ = "parameters to add a tool to registry"

newtype AddToolResult = AddToolResult Text
  deriving stock (Show, Eq)
  deriving (Autodocodec.HasCodec) via Text

instance UniversalLLM.Tools.ToolParameter AddToolResult where
  paramName _ _ = "result"
  paramDescription _ = "result of adding tool"

instance UniversalLLM.Tools.ToolFunction AddToolResult where
  toolFunctionName _ = "add_generated_tool"
  toolFunctionDescription _ = "Add a new tool to the registry (import, export, and list entry)"

-- | List all generated tools in the registry
listGeneratedTools
  :: forall r.
     ( Member Fail r
     , Member (FileSystemRead RunixToolsFS) r
     , Member Logging r
     )
  => ListToolsParams
  -> Sem r ListToolsResult
listGeneratedTools _ = do
  -- Use path relative to filesystem root (which is chrooted)
  let registryPath = "/generated-tools/GeneratedTools.hs"

  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent
      toolsList = extractToolsList registryText
  info "Listed generated tools from registry"
  return $ ListToolsResult toolsList

-- No parameters needed for listing
data ListToolsParams = ListToolsParams
  deriving (Show, Eq)

instance Autodocodec.HasCodec ListToolsParams where
  codec = Autodocodec.object "ListToolsParams" $ pure ListToolsParams

instance UniversalLLM.Tools.ToolParameter ListToolsParams where
  paramName _ _ = "list_tools_params"
  paramDescription _ = "no parameters needed"

newtype ListToolsResult = ListToolsResult Text
  deriving stock (Show, Eq)
  deriving (Autodocodec.HasCodec) via Text

instance UniversalLLM.Tools.ToolParameter ListToolsResult where
  paramName _ _ = "tools"
  paramDescription _ = "list of generated tools with their status"

instance UniversalLLM.Tools.ToolFunction ListToolsResult where
  toolFunctionName _ = "list_generated_tools"
  toolFunctionDescription _ = "List all generated tools showing which are enabled/disabled"

--------------------------------------------------------------------------------
-- Helper Functions (Structured Registry Manipulation)
--------------------------------------------------------------------------------

-- | Tool definition with name and status
data ToolDef = ToolDef
  { toolDefModule :: Text
  , toolDefFunction :: Text
  , toolDefDisabled :: Bool
  } deriving (Show, Eq)

-- | Derive unique module names from tools list
deriveImports :: [ToolDef] -> [Text]
deriveImports tools =
  let modules = map toolDefModule tools
      unique [] = []
      unique (x:xs) = x : unique (filter (/= x) xs)
  in unique modules

-- | Derive exports from tools list
deriveExports :: [ToolDef] -> [Text]
deriveExports tools = map (\t -> toolDefModule t <> "." <> toolDefFunction t) tools

-- | Render imports section from list of module names
renderImports :: [Text] -> Text
renderImports modules =
  T.unlines $ map renderImport modules
  where
    renderImport modName =
      "import qualified GeneratedTools." <> modName <> " as " <> modName

-- | Render exports section from list of export names
renderExports :: [Text] -> Text
renderExports exports =
  T.unlines $ map renderExport exports
  where
    renderExport exportName = "  , " <> exportName

-- | Parse tools list section into structured data
parseToolsList :: Text -> [ToolDef]
parseToolsList registryContent =
  let contentLines = T.lines registryContent
      (_, afterStart) = break (T.isInfixOf "GENERATED_TOOLS_LIST_START") contentLines
      toolLines = case afterStart of
        (_:rest) -> takeWhile (not . T.isInfixOf "GENERATED_TOOLS_LIST_END") rest
        [] -> []
  in mapMaybe parseToolLine toolLines
  where
    parseToolLine line
      | "LLMTool" `T.isInfixOf` line =
          let stripped = T.strip line
              isDisabled = "--" `T.isPrefixOf` stripped
              -- Remove comment prefix if present
              cleaned = if isDisabled then T.strip (T.drop 2 stripped) else stripped
              -- Find "LLMTool" and extract everything after it
              afterLLMTool = case T.breakOn "LLMTool" cleaned of
                (_, rest) -> T.strip $ T.drop (T.length "LLMTool") rest
              -- Split Module.function (take the first dot as separator)
              (modName, funcName) = case T.breakOn "." afterLLMTool of
                (m, f) | not (T.null f) -> (T.strip m, T.strip $ T.drop 1 f)
                _ -> (afterLLMTool, afterLLMTool)
          in Just $ ToolDef modName funcName isDisabled
      | otherwise = Nothing

-- | Render tools list section from structured data
renderToolsList :: [ToolDef] -> Text
renderToolsList tools =
  let renderTool isFirst (ToolDef modName funcName disabled) =
        let comma = if isFirst then "    " else "  , "
            line = comma <> "LLMTool " <> modName <> "." <> funcName
        in if disabled then "-- " <> line else line
      rendered = case tools of
        [] -> []
        (t:ts) -> renderTool True t : map (renderTool False) ts
  in T.unlines rendered

-- | Replace entire registry from tools list (derives imports and exports)
setRegistryFromTools :: Text -> [ToolDef] -> Text
setRegistryFromTools registryContent tools =
  let imports = deriveImports tools
      exports = deriveExports tools
      step1 = setImportsSection registryContent imports
      step2 = setExportsSection step1 exports
      step3 = setToolsListSection step2 tools
  in step3
  where
    setImportsSection content importModules =
      let contentLines = T.lines content
          (beforeStart, afterStart) = break (T.isInfixOf "GENERATED_TOOL_IMPORTS_START") contentLines
          (_, afterEnd) = break (T.isInfixOf "GENERATED_TOOL_IMPORTS_END") afterStart
      in case (afterStart, afterEnd) of
        (startMarker:_, endMarker:rest) ->
          let renderedImports = T.lines $ renderImports importModules
          in T.unlines $ beforeStart ++ [startMarker] ++ renderedImports ++
               ["-- Tool module imports will be added here automatically"] ++
               ["-- Example: import qualified GeneratedTools.Echo as Echo"] ++
               [endMarker] ++ rest
        _ -> content

    setExportsSection content exportNames =
      let contentLines = T.lines content
          (beforeStart, afterStart) = break (T.isInfixOf "GENERATED_TOOL_EXPORTS_START") contentLines
          (_, afterEnd) = break (T.isInfixOf "GENERATED_TOOL_EXPORTS_END") afterStart
      in case (afterStart, afterEnd) of
        (startMarker:_, endMarker:rest) ->
          let renderedExports = T.lines $ renderExports exportNames
          in T.unlines $ beforeStart ++ [startMarker] ++ renderedExports ++
               ["    -- Tool exports will be added here automatically"] ++
               [endMarker] ++ rest
        _ -> content

    setToolsListSection content toolsList =
      let contentLines = T.lines content
          (beforeStart, afterStart) = break (T.isInfixOf "GENERATED_TOOLS_LIST_START") contentLines
          (_, afterEnd) = break (T.isInfixOf "GENERATED_TOOLS_LIST_END") afterStart
      in case (afterStart, afterEnd) of
        (startMarker:_, endMarker:rest) ->
          let renderedTools = T.lines $ renderToolsList toolsList
          in T.unlines $ beforeStart ++ [startMarker] ++ renderedTools ++
               ["    -- Tools will be added here automatically"] ++
               ["    -- Example: LLMTool Echo.echoTool"] ++
               [endMarker] ++ rest
        _ -> content

-- | Pure list operations on tool definitions

-- | Add tool to list
addToolToList :: Text -> Text -> [ToolDef] -> [ToolDef]
addToolToList moduleName functionName tools =
  tools ++ [ToolDef moduleName functionName False]

-- | Remove tool from list
removeToolFromList :: Text -> Text -> [ToolDef] -> [ToolDef]
removeToolFromList moduleName functionName tools =
  filter (\t -> not (toolDefModule t == moduleName && toolDefFunction t == functionName)) tools

-- | Enable tool in list
enableToolInList :: Text -> Text -> [ToolDef] -> [ToolDef]
enableToolInList moduleName functionName tools =
  map (\t -> if toolDefModule t == moduleName && toolDefFunction t == functionName
             then t { toolDefDisabled = False }
             else t) tools

-- | Disable tool in list
disableToolInList :: Text -> Text -> [ToolDef] -> [ToolDef]
disableToolInList moduleName functionName tools =
  map (\t -> if toolDefModule t == moduleName && toolDefFunction t == functionName
             then t { toolDefDisabled = True }
             else t) tools

-- | File operations: read and write only

-- | Add tool to registry (read → transform → write)
addToolToRegistry :: Text -> Text -> Text -> Text -> Text
addToolToRegistry moduleName _qualifiedModuleName functionName registryContent =
  let tools = parseToolsList registryContent
      updatedTools = addToolToList moduleName functionName tools
  in setRegistryFromTools registryContent updatedTools

-- | Remove tool from registry (read → transform → write)
removeToolFromRegistry :: Text -> Text -> Text -> Text
removeToolFromRegistry moduleName functionName registryContent =
  let tools = parseToolsList registryContent
      updatedTools = removeToolFromList moduleName functionName tools
  in setRegistryFromTools registryContent updatedTools

-- | Enable tool (read → transform → write)
enableToolInRegistry :: Text -> Text -> Text -> Text
enableToolInRegistry moduleName functionName registryContent =
  let tools = parseToolsList registryContent
      updatedTools = enableToolInList moduleName functionName tools
  in setRegistryFromTools registryContent updatedTools

-- | Disable tool (read → transform → write)
disableToolInRegistry :: Text -> Text -> Text -> Text
disableToolInRegistry moduleName functionName registryContent =
  let tools = parseToolsList registryContent
      updatedTools = disableToolInList moduleName functionName tools
  in setRegistryFromTools registryContent updatedTools

-- | Extract list of tools from registry with their enabled/disabled status
extractToolsList :: Text -> Text
extractToolsList registryText =
  let tools = parseToolsList registryText
      formatted = map formatTool tools
  in T.unlines $ ["Generated Tools:", ""] ++ formatted
  where
    formatTool (ToolDef modName funcName disabled) =
      let status = if disabled then "[DISABLED]" else "[ENABLED] "
          name = modName <> "." <> funcName
      in "  " <> status <> " " <> name

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
     , Member Cmds r
     , Member Fail r
     , Member (Grep RunixToolsFS) r
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
toolBuilderLoop systemPrompt cabalPath registryPath modulesDir build =
  toolBuilderLoopWithCount @model systemPrompt cabalPath registryPath modulesDir build 20

-- | Internal loop with iteration counter
toolBuilderLoopWithCount
  :: forall model r.
     ( Member (LLM model) r
     , Member Logging r
     , Member Cmds r
     , Member Fail r
     , Member (Grep RunixToolsFS) r
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
  -> Int       -- ^ Current iteration count
  -> Sem r BuildToolResult
toolBuilderLoopWithCount systemPrompt cabalPath registryPath modulesDir build iterationCount = do
  info $ "Tool builder iterations remaining: " <> T.pack (show iterationCount)

  -- CRITICAL: Explicit type signature with ScopedTypeVariables to bind 'r'
  let
      builderTools =
        [ LLMTool (Tools.readFile @RunixToolsFS)
        , LLMTool (Tools.glob @RunixToolsFS)
        , LLMTool (Tools.grep @RunixToolsFS)
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
  generatedToolFiles <- Tools.glob @RunixToolsFS (Tools.Pattern "/generated-tools/**/*.hs")
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
          toolBuilderLoopWithCount @model systemPrompt cabalPath registryPath modulesDir build (iterationCount - 1)

    calls -> do
      -- Execute tools and continue
      results <- mapM (executeTool builderTools) calls
      let historyWithResults = updatedHistory ++ map ToolResultMsg results
      put @[Message model] historyWithResults

      -- Check if we hit max retries (safety limit)
      if iterationCount <= 0
        then do
          -- Ask the LLM to explain why it failed before terminating
          info "Tool builder exceeded iteration limit, requesting failure summary from LLM..."
          let summaryRequest = SystemText $ T.unlines
                [ "ITERATION LIMIT REACHED (0 iterations remaining)"
                , ""
                , "You have exceeded the maximum number of iterations."
                , "Please provide a brief summary explaining:"
                , "1. What you were trying to accomplish"
                , "2. What obstacles or errors prevented completion"
                , "3. What would be needed to complete the task successfully"
                , ""
                , "Keep your explanation concise (2-3 paragraphs)."
                ]
              historyWithRequest = historyWithResults ++ [summaryRequest]

          -- Query LLM for explanation (without tools)
          explanationMsgs <- queryLLM [ULL.SystemPrompt systemPrompt] historyWithRequest

          let explanationText = case [txt | AssistantText txt <- explanationMsgs] of
                (txt:_) -> txt
                [] -> "No explanation provided"

          fail $ T.unpack $ T.unlines
            [ "Tool builder exceeded maximum iterations (20)"
            , ""
            , "=== LLM Failure Summary ==="
            , explanationText
            , "=== End Summary ==="
            ]

        else do
          -- Add warning if running low on iterations
          let historyWithWarning = if iterationCount < 3
                then historyWithResults ++ [SystemText $ T.unlines
                       [ "WARNING: You only have " <> T.pack (show iterationCount) <> " iteration(s) left!"
                       , "Time to get this done or prepare a failure report."
                       ]]
                else historyWithResults
          put @[Message model] historyWithWarning
          toolBuilderLoopWithCount @model systemPrompt cabalPath registryPath modulesDir build (iterationCount - 1)

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
