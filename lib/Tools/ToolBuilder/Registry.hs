{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure registry manipulation and file I/O for generated tools
--
-- This module provides a clean separation between:
-- - Pure transformations on [ToolDef]
-- - Single file I/O function that applies transformations
-- - Thin tool wrappers for agent use
module Tools.ToolBuilder.Registry
  ( -- * Core types
    ToolDef(..)
  , -- * File I/O (single point of access)
    modifyRegistry
  , readRegistry
  , -- * Pure transformations (composable)
    addTool
  , removeTool
  , enableTool
  , disableTool
  , -- * Parsing and rendering
    parseToolsList
  , renderToolsList
  , setRegistryFromTools
  , -- * Tool functions (for agents)
    enableGeneratedTool
  , disableGeneratedTool
  , listGeneratedTools
  , EnableToolParams(..)
  , DisableToolParams(..)
  , ListToolsParams(..)
  , EnableToolResult(..)
  , DisableToolResult(..)
  , ListToolsResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Polysemy (Member, Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystemRead, FileSystemWrite)
import Config (RunixToolsFS(..))
import Runix.Logging (Logging, info)
import qualified Tools
import qualified Autodocodec
import qualified UniversalLLM.Tools

--------------------------------------------------------------------------------
-- Core Types
--------------------------------------------------------------------------------

-- | Tool definition with name and status
data ToolDef = ToolDef
  { toolDefModule :: Text
  , toolDefFunction :: Text
  , toolDefDisabled :: Bool
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- File I/O (Single Point of Access)
--------------------------------------------------------------------------------

-- | Read registry and extract tools list
readRegistry
  :: forall r.
     ( Member Fail r
     , Member (FileSystemRead RunixToolsFS) r
     )
  => Sem r [ToolDef]
readRegistry = do
  let registryPath = "/generated-tools/GeneratedTools.hs"
  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent
  return $ parseToolsList registryText

-- | Modify registry by applying a transformation to the tools list
modifyRegistry
  :: forall r.
     ( Member Fail r
     , Members '[FileSystemRead RunixToolsFS, FileSystemWrite RunixToolsFS] r
     , Member Logging r
     )
  => ([ToolDef] -> [ToolDef])
  -> Sem r ()
modifyRegistry transform = do
  let registryPath = "/generated-tools/GeneratedTools.hs"

  -- Read current registry
  registryContent <- Tools.readFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath)
  let Tools.ReadFileResult registryText = registryContent

  -- Parse, transform, render
  let currentTools = parseToolsList registryText
      updatedTools = transform currentTools
      updatedRegistry = setRegistryFromTools registryText updatedTools

  -- Write back
  _ <- Tools.writeFile @RunixToolsFS (Tools.FilePath $ T.pack registryPath) (Tools.FileContent updatedRegistry)
  info "Registry updated"

--------------------------------------------------------------------------------
-- Pure Transformations (Composable)
--------------------------------------------------------------------------------

-- | Add tool to list (idempotent - replaces if already exists)
addTool :: Text -> Text -> [ToolDef] -> [ToolDef]
addTool moduleName functionName tools =
  let newTool = ToolDef moduleName functionName False
      -- Check if tool already exists
      exists = any (\t -> toolDefModule t == moduleName && toolDefFunction t == functionName) tools
  in if exists
     then tools  -- Already exists, don't add duplicate
     else tools ++ [newTool]

-- | Remove tool from list
removeTool :: Text -> Text -> [ToolDef] -> [ToolDef]
removeTool moduleName functionName tools =
  filter (\t -> not (toolDefModule t == moduleName && toolDefFunction t == functionName)) tools

-- | Enable tool in list
enableTool :: Text -> Text -> [ToolDef] -> [ToolDef]
enableTool moduleName functionName tools =
  map (\t -> if toolDefModule t == moduleName && toolDefFunction t == functionName
             then t { toolDefDisabled = False }
             else t) tools

-- | Disable tool in list
disableTool :: Text -> Text -> [ToolDef] -> [ToolDef]
disableTool moduleName functionName tools =
  map (\t -> if toolDefModule t == moduleName && toolDefFunction t == functionName
             then t { toolDefDisabled = True }
             else t) tools

--------------------------------------------------------------------------------
-- Parsing and Rendering
--------------------------------------------------------------------------------

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

-- | Format tools list for display
formatToolsList :: [ToolDef] -> Text
formatToolsList tools =
  let formatted = map formatTool tools
  in T.unlines $ ["Generated Tools:", ""] ++ formatted
  where
    formatTool (ToolDef modName funcName disabled) =
      let status = if disabled then "[DISABLED]" else "[ENABLED] "
          name = modName <> "." <> funcName
      in "  " <> status <> " " <> name

--------------------------------------------------------------------------------
-- Tool Functions (For Agents)
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
  modifyRegistry (enableTool moduleName functionName)
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
  modifyRegistry (disableTool moduleName functionName)
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
  tools <- readRegistry
  info "Listed generated tools from registry"
  return $ ListToolsResult (formatToolsList tools)

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
