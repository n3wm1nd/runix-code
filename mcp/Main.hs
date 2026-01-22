{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeAbstractions #-}

module Main (main) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (catch, SomeException)
import System.Exit (exitSuccess)
import qualified System.Directory as Dir
import Data.IORef (IORef, newIORef, readIORef)

-- MCP Server imports
import qualified MCP.Server as MCP
import MCP.Server.Types (McpServerInfo(..), McpServerHandlers(..)
                        , InputSchemaDefinition(..)
                        , Content(..), Error(..))
import qualified MCP.Server.Types as MCPTypes

-- Runix Code imports
import Config (RunixDataDir(..), ProjectFS(..))

-- Tools and LLM
import UniversalLLM (ToolDefinition(..), ToolCall(..), ToolResult(..))
import UniversalLLM.Tools (LLMTool(..), llmToolToDefinition, executeToolCallFromList)
import qualified Tools

-- Polysemy and effects
import Polysemy (Sem, runM, Member)
import Polysemy.Error (runError)
import Polysemy.Fail (Fail)
import Runix.Runner (loggingIO, failLog)
import Runix.FileSystem (FileSystem, fileSystemLocal)
import qualified Runix.FileSystem.System
import UI.UserInput (ImplementsWidget(..), RenderRequest)
import qualified Paths_runix_code
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------
-- MCP Widget Type (for UserInput effect)
--------------------------------------------------------------------------------

-- | Phantom type for MCP widget system (non-interactive, like CLI)
data MCPWidget

-- | RenderRequest for MCP (never actually used since we always fail)
data instance RenderRequest MCPWidget a = MCPRenderRequest Text a

-- | ImplementsWidget instance for Text
instance ImplementsWidget MCPWidget Text where
  askWidget prompt defaultValue = MCPRenderRequest prompt defaultValue

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code-mcp
--
-- This is an MCP server that exposes runix-code tools via the Model Context Protocol
-- Uses STDIO transport for integration with Claude Desktop and other MCP clients
main :: IO ()
main = do
  -- Get current working directory for filesystem chroot
  cwd <- Dir.getCurrentDirectory

  -- Get runix data directory
  runixDataDir <- RunixDataDir <$> Paths_runix_code.getDataDir

  -- Build the tool list and interpreter
  (toolsRef, runner) <- buildToolsWithInterpreter runixDataDir cwd

  let serverInfo = McpServerInfo
        { serverName = "runix-code-mcp"
        , serverVersion = "0.1.0"
        , serverInstructions = "Runix Code MCP Server - AI coding assistant tools"
        }

      -- Create handlers that use the tools
      handlers = McpServerHandlers
        { prompts = Nothing
        , resources = Nothing
        , tools = Just (listTools toolsRef, callTool toolsRef runner)
        }

  -- Run the MCP server over STDIO with graceful shutdown on EOF/exceptions
  MCP.runMcpServerStdio serverInfo handlers `catch` handleShutdown
  where
    handleShutdown :: SomeException -> IO ()
    handleShutdown _e = exitSuccess

--------------------------------------------------------------------------------
-- Tool Configuration
--------------------------------------------------------------------------------

-- | Available tools exposed via MCP
-- Add or remove tools here to customize what's available to MCP clients
availableTools :: (Member Fail r, Member (FileSystem ProjectFS) r) => [LLMTool (Sem r)]
availableTools =
  [ LLMTool (Tools.getCwd @ProjectFS)
  -- Add more tools here:
  -- , LLMTool (Tools.readFile @ProjectFS)
  -- , LLMTool (Tools.glob @ProjectFS)
  -- , LLMTool (Tools.grep @ProjectFS)
  ]

--------------------------------------------------------------------------------
-- Tool Building and Conversion
--------------------------------------------------------------------------------

-- | Newtype wrapper for the interpreter runner to avoid ImpredicativeTypes
newtype InterpreterRunner r = InterpreterRunner
  { runInterpreter :: forall a. Sem r a -> IO a
  }

-- | Build the list of LLMTools and the interpreter runner
-- Returns both the tools (in Sem) and a function to run them in IO
buildToolsWithInterpreter runixDataDir cwd = do
  -- Build the interpreter runner (just like in tui/Main.hs)
  let runner = InterpreterRunner $ \sem -> do
        result <- runM
                . runError
                . loggingIO
                . failLog
                . Runix.FileSystem.System.filesystemIO
                . fileSystemLocal (ProjectFS cwd)
                $ sem
        case result of
          Left err -> error $ "Effect failed: " <> err
          Right val -> return val

  toolsRef <- newIORef availableTools
  return (toolsRef, runner)

-- | List all available tools
listTools :: IORef [LLMTool (Sem r)] -> IO [MCPTypes.ToolDefinition]
listTools toolsRef = do
  tools <- readIORef toolsRef
  -- Convert each LLMTool to MCP ToolDefinition
  return $ map convertLLMToolToMCP tools

-- | Convert an LLMTool to an MCP ToolDefinition
convertLLMToolToMCP :: LLMTool (Sem r) -> MCPTypes.ToolDefinition
convertLLMToolToMCP llmTool =
  let def = llmToolToDefinition llmTool
  in MCPTypes.ToolDefinition
       (toolDefName def)
       (toolDefDescription def)
       (convertSchemaToMCP (toolDefParameters def))
       Nothing

-- | Convert JSON schema Value to MCP InputSchemaDefinition
-- This is simplified - in reality we'd parse the JSON schema properly
convertSchemaToMCP :: Value -> InputSchemaDefinition
convertSchemaToMCP _schema =
  -- TODO: Parse the actual JSON schema and convert properties
  InputSchemaDefinitionObject [] []

-- | Execute a tool call
callTool :: IORef [LLMTool (Sem r)]
         -> InterpreterRunner r
         -> Text
         -> [(Text, Text)]
         -> IO (Either Error Content)
callTool toolsRef (InterpreterRunner runToIO) toolName args = do
  tools <- readIORef toolsRef

  -- Convert MCP args to JSON object for executeToolCall
  let argsObject = Aeson.object [ (Data.Aeson.Key.fromText k, Aeson.String v) | (k, v) <- args ]
      toolCall = ToolCall "mcp-call" toolName argsObject

  -- Execute the tool using UniversalLLM's executeToolCallFromList
  toolResult <- runToIO $ executeToolCallFromList tools toolCall

  -- Convert ToolResult to MCP Content
  case toolResult of
    ToolResult _ (Right value) -> do
      -- Success - convert JSON value to text
      -- If it's a JSON string, unwrap it; otherwise encode as JSON
      let textContent = case value of
            Aeson.String txt -> txt
            _ -> T.decodeUtf8 $ BSL.toStrict $ Aeson.encode value
      return $ Right $ ContentText textContent
    ToolResult _ (Left err) ->
      -- Error from tool execution
      return $ Left $ InternalError err

-- | Find a tool by name in the tool list
findToolByName :: [LLMTool (Sem r)] -> Text -> Maybe (LLMTool (Sem r))
findToolByName tools name =
  let matches = filter (\t ->
        let def = llmToolToDefinition t
        in toolDefName def == name) tools
  in case matches of
       (t:_) -> Just t
       [] -> Nothing
