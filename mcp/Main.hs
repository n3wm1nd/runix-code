{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeAbstractions #-}

module Main (main) where

import Prelude hiding (readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (catch, SomeException)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess)

-- MCP Server imports
import qualified MCP.Server as MCP
import MCP.Server.Types (McpServerInfo(..), McpServerHandlers(..), ServerCapabilities(..)
                        , ToolDefinition(..), InputSchemaDefinition(..), InputSchemaDefinitionProperty(..)
                        , Content(..), Error(..))

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

-- | Main entry point for runix-code-mcp
--
-- This is an MCP server that exposes runix-code tools via the Model Context Protocol
-- Uses STDIO transport for integration with Claude Desktop and other MCP clients
main :: IO ()
main = do
  let serverInfo = McpServerInfo
        { serverName = "runix-code-mcp"
        , serverVersion = "0.1.0"
        , serverInstructions = "Runix Code MCP Server - AI coding assistant tools"
        }

      -- Create handlers - for now just tools, no resources or prompts
      handlers = McpServerHandlers
        { prompts = Nothing
        , resources = Nothing
        , tools = Just (listTools, callTool)
        }

  -- Run the MCP server over STDIO with graceful shutdown on EOF/exceptions
  MCP.runMcpServerStdio serverInfo handlers `catch` handleShutdown
  where
    handleShutdown :: SomeException -> IO ()
    handleShutdown _e = do
      -- Exit cleanly - EOF from stdin is normal termination
      exitSuccess

--------------------------------------------------------------------------------
-- Tool Handlers
--------------------------------------------------------------------------------

-- | List all available tools
listTools :: IO [ToolDefinition]
listTools = do
  -- For now, return a simple echo tool as a test
  let echoTool = ToolDefinition
        "echo"
        "Echo back the input message"
        (InputSchemaDefinitionObject
          [("message", InputSchemaDefinitionProperty "string" "The message to echo")]
          ["message"])
        Nothing

  return [echoTool]

-- | Execute a tool call
--
-- Proper error handling: tool calls can fail for various reasons:
-- - Missing required parameters -> MissingRequiredParams
-- - Unknown tool name -> UnknownTool
-- - Tool execution failures -> InternalError
callTool :: Text -> [(Text, Text)] -> IO (Either Error Content)
callTool toolName args = do
  case toolName of
    "echo" -> do
      let message = lookup "message" args
      case message of
        Nothing -> return $ Left $ MissingRequiredParams "Missing required parameter 'message'"
        Just msg -> return $ Right $ ContentText $ "Echo: " <> msg
    _ -> return $ Left $ UnknownTool toolName
