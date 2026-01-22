{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | MCP (Model Context Protocol) Client Effect
--
-- This module provides a Polysemy effect for connecting to MCP servers
-- and importing their tools into the runix-code tool system.
--
-- The effect is parameterized by server type to allow multiple MCP servers
-- on the effect stack simultaneously.
module Runix.MCPClient
  ( -- * Effect
    MCPClient(..)
  , listMCPTools
  , callMCPTool

  -- * Tool Generation
  , getMCPTools

  -- * Interpreters
  , interpretMCPStdio
  , interpretMCPHttp

  -- * Configuration
  , MCPConfig(..)

  -- * Types
  , MCPToolResult(..)
  , MCPToolArgs(..)
  , MCPToolCallResult(..)
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Polysemy.State (State, get, runState)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.:?), (.=), (.!=), Value, Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseMaybe)
import Control.Monad (forM)
import GHC.Generics (Generic)
import Control.Concurrent.STM (TMVar, newTMVarIO, atomically, takeTMVar, putTMVar)
import System.Process (createProcess, proc, std_in, std_out, std_err, StdStream(..), ProcessHandle)
import System.IO (Handle, hFlush)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import UniversalLLM.Tools (LLMTool(..), mkTool, ToolParameter(..), ToolWrapped, Callable(..))
import Runix.LLM.ToolInstances ()  -- Import orphan instances for Sem
import Autodocodec (HasCodec)
import qualified Autodocodec
import MCP.Server.Types (InputSchemaDefinition(..), Content(..))
import qualified MCP.Server.Types as MCP

--------------------------------------------------------------------------------
-- Tool Result Types (defined before effect for scoping)
--------------------------------------------------------------------------------

-- | Result type for MCP tool calls
newtype MCPToolResult = MCPToolResult { mcpResult :: Text }
  deriving (Show, Eq, Generic)

instance HasCodec MCPToolResult where
  codec = Autodocodec.dimapCodec MCPToolResult mcpResult Autodocodec.codec

instance ToolParameter MCPToolResult where
  paramName _ _ = "result"
  paramDescription _ = "result from MCP tool"

-- | Arguments type for MCP tools (JSON object)
newtype MCPToolArgs = MCPToolArgs { argsObject :: Value }
  deriving (Show, Eq, Generic)

instance HasCodec MCPToolArgs where
  codec = Autodocodec.dimapCodec MCPToolArgs argsObject Autodocodec.codec

instance ToolParameter MCPToolArgs where
  paramName _ _ = "args"
  paramDescription _ = "arguments as JSON object"

-- | MCP tools/call response
newtype MCPToolCallResult = MCPToolCallResult
  { content :: [Content]
  } deriving (Show, Generic)

instance FromJSON MCPToolCallResult where
  parseJSON = Aeson.withObject "MCPToolCallResult" $ \v -> MCPToolCallResult
    <$> v .: "content"

--------------------------------------------------------------------------------
-- Effect Definition
--------------------------------------------------------------------------------

-- | MCP Client effect parameterized by server identifier
data MCPClient (server :: Type) (m :: Type -> Type) a where
  -- | List all available tools from the MCP server
  ListMCPTools :: MCPClient server m (Either String [MCP.ToolDefinition])

  -- | Call an MCP tool by name with JSON arguments
  CallMCPTool :: Text -> Value -> MCPClient server m (Either String MCPToolCallResult)

makeSem ''MCPClient

-- | Configuration for MCP client
data MCPConfig = MCPConfig
  { mcpServerName :: Text  -- ^ Human-readable server name for logging
  , mcpNamespace :: Maybe Text  -- ^ Optional namespace prefix for tools (e.g., "lsp__")
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Tool Generation
--------------------------------------------------------------------------------

-- | Get LLMTools from an MCP server
-- This function creates tools that use the MCPClient effect to communicate
getMCPTools :: forall server r. (Member (MCPClient server) r, Member Fail r)
            => Maybe Text  -- ^ Optional namespace prefix
            -> Sem r [LLMTool (Sem (Fail : r))]
getMCPTools namespacePrefix = do
  toolDefsResult <- listMCPTools @server
  case toolDefsResult of
    Left err -> fail $ "Failed to list MCP tools: " <> err
    Right toolDefs -> return $ map (mcpToolDefToLLMTool @server namespacePrefix) toolDefs

-- | Convert MCP ToolDefinition to LLMTool
mcpToolDefToLLMTool :: forall server r. (Member (MCPClient server) r, Member Fail r)
                    => Maybe Text -> MCP.ToolDefinition -> LLMTool (Sem (Fail : r))
mcpToolDefToLLMTool namespacePrefix (MCP.ToolDefinition name desc _schema _extra) =
  let toolNameWithNS = case namespacePrefix of
        Nothing -> name
        Just prefix -> prefix <> name
      toolFn :: MCPToolArgs -> Sem (Fail : r) MCPToolResult
      toolFn (MCPToolArgs args) = do
        result <- raise $ callMCPTool @server name args
        case result of
          Left err -> fail $ "MCP tool call failed: " <> err
          Right callResult -> do
            -- Extract text from Content list
            let texts = [txt | ContentText txt <- content callResult]
            return $ MCPToolResult $ T.intercalate "\n" texts
  in LLMTool $ mkTool toolNameWithNS desc toolFn

--------------------------------------------------------------------------------
-- Protocol Types
--------------------------------------------------------------------------------

-- | MCP initialize request parameters
data MCPInitializeParams = MCPInitializeParams
  { protocolVersion :: Text
  , clientCapabilities :: Value
  , clientInfo :: MCPClientInfo
  } deriving (Show, Generic)

data MCPClientInfo = MCPClientInfo
  { name :: Text
  , version :: Text
  } deriving (Show, Generic)

instance ToJSON MCPInitializeParams where
  toJSON params = Aeson.object
    [ "protocolVersion" .= protocolVersion params
    , "capabilities" .= clientCapabilities params
    , "clientInfo" .= Aeson.object
        [ "name" .= name (clientInfo params)
        , "version" .= version (clientInfo params)
        ]
    ]

-- | MCP initialize response
data MCPInitializeResult = MCPInitializeResult
  { serverInfo :: MCPServerInfo
  , capabilities :: Value
  } deriving (Show, Generic)

data MCPServerInfo = MCPServerInfo
  { serverName :: Text
  , serverVersion :: Text
  } deriving (Show, Generic)

instance FromJSON MCPInitializeResult where
  parseJSON = Aeson.withObject "MCPInitializeResult" $ \v -> do
    serverInfoObj <- v .: "serverInfo"
    MCPInitializeResult
      <$> (MCPServerInfo
            <$> serverInfoObj .: "name"
            <*> serverInfoObj .: "version")
      <*> v .: "capabilities"

-- | MCP tools/list response
newtype MCPToolsListResult = MCPToolsListResult
  { tools :: [MCP.ToolDefinition]
  } deriving (Show, Generic)

instance FromJSON MCP.InputSchemaDefinitionProperty where
  parseJSON = Aeson.withObject "InputSchemaDefinitionProperty" $ \v ->
    MCP.InputSchemaDefinitionProperty
      <$> v .: "type"
      <*> v .: "description"

instance FromJSON InputSchemaDefinition where
  parseJSON = Aeson.withObject "InputSchemaDefinition" $ \v -> do
    props <- v .: "properties"
    reqs <- v .:? "required" .!= []
    -- Convert JSON object of properties to list of (Text, Property) pairs
    propsList <- case props of
      Aeson.Object obj -> do
        forM (KeyMap.toList obj) $ \(key, val) -> do
          prop <- parseJSON val
          return (AesonKey.toText key, prop)
      _ -> fail "properties must be an object"
    return $ InputSchemaDefinitionObject propsList reqs

instance FromJSON MCP.ToolDefinition where
  parseJSON = Aeson.withObject "ToolDefinition" $ \v -> MCP.ToolDefinition
    <$> v .: "name"
    <*> v .: "description"
    <*> v .: "inputSchema"
    <*> v .:? "extra"

instance FromJSON MCPToolsListResult where
  parseJSON = Aeson.withObject "MCPToolsListResult" $ \v -> MCPToolsListResult
    <$> v .: "tools"

-- | MCP tools/call request parameters
data MCPToolCallParams = MCPToolCallParams
  { toolName :: Text
  , arguments :: Value
  } deriving (Show, Generic)

instance ToJSON MCPToolCallParams where
  toJSON params = Aeson.object
    [ "name" .= toolName params
    , "arguments" .= arguments params
    ]

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------

-- | State held by the stdio interpreter
data MCPStdioState = MCPStdioState
  { stateHandleIn :: Handle
  , stateHandleOut :: Handle
  , stateReqIdVar :: TMVar Int
  , stateProcessHandle :: ProcessHandle
  }

-- | Interpret MCP client via stdio process
-- Maintains a persistent connection to the MCP server process
interpretMCPStdio
  :: forall server r a.
     ( Member (Embed IO) r
     , Member Fail r
     )
  => MCPConfig
  -> FilePath  -- ^ Executable path
  -> [String]  -- ^ Arguments
  -> Sem (MCPClient server : r) a
  -> Sem r a
interpretMCPStdio _config executable args program = do
  -- Initialize connection
  initResult <- embed $ do
    -- Create process with pipes for stdin/stdout
    (Just hIn, Just hOut, Just _hErr, ph) <- createProcess (proc executable args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

    -- Create TMVar for synchronizing JSON-RPC requests/responses
    reqIdVar <- newTMVarIO (0 :: Int)

    -- Initialize MCP connection
    initResult <- mcpInitialize hIn hOut reqIdVar
    case initResult of
      Left err -> return $ Left $ "MCP initialization failed: " <> T.unpack err
      Right _ -> return $ Right $ MCPStdioState hIn hOut reqIdVar ph

  case initResult of
    Left err -> fail err
    Right initialState -> do
      -- Run the program with state
      (_finalState, result) <- runState initialState $ reinterpret handleMCP program
      return result
  where
    handleMCP :: MCPClient server m x -> Sem (State MCPStdioState : r) x
    handleMCP = \case
      ListMCPTools -> do
        MCPStdioState{stateHandleIn, stateHandleOut, stateReqIdVar} <- get
        result <- embed $ mcpToolsList stateHandleIn stateHandleOut stateReqIdVar
        return $ case result of
          Left err -> Left $ "Failed to list MCP tools: " <> T.unpack err
          Right toolsList -> Right (tools toolsList)

      CallMCPTool toolName args -> do
        MCPStdioState{stateHandleIn, stateHandleOut, stateReqIdVar} <- get
        result <- embed $ mcpToolCallExec stateHandleIn stateHandleOut stateReqIdVar toolName args
        return $ case result of
          Left err -> Left $ "MCP tool call failed: " <> T.unpack err
          Right res -> Right res

-- | MCP tools/call request
mcpToolCallExec :: Handle -> Handle -> TMVar Int -> Text -> Value -> IO (Either Text MCPToolCallResult)
mcpToolCallExec hIn hOut reqIdVar toolName args = sendJSONRPC hIn hOut reqIdVar "tools/call" params
  where
    params = MCPToolCallParams
      { toolName = toolName
      , arguments = args
      }

-- | Interpret MCP client via HTTP
interpretMCPHttp
  :: forall server r a.
     Member (Embed IO) r
  => MCPConfig
  -> String  -- ^ Base URL
  -> Sem (MCPClient server : r) a
  -> Sem r a
interpretMCPHttp _config _baseUrl = interpret $ \case
  ListMCPTools -> return $ Left "HTTP MCP client not yet implemented"
  CallMCPTool _ _ -> return $ Left "HTTP MCP client not yet implemented"

--------------------------------------------------------------------------------
-- JSON-RPC Helpers (stdio transport)
--------------------------------------------------------------------------------

-- | Send JSON-RPC request and wait for response
sendJSONRPC :: (ToJSON params, FromJSON result)
            => Handle  -- stdin of process
            -> Handle  -- stdout of process
            -> TMVar Int  -- request ID generator
            -> Text  -- method name
            -> params  -- parameters
            -> IO (Either Text result)
sendJSONRPC hIn hOut reqIdVar method params = do
  -- Get next request ID
  reqId <- atomically $ do
    currentId <- takeTMVar reqIdVar
    putTMVar reqIdVar (currentId + 1)
    return currentId

  -- Build JSON-RPC request
  let request = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= reqId
        , "method" .= method
        , "params" .= toJSON params
        ]

  -- Send request
  BSL.hPutStr hIn (Aeson.encode request)
  BSL.hPutStr hIn "\n"
  hFlush hIn

  -- Read response (blocking), skipping any log lines
  let readJSONLine = do
        line <- BS.hGetLine hOut
        case Aeson.decode (BSL.fromStrict line) :: Maybe Value of
          Just val -> return $ Right val
          Nothing ->
            -- Not JSON, might be a log line - try next line
            if BS.isPrefixOf "info:" line || BS.isPrefixOf "warn:" line || BS.isPrefixOf "error:" line
            then readJSONLine  -- Skip log lines and try again
            else return $ Left $ "Failed to decode JSON-RPC response: " <> TE.decodeUtf8 line

  responseResult <- readJSONLine
  case responseResult of
    Left err -> return $ Left err
    Right responseObj -> case parseJSONRPCResponse responseObj of
      Left err -> return $ Left err
      Right result -> return $ Right result

-- | Parse JSON-RPC response
parseJSONRPCResponse :: FromJSON a => Value -> Either Text a
parseJSONRPCResponse val = case Aeson.fromJSON val of
  Aeson.Error msg -> Left $ "Failed to parse response: " <> T.pack msg
  Aeson.Success obj -> case parseMaybe (.: "error") obj of
    Just (err :: Value) -> Left $ "JSON-RPC error: " <> T.pack (show err)
    Nothing -> case parseMaybe (.: "result") obj of
      Nothing -> Left "Response has no result and no error"
      Just result -> case Aeson.fromJSON result of
        Aeson.Error msg -> Left $ "Failed to parse result: " <> T.pack msg
        Aeson.Success res -> Right res

-- | MCP initialize handshake
mcpInitialize :: Handle -> Handle -> TMVar Int -> IO (Either Text MCPInitializeResult)
mcpInitialize hIn hOut reqIdVar = sendJSONRPC hIn hOut reqIdVar "initialize" params
  where
    params = MCPInitializeParams
      { protocolVersion = "2025-06-18"
      , clientCapabilities = Aeson.object []  -- Empty capabilities for now
      , clientInfo = MCPClientInfo
          { name = "runix-code"
          , version = "0.1.0"
          }
      }

-- | MCP tools/list request
mcpToolsList :: Handle -> Handle -> TMVar Int -> IO (Either Text MCPToolsListResult)
mcpToolsList hIn hOut reqIdVar = sendJSONRPC hIn hOut reqIdVar "tools/list" emptyParams
  where
    emptyParams = Aeson.object []
