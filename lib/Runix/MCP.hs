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
-- Generic effect for connecting to MCP servers, parameterized by server type.
-- Works with raw 'Value' in/out. For dynamic tool bridging, see "Runix.MCP.DynamicTools".
-- For typed wrappers over specific MCP servers, see e.g. "Runix.MCP.Playwright".
module Runix.MCP
  ( -- * Effect
    MCP(..)
  , listTools
  , callTool

  -- * Interpreters
  , interpretStdio
  , interpretHttp

  -- * Configuration
  , Config(..)

  -- * Types
  , HttpEndpoint(..)
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Polysemy.State (State, get, runState)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON(..), FromJSON(..), (.:), (.:?), (.=), (.!=), Value)
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

import MCP.Server.Types (InputSchemaDefinition(..), Content(..))
import qualified MCP.Server.Types as MCP
import Runix.RestAPI (RestEndpoint(..))
import Runix.HTTP (HTTP)
import qualified Runix.HTTP
import Runix.SSE (SSEEvent(..), parseSSEComplete)

--------------------------------------------------------------------------------
-- Internal types
--------------------------------------------------------------------------------

-- | MCP tools/call response
newtype ToolCallResult = ToolCallResult
  { content :: [Content]
  } deriving (Show, Generic)

instance FromJSON ToolCallResult where
  parseJSON = Aeson.withObject "ToolCallResult" $ \v -> ToolCallResult
    <$> v .: "content"

-- | JSON-RPC response wrapper
data JSONRPCResponse result = JSONRPCResponse
  { rpcId :: Int
  , rpcResult :: result
  } deriving (Show, Generic)

instance FromJSON result => FromJSON (JSONRPCResponse result) where
  parseJSON = Aeson.withObject "JSONRPCResponse" $ \v -> do
    mError <- v .:? "error"
    case mError of
      Just (err :: Value) -> fail $ "JSON-RPC error: " <> show err
      Nothing -> JSONRPCResponse
        <$> v .: "id"
        <*> v .: "result"

--------------------------------------------------------------------------------
-- Effect Definition
--------------------------------------------------------------------------------

-- | MCP effect parameterized by server identifier.
-- Provides raw JSON-level access to any MCP server.
data MCP (server :: Type) (m :: Type -> Type) a where
  -- | List all available tools from the MCP server
  ListTools :: MCP server m (Either Text [MCP.ToolDefinition])

  -- | Call an MCP tool by name with JSON arguments
  CallTool :: Text -> Value -> MCP server m (Either Text Value)

-- | List all available tools from the MCP server.
-- Converts 'Either' errors to 'Fail'.
listTools :: forall server r. (Member (MCP server) r, Member Fail r)
          => Sem r [MCP.ToolDefinition]
listTools = send @(MCP server) ListTools >>= either (fail . T.unpack) return

-- | Call an MCP tool by name with JSON arguments.
-- Converts 'Either' errors to 'Fail'.
callTool :: forall server r. (Member (MCP server) r, Member Fail r)
         => Text -> Value -> Sem r Value
callTool name args = send @(MCP server) (CallTool name args) >>= either (fail . T.unpack) return

-- | Configuration for MCP client
data Config = Config
  { mcpServerName :: Text  -- ^ Human-readable server name for logging
  , mcpNamespace :: Maybe Text  -- ^ Optional namespace prefix for tools (e.g., "lsp__")
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Protocol Types
--------------------------------------------------------------------------------

-- | MCP initialize request parameters
data InitializeParams = InitializeParams
  { protocolVersion :: Text
  , clientCapabilities :: Value
  , clientInfo :: ClientInfo
  } deriving (Show, Generic)

data ClientInfo = ClientInfo
  { name :: Text
  , version :: Text
  } deriving (Show, Generic)

instance ToJSON InitializeParams where
  toJSON params = Aeson.object
    [ "protocolVersion" .= protocolVersion params
    , "capabilities" .= clientCapabilities params
    , "clientInfo" .= Aeson.object
        [ "name" .= name (clientInfo params)
        , "version" .= version (clientInfo params)
        ]
    ]

-- | MCP initialize response
data InitializeResult = InitializeResult
  { serverInfo :: ServerInfo
  , capabilities :: Value
  } deriving (Show, Generic)

data ServerInfo = ServerInfo
  { serverName :: Text
  , serverVersion :: Text
  } deriving (Show, Generic)

instance FromJSON InitializeResult where
  parseJSON = Aeson.withObject "InitializeResult" $ \v -> do
    serverInfoObj <- v .: "serverInfo"
    InitializeResult
      <$> (ServerInfo
            <$> serverInfoObj .: "name"
            <*> serverInfoObj .: "version")
      <*> v .: "capabilities"

-- | MCP tools/list response
newtype ToolsListResult = ToolsListResult
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

instance FromJSON ToolsListResult where
  parseJSON = Aeson.withObject "ToolsListResult" $ \v -> ToolsListResult
    <$> v .: "tools"

-- | MCP tools/call request parameters
data ToolCallParams = ToolCallParams
  { toolName :: Text
  , arguments :: Value
  } deriving (Show, Generic)

instance ToJSON ToolCallParams where
  toJSON params = Aeson.object
    [ "name" .= toolName params
    , "arguments" .= arguments params
    ]

--------------------------------------------------------------------------------
-- Interpreters
--------------------------------------------------------------------------------

-- | Extract text content from ToolCallResult and parse as JSON
extractContentAsJSON :: ToolCallResult -> Either Text Value
extractContentAsJSON callResult =
  let texts = [txt | ContentText txt <- content callResult]
      combinedText = T.concat texts
  in if T.null combinedText
     then Left "MCP tool returned empty content"
     else case Aeson.decode (BSL.fromStrict $ TE.encodeUtf8 combinedText) of
            Nothing ->
              Right $ Aeson.String combinedText
            Just val -> Right val

-- | State held by the stdio interpreter
data StdioState = StdioState
  { stateHandleIn :: Handle
  , stateHandleOut :: Handle
  , stateReqIdVar :: TMVar Int
  , stateProcessHandle :: ProcessHandle
  }

-- | State held by the HTTP interpreter
data HttpState = HttpState
  { httpBaseUrl :: String
  , httpReqIdVar :: TMVar Int
  , httpSessionId :: Maybe Text
  }

-- | RestEndpoint instance for MCP HTTP transport
newtype HttpEndpoint = HttpEndpoint String

instance RestEndpoint HttpEndpoint where
  apiroot (HttpEndpoint base) = base
  authheaders _ =
    [ ("Accept", "application/json, text/event-stream")
    ]

-- | Interpret MCP effect via stdio process.
-- Spawns the MCP server process and maintains a persistent connection.
interpretStdio
  :: forall server r a.
     ( Member (Embed IO) r
     , Member Fail r
     )
  => Config
  -> FilePath  -- ^ Executable path
  -> [String]  -- ^ Arguments
  -> Sem (MCP server : r) a
  -> Sem r a
interpretStdio _config executable procArgs program = do
  initResult <- embed $ do
    (Just hIn, Just hOut, Just _hErr, ph) <- createProcess (proc executable procArgs)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }

    reqIdVar <- newTMVarIO (0 :: Int)

    initRes <- mcpInitialize hIn hOut reqIdVar
    case initRes of
      Left err -> return $ Left $ "MCP initialization failed: " <> err
      Right _ -> return $ Right $ StdioState hIn hOut reqIdVar ph

  case initResult of
    Left err -> fail $ T.unpack err
    Right initialState -> do
      (_finalState, result) <- runState initialState $ reinterpret handleMCP program
      return result
  where
    handleMCP :: MCP server m x -> Sem (State StdioState : r) x
    handleMCP = \case
      ListTools -> do
        StdioState{stateHandleIn, stateHandleOut, stateReqIdVar} <- get
        result <- embed $ mcpToolsList stateHandleIn stateHandleOut stateReqIdVar
        return $ case result of
          Left err -> Left $ "Failed to list MCP tools: " <> err
          Right toolsList -> Right (tools toolsList)

      CallTool tName args -> do
        StdioState{stateHandleIn, stateHandleOut, stateReqIdVar} <- get
        result <- embed $ mcpToolCallExec stateHandleIn stateHandleOut stateReqIdVar tName args
        return $ case result of
          Left err -> Left $ "MCP tool call failed: " <> err
          Right callResult -> extractContentAsJSON callResult

-- | MCP tools/call request
mcpToolCallExec :: Handle -> Handle -> TMVar Int -> Text -> Value -> IO (Either Text ToolCallResult)
mcpToolCallExec hIn hOut reqIdVar tName args = sendJSONRPC hIn hOut reqIdVar "tools/call" params
  where
    params = ToolCallParams
      { toolName = tName
      , arguments = args
      }

-- | Interpret MCP effect via HTTP.
-- Connects to an already-running MCP server over HTTP.
interpretHttp
  :: forall server r a.
     ( Member (Embed IO) r
     , Member Fail r
     , Member HTTP r
     )
  => Config
  -> String  -- ^ Base URL (e.g., "http://localhost:5056")
  -> Sem (MCP server : r) a
  -> Sem r a
interpretHttp _config baseUrl' program = do
  reqIdVar <- embed $ newTMVarIO (0 :: Int)
  let initialState = HttpState baseUrl' reqIdVar Nothing

  initResult <- mcpInitializeHttp baseUrl' reqIdVar
  case initResult of
    Left err -> fail $ T.unpack $ "MCP HTTP initialization failed: " <> err
    Right (_initResponse, mSessionId) -> do
      let stateWithSession = initialState { httpSessionId = mSessionId }
      (_, result) <- runState stateWithSession $ reinterpret handleMCP program
      return result
  where
    handleMCP :: MCP server m x -> Sem (State HttpState : r) x
    handleMCP = \case
      ListTools -> do
        HttpState{httpBaseUrl = url, httpSessionId = sessionId, httpReqIdVar = reqIdVar} <- get @HttpState
        result <- raise $ mcpToolsListHttp url sessionId reqIdVar
        return $ case result of
          Left err -> Left $ "Failed to list MCP tools: " <> err
          Right toolsList -> Right (tools toolsList)

      CallTool tName toolArgs -> do
        HttpState{httpBaseUrl = url, httpSessionId = sessionId, httpReqIdVar = reqIdVar} <- get @HttpState
        result <- raise $ mcpToolCallHttp url sessionId reqIdVar tName toolArgs
        return $ case result of
          Left err -> Left $ "MCP tool call failed: " <> err
          Right callResult -> extractContentAsJSON callResult

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
  reqId <- atomically $ do
    currentId <- takeTMVar reqIdVar
    putTMVar reqIdVar (currentId + 1)
    return currentId

  let request = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= reqId
        , "method" .= method
        , "params" .= toJSON params
        ]

  BSL.hPutStr hIn (Aeson.encode request)
  BSL.hPutStr hIn "\n"
  hFlush hIn

  let readJSONLine = do
        line <- BS.hGetLine hOut
        case Aeson.decode (BSL.fromStrict line) :: Maybe Value of
          Just val -> return $ Right val
          Nothing ->
            if BS.isPrefixOf "info:" line || BS.isPrefixOf "warn:" line || BS.isPrefixOf "error:" line
            then readJSONLine
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
mcpInitialize :: Handle -> Handle -> TMVar Int -> IO (Either Text InitializeResult)
mcpInitialize hIn hOut reqIdVar = sendJSONRPC hIn hOut reqIdVar "initialize" params
  where
    params = InitializeParams
      { protocolVersion = "2025-06-18"
      , clientCapabilities = Aeson.object []
      , clientInfo = ClientInfo
          { name = "runix-code"
          , version = "0.1.0"
          }
      }

-- | MCP tools/list request
mcpToolsList :: Handle -> Handle -> TMVar Int -> IO (Either Text ToolsListResult)
mcpToolsList hIn hOut reqIdVar = sendJSONRPC hIn hOut reqIdVar "tools/list" emptyParams
  where
    emptyParams = Aeson.object []

--------------------------------------------------------------------------------
-- JSON-RPC Helpers (HTTP transport)
--------------------------------------------------------------------------------

-- | Parse SSE response to extract JSON data
parseSSEResponse :: BSL.ByteString -> Either Text Value
parseSSEResponse body =
  let strictBody = BSL.toStrict body
      sseEvents = parseSSEComplete strictBody
  in case sseEvents of
       (event:_) ->
         let jsonText = eventData event
         in if T.null jsonText
            then Left "SSE event has no data"
            else case Aeson.decode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
                   Nothing -> Left $ "Failed to parse JSON from SSE data: " <> jsonText
                   Just val -> Right val
       [] -> Left "No SSE events in response"

-- | Send JSON-RPC request via HTTP POST, handling SSE responses
sendJSONRPCHttp :: forall params result r.
                   (ToJSON params, FromJSON result, Members [HTTP, Fail, Embed IO] r)
                => String
                -> Maybe Text
                -> TMVar Int
                -> Text
                -> params
                -> Sem r (Either Text result)
sendJSONRPCHttp baseUrl mSessionId reqIdVar method params = do
  reqId <- embed $ atomically $ do
    currentId <- takeTMVar reqIdVar
    putTMVar reqIdVar (currentId + 1)
    return currentId

  let requestBody = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= reqId
        , "method" .= method
        , "params" .= toJSON params
        ]

  let sessionHeaders = case mSessionId of
        Nothing -> []
        Just sessionId -> [("Mcp-Session-Id", T.unpack sessionId)]
      httpReq = Runix.HTTP.HTTPRequest
        { Runix.HTTP.method = "POST"
        , Runix.HTTP.uri = baseUrl ++ "/mcp"
        , Runix.HTTP.headers =
            [ ("Content-Type", "application/json")
            , ("Accept", "application/json, text/event-stream")
            ] ++ sessionHeaders
        , Runix.HTTP.body = Just $ Aeson.encode requestBody
        }

  Runix.HTTP.HTTPResponse{Runix.HTTP.body = responseBody} <- Runix.HTTP.httpRequest httpReq

  case parseSSEResponse responseBody of
    Left err -> return $ Left err
    Right responseJson -> return $ parseJSONRPCResponse responseJson

-- | MCP initialize handshake via HTTP, returns (result, sessionId)
mcpInitializeHttp :: Members [HTTP, Fail, Embed IO] r
                  => String
                  -> TMVar Int
                  -> Sem r (Either Text (InitializeResult, Maybe Text))
mcpInitializeHttp baseUrl reqIdVar = do
  reqId <- embed $ atomically $ do
    currentId <- takeTMVar reqIdVar
    putTMVar reqIdVar (currentId + 1)
    return currentId

  let requestBody = Aeson.object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id" .= reqId
        , "method" .= ("initialize" :: Text)
        , "params" .= toJSON params
        ]

  let httpReq = Runix.HTTP.HTTPRequest
        { Runix.HTTP.method = "POST"
        , Runix.HTTP.uri = baseUrl ++ "/mcp"
        , Runix.HTTP.headers =
            [ ("Content-Type", "application/json")
            , ("Accept", "application/json, text/event-stream")
            ]
        , Runix.HTTP.body = Just $ Aeson.encode requestBody
        }

  Runix.HTTP.HTTPResponse{Runix.HTTP.body = responseBody, Runix.HTTP.headers = responseHeaders} <- Runix.HTTP.httpRequest httpReq

  let sessionId = lookupHeader "mcp-session-id" responseHeaders
      lookupHeader hName headers =
        let stripQuotes s = T.strip $ T.dropAround (== '"') $ T.pack s
            headerMap = [(T.toLower $ stripQuotes k, stripQuotes v) | (k, v) <- headers]
        in lookup hName headerMap

  case parseSSEResponse responseBody of
    Left err -> return $ Left err
    Right responseJson -> case parseJSONRPCResponse responseJson of
      Left err -> return $ Left err
      Right result -> return $ Right (result, sessionId)
  where
    params = InitializeParams
      { protocolVersion = "2025-06-18"
      , clientCapabilities = Aeson.object []
      , clientInfo = ClientInfo
          { name = "runix-code"
          , version = "0.1.0"
          }
      }

-- | MCP tools/list via HTTP
mcpToolsListHttp :: Members [HTTP, Fail, Embed IO] r
                 => String
                 -> Maybe Text
                 -> TMVar Int
                 -> Sem r (Either Text ToolsListResult)
mcpToolsListHttp baseUrl sessionId reqIdVar = sendJSONRPCHttp baseUrl sessionId reqIdVar "tools/list" emptyParams
  where
    emptyParams = Aeson.object []

-- | MCP tools/call via HTTP
mcpToolCallHttp :: Members [HTTP, Fail, Embed IO] r
                => String
                -> Maybe Text
                -> TMVar Int
                -> Text
                -> Value
                -> Sem r (Either Text ToolCallResult)
mcpToolCallHttp baseUrl sessionId reqIdVar tName args = sendJSONRPCHttp baseUrl sessionId reqIdVar "tools/call" params
  where
    params = ToolCallParams
      { toolName = tName
      , arguments = args
      }
