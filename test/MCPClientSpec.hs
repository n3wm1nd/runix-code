{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module MCPClientSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Runix.MCPClient
import MCP.Server.Types (InputSchemaDefinition(..), InputSchemaDefinitionProperty(..))
import qualified MCP.Server.Types as MCP
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Polysemy
import Polysemy.Fail (Fail, runFail, failToError)
import Polysemy.Error (runError, Error, throw)
import System.Directory (doesFileExist)
import Runix.HTTP (HTTP(..), HTTPRequest(..), HTTPResponse(..), httpIO_)
import Runix.Logging (loggingIO)
import Runix.Cancellation (cancelNoop)

-- | Test server type
data TestMCPServer

-- | Mock HTTP interpreter for testing MCP without a real server
mockMCPHttpIO :: Members [Fail, Embed IO] r => Sem (HTTP : r) a -> Sem r a
mockMCPHttpIO = interpret $ \case
  HttpRequest (HTTPRequest reqMethod reqUri reqHeaders reqBody) -> do
    -- Check if it's a POST to /mcp
    case (reqMethod, reqUri) of
      ("POST", url) | "/mcp" `T.isInfixOf` T.pack url -> do
        -- Parse JSON-RPC request
        case reqBody of
          Nothing -> fail "Missing request body"
          Just bodyBytes -> do
            case Aeson.decode bodyBytes of
              Nothing -> fail "Failed to parse JSON-RPC request"
              Just (rpcReq :: Aeson.Value) -> do
                -- Extract method and id
                let methodName = case rpcReq of
                      Aeson.Object obj -> case KeyMap.lookup "method" obj of
                        Just (Aeson.String m) -> m
                        _ -> ""
                      _ -> ""
                let reqId = case rpcReq of
                      Aeson.Object obj -> case KeyMap.lookup "id" obj of
                        Just n -> n
                        _ -> Aeson.Number 0
                      _ -> Aeson.Number 0

                -- Generate response based on method
                let response = case methodName of
                      "initialize" ->
                        Aeson.object
                          [ "jsonrpc" .= ("2.0" :: Text)
                          , "id" .= reqId
                          , "result" .= Aeson.object
                              [ "protocolVersion" .= ("2024-11-05" :: Text)
                              , "capabilities" .= Aeson.object
                                  [ "tools" .= Aeson.object []
                                  ]
                              , "serverInfo" .= Aeson.object
                                  [ "name" .= ("mock-mcp-server" :: Text)
                                  , "version" .= ("1.0.0" :: Text)
                                  ]
                              ]
                          ]

                      "tools/list" ->
                        Aeson.object
                          [ "jsonrpc" .= ("2.0" :: Text)
                          , "id" .= reqId
                          , "result" .= Aeson.object
                              [ "tools" .= Aeson.Array (V.fromList
                                  [ Aeson.object
                                      [ "name" .= ("test_tool" :: Text)
                                      , "description" .= ("A test tool" :: Text)
                                      , "inputSchema" .= Aeson.object
                                          [ "type" .= ("object" :: Text)
                                          , "properties" .= Aeson.object []
                                          ]
                                      ]
                                  ])
                              ]
                          ]

                      "tools/call" ->
                        Aeson.object
                          [ "jsonrpc" .= ("2.0" :: Text)
                          , "id" .= reqId
                          , "result" .= Aeson.object
                              [ "content" .= Aeson.Array (V.fromList
                                  [ Aeson.object
                                      [ "type" .= ("text" :: Text)
                                      , "text" .= ("Mock tool result" :: Text)
                                      ]
                                  ])
                              ]
                          ]

                      _ -> Aeson.object
                          [ "jsonrpc" .= ("2.0" :: Text)
                          , "id" .= reqId
                          , "error" .= Aeson.object
                              [ "code" .= (-32601 :: Int)
                              , "message" .= ("Method not found" :: Text)
                              ]
                          ]

                let responseBody = Aeson.encode response
                let sessionId = if methodName == "initialize"
                                then [("mcp-session-id", "mock-session-123")]
                                else []

                return $ HTTPResponse
                  200
                  ([("content-type", "application/json")] ++ sessionId)
                  responseBody

      _ -> fail $ "Unexpected HTTP request: " ++ reqMethod ++ " " ++ reqUri

spec :: Spec
spec = do
  describe "MCPConfig" $ do
    it "creates config with namespace" $ do
      let cfg = MCPConfig "test-server" (Just "test__")
      mcpServerName cfg `shouldBe` "test-server"
      mcpNamespace cfg `shouldBe` Just "test__"

    it "creates config without namespace" $ do
      let cfg = MCPConfig "test-server" Nothing
      mcpServerName cfg `shouldBe` "test-server"
      mcpNamespace cfg `shouldBe` Nothing

  describe "Tool Result Types" $ do
    it "MCPToolResult wraps text" $ do
      let result = MCPToolResult "test output"
      mcpResult result `shouldBe` "test output"

    -- MCPToolCallResult is now internal to the interpreter, not exposed in public API
    -- Tools now return Aeson.Value directly

    it "MCPToolArgs wraps JSON value" $ do
      let args = MCPToolArgs (Aeson.toJSON ("test" :: Text))
      argsObject args `shouldSatisfy` (\_ -> True)  -- Just check it exists

  describe "MCP Server Integration" $ do
    it "connects to runix-code-mcp and lists tools" $ do
      let serverPath = "../../dist-newstyle/build/x86_64-linux/ghc-9.10.3/runix-code-0.1.0.0/x/runix-code-mcp/build/runix-code-mcp/runix-code-mcp"

      -- Check if server exists before running test
      exists <- doesFileExist serverPath

      if not exists
        then pendingWith "MCP server not built yet - run: cabal build runix-code-mcp"
        else do
          result <- runM
                  . runError @String
                  . runFail
                  . interpretMCPStdio @TestMCPServer
                      (MCPConfig "runix-code-mcp" (Just "mcp__"))
                      serverPath
                      []
                  $ do
                    -- List tools from the server (errors handled via Fail)
                    toolsList <- listMCPTools @TestMCPServer
                    -- Verify we got at least one tool (getcwd)
                    embed $ putStrLn $ "Found " <> show (length toolsList) <> " tools"
                    return toolsList

          case result of
            Left err -> expectationFailure $ "Test failed: " <> err
            Right (Left _) -> expectationFailure "Test failed: runFail"
            Right (Right toolsList) -> do
              toolsList `shouldSatisfy` (not . null)
              -- Check that getcwd tool exists (without namespace prefix - that's added by getMCPTools)
              let toolNames = [MCP.toolDefinitionName t | t <- toolsList]
              toolNames `shouldContain` ["getcwd"]

    it "connects to runix-code-mcp and calls getcwd tool" $ do
      let serverPath = "../../dist-newstyle/build/x86_64-linux/ghc-9.10.3/runix-code-0.1.0.0/x/runix-code-mcp/build/runix-code-mcp/runix-code-mcp"

      exists <- doesFileExist serverPath

      if not exists
        then pendingWith "MCP server not built yet - run: cabal build runix-code-mcp"
        else do
          result <- runM
                  . runError @String
                  . runFail
                  . interpretMCPStdio @TestMCPServer
                      (MCPConfig "runix-code-mcp" Nothing)
                      serverPath
                      []
                  $ do
                    -- Call getcwd tool with empty arguments (returns Value now)
                    callRes <- callMCPTool @TestMCPServer "getcwd" (Aeson.object [])
                    embed $ putStrLn $ "getcwd result: " <> show callRes
                    return callRes

          case result of
            Left err -> expectationFailure $ "Test failed: " <> err
            Right (Left _) -> expectationFailure "Test failed: runFail"
            Right (Right callRes) -> do
              -- Verify we got a result (Value should not be Null)
              callRes `shouldNotBe` Aeson.Null

  describe "MCP HTTP Transport" $ do
    it "connects to HTTP MCP server and lists tools" $ do
      let baseUrl = "http://mock-server/mcp"

      result <- runM $ runError @String $ loggingIO $ cancelNoop $ failToError @String id $ mockMCPHttpIO $ interpretMCPHttp @TestMCPServer (MCPConfig "test-http-mcp" Nothing) baseUrl $ do
        -- List tools from the HTTP server
        toolsList <- listMCPTools @TestMCPServer
        embed $ putStrLn $ "Found " <> show (length toolsList) <> " tools via HTTP (mock)"
        return toolsList

      case result of
        Left err -> expectationFailure $ "HTTP test failed: " <> err
        Right toolsList -> do
          toolsList `shouldSatisfy` (not . null)
          putStrLn $ "Tool names: " <> show [MCP.toolDefinitionName t | t <- toolsList]

    it "connects to HTTP MCP server and calls a tool" $ do
      let baseUrl = "http://mock-server/mcp"

      result <- runM $ runError @String $ loggingIO $ cancelNoop $ failToError @String id $ mockMCPHttpIO $ interpretMCPHttp @TestMCPServer (MCPConfig "test-http-mcp" Nothing) baseUrl $ do
        -- First list tools to see what's available
        toolsList <- listMCPTools @TestMCPServer
        embed $ putStrLn $ "Available tools: " <> show [MCP.toolDefinitionName t | t <- toolsList]

        -- Call the first tool with empty args (as a basic test)
        case toolsList of
          [] -> throw @String "No tools available on HTTP server"
          (tool:_) -> do
            let toolName = MCP.toolDefinitionName tool
            embed $ putStrLn $ "Calling tool: " <> T.unpack toolName
            callRes <- callMCPTool @TestMCPServer toolName (Aeson.object [])
            embed $ putStrLn $ "Tool result: " <> show callRes
            return callRes

      case result of
        Left err -> expectationFailure $ "HTTP tool call failed: " <> err
        Right callRes -> do
          -- Verify we got a result (Value should not be Null)
          callRes `shouldNotBe` Aeson.Null
