{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module MCPSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Runix.MCP
import Runix.MCP.DynamicTools
import qualified MCP.Server.Types as MCP
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as V
import Polysemy
import Polysemy.Fail (Fail, runFail, failToError)
import Polysemy.Error (runError, throw)
import System.Directory (doesFileExist)
import Runix.HTTP (HTTP(..), HTTPRequest(..), HTTPResponse(..))
import Runix.Logging (loggingIO)
import Runix.Cancellation (cancelNoop)

-- | Test server type
data TestMCPServer

-- | Mock HTTP interpreter for testing MCP without a real server
mockMCPHttpIO :: Members [Fail, Embed IO] r => Sem (HTTP : r) a -> Sem r a
mockMCPHttpIO = interpret $ \case
  HttpRequest (HTTPRequest reqMethod reqUri _reqHeaders reqBody) -> do
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

                -- Format as SSE (Server-Sent Events)
                let jsonResponse = Aeson.encode response
                let sseBody = "data: " <> jsonResponse <> "\n\n"
                let sessionId = if methodName == "initialize"
                                then [("mcp-session-id", "mock-session-123")]
                                else []

                return $ Right $ HTTPResponse
                  200
                  ([("content-type", "text/event-stream")] ++ sessionId)
                  sseBody

      _ -> fail $ "Unexpected HTTP request: " ++ reqMethod ++ " " ++ reqUri

spec :: Spec
spec = do
  describe "Config" $ do
    it "creates config with namespace" $ do
      let cfg = Config "test-server" (Just "test__")
      mcpServerName cfg `shouldBe` "test-server"
      mcpNamespace cfg `shouldBe` Just "test__"

    it "creates config without namespace" $ do
      let cfg = Config "test-server" Nothing
      mcpServerName cfg `shouldBe` "test-server"
      mcpNamespace cfg `shouldBe` Nothing

  describe "DynamicTools Types" $ do
    it "DynamicResult wraps text" $ do
      let result = DynamicResult "test output"
      dynamicResult result `shouldBe` "test output"

    it "DynamicArgs wraps JSON value" $ do
      let args = DynamicArgs (Aeson.toJSON ("test" :: Text))
      argsObject args `shouldSatisfy` (\_ -> True)

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
                  . interpretStdio @TestMCPServer
                      (Config "runix-code-mcp" (Just "mcp__"))
                      serverPath
                      []
                  $ do
                    toolsList <- listTools @TestMCPServer
                    embed $ putStrLn $ "Found " <> show (length toolsList) <> " tools"
                    return toolsList

          case result of
            Left err -> expectationFailure $ "Test failed: " <> err
            Right (Left _) -> expectationFailure "Test failed: runFail"
            Right (Right toolsList) -> do
              toolsList `shouldSatisfy` (not . null)
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
                  . interpretStdio @TestMCPServer
                      (Config "runix-code-mcp" Nothing)
                      serverPath
                      []
                  $ do
                    callRes <- callTool @TestMCPServer "getcwd" (Aeson.object [])
                    embed $ putStrLn $ "getcwd result: " <> show callRes
                    return callRes

          case result of
            Left err -> expectationFailure $ "Test failed: " <> err
            Right (Left _) -> expectationFailure "Test failed: runFail"
            Right (Right callRes) -> do
              callRes `shouldNotBe` Aeson.Null

  describe "MCP HTTP Transport" $ do
    it "connects to HTTP MCP server and lists tools" $ do
      let baseUrl = "http://mock-server/mcp"

      result <- runM $ runError @String $ loggingIO $ cancelNoop $ failToError @String id $ mockMCPHttpIO $ interpretHttp @TestMCPServer (Config "test-http-mcp" Nothing) baseUrl $ do
        toolsList <- listTools @TestMCPServer
        embed $ putStrLn $ "Found " <> show (length toolsList) <> " tools via HTTP (mock)"
        return toolsList

      case result of
        Left err -> expectationFailure $ "HTTP test failed: " <> err
        Right toolsList -> do
          toolsList `shouldSatisfy` (not . null)
          putStrLn $ "Tool names: " <> show [MCP.toolDefinitionName t | t <- toolsList]

    it "connects to HTTP MCP server and calls a tool" $ do
      let baseUrl = "http://mock-server/mcp"

      result <- runM $ runError @String $ loggingIO $ cancelNoop $ failToError @String id $ mockMCPHttpIO $ interpretHttp @TestMCPServer (Config "test-http-mcp" Nothing) baseUrl $ do
        toolsList <- listTools @TestMCPServer
        embed $ putStrLn $ "Available tools: " <> show [MCP.toolDefinitionName t | t <- toolsList]

        case toolsList of
          [] -> throw @String "No tools available on HTTP server"
          (tool:_) -> do
            let toolName = MCP.toolDefinitionName tool
            embed $ putStrLn $ "Calling tool: " <> T.unpack toolName
            callRes <- callTool @TestMCPServer toolName (Aeson.object [])
            embed $ putStrLn $ "Tool result: " <> show callRes
            return callRes

      case result of
        Left err -> expectationFailure $ "HTTP tool call failed: " <> err
        Right callRes -> do
          callRes `shouldNotBe` Aeson.Null
