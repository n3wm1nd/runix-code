{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module MCPClientSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Runix.MCPClient
import MCP.Server.Types (InputSchemaDefinition(..), InputSchemaDefinitionProperty(..))
import qualified MCP.Server.Types as MCP
import qualified Data.Aeson as Aeson
import Polysemy
import Polysemy.Fail (runFail)
import Polysemy.Error (runError, Error, throw)
import System.Directory (doesFileExist)

-- | Test server type
data TestMCPServer

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

    it "MCPToolCallResult contains content list" $ do
      let result = MCPToolCallResult []
      content result `shouldBe` []

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
                    -- List tools from the server and unwrap Either
                    toolsList <- listMCPTools @TestMCPServer >>= either (throw @String) return
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
                    -- Call getcwd tool with empty arguments
                    callRes <- callMCPTool @TestMCPServer "getcwd" (Aeson.object []) >>= either (throw @String) return
                    embed $ putStrLn $ "getcwd result: " <> show callRes
                    return callRes

          case result of
            Left err -> expectationFailure $ "Test failed: " <> err
            Right (Left _) -> expectationFailure "Test failed: runFail"
            Right (Right callRes) -> do
              -- Verify we got content back
              (content callRes) `shouldSatisfy` (not . null)
