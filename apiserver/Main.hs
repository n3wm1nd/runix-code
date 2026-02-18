{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (Text)
import Data.Aeson (ToJSON, Value, (.=), object, toJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Directory as Dir

-- Config and types
import Config (RunixDataDir(..), ProjectFS(..))
import qualified Paths_runix_code

-- Tools
import qualified Runix.Tools as Tools
import UniversalLLM (ToolDefinition(..), ToolCall(..), ToolResult(..))
import UniversalLLM.Tools (LLMTool(..), llmToolToDefinition, executeToolCallFromList)

-- Polysemy
import Polysemy (Sem, runM, Member)
import Polysemy.Error (runError)
import Polysemy.Fail (Fail)
import Runix.Runner (loggingIO, failLog)
import Runix.FileSystem (FileSystem, fileSystemLocal)
import qualified Runix.FileSystem.System

--------------------------------------------------------------------------------
-- Tool Configuration
--------------------------------------------------------------------------------

-- | Available tools exposed via REST API
-- Add or remove tools here to customize what's available to API clients
availableTools :: (Member Fail r, Member (FileSystem ProjectFS) r) => [LLMTool (Sem r)]
availableTools =
  [ LLMTool (Tools.getCwd @ProjectFS)
  -- Add more tools here:
  -- , LLMTool (Tools.readFile @ProjectFS)
  -- , LLMTool (Tools.glob @ProjectFS)
  -- , LLMTool (Tools.grep @ProjectFS)
  ]

--------------------------------------------------------------------------------
-- API Types
--------------------------------------------------------------------------------

-- | Tool definition for API responses
data ApiToolDefinition = ApiToolDefinition
  { name :: Text
  , description :: Text
  , parameters :: Value
  } deriving (Generic, Show)

instance ToJSON ApiToolDefinition

-- | Tool execution response
data ToolExecuteResponse = ToolExecuteResponse
  { success :: Bool
  , result :: Maybe Value
  , errorMessage :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON ToolExecuteResponse where
  toJSON (ToolExecuteResponse s r e) = object
    [ "success" .= s
    , "result" .= r
    , "error" .= e
    ]

--------------------------------------------------------------------------------
-- API Definition
--------------------------------------------------------------------------------

type ToolsAPI =
       "tools" :> Get '[JSON] [ApiToolDefinition]
  :<|> "tools" :> Capture "toolName" Text :> ReqBody '[JSON] Value :> Post '[JSON] ToolExecuteResponse

toolsAPI :: Proxy ToolsAPI
toolsAPI = Proxy

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

-- | Newtype wrapper for the interpreter runner
newtype InterpreterRunner r = InterpreterRunner
  { runInterpreter :: forall a. Sem r a -> IO a
  }

-- | Build the interpreter runner
buildInterpreter runixDataDir cwd = do
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
          Left err -> Prelude.error $ "Effect failed: " <> err
          Right val -> return val

  return runner

--------------------------------------------------------------------------------
-- API Handlers
--------------------------------------------------------------------------------

-- | List all available tools
listTools :: forall r. (Member Fail r, Member (FileSystem ProjectFS) r) => Handler [ApiToolDefinition]
listTools = do
  return $ map convertToolDef (availableTools :: [LLMTool (Sem r)])
  where
    convertToolDef :: LLMTool (Sem r) -> ApiToolDefinition
    convertToolDef llmTool =
      let def = llmToolToDefinition llmTool
      in ApiToolDefinition
           { name = toolDefName def
           , description = toolDefDescription def
           , parameters = toolDefParameters def
           }

-- | Execute a tool
executeTool :: forall r. (Member Fail r, Member (FileSystem ProjectFS) r)
            => InterpreterRunner r -> Text -> Value -> Handler ToolExecuteResponse
executeTool (InterpreterRunner runToIO) toolName args = do
  let toolCall = ToolCall "api-call" toolName args

  -- Execute the tool
  toolResult <- liftIO $ runToIO $ executeToolCallFromList (availableTools :: [LLMTool (Sem r)]) toolCall

  -- Convert result
  return $ case toolResult of
    ToolResult _ (Right value) ->
      ToolExecuteResponse
        { success = True
        , result = Just value
        , errorMessage = Nothing
        }
    ToolResult _ (Left err) ->
      ToolExecuteResponse
        { success = False
        , result = Nothing
        , errorMessage = Just err
        }

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

server :: forall r. (Member Fail r, Member (FileSystem ProjectFS) r)
       => InterpreterRunner r -> Server ToolsAPI
server runner =
       listTools @r
  :<|> executeTool @r runner

app :: forall r. (Member Fail r, Member (FileSystem ProjectFS) r)
    => InterpreterRunner r -> Application
app runner = serve toolsAPI (server @r runner)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Get current working directory
  cwd <- Dir.getCurrentDirectory

  -- Get runix data directory
  runixDataDir <- RunixDataDir <$> Paths_runix_code.getDataDir

  -- Build interpreter
  runner <- buildInterpreter runixDataDir cwd

  putStrLn "Starting Runix Code API server on http://localhost:8080"
  putStrLn "Endpoints:"
  putStrLn "  GET  /tools           - List available tools"
  putStrLn "  POST /tools/:toolName - Execute a tool (JSON body with arguments)"

  run 8080 (app runner)
