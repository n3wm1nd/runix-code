{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module GeneratedTools
  ( -- * Tool Registration
    generatedTools

    -- * Generated types and functions will be exported here
  , helloGreetingTool
    -- (tool-builder will add exports automatically)
  , echoTool
  , helloworldTool
  ) where

import UniversalLLM.Core.Tools (LLMTool(..), ToolFunction(..), ToolParameter(..))
import Polysemy (Sem, Member, Members)
import Polysemy.Fail (Fail)
-- Orphan instances for Tool system are re-exported via Runix.LLM.Effects
import Runix.LLM.Effects (LLM)
import Data.Text (Text)
import qualified Data.Text as T
import Autodocodec (HasCodec(..))
import qualified Autodocodec

-- Import effects that generated tools might need
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite)
import Runix.Grep.Effects (Grep)
import Runix.Cmd.Effects (Cmd)
import Runix.Bash.Effects (Bash)

--------------------------------------------------------------------------------
-- Tool Registration List
--------------------------------------------------------------------------------

-- | All generated tools are registered here
-- The tool-builder agent will add new tools to this list
generatedTools :: forall r. [LLMTool (Sem (Fail ': r))]
generatedTools =
  [ -- Tools will be added here by tool-builder
    -- Each new tool gets added as: LLMTool toolFunctionName
    LLMTool echoTool
  , LLMTool helloworldTool
  , LLMTool helloGreetingTool
  ]

--------------------------------------------------------------------------------
-- Generated Tools
--------------------------------------------------------------------------------

-- Tools will be appended below this line by the tool-builder agent
-- Each tool follows the pattern:
-- 1. Comment marker: -- Generated tool: toolName
-- 2. Result type with HasCodec instance
-- 3. ToolParameter instance for result type
-- 4. ToolFunction instance (defines tool name/description)
-- 5. Function implementation
-- 6. Blank line separator


-- Generated tool: echo
newtype EchoInput = EchoInput Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter EchoInput where
  paramName _ _ = "input"
  paramDescription _ = "The text to be echoed back"

newtype EchoResult = EchoResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter EchoResult where
  paramName _ _ = "result"
  paramDescription _ = "The echoed text"

instance ToolFunction EchoResult where
  toolFunctionName _ = "echo"
  toolFunctionDescription _ = "Simple echo tool that returns the input text unchanged"

echoTool :: EchoInput -> Sem r EchoResult
echoTool (EchoInput input) = return $ EchoResult input

-- Generated tool: helloworld
newtype HelloWorldInput = HelloWorldInput Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter HelloWorldInput where
  paramName _ _ = "name"
  paramDescription _ = "Name to include in the greeting"

newtype HelloWorldResult = HelloWorldResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter HelloWorldResult where
  paramName _ _ = "greeting"
  paramDescription _ = "A friendly greeting message"

instance ToolFunction HelloWorldResult where
  toolFunctionName _ = "helloworld"
  toolFunctionDescription _ = "Generates a friendly hello world greeting"

helloworldTool :: HelloWorldInput -> Sem r HelloWorldResult
helloworldTool (HelloWorldInput name) = 
  return $ HelloWorldResult $ "Hello, " <> name <> "! Welcome to the world!"


-- Generated tool: hello-greeting
-- Generated tool: hello-greeting
newtype HelloGreetingResult = HelloGreetingResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter HelloGreetingResult where
  paramName _ _ = "greeting"
  paramDescription _ = "A fixed greeting message"

instance ToolFunction HelloGreetingResult where
  toolFunctionName _ = "hello-greeting"
  toolFunctionDescription _ = "Returns a simple hello greeting without any inputs"

helloGreetingTool :: Sem r HelloGreetingResult
helloGreetingTool = return $ HelloGreetingResult "Hello, World!"
