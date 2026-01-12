{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

-- | Type system for tool-builder agent
--
-- Custom newtypes ensure type safety and provide ToolParameter instances
-- for the tool-builder's API.
module Tools.ToolBuilder.Types
  ( -- * Parameter Types
    ToolName (..)
  , ToolDescription (..)
  , ToolImplementation (..)
  , BuildMode (..)

    -- * Result Types
  , BuildToolResult (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Autodocodec (HasCodec(..))
import qualified Autodocodec
import UniversalLLM.Tools (ToolParameter(..), ToolFunction(..))

--------------------------------------------------------------------------------
-- Parameter Types
--------------------------------------------------------------------------------

-- | Name of the tool to build/modify
newtype ToolName = ToolName Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Human-readable description of what the tool does
newtype ToolDescription = ToolDescription Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | The complete Haskell implementation (signature + body + instances)
newtype ToolImplementation = ToolImplementation Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Operation mode: Create new tool or modify existing one
data BuildMode
  = CreateNew
  | ModifyExisting ToolName
  deriving stock (Show, Eq)

instance HasCodec BuildMode where
  codec = Autodocodec.dimapCodec
    (\t -> if t == "create" then CreateNew else ModifyExisting (ToolName t))
    (\case
      CreateNew -> "create"
      ModifyExisting (ToolName n) -> n)
    codec

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

-- | Result from the tool-builder agent
data BuildToolResult = BuildToolResult
  { 
    buildMessage :: Text
  } deriving stock (Show, Eq)

instance HasCodec BuildToolResult where
  codec = Autodocodec.object "BuildToolResult" $
    BuildToolResult
      <$> Autodocodec.requiredField "message" "summary of what happened" Autodocodec..= buildMessage

--------------------------------------------------------------------------------
-- ToolParameter Instances
--------------------------------------------------------------------------------

instance ToolParameter ToolName where
  paramName _ _ = "tool_name"
  paramDescription _ = "name of the tool (e.g., searchFiles, analyzeCode)"

instance ToolParameter ToolDescription where
  paramName _ _ = "description"
  paramDescription _ = "what the tool does and when to use it"

instance ToolParameter ToolImplementation where
  paramName _ _ = "implementation"
  paramDescription _ = "complete Haskell code including type signature, function body, and all required instances"

instance ToolParameter BuildMode where
  paramName _ _ = "mode"
  paramDescription _ = "whether to create new tool or modify existing one (use 'create' or the name of an existing tool)"

instance ToolParameter BuildToolResult where
  paramName _ _ = "build_result"
  paramDescription _ = "result of tool build operation"

--------------------------------------------------------------------------------
-- ToolFunction Instance
--------------------------------------------------------------------------------

instance ToolFunction BuildToolResult where
  toolFunctionName _ = "build_tool"
  toolFunctionDescription _ = "Build or modify a tool using the tool-builder agent. Creates new tools or modifies existing ones with full compilation validation and automatic registration."
