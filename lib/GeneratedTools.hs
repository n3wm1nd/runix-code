{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

module GeneratedTools
  ( -- * Tool Registration
    generatedTools

    -- * Generated types and functions will be exported here
    -- (tool-builder will add exports automatically)
  ) where

import UniversalLLM.Core.Tools (LLMTool(..), ToolFunction(..), ToolParameter(..))
import Polysemy (Sem, Member, Members)
import Polysemy.Fail (Fail)
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
    -- Each new tool gets added as: , LLMTool toolFunctionName
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
