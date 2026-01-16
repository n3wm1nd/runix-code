{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-exported-signatures #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Registry module for dynamically generated tools
--
-- This module maintains the list of all generated tools and re-exports them.
-- The tool-builder agent automatically updates this file when creating new tools.
module GeneratedTools
  ( generatedTools
    -- * Generated tool exports
    -- GENERATED_TOOL_EXPORTS_START
  , Echo.echoTool
    -- Tool exports will be added here automatically
    -- GENERATED_TOOL_EXPORTS_END
  ) where

import Runix.Safe.Polysemy (Sem)
import Runix.Safe.Polysemy.Fail (Fail)
import Runix.LLM (LLMTool(..))
import Runix.Cmd (Cmd)
import Runix.Safe.Polysemy
import Runix.FileSystem.Simple (FileSystem, FileSystemRead, FileSystemWrite)

-- GENERATED_TOOL_IMPORTS_START
import qualified GeneratedTools.Echo as Echo
-- Tool module imports will be added here automatically
-- Example: import qualified GeneratedTools.Echo as Echo
-- GENERATED_TOOL_IMPORTS_END

--------------------------------------------------------------------------------
-- Tool Registration List
--------------------------------------------------------------------------------

-- | All generated tools are registered here
-- The tool-builder agent will add new tools to this list
generatedTools :: forall r. (
  Member (Cmd "cabal") r
  , Members [FileSystem, FileSystemRead, FileSystemWrite] r
  ) => [LLMTool (Sem (Fail ': r))]
generatedTools =
  [ -- GENERATED_TOOLS_LIST_START
    LLMTool Echo.echoTool
    -- Tools will be added here automatically
    -- Example: LLMTool Echo.echoTool
    -- GENERATED_TOOLS_LIST_END
  ]

