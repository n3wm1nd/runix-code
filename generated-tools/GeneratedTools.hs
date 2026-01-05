{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Registry module for dynamically generated tools
--
-- This module maintains the list of all generated tools and re-exports them.
-- The tool-builder agent automatically updates this file when creating new tools.
module GeneratedTools
  ( generatedTools
    -- * Generated tool exports
    -- GENERATED_TOOL_EXPORTS_START
    -- Tool exports will be added here automatically
    -- GENERATED_TOOL_EXPORTS_END
  ) where

import UniversalLLM.Core.Tools (LLMTool(..))
import Polysemy (Sem)
import Polysemy.Fail (Fail)

-- GENERATED_TOOL_IMPORTS_START
-- Tool module imports will be added here automatically
-- Example: import qualified GeneratedTools.Echo as Echo
-- GENERATED_TOOL_IMPORTS_END

--------------------------------------------------------------------------------
-- Tool Registration List
--------------------------------------------------------------------------------

-- | All generated tools are registered here
-- The tool-builder agent will add new tools to this list
generatedTools :: forall r. [LLMTool (Sem (Fail ': r))]
generatedTools =
  [ -- GENERATED_TOOLS_LIST_START
    -- Tools will be added here automatically
    -- Example: LLMTool Echo.echoTool
    -- GENERATED_TOOLS_LIST_END
  ]
