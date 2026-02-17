{-# LANGUAGE Trustworthy #-}
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

-- | Dynamic tool bridge for connecting to unknown MCP servers.
--
-- Provides generic 'LLMTool' wrappers that work with any MCP server
-- by passing arguments and results as raw JSON.
module Runix.MCP.DynamicTools
  ( -- * Tool Generation
    getDynamicTools

  -- * Types
  , DynamicArgs(..)
  , DynamicResult(..)
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics (Generic)

import UniversalLLM.Tools (LLMTool(..), mkTool, ToolParameter(..))
import Runix.LLM.ToolInstances ()  -- Import orphan instances for Sem
import Autodocodec (HasCodec)
import qualified Autodocodec
import qualified MCP.Server.Types as MCP

import Runix.MCP (MCP, listTools, callTool)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result type for dynamically-discovered MCP tool calls
newtype DynamicResult = DynamicResult { dynamicResult :: Text }
  deriving (Show, Eq, Generic)

instance HasCodec DynamicResult where
  codec = Autodocodec.dimapCodec DynamicResult dynamicResult Autodocodec.codec

instance ToolParameter DynamicResult where
  paramName _ _ = "result"
  paramDescription _ = "result from MCP tool"

-- | Arguments type for dynamically-discovered MCP tools (JSON object)
newtype DynamicArgs = DynamicArgs { argsObject :: Value }
  deriving (Show, Eq, Generic)

instance HasCodec DynamicArgs where
  codec = Autodocodec.dimapCodec DynamicArgs argsObject Autodocodec.codec

instance ToolParameter DynamicArgs where
  paramName _ _ = "args"
  paramDescription _ = "arguments as JSON object"

--------------------------------------------------------------------------------
-- Tool Generation
--------------------------------------------------------------------------------

-- | Get LLMTools from an MCP server via dynamic discovery.
-- Each discovered tool is wrapped as a generic JSON-in/JSON-out 'LLMTool'.
getDynamicTools :: forall (server :: Type) r. (Member (MCP server) r, Member Fail r)
                => Maybe Text  -- ^ Optional namespace prefix
                -> Sem r [LLMTool (Sem (Fail : r))]
getDynamicTools namespacePrefix = do
  toolDefs <- listTools @server
  return $ map (dynamicToolToLLMTool @server namespacePrefix) toolDefs

-- | Convert MCP ToolDefinition to LLMTool using dynamic JSON bridge
dynamicToolToLLMTool :: forall (server :: Type) r. (Member (MCP server) r, Member Fail r)
                     => Maybe Text -> MCP.ToolDefinition -> LLMTool (Sem (Fail : r))
dynamicToolToLLMTool namespacePrefix (MCP.ToolDefinition toolDefName desc _schema _extra) =
  let toolNameWithNS = case namespacePrefix of
        Nothing -> toolDefName
        Just prefix -> prefix <> toolDefName
      toolFn :: DynamicArgs -> Sem (Fail : r) DynamicResult
      toolFn (DynamicArgs args) = do
        resultValue <- raise $ callTool @server toolDefName args
        let resultText = case resultValue of
              Aeson.String txt -> txt
              other -> TE.decodeUtf8 $ BSL.toStrict $ Aeson.encode other
        return $ DynamicResult resultText
  in LLMTool $ mkTool toolNameWithNS desc toolFn
