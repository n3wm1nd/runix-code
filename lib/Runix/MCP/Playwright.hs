{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

-- | Typed Polysemy effect for Playwright browser automation via MCP.
--
-- Provides a proper effect with typed constructors, backed by a reinterpreter
-- from the generic 'MCP Playwright' effect. Consumer code requires only
-- @Member Playwright r@ — no knowledge of MCP is needed.
--
-- @
-- runPlaywright serverUrl
--   $ do navigate (Url "https://example.com")
--        snap <- snapshot
--        closePage
-- @
module Runix.MCP.Playwright
  ( -- * Effect
    Playwright(..)
  , navigate
  , navigateBack
  , click
  , hover
  , typeText
  , snapshot
  , takeScreenshot
  , closePage
  , waitFor
  , evaluate

  -- * Server tag
  , PlaywrightServer

  -- * Reinterpreter
  , playwrightFromMCP

  -- * Stack helpers
  , runPlaywright
  , runPlaywrightStdio

  -- * Param types
  , Url(..)
  , ElementRef(..)
  , InputText(..)
  , WaitSpec(..)
  , JsExpression(..)

  -- * Result types
  , NavigateResult(..)
  , NavigateBackResult(..)
  , ClickResult(..)
  , HoverResult(..)
  , TypeResult(..)
  , SnapshotResult(..)
  , ScreenshotResult(..)
  , CloseResult(..)
  , WaitResult(..)
  , EvaluateResult(..)
  ) where

import Polysemy
import Polysemy.Fail (Fail)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Aeson ((.=), Value, FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)

import Autodocodec (HasCodec)
import UniversalLLM.Tools (ToolParameter(..), ToolFunction(..))

import Runix.MCP (MCP, callTool, Config(..), interpretStdio, interpretHttp)
import Runix.HTTP (HTTP)

-- | Phantom type tag for the Playwright MCP server.
-- Used as the server parameter in @MCP PlaywrightServer@.
data PlaywrightServer

--------------------------------------------------------------------------------
-- Param types
--------------------------------------------------------------------------------

newtype Url = Url { unUrl :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter Url where
  paramName _ _ = "url"
  paramDescription _ = "URL to navigate to"

newtype ElementRef = ElementRef { unElementRef :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter ElementRef where
  paramName _ _ = "element"
  paramDescription _ = "Element reference from a page snapshot"

newtype InputText = InputText { unInputText :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter InputText where
  paramName _ _ = "text"
  paramDescription _ = "Text to type into the element"

newtype WaitSpec = WaitSpec { unWaitSpec :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter WaitSpec where
  paramName _ _ = "selector"
  paramDescription _ = "CSS selector or condition to wait for"

newtype JsExpression = JsExpression { unJsExpression :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter JsExpression where
  paramName _ _ = "expression"
  paramDescription _ = "JavaScript expression to evaluate in the browser"

--------------------------------------------------------------------------------
-- Result types
--------------------------------------------------------------------------------

newtype NavigateResult = NavigateResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter NavigateResult where
  paramName _ _ = "result"
  paramDescription _ = "Navigation result"

instance ToolFunction NavigateResult where
  toolFunctionName _ = "browser_navigate"
  toolFunctionDescription _ = "Navigate to a URL in the browser"

newtype NavigateBackResult = NavigateBackResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter NavigateBackResult where
  paramName _ _ = "result"
  paramDescription _ = "Navigate-back result"

instance ToolFunction NavigateBackResult where
  toolFunctionName _ = "browser_navigate_back"
  toolFunctionDescription _ = "Navigate back in browser history"

newtype ClickResult = ClickResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter ClickResult where
  paramName _ _ = "result"
  paramDescription _ = "Click result"

instance ToolFunction ClickResult where
  toolFunctionName _ = "browser_click"
  toolFunctionDescription _ = "Click an element on the page"

newtype HoverResult = HoverResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter HoverResult where
  paramName _ _ = "result"
  paramDescription _ = "Hover result"

instance ToolFunction HoverResult where
  toolFunctionName _ = "browser_hover"
  toolFunctionDescription _ = "Hover over an element on the page"

newtype TypeResult = TypeResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter TypeResult where
  paramName _ _ = "result"
  paramDescription _ = "Type result"

instance ToolFunction TypeResult where
  toolFunctionName _ = "browser_type"
  toolFunctionDescription _ = "Type text into an element"

newtype SnapshotResult = SnapshotResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter SnapshotResult where
  paramName _ _ = "result"
  paramDescription _ = "Page accessibility snapshot"

instance ToolFunction SnapshotResult where
  toolFunctionName _ = "browser_snapshot"
  toolFunctionDescription _ = "Take an accessibility snapshot of the page"

newtype ScreenshotResult = ScreenshotResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter ScreenshotResult where
  paramName _ _ = "result"
  paramDescription _ = "Screenshot data"

instance ToolFunction ScreenshotResult where
  toolFunctionName _ = "browser_take_screenshot"
  toolFunctionDescription _ = "Take a screenshot of the page"

newtype CloseResult = CloseResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter CloseResult where
  paramName _ _ = "result"
  paramDescription _ = "Close result"

instance ToolFunction CloseResult where
  toolFunctionName _ = "browser_close"
  toolFunctionDescription _ = "Close the current browser page"

newtype WaitResult = WaitResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter WaitResult where
  paramName _ _ = "result"
  paramDescription _ = "Wait result"

instance ToolFunction WaitResult where
  toolFunctionName _ = "browser_wait_for"
  toolFunctionDescription _ = "Wait for a condition or selector"

newtype EvaluateResult = EvaluateResult Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (HasCodec)

instance ToolParameter EvaluateResult where
  paramName _ _ = "result"
  paramDescription _ = "JavaScript evaluation result"

instance ToolFunction EvaluateResult where
  toolFunctionName _ = "browser_evaluate"
  toolFunctionDescription _ = "Evaluate JavaScript in the browser"

--------------------------------------------------------------------------------
-- Effect Definition
--------------------------------------------------------------------------------

-- | Typed Playwright browser automation effect.
-- Also serves as the phantom type for @MCP Playwright@.
data Playwright (m :: Type -> Type) a where
  Navigate     :: Url          -> Playwright m NavigateResult
  NavigateBack ::                 Playwright m NavigateBackResult
  Click        :: ElementRef   -> Playwright m ClickResult
  Hover        :: ElementRef   -> Playwright m HoverResult
  TypeText     :: ElementRef   -> InputText -> Playwright m TypeResult
  Snapshot     ::                 Playwright m SnapshotResult
  TakeScreenshot ::               Playwright m ScreenshotResult
  ClosePage    ::                 Playwright m CloseResult
  WaitFor      :: WaitSpec     -> Playwright m WaitResult
  Evaluate     :: JsExpression -> Playwright m EvaluateResult

makeSem ''Playwright

--------------------------------------------------------------------------------
-- Reinterpreter
--------------------------------------------------------------------------------

-- | Parse a JSON 'Value' into a result type, failing with 'Fail' on error.
parseResult :: (Member Fail r, FromJSON a) => Value -> Sem r a
parseResult val = case Aeson.fromJSON val of
  Aeson.Error msg -> fail $ "Failed to parse MCP result: " <> msg
  Aeson.Success a -> return a

-- All result newtypes wrap Text, so they parse from JSON strings:
instance FromJSON NavigateResult     where parseJSON = fmap NavigateResult     . parseJSON
instance FromJSON NavigateBackResult where parseJSON = fmap NavigateBackResult . parseJSON
instance FromJSON ClickResult        where parseJSON = fmap ClickResult        . parseJSON
instance FromJSON HoverResult        where parseJSON = fmap HoverResult        . parseJSON
instance FromJSON TypeResult         where parseJSON = fmap TypeResult         . parseJSON
instance FromJSON SnapshotResult     where parseJSON = fmap SnapshotResult     . parseJSON
instance FromJSON ScreenshotResult   where parseJSON = fmap ScreenshotResult   . parseJSON
instance FromJSON CloseResult        where parseJSON = fmap CloseResult        . parseJSON
instance FromJSON WaitResult         where parseJSON = fmap WaitResult         . parseJSON
instance FromJSON EvaluateResult     where parseJSON = fmap EvaluateResult     . parseJSON

instance ToJSON NavigateResult     where toJSON (NavigateResult t)     = toJSON t
instance ToJSON NavigateBackResult where toJSON (NavigateBackResult t) = toJSON t
instance ToJSON ClickResult        where toJSON (ClickResult t)        = toJSON t
instance ToJSON HoverResult        where toJSON (HoverResult t)        = toJSON t
instance ToJSON TypeResult         where toJSON (TypeResult t)         = toJSON t
instance ToJSON SnapshotResult     where toJSON (SnapshotResult t)     = toJSON t
instance ToJSON ScreenshotResult   where toJSON (ScreenshotResult t)   = toJSON t
instance ToJSON CloseResult        where toJSON (CloseResult t)        = toJSON t
instance ToJSON WaitResult         where toJSON (WaitResult t)         = toJSON t
instance ToJSON EvaluateResult     where toJSON (EvaluateResult t)     = toJSON t

-- | Reinterpret Playwright into MCP PlaywrightServer calls.
--
-- Replaces @Playwright@ on the effect stack with @MCP PlaywrightServer@.
-- The @MCP PlaywrightServer@ layer must then be consumed by 'interpretStdio'
-- or 'interpretHttp'.
--
-- @
-- interpretStdio \@PlaywrightServer cfg cmd args
--   . playwrightFromMCP
--   $ program   -- uses Member Playwright r
-- @
playwrightFromMCP :: Member Fail r
                  => Sem (Playwright : r) a -> Sem (MCP PlaywrightServer : r) a
playwrightFromMCP = reinterpret $ \case
  Navigate (Url url) -> do
    val <- callTool @PlaywrightServer "browser_navigate" $ Aeson.object ["url" .= url]
    parseResult val

  NavigateBack -> do
    val <- callTool @PlaywrightServer "browser_navigate_back" $ Aeson.object []
    parseResult val

  Click (ElementRef ref) -> do
    val <- callTool @PlaywrightServer "browser_click" $ Aeson.object ["ref" .= ref]
    parseResult val

  Hover (ElementRef ref) -> do
    val <- callTool @PlaywrightServer "browser_hover" $ Aeson.object ["ref" .= ref]
    parseResult val

  TypeText (ElementRef ref) (InputText txt) -> do
    val <- callTool @PlaywrightServer "browser_type" $
      Aeson.object ["ref" .= ref, "text" .= txt]
    parseResult val

  Snapshot -> do
    val <- callTool @PlaywrightServer "browser_snapshot" $ Aeson.object []
    parseResult val

  TakeScreenshot -> do
    val <- callTool @PlaywrightServer "browser_take_screenshot" $ Aeson.object []
    parseResult val

  ClosePage -> do
    val <- callTool @PlaywrightServer "browser_close" $ Aeson.object []
    parseResult val

  WaitFor (WaitSpec selector) -> do
    val <- callTool @PlaywrightServer "browser_wait_for" $
      Aeson.object ["selector" .= selector]
    parseResult val

  Evaluate (JsExpression expr) -> do
    val <- callTool @PlaywrightServer "browser_evaluate" $
      Aeson.object ["expression" .= expr]
    parseResult val

--------------------------------------------------------------------------------
-- Stack Helpers
--------------------------------------------------------------------------------

-- | Run Playwright via HTTP MCP transport (server already running).
runPlaywright :: (Member (Embed IO) r, Member Fail r, Member HTTP r)
             => String  -- ^ MCP server URL
             -> Sem (Playwright : r) a -> Sem r a
runPlaywright serverUrl =
    interpretHttp @PlaywrightServer (Config "playwright" Nothing) serverUrl
  . playwrightFromMCP

-- | Run Playwright via stdio MCP transport (spawns the MCP process).
runPlaywrightStdio :: (Member (Embed IO) r, Member Fail r)
                   => Sem (Playwright : r) a -> Sem r a
runPlaywrightStdio =
    interpretStdio @PlaywrightServer (Config "playwright" Nothing) "npx" ["@playwright/mcp@latest"]
  . playwrightFromMCP
