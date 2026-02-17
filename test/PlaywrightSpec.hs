{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module PlaywrightSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Polysemy
import Polysemy.Fail (runFail)
import Runix.MCP (MCP(..))
import Runix.MCP.Playwright

--------------------------------------------------------------------------------
-- Canned responses (recorded from real Playwright MCP server)
--------------------------------------------------------------------------------

snapshotBlank :: Text
snapshotBlank = "### Page\n- Page URL: about:blank\n### Snapshot\n```yaml\n\n```"

navigateResponse :: Text
navigateResponse = T.unlines
  [ "### Ran Playwright code"
  , "```js"
  , "await page.goto('https://example.com');"
  , "```"
  , "### Page"
  , "- Page URL: https://example.com/"
  , "- Page Title: Example Domain"
  , "### Snapshot"
  , "```yaml"
  , "- generic [ref=e2]:"
  , "  - heading \"Example Domain\" [level=1] [ref=e3]"
  , "  - paragraph [ref=e4]: This domain is for use in illustrative examples."
  , "  - paragraph [ref=e5]:"
  , "    - link \"More information...\" [ref=e6] [cursor=pointer]:"
  , "      - /url: https://www.iana.org/domains/example"
  , "```"
  ]

clickResponse :: Text
clickResponse = "### Ran Playwright code\n```js\nawait page.locator('[ref=e6]').click();\n```"

hoverResponse :: Text
hoverResponse = "### Ran Playwright code\n```js\nawait page.locator('[ref=e3]').hover();\n```"

typeResponse :: Text
typeResponse = "### Ran Playwright code\n```js\nawait page.locator('[ref=e10]').fill('hello');\n```"

evaluateResponse :: Text
evaluateResponse = "### Ran Playwright code\n```js\nawait page.evaluate(() => document.title);\n```\n### Result\nExample Domain"

closeResponse :: Text
closeResponse = "### Ran Playwright code\n```js\nawait page.close();\n```"

waitResponse :: Text
waitResponse = "### Ran Playwright code\n```js\nawait page.waitForSelector('h1');\n```"

navigateBackResponse :: Text
navigateBackResponse = "### Ran Playwright code\n```js\nawait page.goBack();\n```\n### Page\n- Page URL: about:blank"

screenshotResponse :: Text
screenshotResponse = "### Screenshot\nScreenshot saved to /tmp/screenshot.png"

--------------------------------------------------------------------------------
-- Mock MCP interpreter
--------------------------------------------------------------------------------

-- | Mock interpreter for MCP PlaywrightServer that returns canned responses.
-- Handles tool calls by matching tool name and returning recorded responses.
mockPlaywright :: Sem (MCP PlaywrightServer : r) a -> Sem r a
mockPlaywright = interpret $ \case
  ListTools -> return $ Right []  -- Not needed for typed effect tests

  CallTool toolName _args ->
    return $ Right $ Aeson.String $ case toolName of
      "browser_snapshot"        -> snapshotBlank
      "browser_navigate"        -> navigateResponse
      "browser_navigate_back"   -> navigateBackResponse
      "browser_click"           -> clickResponse
      "browser_hover"           -> hoverResponse
      "browser_type"            -> typeResponse
      "browser_take_screenshot" -> screenshotResponse
      "browser_close"           -> closeResponse
      "browser_wait_for"        -> waitResponse
      "browser_evaluate"        -> evaluateResponse
      other                     -> "Unknown tool: " <> other

-- | Run a Playwright action against the mock
runMock action =
  run . runFail . mockPlaywright . playwrightFromMCP $ action

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Typed Playwright Effect (mock)" $ do
    it "takes a snapshot" $ do
      let result = runMock snapshot
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (SnapshotResult txt) -> do
          txt `shouldSatisfy` T.isInfixOf "about:blank"
          txt `shouldSatisfy` T.isInfixOf "### Snapshot"

    it "navigates to a URL" $ do
      let result = runMock $ navigate (Url "https://example.com")
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (NavigateResult txt) -> do
          txt `shouldSatisfy` T.isInfixOf "Example Domain"
          txt `shouldSatisfy` T.isInfixOf "[ref=e3]"
          txt `shouldSatisfy` T.isInfixOf "https://example.com/"

    it "navigates back" $ do
      let result = runMock navigateBack
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (NavigateBackResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "goBack"

    it "clicks an element" $ do
      let result = runMock $ click (ElementRef "e6")
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (ClickResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "click"

    it "hovers an element" $ do
      let result = runMock $ hover (ElementRef "e3")
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (HoverResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "hover"

    it "types text into an element" $ do
      let result = runMock $ typeText (ElementRef "e10") (InputText "hello")
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (TypeResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "fill"

    it "takes a screenshot" $ do
      let result = runMock takeScreenshot
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (ScreenshotResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "Screenshot"

    it "closes the page" $ do
      let result = runMock closePage
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (CloseResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "close"

    it "waits for a selector" $ do
      let result = runMock $ waitFor (WaitSpec "h1")
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (WaitResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "waitForSelector"

    it "evaluates JavaScript" $ do
      let result = runMock $ evaluate (JsExpression "document.title")
      case result of
        Left err -> expectationFailure $ "Failed: " <> err
        Right (EvaluateResult txt) ->
          txt `shouldSatisfy` T.isInfixOf "Example Domain"
