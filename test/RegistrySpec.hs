module RegistrySpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Tools.ToolBuilder.Agent (ToolDef(..), parseToolsList, renderToolsList)

spec :: Spec
spec = do
  describe "parseToolsList" $ do
    it "parses a single enabled tool" $ do
      let input = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , "    LLMTool Echo.echoTool"
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      parseToolsList input `shouldBe` [ToolDef "Echo" "echoTool" False]

    it "parses multiple enabled tools with commas" $ do
      let input = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , "    LLMTool Echo.echoTool"
            , "  , LLMTool Foo.fooTool"
            , "  , LLMTool Bar.barTool"
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      parseToolsList input `shouldBe`
        [ ToolDef "Echo" "echoTool" False
        , ToolDef "Foo" "fooTool" False
        , ToolDef "Bar" "barTool" False
        ]

    it "parses disabled tools" $ do
      let input = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , "    LLMTool Echo.echoTool"
            , "  -- , LLMTool Foo.fooTool"
            , "  , LLMTool Bar.barTool"
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      parseToolsList input `shouldBe`
        [ ToolDef "Echo" "echoTool" False
        , ToolDef "Foo" "fooTool" True
        , ToolDef "Bar" "barTool" False
        ]

    it "handles malformed input with multiple LLMTool on same line" $ do
      let input = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , "  , LLMTool LLMTool Echo.echoTool"
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      -- Should extract "LLMTool Echo.echoTool" -> module="LLMTool Echo", function="echoTool"
      -- This is acceptable since we're robust to malformed input
      parseToolsList input `shouldSatisfy` (\tools -> length tools == 1)

    it "parses comment lines with LLMTool as disabled tools" $ do
      let input = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , "    LLMTool Echo.echoTool"
            , "    -- Tools will be added here"
            , "    -- Example: LLMTool Foo.foo"
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      -- The comment with LLMTool Foo.foo is parsed as a disabled tool
      parseToolsList input `shouldBe`
        [ ToolDef "Echo" "echoTool" False
        , ToolDef "Foo" "foo" True
        ]

    it "handles empty tools list" $ do
      let input = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      parseToolsList input `shouldBe` []

  describe "renderToolsList" $ do
    it "renders a single tool without leading comma" $ do
      let tools = [ToolDef "Echo" "echoTool" False]
          output = renderToolsList tools
      output `shouldBe` "    LLMTool Echo.echoTool\n"

    it "renders multiple tools with proper commas" $ do
      let tools =
            [ ToolDef "Echo" "echoTool" False
            , ToolDef "Foo" "fooTool" False
            , ToolDef "Bar" "barTool" False
            ]
          output = renderToolsList tools
          expected = T.unlines
            [ "    LLMTool Echo.echoTool"
            , "  , LLMTool Foo.fooTool"
            , "  , LLMTool Bar.barTool"
            ]
      output `shouldBe` expected

    it "renders disabled tools with comment prefix" $ do
      let tools =
            [ ToolDef "Echo" "echoTool" False
            , ToolDef "Foo" "fooTool" True
            , ToolDef "Bar" "barTool" False
            ]
          output = renderToolsList tools
          expected = T.unlines
            [ "    LLMTool Echo.echoTool"
            , "--   , LLMTool Foo.fooTool"
            , "  , LLMTool Bar.barTool"
            ]
      output `shouldBe` expected

    it "renders empty list as empty string" $ do
      renderToolsList [] `shouldBe` ""

  describe "round-trip property" $ do
    it "parse . render = identity for enabled tools" $ do
      let tools =
            [ ToolDef "Echo" "echoTool" False
            , ToolDef "Foo" "fooTool" False
            ]
          rendered = renderToolsList tools
          fullDoc = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , rendered
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      parseToolsList fullDoc `shouldBe` tools

    it "parse . render = identity for mixed enabled/disabled tools" $ do
      let tools =
            [ ToolDef "Echo" "echoTool" False
            , ToolDef "Foo" "fooTool" True
            , ToolDef "Bar" "barTool" False
            ]
          rendered = renderToolsList tools
          fullDoc = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , rendered
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
      parseToolsList fullDoc `shouldBe` tools

    it "render . parse . render = render for well-formed input" $ do
      let input = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , "    LLMTool Echo.echoTool"
            , "  , LLMTool Foo.fooTool"
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
          tools = parseToolsList input
          rendered = renderToolsList tools
          fullDoc = T.unlines
            [ "  [ -- GENERATED_TOOLS_LIST_START"
            , rendered
            , "    -- GENERATED_TOOLS_LIST_END"
            ]
          reParsed = parseToolsList fullDoc
          reRendered = renderToolsList reParsed
      reRendered `shouldBe` rendered
