-- | Agent-specific tools for runix-code
--
-- This module is the home for tools that are specific to the runix-code agent
-- and its particular codebase layout (paths, conventions, generated-tools
-- machinery, etc.).  Generic, universally reusable tools live in the
-- @runix-tools@ package under @Runix.Tools.*@.
--
-- = Where tools live
--
-- * @libs\/runix-tools@ — the standard library of agent capabilities.
--   Independently trustable (@-trust runix-tools@), so generated tools can
--   safely import from it.  Organised as:
--
--     * "Runix.Tools"              — file ops, shell, bash, todo list, cabal build
--     * "Runix.Tools.Config"       — filesystem phantom types (@ProjectFS@ etc.)
--     * "Runix.Tools.Claude"       — subagent \/ skill loading
--     * "Runix.Tools.ToolBuilder.*"— registry management
--     * "UI.UserInput"             — user-input effect (runix-code only)
--
--   Future additions go here too: @Runix.Tools.Git@, @Runix.Tools.JS@, …
--
-- * @apps\/runix-code\/lib\/Tools.hs@ (this file) — runix-code-specific tools
--   that depend on Cabal's @Paths_runix_code@, hard-coded project paths, or
--   the generated-tools infrastructure.
--
-- = How a tool is written
--
-- A tool is just a plain Haskell function in @Sem r@.  @universal-llm@
-- dispatches to it via the 'ToolFunction' typeclass on its /result type/.
-- Each tool gets a unique result newtype so the typeclass can distinguish them.
--
-- Here is a commented walk-through using @readFile@ from "Runix.Tools":
--
-- @
-- -- Unique result type — this is what makes the ToolFunction instance work.
-- -- ToolFunction picks the tool name\/description from the result type, not
-- -- the function name, so multiple functions could share a result type if
-- -- they have the same LLM-visible interface (they rarely do).
-- newtype ReadFileResult = ReadFileResult Text
--   deriving stock (Show, Eq)
--
-- instance HasCodec ReadFileResult where ...       -- JSON schema for the LLM
-- instance ToolParameter ReadFileResult where ...  -- how it appears in a call
-- instance ToolFunction ReadFileResult where
--   toolFunctionName _ = \"read_file\"
--   toolFunctionDescription _ = \"Read a file …\"
--
-- -- The function itself.  Effects are declared in the constraint row 'r':
-- --   FileSystemRead project  — read-only access to the 'project' filesystem
-- --   Fail                    — can fail with a message (propagated to LLM)
-- -- The @\@project@ type application selects which chrooted filesystem to use
-- -- (e.g. ProjectFS, RunixToolsFS) — compile-time enforcement of boundaries.
-- readFile
--   :: forall project r. (Members [FileSystemRead project, Fail] r)
--   => FilePath           -- ^ parameter newtype wrapping Text
--   -> Sem r ReadFileResult
-- readFile (FilePath path) = do
--   contents <- FileSystem.readFile \@project (T.unpack path)
--   return $ ReadFileResult (T.decodeUtf8 contents)
-- @
--
-- The agent loop in "Agent" collects a list of @'LLMTool' (Sem r)@ values,
-- one per tool, and hands them to @universal-llm@ which handles JSON
-- serialisation, schema generation, and dispatch automatically.
module Tools
  ( -- * Demonstration tools (not wired into the agent — illustrate the pattern)
    echo
  , echoCmd

    -- * Result Types
  , EchoResult (..)

    -- * User Interaction
  , ask

    -- * Result Types
  , AskResult (..)

    -- * Code Generation
  , generateTool

    -- * Result Types
  , GenerateToolResult (..)

    -- * Parameter Types
  , FunctionName (..)
  , FunctionSignature (..)
  , FunctionBody (..)
  , CompileError (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Polysemy (Sem, Member, Members)
import Polysemy.Fail (Fail)
import UI.UserInput (UserInput, ImplementsWidget, requestInput)
import Autodocodec (HasCodec(..))
import qualified Autodocodec
import UniversalLLM.Tools (ToolFunction(..), ToolParameter(..))
import Runix.Cmd (Cmd)
import qualified Runix.Cmd as Cmd
import Runix.FileSystem (FileSystemRead, FileSystemWrite)
import qualified Runix.FileSystem as FileSystem
import Runix.Logging (Logging, info)
import Runix.Tools (CabalBuildResult(..), WorkingDirectory(..), cabalBuild)

--------------------------------------------------------------------------------
-- Demonstration Tools
--
-- These two tools illustrate the two most common patterns:
--
--   echo    — no effects beyond Fail; the result is computed purely from
--             the input.  This is as simple as a tool gets.
--
--   echoCmd — adds a 'Cmd "echo"' effect, showing how to declare and use a
--             typed command effect.  The effect is interpreted at the call
--             site (Runner.hs / Main.hs), so the tool itself stays pure in
--             the effect-system sense: it just describes what it *needs*.
--             Under the hood the interpreter calls /usr/bin/echo via exec(3),
--             not through a shell — there is no shell involved at all.
--
-- Neither is registered in the agent tool list in Agent.hs; they exist purely
-- as readable examples.
--------------------------------------------------------------------------------

-- | Shared result type for both echo variants.
-- The result type is what the LLM sees returned; it says nothing about *how*
-- the value was produced (pure vs. effectful), only what shape it has.
newtype EchoResult = EchoResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter EchoResult where
  paramName _ _ = "message"
  paramDescription _ = "the echoed text"

instance ToolFunction EchoResult where
  toolFunctionName _ = "echo"
  toolFunctionDescription _ = "Return the input text unchanged"

-- | Echo text back — no effects needed at all.
-- The simplest possible tool: a pure function wrapped in 'return'.
echo :: Applicative f => Text -> f EchoResult
echo msg = pure (EchoResult msg)

-- | Echo text back via the system @echo@ binary (exec, no shell).
-- Demonstrates adding a typed 'Cmd' effect.  The @"echo"@ type-level string
-- is the key used by the interpreter to resolve the executable path.
echoCmd :: Member (Cmd "echo") r => Text -> Sem r EchoResult
echoCmd msg = do
  output <- Cmd.call @"echo" [T.unpack msg]
  return $ EchoResult (Cmd.stdout output)

--------------------------------------------------------------------------------
-- User Interaction
--------------------------------------------------------------------------------

-- | Result from ask — the user's text response.
newtype AskResult = AskResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter AskResult where
  paramName _ _ = "answer"
  paramDescription _ = "the user's text response"

instance ToolFunction AskResult where
  toolFunctionName _ = "ask"
  toolFunctionDescription _ = "Ask the user for small mid-task clarifications (e.g., 'how many retries maximum?', 'what should the timeout be?'). NOT for architectural or strategic decisions - use regular text response for those."

-- | Ask the user for text input during task execution.
-- Use for small mid-task clarifications, not architectural decisions.
-- Fails (via 'Fail') if the user cancels with Esc.
ask
  :: forall widget r. (Member (UserInput widget) r, Member Fail r, ImplementsWidget widget Text)
  => Text  -- ^ Question/prompt to show the user
  -> Sem r AskResult
ask question = do
  mAnswer <- requestInput @widget question ""
  case mAnswer of
    Nothing -> fail "User cancelled input"
    Just answer -> return $ AskResult answer

--------------------------------------------------------------------------------
-- Parameter Types
--------------------------------------------------------------------------------

newtype FunctionName = FunctionName Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype FunctionSignature = FunctionSignature Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype FunctionBody = FunctionBody Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype CompileError = CompileError Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

instance ToolParameter FunctionName where
  paramName _ _ = "function_name"
  paramDescription _ = "name of the function to generate"

instance ToolParameter FunctionSignature where
  paramName _ _ = "function_signature"
  paramDescription _ = "complete type signature with function name (e.g. 'add :: Int -> Int -> Int')"

instance ToolParameter FunctionBody where
  paramName _ _ = "function_body"
  paramDescription _ = "function implementation with name (e.g. 'add x y = x + y')"

instance ToolParameter CompileError where
  paramName _ _ = "compile_error"
  paramDescription _ = "compilation error message"

--------------------------------------------------------------------------------
-- Result Types
--------------------------------------------------------------------------------

data GenerateToolResult = GenerateToolResult
  { generateSuccess :: Bool
  , generateMessage :: Text
  , generatedFilePath :: Maybe Text
  } deriving stock (Show, Eq)

instance HasCodec GenerateToolResult where
  codec = Autodocodec.object "GenerateToolResult" $
    GenerateToolResult
      <$> Autodocodec.requiredField "success" "whether tool generation succeeded" Autodocodec..= generateSuccess
      <*> Autodocodec.requiredField "message" "success message or error details" Autodocodec..= generateMessage
      <*> Autodocodec.optionalField "file_path" "path to generated tool file" Autodocodec..= generatedFilePath

instance ToolParameter GenerateToolResult where
  paramName _ _ = "generate_tool_result"
  paramDescription _ = "tool generation result with success status and details"

instance ToolFunction GenerateToolResult where
  toolFunctionName _ = "generate_tool"
  toolFunctionDescription _ = "Generate a new tool by providing function name, signature, and complete function definition. The function definition should include both type signature and implementation."

--------------------------------------------------------------------------------
-- Code Generation
--------------------------------------------------------------------------------

-- | Generate a new tool by writing source code and compiling it
-- Fails if file operations cannot be completed
generateTool
  :: forall project r. Members '[FileSystemRead project, FileSystemWrite project, Logging, Runix.Cmd.Cmd "cabal", Fail] r
  => FunctionName
  -> FunctionSignature  -- The required type signature (interface contract)
  -> FunctionBody       -- Complete function definition from LLM
  -> Sem r GenerateToolResult
generateTool (FunctionName expectedFuncName) (FunctionSignature funcSig) (FunctionBody functionDef) = do
  let generatedToolsPath = "apps/runix-code/lib/GeneratedTools.hs"
      workingDir = "."

  -- Step 1: Validate both signature and body start with expected function name
  case (validateFunctionSignature expectedFuncName funcSig, validateFunctionDef expectedFuncName functionDef) of
    (Left errMsg, _) -> return $ GenerateToolResult False errMsg Nothing
    (_, Left errMsg) -> return $ GenerateToolResult False errMsg Nothing
    (Right (), Right ()) -> do
      -- Step 2: Read GeneratedTools.hs (must exist since it's in cabal build)
      currentContent <- T.decodeUtf8 <$> FileSystem.readFile @project generatedToolsPath

      -- Step 3: Generate new tool code with fixed signature
      let newToolCode = formatToolCodeWithSignature expectedFuncName funcSig functionDef
          updatedContent = appendToolToModule currentContent newToolCode

      -- Step 4: Store original content and write updated version
      let originalContent = currentContent
      FileSystem.writeFile @project generatedToolsPath (T.encodeUtf8 updatedContent)

      info "file written, compiling now"
      -- Step 5: Test compilation
      CabalBuildResult success _stdout errors <- cabalBuild (WorkingDirectory workingDir)

      if success
        then return $ GenerateToolResult True ("Successfully generated tool: " <> expectedFuncName) (Just (T.pack generatedToolsPath))
        else do
          -- Failure: revert to original content
          FileSystem.writeFile @project generatedToolsPath (T.encodeUtf8 originalContent)
          return $ GenerateToolResult False ("Compilation failed: " <> errors) Nothing

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

validateFunctionSignature :: Text -> Text -> Either Text ()
validateFunctionSignature expectedName funcSig =
  let trimmedSig = T.strip funcSig
      expectedPattern = expectedName <> " :: "
  in if expectedPattern `T.isPrefixOf` trimmedSig
     then Right ()
     else Left $ "Function signature must start with '" <> expectedPattern <> "', got: " <> T.take 50 trimmedSig

validateFunctionDef :: Text -> Text -> Either Text ()
validateFunctionDef expectedName functionDef =
  let firstLine = T.take 100 functionDef
      namePattern = expectedName <> " "
  in if namePattern `T.isInfixOf` firstLine
     then Right ()
     else Left $ "Function definition must start with function name '" <> expectedName <> "', got: " <> T.take 50 firstLine

formatToolCodeWithSignature :: Text -> Text -> Text -> Text
formatToolCodeWithSignature funcName funcSig functionDef = T.unlines
  [ "-- Generated tool: " <> funcName
  , funcSig
  , functionDef
  , ""
  ]

appendToolToModule :: Text -> Text -> Text
appendToolToModule currentContent newToolCode = currentContent <> "\n" <> newToolCode
