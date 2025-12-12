{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Tools for Runix Code agent
--
-- Each tool is just a function in Sem r that the LLM can call.
-- universal-llm handles dispatching via ToolFunction instances.
--
-- Design principles:
-- - Each tool has a unique result type (for ToolFunction instance)
-- - Newtypes for semantic parameters (not just Text everywhere)
-- - Functions run in Sem r with required effects
-- - Keep it simple - tools are just functions
module Tools
  ( -- * File Operations
    readFile
  , writeFile
  , editFile
  , glob
  , grep
  , diff

    -- * Shell
  , bash

    -- * Build Tools
  , cabalBuild

    -- * Code Generation Tools
  , generateTool

    -- * User Interaction
  , ask

    -- * Meta
  , todoWrite
  , todoRead
  , todoCheck
  , todoDelete
  , Todo (..)

    -- * Result Types
  , ReadFileResult (..)
  , WriteFileResult (..)
  , EditFileResult (..)
  , GlobResult (..)
  , GrepResult (..)
  , DiffResult (..)
  , BashResult (..)
  , CabalBuildResult (..)
  , GenerateToolResult (..)
  , AskResult (..)
  , TodoWriteResult (..)
  , TodoReadResult (..)
  , TodoCheckResult (..)
  , TodoDeleteResult (..)

    -- * Parameter Types
  , FilePath (..)
  , FileContent (..)
  , OldString (..)
  , NewString (..)
  , Pattern (..)
  , Command (..)
  , TodoText (..)
  , WorkingDirectory (..)
  , FunctionName (..)
  , FunctionSignature (..)
  , FunctionBody (..)
  , CompileError (..)
  ) where

import Prelude hiding (readFile, writeFile, FilePath)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Polysemy (Sem, Member, Members)
import Polysemy.State (State, modify, get, put)
import Polysemy.Fail (Fail)
import Autodocodec (HasCodec(..))
import qualified Autodocodec
import UniversalLLM.Core.Tools (ToolFunction(..), ToolParameter(..))
import Runix.FileSystem.Effects (FileSystemRead, FileSystemWrite)
import qualified Runix.FileSystem.Effects
import Runix.Grep.Effects (Grep)
import Runix.Logging.Effects 
import qualified Runix.Grep.Effects
import Runix.Bash.Effects (Bash)
import qualified Runix.Bash.Effects
import Runix.Cmd.Effects (Cmd)
import qualified Runix.Cmd.Effects
import UI.UserInput (UserInput, requestInput, ImplementsWidget)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A single todo item with completion status
data Todo = Todo
  { todoText :: Text
  , todoCompleted :: Bool
  } deriving stock (Show, Eq)

instance HasCodec Todo where
  codec = Autodocodec.object "Todo" $
    Todo
      <$> Autodocodec.requiredField "text" "todo item text" Autodocodec..= todoText
      <*> Autodocodec.requiredField "completed" "completion status" Autodocodec..= todoCompleted

instance ToolParameter Todo where
  paramName _ _ = "todo"
  paramDescription _ = "a todo item"

--------------------------------------------------------------------------------
-- Parameter Newtypes
--------------------------------------------------------------------------------

newtype FilePath = FilePath Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype FileContent = FileContent Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype OldString = OldString Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype NewString = NewString Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype Pattern = Pattern Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype Command = Command Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype TodoText = TodoText Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype WorkingDirectory = WorkingDirectory Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- Code generation types
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

-- ToolParameter instances for parameters
instance ToolParameter FilePath where
  paramName _ _ = "file_path"
  paramDescription _ = "absolute path to the file"

instance ToolParameter FileContent where
  paramName _ _ = "content"
  paramDescription _ = "content to write to the file"

instance ToolParameter OldString where
  paramName _ _ = "old_string"
  paramDescription _ = "exact string to find and replace"

instance ToolParameter NewString where
  paramName _ _ = "new_string"
  paramDescription _ = "string to replace with"

instance ToolParameter Pattern where
  paramName _ _ = "pattern"
  paramDescription _ = "glob or regex pattern to match"

instance ToolParameter Command where
  paramName _ _ = "command"
  paramDescription _ = "shell command to execute"

instance ToolParameter TodoText where
  paramName _ _ = "text"
  paramDescription _ = "text or prefix of the todo item"

instance ToolParameter WorkingDirectory where
  paramName _ _ = "working_directory"
  paramDescription _ = "directory to run the command in"

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
-- Result Types (unique for ToolFunction instances)
--------------------------------------------------------------------------------

newtype ReadFileResult = ReadFileResult Text
  deriving stock (Show, Eq)

instance HasCodec ReadFileResult where
  codec = Autodocodec.dimapCodec ReadFileResult (\(ReadFileResult t) -> t) codec

newtype WriteFileResult = WriteFileResult Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

data EditFileResult = EditFileResult
  { editSuccess :: Bool
  , editMessage :: Text
  } deriving stock (Show, Eq)

instance HasCodec EditFileResult where
  codec = Autodocodec.object "EditFileResult" $
    EditFileResult
      <$> Autodocodec.requiredField "success" "whether the edit succeeded" Autodocodec..= editSuccess
      <*> Autodocodec.requiredField "message" "description of what happened" Autodocodec..= editMessage

newtype GlobResult = GlobResult [Text]
  deriving stock (Show, Eq)
  deriving (HasCodec) via [Text]

newtype GrepResult = GrepResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype DiffResult = DiffResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype BashResult = BashResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Result from cabal build - returns success status and output
data CabalBuildResult = CabalBuildResult
  { buildSuccess :: Bool
  , buildOutput :: Text
  , buildErrors :: Text
  } deriving stock (Show, Eq)

instance HasCodec CabalBuildResult where
  codec = Autodocodec.object "CabalBuildResult" $
    CabalBuildResult
      <$> Autodocodec.requiredField "success" "whether build succeeded" Autodocodec..= buildSuccess
      <*> Autodocodec.requiredField "output" "build stdout" Autodocodec..= buildOutput
      <*> Autodocodec.requiredField "errors" "build stderr" Autodocodec..= buildErrors

-- | Result from generateTool - returns success status and file path or error
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

-- | Result from ask - returns the user's response as text
newtype AskResult = AskResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Result from todo_write - unit type (nothing to return to LLM)
-- State effect handles the actual todo list mutation
data TodoWriteResult = TodoWriteResult
  deriving stock (Show, Eq)

instance HasCodec TodoWriteResult where
  codec = Autodocodec.dimapCodec (const TodoWriteResult) (const ()) Autodocodec.nullCodec

-- | Result from todo_read - returns the list of todos
newtype TodoReadResult = TodoReadResult [Todo]
  deriving stock (Show, Eq)
  deriving (HasCodec) via [Todo]

-- | Result from todo_check - returns a message about what happened
newtype TodoCheckResult = TodoCheckResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- | Result from todo_delete - returns a message about what happened
newtype TodoDeleteResult = TodoDeleteResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

-- ToolParameter instances for result types (required by ToolFunction)
instance ToolParameter ReadFileResult where
  paramName _ _ = "read_file_result"
  paramDescription _ = "file contents"

instance ToolParameter WriteFileResult where
  paramName _ _ = "write_file_result"
  paramDescription _ = "write success status"

instance ToolParameter EditFileResult where
  paramName _ _ = "edit_file_result"
  paramDescription _ = "edit result with success status and message"

instance ToolParameter GlobResult where
  paramName _ _ = "glob_result"
  paramDescription _ = "list of matching file paths"

instance ToolParameter GrepResult where
  paramName _ _ = "grep_result"
  paramDescription _ = "search results"

instance ToolParameter DiffResult where
  paramName _ _ = "diff_result"
  paramDescription _ = "unified diff output showing differences between files"

instance ToolParameter BashResult where
  paramName _ _ = "bash_result"
  paramDescription _ = "command output"

instance ToolParameter CabalBuildResult where
  paramName _ _ = "cabal_build_result"
  paramDescription _ = "cabal build result with success status and output"

instance ToolParameter GenerateToolResult where
  paramName _ _ = "generate_tool_result"
  paramDescription _ = "tool generation result with success status and details"

instance ToolParameter TodoWriteResult where
  paramName _ _ = "result"
  paramDescription _ = "todo write result (always succeeds)"

instance ToolParameter TodoReadResult where
  paramName _ _ = "todos"
  paramDescription _ = "list of all todos"

instance ToolParameter TodoCheckResult where
  paramName _ _ = "result"
  paramDescription _ = "message describing what happened (todo checked, no match found, or multiple matches)"

instance ToolParameter TodoDeleteResult where
  paramName _ _ = "result"
  paramDescription _ = "message describing what happened (todo deleted, no match found, or multiple matches)"

instance ToolParameter AskResult where
  paramName _ _ = "answer"
  paramDescription _ = "the user's text response"

-- ToolFunction instances for result types
instance ToolFunction ReadFileResult where
  toolFunctionName _ = "read_file"
  toolFunctionDescription _ = "Read a file from the filesystem and return its contents"

instance ToolFunction WriteFileResult where
  toolFunctionName _ = "write_file"
  toolFunctionDescription _ = "Write content to a new file or overwrite existing file"

instance ToolFunction EditFileResult where
  toolFunctionName _ = "edit_file"
  toolFunctionDescription _ = "Edit an existing file by replacing old_string with new_string"

instance ToolFunction GlobResult where
  toolFunctionName _ = "glob"
  toolFunctionDescription _ = "Find files matching a glob pattern"

instance ToolFunction GrepResult where
  toolFunctionName _ = "grep"
  toolFunctionDescription _ = "Search file contents using regex pattern"

instance ToolFunction DiffResult where
  toolFunctionName _ = "diff"
  toolFunctionDescription _ = "Show differences between two files using unified diff format"

instance ToolFunction BashResult where
  toolFunctionName _ = "bash"
  toolFunctionDescription _ = "Execute a bash command and return output"

instance ToolFunction CabalBuildResult where
  toolFunctionName _ = "cabal_build"
  toolFunctionDescription _ = "Run cabal build in a specified directory and return build results"

instance ToolFunction GenerateToolResult where
  toolFunctionName _ = "generate_tool"
  toolFunctionDescription _ = "Generate a new tool by providing function name, signature, and complete function definition. The function definition should include both type signature and implementation."

instance ToolFunction AskResult where
  toolFunctionName _ = "ask"
  toolFunctionDescription _ = "Ask the user for input - use this to get confirmation, ask questions, or request information from the user"

instance ToolFunction TodoWriteResult where
  toolFunctionName _ = "todo_write"
  toolFunctionDescription _ = "Add a new todo item to the list"

instance ToolFunction TodoReadResult where
  toolFunctionName _ = "todo_read"
  toolFunctionDescription _ = "Read all current todos with their completion status"

instance ToolFunction TodoCheckResult where
  toolFunctionName _ = "todo_check"
  toolFunctionDescription _ = "Mark a todo as completed by text prefix (must match exactly one todo)"

instance ToolFunction TodoDeleteResult where
  toolFunctionName _ = "todo_delete"
  toolFunctionDescription _ = "Delete a todo by text prefix (must match exactly one todo)"

--------------------------------------------------------------------------------
-- File Operations
--------------------------------------------------------------------------------

-- | Read a file from the filesystem
readFile
  :: forall r. (Members [FileSystemRead, FileSystemWrite, Fail] r) => FilePath
  -> Sem r ReadFileResult
readFile (FilePath path) = do
  contents <- Runix.FileSystem.Effects.readFile (T.unpack path)
  return $ ReadFileResult (T.decodeUtf8 contents)

-- | Write a new file
-- Fails if write operation cannot be completed
writeFile
  :: Members [FileSystemRead, FileSystemWrite, Fail] r
  => FilePath
  -> FileContent
  -> Sem r WriteFileResult
writeFile (FilePath path) (FileContent content) = do
  let bytes = T.encodeUtf8 content
  Runix.FileSystem.Effects.writeFile (T.unpack path) bytes
  return $ WriteFileResult True

-- | Edit existing file via string replacement
-- Returns error if old_string matches 0 or >1 times (must match exactly once)
-- Fails if file cannot be read or written
editFile
  :: Members [FileSystemRead, FileSystemWrite, Fail] r
  => FilePath
  -> OldString
  -> NewString
  -> Sem r EditFileResult
editFile (FilePath path) (OldString old) (NewString new) = do
  contents <- Runix.FileSystem.Effects.readFile (T.unpack path)
  let contentText = T.decodeUtf8 contents
      (replaced, occurrences) = replaceAndCount old new contentText
  case occurrences of
    0 -> return $ EditFileResult False $
           "Error: old_string not found in file. No changes made."
    1 -> do
      let newBytes = T.encodeUtf8 replaced
      Runix.FileSystem.Effects.writeFile (T.unpack path) newBytes
      return $ EditFileResult True $
        "Successfully replaced 1 occurrence in " <> path
    n -> return $ EditFileResult False $
           "Error: old_string appears " <> T.pack (show n) <> " times in file. " <>
           "To avoid unintended changes, old_string must match exactly once. " <>
           "Please make old_string more specific."

-- | Replace all occurrences and count how many replacements were made
replaceAndCount :: Text -> Text -> Text -> (Text, Int)
replaceAndCount old new haystack
  | T.null old = (haystack, 0)
  | otherwise = go [] 0 haystack
  where
    go acc count text =
      case T.breakOn old text of
        (_, rest) | T.null rest -> (T.concat (reverse (text : acc)), count)
        (before, rest) ->
          let after = T.drop (T.length old) rest
          in go (new : before : acc) (count + 1) after

-- | Find files matching a pattern
-- Fails if glob operation cannot be completed
glob
  :: Members [FileSystemRead, FileSystemWrite, Fail] r
  => Pattern
  -> Sem r GlobResult
glob (Pattern pattern) = do
  -- Glob from current directory
  files <- Runix.FileSystem.Effects.glob "." (T.unpack pattern)
  return $ GlobResult (map T.pack files)

-- | Search file contents with regex
grep
  :: Member Grep r
  => Pattern
  -> Sem r GrepResult
grep (Pattern pattern) = do
  -- Grep from current directory
  matches <- Runix.Grep.Effects.grepSearch "." (T.unpack pattern)
  -- Format matches as text
  let formatted = T.intercalate "\n" $
        map (\m -> T.pack (Runix.Grep.Effects.matchFile m) <> ":" <>
                   T.pack (show $ Runix.Grep.Effects.matchLine m) <> ":" <>
                   Runix.Grep.Effects.matchText m) matches
  return $ GrepResult formatted

-- | Run unified diff between two files
-- Fails if either file doesn't exist (uses Fail effect)
diff
  :: Members '[FileSystemRead, Cmd, Fail] r
  => FilePath
  -> FilePath
  -> Sem r DiffResult
diff (FilePath file1) (FilePath file2) = do
  -- Verify files exist (for safety)
  exists1 <- Runix.FileSystem.Effects.fileExists (T.unpack file1)
  exists2 <- Runix.FileSystem.Effects.fileExists (T.unpack file2)

  if not exists1
    then fail $ "File does not exist: " ++ T.unpack file1
    else if not exists2
    then fail $ "File does not exist: " ++ T.unpack file2
    else do
      -- Run diff command with unified format (-u)
      -- diff returns exit code 0 if files are same, 1 if different, 2 on error
      output <- Runix.Cmd.Effects.cmdExec "diff" ["-u", T.unpack file1, T.unpack file2]
      -- Check if diff failed (exit code 2 means error)
      if Runix.Cmd.Effects.exitCode output == 2
        then fail $ "diff command failed: " ++ T.unpack (Runix.Cmd.Effects.stderr output)
        else return $ DiffResult $ Runix.Cmd.Effects.stdout output

--------------------------------------------------------------------------------
-- Shell Operations
--------------------------------------------------------------------------------

-- | Execute a bash command
bash
  :: Member Bash r
  => Command
  -> Sem r BashResult
bash (Command cmd) = do
  output <- Runix.Bash.Effects.bashExec (T.unpack cmd)
  -- Format output with stdout and stderr
  let result = if Runix.Bash.Effects.exitCode output == 0
               then Runix.Bash.Effects.stdout output
               else Runix.Bash.Effects.stdout output <> "\nSTDERR:\n" <> Runix.Bash.Effects.stderr output
  return $ BashResult result

-- | Run cabal build in a specified directory
cabalBuild
  :: Member Cmd r
  => WorkingDirectory
  -> Sem r CabalBuildResult
cabalBuild (WorkingDirectory workDir) = do
  output <- Runix.Cmd.Effects.cmdExecIn (T.unpack workDir) "cabal" ["build", "all"]
  let success = Runix.Cmd.Effects.exitCode output == 0
      stdout = Runix.Cmd.Effects.stdout output
      stderr = Runix.Cmd.Effects.stderr output
  return $ CabalBuildResult success stdout stderr

-- | Generate a new tool by writing source code and compiling it
-- Fails if file operations cannot be completed
generateTool
  :: Members '[FileSystemRead, FileSystemWrite, Logging, Cmd, Fail] r
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
      currentContent <- T.decodeUtf8 <$> Runix.FileSystem.Effects.readFile generatedToolsPath

      -- Step 3: Generate new tool code with fixed signature
      let newToolCode = formatToolCodeWithSignature expectedFuncName funcSig functionDef
          updatedContent = appendToolToModule currentContent newToolCode

      -- Step 4: Store original content and write updated version
      let originalContent = currentContent
      Runix.FileSystem.Effects.writeFile generatedToolsPath (T.encodeUtf8 updatedContent)

      info("file written, compiling now")
      -- Step 5: Test compilation
      buildResult <- cabalBuild (WorkingDirectory workingDir)

      if buildSuccess buildResult
        then do
          -- Success: keep the updated file
          return $ GenerateToolResult True ("Successfully generated tool: " <> expectedFuncName) (Just (T.pack generatedToolsPath))
        else do
          -- Failure: revert to original content
          Runix.FileSystem.Effects.writeFile generatedToolsPath (T.encodeUtf8 originalContent)
          let errorMsg = "Compilation failed: " <> buildErrors buildResult
          return $ GenerateToolResult False errorMsg Nothing

-- Helper functions for code generation

-- Validate that the function signature starts with the expected function name and has ::
validateFunctionSignature :: Text -> Text -> Either Text ()
validateFunctionSignature expectedName funcSig =
  let trimmedSig = T.strip funcSig
      expectedPattern = expectedName <> " :: "
  in if expectedPattern `T.isPrefixOf` trimmedSig
     then Right ()
     else Left $ "Function signature must start with '" <> expectedPattern <> "', got: " <> T.take 50 trimmedSig

-- Validate that the function definition starts with the expected function name
validateFunctionDef :: Text -> Text -> Either Text ()
validateFunctionDef expectedName functionDef =
  let firstLine = T.take 100 functionDef -- Look at first 100 chars
      namePattern = expectedName <> " "
  in if namePattern `T.isInfixOf` firstLine
     then Right ()
     else Left $ "Function definition must start with function name '" <> expectedName <> "', got: " <> T.take 50 firstLine

-- Format tool code with LLM-provided signature and body (both should include function name)
formatToolCodeWithSignature :: Text -> Text -> Text -> Text
formatToolCodeWithSignature funcName funcSig functionDef = T.unlines
  [ "-- Generated tool: " <> funcName
  , funcSig
  , functionDef
  , ""
  ]

appendToolToModule :: Text -> Text -> Text
appendToolToModule currentContent newToolCode = currentContent <> "\n" <> newToolCode

--------------------------------------------------------------------------------
-- User Interaction
--------------------------------------------------------------------------------

-- | Ask the user for text input
-- Polymorphic over widget system - works with any UI that implements ImplementsWidget
-- The widget system determines how the input is displayed (TUI widget, CLI prompt, etc.)
-- Fails (using Fail effect) if the user cancels by pressing Esc
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
-- Meta Operations
--------------------------------------------------------------------------------

-- | Add a todo to the list using State effect
-- Tool mutates state directly, returns unit to LLM
todoWrite
  :: Member (State [Todo]) r
  => TodoText
  -> Sem r TodoWriteResult
todoWrite (TodoText text) = do
  let newTodo = Todo { todoText = text, todoCompleted = False }
  modify (newTodo :)
  return TodoWriteResult

-- | Read all todos from the state
todoRead
  :: Member (State [Todo]) r
  => Sem r TodoReadResult
todoRead = do
  todos <- get @[Todo]
  -- Return in reverse order so newest todos are at the end (more natural)
  return $ TodoReadResult (reverse todos)

-- | Mark a todo as completed by text prefix
-- Returns a message indicating success, no match, or multiple matches
todoCheck
  :: Member (State [Todo]) r
  => TodoText
  -> Sem r TodoCheckResult
todoCheck (TodoText prefix) = do
  todos <- get @[Todo]
  let matches = filter (\todo -> prefix `T.isPrefixOf` todoText todo) todos
  case matches of
    [] -> return $ TodoCheckResult "No todo found matching that prefix"
    [matchedTodo] -> do
      -- Update the single matching todo
      let updatedTodos = map
            (\todo -> if todoText todo == todoText matchedTodo
                      then todo { todoCompleted = True }
                      else todo)
            todos
      put @[Todo] updatedTodos
      return $ TodoCheckResult $ "Checked off: " <> todoText matchedTodo
    multiple -> do
      let matchTexts = T.intercalate ", " (map todoText multiple)
      return $ TodoCheckResult $ "Multiple matches found: " <> matchTexts

-- | Delete a todo by text prefix
-- Returns a message indicating success, no match, or multiple matches
todoDelete
  :: Member (State [Todo]) r
  => TodoText
  -> Sem r TodoDeleteResult
todoDelete (TodoText prefix) = do
  todos <- get @[Todo]
  let matches = filter (\todo -> prefix `T.isPrefixOf` todoText todo) todos
  case matches of
    [] -> return $ TodoDeleteResult "No todo found matching that prefix"
    [matchedTodo] -> do
      -- Remove the single matching todo
      let updatedTodos = filter (\todo -> todoText todo /= todoText matchedTodo) todos
      put @[Todo] updatedTodos
      return $ TodoDeleteResult $ "Deleted: " <> todoText matchedTodo
    multiple -> do
      let matchTexts = T.intercalate ", " (map todoText multiple)
      return $ TodoDeleteResult $ "Multiple matches found: " <> matchTexts
