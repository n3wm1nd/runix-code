{-# LANGUAGE UndecidableInstances #-}
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

    -- * Shell
  , bash

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
  , BashResult (..)
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
  ) where

import Prelude hiding (readFile, writeFile, FilePath)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Polysemy (Sem, Member, embed)
import Polysemy.State (State, modify, get, put)
import Polysemy.Embed (Embed)
import Autodocodec (HasCodec(..))
import qualified Autodocodec
import UniversalLLM.Core.Tools (ToolFunction(..), ToolParameter(..))
import Runix.FileSystem.Effects (FileSystem)
import qualified Runix.FileSystem.Effects
import Runix.Grep.Effects (Grep)
import qualified Runix.Grep.Effects
import Runix.Bash.Effects (Bash)
import qualified Runix.Bash.Effects

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

newtype EditFileResult = EditFileResult Bool
  deriving stock (Show, Eq)
  deriving (HasCodec) via Bool

newtype GlobResult = GlobResult [Text]
  deriving stock (Show, Eq)
  deriving (HasCodec) via [Text]

newtype GrepResult = GrepResult Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

newtype BashResult = BashResult Text
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
  paramDescription _ = "edit success status"

instance ToolParameter GlobResult where
  paramName _ _ = "glob_result"
  paramDescription _ = "list of matching file paths"

instance ToolParameter GrepResult where
  paramName _ _ = "grep_result"
  paramDescription _ = "search results"

instance ToolParameter BashResult where
  paramName _ _ = "bash_result"
  paramDescription _ = "command output"

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

instance ToolFunction BashResult where
  toolFunctionName _ = "bash"
  toolFunctionDescription _ = "Execute a bash command and return output"

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
  :: forall r. (Member FileSystem r) => FilePath
  -> Sem r ReadFileResult
readFile (FilePath path) = do
  contents <- Runix.FileSystem.Effects.readFile (T.unpack path)
  return $ ReadFileResult (T.decodeUtf8 $ BL.toStrict contents)

-- | Write a new file
writeFile
  :: Member FileSystem r
  => FilePath
  -> FileContent
  -> Sem r WriteFileResult
writeFile (FilePath path) (FileContent content) = do
  let bytes = BL.fromStrict $ T.encodeUtf8 content
  Runix.FileSystem.Effects.writeFile (T.unpack path) bytes
  return $ WriteFileResult True

-- | Edit existing file via string replacement
editFile
  :: Member FileSystem r
  => FilePath
  -> OldString
  -> NewString
  -> Sem r EditFileResult
editFile (FilePath path) (OldString old) (NewString new) = do
  contents <- Runix.FileSystem.Effects.readFile (T.unpack path)
  let contentText = T.decodeUtf8 $ BL.toStrict contents
      replaced = T.replace old new contentText
      newBytes = BL.fromStrict $ T.encodeUtf8 replaced
  Runix.FileSystem.Effects.writeFile (T.unpack path) newBytes
  return $ EditFileResult True

-- | Find files matching a pattern
glob
  :: Member FileSystem r
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
