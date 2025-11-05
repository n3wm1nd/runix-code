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
  , Todo (..)

    -- * Result Types
  , ReadFileResult (..)
  , WriteFileResult (..)
  , EditFileResult (..)
  , GlobResult (..)
  , GrepResult (..)
  , BashResult (..)
  , TodoWriteResult (..)

    -- * Parameter Types
  , FilePath (..)
  , FileContent (..)
  , OldString (..)
  , NewString (..)
  , Pattern (..)
  , Command (..)
  ) where

import Prelude hiding (readFile, writeFile, FilePath)
import Data.Text (Text)
import Polysemy (Sem, Member)
import Polysemy.State (State, modify)
import Autodocodec (HasCodec(..))
import qualified Autodocodec
import UniversalLLM.Core.Tools (ToolFunction(..), ToolParameter(..))
import Runix.FileSystem.Effects (FileSystem)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | A single todo item
newtype Todo = Todo Text
  deriving stock (Show, Eq)
  deriving (HasCodec) via Text

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
  toolFunctionDescription _ = "Update the todo list to track task progress"

--------------------------------------------------------------------------------
-- File Operations
--------------------------------------------------------------------------------

-- | Read a file from the filesystem
readFile
  :: forall r. (Member FileSystem r) => FilePath
  -> Sem r ReadFileResult
readFile _path = return $ ReadFileResult ""

-- | Write a new file
writeFile
  :: FilePath
  -> FileContent
  -> Sem r WriteFileResult
writeFile _path _content = return $ WriteFileResult True

-- | Edit existing file via string replacement
editFile
  :: FilePath
  -> OldString
  -> NewString
  -> Sem r EditFileResult
editFile _path _old _new = undefined

-- | Find files matching a pattern
glob
  :: Pattern
  -> Sem r GlobResult
glob _pattern = undefined

-- | Search file contents with regex
grep
  :: Pattern
  -> Sem r GrepResult
grep _pattern = undefined

--------------------------------------------------------------------------------
-- Shell Operations
--------------------------------------------------------------------------------

-- | Execute a bash command
bash
  :: Command
  -> Sem r BashResult
bash _cmd = undefined

--------------------------------------------------------------------------------
-- Meta Operations
--------------------------------------------------------------------------------

-- | Add a todo to the list using State effect
-- Tool mutates state directly, returns unit to LLM
todoWrite
  :: Member (State [Todo]) r
  => Todo
  -> Sem r TodoWriteResult
todoWrite todo = do
  modify (todo :)
  return TodoWriteResult
