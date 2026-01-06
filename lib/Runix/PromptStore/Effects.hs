{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | Effect for loading prompt files from the data directory
--
-- This effect abstracts away the details of how prompts are stored
-- and loaded, allowing the interpreter to handle the actual I/O and
-- path resolution using Cabal's data file mechanism.
module Runix.PromptStore.Effects
  ( -- * Effect
    PromptStore(..)
  , getPrompt
    -- * Interpreter
  , promptStoreIO
  ) where

import Data.Kind (Type)
import Polysemy (makeSem, Sem, Member, Embed, interpret, embed)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.FilePath ((</>))
import Control.Exception (try, SomeException)
import qualified Paths_runix_code

-- | Effect for accessing stored prompt files
data PromptStore (m :: Type -> Type) a where
  -- | Get a prompt by name from the prompt directory
  -- The name should be relative to the prompt directory (e.g., "tool-builder.md", "runix-code.md")
  -- Returns Nothing if the prompt file doesn't exist or can't be read
  GetPrompt :: Text -> PromptStore m (Maybe Text)

makeSem ''PromptStore

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

-- | Interpret PromptStore using Cabal's data directory and IO
--
-- Prompts are loaded from the data directory (as specified in the .cabal file)
-- using Paths_runix_code.getDataFileName to resolve the full path.
promptStoreIO :: Member (Embed IO) r => Sem (PromptStore ': r) a -> Sem r a
promptStoreIO = interpret $ \case
  GetPrompt promptName -> embed $ do
    -- Get the full path using Cabal's mechanism
    let promptRelPath = "prompt" </> T.unpack promptName
    promptPath <- Paths_runix_code.getDataFileName promptRelPath

    -- Try to read the file, returning Nothing on any error
    result <- try @SomeException $ BS.readFile promptPath
    case result of
      Left _err -> return Nothing
      Right contents -> return $ Just (TE.decodeUtf8 contents)
