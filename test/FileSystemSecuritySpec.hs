{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module FileSystemSecuritySpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, runFail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (normalise)
import Prelude hiding (readFile)

import Runix.FileSystem.Effects

--------------------------------------------------------------------------------
-- Dummy Filesystem Interpreter
--------------------------------------------------------------------------------

-- | In-memory filesystem for testing
type DummyFS = Map FilePath BS.ByteString

-- | Dummy interpreter that uses an in-memory filesystem
-- Takes a current working directory and a map of files
filesystemReadDummy :: Member (Error String) r
                    => FilePath  -- ^ Current working directory
                    -> DummyFS   -- ^ In-memory filesystem
                    -> Sem (FileSystemRead : r) a
                    -> Sem r a
filesystemReadDummy cwd fs = interpret $ \case
    ReadFile p -> return $ case Map.lookup (normalise p) fs of
        Just content -> Right content
        Nothing -> Left $ "File not found: " ++ p

    ListFiles p -> return $ case Map.lookup (normalise p) fs of
        Just _ -> Left $ "Not a directory: " ++ p
        Nothing -> Left $ "Directory not found: " ++ p

    FileExists p -> return $ Right $ Map.member (normalise p) fs

    IsDirectory _p -> return $ Right False  -- Simplified: no directories in our dummy FS

    Glob _base _pat -> return $ Right []  -- Simplified: no glob support

    GetCwd -> return cwd

filesystemWriteDummy :: Member (Error String) r
                     => DummyFS
                     -> Sem (FileSystemWrite : r) a
                     -> Sem r a
filesystemWriteDummy _fs = interpret $ \case
    WriteFile p _content -> return $ Right ()  -- Just succeed for now

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run a filesystem program with the dummy interpreter
runFS :: FilePath -> DummyFS -> Sem '[FileSystemRead, Error String] a -> Either String a
runFS cwd fs = run . runError @String . filesystemReadDummy cwd fs

-- | Run a filesystem program that may fail
runFSWithFail :: FilePath -> DummyFS -> Sem [FileSystemRead, Fail, Error String] a -> Either String a
runFSWithFail cwd fs prog =
  run
    . runError @String
    . failToError id
    . filesystemReadDummy cwd fs
    $ prog

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Dummy Filesystem Interpreter" $ do
    it "returns file content for existing files" $ do
      let fs = Map.fromList [("/test/file.txt", "Hello, World!")]
          result = runFSWithFail "/test" fs $ readFile "/test/file.txt"

      result `shouldBe` Right "Hello, World!"

    it "returns error for non-existing files" $ do
      let fs = Map.fromList [("/test/file.txt", "Hello, World!")]
          result = runFSWithFail "/test" fs $ readFile "/test/missing.txt"

      result `shouldSatisfy` \case
        Left err -> "File not found" `elem` words err
        Right _ -> False

    it "fileExists returns True for existing files" $ do
      let fs = Map.fromList [("/test/file.txt", "content")]
          result = runFSWithFail "/test" fs $ fileExists "/test/file.txt"

      result `shouldBe` Right True

    it "fileExists returns False for missing files" $ do
      let fs = Map.fromList []
          result = runFSWithFail "/test" fs $ fileExists "/test/missing.txt"

      result `shouldBe` Right False

    it "getCwd returns the working directory" $ do
      let fs = Map.empty
          result = runFS "/home/user/project" fs getCwd

      result `shouldBe` Right "/home/user/project"
