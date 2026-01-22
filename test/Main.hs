module Main (main) where

import Test.Hspec
import qualified OutputHistorySpec
import qualified RegistrySpec
import qualified MCPClientSpec

main :: IO ()
main = hspec $ do
  describe "OutputHistory" OutputHistorySpec.spec
  describe "Registry" RegistrySpec.spec
  describe "MCPClient" MCPClientSpec.spec
