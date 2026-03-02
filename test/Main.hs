module Main (main) where

import Test.Hspec
import qualified OutputHistorySpec
import qualified RegistrySpec
import qualified MCPSpec
import qualified PlaywrightSpec
import qualified UserInterfaceSpec
import qualified ZipperSpec

main :: IO ()
main = hspec $ do
  describe "OutputHistory" OutputHistorySpec.spec
  describe "Registry" RegistrySpec.spec
  describe "MCP" MCPSpec.spec
  describe "Playwright" PlaywrightSpec.spec
  describe "UserInterface" UserInterfaceSpec.spec
  describe "Zipper" ZipperSpec.spec
