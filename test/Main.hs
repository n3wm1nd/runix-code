module Main (main) where

import Test.Hspec
import qualified OutputHistorySpec
import qualified RegistrySpec

main :: IO ()
main = hspec $ do
  describe "OutputHistory" OutputHistorySpec.spec
  describe "Registry" RegistrySpec.spec
