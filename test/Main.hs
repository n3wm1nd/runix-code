module Main (main) where

import Test.Hspec
import qualified OutputHistorySpec

main :: IO ()
main = hspec $ do
  describe "OutputHistory" OutputHistorySpec.spec
