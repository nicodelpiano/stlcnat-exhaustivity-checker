module CheckTest where

import Test.Hspec
import Test.QuickCheck

f 1 = 1

main :: IO ()
main = hspec $ do
  describe "Test" $ do
    it "Suite" $ f 1 `shouldBe` 1
