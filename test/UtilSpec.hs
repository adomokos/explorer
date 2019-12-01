module UtilSpec (spec) where

import Data.Char (toLower)
import Explorer.Import
import Explorer.Util
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Util" $
    context "apply f on first element of a list" $ do
      it "empty list returns empty" $
        applyFirst (+2) ([] :: [Int]) `shouldBe` []

      it "applies the function on the single item" $
        applyFirst (+2) [1,2,3] `shouldBe` [3,2,3]

      it "applies the function on first item in a list" $
        applyFirst toLower "HelloWorld" `shouldBe` "helloWorld"
