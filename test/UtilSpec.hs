module UtilSpec (spec) where

import Data.Char (toLower)
import Explorer.Import
import Explorer.Util
import System.IO (readFile)
import Test.Hspec

spec :: Spec
spec =
  describe "Util" $ do
    context "applyFirst" $ do
      it "empty list returns empty" $ do
        json <- readFile "./resources/fixtures/user.json"
        runSimpleApp . logInfo $ "Number of ghMetrics: " <> displayShow json

        applyFirst (+2) ([] :: [Int]) `shouldBe` []

      it "applies the function on the single item" $
        applyFirst (+2) ([1,2,3] :: [Int]) `shouldBe` [3,2,3]

      it "applies the function on first item in a list" $
        applyFirst toLower "HelloWorld" `shouldBe` "helloWorld"

    context "toEither" $ do
      it "returns the Nothing on the Left" $
        toEither "Some error" Nothing
          `shouldBe` (Left "Some error" :: Either String Int)

      it "returns the Just value on the Right" $
        toEither "Some error" (Just 1)
          `shouldBe` (Right 1 :: Either String Int)

    context "showEither" $ do
      it "converts the Left value to Utf8Builder" $
        textDisplay (showEither (Left "some error" :: Either String Int))
          `shouldBe` "Left: \"some error\""

      it "converts the Right value to Utf8Builder" $
        textDisplay (showEither (Right 2 :: Either String Int))
          `shouldBe` "Right: 2"
