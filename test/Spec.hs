import Parse

import Test.Hspec
import Data.Time.Calendar

main :: IO ()
main = hspec $ do
  describe "Parse.totalCents" $ do
    it "parses the numbers as cents" $ do
      totalCents "15,99€"  `shouldBe` Right (1599 :: Int)
      totalCents "-15,99€" `shouldBe` Right (-1599 :: Int)

    it "handles error conditions" $ do
      totalCents ""       `shouldBe` Left  "empty input"
      totalCents "15.99€" `shouldBe` Left  "Input contained no ','"

  describe "Parse.purchaseDate" $ do
    it "parses dates" $ do
      purchaseDate "28 Feb, 2016" `shouldBe` Right (fromGregorian 2016 2 28)
      purchaseDate "4 Feb, 2016"  `shouldBe` Right (fromGregorian 2016 2 4)
      purchaseDate "30 May, 2014" `shouldBe` Right (fromGregorian 2014 5 30)
      purchaseDate "bzzzzt"       `shouldBe` Left "Unable to parse 'bzzzzt' as date"
