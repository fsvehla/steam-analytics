import Parse

import Test.Hspec

main :: IO ()
main = hspec $
  describe "Parse.totalCents" $ do
    it "parses the numbers as cents" $ do
      totalCents "15,99€"  `shouldBe` Right (1599 :: Int)
      totalCents "-15,99€" `shouldBe` Right (-1599 :: Int)

    it "handles error conditions" $ do
      totalCents ""       `shouldBe` Left  "empty input"
      totalCents "15.99€" `shouldBe` Left  "Input contained no ','"
