module Main (main) where

import BankRecord (BankRecordB (BankRecordB), bankRecordP)
import Barbies.Bare (bstrip)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Extra (usVerboseDayP)
import Kitchen qualified
import Relude
import Test.Hspec (describe, hspec, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.Megaparsec.Char qualified as MP
import Text.Megaparsec.Extra (parsePretty)

main :: IO ()
main = hspec $ do
  describe "BankRecord" $ do
    it "parses with the default recipe" $ do
      parsePretty bankRecordP "test" "2022-01-01,Buy"
        `shouldBe` Right (BankRecordB (fromGregorian 2022 1 1) "Buy")
    it "parsers with a custom recipe" $ do
      parsePretty (fmap bstrip . Kitchen.cook $ BankRecordB usVerboseDayP (MP.string "Buy")) "test" "May 4, 2023,Buy"
        `shouldBe` Right (BankRecordB (fromGregorian 2023 5 4) "Buy")
