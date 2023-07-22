{-# LANGUAGE OverloadedStrings #-}

import Lib (NumberFormat (NumberFormat), parseValue)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "parsing values" $ do
        it "parses a positive value" $ do
            parseValue (NumberFormat ',' '.') "1.234,56"
                `shouldBe` Right 1234.56
        it "parses a negative value" $ do
            parseValue (NumberFormat ',' '.') "-1.234,56"
                `shouldBe` Right (-1234.56)
        it "parses a value with no decimals" $ do
            parseValue (NumberFormat ',' '.') "1.234"
                `shouldBe` Right 1234
        it "parses a value with no decimals and no thousands separator" $ do
            parseValue (NumberFormat ',' '.') "1234"
                `shouldBe` Right 1234
        it "parses a positive value in american format" $ do
            parseValue (NumberFormat '.' ',') "1,234.56"
                `shouldBe` Right 1234.56
        it "parses a negative value in american format" $ do
            parseValue (NumberFormat '.' ',') "-1,234.56"
                `shouldBe` Right (-1234.56)
    describe "parding dates"
