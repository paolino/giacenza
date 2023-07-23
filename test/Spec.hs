{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Compute (parseValue)
import Logic.Interpreter.Synchronous
    ( emptyServerState
    , SynchronicStateWithMockTime, runSynchronicStateWithMockTime
    )
import Logic.Invariants (addFileWithoutSessionProducesNoSession, getFileAfterAddFileProducesNotDone)
import Protolude
import Test.Hspec (describe, hspec, it, shouldBe)
import Types (Cookie (..), CookieGen (..), NumberFormat (NumberFormat))

sequentialCookieGen :: CookieGen
sequentialCookieGen = go 0
  where
    go :: Int -> CookieGen
    go n = CookieGen (Cookie (show n)) $ go (n + 1)



testSynchronicState :: t -> SynchronicStateWithMockTime t Bool -> IO ()
testSynchronicState t f = let
    r = runSynchronicStateWithMockTime
            do emptyServerState sequentialCookieGen
            do t
            do f
    in snd <$> r `shouldBe` Right True

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
    describe "synchronic state" $ do
        it "respects the afterAddFileGettingItProducesNotDone" $ do
            testSynchronicState ()
                getFileAfterAddFileProducesNotDone
        it "respects the afterAddFileWithoutSessionProducesNoSession" $ do
            testSynchronicState ()
                addFileWithoutSessionProducesNoSession
