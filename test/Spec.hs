{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Compute (parseValue)
import Logic.Interpreter.Synchronous
    ( ServerState
    , SessionState
    , runPureSessionState
    , runPureSynchronicState
    )
import Logic.Invariants
    ( createdSessionsAreUsable
    , deletedSessionsAreNotReusable
    , filesAreStored
    , sessionsAreResusable
    )
import Logic.Language (SessionE, StateSem)
import Polysemy (Sem)
import Polysemy.State (State)
import Protolude hiding (State)
import Test.Hspec (Expectation, describe, hspec, it, shouldBe)
import Types (Cookie (..), CookieGen (..), NumberFormat (NumberFormat))

sequentialCookieGen :: CookieGen
sequentialCookieGen = go 0
  where
    go :: Int -> CookieGen
    go n = CookieGen (Cookie (show n)) $ go (n + 1)

testSynchronicState
    :: StateSem SessionState '[State ServerState] Bool
    -> Expectation
testSynchronicState f =
    let r = runPureSynchronicState sequentialCookieGen f
    in  r `shouldBe` True

testPureSessionState :: Sem '[SessionE, State SessionState] Bool -> Expectation
testPureSessionState f =
    let r = runPureSessionState f
    in  r `shouldBe` True

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
    describe "pure session state" $ do
        it "respects the filesAreStored invariant" $ do
            testPureSessionState filesAreStored
    describe "synchronic state" $ do
        it "respects the afterAddFileGettingItProducesNotDone" $ do
            testSynchronicState createdSessionsAreUsable
        it "respect the sessionsAreResusable " $ do
            testSynchronicState sessionsAreResusable
        it "respects the deletedSessionsAreNotReusable" $ do
            testSynchronicState deletedSessionsAreNotReusable
