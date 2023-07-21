{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import API (parseNumberFormat)
import Compute
    ( program
    , showEuro
    )
import Data.Map.Strict qualified as Map
import Protolude
import Turtle
    ( Parser
    , arg
    , argPath
    , argText
    , options
    )
import Types
    ( Config (Config)
    , Giacenza (Giacenza)
    , NumberFormatKnown
    , Result (Result)
    , Saldo (Saldo)
    , Year (..)
    )
import Data.String (String)

parser :: Parser (FilePath, Text, Text, NumberFormatKnown)
parser =
    (,,,)
        <$> argPath "csv" "The csv file"
        <*> argText "date-name" "The date field name"
        <*> argText "amount-name" "The amount field name"
        <*> arg
            (either (const Nothing) pure . parseNumberFormat)
            "number-format"
            "The number format, european or american"

main :: IO ()
main = do
    (file, dateField, amountField, numberFormat) <- options "Giacenza media" parser
    r <- runExceptT $ program (Config numberFormat dateField amountField) file
    case r of
        Left e -> print e
        Right (Result m) -> sequence_ $ do
            (Year year, (Saldo s, Giacenza g)) <- Map.assocs m
            pure $ do
                putText "\n--------------------------"
                putText $ tabulate $ 15 ./. "Year" $ 15 ./. show year $ emptyTabulation
                putText $ tabulate $ 15 ./. "Giacenza media:" $ 15 ./. showEuro g $ emptyTabulation
                putText $ tabulate $ 15 ./. "Saldo:" $ 15 ./. showEuro s $ emptyTabulation

newtype Tabulation = Tabulation {unTabulation :: [(Int, Text)]}

emptyTabulation :: Tabulation
emptyTabulation = Tabulation []

(./.) :: Int -> Text -> Tabulation -> Tabulation
(./.) n t (Tabulation ts) = Tabulation $ (n, t) : ts

tabulate :: Tabulation -> Text
tabulate (Tabulation []) = ""
tabulate (Tabulation ((n, t) : ts)) =
    let l = length (toS t :: String)
     in toS (replicate (n - l) ' ')
            <> t
            <> tabulate (Tabulation ts)