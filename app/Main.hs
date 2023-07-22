{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import API (main, parseNumberFormat)
import Compute
    ( program
    , showEuro
    )
import Data.Map.Strict qualified as Map
import Data.String (String)
import Turtle
    ( Parser
    , arg
    , argInt
    , argPath
    , argText
    , options
    , subcommand
    )
import Types
    ( Config (Config)
    , Giacenza (Giacenza)
    , NumberFormatKnown
    , Result (Result)
    , Saldo (Saldo)
    , Year (..)
    )

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

parserServe :: Parser (Text, Int, Text)
parserServe =
    subcommand "serve" "Start the web server"
        $ (,,)
            <$> argText "host" "The host to bind to"
            <*> argInt "port" "The port to bind to"
            <*> argText "prefix" "The prefix to add to the http calls"

main :: IO ()
main = do
    os <- options "Giacenza media" (fmap Left parserServe <|> fmap Right parser)

    case os of
        Right (file, dateField, amountField, numberFormat) -> do
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
        Left (host, port, prefix) -> do
            putText $ "Starting server on " <> host <> ":" <> show port
            API.main prefix port $ toS host

newtype Tabulation = Tabulation {unTabulation :: [(Int, Text)]}

emptyTabulation :: Tabulation
emptyTabulation = Tabulation []

(./.) :: Int -> Text -> Tabulation -> Tabulation
(./.) n t (Tabulation ts) = Tabulation $ (n, t) : ts

tabulate :: Tabulation -> Text
tabulate (Tabulation []) = ""
tabulate (Tabulation ((n, t) : ts)) =
    let l = length (toS t :: String)
    in  toS (replicate (n - l) ' ')
            <> t
            <> tabulate (Tabulation ts)
