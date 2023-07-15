{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Lib (Year (Year), giacenza, showEuro, tabulate, (./.), emptyTabulation)
import Streaming (Of ((:>)))
import Turtle
    ( Parser
    , argInteger
    , argPath
    , argText
    , options
    )

parser :: Parser (FilePath, Integer, Text, Text)
parser =
    (,,,)
        <$> argPath "dir" "The directory with the statements"
        <*> argInteger "year" "The year for the giacenza"
        <*> argText "date-name" "The date field name"
        <*> argText "amount-name" "The amount field name"

main :: IO ()
main = do
    (dir, year, dateField, amountField) <- options "Giacenza media" parser
    (s,g) :> n <-
        giacenza
            dir
            do encodeUtf8 dateField
            do encodeUtf8 amountField
            $ Year year
    putText "\n--------------------------"
    putText $ show n <> " files analyzed"
    putText $ tabulate $ 15 ./. "Year" $ 15 ./. show year $ emptyTabulation
    putText $ tabulate $ 15 ./. "Giacenza media:" $ 15 ./.  showEuro g $ emptyTabulation
    putText $ tabulate $ 15 ./. "Saldo:" $ 15 ./. showEuro s $ emptyTabulation
    
