{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Lib (Year (Year), giacenza, showEuro, tabulate, (./.), emptyTabulation, NumberFormat(..))
import Streaming (Of ((:>)))
import Turtle
    ( Parser
    , argInteger
    , argPath
    , argText
    , arg 
    , options
    )

numberFormatP :: Text -> Maybe NumberFormat
numberFormatP "european" = Just $ NumberFormat ',' '.'
numberFormatP "american" = Just $ NumberFormat '.' ','
numberFormatP _ = Nothing

parser :: Parser (FilePath, Integer, Text, Text, NumberFormat)
parser =
    (,,,,)
        <$> argPath "dir" "The directory with the statements"
        <*> argInteger "year" "The year for the giacenza"
        <*> argText "date-name" "The date field name"
        <*> argText "amount-name" "The amount field name"
        <*> arg numberFormatP "number-format" "The number format, european or american"

main :: IO ()
main = do
    (dir, year, dateField, amountField, numberFormat) <- options "Giacenza media" parser
    (s,g) :> n <-
        giacenza
            do numberFormat
            do dir
            do encodeUtf8 dateField
            do encodeUtf8 amountField
            $ Year year
    putText "\n--------------------------"
    putText $ show n <> " files analyzed"
    putText $ tabulate $ 15 ./. "Year" $ 15 ./. show year $ emptyTabulation
    putText $ tabulate $ 15 ./. "Giacenza media:" $ 15 ./.  showEuro g $ emptyTabulation
    putText $ tabulate $ 15 ./. "Saldo:" $ 15 ./. showEuro s $ emptyTabulation
    
