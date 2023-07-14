{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import Lib (Year (Year), giacenza)
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
    v :> n <-
        giacenza
            dir
            do encodeUtf8 dateField
            do encodeUtf8 amountField
            $ Year year
    putText "\n--------------------------"
    putText $ show n <> " files analyzed"
    putText $ "Giacenza media: " <> show v
    
