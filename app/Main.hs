{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Compute
    ( program
    , showEuro
    )
import Configuration (parseOptions)
import Data.Map.Strict qualified as Map
import Data.String (String)
import Protolude
    ( Applicative (pure)
    , ConvertText (toS)
    , Either (Left, Right)
    , Foldable (length)
    , IO
    , Int
    , Num ((-))
    , Semigroup ((<>))
    , Text
    , print
    , putText
    , replicate
    , runExceptT
    , sequence_
    , show
    , ($)
    )
import Server (runServer)
import Turtle
    ( options
    )
import Types
    ( Config (Config)
    , Giacenza (Giacenza)
    , Result (Result)
    , Saldo (Saldo)
    , Year (..)
    )

main :: IO ()
main = do
    os <- options "Giacenza media" parseOptions
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
                        putText
                            $ tabulate
                            $ 15 ./. "Giacenza media:"
                            $ 15 ./. showEuro g
                            $ emptyTabulation
                        putText $ tabulate $ 15 ./. "Saldo:" $ 15 ./. showEuro s $ emptyTabulation
        Left (host, port, prefix) -> do
            putText $ "Starting server on " <> host <> ":" <> show port
            runServer prefix port $ toS host

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
