{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Protolude
    ( Char
    , Double
    , Eq
    , Fractional
    , Integer
    , Map
    , Num
    , Ord
    , Show
    , Text
    )

import Data.Time (Day)

newtype Value = Value {unValue :: Double}
    deriving (Num, Show, Fractional)

newtype Result = Result (Map Year (Saldo Value, Giacenza Value))

data NumberFormatKnown = European | American
    deriving (Show, Eq)

numberFormatOf :: NumberFormatKnown -> NumberFormat
numberFormatOf European = NumberFormat ',' '.'
numberFormatOf American = NumberFormat '.' ','

data NumberFormat = NumberFormat
    { decimalSeparator :: Char
    , thousandsSeparator :: Char
    }

data Config = Config
    { numberFormat :: !NumberFormatKnown
    , dateField :: Text
    , amountField :: Text
    }

data Movement = Movement {date :: Day, amount :: Value}
    deriving (Show)

newtype Giacenza x = Giacenza {unGiacenza :: x}
    deriving newtype (Num, Fractional, Show, Eq)

newtype Year = Year {unYear :: Integer} deriving (Eq, Show, Num, Ord)

newtype Saldo x = Saldo {unSaldo :: x}
    deriving newtype (Num, Fractional, Show, Eq)
