{-# LANGUAGE DerivingStrategies #-}

module Types where

import Data.String (String)
import Data.Time (Day)
import Protolude

newtype Value = Value {unValue :: Double}
    deriving (Num, Show, Fractional, Eq)

newtype Result = Result (Map Year (Saldo Value, Giacenza Value))
    deriving (Show, Eq)

data NumberFormatKnown = European | American
    deriving (Show, Eq)

numberFormatOf :: NumberFormatKnown -> NumberFormat
numberFormatOf European = NumberFormat ',' '.'
numberFormatOf American = NumberFormat '.' ','

data NumberFormat = NumberFormat
    { decimalSeparator :: Char
    , thousandsSeparator :: Char
    }

parseNumberFormat :: Text -> Either String NumberFormatKnown
parseNumberFormat = \case
    "european" -> Right European
    "american" -> Right American
    _ -> Left "Invalid number format"

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
