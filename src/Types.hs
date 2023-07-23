{-# LANGUAGE DerivingStrategies #-}

module Types where

import Data.String (String)
import Data.Time (Day)
import Protolude
import Protolude.Base (Show (..))

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
    deriving (Show, Eq)

newtype Giacenza x = Giacenza {unGiacenza :: x}
    deriving newtype (Num, Fractional, Show, Eq)

newtype Year = Year {unYear :: Integer} deriving (Eq, Show, Num, Ord)

newtype Saldo x = Saldo {unSaldo :: x}
    deriving newtype (Num, Fractional, Show, Eq)

newtype Cookie = Cookie Text
    deriving (Eq, Ord, Show)

data CookieGen = CookieGen Cookie CookieGen

instance Show CookieGen where
    show = const "CookieGen"

instance Eq CookieGen where
    _ == _ = True

data Analysis = NotDone | Failed Failure | Success Result
    deriving (Eq, Show)

data Failure = ParsingOfFileFailed | AnalysisFailed
    deriving (Eq, Show)

newtype FileName = FileName Int
    deriving (Eq, Ord, Show, Num)
