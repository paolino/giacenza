{-# LANGUAGE DerivingStrategies #-}

module Types where

import Data.String (String)
import Data.Time (Day)
import Protolude
import Protolude.Base (Show (..))
import Streaming.Cassava (CsvParseException)
import Data.Aeson (ToJSON (..))

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
    deriving (Show, Eq)

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

data Analysis
    = FileAbsent
    | NotDone
    | Configured Config
    | Failed Failure Config
    | Success Result Config
    deriving (Eq, Show)

instance ToJSON Analysis where
    toJSON = \case
        FileAbsent -> "FileAbsent"
        NotDone -> "NotDone"
        Configured _ -> "Configured"
        Failed _ _ -> "Failed"
        Success _ _ -> "Success"

newtype Failure = ParsingOfFileFailed CsvParseException
    deriving (Eq, Show)

newtype FileName = FileName Text
    deriving (Eq, Ord, Show, IsString, ToJSON)

type Analyzer = Config -> FilePath -> IO (Either Failure Result)

newtype Randomness = Randomness Text

newtype DownloadPath = DownloadPath FilePath
    deriving (Eq, Show, IsString)

newtype UploadPath = UploadPath FilePath
    deriving (Eq, Show, IsString)

newtype StoragePath = StoragePath FilePath
    deriving (Eq, Show, IsString)
