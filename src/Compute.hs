{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Compute where

import Control.Foldl qualified as L
import Control.Monad (fail)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Csv (Field, FromField (..), NamedRecord, (.:))
import Data.Csv qualified as CSV
import Data.HashMap.Strict qualified as HM
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.String (String)
import Data.Text qualified as T
import Data.Time.Calendar (isLeapYear)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Lens (Day, getL, setL)
import Data.Time.Lens qualified as L
import Numeric (showFFloat)
import Protolude
import Streaming
    ( MFunctor (hoist)
    , Of (..)
    , Stream
    , distribute
    , wrap
    )
import Streaming.ByteString (ByteStream)
import Streaming.Cassava (CsvParseException, defaultDecodeOptions)
import Streaming.Cassava qualified as SC
import Streaming.NonEmpty (NEStream (NEStream))
import Streaming.NonEmpty qualified as NES
import Streaming.Prelude qualified as S
import Streaming.With (withBinaryFileContents)
import Types
    ( Config (Config)
    , Giacenza (Giacenza)
    , Movement (Movement)
    , NumberFormat (..)
    , Result (..)
    , Saldo (Saldo)
    , Value (Value)
    , Year (Year)
    , numberFormatOf
    )

parseValue :: NumberFormat -> ByteString -> Either String Value
parseValue NumberFormat{..} = parseOnly $ do
    v <- optional $ char '-'
    d <- foldl' (\w t -> w * 1000 + t) 0 <$> euros
    void $ char decimalSeparator
    c <- decimal @Int
    pure
        $ Value
        $ (if isJust v then negate else identity) (fromIntegral d + fromIntegral c / 100)
  where
    euros :: A.Parser [Int]
    euros = do
        d <- decimal
        r <- optional $ char thousandsSeparator
        case r of
            Nothing -> pure [d]
            Just _ -> (d :) <$> euros

parseWithEithers :: (t -> Either String b) -> t -> CSV.Parser b
parseWithEithers p x = case p x of
    Left e -> fail e
    Right v -> pure v

parseNR :: (ByteString -> CSV.Parser a) -> NamedRecord -> Field -> CSV.Parser a
parseNR p nr f = case HM.lookup f nr of
    Nothing -> fail $ "Field " <> show f <> " not found"
    Just v -> p v

instance FromField Day where
    parseField b =
        maybe
            mzero
            pure
            (parseTimeM False defaultTimeLocale "%Y-%m-%d" (toS $ decodeUtf8 b))

parseNamedRecord' :: Config -> NamedRecord -> CSV.Parser Movement
parseNamedRecord' (Config nf (encodeUtf8 -> dateField) (encodeUtf8 -> amountField)) m =
    Movement
        <$> m
        .: dateField
        <*> parseNR (parseWithEithers $ parseValue $ numberFormatOf nf) m amountField

{- giacenza
    :: Config
    -> FilePath
    -> Year
    -> IO (Of (Saldo Value, Giacenza Value) Int)
giacenza cfg dir year = foldResults $ analyzeDir cfg dir year -}

foldResults
    :: (Monad m, Num a, Num b) => Stream (Of (a, b)) m r -> m (Of (a, b) r)
foldResults = S.fold (\(s, n) (s', n') -> (s + s', n + n')) (0, 0) identity

analyzeData
    :: (Monad m)
    => Stream (Of Movement) m r
    -> Stream
        (Of (Year, Giacenza Value))
        (Stream (Of (Year, Saldo Value)) m)
        r
analyzeData =
    giacenzas
        . saldos
        . foldDays

parseCSV
    :: (MonadError SC.CsvParseException m)
    => Config
    -> ByteStream m r
    -> Stream (Of Movement) m r
parseCSV cfg = SC.decodeByNameWithP (parseNamedRecord' cfg) defaultDecodeOptions

program :: Config -> FilePath -> ExceptT CsvParseException IO Result
program cfg fp = readCSVFile cfg fp collectResult

readCSVFile
    :: (ExceptT SC.CsvParseException IO ~ m)
    => Config
    -> FilePath
    -> ( Stream
            (Of (Year, Giacenza Value))
            (Stream (Of (Year, Saldo Value)) m)
            ()
         -> m r
       )
    -> m r
readCSVFile cfg filePath f =
    withBinaryFileContents filePath
        $ f . analyzeData . parseCSV cfg

showEuro :: Value -> Text
showEuro (Value v) = T.pack (showFFloat (Just 2) v "") <> " €"

collectResults :: (Num a1, Num b) => Of (Maybe (a2, a1)) (Maybe b) -> (a1, b)
collectResults (ms :> mg) = (maybe 0 snd ms, fromMaybe 0 mg)

yearOfDay :: Day -> Year
yearOfDay = Year . fst . toOrdinalDate

isLastYearDay :: Day -> Bool
isLastYearDay d =
    getL L.month d == 12 && getL L.day d == 31

saldos
    :: (Monad m, Num b, Fractional b)
    => Stream (Of (Day, b)) m r
    -> Stream (Of (Day, b)) (Stream (Of (Year, Saldo b)) m) r
saldos s = S.for
    (hoist lift s)
    $ \(d, v) -> do
        when (isLastYearDay d)
            $ lift
            $ S.yield
                (Year $ getL L.year d, Saldo v)
        S.yield (d, v)

logS :: (MonadIO m, Show a) => Stream (Of a) m r -> Stream (Of a) m r
logS = S.store S.print

foldDays
    :: (Monad m)
    => S.Stream (Of Movement) m r
    -> S.Stream (Of (Day, Value)) m r
foldDays s = do
    (r, last) <- flip runStateT Nothing
        . distribute
        . S.for (hoist lift s)
        $ \(Movement day differ) -> do
            st <- get
            case st of
                Nothing -> put $ Just (day, differ)
                Just (day', value) -> do
                    let value' = value + differ
                    put $ Just (day, value')
                    forM_ [day' .. pred day] $ \d -> S.yield (d, value)
    maybe
        do pure ()
        do \(day, value) -> S.each [(d, value) | d <- [day .. lastDayOfYear day]]
        do last
    pure r

lastDayOfYear :: Day -> Day
lastDayOfYear = setL L.month 12 . setL L.day 31

giacenzas
    :: (Monad m, Num b, Fractional b)
    => Stream (Of (Day, b)) m r
    -> Stream (Of (Year, Giacenza b)) m r
giacenzas = S.mapped g . NES.groupBy ((==) `on` fst) . S.map (first q)
  where
    q (toOrdinalDate -> (y, _)) = y
    g (NEStream s'@((y, _) :> _)) =
        fmap (first $ (Year y,) . Giacenza)
            $ L.purely S.fold ((/ days y) <$> L.sum)
            $ S.map snd
            $ wrap s'
    days y = if isLeapYear y then 366 else 365

collectResult
    :: Stream
        (Of (Year, Giacenza Value))
        (Stream (Of (Year, Saldo Value)) (ExceptT CsvParseException IO))
        ()
    -> ExceptT CsvParseException IO Result
collectResult = fmap f . S.toList . S.toList_
  where
    f :: Of [(Year, Saldo Value)] [(Year, Giacenza Value)] -> Result
    f (s :> g) =
        Result
            $ Map.merge
                Map.dropMissing
                Map.dropMissing
                (Map.zipWithMatched $ \_k x y -> (x, y))
                (Map.fromList s)
                (Map.fromList g)
