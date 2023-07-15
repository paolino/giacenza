{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wunused-imports #-}

module Lib where

import Protolude

import Control.Foldl qualified as L
import Control.Monad (fail)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Csv (Field, FromField (..), NamedRecord, (.:))
import Data.Csv qualified as CSV
import Data.Streaming.Filesystem
    ( DirStream
    , openDirStream
    , readDirStream
    )
import Data.String (String)
import Data.Text qualified as T
import Data.Time.Calendar (isLeapYear)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.Lens (Day, getL, setL)
import Data.Time.Lens qualified as L
import Numeric (showFFloat)
import Streaming
    ( MFunctor (hoist)
    , Of (..)
    , Stream
    , distribute
    , wrap
    )
import Streaming.Cassava (defaultDecodeOptions)
import Streaming.Cassava qualified as SC
import Streaming.NonEmpty (NEStream (NEStream))
import Streaming.NonEmpty qualified as NES
import Streaming.Prelude qualified as S
import Streaming.With (withBinaryFileContents)
import System.FilePath ((</>))

newtype Value = Value {unValue :: Double} deriving (Num, Show, Fractional)

parseValue :: ByteString -> Either String Value
parseValue = parseOnly $ do
    v <- optional $ char '-'
    d <- foldl' (\w t -> w * 1000 + t) 0 <$> euros
    void $ char ','
    c <- decimal @Int
    pure $ Value $ (if isJust v then negate else identity) (fromIntegral d + fromIntegral c / 100)
  where
    euros :: A.Parser [Int]
    euros = do
        d <- decimal
        r <- optional $ char '.'
        case r of
            Nothing -> pure [d]
            Just _ -> (d :) <$> euros
instance FromField Value where
    parseField b = case parseValue b of
        Left e -> fail e
        Right v -> pure v

data Movement = Movement {date :: !Day, amount :: !Value}
    deriving (Show)

instance FromField Day where
    parseField b =
        maybe
            mzero
            pure
            (parseTimeM False defaultTimeLocale "%Y-%m-%d" (toS $ decodeUtf8 b))

parseNamedRecord'
    :: ByteString
    -> ByteString
    -> NamedRecord
    -> CSV.Parser Movement
parseNamedRecord' dateField amountField m = Movement <$> m .: dateField <*> m .: amountField

newtype Year = Year {unYear :: Integer} deriving (Eq, Show, Num)

giacenza :: FilePath -> Field -> Field -> Year -> IO (Of (Value, Value) Int)
giacenza dir dateField amountField year =
    foldResults
        $ analyzeDir dir dateField amountField year

foldResults :: (Monad m, Num a, Num b) => Stream (Of (a, b)) m r -> m (Of (a, b) r)
foldResults = S.fold (\(s, n) (s', n') -> (s + s', n + n')) (0, 0) identity

analyzeDir :: FilePath -> Field -> Field -> Year -> Stream (Of (Value, Value)) IO Int
analyzeDir dir dateField amountField year = do
    d <- liftIO $ openDirStream dir
    analyzeFile
        dateField
        amountField
        year
        $ S.store S.length_
        $ S.chain putStrLn
        $ S.unfoldr r d
  where
    r :: DirStream -> IO (Either () (FilePath, DirStream))
    r d = do
        mf <- readDirStream d
        pure $ maybe (Left ()) (Right . (,d) . (dir </>)) mf

analyzeFile :: Field -> Field -> Year -> Stream (Of FilePath) IO r -> Stream (Of (Value, Value)) IO r
analyzeFile dateField amountField year filePaths = S.for filePaths $ \filePath -> do
    r :: Either a (Value, Value) <-
        liftIO
            $ runExceptT
            $ withBinaryFileContents filePath
            $ fmap collectResults
                . S.head
                . S.chain (\(d, v) -> putStrLn $ tabulate $ 15 ./. "saldo:" $ 12 ./. show d $ 15 ./. showEuro v $ emptyTabulation)
                . S.filter ((==) year . yearOfDay . fst)
                -- . S.chain (\(d, v) -> putStrLn $ tabulate $ 15 ./. "saldo:" $ 12 ./. show d $ 15 ./. showEuro v $ emptyTabulation)
                . S.head_
                . S.map snd
                . S.chain (\(Year y, v) -> putStrLn $ tabulate $ 15 ./. "giacenza:" $ 12 ./. show y $ 15 ./. showEuro v $ emptyTabulation)
                . S.filter ((== year) . fst)
                -- S.chain (\(Year y, v) -> putStrLn $ tabulate $ 15 ./. "giacenza:" $ 12 ./. show y $ 15 ./. showEuro v $ emptyTabulation)
                . groupYears
                . saldos
                . foldDays (setL L.month 12 $ setL L.day 31 $ fromOrdinalDate (unYear year) 1)
                . SC.decodeByNameWithP
                    (parseNamedRecord' dateField amountField)
                    defaultDecodeOptions
    either
        do panic . show
        do S.yield
        r

showEuro :: Value -> Text
showEuro (Value v) = T.pack (showFFloat (Just 2) v "") <> " €"

newtype Tabulation = Tabulation {unTabulation :: [(Int, Text)]}

emptyTabulation :: Tabulation
emptyTabulation = Tabulation []

(./.) :: Int -> Text -> Tabulation -> Tabulation
(./.) n t (Tabulation ts) = Tabulation $ (n, t) : ts

tabulate :: Tabulation -> Text
tabulate (Tabulation []) = ""
tabulate (Tabulation ((n, t) : ts)) =
    let l = T.length t
     in T.pack (replicate (n - l) ' ')
            <> t
            <> tabulate (Tabulation ts)

collectResults :: Of (Maybe (Day, Value)) (Maybe Value) -> (Value, Value)
collectResults (ms :> mg) = (maybe 0 snd ms, fromMaybe 0 mg)

yearOfDay :: Day -> Year
yearOfDay = Year . fst . toOrdinalDate

isLastYearDay :: Day -> Bool
isLastYearDay d =
    getL L.month d == 12 && getL L.day d == 31

saldos :: (Monad m, Num b, Fractional b) => Stream (Of (Day, b)) m r -> Stream (Of (Day, b)) (Stream (Of (Day, b)) m) r
saldos s = S.for
    (hoist lift s)
    $ \(d, v) -> do
        when (isLastYearDay d) $ lift $ S.yield (d, v)
        S.yield (d, v)

logS :: (MonadIO m, Show a) => Stream (Of a) m r -> Stream (Of a) m r
logS = S.store S.print

foldDays
    :: (Monad m)
    => Day -- top
    -> S.Stream (Of Movement) m r
    -> S.Stream (Of (Day, Value)) m r
foldDays top s = do
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
        (pure ())
        do
            \(day, value) ->
                S.each [(d, value) | d <- [day .. top]]
        do last
    pure r

groupYears
    :: (Monad m, Num b, Fractional b)
    => Stream (Of (Day, b)) m r
    -> Stream (Of (Year, b)) m r
groupYears = S.mapped g . NES.groupBy ((==) `on` fst) . S.map (first q)
  where
    q (toOrdinalDate -> (y, _)) = y
    g (NEStream s'@((y, _) :> _)) =
        fmap (first (Year y,))
            $ L.purely S.fold ((/ days y) <$> L.sum)
            $ S.map snd
            $ wrap s'
    days y = if isLeapYear y then 366 else 365