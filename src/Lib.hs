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
import Data.Time.Calendar (Day, isLeapYear)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
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

newtype Year = Year Integer deriving (Eq, Show, Num)

giacenza :: FilePath -> Field -> Field -> Year -> IO (Of Double Int)
giacenza dir dateField amountField year =
    fmap (first unValue)
        $ S.sum
        $ analyzeDir dir dateField amountField year

analyzeDir :: FilePath -> Field -> Field -> Year -> Stream (Of Value) IO Int
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

analyzeFile :: Field -> Field -> Year -> Stream (Of FilePath) IO r -> Stream (Of Value) IO r
analyzeFile dateField amountField year filePaths = S.for filePaths $ \filePath -> do
    r <-
        liftIO
            $ runExceptT
            $ withBinaryFileContents filePath
            $ S.head_ 
                . S.map snd
                . S.chain (\(_, Value v) -> putText $ "  -> " <> show v)
                . S.filter ((== year) . fst)
                . groupYears
                . foldDays
                . SC.decodeByNameWithP
                    (parseNamedRecord' dateField amountField)
                    defaultDecodeOptions
    either
        do panic . show
        do maybe (pure ()) S.yield
        r

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
        $ \(Movement day diff) -> do
            state <- get
            case state of
                Nothing -> put $ Just (day, diff)
                Just (day', value) -> do
                    let value' = value + diff
                    put $ Just (day, value')
                    forM_ [day' .. pred day] $ \d -> S.yield (d, value)
    maybe (pure ()) S.yield last
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