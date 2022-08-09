
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wunused-imports #-}


module Lib where

import Control.Foldl qualified as L
import Data.Csv (FromField (..), FromNamedRecord (..), (.:))
import Data.Streaming.Filesystem
  ( DirStream,
    openDirStream,
    readDirStream,
  )
import Data.String (String)
import Data.Time.Calendar (Day, isLeapYear)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Protolude hiding (state, diff)
import Streaming
  ( MFunctor (hoist),
    Of (..),
    Stream,
    distribute,
    wrap,
  )
import Streaming.Cassava qualified as SC
import Streaming.NonEmpty (NEStream (NEStream))
import Streaming.NonEmpty qualified as NES
import Streaming.Prelude qualified as S
import Streaming.With (withBinaryFileContents)
import System.FilePath ((</>))

newtype Value = Value {unValue :: Double} deriving (Num, Show, Fractional)

instance FromField Value where
  parseField b =
    Value <$> case reads (filter isGood $ toS $ decodeUtf8 b) of
      [(r, _)] -> pure r
      _ -> mzero
    where
    isGood :: Char -> Bool
    isGood x
      | isDigit x = True
      | x `elem` (".-" :: String) = True
      | otherwise = False

data Movement = Movement {date :: !Day, amount :: !Value}
  deriving (Show)

instance FromField Day where
  parseField b =
    maybe
      mzero
      pure
      (parseTimeM False defaultTimeLocale "%Y-%m-%d" (toS $ decodeUtf8 b))

instance FromNamedRecord Movement where
  parseNamedRecord m = Movement <$> m .: "Date" <*> m .: "Amount"

newtype Year = Year Integer deriving (Eq, Show, Num)

giacenza :: FilePath -> Year -> IO (Of Double Int)
giacenza dir year = fmap (first unValue) $ S.sum $ analyzeDir dir year

analyzeDir :: FilePath -> Year -> Stream (Of Value) IO Int
analyzeDir dir year = do
  d <- liftIO $ openDirStream dir
  analyzeFile year $ S.store S.length_ $ S.store S.print $ S.unfoldr r d
  where
    r :: DirStream -> IO (Either () (FilePath, DirStream))
    r d = do
      mf <- readDirStream d
      pure $ maybe (Left ()) (Right . (,d) . (dir </>)) mf

analyzeFile :: Year -> Stream (Of FilePath) IO r -> Stream (Of Value) IO r
analyzeFile year filePaths = S.for filePaths $ \filePath -> do
  r <-
    liftIO $
      runExceptT $
        withBinaryFileContents filePath $
          S.print . S.store (S.head_  . S.map snd) . S.filter ((== year) . fst)
            -- . S.store S.print
            . groupYears
            . foldDays
            . SC.decodeByName
  either
    do panic . show
    do maybe (pure ()) S.yield
    r

logS :: (MonadIO m, Show a) => Stream (Of a) m r -> Stream (Of a) m r
logS = S.store S.print

foldDays ::
  (Monad m) =>
  S.Stream (Of Movement) m r ->
  S.Stream (Of (Day, Value)) m r
foldDays s = do
  (r, last) <- flip runStateT Nothing . distribute
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

groupYears ::
  (Monad m, Num b, Fractional b) =>
  Stream (Of (Day, b)) m r ->
  Stream (Of (Year, b)) m r
groupYears = S.mapped g . NES.groupBy ((==) `on` fst) . S.map (first q)
  where
    q (toOrdinalDate -> (y, _)) = y
    g (NEStream s'@((y, _) :> _)) =
      fmap (first (Year y,)) $
        L.purely S.fold ((/ days y) <$> L.sum) $ S.map snd $ wrap s'
    days y = if isLeapYear y then 366 else 365