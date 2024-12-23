module Header (readCSVHeader) where

import Protolude

import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Char8 (hGetLine)
import Data.Csv.Parser (header)
import System.IO (withBinaryFile)
import Types (Failure (HeaderParseException), Header)

readCSVHeader :: Header
readCSVHeader fp = do
    withBinaryFile fp ReadMode $ \h -> do
        x <- hGetLine h
        let r = parseOnly (header $ fromIntegral $ ord ',') (x <> "\n")
        pure $ bimap HeaderParseException (fmap decodeUtf8 . toList) r
