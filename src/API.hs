{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module API where

import Protolude hiding (Handler)

import Compute
    ( analyzeData
    , collectResult
    , parseCSV
    , readCSVFile
    )
import Data.Aeson (ToJSON (..), object)
import Data.Map.Strict qualified as Map
import Data.String (String)
import Form
    ( Feedback (..)
    , HTML
    , Page (..)
    , RawHtml
    , page
    )
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setLogger
    , setPort
    )
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import Servant (Context (..), FromHttpApiData (..), serveWithContext, (:<|>) (..), (:>))
import Servant.API
    ( Get
    , JSON
    , NoFraming
    , OctetStream
    , Post
    , QueryParam'
    , Required
    , StreamBody
    )
import Servant.Multipart
    ( FileData (fdFileName, fdPayload)
    , FromMultipart (..)
    , MultipartData
    , MultipartForm
    , MultipartOptions (generalOptions)
    , Tmp
    , defaultMultipartOptions
    , lookupFile
    , lookupInput
    )
import Servant.Server (Application, Handler (..), Server, ServerError (..), err406)
import Streaming (MFunctor (hoist))
import Streaming.ByteString (ByteStream)
import Streaming.Cassava (CsvParseException)
import Streaming.Servant ()
import Types
    ( Config (Config, amountField, dateField, numberFormat)
    , Giacenza (Giacenza)
    , NumberFormatKnown (..)
    , Result (..)
    , Saldo (Saldo)
    , Value (Value)
    , Year (Year)
    )

type API =
    "deposit"
        :> QueryParam' '[Required] "date-name" Text
        :> QueryParam' '[Required] "amount-name" Text
        :> QueryParam' '[Required] "number-format" NumberFormatKnown
        :> StreamBody NoFraming OctetStream (ByteStream IO ()) -- (SourceIO BS.ByteString)
        :> Post '[JSON] Result
        :<|> "deposit"
            :> "form"
            -- :> QueryParam' '[Required] "year" Integer
            -- :> QueryParam' '[Required] "date-name" Text
            -- :> QueryParam' '[Required] "amount-name" Text
            -- :> QueryParam' '[Required] "number-format" NumberFormat
            :> MultipartForm Tmp GiacenzaInput
            :> Post '[HTML] RawHtml
        :<|> Get '[HTML] RawHtml
        :<|> "about" :> Get '[HTML] RawHtml

-- MultipartData consists in textual inputs,
-- accessible through its "inputs" field, as well
-- as files, accessible through its "files" field.

instance ToJSON Result where
    toJSON (Result m) = toJSON $ do
        (Year y, (Saldo (Value s), Giacenza (Value g))) <- Map.toList m
        return
            ( y
            , object
                [ "giacenza" .= toJSON g
                , "balance" .= toJSON s
                ]
            )
      where
        (.=) = (,)

data GiacenzaInput = GiacenzaInput
    { dateField :: Text
    , amountField :: Text
    , numberFormat :: NumberFormatKnown
    , clientFilename :: Text
    , path :: FilePath
    }
instance FromMultipart Tmp GiacenzaInput where
    fromMultipart :: MultipartData Tmp -> Either String GiacenzaInput
    fromMultipart multipartData =
        GiacenzaInput
            <$> lookupInput "date-name" multipartData
            <*> lookupInput "amount-name" multipartData
            <*> do
                lookupInput "number-format" multipartData
                    >>= parseNumberFormat
            <*> do
                fdFileName <$> lookupFile "filename" multipartData
            <*> do
                fdPayload <$> lookupFile "filename" multipartData

parseNumberFormat :: Text -> Either String NumberFormatKnown
parseNumberFormat = \case
    "european" -> Right European
    "american" -> Right American
    _ -> Left "Invalid number format"

parseYear :: Text -> Either String Year
parseYear = fmap Year . readEither . toS

server :: Server API
server =
    giacenza
        :<|> form
        :<|> getForm
        :<|> pure (page About)
  where
    giacenza dateField amountField numberFormat body =
        Handler
            $ withExceptT convertCsvParseException
            $ collectResult
            $ analyzeData
            $ parseCSV
                Config{..}
            $ hoist liftToCsvException body
    form GiacenzaInput{..} = Handler $ do
        result <-
            liftIO
                $ runExceptT
                $ readCSVFile
                    (Config numberFormat dateField amountField)
                    path
                    collectResult
        pure
            $ case result of
                Right m -> page $ Positive Feedback{..} m
                Left exc -> page $ Negative Feedback{..} $ show exc

    getForm = pure $ page Home

convertCsvParseException :: CsvParseException -> ServerError
convertCsvParseException exc = err406{errBody = "CSV parse error:" <> show exc}

liftToCsvException :: IO a -> ExceptT CsvParseException IO a
liftToCsvException = lift

myApi :: Proxy API
myApi = Proxy

app :: Application
app =
    let
        size10MB = 10_000_000
        multipartOpts =
            (defaultMultipartOptions (Proxy :: Proxy Tmp))
                { generalOptions =
                    setMaxRequestFileSize
                        size10MB
                        defaultParseRequestBodyOptions
                }
        context = multipartOpts :. EmptyContext
     in
        serveWithContext myApi context server

main :: IO ()
main = withStdoutLogger $ \aplogger -> do
    let settings = setPort 8_080 $ setLogger aplogger defaultSettings
    runSettings settings app

instance FromHttpApiData NumberFormatKnown where
    parseUrlPiece "european" = Right European
    parseUrlPiece "american" = Right American
    parseUrlPiece _ = Left "Invalid number format"
