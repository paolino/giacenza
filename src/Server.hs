{-# OPTIONS_GHC -Wno-orphans #-}

module Server (runServer) where

import Compute
    ( analyzeData
    , collectResult
    , parseCSV
    , readCSVFile
    )
import Data.Aeson (ToJSON (..), object)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..), String)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setHost
    , setLogger
    , setPort
    )
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import Pages.Page qualified as Page
import Pages.Types
    ( Feedback (..)
    , HTML
    , Page (..)
    , RawHtml
    )
import Protolude hiding (Handler)
import Servant
    ( Context (..)
    , FromHttpApiData (..)
    , Header
    , Headers
    , addHeader
    , serveWithContext
    , (:<|>) (..)
    , (:>)
    )
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
import Servant.Server
    ( Application
    , Handler (..)
    , Server
    , ServerError (..)
    , err406
    )
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
    , parseNumberFormat
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
        :<|> Get '[HTML] (Headers '[Header "Set-Cookie" Text] RawHtml)
        :<|> "about" :> Get '[HTML] RawHtml

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

server :: Text -> Server API
server prefix =
    giacenza
        :<|> form
        :<|> getForm
        :<|> pure (page' About)
  where
    page' = Page.page prefix
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
                Right m -> page' $ Positive Feedback{..} m
                Left exc -> page' $ Negative Feedback{..} $ show exc

    getForm = pure $ addHeader (giacenzaCookie <> "=" <> cookie) $ page' Home

cookie :: Text
cookie = "true"

giacenzaCookie :: Text
giacenzaCookie = "giacenza"

convertCsvParseException :: CsvParseException -> ServerError
convertCsvParseException exc = err406{errBody = "CSV parse error:" <> show exc}

liftToCsvException :: IO a -> ExceptT CsvParseException IO a
liftToCsvException = lift

myApi :: Proxy API
myApi = Proxy

app :: Text -> Application
app prefix =
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
        serveWithContext myApi context $ server prefix

runServer
    :: Text
    -- ^ prefix
    -> Int
    -- ^ port
    -> String
    -- ^ host
    -> IO ()
runServer prefix port host = withStdoutLogger $ \aplogger -> do
    let settings =
            setPort port
                $ setHost (fromString host)
                $ setLogger aplogger defaultSettings
    runSettings settings $ app prefix

instance FromHttpApiData NumberFormatKnown where
    parseUrlPiece "european" = Right European
    parseUrlPiece "american" = Right American
    parseUrlPiece _ = Left "Invalid number format"
