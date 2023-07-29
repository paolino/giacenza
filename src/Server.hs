{-# OPTIONS_GHC -Wno-orphans #-}

module Server (runServer) where

import Control.Concurrent.STM (TVar, newTVarIO)
import Data.String (IsString (..), String)
import Data.Text (pack)
import Logic.Interpreter.Synchronous (ServerState, StateConfig (..), emptyServerState)
import Logic.Serve (StateHtml, mkSynchronicResponder, serveStateHtml)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setHost
    , setLogger
    , setPort
    )
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize, setMaxRequestNumFiles)
import Pages.Page qualified as Page
import Pages.Types
    ( HTML
    , Page (..)
    , RawHtml
    )
import Protolude hiding (Handler)
import Servant
    ( Context (..)
    , FromHttpApiData (..)
    , serveWithContext
    , (:<|>) (..)
    )
import Servant.API
    ( Get
    )
import Servant.Multipart
    ( MultipartOptions (generalOptions)
    , Tmp
    , defaultMultipartOptions
    )
import Servant.Server
    ( Application
    )
import Streaming.Servant ()
import System.Random (StdGen, getStdGen, randomRs, split)
import Types
    ( Config (..)
    , Cookie (..)
    , CookieGen (..)
    , FileName (..)
    , NumberFormatKnown (..)
    , StoragePath (..)
    )
import Web.Internal.FormUrlEncoded (FromForm (..), parseUnique)

type API =
    Get '[HTML] RawHtml
        :<|> StateHtml

type StateVar = TVar (CookieGen, ServerState)

app :: StateVar -> Text -> Application
app stateVar prefix =
    serveWithContext (Proxy @API) context $ about :<|> stateFulStuff
  where
    about = pure $ page' Nothing mempty About
    stateFulStuff = serveStateHtml
        do page'
        do
            mkSynchronicResponder
                do stateVar
                do StateConfig $ StoragePath "."
    page' focus mcfg = Page.page focus mcfg prefix

context :: Context '[MultipartOptions Tmp]
context = multipartOpts :. EmptyContext
  where
    size10MB = 10_000_000
    multipartOpts =
        (defaultMultipartOptions (Proxy @Tmp))
            { generalOptions =
                setMaxRequestNumFiles 50
                    $ setMaxRequestFileSize
                        size10MB
                        defaultParseRequestBodyOptions
            }
cookieGen :: StdGen -> CookieGen
cookieGen = fix \go seed ->
    CookieGen (mkCookie seed) $ go $ snd $ split seed
  where
    mkCookie = Cookie . pack . take 16 . randomRs ('a', 'z')

runServer
    :: Text
    -- ^ prefix
    -> Int
    -- ^ port
    -> String
    -- ^ host
    -> IO ()
runServer prefix port host = do
    cookieG <- getStdGen <&> cookieGen
    stateVar <- newTVarIO (cookieG, emptyServerState)
    withStdoutLogger $ \aplogger -> do
        let settings =
                setPort port
                    $ setHost (fromString host)
                    $ setLogger aplogger defaultSettings
        runSettings settings $ app stateVar prefix

instance FromHttpApiData NumberFormatKnown where
    parseUrlPiece "european" = Right European
    parseUrlPiece "american" = Right American
    parseUrlPiece _ = Left "Invalid number format"

instance FromHttpApiData FileName where
    parseUrlPiece = Right . FileName

instance FromForm Config where
    fromForm f = do
        nf <- parseUnique "number-format" f
        dateField <- parseUnique "date-name" f
        amountField <- parseUnique "amount-name" f
        pure $ Config nf dateField amountField
