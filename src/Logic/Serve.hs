{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Logic.Serve (serveStateHtml, StateHtml, mkSynchronicResponder) where

import Compute (analyzer)
import Control.Concurrent.STM (TVar, readTVarIO, writeTVar)
import Data.List (lookup)
import Data.Map qualified as Map (fromList)
import Data.String (String)
import Header (readCSVHeader)
import Logic.Interpreter.Synchronous (ServerState, StateConfig, SynchronicState, WebState, interpretProductionEffects)
import Logic.Language (getConfiguration, storeOldConfig)
import Logic.Program (addFileP, analyzeFileP, configureFileP, deleteAllFilesP, deleteFileP, getOldConfigurations, listFilesP, notDonesOrFaileds, reconfigureAllFilesP, resetFileP, sumsP, withCurrentSession)
import Pages.Types (HTML, Page (ListFiles), RawHtml)
import Polysemy (Sem)
import Protolude hiding (Handler, State, get, gets, modify, put)
import Servant
    ( FormUrlEncoded
    , Get
    , Handler
    , Header
    , Headers
    , Post
    , QueryParam
    , ReqBody
    , Server
    , ServerError (errBody)
    , addHeader
    , err500
    , (:<|>) (..)
    , type (:>)
    )
import Servant.API (FromHttpApiData (..))
import Servant.Multipart
    ( FileData (fdFileName, fdPayload)
    , FromMultipart (..)
    , MultipartData (..)
    , MultipartForm
    , Tmp
    )
import Types
    ( CSVLayer (CSVLayer)
    , Config (..)
    , Cookie (..)
    , CookieGen
    , DownloadPath (..)
    , FileName (..)
    , NumberFormatKnown (..)
    , Result
    )
import Web.Cookie
    ( Cookies
    , SetCookie (setCookieName, setCookieValue)
    , defaultSetCookie
    , parseCookies
    )
import Web.FormUrlEncoded (FromForm (..), parseUnique)

type CookieResponse a = Headers '[Header "Set-Cookie" SetCookie] a

type CookieRequest = Header "Cookie" Cookies'

newtype Cookies' = Cookies' Cookies -- type Cookies = [(BS.ByteString, BS.ByteString)]

instance FromHttpApiData Cookies' where
    parseHeader = return . Cookies' . parseCookies
    parseQueryParam = return . Cookies' . parseCookies . encodeUtf8

data AddFileS = AddFileS
    { _clientFilename :: FileName
    , _path :: DownloadPath
    }

instance FromMultipart Tmp [AddFileS] where
    fromMultipart :: MultipartData Tmp -> Either String [AddFileS]
    fromMultipart multipartData =
        forM (files multipartData)
            $ \fd -> pure
                $ AddFileS
                    do (FileName . fdFileName) fd
                    do (DownloadPath . fdPayload) fd
type CookieResponseHtml
    (v :: [Type] -> Type -> Type) =
    v '[HTML] (CookieResponse RawHtml)

data ConfigAndPropagate = ConfigAndPropagate
    { _config :: Config
    , _propagate :: Bool
    }
    deriving (Eq, Show)

instance FromHttpApiData NumberFormatKnown where
    parseUrlPiece "european" = Right European
    parseUrlPiece "american" = Right American
    parseUrlPiece _ = Left "Invalid number format"

instance FromForm ConfigAndPropagate where
    fromForm form = do
        ConfigAndPropagate
            <$> do
                nf <- parseUnique "number-format" form
                dateField <- parseUnique "date-name" form
                amountField <- parseUnique "amount-name" form
                pure $ Config nf dateField amountField
            <*> do
                r <- parseUnique @Text "propagate" form
                case r of
                    "on" -> pure True
                    "off" -> pure False
                    _ -> Left "Invalid propagate value"

type StateHtml' =
    -- Form to upload a file
    --  List of files
    "all" :> CookieResponseHtml Get
        -- Add a file
        :<|> MultipartForm Tmp [AddFileS]
            :> CookieResponseHtml Post
        -- Configure a file
        :<|> "configure"
            :> QueryParam "filename" FileName
            :> ReqBody '[FormUrlEncoded] ConfigAndPropagate
            :> CookieResponseHtml Post
        -- Analyze a file
        :<|> "analyze"
            :> QueryParam "filename" FileName
            :> CookieResponseHtml Post
        :<|> "reconfigure"
            :> QueryParam "filename" FileName
            :> CookieResponseHtml Post
        :<|> "delete"
            :> QueryParam "filename" FileName
            :> CookieResponseHtml Post
        :<|> "delete-all"
            :> CookieResponseHtml Post
        :<|> "reconfigure-all"
            :> CookieResponseHtml Post

type family Prepend f xs where
    Prepend q (f :<|> g) = (q :> f) :<|> Prepend q g
    Prepend q f = q :> f

type StateHtml = Prepend "file" (Prepend CookieRequest StateHtml')

type Responder a =
    Maybe Cookies'
    -- ^ incoming cookies
    -> Sem SynchronicState a
    -- ^ computation over the state
    -> Handler (CookieResponse a)

serveStateHtml
    :: (Maybe FileName -> Map FileName Config -> Result -> Page -> RawHtml)
    -> Responder RawHtml
    -> Server StateHtml
serveStateHtml mkPage respond' =
    respondListFiles
        :<|> respondAddFiles
        :<|> respondConfigureFile
        :<|> respondAnalyzeFile
        :<|> respondReconfigureFile
        :<|> respondDeleteFile
        :<|> respondDeleteAllFiles
        :<|> respondReconfigureAllFiles
  where
    listFilesPage focus = do
        cfgs <- Map.fromList <$> getOldConfigurations
        sums <- sumsP
        listFilesP <&> mkPage focus cfgs sums . ListFiles
    respondListFiles mc = respond' mc $ do
        listFilesPage Nothing
    respondAddFiles mc afs = respond' mc $ do
        forM_ afs $ \(AddFileS fn dp) -> do
            addFileP fn dp
        listFilesPage $ listToMaybe $ _clientFilename <$> afs
    respondConfigureFile
        mc
        (Just fn)
        (ConfigAndPropagate cfg' propagate) = respond' mc $ do
            targets <- if propagate then notDonesOrFaileds else pure [fn]
            forM_ targets $ \fn' -> do
                configureFileP fn' cfg'
                analyzeFileP @IOException fn'
                cfg <- withCurrentSession $ getConfiguration @IOException fn'
                storeOldConfig fn' cfg
            listFilesPage $ Just fn
    respondConfigureFile mc Nothing _ = respond' mc do
        listFilesPage Nothing
    respondAnalyzeFile mc (Just fn) = respond' mc $ do
        analyzeFileP @IOException fn
        listFilesPage $ Just fn
    respondAnalyzeFile mc Nothing = respond' mc do
        listFilesPage Nothing
    respondReconfigureFile mc (Just fn) = respond' mc $ do
        resetFileP fn
        listFilesPage $ Just fn
    respondReconfigureFile mc Nothing = respond' mc do
        listFilesPage Nothing
    respondDeleteFile mc (Just fn) = respond' mc $ do
        deleteFileP fn
        listFilesPage Nothing
    respondDeleteFile mc Nothing = respond' mc do
        listFilesPage Nothing
    respondDeleteAllFiles mc = respond' mc $ do
        deleteAllFilesP
        listFilesPage Nothing
    respondReconfigureAllFiles mc = respond' mc $ do
        reconfigureAllFilesP
        listFilesPage Nothing
mkSynchronicResponder
    :: TVar (CookieGen, ServerState, WebState)
    -- ^ State variable
    -> StateConfig
    -- ^ configuration
    -> Responder a
mkSynchronicResponder stateVar stateConfig mSetCookie f = do
    -- read the state
    (cg, oldState, oldWebState) <- liftIO $ readTVarIO stateVar
    -- extract the session cookie
    let mCookie = do
            Cookies' setCookie <- mSetCookie
            cookie <- lookup "session" setCookie
            pure $ Cookie $ decodeUtf8 cookie
    -- run the computation
    (webState', ((newcookie, cg'), r)) <-
        liftIO
            $ interpretProductionEffects
                stateConfig
                cg
                oldState
                oldWebState
                mCookie
                do CSVLayer analyzer readCSVHeader
                f
    -- handle the result
    (newState, outputWithCookie) <- case r of
        Left e -> throwError $ err500{errBody = show e} -- should be handled
        Right (newState, output) -> do
            case newcookie of
                Just cookie ->
                    pure (newState, addHeader (mkCookies cookie) output)
                Nothing ->
                    panic "respondP: no cookie generated"
    -- write the new state
    liftIO $ atomically $ writeTVar stateVar (cg', newState, webState')
    pure outputWithCookie
  where
    mkCookies :: Cookie -> SetCookie
    mkCookies (Cookie cookie) =
        defaultSetCookie
            { setCookieName = "session"
            , setCookieValue = encodeUtf8 cookie
            }
