{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Logic.Serve (serveStateHtml, StateHtml, mkSynchronicResponder) where

import Compute (analyzer)
import Control.Concurrent.STM (TVar, readTVarIO, writeTVar)
import Data.List (lookup)
import Data.String (String)
import Logic.Interpreter.Synchronous (ServerState, StateConfig, SynchronicState, interpretProductionEffects)
import Logic.Program (addFileP, analyzeFileP, configureFileP, deleteFileP, listFilesP, resetFileP)
import Pages.Types (HTML, Page (ListFiles), RawHtml)
import Polysemy (Sem)
import Protolude hiding (Handler)
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
    , MultipartData
    , MultipartForm
    , Tmp
    , lookupFile
    )
import Types (Config, Cookie (..), CookieGen, DownloadPath (..), FileName (..))
import Web.Cookie
    ( Cookies
    , SetCookie (setCookieName, setCookieValue)
    , defaultSetCookie
    , parseCookies
    )

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

instance FromMultipart Tmp AddFileS where
    fromMultipart :: MultipartData Tmp -> Either String AddFileS
    fromMultipart multipartData =
        AddFileS
            <$> do
                FileName . fdFileName <$> lookupFile "filename" multipartData
            <*> do
                DownloadPath . fdPayload <$> lookupFile "filename" multipartData
type CookieResponseHtml
    (v :: [Type] -> Type -> Type) =
    v '[HTML] (CookieResponse RawHtml)

type StateHtml' =
    -- Form to upload a file
    --  List of files
    "all" :> CookieResponseHtml Get
        -- Add a file
        :<|> MultipartForm Tmp AddFileS
            :> CookieResponseHtml Post
        -- Configure a file
        :<|> "configure"
            :> QueryParam "filename" FileName
            :> ReqBody '[FormUrlEncoded] Config
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

serveStateHtml :: (Maybe FileName -> Page -> RawHtml) -> Responder RawHtml -> Server StateHtml
serveStateHtml mkPage respond' =
    respondListFiles
        :<|> respondAddFile
        :<|> respondConfigureFile
        :<|> respondAnalyzeFile
        :<|> respondReconfigureFile
        :<|> respondDeleteFile
  where
    listFilesPage focus = listFilesP <&> mkPage focus . ListFiles
    respondListFiles mc = respond' mc $ listFilesPage Nothing
    respondAddFile mc (AddFileS fn dp) = respond' mc $ do
        addFileP fn dp
        listFilesPage $ Just fn
    respondConfigureFile mc (Just fn) cfg' = respond' mc $ do
        configureFileP fn cfg'
        analyzeFileP @IOException fn
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

mkSynchronicResponder
    :: TVar (CookieGen, ServerState)
    -- ^ State variable
    -> StateConfig
    -- ^ configuration
    -> Responder a
mkSynchronicResponder stateVar stateConfig mSetCookie f = do
    -- read the state
    (cg, oldState) <- liftIO $ readTVarIO stateVar
    -- extract the session cookie
    let mCookie = do
            Cookies' setCookie <- mSetCookie
            cookie <- lookup "session" setCookie
            pure $ Cookie $ decodeUtf8 cookie
    -- run the computation
    ((newcookie, cg'), r) <-
        liftIO
            $ interpretProductionEffects
                stateConfig
                cg
                oldState
                mCookie
                analyzer
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
    liftIO $ atomically $ writeTVar stateVar (cg', newState)
    pure outputWithCookie
  where
    mkCookies :: Cookie -> SetCookie
    mkCookies (Cookie cookie) =
        defaultSetCookie
            { setCookieName = "session"
            , setCookieValue = encodeUtf8 cookie
            }
