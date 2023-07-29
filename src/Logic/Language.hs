{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Logic.Language
    ( SessionE (..)
    , StateE (..)
    , FileStorageE (..)
    , RecoverR (..)
    , StateError (..)
    , GetCookieE (..)
    , AnalyzerE (..)
    , ConfigE (..)
    , deleteSession
    , withSession
    , getFiles
    , getFile
    , addFile
    , setResult
    , setFailure
    , deleteFile
    , noSession
    , recover
    , getFilePath
    , putFilePath
    , deleteFilePath
    , analyze
    , getCookie
    , setConfig
    , StateSem
    , getStoragePath
    , storageOperationFailure
    , fileNotConfigured
    , getConfiguration
    , header
    )
where

import Polysemy (Member, Sem, makeSem)
import Polysemy.Internal.Kind (Effect)
import Protolude hiding (State, get, put, runState)
import Types (Analysis (..), Config, Cookie, DownloadPath, Failure, FileName, Result, StoragePath)

----- SessionE language -------------------------------------------------------

data SessionE :: Effect where
    GetFiles :: SessionE m [FileName]
    GetFile :: FileName -> SessionE m Analysis
    AddFile :: FileName -> SessionE m ()
    SetConfig :: FileName -> Config -> SessionE m ()
    SetResult :: FileName -> Result -> SessionE m ()
    SetFailure :: FileName -> Failure -> SessionE m ()
    DeleteFile :: FileName -> SessionE m ()

makeSem ''SessionE

data FileStorageE :: Effect where
    GetFilePath :: FileName -> FileStorageE m StoragePath
    PutFilePath :: FileName -> DownloadPath -> FileStorageE m ()
    DeleteFilePath :: FileName -> FileStorageE m ()

makeSem ''FileStorageE

data AnalyzerE :: Effect where
    Analyze :: StoragePath -> Config -> AnalyzerE m (Either Failure Result)
    Header :: StoragePath -> AnalyzerE m (Either Failure [Text])

makeSem ''AnalyzerE

data GetCookieE :: Effect where
    GetCookie :: GetCookieE m Cookie

makeSem ''GetCookieE

----- StateE language ---------------------------------------------------------

data StateE e :: Effect where
    DeleteSession :: Cookie -> StateE e m ()
    WithSession :: Cookie -> Sem (SessionE ': e) a -> StateE e m a

-- this is somehow limiting, and doesn't help much in disambiguition

type StateSem effs = Sem (StateE effs ': effs)

makeSem ''StateE

---- RecoverR language --------------------------------------------------------

data StateError e
    = ErrNoSession
    | ErrStorageOperation FileName e
    | ErrFileNotConfigured FileName
    deriving (Eq, Show)

data RecoverR e :: Effect where
    NoSession :: RecoverR e m b
    StorageOperationFailure :: FileName -> e -> RecoverR e m b
    FileNotConfigured :: FileName -> RecoverR e m b
    Recover :: m a -> RecoverR e m (Either (StateError e) a)

makeSem ''RecoverR

data ConfigE :: Effect where
    GetStoragePath :: ConfigE m StoragePath

makeSem ''ConfigE

getConfiguration
    :: forall e r
     . (Member (RecoverR e) r, Member SessionE r)
    => FileName
    -> Sem r Config
getConfiguration fileName = do
    status <- getFile fileName
    case status of
        Configured config -> pure config
        Success _ config -> pure config
        Failed _ config -> pure config
        _ -> fileNotConfigured @e fileName
