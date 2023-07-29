{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}

module Logic.Interpreter.Synchronous
    ( ServerState (..)
    , emptyServerState
    , SessionState (..)
    , emptySessionState

      -- * pure interpreters
    , runPureSynchronicState
    , runPureSessionState
    , runRecoverR
    , runFileStorageE
    , interpretProductionEffects
    , StateConfig (..)
    , SynchronicState
    , SynchronicStateBase
    )
where

import Control.Exception qualified as E
import Data.ByteString qualified as B (readFile, writeFile)
import Data.Map qualified as Map
import Logic.Language
    ( AnalyzerE (..)
    , ConfigE (..)
    , FileStorageE (..)
    , GetCookieE (..)
    , RecoverR (..)
    , SessionE (..)
    , StateE (..)
    , StateError (..)
    , StateSem
    , getConfiguration
    , getStoragePath
    , storageOperationFailure
    )
import Polysemy
    ( Embed
    , Inspector (..)
    , Member
    , Members
    , Sem
    , embed
    , getInspectorT
    , interpret
    , interpretH
    , pureT
    , raise
    , raiseUnder
    , reinterpret
    , run
    , runM
    , runT
    )
import Polysemy.Error (Error, runError, throw, try)
import Polysemy.State (State, evalState, get, put, runState)
import Protolude hiding (State, evalState, get, put, runState, try)
import System.Directory (removeFile)
import System.FilePath ((</>))
import Types
    ( Analysis (..)
    , CSVLayer (..)
    , Cookie (..)
    , CookieGen (..)
    , DownloadPath (..)
    , FileName (..)
    , StoragePath (..)
    )

newtype ServerState = ServerState
    { sessions :: Map Cookie SessionState
    }
    deriving (Eq, Show)

emptyServerState :: ServerState
emptyServerState = ServerState mempty

newtype SessionState = SessionState
    { files :: Map FileName Analysis
    }
    deriving (Eq, Show)

emptySessionState :: SessionState
emptySessionState = SessionState mempty

runStateE
    :: forall e effs a
     . Members '[RecoverR e] effs
    => StateSem effs a
    -> Sem (State ServerState ': effs) a
runStateE = reinterpret $ \case
    DeleteSession cookie -> do
        ServerState{sessions} <- get
        let sessions' = Map.delete cookie sessions
        put $ ServerState sessions'
    WithSession cookie q -> do
        ServerState{sessions} <- get
        let session = fromMaybe emptySessionState (Map.lookup cookie sessions)
        (session', x) <-
            raise
                . runState session
                . runSessionE @e
                . raiseUnder
                $ q
        put $ ServerState (Map.insert cookie session' sessions)
        pure x

runSessionE
    :: forall e effs a
     . (Member (State SessionState) effs, Member (RecoverR e) effs)
    => Sem (SessionE ': effs) a
    -> Sem effs a
runSessionE = interpret $ \case
    GetFiles -> do
        SessionState{files} <- get
        pure $ Map.keys files
    GetFile fileName -> do
        SessionState{files} <- get
        pure $ fromMaybe FileAbsent $ Map.lookup fileName files
    AddFile fileName -> do
        SessionState{files} <- get
        put $ SessionState $ Map.insert fileName NotDone files
    SetConfig fileName config -> do
        SessionState{files} <- get
        put $ SessionState $ Map.insert fileName (Configured config) files
    SetResult fileName result -> do
        SessionState{files} <- get
        configuration <- runSessionE @e $ getConfiguration @e fileName
        put
            $ SessionState
            $ Map.insert fileName (Success result configuration) files
    SetFailure fileName failure -> do
        configuration <- runSessionE @e $ getConfiguration @e fileName
        SessionState{files} <- get
        put
            $ SessionState
            $ Map.insert fileName (Failed failure configuration) files
    DeleteFile fileName -> do
        SessionState{files} <- get
        put $ SessionState $ Map.delete fileName files

runPureSynchronicState
    :: forall e a
     . StateSem '[RecoverR e, Error (StateError e)] a
    -> Either (StateError e) a
runPureSynchronicState =
    run
        . runError
        . runRecoverR
        . evalState emptyServerState
        . runStateE @e

runPureSessionState
    :: forall e a
     . Sem
        '[ SessionE
         , State SessionState
         , RecoverR e
         , Error (StateError e)
         ]
        a
    -> Either (StateError e) a
runPureSessionState =
    run
        . runError
        . runRecoverR
        . evalState emptySessionState
        . runSessionE @e

runFileStorageE
    :: Members '[Embed IO, ConfigE, RecoverR IOException] r
    => Sem (FileStorageE : r) a
    -> Sem r a
runFileStorageE = interpret $ \case
    GetFilePath name -> do
        tmp <- getStoragePath
        pure $ storagePath tmp name
    PutFilePath name (DownloadPath origin) -> do
        tmp <- getStoragePath
        let StoragePath path = storagePath tmp name
        embed $ B.writeFile path =<< B.readFile origin
    DeleteFilePath name -> do
        tmp <- getStoragePath
        let StoragePath path = storagePath tmp name
        result <- embed $ E.try @E.IOException $ removeFile path
        case result of
            Left (e :: IOException) -> storageOperationFailure name e
            Right r -> pure r

storagePath :: StoragePath -> FileName -> StoragePath
storagePath (StoragePath path) (FileName fn) = StoragePath $ path </> show fn

runRecoverR
    :: forall e r a
     . Member (Error (StateError e)) r
    => Sem (RecoverR e : r) a
    -> Sem r a
runRecoverR = interpretH $ \case
    StorageOperationFailure fileName e -> throw $ ErrStorageOperation fileName e
    FileNotConfigured fileName ->
        throw @(StateError e)
            $ ErrFileNotConfigured fileName
    NoSession -> throw @(StateError e) ErrNoSession
    Recover m -> do
        m' <- runT m
        r <- raise $ runRecoverR $ try m'
        case r of
            Left e -> pureT $ Left e
            Right r' -> do
                i <- getInspectorT
                case inspect i r' of
                    Nothing -> panic "runRecoverR: impossible"
                    Just r'' -> pureT $ Right r''

newtype StateConfig = StateConfig
    { configStoragePath :: StoragePath
    }

runConfigE :: StateConfig -> Sem (ConfigE : r) a -> Sem r a
runConfigE StateConfig{configStoragePath} =
    interpret $ \case
        GetStoragePath -> pure configStoragePath

runGetCookieE
    :: Sem (GetCookieE : r) a
    -> Sem (State (Maybe Cookie, CookieGen) ': r) a
runGetCookieE = reinterpret $ \case
    GetCookie -> do
        (mcookie, CookieGen cookie' f) <- get
        case mcookie of
            Nothing -> do
                put (Just cookie', f)
                pure cookie'
            Just cookie -> do
                pure cookie

runAnalyzeE :: Member (Embed IO) r => CSVLayer -> Sem (AnalyzerE : r) a -> Sem r a
runAnalyzeE (CSVLayer analyzer header) = interpret $ \case
    Analyze (StoragePath path) cfg -> embed $ analyzer cfg path
    Header (StoragePath path) -> embed $ header path

type SynchronicStateBase =
    [ FileStorageE
    , RecoverR IOException
    , Error (StateError IOException)
    , AnalyzerE
    , GetCookieE
    , ConfigE
    , Embed IO
    ]
type SynchronicState = StateE SynchronicStateBase : SynchronicStateBase

interpretProductionEffects
    :: forall a
     . StateConfig
    -> CookieGen
    -> ServerState
    -> Maybe Cookie
    -> CSVLayer
    -> Sem SynchronicState a
    -> IO ((Maybe Cookie, CookieGen), Either (StateError IOException) (ServerState, a))
interpretProductionEffects cfg cg serverState mcookie csvLayer =
    runM
        . runConfigE cfg
        . runState (mcookie, cg)
        . runGetCookieE
        . runAnalyzeE csvLayer
        . runError
        . runRecoverR
        . runFileStorageE
        . runState serverState
        . runStateE @IOException
