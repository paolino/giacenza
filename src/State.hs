{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module State
    ( Analysis (..)
    , Failure (..)
    , FileName (..)
    , SessionE (..)
    , Cookie (..)
    , StateE (..)
    , TimeE (..)
    , RecoverR (..)
    , CookieGen (..)
    , ServerState (..)
    , SessionState (..)
    , consumeServerState
    , runSessionE
    , runServerE
    , onLocalSession
    , addFileP
    , getFilesP
    , memptySessionState
    ) where

import Protolude hiding (State, get, put, runState)

import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Time.Clock qualified as IO (getCurrentTime)
import Polysemy (Effect, Embed, Member, Sem, embed, interpret, makeSem, runM)
import Polysemy.Error
import Polysemy.State (State, get, put, runState)
import Types (Result)

----- StateE language ---------------------------------------------------------

newtype Cookie = Cookie Text
    deriving (Eq, Ord)

data StateE :: Effect where
    GetSession :: Cookie -> StateE m Cookie
    NewSession :: StateE m Cookie
    DeleteSession :: Cookie -> StateE m ()

makeSem ''StateE

----- SessionE language -------------------------------------------------------
data Analysis = NotDone | Failed Failure | Success Result

data Failure = ParsingOfFileFailed | AnalysisFailed

newtype FileName = FileName Int
    deriving (Eq, Ord)

data SessionE :: Effect where
    GetFiles :: SessionE m [FileName]
    GetFile :: FileName -> SessionE m Analysis
    UpdateTime :: SessionE m ()
    GetTime :: SessionE m UTCTime
    AddFile :: FilePath -> SessionE m FileName
    SetResult :: FileName -> Result -> SessionE m ()
    SetFailure :: FileName -> Failure -> SessionE m ()
    DeleteFile :: FileName -> SessionE m ()

makeSem ''SessionE

---- Public API of SessionE ----------------------------------------------------

withSession :: (Member StateE r) => Cookie -> Sem r a -> Sem r (Cookie, a)
withSession cookie f = do
    cookie' <- getSession cookie
    (cookie',) <$> f

withSession_ :: (Member StateE r) => Cookie -> Sem r a -> Sem r Cookie
withSession_ cookie f = do
    cookie' <- getSession cookie
    f $> cookie'

addFileP
    :: (Member SessionE effs, Member StateE effs)
    => Cookie
    -> FilePath
    -> Sem effs (Cookie, FileName)
addFileP cookie path = withSession cookie $ addFile path

getFilesP
    :: (Member StateE r, Member SessionE r)
    => Cookie
    -> Sem r (Cookie, [FileName])
getFilesP cookie = withSession cookie getFiles

getFileP
    :: (Member StateE r, Member SessionE r)
    => Cookie
    -> FileName
    -> Sem r (Cookie, Analysis)
getFileP cookie fileName = withSession cookie $ getFile fileName

getTimeP :: (Member StateE r, Member SessionE r) => Cookie -> Sem r Cookie
getTimeP cookie = withSession_ cookie getTime

setResultP
    :: (Member StateE r, Member SessionE r)
    => Cookie
    -> FileName
    -> Result
    -> Sem r Cookie
setResultP cookie fileName result =
    withSession_ cookie $ setResult fileName result

setFailureP
    :: (Member StateE r, Member SessionE r)
    => Cookie
    -> FileName
    -> Failure
    -> Sem r Cookie
setFailureP cookie fileName failure =
    withSession_ cookie $ setFailure fileName failure

deleteFileP
    :: (Member StateE r, Member SessionE r)
    => Cookie
    -> FileName
    -> Sem r Cookie
deleteFileP cookie fileName = withSession_ cookie $ deleteFile fileName

---- TimeE language -----------------------------------------------------------

data TimeE :: Effect where
    GetCurrentTime :: TimeE m UTCTime

makeSem ''TimeE

---- RecoverR language --------------------------------------------------------
data RecoverR :: Effect where
    NoSession :: RecoverR m b

makeSem ''RecoverR

---- Synchronic state implementation -------------------------------------------

newtype CookieGen = CookieGen (Cookie, CookieGen)

data ServerState = ServerState
    { sessions :: Map Cookie SessionState
    , cookies :: CookieGen
    , localSession :: Maybe Cookie
    }

data SessionState = SessionState
    { files :: Map FileName Analysis
    , time :: UTCTime
    }

memptySessionState :: UTCTime -> SessionState
memptySessionState = SessionState mempty
runServerE
    :: (Member (State ServerState) effs, Member TimeE effs)
    => Sem (StateE ': effs) a
    -> Sem effs a
runServerE = interpret $ \case
    GetSession cookie -> do
        ServerState{sessions, cookies} <- get
        case Map.lookup cookie sessions of
            Nothing -> do
                let CookieGen (cookie', f) = cookies
                 in do
                        time <- getCurrentTime
                        put
                            $ ServerState
                                (Map.insert cookie (memptySessionState time) sessions)
                                f
                                (Just cookie)
                        pure cookie'
            Just _ -> pure cookie
    NewSession -> do
        ServerState{sessions, cookies} <- get
        let CookieGen (cookie, f) = cookies
         in do
                time <- getCurrentTime
                put $ ServerState (Map.insert cookie (memptySessionState time) sessions) f (Just cookie)
                pure cookie
    DeleteSession cookie -> do
        ServerState{sessions, cookies, localSession} <- get
        case Map.lookup cookie sessions of
            Nothing -> pure ()
            Just _ -> do
                let sessions' = Map.delete cookie sessions
                put $ ServerState sessions' cookies localSession
        pure ()

consumeServerState
    :: (Member TimeE r, Member RecoverR r)
    => ServerState
    -> Sem (StateE : SessionE : State ServerState : r) a
    -> Sem r (ServerState, a)
consumeServerState s = runState s . runSessionE . runServerE

onLocalSession
    :: (Member (State ServerState) r, Member RecoverR r)
    => ((SessionState -> Sem r ()) -> SessionState -> Sem r b)
    -> Sem r b
onLocalSession g = do
    ServerState{sessions, cookies, localSession} <- get
    case localSession of
        Nothing -> noSession
        Just cookie -> g
            do \s -> put $ ServerState (Map.insert cookie s sessions) cookies localSession
            do sessions Map.! cookie

runSessionE
    :: (Member (State ServerState) effs, Member TimeE effs, Member RecoverR effs)
    => Sem (SessionE ': effs) a
    -> Sem effs a
runSessionE = interpret $ \case
    GetFiles -> onLocalSession
        $ \_ SessionState{files} -> pure $ Map.keys files
    GetFile fileName -> onLocalSession
        $ \_ SessionState{files} -> pure $ files Map.! fileName
    UpdateTime -> onLocalSession
        $ \put' s -> do
            time <- getCurrentTime
            put' $ s{time = time}
    GetTime -> onLocalSession
        $ \_ SessionState{time} -> pure time
    AddFile path -> onLocalSession
        $ \put' s -> do
            let fileName = FileName $ hash path
            put' $ s{files = Map.insert fileName NotDone $ files s}
            pure fileName
    SetResult fileName result -> onLocalSession
        $ \put' s -> do
            put' $ s{files = Map.insert fileName (Success result) $ files s}
    SetFailure fileName failure -> onLocalSession
        $ \put' s -> do
            put' $ s{files = Map.insert fileName (Failed failure) $ files s}
    DeleteFile fileName -> onLocalSession
        $ \put' s -> do
            put' $ s{files = Map.delete fileName $ files s}

runTimeIOE
    :: (Member (Embed IO) r)
    => Sem (TimeE : r) a
    -> Sem r a
runTimeIOE = interpret $ \case
    GetCurrentTime -> embed IO.getCurrentTime

data StateError = ErrNoSession

runRecoverR
    :: (Member (Error StateError) r)
    => Sem (RecoverR ': r) a
    -> Sem r a
runRecoverR = interpret $ \case
    NoSession -> throw ErrNoSession

runSynchronicState
    :: ServerState
    -> Sem
        '[ StateE
         , SessionE
         , State ServerState
         , TimeE
         , RecoverR
         , Error StateError
         , Embed IO
         ]
        a
    -> IO (Either StateError (ServerState, a))
runSynchronicState s =
    runM @IO
        . runError
        . runRecoverR
        . runTimeIOE
        . runState s
        . runSessionE
        . runServerE
