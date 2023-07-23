{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module State where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Polysemy
import Polysemy.State
import Protolude (Applicative (pure), Eq, FilePath, Map, Maybe (..), Monad (..), Monoid (..), Ord, Text, ($), (.))
import Servant.Multipart (Mem)
import Types (Result)

data Analysis = NotDone | Failed Failure | Success Result

data Failure = ParsingOfFileFailed | AnalysisFailed

newtype FileName = FileName Text
    deriving (Eq, Ord)

data SessionE :: Effect where
    GetFiles :: SessionE m [FileName]
    GetFile :: FileName -> SessionE m Analysis
    UpdateTime :: SessionE m ()
    GetTime :: SessionE m UTCTime
    AddFile :: FilePath -> SessionE m FileName
    SetAnalysis :: FileName -> Analysis -> SessionE m ()
    DeleteFile :: FileName -> SessionE m ()

makeSem ''SessionE

newtype Cookie = Cookie Text
    deriving (Eq, Ord)

data StateE :: Effect where
    GetSession :: Cookie -> StateE m Cookie
    NewSession :: StateE m Cookie
    DeleteSession :: Cookie -> StateE m ()

makeSem ''StateE

data TimeE :: Effect where
    GetCurrentTime :: TimeE m UTCTime

makeSem ''TimeE


data RecoverR :: Effect where
    NoSession :: RecoverR m b

makeSem ''RecoverR

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

addFileP
    :: (Member SessionE effs, Member StateE effs)
    => Cookie
    -> FilePath
    -> Sem effs (Cookie, FileName)
addFileP cookie path = do
    cookie' <- getSession cookie
    fileName <- addFile path
    pure (cookie', fileName)

getFilesP
    :: (Member StateE r, Member SessionE r)
    => Cookie
    -> Sem r (Cookie, [FileName])
getFilesP cookie = do
    cookie' <- getSession cookie
    files <- getFiles
    pure (cookie', files)

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
                 in do  time <- getCurrentTime
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
         in do  time <- getCurrentTime
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
    :: (Member TimeE r)
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
        do \_ SessionState{files} -> pure $ Map.keys files
    GetFile fileName -> onLocalSession
        do \_ SessionState{files} -> pure $ files Map.! fileName
    UpdateTime -> onLocalSession
        do
            \p s -> do
                time <- getCurrentTime
                p $ s {time = time}
    GetTime -> onLocalSession
        do \_ SessionState{time} -> pure time
    AddFile path -> onLocalSession
        do
            \p s -> pure undefined