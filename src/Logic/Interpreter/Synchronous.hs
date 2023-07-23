{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Logic.Interpreter.Synchronous
    ( ServerState (..)
    , emptyServerState
    , SessionState (..)
    , emptySessionState
    , runSynchronicState
    , SynchronicState
    , SynchronicStateWithMockTime
    , runSynchronicStateWithMockTime
    , setTime
    , MockTime
    )
where

import Data.Map qualified as Map
import Data.Time.Clock (UTCTime)
import Data.Time.Clock qualified as IO (getCurrentTime)
import Logic.Language
    ( RecoverR (..)
    , SessionE (..)
    , SessionTimeE (..)
    , StateE (..)
    , StateError (..)
    , TimeE (..)
    , getCurrentTime
    , noSession
    )
import Polysemy
    ( Effect
    , Embed
    , Member
    , Sem
    , embed
    , interpret
    , interpretH
    , makeSem
    , pureT
    , raise
    , run
    , runM
    , runT
    )
import Polysemy.Error (Error, runError, throw, try)
import Polysemy.Internal.Kind (Append)
import Polysemy.Internal.Tactics (Inspector (..), getInspectorT)
import Polysemy.State (State, evalState, get, put, runState)
import Protolude hiding (State, evalState, get, put, runState, try)
import Types (Analysis (..), Cookie (..), CookieGen (..), FileName (..))

data ServerState t = ServerState
    { sessions :: Map Cookie (SessionState t)
    , cookies :: CookieGen
    , localSession :: Maybe Cookie
    }
    deriving (Eq, Show)

emptyServerState :: CookieGen -> ServerState t
emptyServerState cg = ServerState mempty cg Nothing

data SessionState t = SessionState
    { files :: Map FileName Analysis
    , time :: t
    }
    deriving (Eq, Show)

emptySessionState :: t -> SessionState t
emptySessionState = SessionState mempty

runServerE
    :: forall t effs a
     . (Member (State (ServerState t)) effs, Member (TimeE t) effs)
    => Sem (StateE ': effs) a
    -> Sem effs a
runServerE = interpret $ \case
    GetSession cookie -> do
        ServerState{sessions, cookies = CookieGen cookie' cookieGen'} <- get
        case Map.lookup cookie sessions of
            Nothing -> do
                time <- getCurrentTime @t
                put
                    $ ServerState
                        do Map.insert cookie (emptySessionState time) sessions
                        do cookieGen'
                        do Just cookie
                pure cookie'
            Just _ -> pure cookie
    NewSession -> do
        ServerState{sessions, cookies = CookieGen cookie cookieGen'} <- get
        time <- getCurrentTime @t
        put $ ServerState
            do Map.insert cookie (emptySessionState time) sessions
            do cookieGen'
            do Just cookie
        pure cookie
    DeleteSession cookie -> do
        ServerState{sessions, cookies, localSession} <-
            get @(ServerState t)
        case Map.lookup cookie sessions of
            Nothing -> pure ()
            Just _ -> do
                let sessions' = Map.delete cookie sessions
                put $ ServerState sessions' cookies localSession

onLocalSession
    :: (Member (State (ServerState t)) r, Member RecoverR r)
    => ((SessionState t -> Sem r ()) -> SessionState t -> Sem r b)
    -> Sem r b
onLocalSession g = do
    ServerState{sessions, cookies, localSession} <- get
    case localSession of
        Nothing -> noSession
        Just cookie -> g
            do
                \s -> put
                    $ ServerState
                        do Map.insert cookie s sessions
                        do cookies
                        do localSession
            do sessions Map.! cookie

runSessionE
    :: forall t effs a
     . ( Member (State (ServerState t)) effs
       , Member RecoverR effs
       )
    => Sem (SessionE ': effs) a
    -> Sem effs a
runSessionE = interpret $ \case
    GetFiles -> onLocalSession @t
        $ \_ SessionState{files} -> pure $ Map.keys files
    GetFile fileName -> onLocalSession @t
        $ \_ SessionState{files} -> pure $ files Map.! fileName
    AddFile path -> onLocalSession @t
        $ \put' s -> do
            let fileName = FileName $ hash path
            put' $ s{files = Map.insert fileName NotDone $ files s}
            pure fileName
    SetResult fileName result -> onLocalSession @t
        $ \put' s -> do
            put' $ s{files = Map.insert fileName (Success result) $ files s}
    SetFailure fileName failure -> onLocalSession @t
        $ \put' s -> do
            put' $ s{files = Map.insert fileName (Failed failure) $ files s}
    DeleteFile fileName -> onLocalSession @t
        $ \put' s -> do
            put' $ s{files = Map.delete fileName $ files s}

runSessionTimeE
    :: forall t effs a
     . ( Member (State (ServerState t)) effs
       , Member RecoverR effs
       )
    => Sem (SessionTimeE t : effs) a
    -> Sem effs a
runSessionTimeE = interpret $ \case
    UpdateTime t -> onLocalSession
        $ \put' s -> do
            put' $ s{time = t}
    GetTime -> onLocalSession
        $ \_ SessionState{time} -> pure time

runTimeIOE
    :: (Member (Embed IO) r)
    => Sem (TimeE UTCTime : r) a
    -> Sem r a
runTimeIOE = interpret $ \case
    GetCurrentTime -> embed IO.getCurrentTime

runRecoverR :: (Member (Error StateError) r) => Sem (RecoverR : r) a -> Sem r a
runRecoverR = interpretH $ \case
    NoSession -> throw ErrNoSession
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

type SynchronicState t ks =
    Sem
        ( Append
            '[ StateE
             , SessionE
             , SessionTimeE t
             , State (ServerState t)
             , TimeE t
             , RecoverR
             , Error StateError
             ]
            ks
        )

runSynchronicState
    :: ServerState UTCTime
    -> SynchronicState UTCTime '[Embed IO] a
    -> IO (Either StateError (ServerState UTCTime, a))
runSynchronicState s =
    runM @IO
        . runError
        . runRecoverR
        . runTimeIOE
        . runState s
        . runSessionTimeE
        . runSessionE @UTCTime
        . runServerE @UTCTime

data MockTime t :: Effect where
    SetTime :: t -> MockTime t m ()

makeSem ''MockTime

runMockTime
    :: (Member (State t) r)
    => Sem (MockTime t : r) a
    -> Sem r a
runMockTime = interpret $ \case
    SetTime time -> put time

runTimeWithMockTime
    :: (Member (State t) r)
    => Sem (TimeE t : r) a
    -> Sem r a
runTimeWithMockTime = interpret $ \case
    GetCurrentTime -> get

type SynchronicStateWithMockTime t
    = SynchronicState t '[MockTime t, State t]

runSynchronicStateWithMockTime
    :: forall t a
     . ServerState t
    -> t
    -> SynchronicStateWithMockTime t a
    -> Either StateError (ServerState t, a)
runSynchronicStateWithMockTime s t =
    run
        . evalState t
        . runMockTime
        . runError
        . runRecoverR
        . runTimeWithMockTime
        . runState s
        . runSessionTimeE @t
        . runSessionE @t
        . runServerE @t
