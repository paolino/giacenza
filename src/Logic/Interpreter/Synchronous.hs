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
    )
where

import Data.Map qualified as Map
import Logic.Language
    ( SessionE (..)
    , SessionId
    , StateE (..)
    , StateSem
    )
import Polysemy
    ( Member
    , Members
    , Sem
    , interpret
    , run
    )
import Polysemy.State (State, evalState, get, put, runState)
import Protolude hiding (State, evalState, get, put, runState, try)
import Types (Analysis (..), Cookie (..), CookieGen (..), FileName (..))

type instance SessionId SessionState = Cookie

data ServerState = ServerState
    { sessions :: Map Cookie SessionState
    , cookies :: CookieGen
    }
    deriving (Eq, Show)

emptyServerState :: CookieGen -> ServerState
emptyServerState = ServerState mempty

newtype SessionState = SessionState
    { files :: Map FileName Analysis
    }
    deriving (Eq, Show)

emptySessionState :: SessionState
emptySessionState = SessionState mempty

runStateE
    :: forall effs a
     . (Members '[State ServerState] effs)
    => StateSem SessionState effs a
    -> Sem effs a
runStateE = interpret $ \case
    GetSession cookie -> do
        ServerState{sessions, cookies} <- get
        case Map.lookup cookie sessions of
            Nothing -> do
                let CookieGen cookie' next = cookies
                put $ ServerState
                    do Map.insert cookie' emptySessionState sessions
                    do next
                pure emptySessionState
            Just x -> pure x
    NewSession -> do
        ServerState{sessions, cookies} <- get
        let CookieGen cookie' f = cookies
        put $ ServerState
            do Map.insert cookie' emptySessionState sessions
            do f
        pure cookie'
    DeleteSession cookie -> do
        ServerState{sessions, cookies} <- get
        let sessions' = Map.delete cookie sessions
        put $ ServerState sessions' cookies
    WithSession cookie q -> do
        ServerState{sessions, cookies} <- get
        let session = fromMaybe emptySessionState (Map.lookup cookie sessions)
        (session', x) <- runState session . runSessionE $ q
        put $ ServerState (Map.insert cookie session' sessions) cookies
        pure x

runSessionE
    :: forall effs a
     . ( Member (State SessionState) effs
       )
    => Sem (SessionE ': effs) a
    -> Sem effs a
runSessionE = interpret $ \case
    GetFiles -> do
        SessionState{files} <- get
        pure $ Map.keys files
    GetFile fileName -> do
        SessionState{files} <- get
        pure $ fromMaybe FileAbsent $ Map.lookup fileName files
    AddFile path -> do
        SessionState{files} <- get
        let fileName = FileName $ hash path
        put $ SessionState $ Map.insert fileName NotDone files
        pure fileName
    SetResult fileName result -> do
        SessionState{files} <- get
        put $ SessionState $ Map.insert fileName (Success result) files
    SetFailure fileName failure -> do
        SessionState{files} <- get
        put $ SessionState $ Map.insert fileName (Failed failure) files
    DeleteFile fileName -> do
        SessionState{files} <- get
        put $ SessionState $ Map.delete fileName files

runPureSynchronicState
    :: CookieGen
    -> StateSem SessionState '[State ServerState] a
    -> a
runPureSynchronicState cg = run . evalState (emptyServerState cg) . runStateE

runPureSessionState
    :: Sem '[SessionE, State SessionState] a
    -> a
runPureSessionState = run . evalState emptySessionState . runSessionE

{- runSessionTimeE
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
                    Just r'' -> pureT $ Right r'' -}
{- runSynchronicState
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
        . runServerE @UTCTime -}

{- data MockTime t :: Effect where
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
        . runServerE @t -}
