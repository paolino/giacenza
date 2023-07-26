{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Logic.Invariants where

import Logic.Language
    ( GetCookieE
    , SessionE
    , StateE
    , addFile
    , deleteSession
    , getCookie
    , getFile
    , withSession
    )
import Polysemy (Member, Members, Sem)
import Protolude hiding (State, get, put, runState)
import Types (Analysis (..))

-- adding a file and getting it will produce a NotDone analysis
filesAreStored :: Member SessionE r => Sem r Bool
filesAreStored = do
    let filename = "file"
    addFile filename
    analysis <- getFile filename
    pure $ case analysis of
        NotDone -> True
        _ -> False

-- sadly because the ServerE language is polymorphic in the session state
-- we have to help the compiler with type annotations

-- we can operate on a session after it's been created
createdSessionsAreUsable
    :: forall effs
     . Members '[GetCookieE, StateE effs] effs
    => Sem effs Bool
createdSessionsAreUsable = do
    cookie <- getCookie @effs
    withSession @effs cookie filesAreStored

-- we can reuse a session
sessionsAreResusable
    :: forall effs
     . Members '[GetCookieE, StateE effs] effs
    => Sem effs Bool
sessionsAreResusable = do
    cookie <- getCookie
    let filename = "file"
    withSession @effs cookie $ addFile filename
    analysis <- withSession @effs cookie $ getFile filename
    pure $ case analysis of
        NotDone -> True
        _ -> False

-- we cannot reuse a deleted session
deletedSessionsAreNotReusable
    :: forall effs
     . Members '[GetCookieE, StateE effs] effs
    => Sem effs Bool
deletedSessionsAreNotReusable = do
    cookie <- getCookie
    let filename = "file"
    withSession @effs cookie $ addFile filename
    deleteSession @effs cookie
    analysis <- withSession @effs cookie $ getFile filename
    pure $ case analysis of
        FileAbsent -> True
        _ -> False
