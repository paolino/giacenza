{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Logic.Invariants where

import Logic.Language
    ( SessionE
    , StateSem
    , addFile
    , deleteSession
    , getFile
    , newSession
    , withSession
    )
import Polysemy (Member, Sem)
import Protolude hiding (State, get, put, runState)
import Types (Analysis (..))

-- adding a file and getting it will produce a NotDone analysis
filesAreStored :: (Member SessionE r) => Sem r Bool
filesAreStored = do
    file <- addFile "file"
    analysis <- getFile file
    pure $ case analysis of
        NotDone -> True
        _ -> False

-- sadly because the ServerE language is polymorphic in the session state
-- we have to help the compiler with type annotations

-- we can operate on a session after it's been created
createdSessionsAreUsable
    :: forall s effs
     . StateSem s effs Bool
createdSessionsAreUsable = do
    cookie <- newSession @s @effs
    withSession @s @effs cookie filesAreStored

-- we can reuse a session
sessionsAreResusable
    :: forall s effs
     . StateSem s effs Bool
sessionsAreResusable = do
    cookie <- newSession @s @effs
    file <- withSession @s @effs cookie $ do
        addFile "file"
    analysis <- withSession @s @effs cookie $ getFile file
    pure $ case analysis of
        NotDone -> True
        _ -> False

-- we cannot reuse a deleted session
deletedSessionsAreNotReusable
    :: forall s effs
     . StateSem s effs Bool
deletedSessionsAreNotReusable = do
    cookie <- newSession @s @effs
    file <- withSession @s @effs cookie $ do
        addFile "file"
    deleteSession @s @effs cookie
    analysis <- withSession @s @effs cookie $ getFile file
    pure $ case analysis of
        FileAbsent -> True
        _ -> False
