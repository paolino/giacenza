{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Logic.Language
    ( SessionE (..)
    , SessionTimeE (..)
    , StateE (..)
    , TimeE (..)
    , RecoverR (..)
    , StateError (..)
    , SessionId
    , getSession
    , newSession
    , deleteSession
    , withSession
    , getTime
    , updateTime
    , getFiles
    , getFile
    , addFile
    , setResult
    , setFailure
    , deleteFile
    , getCurrentTime
    , noSession
    , recover
    , StateSem
    )
where

import Polysemy (Effect, Sem, makeSem)
import Polysemy.State (State)
import Protolude hiding (State, get, put, runState)
import Types (Analysis, Failure, FileName, Result)

----- SessionE language -------------------------------------------------------

data SessionE :: Effect where
    GetFiles :: SessionE m [FileName]
    GetFile :: FileName -> SessionE m Analysis
    AddFile :: FilePath -> SessionE m FileName
    SetResult :: FileName -> Result -> SessionE m ()
    SetFailure :: FileName -> Failure -> SessionE m ()
    DeleteFile :: FileName -> SessionE m ()

makeSem ''SessionE

data SessionTimeE t :: Effect where
    UpdateTime :: t -> SessionTimeE t m ()
    GetTime :: SessionTimeE t m t

makeSem ''SessionTimeE

----- StateE language ---------------------------------------------------------

type family SessionId s

data StateE s e :: Effect where
    GetSession :: SessionId s -> StateE s e m s
    NewSession :: StateE s m e (SessionId s)
    DeleteSession :: SessionId s -> StateE s e m ()
    WithSession :: SessionId s -> Sem (SessionEffect s e) a -> StateE s e m a

-- this is somehow limiting, and doesn't help much in disambiguition
type SessionEffect s effs = SessionE ': State s ': effs

type StateSem s effs = Sem (StateE s effs ': effs)

makeSem ''StateE

---- TimeE language -----------------------------------------------------------

data TimeE t :: Effect where
    GetCurrentTime :: TimeE t m t

makeSem ''TimeE

---- RecoverR language --------------------------------------------------------

data StateError = ErrNoSession
    deriving (Eq, Show)

data RecoverR :: Effect where
    NoSession :: RecoverR m b
    Recover :: m a -> RecoverR m (Either StateError a)

makeSem ''RecoverR
