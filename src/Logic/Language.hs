{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Logic.Language
    ( SessionE (..)
    , StateE (..)
    , TimeE (..)
    , RecoverR (..)
    , StateError (..)
    , withSession
    , getSession
    , newSession
    , deleteSession
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
    ) where

import Protolude hiding (State, get, put, runState)

import Polysemy (Effect, Member, Sem, makeSem)
import Types (Analysis, Cookie, Failure, FileName, Result)

----- StateE language ---------------------------------------------------------

data StateE :: Effect where
    GetSession :: Cookie -> StateE m Cookie
    NewSession :: StateE m Cookie
    DeleteSession :: Cookie -> StateE m ()

makeSem ''StateE

----- SessionE language -------------------------------------------------------

type family TimeOf m

data SessionE t :: Effect where
    GetFiles :: SessionE t m [FileName]
    GetFile :: FileName -> SessionE t m Analysis
    UpdateTime :: t -> SessionE t m ()
    GetTime :: SessionE t m t
    AddFile :: FilePath -> SessionE t m FileName
    SetResult :: FileName -> Result -> SessionE t m ()
    SetFailure :: FileName -> Failure -> SessionE t m ()
    DeleteFile :: FileName -> SessionE t m ()

makeSem ''SessionE

---- Public API of SessionE ----------------------------------------------------

withSession :: (Member StateE r) => Cookie -> Sem r a -> Sem r (Cookie, a)
withSession cookie f = do
    cookie' <- getSession cookie
    (cookie',) <$> f

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
