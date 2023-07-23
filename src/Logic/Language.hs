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

import Polysemy (Effect, Member, Sem, makeSem)
import Protolude hiding (State, get, put, runState)
import Types (Analysis, Cookie, Failure, FileName, Result)

----- StateE language ---------------------------------------------------------

data StateE :: Effect where
    GetSession :: Cookie -> StateE m Cookie
    NewSession :: StateE m Cookie
    DeleteSession :: Cookie -> StateE m ()

makeSem ''StateE

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
