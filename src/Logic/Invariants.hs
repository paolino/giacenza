{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Logic.Invariants where

import Logic.Language
    ( RecoverR
    , SessionE
    , StateE
    , StateError (ErrNoSession)
    , addFile
    , getFile
    , newSession
    , recover
    )
import Polysemy (Members, Sem)
import Protolude
import Types (Analysis (..))

-- adding a file and getting it will produce a NotDone analysis

getFileAfterAddFileProducesNotDone
    :: forall t effs
     . Members '[SessionE t, StateE, RecoverR] effs
    => Sem effs Bool
getFileAfterAddFileProducesNotDone = do
    _ <- newSession
    file <- addFile @t "file"
    analysis <- getFile @t file
    pure $ case analysis of
        NotDone -> True
        _ -> False

-- adding a file without creating a session will produce an error NoSession

addFileWithoutSessionProducesNoSession
    :: forall t effs
     . (Members '[SessionE t, StateE, RecoverR] effs)
    => Sem effs Bool
addFileWithoutSessionProducesNoSession = do
    file <- recover $ addFile @t "file"
    pure $ case file of
        Left ErrNoSession -> True
        _ -> False
