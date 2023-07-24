{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Logic.Invariants where

import Logic.Language
    ( StateE
    , addFile
    , getFile
    , newSession
    , updateSession
    )
import Polysemy (Member, Sem)
import Protolude hiding (State, get, put, runState)
import Types (Analysis (..))

-- adding a file and getting it will produce a NotDone analysis

getFileAfterAddFileProducesNotDone
    :: forall s effs
        . Sem (StateE s effs ': effs) Bool
getFileAfterAddFileProducesNotDone = do
    cookie <- newSession @s @effs
    updateSession @s @effs cookie $ do
        file <- addFile "file"
        analysis <- getFile file
        pure $ case analysis of
            NotDone -> True
            _ -> False

-- updating a deleted
