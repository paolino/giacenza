{-# LANGUAGE ConstraintKinds, AllowAmbiguousTypes #-}

module Logic.Program where

import Logic.Language
    ( AnalyzerE
    , FileStorageE
    , GetCookieE
    , SessionE
    , StateE
    , addFile
    , analyze
    , deleteFile
    , deleteSession
    , getCookie
    , getFile
    , getFilePath
    , getFiles
    , putFilePath
    , setFailure
    , setResult
    , withSession, getConfiguration, RecoverR
    )
import Polysemy (Member, Members, Sem)
import Protolude
import Types
    ( Analysis (..)
    , Cookie (..)
    , DownloadPath
    , FileName (..)
    , StoragePath (..)
    , UploadPath (..)
    )

type StateEffs effs = StateE effs ': effs

-- | Run a session effect with the current cookie
withCurrentSession
    :: forall effs b
     . Member GetCookieE effs
    => Sem (SessionE : effs) b
    -> Sem (StateEffs effs) b
withCurrentSession f = do
    cookie <- getCookie
    withSession cookie f

-- | Generate a unique filename by prepending the current cookie
-- This implies that loosing a cookie implies loosing the session !
uniqueFilename :: Member GetCookieE r => FileName -> Sem r FileName
uniqueFilename (FileName filename) = do
    Cookie cookie <- getCookie
    pure $ FileName $ cookie <> "-" <> filename

-- | Add a file to the current session and store it
addFileP
    :: forall effs
     . Members '[GetCookieE, FileStorageE] effs
    => FileName
    -> DownloadPath
    -> Sem (StateEffs effs) ()
addFileP name input = withCurrentSession @effs do
    name' <- uniqueFilename name
    putFilePath name' input
    addFile name

-- | List all files with their analysis status in the current session
listFilesP
    :: forall r
     . Members '[GetCookieE, AnalyzerE] r
    => Sem (StateEffs r) [(FileName, Analysis)]
listFilesP = withCurrentSession $ do
    files <- getFiles
    forM files $ \file -> do
        analysis <- getFile file
        pure (file, analysis)

-- | Analyze a file and store the result
analyzeFileP
    :: forall e effs
     . Members '[GetCookieE, FileStorageE, AnalyzerE, RecoverR e] effs
    => FileName
    -> Sem (StateEffs effs) ()
analyzeFileP name = withCurrentSession do
    name' <- uniqueFilename name
    path <- getFilePath name'
    cfg <- getConfiguration @e name
    result <- analyze path cfg
    case result of
        Left err -> setFailure name err
        Right analysis -> setResult name analysis

-- | Delete a file from the current session
deleteSessionP
    :: forall effs
     . Members '[GetCookieE] effs
    => Sem (StateEffs effs) ()
deleteSessionP = do
    withCurrentSession do
        names <- getFiles
        forM_ names deleteFile
    getCookie >>= deleteSession @effs

-- | Retrieve session for downloading
retrieveSessionP
    :: forall r
     . Members '[GetCookieE, FileStorageE] r
    => Sem (StateEffs r) [(FileName, UploadPath)]
retrieveSessionP = withCurrentSession do
    names <- getFiles
    forM names \name -> do
        name' <- uniqueFilename name
        StoragePath path <- getFilePath name'
        pure (name, UploadPath path)

-- | Restore a session from a list of files
restoreSessionP
    :: forall e effs
     . Members '[GetCookieE, FileStorageE, AnalyzerE, RecoverR e] effs
    => [(FileName, DownloadPath)]
    -> Sem (StateEffs effs) ()
restoreSessionP files = withCurrentSession $ do
    forM_ files \(name, path) -> do
        name' <- uniqueFilename name
        putFilePath name' path
        addFile name
    names <- getFiles
    forM_ names \name -> do
        name' <- uniqueFilename name
        path <- getFilePath name'
        analysis <- getConfiguration @e name >>= analyze path
        case analysis of
            Left err -> setFailure name err
            Right success -> setResult name success