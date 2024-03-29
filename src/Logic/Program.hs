{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Logic.Program where

import Compute (sumResults)
import Logic.Language
    ( AnalyzerE
    , FileStorageE
    , GetCookieE
    , RecoverR
    , SessionE
    , StateE
    , WebE
    , addFile
    , analyze
    , deleteFile
    , deleteFilePath
    , deleteOldConfig
    , deleteSession
    , getConfiguration
    , getCookie
    , getFile
    , getFilePath
    , getFiles
    , getOldConfig
    , header
    , putFilePath
    , setConfig
    , setFailure
    , setResult
    , storeOldConfig
    , withSession
    )
import Polysemy (Member, Members, Sem)
import Protolude
import Types
    ( Analysis (..)
    , Config
    , Cookie (..)
    , DownloadPath
    , FileName (..)
    , Result
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

-- | Delete a file from the current session
deleteFileP
    :: (Member GetCookieE r, Member FileStorageE r)
    => FileName
    -> Sem (StateEffs r) ()
deleteFileP = withCurrentSession . deleteFileP'

deleteFileP'
    :: ( Member GetCookieE r
       , Member FileStorageE r
       , Member SessionE r
       )
    => FileName
    -> Sem r ()
deleteFileP' name = do
    name' <- uniqueFilename name
    deleteFilePath name'
    deleteFile name

-- | List all files with their analysis status in the current session
listFilesP
    :: forall r
     . Members '[GetCookieE, AnalyzerE, FileStorageE] r
    => Sem (StateEffs r) [(FileName, [Text], Analysis)]
listFilesP = withCurrentSession $ do
    files <- getFiles
    forM files $ \name -> do
        name' <- uniqueFilename name
        path <- getFilePath name'
        cols <- header path
        case cols of
            Left err -> pure (name, [], Unconfigurable err)
            Right cols' -> do
                analysis <- getFile name
                pure (name, cols', analysis)

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
     . (Members '[GetCookieE] effs, Member WebE effs, Member FileStorageE effs)
    => Sem (StateEffs effs) ()
deleteSessionP = do
    deleteAllFilesP
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

-- | Configure a not done file
configureFileP
    :: forall effs
     . Members '[GetCookieE] effs
    => FileName
    -> Config
    -> Sem (StateEffs effs) ()
configureFileP name cfg = withCurrentSession do
    setConfig name cfg

-- | Set file state to NotDone
resetFileP
    :: forall effs
     . Members '[GetCookieE] effs
    => FileName
    -> Sem (StateEffs effs) ()
resetFileP name = withCurrentSession do
    addFile name

getOldConfigurations
    :: forall effs
     . ( Member GetCookieE effs
       , Member WebE effs
       )
    => Sem (StateEffs effs) [(FileName, Config)]
getOldConfigurations = withCurrentSession do
    files <- getFiles
    rs <- forM files $ \fileName -> do
        config <- getOldConfig fileName
        pure $ (fileName,) <$> config
    pure $ catMaybes rs

propagateConfigP
    :: ( Member (RecoverR IOException) effs
       , Member GetCookieE effs
       , Member WebE effs
       )
    => FileName
    -> Sem (StateEffs effs) ()
propagateConfigP fn = withCurrentSession do
    cfg <- getConfiguration @IOException fn
    files <- getFiles
    forM_ files $ \fn' -> do
        fileState <- getFile fn'
        store <- case fileState of
            NotDone -> pure True
            Failed _ _ -> addFile fn' $> True
            _ -> pure False
        when store $ storeOldConfig fn' cfg

notDonesOrFaileds
    :: Member GetCookieE effs
    => Sem (StateEffs effs) [FileName]
notDonesOrFaileds = withCurrentSession do
    files <- getFiles
    fmap concat $ forM files $ \fn' -> do
        fileState <- getFile fn'
        case fileState of
            NotDone -> pure [fn']
            Failed _ _ -> pure [fn']
            _ -> pure []

deleteAllFilesP
    :: forall effs
     . (Members '[GetCookieE, FileStorageE] effs, Member WebE effs)
    => Sem (StateEffs effs) ()
deleteAllFilesP = withCurrentSession do
    names <- getFiles
    forM_ names $ \fn -> do
        deleteFileP' fn
        deleteOldConfig fn

reconfigureAllFilesP
    :: Member GetCookieE effs
    => Sem (StateEffs effs) ()
reconfigureAllFilesP = withCurrentSession do
    files <- getFiles
    forM_ files addFile

sumsP
    :: Member GetCookieE effs
    => Sem (StateEffs effs) Result
sumsP = withCurrentSession do
    files <- getFiles
    rs <- forM files $ \fn -> do
        fileState <- getFile fn
        pure $ case fileState of
            Success result _ -> Just result
            _ -> Nothing
    pure $ sumResults $ catMaybes rs
