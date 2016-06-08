{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Workspace
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Represents a workspace, a work unit, which can be composed of multiple packages
--
-----------------------------------------------------------------------------
module IDE.Workspaces (
    workspaceNew
,   workspaceOpen
,   workspaceTry
,   workspaceOpenThis
,   workspaceClose
,   workspaceClean
,   workspaceMake
,   workspaceActivatePackage
,   workspaceAddPackage
,   workspaceAddPackage'
,   workspaceRemovePackage
,   workspacePackageNew
,   workspacePackageClone
,   workspaceTryQuiet
,   workspaceNewHere
,   packageTry
,   packageTryQuiet

,   backgroundMake
,   makePackage
,   fileOpen
,   fileOpen'
) where

import Prelude ()
import Prelude.Compat
import IDE.Core.State
import Graphics.UI.Editor.Parameters
       (dialogRun', dialogSetDefaultResponse', dialogAddButton',
        Parameter(..), (<<<-), paraName, emptyParams)
import Control.Monad (filterM, void, unless, when, liftM)
import Data.Maybe (isJust, fromJust, catMaybes)
import IDE.Utils.GUIUtils
    (chooseFile, chooseSaveFile, __)
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory)
import Text.PrinterParser
    (readFields,
     writeFields,
     readParser,
     stringParser,
     intParser,
     mkFieldS,
     FieldDescriptionS(..))
import qualified Text.PrettyPrint as  PP (text)
import IDE.Pane.PackageEditor (packageNew', packageClone, choosePackageFile, standardSetup)
import Data.List (delete)
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, idePackageFromPath, idePackageFromPath)
import System.Directory
       (doesDirectoryExist, getDirectoryContents, getHomeDirectory,
        createDirectoryIfMissing, doesFileExist)
import System.Time (getClockTime)
import qualified Control.Exception as Exc (SomeException(..), throw, Exception)
import qualified Data.Map as  Map (empty)
import IDE.Pane.SourceBuffer
       (belongsToWorkspace, IDEBuffer(..), maybeActiveBuf, fileOpenThis,
        fileCheckAll, belongsToPackages')
import Control.Applicative ((<$>))
import IDE.Build
import IDE.Utils.FileUtils(myCanonicalizePath)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import qualified Data.Set as Set (toList)
import Distribution.PackageDescription (hsSourceDirs)
import IDE.Command.VCS.Common.Workspaces as VCSWS
import qualified VCSWrapper.Common as VCS
import qualified VCSGui.Common as VCSGUI
import qualified IDE.Workspaces.Writer as Writer
import System.Log.Logger (debugM)
import IDE.Pane.Log (showDefaultLogLaunch', getLog)
import IDE.LogRef (logOutputDefault)
import Data.Foldable (forM_)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import Data.Monoid ((<>))
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import qualified Data.Text.IO as T (writeFile)
import Control.Exception (SomeException(..), catch)
import GI.Gtk.Objects.MessageDialog
       (setMessageDialogText, constructMessageDialogButtons, setMessageDialogMessageType,
        MessageDialog(..))
import GI.Gtk.Objects.Dialog (constructDialogUseHeaderBar)
import GI.Gtk.Objects.Window
       (windowSetTransientFor, setWindowTitle, Window(..),
        setWindowWindowPosition, setWindowModal)
import GI.Gtk.Enums
       (FileChooserAction(..), ResponseType(..), ButtonsType(..),
        WindowPosition(..), MessageType(..))
import Data.GI.Base (set, nullToNothing, new')
import GI.Gtk.Objects.Widget
       (widgetShow, widgetDestroy, widgetHide)
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Interfaces.FileChooser
       (fileChooserGetFilename, fileChooserSetCurrentFolder,
        fileChooserSetAction)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Constructs a new workspace and makes it the current workspace
workspaceNew :: IDEAction
workspaceNew = do
    window <- getMainWindow
    mbFile <- liftIO $ chooseSaveFile window (__ "New file for workspace") Nothing
    forM_ mbFile workspaceNewHere

workspaceNewHere :: FilePath -> IDEAction
workspaceNewHere filePath =
    let realPath = if takeExtension filePath == leksahWorkspaceFileExtension
                            then filePath
                            else addExtension filePath leksahWorkspaceFileExtension
    in do
        dir <- liftIO $ myCanonicalizePath $ dropFileName realPath
        let cPath = dir </> takeFileName realPath
            newWorkspace = emptyWorkspace {
                            wsName = T.pack $ takeBaseName cPath,
                            wsFile = cPath}
        liftIO $ writeFields cPath newWorkspace Writer.workspaceDescr
        workspaceOpenThis False (Just cPath)
        return ()

workspaceOpen :: IDEAction
workspaceOpen = do
    window     <- getMainWindow
    mbFilePath <- liftIO $ chooseWorkspaceFile window
    workspaceOpenThis True mbFilePath
    return ()

workspaceTryQuiet :: WorkspaceAction -> IDEAction
workspaceTryQuiet f = do
    maybeWorkspace <- readIDE workspace
    case maybeWorkspace of
        Just ws -> runWorkspace f ws
        Nothing -> ideMessage Normal (__ "No workspace open")

workspaceTry :: WorkspaceAction -> IDEAction
workspaceTry f = do
    maybeWorkspace <- readIDE workspace
    case maybeWorkspace of
        Just ws -> runWorkspace f ws
        Nothing -> do
            mainWindow <- getMainWindow
            defaultWorkspace <- liftIO $ (</> "leksah.lkshw") <$> getHomeDirectory
            defaultExists <- liftIO $ doesFileExist defaultWorkspace
            md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeCancel]
            setWindowModal md True
            setMessageDialogMessageType md MessageTypeQuestion
            setMessageDialogText md (
                    __ "You need to have a workspace open for this to work. "
                 <> __ "Choose ~/leksah.lkshw to "
                 <> __ (if defaultExists then "open workspace " else "create a workspace ")
                 <> T.pack defaultWorkspace)
            windowSetTransientFor md (Just mainWindow)
            dialogAddButton' md (__ "_New Workspace") (AnotherResponseType 1)
            dialogAddButton' md (__ "_Open Workspace") (AnotherResponseType 2)
            dialogAddButton' md "~/leksah.lkshw" (AnotherResponseType 3)
            dialogSetDefaultResponse' md (AnotherResponseType 3)
            setWindowWindowPosition md WindowPositionCenterOnParent
            resp <- dialogRun' md
            widgetHide md
            case resp of
                AnotherResponseType 1 -> do
                    workspaceNew
                    postAsyncIDE $ workspaceTryQuiet f
                AnotherResponseType 2 -> do
                    workspaceOpen
                    postAsyncIDE $ workspaceTryQuiet f
                AnotherResponseType 3 -> do
                    defaultExists <- liftIO $ doesFileExist defaultWorkspace
                    if defaultExists
                        then workspaceOpenThis True (Just defaultWorkspace)
                        else workspaceNewHere defaultWorkspace
                    postAsyncIDE $ workspaceTryQuiet f
                _  -> return ()

chooseWorkspaceFile :: Window -> IO (Maybe FilePath)
chooseWorkspaceFile window = chooseFile window (__ "Select leksah workspace file (.lkshw)") Nothing [("Leksah Workspace Files", ["*.lkshw"])]

workspaceOpenThis :: Bool -> Maybe FilePath -> IDEAction
workspaceOpenThis askForSession mbFilePath =
    case mbFilePath of
        Nothing -> return ()
        Just filePath -> do
            liftIO . debugM "leksah" $ "workspaceOpenThis " ++ show askForSession ++ " " ++ filePath
            let spath =  dropExtension filePath ++ leksahSessionFileExtension
            workspaceClose
            exists <- liftIO $ doesFileExist spath
            wantToLoadSession <-
                if exists && askForSession
                    then do
                        window <- getMainWindow
                        md <- new' MessageDialog [
                            constructDialogUseHeaderBar 0,
                            constructMessageDialogButtons ButtonsTypeNone]
                        setMessageDialogMessageType md MessageTypeQuestion
                        setMessageDialogText md $ __ "There are session settings stored with this workspace."
                        windowSetTransientFor md (Just window)
                        dialogAddButton' md (__ "_Ignore Session") ResponseTypeCancel
                        dialogAddButton' md (__ "_Load Session") ResponseTypeYes
                        dialogSetDefaultResponse' md ResponseTypeYes
                        setWindowWindowPosition md WindowPositionCenterOnParent
                        rid <- dialogRun' md
                        widgetDestroy md
                        return $ rid == ResponseTypeYes
                    else return False
            if wantToLoadSession
                then void (triggerEventIDE (LoadSession spath))
                else do
                    ideR <- ask
                    catchIDE (do
                        workspace <- readWorkspace filePath
                        Writer.setWorkspace (Just workspace {wsFile = filePath})
                        VCSWS.onWorkspaceOpen workspace)
                           (\ (e :: Exc.SomeException) -> reflectIDE
                                (ideMessage Normal (T.pack $ printf (__ "Can't load workspace file %s\n%s") filePath (show e))) ideR)


-- | Closes a workspace
workspaceClose :: IDEAction
workspaceClose = do
    liftIO $ debugM "leksah" "workspaceClose"
    oldWorkspace <- readIDE workspace
    case oldWorkspace of
        Nothing -> return ()
        Just ws -> do
            VCSWS.onWorkspaceClose
            let oldActivePackFile = wsActivePackFile ws
            prefs <- readIDE prefs
            when (saveSessionOnClose prefs) $
                triggerEventIDE_ (SaveSession (dropExtension (wsFile ws) ++ leksahSessionFileExtension))
            addRecentlyUsedWorkspace (wsFile ws)
            Writer.setWorkspace Nothing
            when (isJust oldActivePackFile) $ do
                triggerEventIDE (Sensitivity [(SensitivityProjectActive, False),
                    (SensitivityWorkspaceOpen, False)])
                return ()
            return ()
    return ()


workspacePackageNew :: WorkspaceAction
workspacePackageNew = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    lift $ packageNew' path logOutputDefault (\isNew fp -> do
        window     <-  getMainWindow
        workspaceTry $ void (workspaceAddPackage' fp)
        when isNew $ do
            mbPack <- idePackageFromPath logOutputDefault fp
            constructAndOpenMainModules mbPack
        void (triggerEventIDE UpdateWorkspaceInfo))

workspacePackageClone :: WorkspaceAction
workspacePackageClone = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    lift $ packageClone path logOutputDefault (\fp -> do
        window     <-  getMainWindow
        workspaceTry $ void (workspaceAddPackage' fp)
        void (triggerEventIDE UpdateWorkspaceInfo))

constructAndOpenMainModules :: Maybe IDEPackage -> IDEAction
constructAndOpenMainModules Nothing = return ()
constructAndOpenMainModules (Just idePackage) =
    forM_ (ipdMain idePackage) $ \(target, bi, isTest) -> do
        mbPD <- getPackageDescriptionAndPath
        case mbPD of
            Just (pd,_) ->
                case hsSourceDirs bi of
                    path:_ -> do
                        liftIO $ createDirectoryIfMissing True path
                        alreadyExists <- liftIO $ doesFileExist (path </> target)
                        unless alreadyExists $ do
                            template <- liftIO $ getModuleTemplate (if isTest then "testmain" else "main") pd "Main" "" ""
                            liftIO $ T.writeFile (path </> target) template
                            fileOpenThis (path </> target)
                    _ -> return ()
            Nothing     -> ideMessage Normal (__ "No package description")

workspaceAddPackage :: WorkspaceAction
workspaceAddPackage = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    window <-  lift getMainWindow
    mbFilePath <- liftIO $ choosePackageFile window (Just path)
    case mbFilePath of
        Nothing -> return ()
        Just fp -> do
            void (workspaceAddPackage' fp)
            lift $ void (triggerEventIDE UpdateWorkspaceInfo)

workspaceAddPackage' :: FilePath -> WorkspaceM (Maybe IDEPackage)
workspaceAddPackage' fp = do
    ws <- ask
    cfp <- liftIO $ myCanonicalizePath fp
    mbPack <- lift $ idePackageFromPath logOutputDefault cfp
    case mbPack of
        Just pack -> do
            unless (cfp `elem` map ipdCabalFile (wsPackages ws)) $ lift $
                Writer.writeWorkspace $ ws {wsPackages =  pack : wsPackages ws,
                                     wsActivePackFile =  Just (ipdCabalFile pack),
                                     wsActiveExe = Nothing}
            return (Just pack)
        Nothing -> return Nothing

packageTryQuiet :: PackageAction -> IDEAction
packageTryQuiet f = do
    maybePackage <- readIDE activePack
    case maybePackage of
        Just p  -> workspaceTryQuiet $ runPackage f p
        Nothing -> ideMessage Normal (__ "No active package")

packageTry :: PackageAction -> IDEAction
packageTry f = workspaceTry $ do
        maybePackage <- lift $ readIDE activePack
        case maybePackage of
            Just p  -> runPackage f p
            Nothing -> do
                window <- lift getMainWindow
                md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeCancel]
                setMessageDialogMessageType md MessageTypeQuestion
                setMessageDialogText md $ __ "You need to have an active package for this to work."
                windowSetTransientFor md (Just window)
                dialogAddButton' md (__ "_New Package") (AnotherResponseType 1)
                dialogAddButton' md (__ "_Add Package") (AnotherResponseType 2)
                dialogSetDefaultResponse' md (AnotherResponseType 2)
                setWindowWindowPosition md WindowPositionCenterOnParent
                resp <- dialogRun' md
                widgetHide md
                case resp of
                    AnotherResponseType 1 -> do
                        workspacePackageNew
                        lift $ postAsyncIDE $ packageTryQuiet f
                    AnotherResponseType 2 -> do
                        workspaceAddPackage
                        lift $ postAsyncIDE $ packageTryQuiet f
                    _  -> return ()

workspaceRemovePackage :: IDEPackage -> WorkspaceAction
workspaceRemovePackage pack = do
    ws <- ask
    when (pack `elem` wsPackages ws) $ lift $
        Writer.writeWorkspace ws {wsPackages =  delete pack (wsPackages ws)}
    return ()

workspaceActivatePackage :: IDEPackage -> Maybe Text -> WorkspaceAction
workspaceActivatePackage pack exe = do
    ws <- ask
    let activePath = takeDirectory $ ipdCabalFile pack
    lift $ activatePackage (Just activePath) (Just pack) exe
    when (pack `elem` wsPackages ws) $ lift $ do
        Writer.writeWorkspace ws {wsActivePackFile =  Just (ipdCabalFile pack)
                                 ,wsActiveExe = exe}
        return ()
    return ()



readWorkspace :: FilePath -> IDEM Workspace
readWorkspace fp = do
    liftIO $ debugM "leksah" "readWorkspace"
    ws <- liftIO $ readFields fp Writer.workspaceDescr emptyWorkspace
    ws' <- liftIO $ makePathsAbsolute ws fp
    packages <- mapM (idePackageFromPath logOutputDefault) (wsPackagesFiles ws')
    --TODO set package vcs here
    return ws'{ wsPackages = catMaybes packages}




makePathsAbsolute :: Workspace -> FilePath -> IO Workspace
makePathsAbsolute ws bp = do
    wsFile'                     <-  myCanonicalizePath bp
    wsActivePackFile'           <-  case wsActivePackFile ws of
                                        Nothing -> return Nothing
                                        Just fp -> do
                                            fp' <- makeAbsolute (dropFileName wsFile') fp
                                            return (Just fp')
    wsPackagesFiles'            <-  mapM (makeAbsolute (dropFileName wsFile')) (wsPackagesFiles ws)
    return ws {wsActivePackFile = wsActivePackFile', wsFile = wsFile', wsPackagesFiles = wsPackagesFiles'}
    where
        makeAbsolute basePath relativePath  =
            myCanonicalizePath
               (if isAbsolute relativePath
                    then relativePath
                    else basePath </> relativePath)

emptyWorkspace =  Workspace {
    wsVersion       =   Writer.workspaceVersion
,   wsSaveTime      =   ""
,   wsName          =   ""
,   wsFile          =   ""
,   wsPackages      =   []
,   wsPackagesFiles =   []
,   wsActivePackFile =   Nothing
,   wsActiveExe     =   Nothing
,   wsNobuildPack   =   []
,   packageVcsConf  =   Map.empty
}



addRecentlyUsedWorkspace :: FilePath -> IDEAction
addRecentlyUsedWorkspace fp = do
    state <- readIDE currentState
    unless (isStartingOrClosing state) $ do
        recentWorkspaces' <- readIDE recentWorkspaces
        unless (fp `elem` recentWorkspaces') $
            modifyIDE_ (\ide -> ide{recentWorkspaces = take 12 (fp : recentWorkspaces')})
        triggerEventIDE UpdateRecent
        return ()

removeRecentlyUsedWorkspace :: FilePath -> IDEAction
removeRecentlyUsedWorkspace fp = do
    state <- readIDE currentState
    unless (isStartingOrClosing state) $ do
        recentWorkspaces' <- readIDE recentWorkspaces
        when (fp `elem` recentWorkspaces') $
            modifyIDE_ (\ide -> ide{recentWorkspaces = filter (/= fp) recentWorkspaces'})
        triggerEventIDE UpdateRecent
        return ()

------------------------
-- Workspace make

workspaceClean :: WorkspaceAction
workspaceClean = do
    ws <- ask
    settings <- lift $ do
        prefs' <- readIDE prefs
        return (defaultMakeSettings prefs')
    makePackages settings (wsPackages ws) MoClean MoClean moNoOp

buildSteps :: MakeSettings -> IDEM [MakeOp]
buildSteps settings = do
    let runTests  = msRunUnitTests settings
    let runBenchmarks = msRunBenchmarks settings
    debug <- isJust <$> readIDE debugState
    return $ case (runTests, debug,runBenchmarks) of
                (True, True,_)   -> [MoBuild,MoDocu]
                (True, False,False)  -> [MoBuild,MoDocu,MoTest,MoInstall]
                (True, False,True)  -> [MoBuild,MoDocu,MoTest,MoInstall,MoBench]
                (False, True,_)  -> [MoBuild]
                (False, False,False) -> [MoBuild,MoInstall]
                (False, False,True) -> [MoBuild,MoInstall,MoBench]

workspaceMake :: WorkspaceAction
workspaceMake = do
    ws <- ask
    settings <- lift $ do
        prefs' <- readIDE prefs
        return ((defaultMakeSettings prefs'){
                    msMakeMode           = True,
                    msBackgroundBuild    = False})
    build <- lift . buildSteps $ settings
    makePackages settings (wsPackages ws) (MoComposed build) (MoComposed build) MoMetaInfo

backgroundMake :: IDEAction
backgroundMake = catchIDE (do
    ideR        <- ask
    prefs       <- readIDE prefs
    debug       <- isJust <$> readIDE debugState
    modifiedPacks <- if saveAllBeforeBuild prefs
                        then fileCheckAll belongsToPackages'
                        else return []
    let isModified = not (null modifiedPacks)
    when isModified $ do
        let settings = defaultMakeSettings prefs
        steps <- buildSteps settings
        workspaceTryQuiet $ if debug || msSingleBuildWithoutLinking settings && not (msMakeMode settings)
            then makePackages settings modifiedPacks (MoComposed steps) (MoComposed []) moNoOp
            else makePackages settings modifiedPacks (MoComposed steps)
                                (MoComposed steps) MoMetaInfo
    )
    (\(e :: Exc.SomeException) -> sysMessage Normal (T.pack $ show e))

makePackage ::  PackageAction
makePackage = do
  p <- ask
  liftIDE $ do
    getLog >>= bringPaneToFront
    showDefaultLogLaunch'
    prefs' <- readIDE prefs
    mbWs   <- readIDE workspace
    let settings = (defaultMakeSettings prefs'){msBackgroundBuild = False}
    case mbWs of
        Nothing -> sysMessage Normal (__ "No workspace for build.")
        Just ws -> do
            debug <- isJust <$> readIDE debugState
            steps <- buildSteps settings
            if debug || msSingleBuildWithoutLinking settings && not (msMakeMode settings)
                then runWorkspace
                        (makePackages settings [p] (MoComposed steps) (MoComposed []) moNoOp) ws
                else
                    runWorkspace
                        (makePackages settings [p]
                        (MoComposed steps)
                        (MoComposed steps)
                        MoMetaInfo) ws

fileOpen :: IDEAction
fileOpen = do
    window <- getMainWindow
    prefs <- readIDE prefs
    mbBuf <- maybeActiveBuf
    dialog <- new' FileChooserDialog []
    setWindowTitle dialog $ __ "Open File"
    windowSetTransientFor dialog $ Just window
    fileChooserSetAction dialog FileChooserActionOpen
    dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    dialogAddButton' dialog "gtk-open" ResponseTypeAccept
    case mbBuf >>= fileName of
        Just fn -> void (fileChooserSetCurrentFolder dialog (dropFileName fn))
        Nothing -> return ()
    widgetShow dialog
    response <- dialogRun' dialog
    when (response == ResponseTypeAccept) $
        nullToNothing (fileChooserGetFilename dialog) >>= mapM_ fileOpen'
    widgetDestroy dialog

fileOpen' :: FilePath -> IDEAction
fileOpen' fp = do
    window <- getMainWindow
    knownFile <- belongsToWorkspace fp
    unless knownFile $
        if takeExtension fp == ".cabal"
            then do
                md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeNone]
                setMessageDialogMessageType md MessageTypeQuestion
                setMessageDialogText md (__ "Would you like to add the package " <> T.pack fp
                    <> __ " to the workspace so that it can be built by Leksah?")
                windowSetTransientFor md (Just window)
                dialogAddButton' md (__ "_Add " <> T.pack (takeFileName fp)) (AnotherResponseType 1)
                dialogAddButton' md (__ "Just _open " <> T.pack (takeFileName fp)) (AnotherResponseType 2)
                dialogSetDefaultResponse' md (AnotherResponseType 1)
                resp <- dialogRun' md
                widgetDestroy md
                case resp of
                    AnotherResponseType 1 -> workspaceTry $ do
                        workspaceAddPackage' fp
                        return ()
                    _ -> return ()
            else liftIO (findCabalFile fp) >>= \case
                Nothing -> return ()
                Just cabalFile -> do
                    md <- new' MessageDialog [
                        constructDialogUseHeaderBar 0,
                        constructMessageDialogButtons ButtonsTypeNone]
                    setMessageDialogMessageType md MessageTypeQuestion
                    setMessageDialogText md (__ "The file " <> T.pack fp
                        <> __ " seems to belong to the package " <> T.pack cabalFile
                        <> __ " would you like to add " <> T.pack (takeFileName cabalFile)
                        <> __ " to your workspace?")
                    windowSetTransientFor md (Just window)
                    dialogAddButton' md (__ "_Add " <> T.pack (takeFileName cabalFile)) (AnotherResponseType 1)
                    dialogAddButton' md (__ "Just _open " <> T.pack (takeFileName fp)) (AnotherResponseType 2)
                    dialogSetDefaultResponse' md (AnotherResponseType 1)
                    resp <- dialogRun' md
                    widgetDestroy md
                    case resp of
                        AnotherResponseType 1 -> workspaceTry $ do
                            workspaceAddPackage' cabalFile
                            return ()
                        _ -> return ()
    fileOpenThis fp
  where
    findCabalFile :: FilePath -> IO (Maybe FilePath)
    findCabalFile fp = (do
            let dir = takeDirectory fp
            contents <- getDirectoryContents dir
            files    <- filterM (\f -> not <$> doesDirectoryExist (dir </> f)) contents
            let cabal = filter ((== ".cabal") . takeExtension) files
            case cabal of
                (c:_) -> return . Just $ dir </> c
                _ | fp == dir -> return Nothing
                  | otherwise -> findCabalFile dir
        ) `catch` (\(_ :: SomeException) -> return Nothing)
