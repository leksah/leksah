{-# LANGUAGE CPP, ScopedTypeVariables #-}
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
) where

import IDE.Core.State
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import Control.Monad (void, unless, when, liftM)
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
import Graphics.UI.Gtk
       (dialogSetDefaultResponse, windowWindowPosition, widgetDestroy,
        dialogRun, messageDialogNew, dialogAddButton, Window(..),
        widgetHide, DialogFlags(..))
import IDE.Pane.PackageEditor (packageNew', packageClone, choosePackageFile, standardSetup)
import Data.List (delete)
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, idePackageFromPath, idePackageFromPath)
import System.Directory
       (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.Time (getClockTime)
import Graphics.UI.Gtk.Windows.MessageDialog
    (ButtonsType(..), MessageType(..))
import Graphics.UI.Gtk.Windows.Dialog (ResponseId(..))
import qualified Control.Exception as Exc (SomeException(..), throw, Exception)
import qualified Data.Map as  Map (empty)
import IDE.Pane.SourceBuffer (fileOpenThis, fileCheckAll, belongsToPackages)
import qualified System.IO.UTF8 as UTF8 (writeFile)
import System.Glib.Attributes (AttrOp(..), set)
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
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
import Text.Printf (printf)
import System.Log.Logger (debugM)
import IDE.Pane.Log (showDefaultLogLaunch', getLog)
import IDE.LogRef (logOutputDefault)
import Data.Foldable (forM_)

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
                            wsName = takeBaseName cPath,
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
            resp <- liftIO $ do
                defaultExists <- doesFileExist defaultWorkspace
                md <- messageDialogNew (Just mainWindow) [DialogModal] MessageQuestion ButtonsCancel (
                        __ "You need to have a workspace open for this to work. "
                     ++ __ "Choose ~/leksah.lkshw to "
                     ++ __ (if defaultExists then "open workspace " else "create a workspace ")
                     ++ defaultWorkspace)
                dialogAddButton md (__ "_New Workspace") (ResponseUser 1)
                dialogAddButton md (__ "_Open Workspace") (ResponseUser 2)
                dialogAddButton md "~/leksah.lkshw" (ResponseUser 3)
                dialogSetDefaultResponse md (ResponseUser 3)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetHide md
                return resp
            case resp of
                ResponseUser 1 -> do
                    workspaceNew
                    postAsyncIDE $ workspaceTryQuiet f
                ResponseUser 2 -> do
                    workspaceOpen
                    postAsyncIDE $ workspaceTryQuiet f
                ResponseUser 3 -> do
                    defaultExists <- liftIO $ doesFileExist defaultWorkspace
                    if defaultExists
                        then workspaceOpenThis True (Just defaultWorkspace)
                        else workspaceNewHere defaultWorkspace
                    postAsyncIDE $ workspaceTryQuiet f
                _  -> return ()

chooseWorkspaceFile :: Window -> IO (Maybe FilePath)
chooseWorkspaceFile window = chooseFile window (__ "Select leksah workspace file (.lkshw)") Nothing

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
                        liftIO $ do
                            md  <- messageDialogNew (Just window) [] MessageQuestion ButtonsNone
                                    $ __ "There are session settings stored with this workspace."
                            dialogAddButton md (__ "_Ignore Session") ResponseCancel
                            dialogAddButton md (__ "_Load Session") ResponseYes
                            dialogSetDefaultResponse md ResponseYes
                            set md [ windowWindowPosition := WinPosCenterOnParent ]
                            rid <- dialogRun md
                            widgetDestroy md
                            case rid of
                                ResponseYes ->  return True
                                otherwise   ->  return False
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
                                (ideMessage Normal (printf (__ "Can't load workspace file %s\n%s") filePath (show e))) ideR)


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
            triggerEventIDE (SaveSession (dropExtension (wsFile ws) ++ leksahSessionFileExtension))
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
            constructAndOpenMainModule mbPack
        void (triggerEventIDE UpdateWorkspaceInfo))

workspacePackageClone :: WorkspaceAction
workspacePackageClone = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    lift $ packageClone path logOutputDefault (\fp -> do
        window     <-  getMainWindow
        workspaceTry $ void (workspaceAddPackage' fp)
        void (triggerEventIDE UpdateWorkspaceInfo))

constructAndOpenMainModule :: Maybe IDEPackage -> IDEAction
constructAndOpenMainModule Nothing = return ()
constructAndOpenMainModule (Just idePackage) =
    forM_ (ipdMain idePackage) $ \(target, bi, isTest) -> do
        mbPD <- getPackageDescriptionAndPath
        case mbPD of
            Just (pd,_) ->
                case hsSourceDirs bi of
                    path:_ -> do
                        liftIO $ createDirectoryIfMissing True path
                        alreadyExists <- liftIO $ doesFileExist (path </> target)
                        unless alreadyExists $ do
                            template <- liftIO $ getModuleTemplate "main" pd "Main" "" ""
                            liftIO $ UTF8.writeFile (path </> target) template
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
                resp <- liftIO $ do
                    md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                            (__ "You need to have an active package for this to work.")
                    dialogAddButton md (__ "_New Package") (ResponseUser 1)
                    dialogAddButton md (__ "_Add Package") (ResponseUser 2)
                    dialogSetDefaultResponse md (ResponseUser 2)
                    set md [ windowWindowPosition := WinPosCenterOnParent ]
                    resp <- dialogRun md
                    widgetHide md
                    return resp
                case resp of
                    ResponseUser 1 -> do
                        workspacePackageNew
                        lift $ postAsyncIDE $ packageTryQuiet f
                    ResponseUser 2 -> do
                        workspaceAddPackage
                        lift $ postAsyncIDE $ packageTryQuiet f
                    _  -> return ()

workspaceRemovePackage :: IDEPackage -> WorkspaceAction
workspaceRemovePackage pack = do
    ws <- ask
    when (pack `elem` wsPackages ws) $ lift $
        Writer.writeWorkspace ws {wsPackages =  delete pack (wsPackages ws)}
    return ()

workspaceActivatePackage :: IDEPackage -> Maybe String -> WorkspaceAction
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

buildSteps :: Bool -> IDEM [MakeOp]
buildSteps runTests = do
    debug <- isJust <$> readIDE debugState
    return $ case (runTests, debug) of
                (True, True)   -> [MoBuild,MoDocu]
                (True, False)  -> [MoBuild,MoDocu,MoTest,MoCopy,MoRegister]
                (False, True)  -> [MoBuild]
                (False, False) -> [MoBuild,MoCopy,MoRegister]

workspaceMake :: WorkspaceAction
workspaceMake = do
    ws <- ask
    settings <- lift $ do
        prefs' <- readIDE prefs
        return ((defaultMakeSettings prefs'){
                    msMakeMode           = True,
                    msBackgroundBuild    = False})
    build <- lift . buildSteps $ msRunUnitTests settings
    let steps = MoComposed (MoConfigure : build)
    makePackages settings (wsPackages ws) steps steps MoMetaInfo

backgroundMake :: IDEAction
backgroundMake = catchIDE (do
    ideR        <- ask
    prefs       <- readIDE prefs
    mbPackage   <- readIDE activePack
    debug       <- isJust <$> readIDE debugState
    case mbPackage of
        Nothing         -> return ()
        Just package    -> do
            modifiedPacks <- if saveAllBeforeBuild prefs
                                then fileCheckAll belongsToPackages
                                else return []
            let isModified = not (null modifiedPacks)
            when isModified $ do
                let settings = defaultMakeSettings prefs
                steps <- buildSteps $ msRunUnitTests settings
                workspaceTryQuiet $ if debug || msSingleBuildWithoutLinking settings && not (msMakeMode settings)
                    then makePackages settings modifiedPacks (MoComposed steps) (MoComposed []) moNoOp
                    else makePackages settings modifiedPacks (MoComposed steps)
                                        (MoComposed (MoConfigure:steps)) MoMetaInfo
    )
    (\(e :: Exc.SomeException) -> sysMessage Normal (show e))

makePackage ::  PackageAction
makePackage = do
  p <- ask
  liftIDE $ do
    getLog >>= liftIO . bringPaneToFront
    showDefaultLogLaunch'
    prefs' <- readIDE prefs
    mbWs   <- readIDE workspace
    let settings = (defaultMakeSettings prefs'){msBackgroundBuild = False}
    case mbWs of
        Nothing -> sysMessage Normal (__ "No workspace for build.")
        Just ws -> do
            debug <- isJust <$> readIDE debugState
            steps <- buildSteps $ msRunUnitTests settings
            if debug || msSingleBuildWithoutLinking settings && not (msMakeMode settings)
                then runWorkspace
                        (makePackages settings [p] (MoComposed steps) (MoComposed []) moNoOp) ws
                else
                    runWorkspace
                        (makePackages settings [p]
                        (MoComposed steps)
                        (MoComposed (MoConfigure:steps))
                        MoMetaInfo) ws
