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
{-# LANGUAGE DeriveDataTypeable #-}
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
import Control.Monad (forM_, unless, when, liftM)
import Data.Maybe (isJust,fromJust )
import IDE.Utils.GUIUtils
    (chooseFile, chooseSaveFile)
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
import IDE.Pane.PackageEditor (packageNew', choosePackageFile, standardSetup)
import Data.List (delete)
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, idePackageFromPath)
import System.Directory
       (getHomeDirectory, createDirectoryIfMissing, doesFileExist)
import System.Time (getClockTime)
import Graphics.UI.Gtk.Windows.MessageDialog
    (ButtonsType(..), MessageType(..))
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Windows.Dialog (ResponseId(..))
#else
import Graphics.UI.Gtk.General.Structs (ResponseId(..))
#endif
import qualified Control.Exception as Exc (SomeException(..), throw, Exception)
import qualified Data.Map as  Map (empty)
import IDE.Pane.SourceBuffer (fileOpenThis, fileCheckAll, belongsToPackage)
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


-- | Constructs a new workspace and makes it the current workspace
workspaceNew :: IDEAction
workspaceNew = do
    window <- getMainWindow
    mbFile <- liftIO $ do
        chooseSaveFile window "New file for workspace" Nothing
    case mbFile of
        Nothing -> return ()
        Just filePath -> workspaceNewHere filePath

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
        Nothing -> ideMessage Normal "No workspace open"

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
                        "You need to have a workspace open for this to work. "
                     ++ "Choose ~/leksah.lkshw to "
                     ++ (if defaultExists then "open workspace " else "create a workspace ")
                     ++ defaultWorkspace)
                dialogAddButton md "_New Workspace" (ResponseUser 1)
                dialogAddButton md "_Open Workspace" (ResponseUser 2)
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
chooseWorkspaceFile window = chooseFile window "Select leksah workspace file (.lkshw)" Nothing

workspaceOpenThis :: Bool -> Maybe FilePath -> IDEAction
workspaceOpenThis askForSession mbFilePath =
    case mbFilePath of
        Nothing -> return ()
        Just filePath -> do
            let spath =  dropExtension filePath ++ leksahSessionFileExtension
            workspaceClose
            exists <- liftIO $ doesFileExist spath
            wantToLoadSession <-
                if exists && askForSession
                    then do
                        window <- getMainWindow
                        liftIO $ do
                            md  <- messageDialogNew (Just window) [] MessageQuestion ButtonsNone
                                    $ "There are session settings stored with this workspace."
                            dialogAddButton md "_Ignore Session" ResponseCancel
                            dialogAddButton md "_Load Session" ResponseYes
                            dialogSetDefaultResponse md ResponseYes
                            set md [ windowWindowPosition := WinPosCenterOnParent ]
                            rid <- dialogRun md
                            widgetDestroy md
                            case rid of
                                ResponseYes ->  return True
                                otherwise   ->  return False
                    else return False
            if wantToLoadSession
                then triggerEventIDE (LoadSession spath) >> return ()
                else do
                    ideR <- ask
                    catchIDE (do
                        workspace <- readWorkspace filePath
                        Writer.setWorkspace (Just workspace {wsFile = filePath})
                        VCSWS.onWorkspaceOpen workspace)
                           (\ (e :: Exc.SomeException) -> reflectIDE
                                (ideMessage Normal ("Can't load workspace file " ++ filePath ++ "\n" ++ show e)) ideR)


-- | Closes a workspace
workspaceClose :: IDEAction
workspaceClose = do
    oldWorkspace <- readIDE workspace
    case oldWorkspace of
        Nothing -> return ()
        Just ws -> do
            VCSWS.onWorkspaceClose
            let oldActivePackFile = wsActivePackFile ws
            triggerEventIDE (SaveSession ((dropExtension (wsFile ws))
                                ++  leksahSessionFileExtension))
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
    lift $ packageNew' (Just path) (\isNew fp -> do
        window     <-  getMainWindow
        workspaceTry $ workspaceAddPackage' fp >> return ()
        when isNew $ do
            mbPack <- idePackageFromPath fp
            constructAndOpenMainModule mbPack
        triggerEventIDE UpdateWorkspaceInfo >> return ())

constructAndOpenMainModule :: Maybe IDEPackage -> IDEAction
constructAndOpenMainModule Nothing = return ()
constructAndOpenMainModule (Just idePackage) =
    forM_ (ipdMain idePackage) $ \(target, bi, isTest) -> do
        mbPD <- getPackageDescriptionAndPath
        case mbPD of
            Just (pd,_) -> do
                case hsSourceDirs bi of
                    path:_ -> do
                        liftIO $ createDirectoryIfMissing True path
                        alreadyExists <- liftIO $ doesFileExist (path </> target)
                        unless alreadyExists $ do
                            template <- liftIO $ getModuleTemplate "main" pd "Main" "" ""
                            liftIO $ UTF8.writeFile (path </> target) template
                            fileOpenThis (path </> target)
                    _ -> return ()
            Nothing     -> ideMessage Normal "No package description"

workspaceAddPackage :: WorkspaceAction
workspaceAddPackage = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    window <-  lift getMainWindow
    mbFilePath <- liftIO $ choosePackageFile window (Just path)
    case mbFilePath of
        Nothing -> return ()
        Just fp -> do
            workspaceAddPackage' fp >> return ()
            lift $ triggerEventIDE UpdateWorkspaceInfo >> return ()

workspaceAddPackage' :: FilePath -> WorkspaceM (Maybe IDEPackage)
workspaceAddPackage' fp = do
    ws <- ask
    cfp <- liftIO $ myCanonicalizePath fp
    mbPack <- lift $ idePackageFromPath cfp
    case mbPack of
        Just pack -> do
            let dir = takeDirectory cfp
            b1 <- liftIO $ doesFileExist (dir </> "Setup.hs")
            b2 <- liftIO $ doesFileExist (dir </> "Setup.lhs")
            unless (b1 || b2) $ liftIO $ do
                sysMessage Normal "Setup.(l)hs does not exist. Writing Standard"
                writeFile (dir </> "Setup.lhs") standardSetup
            unless (elem cfp (map ipdCabalFile (wsPackages ws))) $ lift $
                Writer.writeWorkspace $ ws {wsPackages =  pack : wsPackages ws,
                                     wsActivePackFile =  Just (ipdCabalFile pack)}
            return (Just pack)
        Nothing -> return Nothing

packageTryQuiet :: PackageAction -> IDEAction
packageTryQuiet f = do
    maybePackage <- readIDE activePack
    case maybePackage of
        Just p  -> runPackage f p
        Nothing -> ideMessage Normal "No active package"

packageTry :: PackageAction -> IDEAction
packageTry f = workspaceTry $ do
        maybePackage <- lift $ readIDE activePack
        case maybePackage of
            Just p  -> lift $ runPackage f p
            Nothing -> do
                window <- lift $ getMainWindow
                resp <- liftIO $ do
                    md <- messageDialogNew (Just window) [] MessageQuestion ButtonsCancel
                            "You need to have an active package for this to work."
                    dialogAddButton md "_New Package" (ResponseUser 1)
                    dialogAddButton md "_Add Package" (ResponseUser 2)
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
    when (elem pack (wsPackages ws)) $ lift $
        Writer.writeWorkspace ws {wsPackages =  delete pack (wsPackages ws)}
    return ()

workspaceActivatePackage :: IDEPackage -> WorkspaceAction
workspaceActivatePackage pack = do
    ws <- ask
    lift $ activatePackage (Just pack)
    when (elem pack (wsPackages ws)) $ lift $ do
        Writer.writeWorkspace ws {wsActivePackFile =  Just (ipdCabalFile pack)}
        return ()
    return ()



readWorkspace :: FilePath -> IDEM Workspace
readWorkspace fp = do
    ws <- liftIO $ readFields fp Writer.workspaceDescr emptyWorkspace
    ws' <- liftIO $ makePathesAbsolute ws fp
    packages <- mapM idePackageFromPath (wsPackagesFiles ws')
    --TODO set package vcs here
    return ws'{ wsPackages = map fromJust $ filter isJust $ packages}




makePathesAbsolute :: Workspace -> FilePath -> IO Workspace
makePathesAbsolute ws bp = do
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
            if isAbsolute relativePath
                then myCanonicalizePath relativePath
                else myCanonicalizePath (basePath </> relativePath)

emptyWorkspace =  Workspace {
    wsVersion       =   Writer.workspaceVersion
,   wsSaveTime      =   ""
,   wsName          =   ""
,   wsFile          =   ""
,   wsPackages      =   []
,   wsPackagesFiles =   []
,   wsActivePackFile =   Nothing
,   wsNobuildPack   =   []
,   packageVcsConf  =   Map.empty
}



addRecentlyUsedWorkspace :: FilePath -> IDEAction
addRecentlyUsedWorkspace fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentWorkspaces' <- readIDE recentWorkspaces
        unless (elem fp recentWorkspaces') $
            modifyIDE_ (\ide -> ide{recentWorkspaces = take 12 (fp : recentWorkspaces')})
        triggerEventIDE UpdateRecent
        return ()

removeRecentlyUsedWorkspace :: FilePath -> IDEAction
removeRecentlyUsedWorkspace fp = do
    state <- readIDE currentState
    when (not $ isStartingOrClosing state) $ do
        recentWorkspaces' <- readIDE recentWorkspaces
        when (elem fp recentWorkspaces') $
            modifyIDE_ (\ide -> ide{recentWorkspaces = filter (\e -> e /= fp) recentWorkspaces'})
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
                                then fileCheckAll belongsToPackage
                                else return []
            let isModified = not (null modifiedPacks)
            when isModified $ do
                let settings = defaultMakeSettings prefs
                steps <- buildSteps $ msRunUnitTests settings
                if debug || msSingleBuildWithoutLinking settings && not (msMakeMode settings)
                    then workspaceTryQuiet $
                        makePackages settings modifiedPacks (MoComposed steps) (MoComposed []) moNoOp
                    else do
                        workspaceTryQuiet $
                            makePackages settings modifiedPacks (MoComposed steps)
                                        (MoComposed (MoConfigure:steps)) MoMetaInfo
    )
    (\(e :: Exc.SomeException) -> sysMessage Normal (show e))

makePackage ::  PackageAction
makePackage = do
  p <- ask
  lift $ do
    prefs' <- readIDE prefs
    mbWs   <- readIDE workspace
    let settings = (defaultMakeSettings prefs'){msBackgroundBuild = False}
    case mbWs of
        Nothing -> sysMessage Normal "No workspace for build."
        Just ws -> do
            debug <- isJust <$> readIDE debugState
            steps <- buildSteps $ msRunUnitTests settings
            if debug || msSingleBuildWithoutLinking settings && not (msMakeMode settings)
                then runWorkspace
                        (makePackages settings [p] (MoComposed steps) (MoComposed []) moNoOp) ws
                else do
                    runWorkspace
                        (makePackages settings [p]
                        (MoComposed steps)
                        (MoComposed (MoConfigure:steps))
                        MoMetaInfo) ws
