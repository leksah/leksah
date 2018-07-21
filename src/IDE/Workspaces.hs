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
,   projectNew
,   workspaceOpen
,   projectOpen
,   workspaceTry
,   workspaceOpenThis
,   projectOpenThis
,   workspaceClose
,   workspaceClean
,   workspaceMake
,   workspaceActivatePackage
,   projectAddPackage
,   projectAddPackage'
,   workspaceRemoveProject
,   projectRemovePackage
,   workspacePackageNew
,   projectPackageClone
,   workspaceTryQuiet
,   workspaceNewHere
,   projectNewHere
,   projectTry
,   projectTryQuiet
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
import Data.Maybe (listToMaybe, isJust, fromJust, catMaybes)
import IDE.Utils.GUIUtils
       (showDialog, showDialogOptions, chooseFile, chooseSaveFile, __)
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory, (<.>))
import qualified Text.PrettyPrint as  PP (text)
import IDE.Pane.PackageEditor (packageNew', packageClone, choosePackageFile, standardSetup)
import Data.List (delete)
import IDE.Package
       (getModuleTemplate, idePackageFromPath',
        getPackageDescriptionAndPath, activatePackage, deactivatePackage,
        ideProjectFromPath)
import System.Directory
       (doesDirectoryExist, getDirectoryContents, getHomeDirectory,
        createDirectoryIfMissing, doesFileExist)
import System.Time (getClockTime)
import qualified Control.Exception as Exc (SomeException(..), throw, Exception)
import qualified Data.Map as  Map (empty)
import IDE.Pane.SourceBuffer
       (belongsToWorkspace, IDEBuffer(..), maybeActiveBuf, fileOpenThis,
        fileCheckAll, belongsToPackages', belongsToPackage)
import Control.Applicative ((<$>))
import IDE.Build
import IDE.Utils.FileUtils(myCanonicalizePath)
import Control.Monad.Trans.Reader (ask, asks)
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
import qualified Data.Text as T
       (unlines, isPrefixOf, lines, unpack, pack)
import Data.Monoid ((<>))
import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)
import qualified Data.Text.IO as T (readFile, writeFile)
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
import Data.GI.Base (set, new')
import GI.Gtk.Objects.Widget
       (widgetShow, widgetDestroy, widgetHide)
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Interfaces.FileChooser
       (fileChooserGetFilename, fileChooserSetCurrentFolder,
        fileChooserSetAction)
import IDE.Workspaces.Writer (WorkspaceFile(..))
import qualified Data.Map as M (member, delete, adjust, insert)
import qualified Data.ByteString.Lazy as LBS (writeFile)
import Data.Aeson.Encode.Pretty (encodePretty)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Constructs a new workspace and makes it the current workspace
workspaceNew :: IDEAction
workspaceNew = do
    window <- getMainWindow
    mbFile <- liftIO $ chooseSaveFile window (__ "New file for workspace") Nothing
    forM_ mbFile workspaceNewHere

projectNew :: WorkspaceAction
projectNew = do
    window <- liftIDE getMainWindow
    mbFile <- liftIO $ chooseSaveFile window (__ "New cabal.project or stack.yaml") Nothing
    forM_ mbFile projectNewHere

workspaceNewHere :: FilePath -> IDEAction
workspaceNewHere filePath =
    let realPath = if takeExtension filePath == leksahWorkspaceFileExtension
                            then filePath
                            else addExtension filePath leksahWorkspaceFileExtension
    in do
        dir <- liftIO $ myCanonicalizePath $ dropFileName realPath
        let cPath = dir </> takeFileName realPath
            newWorkspace = Writer.emptyWorkspaceFile {
                            wsfName = T.pack $ takeBaseName cPath
                            }
        liftIO . LBS.writeFile cPath $ encodePretty newWorkspace
        workspaceOpenThis False cPath
        return ()

projectNewHere :: FilePath -> WorkspaceAction
projectNewHere filePath =
    let realPath
          | takeExtension filePath == ".project" || takeExtension filePath == ".yaml"
            = filePath
          | takeBaseName filePath == "cabal" = filePath <.> "project"
          | otherwise = filePath <.> "yaml"
    in do
        dir <- liftIO $ myCanonicalizePath $ dropFileName realPath
        let cPath = dir </> takeFileName realPath
        liftIO (doesFileExist cPath) >>= \case
            True -> ideMessage Normal $ __ "Project already exists : " <> T.pack cPath
            False -> do
                liftIO $ T.writeFile cPath "packages:\n"
                projectOpenThis cPath

workspaceOpen :: IDEAction
workspaceOpen = do
    window     <- getMainWindow
    liftIO (chooseWorkspaceFile window) >>= mapM_ (workspaceOpenThis True)

projectOpen :: WorkspaceAction
projectOpen = do
    window     <- liftIDE getMainWindow
    liftIO (chooseProjectFile window) >>= mapM_ projectOpenThis

workspaceTryQuiet :: WorkspaceAction -> IDEAction
workspaceTryQuiet f =
    readIDE workspace >>= \case
        Just ws -> runWorkspace f ws
        Nothing -> ideMessage Normal (__ "No workspace open")

projectTryQuiet :: ProjectAction -> IDEAction
projectTryQuiet f = workspaceTryQuiet $
    readIDE activeProject >>= \case
        Just project -> runProject f project
        Nothing      -> ideMessage Normal (__ "No project active")

workspaceTry :: WorkspaceAction -> IDEAction
workspaceTry f =
    readIDE workspace >>= \case
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
                        then workspaceOpenThis True defaultWorkspace
                        else workspaceNewHere defaultWorkspace
                    postAsyncIDE $ workspaceTryQuiet f
                _  -> return ()

projectTry :: ProjectAction -> IDEAction
projectTry f = workspaceTry $
    readIDE activeProject >>= \case
        Just project -> runProject f project
        Nothing -> do
            mainWindow <- liftIDE getMainWindow
            defaultCabal <- (</> "cabal.project") . takeDirectory <$> asks wsFile
            defaultStack <- (</> "stack.yaml") . takeDirectory <$> asks wsFile
            defaultCabalExists <- liftIO $ doesFileExist defaultCabal
            md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeCancel]
            setWindowModal md True
            setMessageDialogMessageType md MessageTypeQuestion
            setMessageDialogText md (
                    __ "You need to have a project open for this to work. "
                 <> __ "Choose cabal.project to "
                 <> __ (if defaultCabalExists then "open project " else "create a workspace ")
                 <> T.pack defaultCabal)
            windowSetTransientFor md (Just mainWindow)
            dialogAddButton' md (__ "_New Project") (AnotherResponseType 1)
            dialogAddButton' md (__ "_Open Project") (AnotherResponseType 2)
            dialogAddButton' md "cabal.project" (AnotherResponseType 3)
            dialogAddButton' md "stack.yaml" (AnotherResponseType 4)
            dialogSetDefaultResponse' md (AnotherResponseType 3)
            setWindowWindowPosition md WindowPositionCenterOnParent
            resp <- dialogRun' md
            widgetHide md
            case resp of
                AnotherResponseType 1 -> do
                    projectNew
                    postAsyncIDE $ projectTryQuiet f
                AnotherResponseType 2 -> do
                    projectOpen
                    postAsyncIDE $ projectTryQuiet f
                AnotherResponseType 3 -> do
                    defaultExists <- liftIO $ doesFileExist defaultCabal
                    if defaultExists
                        then projectOpenThis defaultCabal
                        else projectNewHere defaultCabal
                    postAsyncIDE $ projectTryQuiet f
                _  -> return ()

chooseWorkspaceFile :: Window -> IO (Maybe FilePath)
chooseWorkspaceFile window = chooseFile window (__ "Select leksah workspace file (.lkshw)") Nothing [("Leksah Workspace Files", ["*.lkshw"])]

chooseProjectFile :: Window -> IO (Maybe FilePath)
chooseProjectFile window = chooseFile window (__ "Select cabal.project or stack.yaml file") Nothing [("Haskell Project", ["*.project", "*.yaml"])]

workspaceOpenThis :: Bool -> FilePath -> IDEAction
workspaceOpenThis askForSession filePath = do
    liftIO . debugM "leksah" $ "workspaceOpenThis " ++ show askForSession ++ " " ++ filePath
    getMainWindow >>= open
  where
    open mainWindow = do
        exists <- liftIO $ doesFileExist filePath
        sessionExists <- liftIO $ doesFileExist sessionPath
        if exists
            then workspaceClose >> if askForSession && sessionExists
                then sessionDialog
                else openWithoutSession
            else
                showDialog (Just mainWindow) ("Could not find workspace file at " <> T.pack filePath) MessageTypeError
      where
        sessionPath = dropExtension filePath ++ leksahSessionFileExtension

        openWithoutSession :: IDEAction
        openWithoutSession =
            catchIDE (
                Writer.readWorkspace filePath >>= \case
                    Left errorMsg -> showDialog (Just mainWindow) (T.pack $ "Could not open " <> filePath <> ". " <> errorMsg) MessageTypeError
                    Right workspace -> do
                        Writer.setWorkspace (Just workspace {wsFile = filePath})
                        VCSWS.onWorkspaceOpen workspace)
                   (\ (e :: Exc.SomeException) ->
                        ideMessage Normal (T.pack $ printf (__ "Can't load workspace file %s\n%s") filePath (show e)))

        sessionDialog :: IDEAction
        sessionDialog = showDialogOptions
                            (Just mainWindow)
                            "There are session settings stored with this workspace."
                            MessageTypeQuestion
                            [ ("_Ignore Session", openWithoutSession)
                            , ("_Load Session"  , void (triggerEventIDE (LoadSession sessionPath))) -- Also loads the workspace from that session
                            ]
                            Nothing

projectOpenThis :: FilePath -> WorkspaceAction
projectOpenThis filePath = do
    liftIO . debugM "leksah" $ "projectOpenThis " ++ filePath
    dir <- liftIO $ myCanonicalizePath $ dropFileName filePath
    let cPath = dir </> takeFileName filePath
    liftIO (doesFileExist cPath) >>= \case
        False -> ideMessage Normal $ __ "Project does not exists : " <> T.pack cPath
        True -> do
            ws <- ask
            liftIDE (ideProjectFromPath filePath) >>= \case
                Nothing -> ideMessage Normal $ __ "Unable to load project : " <> T.pack cPath
                Just project ->
                    lift $ Writer.writeWorkspace $ ws {
                        wsProjects = project : wsProjects ws
                      , wsActiveProjectFile = Just cPath
                      , wsActivePackFile = ipdCabalFile <$> listToMaybe (pjPackages project)
                      , wsActiveComponent = Nothing }

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
    liftIDE . packageNew' path (wsProjects ws) logOutputDefault $ \isNew mbProject fp -> do
        liftIO $ debugM "leksah" $ "workspacePackageNew " <> show (isNew, pjFile <$> mbProject, fp)
        case mbProject of
            Just project -> workspaceTryQuiet . (`runProject` project) . void $ projectAddPackage' fp
            Nothing -> postAsyncIDE . workspaceTryQuiet $ do
                let packageDir = dropFileName fp
                liftIO (doesFileExist $ packageDir </> "cabal.project") >>= \case
                    True -> projectOpenThis (packageDir </> "cabal.project")
                    False ->
                        liftIO (doesFileExist $ packageDir </> "stack.yaml") >>= \case
                            True -> projectOpenThis (packageDir </> "stack.yaml")
                            False -> do
                                liftIO $ debugM "leksah" $ "workspacePackageNew show project creation dialog"
                                window <- liftIDE getMainWindow
                                md <- new' MessageDialog [
                                    constructDialogUseHeaderBar 0,
                                    constructMessageDialogButtons ButtonsTypeCancel]
                                setMessageDialogMessageType md MessageTypeQuestion
                                setMessageDialogText md $ __ "No project file found for this package would you like to add one?"
                                windowSetTransientFor md (Just window)
                                dialogAddButton' md (__ "Add cabal.project") (AnotherResponseType 1)
                                dialogAddButton' md (__ "Add stack.yaml") (AnotherResponseType 2)
                                dialogSetDefaultResponse' md (AnotherResponseType 2)
                                setWindowWindowPosition md WindowPositionCenterOnParent
                                resp <- dialogRun' md
                                widgetHide md
                                case resp of
                                    AnotherResponseType 1 -> do
                                        liftIO $ T.writeFile (packageDir </> "cabal.project") "packages:\n ./\n"
                                        projectOpenThis (packageDir </> "cabal.project")
                                    AnotherResponseType 2 -> do
                                        liftIO $ T.writeFile (packageDir </> "stack.yaml") "packages:\n- '.'\n"
                                        projectOpenThis (packageDir </> "stack.yaml")
                                    _  -> return ()
        when isNew $ do
            mbPack <- liftIDE $ idePackageFromPath' fp
            liftIDE $ constructAndOpenMainModules mbPack
        liftIDE $ triggerEventIDE_ (UpdateWorkspaceInfo False)

projectPackageClone :: ProjectAction
projectPackageClone = do
    project <- ask
    let path = dropFileName (pjFile project)
    liftIDE . packageClone path logOutputDefault $ \fp ->
        projectTry $ do
            projectAddPackage' fp
            triggerEventIDE_ (UpdateWorkspaceInfo False)

constructAndOpenMainModules :: Maybe IDEPackage -> IDEAction
constructAndOpenMainModules Nothing = return ()
constructAndOpenMainModules (Just idePackage) =
    forM_ (ipdMain idePackage) $ \(target, bi, isTest) -> do
        mbPD <- getPackageDescriptionAndPath
        case mbPD of
            Just (pd,_) ->
                case hsSourceDirs bi of
                    path':_ -> do
                        let path = ipdPackageDir idePackage </> path'
                        liftIO $ createDirectoryIfMissing True path
                        alreadyExists <- liftIO $ doesFileExist (path </> target)
                        unless alreadyExists $ do
                            template <- liftIO $ getModuleTemplate (if isTest then "testmain" else "main") pd "Main" "" ""
                            liftIO $ T.writeFile (path </> target) template
                            fileOpenThis (path </> target)
                    _ -> return ()
            Nothing     -> ideMessage Normal (__ "No package description")

projectAddPackage :: ProjectAction
projectAddPackage = do
    project <- ask
    let path = dropFileName (pjFile project)
    window <-  liftIDE getMainWindow
    liftIO (choosePackageFile window (Just path)) >>= \case
        Nothing -> return ()
        Just fp -> do
            void (projectAddPackage' fp)
            liftIDE $ triggerEventIDE_ (UpdateWorkspaceInfo False)

projectAddPackage' :: FilePath -> ProjectM (Maybe IDEPackage)
projectAddPackage' fp = do
    project <- ask
    ws <- lift ask
    let projectFile = pjFile project
    cfp <- liftIO $ myCanonicalizePath fp
    liftIDE (idePackageFromPath' cfp) >>= \case
        Just pack -> do
            let indent = case pjTool project of
                                    StackTool -> "- "
                                    CabalTool -> " "
            projectText <- liftIO $ T.readFile projectFile
            let projectLines = T.lines projectText
                relativePath = makeRelative (dropFileName projectFile) (dropFileName cfp)
            case span (/= "packages:") projectLines of
                (before, _:rest) ->
                    case span (indent `T.isPrefixOf`) rest of
                        (packs, rest') -> liftIO $ T.writeFile projectFile . T.unlines $
                            before <> ("packages:":packs) <> [indent <> T.pack relativePath] <> rest'
                _ -> return ()
            unless (cfp `elem` map ipdCabalFile (pjPackages project)) $ liftIDE $
                Writer.writeWorkspace $ ws {
                    wsProjects = map (\p -> if pjFile p == projectFile
                                                then p { pjPackageMap = M.insert (ipdCabalFile pack) pack $ pjPackageMap p }
                                                else p)  (wsProjects ws)
                  , wsActivePackFile = Just (ipdCabalFile pack)
                  , wsActiveComponent = Nothing}
            return (Just pack)
        Nothing -> return Nothing

packageTryQuiet :: PackageAction -> IDEAction
packageTryQuiet f = do
    maybePackage <- readIDE activePack
    case maybePackage of
        Just p  -> projectTryQuiet $ runPackage f p
        Nothing -> ideMessage Normal (__ "No active package")

packageTry :: PackageAction -> IDEAction
packageTry f = workspaceTry $ do
        maybeProject <- lift $ readIDE activeProject
        maybePackage <- lift $ readIDE activePack
        case (maybeProject, maybePackage) of
            (Just project, Just package)  -> runProject (runPackage f package) project
            _ -> do
                window <- liftIDE getMainWindow
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
                        workspacePackageNew
                        lift $ postAsyncIDE $ packageTryQuiet f
                    _  -> return ()

workspaceRemoveProject :: FilePath -> WorkspaceAction
workspaceRemoveProject projectFile = do
    ws <- ask
    when (any ((/= projectFile) . pjFile) $ wsProjects ws) . lift $
        Writer.writeWorkspace ws {wsProjects = filter ((/= projectFile) . pjFile) $ wsProjects ws}

projectRemovePackage :: IDEPackage -> ProjectAction
projectRemovePackage pack = do
--    ws <- ask
--    when (pack `elem` wsPackages ws) $ lift $
--        Writer.writeWorkspace ws {wsProjects =  delete pack (wsPackages ws)}
    ideMessage Normal "projectRemovePackage not implemented"

workspaceActivatePackage :: Project -> Maybe IDEPackage -> Maybe Text -> WorkspaceAction
workspaceActivatePackage project mbPack exe = do
    (mbPackFile, mbExe) <- liftIDE $ case mbPack of
        Just pack | ipdCabalFile pack `elem` map ipdCabalFile (pjPackages project) -> do
            activatePackage (Just (ipdCabalFile pack)) (Just project) (Just pack) exe
            return (Just (ipdCabalFile pack), exe)
        _ -> do
            activatePackage Nothing (Just project) Nothing Nothing
            return (Nothing, Nothing)
    ws <- ask
    let projects = project : filter ((/= pjFile project) . pjFile) (wsProjects ws)
    liftIDE $ Writer.writeWorkspace ws
                             {wsProjects = projects
                             ,wsActiveProjectFile = Just (pjFile project)
                             ,wsActivePackFile = mbPackFile
                             ,wsActiveComponent = mbExe}

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
    lift $ makePackages settings (map (\p -> (p, pjPackages p)) $ wsProjects ws) MoClean MoClean moNoOp

buildSteps :: MakeSettings -> IDEM [MakeOp]
buildSteps settings =
    return $ MoBuild
         : [MoDocu    | msMakeDocs settings]
        ++ [MoTest    | msRunUnitTests settings]
        ++ [MoInstall]
        ++ [MoBench   | msRunBenchmarks settings]

workspaceMake :: WorkspaceAction
workspaceMake = do
    ws <- ask
    settings <- lift $ do
        prefs' <- readIDE prefs
        return ((defaultMakeSettings prefs'){
                    msMakeMode           = True,
                    msBackgroundBuild    = False})
    build <- lift . buildSteps $ settings
    lift $ makePackages settings (map (\p -> (p, pjPackages p)) $ wsProjects ws) (MoComposed build) (MoComposed build) MoMetaInfo

backgroundMake :: IDEAction
backgroundMake = catchIDE (do
    ideR        <- ask
    prefs       <- readIDE prefs
    modifiedFiles <- catMaybes <$> if saveAllBeforeBuild prefs
                        then fileCheckAll (return . return . fileName)
                        else return []
    unless (null modifiedFiles) . postAsyncIDE . workspaceTryQuiet $ do
        ws <- ask
        liftIDE $ do
            let modifiedPacks = filter (not . null . snd) $
                                  map (\project -> (project, filter (\pack -> any (`belongsToPackage` pack) modifiedFiles) (pjPackages project))) (wsProjects ws)
                settings = defaultMakeSettings prefs
            steps <- buildSteps settings
            if msSingleBuildWithoutLinking settings && not (msMakeMode settings)
                then makePackages settings modifiedPacks (MoComposed steps) (MoComposed []) moNoOp
                else makePackages settings modifiedPacks (MoComposed steps)
                                    (MoComposed steps) MoMetaInfo
    )
    (\(e :: Exc.SomeException) -> sysMessage Normal (T.pack $ show e))

makePackage :: PackageAction
makePackage = do
  liftIO $ debugM "leksah" "makePackage'"
  p <- ask
  project <- lift ask
  liftIDE $ do
    getLog >>= bringPaneToFront
    showDefaultLogLaunch'
    prefs' <- readIDE prefs
    mbWs   <- readIDE workspace
    let settings = (defaultMakeSettings prefs')
            { msBackgroundBuild = False
            , msSuccessAction = when (ipdPackageName p == "leksah" && native prefs') $
                readIDE developLeksah >>= \case
                    False -> return ()
                    True -> triggerEventIDE_ QuitToRestart }
    steps <- buildSteps settings
    if msSingleBuildWithoutLinking settings && not (msMakeMode settings)
        then makePackages settings [(project, [p])] (MoComposed steps) (MoComposed []) moNoOp
        else makePackages settings [(project, [p])] (MoComposed steps) (MoComposed steps) MoMetaInfo

fileOpen :: IDEAction
fileOpen = do
    window <- getMainWindow
    prefs <- readIDE prefs
    mbBuf <- maybeActiveBuf
    dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
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
        fileChooserGetFilename dialog >>= mapM_ fileOpen'
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
                    <> __ " to the active project so that it can be built by Leksah?")
                windowSetTransientFor md (Just window)
                dialogAddButton' md (__ "_Add " <> T.pack (takeFileName fp)) (AnotherResponseType 1)
                dialogAddButton' md (__ "Just _open " <> T.pack (takeFileName fp)) (AnotherResponseType 2)
                dialogSetDefaultResponse' md (AnotherResponseType 1)
                resp <- dialogRun' md
                widgetDestroy md
                case resp of
                    AnotherResponseType 1 -> projectTry $ do
                        projectAddPackage' fp
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
                        <> __ " to your active project?")
                    windowSetTransientFor md (Just window)
                    dialogAddButton' md (__ "_Add " <> T.pack (takeFileName cabalFile)) (AnotherResponseType 1)
                    dialogAddButton' md (__ "Just _open " <> T.pack (takeFileName fp)) (AnotherResponseType 2)
                    dialogSetDefaultResponse' md (AnotherResponseType 1)
                    resp <- dialogRun' md
                    widgetDestroy md
                    case resp of
                        AnotherResponseType 1 -> projectTry $ do
                            projectAddPackage' cabalFile
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
