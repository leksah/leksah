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
module IDE.Gtk.Workspaces (
    workspaceNew
,   projectNew
,   workspaceOpen
,   projectOpen
,   workspaceTry
,   workspaceOpenThis
,   workspaceClose
,   projectAddPackage
,   workspacePackageNew
,   projectPackageClone
,   workspaceNewHere
,   projectTry
,   packageTry

,   makePackage
,   fileOpen
,   fileOpen'
) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<$>))
import Control.Exception (SomeException(..), catch)
import Control.Lens ((^.), (.~))
import Control.Monad (filterM, void, unless, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask, asks)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS (writeFile)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
       (unpack, pack)
import qualified Data.Text.IO as T (writeFile)

import qualified Text.Printf as S (printf)
import Text.Printf (PrintfType)

import System.Directory
       (doesDirectoryExist, getDirectoryContents, getHomeDirectory,
        doesFileExist)
import System.FilePath
       (takeFileName, (</>), dropFileName,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory)
import System.Log.Logger (debugM)

import Data.GI.Base (new')
import GI.Gtk.Enums
       (FileChooserAction(..), ResponseType(..), ButtonsType(..),
        WindowPosition(..), MessageType(..))
import GI.Gtk.Interfaces.FileChooser
       (fileChooserGetFilename, fileChooserSetCurrentFolder,
        fileChooserSetAction)
import GI.Gtk.Objects.Dialog (constructDialogUseHeaderBar)
import GI.Gtk.Objects.FileChooserDialog (FileChooserDialog(..))
import GI.Gtk.Objects.MessageDialog
       (setMessageDialogText, constructMessageDialogButtons, setMessageDialogMessageType,
        MessageDialog(..))
import GI.Gtk.Objects.Widget
       (widgetShow, widgetDestroy, widgetHide)
import GI.Gtk.Objects.Window
       (windowSetTransientFor, setWindowTitle, Window(..),
        setWindowWindowPosition, setWindowModal)

import Graphics.UI.Editor.Parameters
       (dialogRun', dialogSetDefaultResponse', dialogAddButton')

import IDE.Core.State
       (modifyIDE_, recentWorkspaces, currentState,
        runPackage, activePack, triggerEventIDE_, saveSessionOnClose,
        prefs, wsActivePackFile, triggerEventIDE, MessageLevel(..),
        ideMessage, catchIDE, wsFile, runProject, activeProject,
        runWorkspace, workspace, readIDE, IDEAction,
        PackageAction, ProjectAction, IDEM, WorkspaceAction,
        IDEEvent(..), Project(..), wsProjects, SensitivityMask(..),
        MonadIDE(..), leksahSessionFileExtension, leksahWorkspaceFileExtension)
import IDE.Gtk.State
       (isStartingOrClosing, postAsyncIDE, bringPaneToFront, getMainWindow)
import IDE.Command.VCS.Common.Workspaces as VCSWS
import IDE.LogRef (logOutputDefault)
import IDE.Package
       (idePackageFromPath')
import IDE.Pane.Log (getLog)
import IDE.Pane.SourceBuffer
       (belongsToWorkspace, IDEBuffer(..), maybeActiveBuf, fileOpenThis)
import IDE.Pane.PackageEditor (packageNew', packageClone, choosePackageFile)
import IDE.Utils.GUIUtils
       (showDialog, showDialogOptions, chooseFile, chooseSaveFile, __)
import IDE.Utils.FileUtils (myCanonicalizePath)
import IDE.Workspaces.Writer (WorkspaceFile(..))
import qualified IDE.Workspaces.Writer as Writer
import IDE.Workspaces
       (projectNewHere, projectOpenThis, workspaceTryQuiet, projectTryQuiet,
        projectAddPackage', constructAndOpenMainModules, makePackage', packageTryQuiet)

printf :: PrintfType r => Text -> r
printf = S.printf . T.unpack

-- | Constructs a new workspace and makes it the current workspace
workspaceNew :: IDEAction
workspaceNew = do
    win <- getMainWindow
    mbFile <- liftIO $ chooseSaveFile win (__ "New file for workspace") Nothing
    forM_ mbFile workspaceNewHere

projectNew :: WorkspaceAction
projectNew = do
    win <- liftIDE getMainWindow
    mbFile <- liftIO $ chooseSaveFile win (__ "New cabal.project or stack.yaml") Nothing
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

workspaceOpen :: IDEAction
workspaceOpen = do
    win <- getMainWindow
    liftIO (chooseWorkspaceFile win) >>= mapM_ (workspaceOpenThis True)

projectOpen :: WorkspaceAction
projectOpen = do
    win <- liftIDE getMainWindow
    liftIO (chooseProjectFile win) >>= mapM_ projectOpenThis

workspaceTry' :: IDEM Int -> WorkspaceAction -> IDEAction
workspaceTry' promptUser f =
    readIDE workspace >>= \case
        Just ws -> runWorkspace f ws
        Nothing ->
            promptUser >>= \case
                1 -> do
                    workspaceNew
                    postAsyncIDE $ workspaceTryQuiet f
                2 -> do
                    workspaceOpen
                    postAsyncIDE $ workspaceTryQuiet f
                3 -> do
                    defaultWorkspace <- liftIO $ (</> "leksah.lkshw") <$> getHomeDirectory
                    liftIO (doesFileExist defaultWorkspace) >>= \case
                        True -> workspaceOpenThis True defaultWorkspace
                        False -> workspaceNewHere defaultWorkspace
                    postAsyncIDE $ workspaceTryQuiet f
                _  -> return ()

workspaceTry :: WorkspaceAction -> IDEAction
workspaceTry =
    workspaceTry' $ do
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
        _ <- dialogAddButton' md (__ "_New Workspace") (AnotherResponseType 1)
        _ <- dialogAddButton' md (__ "_Open Workspace") (AnotherResponseType 2)
        _ <- dialogAddButton' md "~/leksah.lkshw" (AnotherResponseType 3)
        dialogSetDefaultResponse' md (AnotherResponseType 3)
        setWindowWindowPosition md WindowPositionCenterOnParent
        resp <- dialogRun' md
        widgetHide md
        return $ case resp of
            AnotherResponseType n -> n
            _ -> 0

projectTry' :: IDEM Int -> ProjectAction -> WorkspaceAction
projectTry' promptUser f =
    readIDE activeProject >>= \case
        Just project -> runProject f project
        Nothing ->
            liftIDE promptUser >>= \case
                1 -> do
                    projectNew
                    postAsyncIDE $ projectTryQuiet f
                2 -> do
                    projectOpen
                    postAsyncIDE $ projectTryQuiet f
                3 -> do
                    defaultCabal <- (</> "cabal.project") . takeDirectory <$> asks (^. wsFile)
                    defaultExists <- liftIO $ doesFileExist defaultCabal
                    if defaultExists
                        then projectOpenThis defaultCabal
                        else projectNewHere defaultCabal
                    postAsyncIDE $ projectTryQuiet f
                _  -> return ()

projectTry :: ProjectAction -> IDEAction
projectTry f = workspaceTry $ do
    defaultCabal <- (</> "cabal.project") . takeDirectory <$> asks (^. wsFile)
    -- defaultStack <- (</> "stack.yaml") . takeDirectory <$> asks (^. wsFile)
    projectTry' (do
        mainWindow <- liftIDE getMainWindow
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
        _ <- dialogAddButton' md (__ "_New Project") (AnotherResponseType 1)
        _ <- dialogAddButton' md (__ "_Open Project") (AnotherResponseType 2)
        _ <- dialogAddButton' md "cabal.project" (AnotherResponseType 3)
        _ <- dialogAddButton' md "stack.yaml" (AnotherResponseType 4)
        dialogSetDefaultResponse' md (AnotherResponseType 3)
        setWindowWindowPosition md WindowPositionCenterOnParent
        resp <- dialogRun' md
        widgetHide md
        return $ case resp of
            AnotherResponseType n -> n
            _ -> 0) f

chooseWorkspaceFile :: Window -> IO (Maybe FilePath)
chooseWorkspaceFile win = chooseFile win (__ "Select leksah workspace file (.lkshw)") Nothing [("Leksah Workspace Files", ["*.lkshw"])]

chooseProjectFile :: Window -> IO (Maybe FilePath)
chooseProjectFile win = chooseFile win (__ "Select cabal.project or stack.yaml file") Nothing [("Haskell Project", ["*.project", "*.yaml"])]

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
                    Right ws -> do
                        Writer.setWorkspace (Just $ ws & wsFile .~ filePath)
                        VCSWS.onWorkspaceOpen ws)
                   (\ (e :: SomeException) ->
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

-- | Closes a workspace
workspaceClose :: IDEAction
workspaceClose = do
    liftIO $ debugM "leksah" "workspaceClose"
    oldWorkspace <- readIDE workspace
    case oldWorkspace of
        Nothing -> return ()
        Just ws -> do
            VCSWS.onWorkspaceClose
            let oldActivePackFile = ws ^. wsActivePackFile
            prefs' <- readIDE prefs
            when (saveSessionOnClose prefs') $
                triggerEventIDE_ (SaveSession (dropExtension (ws ^. wsFile) ++ leksahSessionFileExtension))
            addRecentlyUsedWorkspace (ws ^. wsFile)
            Writer.setWorkspace Nothing
            when (isJust oldActivePackFile) $
                triggerEventIDE_ (Sensitivity [(SensitivityProjectActive, False),
                    (SensitivityWorkspaceOpen, False)])
    return ()

workspacePackageNew :: WorkspaceAction
workspacePackageNew = do
    ws <- ask
    let path = dropFileName (ws ^. wsFile)
    liftIDE . packageNew' path (ws ^. wsProjects) logOutputDefault $ \isNew mbProject fp -> do
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
                                liftIO $ debugM "leksah" "workspacePackageNew show project creation dialog"
                                win <- liftIDE getMainWindow
                                md <- new' MessageDialog [
                                    constructDialogUseHeaderBar 0,
                                    constructMessageDialogButtons ButtonsTypeCancel]
                                setMessageDialogMessageType md MessageTypeQuestion
                                setMessageDialogText md $ __ "No project file found for this package would you like to add one?"
                                windowSetTransientFor md (Just win)
                                _ <- dialogAddButton' md (__ "Add cabal.project") (AnotherResponseType 1)
                                _ <- dialogAddButton' md (__ "Add stack.yaml") (AnotherResponseType 2)
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
            _ <- projectAddPackage' fp
            triggerEventIDE_ (UpdateWorkspaceInfo False)

projectAddPackage :: ProjectAction
projectAddPackage = do
    project <- ask
    let path = dropFileName (pjFile project)
    win <-  liftIDE getMainWindow
    liftIO (choosePackageFile win (Just path)) >>= \case
        Nothing -> return ()
        Just fp -> do
            void (projectAddPackage' fp)
            liftIDE $ triggerEventIDE_ (UpdateWorkspaceInfo False)

packageTry :: PackageAction -> IDEAction
packageTry f = workspaceTry $ do
        maybeProject <- lift $ readIDE activeProject
        maybePackage <- lift $ readIDE activePack
        case (maybeProject, maybePackage) of
            (Just project, Just package)  -> runProject (runPackage f package) project
            _ -> do
                win <- liftIDE getMainWindow
                md <- new' MessageDialog [
                    constructDialogUseHeaderBar 0,
                    constructMessageDialogButtons ButtonsTypeCancel]
                setMessageDialogMessageType md MessageTypeQuestion
                setMessageDialogText md $ __ "You need to have an active package for this to work."
                windowSetTransientFor md (Just win)
                _ <- dialogAddButton' md (__ "_New Package") (AnotherResponseType 1)
                _ <- dialogAddButton' md (__ "_Add Package") (AnotherResponseType 2)
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

addRecentlyUsedWorkspace :: FilePath -> IDEAction
addRecentlyUsedWorkspace fp = do
    state <- readIDE currentState
    unless (isStartingOrClosing state) $ do
        recentWorkspaces' <- readIDE recentWorkspaces
        unless (fp `elem` recentWorkspaces') $
            modifyIDE_ $ recentWorkspaces .~ take 12 (fp : recentWorkspaces')
        triggerEventIDE_ UpdateRecent

makePackage :: PackageAction
makePackage = do
  liftIDE $ getLog >>= bringPaneToFront
  makePackage'

fileOpen :: IDEAction
fileOpen = do
    win <- getMainWindow
    mbBuf <- maybeActiveBuf
    dialog <- new' FileChooserDialog [constructDialogUseHeaderBar 1]
    setWindowTitle dialog $ __ "Open File"
    windowSetTransientFor dialog $ Just win
    fileChooserSetAction dialog FileChooserActionOpen
    _ <- dialogAddButton' dialog "gtk-cancel" ResponseTypeCancel
    _ <- dialogAddButton' dialog "gtk-open" ResponseTypeAccept
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
    win <- getMainWindow
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
                windowSetTransientFor md (Just win)
                _ <- dialogAddButton' md (__ "_Add " <> T.pack (takeFileName fp)) (AnotherResponseType 1)
                _ <- dialogAddButton' md (__ "Just _open " <> T.pack (takeFileName fp)) (AnotherResponseType 2)
                dialogSetDefaultResponse' md (AnotherResponseType 1)
                resp <- dialogRun' md
                widgetDestroy md
                case resp of
                    AnotherResponseType 1 -> projectTry $ do
                        _ <- projectAddPackage' fp
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
                    windowSetTransientFor md (Just win)
                    _ <- dialogAddButton' md (__ "_Add " <> T.pack (takeFileName cabalFile)) (AnotherResponseType 1)
                    _ <- dialogAddButton' md (__ "Just _open " <> T.pack (takeFileName fp)) (AnotherResponseType 2)
                    dialogSetDefaultResponse' md (AnotherResponseType 1)
                    resp <- dialogRun' md
                    widgetDestroy md
                    case resp of
                        AnotherResponseType 1 -> projectTry $
                            void $ projectAddPackage' cabalFile
                        _ -> return ()
    fileOpenThis fp
  where
    findCabalFile :: FilePath -> IO (Maybe FilePath)
    findCabalFile fpath = (do
            let dir = takeDirectory fpath
            contents <- getDirectoryContents dir
            files    <- filterM (\f -> not <$> doesDirectoryExist (dir </> f)) contents
            let cabal = filter ((== ".cabal") . takeExtension) files
            case cabal of
                (c:_) -> return . Just $ dir </> c
                _ | fpath == dir -> return Nothing
                  | otherwise -> findCabalFile dir
        ) `catch` (\(_ :: SomeException) -> return Nothing)
