{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Workspace
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   workspaceOpenThis
,   workspaceClose
,   workspaceClean
,   workspaceMake
,   workspaceActivatePackage
,   workspaceAddPackage
,   workspaceAddPackage'
,   workspaceRemovePackage

,   makePackage
,   backgroundMake
) where

import IDE.Core.State
import Control.Monad.Trans (liftIO)
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import Control.Monad (unless, when)
import Data.Maybe (isJust,fromJust )
import IDE.Utils.GUIUtils
    (chooseFile, chooseSaveFile)
import System.FilePath
    (dropExtension,
     takeBaseName,
     addExtension,
     takeExtension)
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
    (widgetDestroy, dialogRun, messageDialogNew, Window(..))
import IDE.Pane.PackageEditor (choosePackageFile)
import Data.List ((\\), foldl', nub, delete)
import IDE.Package
    (buildPackage,
     packageInstall',
     packageClean,
     activatePackage,
     deactivatePackage,
     idePackageFromPath)
import System.Directory (doesFileExist, canonicalizePath)
import System.Time (getClockTime)
import Graphics.UI.Gtk.Windows.MessageDialog
    (ButtonsType(..), MessageType(..))
import Graphics.UI.Gtk.General.Structs (ResponseId(..))
import Control.Exception (SomeException(..))
import Control.Monad.Reader.Class (ask)
import Data.Map (Map(..))
import qualified Data.Map as  Map
    (lookup, empty, mapWithKey, fromList, map)
import Distribution.Package (pkgVersion, pkgName, Dependency(..))
import Distribution.Version (withinRange)
import qualified GHC.List as  List (or)
import IDE.Pane.SourceBuffer (belongsToPackage, fileCheckAll)


setWorkspace :: Maybe Workspace -> IDEAction
setWorkspace mbWs = do
    let mbRealWs = case mbWs of
                        Nothing -> Nothing
                        Just ws -> Just ws{wsReverseDeps = calculateReverseDependencies ws}
    mbOldWs <- readIDE workspace
--    trace ("deps " ++ case mbRealWs of
--                        Nothing -> "empty ws"
--                        Just ws -> show $ map (\ (k,v) -> (packageId k, map packageId v))
--                                            (Map.toList (wsReverseDeps ws)))
    modifyIDE_ (\ide -> ide{workspace = mbRealWs})
    let pack =  case mbRealWs of
                    Nothing -> Nothing
                    Just ws -> wsActivePack ws
    let oldPack = case mbOldWs of
                    Nothing -> Nothing
                    Just ws -> wsActivePack ws
    when (pack /= oldPack) $
            case pack of
                Nothing -> deactivatePackage
                Just p  -> activatePackage pack >> return ()
    mbPack <- readIDE activePack
    triggerEventIDE WorkspaceChanged
    let wsStr = case mbRealWs of
                    Nothing -> ""
                    Just ws -> wsName ws
    let txt = wsStr ++ " > " ++
                    (case mbPack of
                            Nothing -> ""
                            Just p  -> packageIdentifierToString (ipdPackageId p))
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    return ()


-- ---------------------------------------------------------------------
-- This needs to be incremented, when the workspace format changes
--
workspaceVersion :: Int
workspaceVersion = 1

-- | Constructs a new workspace and makes it the current workspace
workspaceNew :: IDEAction
workspaceNew = do
    window <- getMainWindow
    mbFile <- liftIO $ do
        chooseSaveFile window "New file for wokspace" Nothing
    case mbFile of
        Nothing -> return ()
        Just filePath ->
            let realPath = if takeExtension filePath == leksahWorkspaceFileExtension
                                    then filePath
                                    else addExtension filePath leksahWorkspaceFileExtension
            in do
                cPath <- liftIO $ canonicalizePath realPath
                let newWorkspace = emptyWorkspace {
                                    wsName = takeBaseName cPath,
                                    wsFile = cPath}
                liftIO $ writeFields cPath newWorkspace workspaceDescr
                workspaceOpenThis False (Just cPath)
                return ()

workspaceOpen :: IDEAction
workspaceOpen = do
    window     <- getMainWindow
    mbFilePath <- liftIO $ chooseWorkspaceFile window
    workspaceOpenThis True mbFilePath

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
                    then liftIO $ do
                        md  <- messageDialogNew Nothing [] MessageQuestion ButtonsYesNo
                                $ "Load the session settings stored with this workspace?"
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
                        setWorkspace (Just workspace {wsFile = filePath,
                                                      wsReverseDeps = calculateReverseDependencies workspace}))
                            (\ (e :: SomeException) -> reflectIDE
                                (ideMessage Normal ("Can't load workspace file " ++ filePath ++ "\n" ++ show e)) ideR)


-- | Closes a workspace
workspaceClose :: IDEAction
workspaceClose = do
    oldWorkspace <- readIDE workspace
    case oldWorkspace of
        Nothing -> return ()
        Just ws -> do
            let oldActivePack = wsActivePack ws
            triggerEventIDE (SaveSession ((dropExtension (wsFile ws))
                                ++  leksahSessionFileExtension))
            addRecentlyUsedWorkspace (wsFile ws)
            setWorkspace Nothing
            triggerEventIDE (ActivePack Nothing)
            when (isJust oldActivePack) $ do
                triggerEventIDE (Sensitivity [(SensitivityProjectActive, False),
                    (SensitivityWorkspaceOpen, False)])
                return ()
            return ()
    return ()


workspaceAddPackage :: IDEAction
workspaceAddPackage = do
    window <-  getMainWindow
    mbFilePath <- liftIO $ choosePackageFile window
    case mbFilePath of
        Nothing -> return ()
        Just fp -> workspaceAddPackage' fp

workspaceAddPackage' :: FilePath -> IDEAction
workspaceAddPackage' fp = do
    cfp <-  liftIO $ canonicalizePath fp
    mbWs <- readIDE workspace
    case mbWs of
        Nothing -> return ()
        Just ws -> do
            mbPack <- idePackageFromPath cfp
            case mbPack of
                Just pack -> do
                    unless (elem cfp (map ipdCabalFile (wsPackages ws))) $
                        writeWorkspace $ ws {wsPackages =  pack : wsPackages ws}
                    return ()
                Nothing -> return ()


workspaceRemovePackage :: IDEPackage -> IDEAction
workspaceRemovePackage pack = do
    mbWs <- readIDE workspace
    case mbWs of
        Nothing -> return ()
        Just ws -> do
            when (elem pack (wsPackages ws)) $
                writeWorkspace ws {wsPackages =  delete pack (wsPackages ws)}
            return ()

workspaceActivatePackage :: IDEPackage -> IDEAction
workspaceActivatePackage pack = do
    mbWs <- readIDE workspace
    case mbWs of
        Nothing -> return ()
        Just ws -> do
            when (elem pack (wsPackages ws)) $ do
                writeWorkspace ws {wsActivePack =  Just pack}
                return ()
            return ()

writeWorkspace :: Workspace -> IDEAction
writeWorkspace ws = do
    cWs          <- liftIO $ makeCanonic ws
    timeNow      <- liftIO getClockTime
    let newWs    =  cWs {wsSaveTime = show timeNow,
                         wsVersion = workspaceVersion,
                         wsActivePackFile = case wsActivePack ws of
                                                Nothing -> Nothing
                                                Just pack -> Just (ipdCabalFile pack),
                         wsPackagesFiles = map ipdCabalFile (wsPackages ws)}
    setWorkspace $ Just cWs
    liftIO $ writeFields (wsFile newWs) (newWs {wsFile = ""}) workspaceDescr

readWorkspace :: FilePath -> IDEM Workspace
readWorkspace fp = do
    ws <- liftIO $ readFields fp workspaceDescr emptyWorkspace
    activePack <- case wsActivePackFile ws of
                        Nothing ->  return Nothing
                        Just fp ->  idePackageFromPath fp
    packages <- mapM idePackageFromPath (wsPackagesFiles ws)
    return ws{ wsActivePack = activePack, wsPackages = map fromJust $ filter isJust $ packages}

makeCanonic :: Workspace -> IO Workspace
makeCanonic ws = do
    wsActivePackFile'           <-  case wsActivePackFile ws of
                                        Nothing -> return Nothing
                                        Just fp -> do
                                            nfp <- canonicalizePath fp
                                            return (Just nfp)
    wsFile'                     <-  canonicalizePath (wsFile ws)
    wsPackagesFiles'            <-  mapM canonicalizePath (wsPackagesFiles ws)
    return ws {wsActivePackFile = wsActivePackFile', wsFile = wsFile', wsPackagesFiles = wsPackagesFiles'}

emptyWorkspace =  Workspace {
    wsVersion       =   workspaceVersion
,   wsSaveTime      =   ""
,   wsName          =   ""
,   wsFile          =   ""
,   wsPackages      =   []
,   wsActivePack    =   Nothing
,   wsPackagesFiles =   []
,   wsActivePackFile =   Nothing
,   wsReverseDeps   =   Map.empty
}

workspaceDescr :: [FieldDescriptionS Workspace]
workspaceDescr = [
        mkFieldS
            (paraName <<<- ParaName "Version of workspace file format" $ emptyParams)
            (PP.text . show)
            intParser
            wsVersion
            (\ b a -> a{wsVersion = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Time of storage" $ emptyParams)
            (PP.text . show)
            stringParser
            wsSaveTime
            (\ b a -> a{wsSaveTime = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Name of the workspace" $ emptyParams)
            (PP.text . show)
            stringParser
            wsName
            (\ b a -> a{wsName = b})
    ,   mkFieldS
            (paraName <<<- ParaName "File paths of contained packages" $ emptyParams)
            (PP.text . show)
            readParser
            wsPackagesFiles
            (\b a -> a{wsPackagesFiles = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Maybe file path of an active package" $ emptyParams)
            (PP.text . show)
            readParser
            wsActivePackFile
            (\fp a -> a{wsActivePackFile = fp})]


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

-- | Calculates for every package a list of packages, that depends directly on it
-- This is the reverse info from cabal, were we specify what a package depends on
calculateReverseDependencies :: Workspace -> Map IDEPackage [IDEPackage]
calculateReverseDependencies ws = Map.map nub $ foldl' calc mpacks packages
    where
    packages = wsPackages ws
    mpacks   = Map.fromList (map (\p -> (p,[])) packages)

    calc :: Map IDEPackage [IDEPackage] -> IDEPackage -> Map IDEPackage [IDEPackage]
    calc theMap pack = Map.mapWithKey (addDeps (ipdDepends pack) pack ) theMap

    addDeps :: [Dependency] -> IDEPackage -> IDEPackage -> [IDEPackage] -> [IDEPackage]
    addDeps dependencies pack pack2 revDeps =  if depsMatch then pack : revDeps else revDeps
        where
        depsMatch = List.or (map (matchOne pack2) dependencies)
        matchOne thePack (Dependency name versionRange) =
            name == pkgName (ipdPackageId thePack)
            &&  withinRange (pkgVersion (ipdPackageId thePack)) versionRange


-- | Select a package which is not direct or indirect dependent on any other package
selectFirstReasonableTarget :: [IDEPackage] -> Map IDEPackage [IDEPackage] -> IDEPackage
selectFirstReasonableTarget [] _      = error "Workspaces>>selectFirstReasonableTarget: empty package list"
selectFirstReasonableTarget [p] _     = p
selectFirstReasonableTarget l revDeps =
    case foldl' removeDeps l l of
        hd:_ -> hd
        []   -> error "Workspaces>>selectFirstReasonableTarget: no remaining"
    where
    removeDeps :: [IDEPackage] -> IDEPackage -> [IDEPackage]
    removeDeps packs p = packs \\ allDeps p

    allDeps :: IDEPackage -> [IDEPackage]
    allDeps p = case Map.lookup p revDeps of
                            Nothing   -> []
                            Just list -> list ++ concatMap allDeps list

makePackages :: Bool -> [IDEPackage] -> IDEAction
makePackages _ []       = return ()
makePackages isBackgroundBuild packages = do
    mbWs <- readIDE workspace
    prefs' <- readIDE prefs
    case mbWs of
        Nothing -> return ()
        Just ws -> do
            let package = selectFirstReasonableTarget packages (wsReverseDeps ws)
            buildPackage isBackgroundBuild package (cont prefs' package ws)
    where
    cont prefs' package ws res =
        if res
            then do
                let deps = case Map.lookup package (wsReverseDeps ws) of
                                        Nothing   -> []
                                        Just list -> list
                -- install if either
                -- AlwaysInstall is set or
                -- deps are not empty and
                when ((not (null deps) && autoInstall prefs' == InstallLibs)
                        || autoInstall prefs' == InstallAlways)
                    $ packageInstall' package (cont2 deps package)
            else cont2 [] package
    cont2 deps package = do
        let nextTargets = delete package $ nub $ packages ++ deps
        makePackages isBackgroundBuild nextTargets


backgroundMake :: IDEAction
backgroundMake = catchIDE (do
    ideR        <- ask
    prefs       <- readIDE prefs
    mbPackage   <- readIDE activePack
    case mbPackage of
        Nothing         -> return ()
        Just package    -> do
            modifiedPacks <- if saveAllBeforeBuild prefs
                                then fileCheckAll belongsToPackage
                                else return []
            let isModified = not (null modifiedPacks)
            when isModified $ do
            makePackages True modifiedPacks
    )
    (\(e :: SomeException) -> sysMessage Normal (show e))

makePackage :: IDEAction
makePackage = do
    activePack' <- readIDE activePack
    case activePack' of
        Nothing -> return ()
        Just p -> makePackages False [p]

workspaceClean = do
    mbWs <-  readIDE workspace
    case mbWs of
        Nothing -> return ()
        Just ws -> mapM_ (packageClean . Just) (wsPackages ws)

workspaceMake = do
    mbWs <-  readIDE workspace
    case mbWs of
        Nothing -> return ()
        Just ws -> makePackages False (wsPackages ws)

