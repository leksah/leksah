{-# LANGUAGE CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Workspace
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   workspaceTry_
,   workspaceOpenThis
,   workspaceClose
,   workspaceClean
,   workspaceMake
,   workspaceActivatePackage
,   workspaceAddPackage
,   workspaceAddPackage'
,   workspaceRemovePackage
,   workspacePackageNew
,   packageTry
,   packageTry_

,   makePackage
,   backgroundMake
,   calculateReverseDependencies
) where

import IDE.Core.State
import Control.Monad.Trans (liftIO)
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import Control.Monad (unless, when, liftM)
import Control.Monad.Reader (lift)
import Data.Maybe (isJust,fromJust )
import IDE.Utils.GUIUtils
    (chooseFile, chooseSaveFile)
import System.FilePath
       ((</>), isAbsolute, dropFileName, makeRelative, dropExtension,
        takeBaseName, addExtension, takeExtension)
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
import IDE.Pane.PackageEditor (packageNew', choosePackageFile)
import Data.List ((\\), foldl', nub, delete)
import IDE.Package
       (packageConfig', getModuleTemplate, getPackageDescriptionAndPath,
        buildPackage, packageInstall', packageClean', activatePackage,
        deactivatePackage, idePackageFromPath)
import System.Directory
       (createDirectoryIfMissing, doesFileExist, canonicalizePath)
import System.Time (getClockTime)
import Graphics.UI.Gtk.Windows.MessageDialog
    (ButtonsType(..), MessageType(..))
#if MIN_VERSION_gtk(0,10,5)
import Graphics.UI.Gtk.Windows.Dialog (ResponseId(..))
#else
import Graphics.UI.Gtk.General.Structs (ResponseId(..))
#endif
import Control.Exception (SomeException(..))
import Control.Monad.Reader.Class (ask)
import Data.Map (Map(..))
import qualified Data.Map as  Map
    (lookup, empty, mapWithKey, fromList, map)
import Distribution.Package (pkgVersion, pkgName, Dependency(..))
import Distribution.Version (withinRange)
import qualified GHC.List as  List (or)
import IDE.Pane.SourceBuffer
       (fileOpenThis, belongsToPackage, fileCheckAll)
import qualified System.IO.UTF8 as UTF8 (writeFile)
import System.Glib.Attributes (AttrOp(..), set)
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))


setWorkspace :: Maybe Workspace -> IDEAction
setWorkspace mbWs = do
    let mbRealWs = case mbWs of
                        Nothing -> Nothing
                        Just ws -> Just ws{wsReverseDeps = calculateReverseDependencies ws}
    mbOldWs <- readIDE workspace
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
    let wsStr = case mbRealWs of
                    Nothing -> ""
                    Just ws -> wsName ws
    let txt = wsStr ++ " > " ++
                    (case mbPack of
                            Nothing -> ""
                            Just p  -> packageIdentifierToString (ipdPackageId p))
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    triggerEventIDE (WorkspaceChanged True True)
    triggerEventIDE UpdateWorkspaceInfo
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
        chooseSaveFile window "New file for workspace" Nothing
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
    return ()

workspaceTryQuiet :: WorkspaceM a -> IDEM (Maybe a)
workspaceTryQuiet f = do
    maybeWorkspace <- readIDE workspace
    case maybeWorkspace of
        Just ws -> liftM Just $ runWorkspace f ws
        Nothing -> return Nothing

workspaceTryQuiet_ :: WorkspaceM a -> IDEAction
workspaceTryQuiet_ f = workspaceTryQuiet f >> return ()

workspaceTry :: WorkspaceM a -> IDEM (Maybe a)
workspaceTry f = do
    maybeWorkspace <- readIDE workspace
    case maybeWorkspace of
        Just ws -> liftM Just $ runWorkspace f ws
        Nothing -> do
            mainWindow <- getMainWindow
            resp <- liftIO $ do
                md <- messageDialogNew (Just mainWindow) [DialogModal] MessageQuestion ButtonsNone
                        "You need to have a workspace for this to work."
                dialogAddButton md "New Workspace" (ResponseUser 1)
                dialogAddButton md "Open Workspace" (ResponseUser 2)
                dialogAddButton md "Cancel" (ResponseCancel)
                dialogSetDefaultResponse md (ResponseUser 2)
                set md [ windowWindowPosition := WinPosCenterOnParent ]
                resp <- dialogRun md
                widgetHide md
                return resp
            case resp of
                ResponseUser 1 -> do
                    workspaceNew
                    workspaceTryQuiet f
                ResponseUser 2 -> do
                    workspaceOpen
                    workspaceTryQuiet f
                _  -> return Nothing

workspaceTry_ :: WorkspaceM a -> IDEAction
workspaceTry_ f = workspaceTry f >> return ()

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
                            md  <- messageDialogNew Nothing [] MessageQuestion ButtonsNone
                                    $ "There are session settings stored with this workspace."
                            dialogAddButton md "Load Session" ResponseYes
                            dialogAddButton md "Ignore Session" ResponseCancel
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
                        setWorkspace (Just workspace {wsFile = filePath})
                        return ())
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
            when (isJust oldActivePack) $ do
                triggerEventIDE (Sensitivity [(SensitivityProjectActive, False),
                    (SensitivityWorkspaceOpen, False)])
                return ()
            return ()
    return ()

workspacePackageNew :: WorkspaceAction
workspacePackageNew = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    lift $ packageNew' (Just path) (\fp -> do
        window     <-  getMainWindow
        workspaceTry_ $ workspaceAddPackage' fp >> return ()
        mbPack <- idePackageFromPath fp
        constructAndOpenMainModule mbPack
        triggerEventIDE UpdateWorkspaceInfo >> return ())

constructAndOpenMainModule :: Maybe IDEPackage -> IDEAction
constructAndOpenMainModule Nothing = return ()
constructAndOpenMainModule (Just idePackage) =
    case (ipdMain idePackage, ipdSrcDirs idePackage) of
        ([target],[path]) -> do
            mbPD <- getPackageDescriptionAndPath
            case mbPD of
                Just (pd,_) -> do
                    liftIO $ createDirectoryIfMissing True path
                    alreadyExists <- liftIO $ doesFileExist (path </> target)
                    if alreadyExists
                        then ideMessage Normal "Main file already exists"
                        else do
                            template <- liftIO $ getModuleTemplate pd (dropExtension target)
                            liftIO $ UTF8.writeFile (path </> target) template
                            fileOpenThis (path </> target)
                Nothing     -> ideMessage Normal "No package description"
        _ -> return ()


workspaceAddPackage :: WorkspaceAction
workspaceAddPackage = do
    ws <- ask
    let path = dropFileName (wsFile ws)
    window <-  lift getMainWindow
    mbFilePath <- liftIO $ choosePackageFile window (Just path)
    case mbFilePath of
        Nothing -> return ()
        Just fp -> workspaceAddPackage' fp >> return ()

workspaceAddPackage' :: FilePath -> WorkspaceM (Maybe IDEPackage)
workspaceAddPackage' fp = do
    ws <- ask
    cfp <-  liftIO $ canonicalizePath fp
    mbPack <- lift $ idePackageFromPath cfp
    case mbPack of
        Just pack -> do
            unless (elem cfp (map ipdCabalFile (wsPackages ws))) $ lift $
                writeWorkspace $ ws {wsPackages =  pack : wsPackages ws,
                                    wsActivePack =  Just pack}
            return (Just pack)
        Nothing -> return Nothing

packageTryQuiet :: PackageM a -> IDEM (Maybe a)
packageTryQuiet f = do
    maybePackage <- readIDE activePack
    case maybePackage of
        Just p  -> liftM Just $ runPackage f p
        Nothing -> return Nothing

packageTryQuiet_ :: PackageM a -> IDEAction
packageTryQuiet_ f = packageTryQuiet f >> return ()

packageTry :: PackageM a -> IDEM (Maybe a)
packageTry f = do
    liftM (>>= id) $ workspaceTry $ do
        maybePackage <- lift $ readIDE activePack
        case maybePackage of
            Just p  -> liftM Just $ lift $ runPackage f p
            Nothing -> do
                window <- lift $ getMainWindow
                resp <- liftIO $ do
                    md <- messageDialogNew (Just window) [] MessageQuestion ButtonsNone
                            "You need to have an active package for this to work."
                    dialogAddButton md "New Package" (ResponseUser 1)
                    dialogAddButton md "Add Package" (ResponseUser 2)
                    dialogAddButton md "Cancel" ResponseCancel
                    dialogSetDefaultResponse md (ResponseUser 2)
                    set md [ windowWindowPosition := WinPosCenterOnParent ]
                    resp <- dialogRun md
                    widgetHide md
                    return resp
                case resp of
                    ResponseUser 1 -> do
                        workspacePackageNew
                        lift $ packageTryQuiet f
                    ResponseUser 2 -> do
                        workspaceAddPackage
                        lift $ packageTryQuiet f
                    _  -> return Nothing

packageTry_ :: PackageM a -> IDEAction
packageTry_ f = packageTry f >> return ()

workspaceRemovePackage :: IDEPackage -> WorkspaceAction
workspaceRemovePackage pack = do
    ws <- ask
    when (elem pack (wsPackages ws)) $ lift $
        writeWorkspace ws {wsPackages =  delete pack (wsPackages ws)}
    return ()

workspaceActivatePackage :: IDEPackage -> WorkspaceAction
workspaceActivatePackage pack = do
    ws <- ask
    lift $ activatePackage (Just pack)
    when (elem pack (wsPackages ws)) $ lift $ do
        writeWorkspace ws {wsActivePack =  Just pack}
        return ()
    return ()

writeWorkspace :: Workspace -> IDEAction
writeWorkspace ws = do
    timeNow      <- liftIO getClockTime
    let newWs    =  ws {wsSaveTime = show timeNow,
                         wsVersion = workspaceVersion,
                         wsActivePackFile = case wsActivePack ws of
                                                Nothing -> Nothing
                                                Just pack -> Just (ipdCabalFile pack),
                         wsPackagesFiles = map ipdCabalFile (wsPackages ws)}
    setWorkspace $ Just newWs
    newWs' <- liftIO $ makePathesRelative newWs
    liftIO $ writeFields (wsFile newWs') (newWs' {wsFile = ""}) workspaceDescr

readWorkspace :: FilePath -> IDEM Workspace
readWorkspace fp = do
    ws <- liftIO $ readFields fp workspaceDescr emptyWorkspace
    ws' <- liftIO $ makePathesAbsolute ws fp
    activePack <- case wsActivePackFile ws' of
                        Nothing ->  return Nothing
                        Just fp ->  idePackageFromPath fp
    packages <- mapM idePackageFromPath (wsPackagesFiles ws')
    return ws'{ wsActivePack = activePack, wsPackages = map fromJust $ filter isJust $ packages}

makePathesRelative :: Workspace -> IO Workspace
makePathesRelative ws = do
    wsFile'                     <-  canonicalizePath (wsFile ws)
    wsActivePackFile'           <-  case wsActivePackFile ws of
                                        Nothing -> return Nothing
                                        Just fp -> do
                                            nfp <- canonicalizePath fp
                                            return (Just (makeRelative (dropFileName wsFile') nfp))
    wsPackagesFiles'            <-  mapM canonicalizePath (wsPackagesFiles ws)
    let relativePathes          =   map (\p -> makeRelative (dropFileName wsFile') p) wsPackagesFiles'
    return ws {wsActivePackFile = wsActivePackFile', wsFile = wsFile', wsPackagesFiles = relativePathes}

makePathesAbsolute :: Workspace -> FilePath -> IO Workspace
makePathesAbsolute ws bp = do
    wsFile'                     <-  canonicalizePath bp
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
                then canonicalizePath relativePath
                else canonicalizePath (basePath </> relativePath)

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
    allDeps p = nub (allDeps' [] p)
        where
        allDeps' :: [IDEPackage] -> IDEPackage -> [IDEPackage]
        allDeps' accu p | elem p accu = accu
                        | otherwise   = case Map.lookup p revDeps of
                                            Nothing   -> accu
                                            Just list -> foldl' allDeps' (accu ++ list) list

makePackages :: Bool -> Bool -> [IDEPackage] -> WorkspaceAction
makePackages _ _ []     = return ()
makePackages isBackgroundBuild needConfigure packages = do
    ws <- ask
    lift $ do
        prefs' <- readIDE prefs
        let package = selectFirstReasonableTarget packages (wsReverseDeps ws)
        if needConfigure
            then packageConfig' package (\b ->
                if b
                    then buildPackage isBackgroundBuild package (cont prefs' package ws)
                    else return ())
            else buildPackage isBackgroundBuild package (cont prefs' package ws)
    where
    cont prefs' package ws res =
        if res
            then do
                let deps = case Map.lookup package (wsReverseDeps ws) of
                                        Nothing   -> []
                                        Just list -> list
                -- install if backgroundLink is set or it is not a background build and either
                -- AlwaysInstall is set or
                -- deps are not empty
                if ((not isBackgroundBuild || backgroundLink prefs') &&
                    ((not (null deps) && autoInstall prefs' == InstallLibs)
                        || autoInstall prefs' == InstallAlways))
                    then packageInstall' package (cont2 deps package)
                    else cont2 deps package True
            else return () -- don't continue, when their was an error
    cont2 deps package res = do
        when res $ do
            let nextTargets = delete package $ nub $ packages ++ deps
            workspaceTryQuiet_ $ makePackages isBackgroundBuild True nextTargets


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
                workspaceTryQuiet_ $ makePackages True False modifiedPacks
    )
    (\(e :: SomeException) -> sysMessage Normal (show e))

makePackage :: PackageAction
makePackage = do
    p <- ask
    lift $ workspaceTry_ $ makePackages False False [p]

workspaceClean :: WorkspaceAction
workspaceClean = do
    ws <- ask
    lift $ mapM_ packageClean' (wsPackages ws)

workspaceMake :: WorkspaceAction
workspaceMake = do
    ws <- ask
    makePackages False True (wsPackages ws)

