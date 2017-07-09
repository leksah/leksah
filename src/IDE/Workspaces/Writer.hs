{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Workspaces.Writer
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Workspaces.Writer (
    writeWorkspace
    ,readWorkspace
    ,makePathsAbsolute
    ,emptyWorkspace
    ,setWorkspace
    ,workspaceDescr
    ,workspaceVersion
) where

import IDE.Core.Types
import IDE.Core.State
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, ideProjectFromPath)
import IDE.Utils.FileUtils(myCanonicalizePath)

import Data.Maybe
import Control.Monad (join, void, when)
import Control.Monad.Trans (liftIO)
import System.Time (getClockTime)
import Text.PrinterParser
    (readFields,
     writeFields,
     readParser,
     stringParser,
     intParser,
     mkFieldS,
     FieldDescriptionS(..))
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory)
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import qualified Text.PrettyPrint as  PP (text)
import System.Log.Logger (debugM)
import qualified Data.Text as T (pack)
import Data.Monoid ((<>))
import System.FSNotify (watchDir, Event(..), watchTree)
import Control.Monad.Reader (MonadReader(..))
import Data.Traversable (forM)
import qualified Data.Map as Map (empty)

writeWorkspace :: Workspace -> IDEAction
writeWorkspace ws = do
    timeNow      <- liftIO getClockTime
    let newWs    =  ws {wsSaveTime = T.pack $ show timeNow,
                         wsVersion = workspaceVersion,
                         wsProjectFiles = map pjFile (wsProjects ws)}
    setWorkspace $ Just newWs
    newWs' <- liftIO $ makePathsRelative newWs
    liftIO $ writeFields (wsFile newWs') (newWs' {wsFile = ""}) workspaceDescr

readWorkspace :: FilePath -> IDEM Workspace
readWorkspace fp = do
    liftIO $ debugM "leksah" "readWorkspace"
    ws <- liftIO $ readFields fp workspaceDescr emptyWorkspace
    ws' <- liftIO $ makePathsAbsolute ws fp
    projects <- mapM ideProjectFromPath (wsProjectFiles ws')
    --TODO set package vcs here
    return ws'{ wsProjects = catMaybes projects}

makePathsAbsolute :: Workspace -> FilePath -> IO Workspace
makePathsAbsolute ws bp = do
    wsFile'           <-  myCanonicalizePath bp
    wsActivePackFile' <-  case wsActivePackFile ws of
                                Nothing -> return Nothing
                                Just fp -> do
                                    fp' <- makeAbsolute (dropFileName wsFile') fp
                                    return (Just fp')
    wsProjectFiles'   <-  mapM (makeAbsolute (dropFileName wsFile')) (wsProjectFiles ws)
    return ws {wsActivePackFile = wsActivePackFile', wsFile = wsFile', wsProjectFiles = wsProjectFiles'}
    where
        makeAbsolute basePath relativePath  =
            myCanonicalizePath
               (if isAbsolute relativePath
                    then relativePath
                    else basePath </> relativePath)

emptyWorkspace =  Workspace {
    wsVersion           =   workspaceVersion
,   wsSaveTime          =   ""
,   wsName              =   ""
,   wsFile              =   ""
,   wsProjects          =   []
,   wsProjectFiles      =   []
,   wsActiveProjectFile =   Nothing
,   wsActivePackFile    =   Nothing
,   wsActiveExe         =   Nothing
,   packageVcsConf      =   Map.empty
}

getProject :: FilePath -> [Project] -> Maybe Project
getProject fp projects =
    case filter (\ p -> pjFile p == fp) projects of
        [p] -> Just p
        l   -> Nothing

getPackage :: FilePath -> [IDEPackage] -> Maybe IDEPackage
getPackage fp packages =
    case filter (\ p -> ipdCabalFile p == fp) packages of
        [p] -> Just p
        l   -> Nothing

-- ---------------------------------------------------------------------
-- This needs to be incremented, when the workspace format changes
--
workspaceVersion :: Int
workspaceVersion = 3

setWorkspace :: Maybe Workspace -> IDEAction
setWorkspace mbWs = do
    liftIO $ debugM "leksah" "setWorkspace"
    ideR <- ask
    mbOldWs <- readIDE workspace
    modifyIDE_ (\ide -> ide{workspace = mbWs})
    let packFileAndExe =  case mbWs of
                            Nothing -> Nothing
                            Just ws -> Just (wsActiveProjectFile ws, wsActivePackFile ws, wsActiveExe ws)
    let oldPackFileAndExe = case mbOldWs of
                            Nothing -> Nothing
                            Just ws -> Just (wsActiveProjectFile ws, wsActivePackFile ws, wsActiveExe ws)
    case (packFileAndExe, mbWs) of
        (Just (Just pj, mbPackFile, mbExe), Just ws) ->
            case getProject pj (wsProjects ws) of
                Just project ->
                    case (`getPackage` pjPackages project) =<< mbPackFile of
                        Just package -> void (activatePackage mbPackFile (Just project) (Just package) mbExe)
                        _ -> void (activatePackage Nothing (Just project) Nothing Nothing)
                _ -> deactivatePackage
        _ -> deactivatePackage
    mbPack <- readIDE activePack
    mbExe  <- readIDE activeExe
    let wsStr = case mbWs of
                    Nothing -> ""
                    Just ws -> wsName ws
    let txt = wsStr <> " "
                 <> (case mbPack of
                            Nothing  -> ""
                            Just p   -> packageIdentifierToString (ipdPackageId p))
                 <> (case mbExe of
                            Nothing  -> ""
                            Just exe -> " " <> exe)
    case mbWs of
        Just ws -> do
            fsn <- readIDE fsnotify
            newStop <- liftIO $ forM (wsProjects ws) (\project ->
                watchDir fsn (dropFileName $ pjFile project) (\case
                        Modified f _ | takeFileName f == takeFileName (pjFile project) -> True
                        _ -> False) $ \_ ->
                    (`reflectIDE` ideR) $ postAsyncIDE $ do
                        ws' <- readWorkspace (wsFile ws)
                        setWorkspace (Just ws'))
            oldStop <- readIDE stopWorkspaceNotify
            modifyIDE_ $ \ide -> ide { stopWorkspaceNotify = sequence_ newStop }
            liftIO oldStop
        Nothing -> return ()
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    triggerEventIDE (WorkspaceChanged True True)
    triggerEventIDE UpdateWorkspaceInfo
    return ()

makePathsRelative :: Workspace -> IO Workspace
makePathsRelative ws = do
    wsFile' <- myCanonicalizePath (wsFile ws)
    wsActivePackFile'           <-  case wsActivePackFile ws of
                                        Nothing -> return Nothing
                                        Just fp -> do
                                            nfp <- liftIO $ myCanonicalizePath fp
                                            return (Just (makeRelative (dropFileName wsFile') nfp))
    wsProjectFiles'            <-  mapM myCanonicalizePath (wsProjectFiles ws)
    let relativePathes          =   map (makeRelative (dropFileName wsFile')) wsProjectFiles'
    return ws {wsActivePackFile = wsActivePackFile', wsFile = wsFile', wsProjectFiles = relativePathes}

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
            (paraName <<<- ParaName "File paths of contained projects" $ emptyParams)
            (PP.text . show)
            readParser
            wsProjectFiles
            (\b a -> a{wsProjectFiles = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Maybe file path of an active project" $ emptyParams)
            (PP.text . show)
            readParser
            wsActiveProjectFile
            (\fp a -> a{wsActiveProjectFile = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Maybe file path of an active package" $ emptyParams)
            (PP.text . show)
            readParser
            wsActivePackFile
            (\fp a -> a{wsActivePackFile = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Maybe name of an active executable" $ emptyParams)
            (PP.text . show)
            readParser
            wsActiveExe
            (\fp a -> a{wsActiveExe = fp})
    ,   mkFieldS
            (paraName <<<- ParaName "Version Control System configurations for packages" $ emptyParams)
            (PP.text . show)
            readParser
            packageVcsConf
            (\filePath a -> a{packageVcsConf = filePath})]
