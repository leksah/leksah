{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
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
    ,WorkspaceFile(..)
    ,emptyWorkspaceFile
    ,setWorkspace
    ,workspaceVersion
) where

import IDE.Core.Types
import IDE.Core.State
import IDE.Package
       (getModuleTemplate, getPackageDescriptionAndPath, activatePackage,
        deactivatePackage, ideProjectFromPath)
import IDE.Utils.FileUtils(myCanonicalizePath)

import Data.Maybe
import Control.Monad (unless, join, void, when)
import Control.Monad.Trans (liftIO)
import System.Time (getClockTime)
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative,
        dropExtension, takeBaseName, addExtension, takeExtension,
        takeDirectory)
import Graphics.UI.Editor.Parameters
    (Parameter(..), (<<<-), paraName, emptyParams)
import qualified Text.PrettyPrint as  PP (text)
import System.Log.Logger (debugM)
import qualified Data.Text as T (unpack, pack)
import Data.Monoid ((<>))
import System.FSNotify (watchDir, Event(..), watchTree, eventPath, isPollingManager)
import Control.Monad.Reader (MonadReader(..))
import Data.Traversable (forM)
import qualified Data.Map as Map (empty)
import Text.ParserCombinators.Parsec (parseFromFile, CharParser)
import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as LBS (readFile, writeFile)
import Data.Aeson
       (eitherDecode, encode, ToJSON(..), FromJSON(..))
import Data.Aeson.Types
       (Options, genericParseJSON, genericToEncoding, genericToJSON,
        defaultOptions, fieldLabelModifier)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (isPrefixOf, stripPrefix)
import Data.Char (toLower)
import IDE.Pane.SourceBuffer (setModifiedOnDisk)
import System.Directory (doesDirectoryExist)
import Control.Concurrent (putMVar, takeMVar, tryPutMVar)
import qualified Data.Set as S (fromList, insert, member)
import Control.Exception (evaluate)
import qualified Data.Map as M
       (partitionWithKey, partition, fromList, member)
import Data.Foldable (forM_)

data WorkspaceFile = WorkspaceFile {
    wsfVersion           ::   Int
,   wsfSaveTime          ::   Text
,   wsfName              ::   Text
,   wsfProjectFiles      ::   [FilePath]
,   wsfActiveProjectFile ::   Maybe FilePath
,   wsfActivePackFile    ::   Maybe FilePath
,   wsfActiveComponent   ::   Maybe Text
,   wsfPackageVcsConf    ::   Map FilePath VCSConf
} deriving (Show, Generic)

wsfAesonOptions :: Options
wsfAesonOptions = defaultOptions
    { fieldLabelModifier = \x -> case fromMaybe x $ stripPrefix "wsf" x of
                                (i:rest) -> toLower i:rest
                                _ -> error "Empty Field Name"
    }

instance ToJSON WorkspaceFile where
    toJSON     = genericToJSON wsfAesonOptions
    toEncoding = genericToEncoding wsfAesonOptions
instance FromJSON WorkspaceFile where
    parseJSON = genericParseJSON wsfAesonOptions

writeWorkspace :: Workspace -> IDEAction
writeWorkspace ws = do
    timeNow      <- liftIO getClockTime
    let newWs    =  ws {wsSaveTime = T.pack $ show timeNow,
                         wsVersion = workspaceVersion}
    setWorkspace $ Just newWs
    newWs' <- liftIO $ makePathsRelative newWs (wsFile ws)
    liftIO . LBS.writeFile (wsFile ws) $ encodePretty newWs'

readWorkspace :: FilePath -> IDEM (Either String Workspace)
readWorkspace fp = do
    liftIO $ debugM "leksah" "readWorkspace"
    liftIO (eitherDecode <$> LBS.readFile fp) >>= \case
        Left pe -> error $ "Error reading file " ++ show fp ++ " " ++ show pe
        Right ws -> do
            ws' <- makePathsAbsolute ws fp
            --TODO set package vcs here
            return $ Right ws'

makePathsAbsolute :: WorkspaceFile -> FilePath -> IDEM Workspace
makePathsAbsolute ws bp = do
    wsFile'           <-  liftIO $ myCanonicalizePath bp
    wsActiveProjectFile' <-  case wsfActiveProjectFile ws of
                                Nothing -> return Nothing
                                Just fp -> do
                                    fp' <- liftIO $ makeAbsolute (dropFileName wsFile') fp
                                    return (Just fp')
    wsActivePackFile' <-  case wsfActivePackFile ws of
                                Nothing -> return Nothing
                                Just fp -> do
                                    fp' <- liftIO $ makeAbsolute (dropFileName wsFile') fp
                                    return (Just fp')
    projectFiles      <- liftIO $ mapM (makeAbsolute (dropFileName wsFile')) (wsfProjectFiles ws)
    projects          <- catMaybes <$> mapM ideProjectFromPath projectFiles
    return Workspace
                { wsFile              = wsFile'
                , wsVersion           = wsfVersion ws
                , wsSaveTime          = wsfSaveTime ws
                , wsName              = wsfName ws
                , wsProjects          = projects
                , wsActiveProjectFile = wsActiveProjectFile'
                , wsActivePackFile    = wsActivePackFile'
                , wsActiveComponent   = wsfActiveComponent ws
                , packageVcsConf      = wsfPackageVcsConf ws
                }
    where
        makeAbsolute basePath relativePath  =
            myCanonicalizePath
               (if isAbsolute relativePath
                    then relativePath
                    else basePath </> relativePath)

emptyWorkspace =  Workspace {
    wsVersion            =   workspaceVersion
,   wsSaveTime           =   ""
,   wsName               =   ""
,   wsFile               =   ""
,   wsProjects           =   []
,   wsActiveProjectFile  =   Nothing
,   wsActivePackFile     =   Nothing
,   wsActiveComponent    =   Nothing
,   packageVcsConf       =   Map.empty
}

emptyWorkspaceFile =  WorkspaceFile {
    wsfVersion           =   workspaceVersion
,   wsfSaveTime          =   ""
,   wsfName              =   ""
,   wsfProjectFiles      =   []
,   wsfActiveProjectFile =   Nothing
,   wsfActivePackFile    =   Nothing
,   wsfActiveComponent   =   Nothing
,   wsfPackageVcsConf    =   Map.empty
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
workspaceVersion = 4

setWorkspace :: Maybe Workspace -> IDEAction
setWorkspace mbWs = do
    liftIO $ debugM "leksah" "setWorkspace"
    ideR <- ask
    mbOldWs <- readIDE workspace
    modifyIDE_ (\ide -> ide{workspace = mbWs})
    let packFileAndExe =  case mbWs of
                            Nothing -> Nothing
                            Just ws -> Just (wsActiveProjectFile ws, wsActivePackFile ws, wsActiveComponent ws)
    let oldPackFileAndExe = case mbOldWs of
                            Nothing -> Nothing
                            Just ws -> Just (wsActiveProjectFile ws, wsActivePackFile ws, wsActiveComponent ws)
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
    mbComponent <- readIDE activeComponent
    let wsStr = case mbWs of
                    Nothing -> ""
                    Just ws -> wsName ws
    let txt = wsStr <> " "
                 <> (case mbPack of
                            Nothing  -> ""
                            Just p   -> packageIdentifierToString (ipdPackageId p))
                 <> (case mbComponent of
                            Nothing  -> ""
                            Just component -> " " <> component)
    case mbWs of
        Just ws -> do
            fsn <- readIDE fsnotify
            tb <- readIDE triggerBuild
            watchersMVar <- readIDE watchers
            extModsMVar <- readIDE externalModified
            let rebuild = void . liftIO $ tryPutMVar tb ()
            unless (isPollingManager fsn) . liftIO $ do
                oldWatchers <- takeMVar watchersMVar
                let projectFiles = S.fromList $ map pjFile $ wsProjects ws
                    packageFiles = S.fromList $ map ipdCabalFile $ pjPackages =<< wsProjects ws
                    newProjects = filter (not . (`M.member` fst oldWatchers) . pjFile) $ wsProjects ws
                    newPackages = filter (not . (`M.member` snd oldWatchers) . ipdCabalFile) $ pjPackages =<< wsProjects ws
                newProjectWatchers <- forM newProjects $ \project -> do
                    debugM "leksah" $ "Watching project " <> show (pjFile project)
                    fmap (pjFile project,) <$> watchDir fsn (pjDir project) (\case
                        Modified f _ _ -> True
                        _ -> False) $ \event -> do
                            let f = eventPath event
                            (`reflectIDE` ideR) $ setModifiedOnDisk f
                            when (takeFileName f == takeFileName (pjFile project)) $
                                (`reflectIDE` ideR) $ postAsyncIDE $
                                    readWorkspace (wsFile ws) >>= \case
                                        Left _ -> return ()
                                        Right ws' -> setWorkspace (Just ws')
                newPackageWatchers <- forM newPackages $ \package -> do
                    debugM "leksah" $ "Watching package " <> show (ipdCabalFile package)
                    nonRootSrcPaths <- map (<>"/") . filter (/=ipdPackageDir package) <$>
                        mapM (myCanonicalizePath . (ipdPackageDir package </>)) (ipdSrcDirs package)

                    fmap (ipdCabalFile package,) <$> watchTree fsn (ipdPackageDir package) (\case
                        Modified f _ _ -> True
                        _ -> False) $ \event -> do
                            let f = eventPath event
                            (`reflectIDE` ideR) $ setModifiedOnDisk f >>= \case
                                True -> rebuild
                                False ->
                                    when (any (`isPrefixOf` f) nonRootSrcPaths) $ do
                                        liftIO $ debugM "leksah" $ "Modified source file " <> f <> " in " <> T.unpack (ipdPackageName package)
                                        extMods <- liftIO $ takeMVar extModsMVar
                                        liftIO $ putMVar extModsMVar =<< evaluate (S.insert f extMods)
                                        rebuild
                let (keepProjectWatchers, discardProjectWatches) = M.partitionWithKey (\f _ -> f `S.member` projectFiles) $ fst oldWatchers
                    (keepPackageWatchers, discardPackageWatches) = M.partitionWithKey (\f _ -> f `S.member` packageFiles) $ snd oldWatchers
                forM_ discardProjectWatches id
                forM_ discardPackageWatches id
                putMVar watchersMVar (keepProjectWatchers <> M.fromList newProjectWatchers, keepPackageWatchers <> M.fromList newPackageWatchers)
        Nothing -> return ()
    triggerEventIDE (StatusbarChanged [CompartmentPackage txt])
    triggerEventIDE (WorkspaceChanged True True)
    triggerEventIDE $ UpdateWorkspaceInfo True
    return ()

makePathsRelative :: Workspace -> FilePath -> IO WorkspaceFile
makePathsRelative ws wsFile' = do
    wsActiveProjectFile' <- case wsActiveProjectFile ws of
                            Nothing -> return Nothing
                            Just fp -> do
                                nfp <- liftIO $ myCanonicalizePath fp
                                return (Just (makeRelative (dropFileName wsFile') nfp))
    wsActivePackFile' <- case wsActivePackFile ws of
                            Nothing -> return Nothing
                            Just fp -> do
                                nfp <- liftIO $ myCanonicalizePath fp
                                return (Just (makeRelative (dropFileName wsFile') nfp))
    wsProjectFiles' <- mapM myCanonicalizePath (wsProjectFiles ws)
    let relativePaths = map (makeRelative (dropFileName wsFile')) wsProjectFiles'
    return WorkspaceFile
                { wsfVersion           = wsVersion ws
                , wsfSaveTime          = wsSaveTime ws
                , wsfName              = wsName ws
                , wsfProjectFiles      = relativePaths
                , wsfActiveProjectFile = wsActiveProjectFile'
                , wsfActivePackFile    = wsActivePackFile'
                , wsfActiveComponent   = wsActiveComponent ws
                , wsfPackageVcsConf    = packageVcsConf ws
                }

