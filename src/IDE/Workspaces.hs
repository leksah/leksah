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
    projectOpenThis
,   workspaceClean
,   workspaceMake
,   workspaceActivatePackage
,   projectAddPackage'
,   workspaceRemoveProject
,   projectRemovePackage
,   workspaceTryQuiet
,   projectNewHere
,   projectTryQuiet
,   packageTryQuiet

,   backgroundMake
,   makePackage'
,   constructAndOpenMainModules
) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<$>))
import Control.Concurrent (putMVar, takeMVar)
import Control.Exception (SomeException)
import Control.Lens ((^.), (.~), (%~), (?~))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)

import Data.Foldable (forM_)
import Data.Function ((&))
import qualified Data.Map as M (insert)
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Set as S (toList)
import Data.Text (Text)
import qualified Data.Text as T
       (unlines, isPrefixOf, lines, pack)
import qualified Data.Text.IO as T (readFile, writeFile)

import Distribution.PackageDescription (hsSourceDirs)

import System.Directory
       (createDirectoryIfMissing, doesFileExist)
import System.FilePath
       (takeFileName, (</>), dropFileName, makeRelative,
        takeBaseName, takeExtension, (<.>))
import System.Log.Logger (debugM)

import IDE.Build
       (moNoOp, makePackages, defaultMakeSettings, MakeOp(..),
        MakeSettings(..))
import IDE.Core.State
       (IDEPackage, IDEAction, readIDE, prefs, liftIDE, WorkspaceAction,
        ProjectAction, PackageAction, ProjectM, ProjectTool(..),
        pjPackageMap, Project, ideMessage, MessageLevel(..), workspace,
        runWorkspace, activeProject, runProject, catchIDE, IDEEvent(..),
        wsProjects, wsActiveProjectFile, wsActivePackFile, ipdCabalFile,
        pjPackages, wsActiveComponent, triggerEventIDE_, pjFile, ipdMain,
        ipdPackageDir, pjTool, activePack, runPackage, saveAllBeforeBuild,
        __, externalModified, forkIDE, sysMessage, ipdPackageName, native,
        developLeksah, belongsToPackage)
import IDE.Package
       (getModuleTemplate, idePackageFromPath',
        getPackageDescriptionAndPath, activatePackage,
        ideProjectFromPath)
import IDE.Pane.Log (showDefaultLogLaunch')
import IDE.Pane.SourceBuffer
       (IDEBuffer(..), fileOpenThis, fileCheckAll)
import qualified IDE.Workspaces.Writer as Writer
import IDE.Utils.FileUtils (myCanonicalizePath)

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
                    lift $ Writer.writeWorkspace $ ws
                      & wsProjects %~ (project :)
                      & wsActiveProjectFile ?~ cPath
                      & wsActivePackFile .~ (ipdCabalFile <$> listToMaybe (pjPackages project))
                      & wsActiveComponent .~ Nothing

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

projectAddPackage' :: FilePath -> ProjectM (Maybe IDEPackage)
projectAddPackage' fp = do
    project <- ask
    ws <- lift ask
    let projectFile = pjFile project
    cfp <- liftIO $ myCanonicalizePath fp
    liftIDE (idePackageFromPath' cfp) >>= \case
        Just pack' -> do
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
                Writer.writeWorkspace $ ws
                  & wsProjects %~ map (\p -> if pjFile p == projectFile
                                                then p { pjPackageMap = M.insert (ipdCabalFile pack') pack' $ pjPackageMap p }
                                                else p)
                  & wsActivePackFile ?~ ipdCabalFile pack'
                  & wsActiveComponent .~ Nothing
            return (Just pack')
        Nothing -> return Nothing

packageTryQuiet :: PackageAction -> IDEAction
packageTryQuiet f = do
    maybePackage <- readIDE activePack
    case maybePackage of
        Just p  -> projectTryQuiet $ runPackage f p
        Nothing -> ideMessage Normal (__ "No active package")


workspaceRemoveProject :: FilePath -> WorkspaceAction
workspaceRemoveProject projectFile = do
    ws <- ask
    when (any ((/= projectFile) . pjFile) $ ws ^. wsProjects) . lift $
        Writer.writeWorkspace $ ws & wsProjects %~ filter ((/= projectFile) . pjFile)

projectRemovePackage :: IDEPackage -> ProjectAction
projectRemovePackage _pack =
--    ws <- ask
--    when (pack `elem` wsPackages ws) $ lift $
--        Writer.writeWorkspace ws {wsProjects =  delete pack (wsPackages ws)}
    ideMessage Normal "projectRemovePackage not implemented"

workspaceActivatePackage :: Project -> Maybe IDEPackage -> Maybe Text -> WorkspaceAction
workspaceActivatePackage project mbPack exe = do
    (mbPackFile, mbExe) <- liftIDE $ case mbPack of
        Just pack' | ipdCabalFile pack' `elem` map ipdCabalFile (pjPackages project) -> do
            activatePackage (Just (ipdCabalFile pack')) (Just project) (Just pack') exe
            return (Just (ipdCabalFile pack'), exe)
        _ -> do
            activatePackage Nothing (Just project) Nothing Nothing
            return (Nothing, Nothing)
    ws <- ask
    liftIDE $ Writer.writeWorkspace $ ws
             & wsProjects %~ ((project :) . filter ((/= pjFile project) . pjFile))
             & wsActiveProjectFile ?~ pjFile project
             & wsActivePackFile .~ mbPackFile
             & wsActiveComponent .~ mbExe

------------------------
-- Workspace make

workspaceClean :: WorkspaceAction
workspaceClean = do
    ws <- ask
    settings <- lift $ do
        prefs' <- readIDE prefs
        return (defaultMakeSettings prefs')
    lift $ makePackages settings (map (\p -> (p, pjPackages p)) $ ws ^. wsProjects) MoClean MoClean moNoOp

buildSteps :: MakeSettings -> [MakeOp]
buildSteps settings =
    MoBuild
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
    let build = buildSteps settings
    lift $ makePackages settings (map (\p -> (p, pjPackages p)) $ ws ^. wsProjects) (MoComposed build) (MoComposed build) MoMetaInfo

backgroundMake :: IDEAction
backgroundMake = catchIDE (do
    prefs'       <- readIDE prefs
    modifiedFiles <- catMaybes <$> if saveAllBeforeBuild prefs'
                        then fileCheckAll (return . return . fileName)
                        else return []
    extModsMVar <- readIDE externalModified
    extMods <- liftIO $ S.toList <$> takeMVar extModsMVar
    liftIO $ putMVar extModsMVar mempty
    workspaceTryQuiet $ do
        ws <- ask
        liftIDE . forkIDE $ do
            let modifiedPacks = filter (not . null . snd) $
                                  map (\project -> (project, filter (\pack' ->
                                    any (`belongsToPackage` pack') $ modifiedFiles <> extMods) (pjPackages project))) (ws ^. wsProjects)
                settings = defaultMakeSettings prefs'
                steps = buildSteps settings
            if msSingleBuildWithoutLinking settings && not (msMakeMode settings)
                then makePackages settings modifiedPacks (MoComposed steps) (MoComposed []) moNoOp
                else makePackages settings modifiedPacks (MoComposed steps)
                                    (MoComposed steps) MoMetaInfo
    )
    (\(e :: SomeException) -> sysMessage Normal (T.pack $ show e))

makePackage' :: PackageAction
makePackage' = do
  liftIO $ debugM "leksah" "makePackage'"
  p <- ask
  project <- lift ask
  liftIDE $ do
    showDefaultLogLaunch'
    prefs' <- readIDE prefs
    let settings = (defaultMakeSettings prefs')
            { msBackgroundBuild = False
            , msSuccessAction = when (ipdPackageName p == "leksah" && native prefs') $
                readIDE developLeksah >>= \case
                    False -> return ()
                    True -> triggerEventIDE_ QuitToRestart }
    let steps = buildSteps settings
    if msSingleBuildWithoutLinking settings && not (msMakeMode settings)
        then makePackages settings [(project, [p])] (MoComposed steps) (MoComposed []) moNoOp
        else makePackages settings [(project, [p])] (MoComposed steps) (MoComposed steps) MoMetaInfo
