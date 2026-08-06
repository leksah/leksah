{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    ,makeProjectKeyAbsolute
    ,makePathsAbsolute
    ,WorkspaceFile(..)
    ,emptyWorkspaceFile
    ,setWorkspace
    ,resolveDeferredProjects
    ,workspaceVersion
) where

import Prelude ()
import Prelude.Compat
import IDE.Core.Types
import IDE.Core.State
import IDE.Gtk.State
import IDE.Package
       (activatePackage, deactivatePackage, ideProjectFromKey)
import IDE.Utils.FileUtils(myCanonicalizePath)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.GhciMode (phaseSince)

import Data.Maybe
import Data.Function ((&))
import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Lens ((^.), (.~), (%~))
import System.Time (getClockTime)
import System.FilePath
       (takeFileName, (</>), isAbsolute, dropFileName, makeRelative)
import System.Log.Logger (debugM)
import qualified Data.Text as T (unpack, pack)
#if !defined(ghcjs_HOST_OS)
import System.FSNotify (watchDir, Event(..), watchTree, eventPath, StopListening, WatchManager)
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode(..))
import Control.Exception (catch, SomeException)
import IDE.Git (runGit)
import IDE.Web.LocalRefresh (requestLocalRefresh)
#endif
import Control.Monad.Reader (MonadReader(..))
import Data.Traversable (forM)
import Data.Time.Clock (getCurrentTime)
import qualified Data.Map as Map (empty)
import Data.Text (Text)
import Data.Map (Map)
import GHC.Generics (Generic)
-- Workspace file access goes through the IDE.Web.FS seam (real FS
-- natively; the in-memory demo tree in the browser build).
import IDE.Web.FS (fsReadFileLazy, fsWriteFileLazy)
import Data.Aeson
       (eitherDecode, ToJSON(..), FromJSON(..))
import Data.Aeson.Types
       (Options, genericParseJSON, genericToEncoding, genericToJSON,
        defaultOptions, fieldLabelModifier)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (isPrefixOf, stripPrefix, partition)
import Data.Char (toLower)
import IDE.Pane.SourceBuffer (setModifiedOnDisk)
import Control.Concurrent (putMVar, takeMVar, tryPutMVar)
import qualified Data.Set as S (fromList, insert, member)
import Control.Exception (evaluate)
import qualified Data.Map as M
       (partitionWithKey, fromList, member, toList, lookup)
import Data.List (find)
import Data.Foldable (forM_)

data WorkspaceFile = WorkspaceFile {
    wsfVersion           ::   Int
,   wsfSaveTime          ::   Text
,   wsfName              ::   Text
,   wsfProjectFiles      ::   [FilePath]
,   wsfProjectKeys       ::   Maybe [ProjectKey]
,   wsfActiveProjectFile ::   Maybe FilePath
,   wsfActiveProjectKey  ::   Maybe ProjectKey
,   wsfActivePackFile    ::   Maybe FilePath
,   wsfActiveComponent   ::   Maybe Text
,   wsfPackageVcsConf    ::   Map FilePath VCSConf
    -- | Per-project settings keyed by the (relativized; remote verbatim)
    -- project file-or-dir.  Maybe so old workspace files still parse.
,   wsfProjectSettings   ::   Maybe (Map FilePath ProjectSettings)
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
    let newWs    =  ws & wsSaveTime .~ T.pack (show timeNow)
                       & wsVersion .~ workspaceVersion
    setWorkspace $ Just newWs
    newWs' <- liftIO $ makePathsRelative newWs (ws ^. wsFile)
    liftIO . fsWriteFileLazy (ws ^. wsFile) $ encodePretty newWs'

-- | Read the workspace file.  Local projects are resolved here (globbing their
-- @packages:@ dirs and parsing each .cabal costs a few ms each); REMOTE
-- (@ssh://@) projects are NOT — they come back as package-less placeholders,
-- with their keys in the second component, for 'resolveDeferredProjects' to
-- fill in off the critical path.  Resolving a remote project needs ssh round
-- trips (measured: 12.5s for a 15-package project), and this function runs
-- before the UI is built — so doing it here made every start and every
-- ghci-mode reload wait on the network.
readWorkspace :: FilePath -> IDEM (Either String (Workspace, [ProjectKey]))
readWorkspace fp = do
    liftIO $ debugM "leksah" "readWorkspace"
    liftIO (eitherDecode <$> fsReadFileLazy fp) >>= \case
        Left pe -> error $ "Error reading file " ++ show fp ++ " " ++ show pe
        Right ws -> do
            r <- makePathsAbsolute ws fp
            --TODO set package vcs here
            return $ Right r

-- | Resolve the projects 'readWorkspace' deferred, one thread each, patching
-- the LIVE workspace as each arrives (the user may have activated a different
-- package while we waited, so never write back a snapshot).  Going through
-- 'setWorkspace' means the file watchers, package activation and the
-- @WorkspaceChanged@ event all catch up exactly as they would have at boot; the
-- web UI's project tree is derived from IDE state, so rows fill in by
-- themselves.
resolveDeferredProjects :: [ProjectKey] -> IDEAction
resolveDeferredProjects keys = forM_ keys $ \k -> forkIDE $ do
    t0 <- liftIO getCurrentTime
    mbProject <- ideProjectFromKey k
    liftIO . phaseSince t0 $ "workspace: deferred project " <> pjFileOrDir k
        <> " (" <> show (maybe 0 (length . pjPackages) mbProject) <> " pkgs)"
    case mbProject of
        Nothing -> ideMessage Normal $
            "Unable to load project : " <> T.pack (show k)
        Just project -> readIDE workspace >>= \case
            -- Still in the workspace (not closed/removed while we waited)?
            Just ws | any ((== k) . pjKey) (ws ^. wsProjects) ->
                setWorkspace . Just $ ws & wsProjects %~
                    map (\p -> if pjKey p == k then project else p)
            _ -> return ()

makeAbsolute :: MonadIO m => FilePath -> FilePath -> m FilePath
makeAbsolute basePath relativePath
    -- ssh://host/… paths are absolute on their host; joining the (local)
    -- workspace dir or canonicalizing locally would mangle them.
    | isRemotePath relativePath = return relativePath
    | otherwise = liftIO $
        myCanonicalizePath
           (if isAbsolute relativePath
                then relativePath
                else basePath </> relativePath)

makeProjectKeyAbsolute :: MonadIO m => FilePath -> ProjectKey -> m ProjectKey
makeProjectKeyAbsolute wsFile' (StackTool (StackProject f)) =
    StackTool . StackProject <$> makeAbsolute (dropFileName wsFile') f
makeProjectKeyAbsolute wsFile' (CabalTool (CabalProject f)) =
    CabalTool . CabalProject <$> makeAbsolute (dropFileName wsFile') f
makeProjectKeyAbsolute wsFile' (CustomTool p) =
    CustomTool . (\dir -> p { pjCustomDir = dir })
        <$> makeAbsolute (dropFileName wsFile') (pjCustomDir p)
makeProjectKeyAbsolute wsFile' (NixTool (NixProject f)) =
    NixTool . NixProject <$> makeAbsolute (dropFileName wsFile') f
makeProjectKeyAbsolute wsFile' (MakeTool (MakeProject f)) =
    MakeTool . MakeProject <$> makeAbsolute (dropFileName wsFile') f

-- | The workspace with every path absolutized, plus the keys of the projects
-- whose contents were NOT loaded (see 'readWorkspace') and which
-- 'resolveDeferredProjects' must still fill in.
makePathsAbsolute :: WorkspaceFile -> FilePath -> IDEM (Workspace, [ProjectKey])
makePathsAbsolute ws bp = do
    wsFile'           <-  liftIO $ myCanonicalizePath bp
    wsActiveProjectKey' <- mapM (makeProjectKeyAbsolute wsFile') $
        wsfActiveProjectKey ws <|> (wsfActiveProjectFile ws >>= filePathToProjectKey)
    wsActivePackFile' <-  case wsfActivePackFile ws of
                                Nothing -> return Nothing
                                Just fp -> do
                                    fp' <- liftIO $ makeAbsolute (dropFileName wsFile') fp
                                    return (Just fp')
    let keys = fromMaybe (mapMaybe filePathToProjectKey (wsfProjectFiles ws)) $ wsfProjectKeys ws
    projectKeys      <- mapM (makeProjectKeyAbsolute wsFile') keys
    -- Local projects load here (a few ms each); remote ones are deferred to
    -- 'resolveDeferredProjects' — they need ssh round trips and this runs
    -- before the UI exists.  A deferred project is still IN the workspace,
    -- as a package-less placeholder, so it keeps its place in the project
    -- tree (and its per-project settings) while its packages are on the way.
    -- Per-project timing is logged either way: this used to be ~14s of an
    -- ~18s ghci reload, essentially all of it the two remote projects.
    let (deferredKeys, localKeys) = partition (isRemotePath . pjFileOrDir) projectKeys
    localProjects     <- fmap catMaybes . forM localKeys $ \k -> do
        t0 <- liftIO getCurrentTime
        r <- ideProjectFromKey k
        liftIO . phaseSince t0 $ "boot: project " <> pjFileOrDir k
            <> " (" <> show (maybe 0 (length . pjPackages) r) <> " pkgs)"
        return r
    -- Keep the file's project order, with placeholders where deferred.  A local
    -- project that failed to load is dropped, as before.
    let byKey    = M.fromList [ (pjKey p, p) | p <- localProjects ]
        projects = concat [ case M.lookup k byKey of
                              Just p                          -> [p]
                              Nothing | k `elem` deferredKeys  -> [Project k mempty]
                                      | otherwise             -> []
                          | k <- projectKeys ]
    -- Re-associate persisted per-project settings with the absolutized keys
    -- (the stored key is the relativized pjFileOrDir; remote ones verbatim).
    projectSettings  <- case wsfProjectSettings ws of
        Nothing -> return mempty
        Just m  -> fmap (M.fromList . catMaybes) . forM (M.toList m) $ \(k, s) -> do
            ak <- makeAbsolute (dropFileName wsFile') k
            return $ (, s) <$> find ((== ak) . pjFileOrDir) projectKeys
    let workspace' = Workspace
                { _wsFile             = wsFile'
                , _wsVersion          = wsfVersion ws
                , _wsSaveTime         = wsfSaveTime ws
                , _wsName             = wsfName ws
                , _wsProjects         = projects
                , _wsProjectSettings  = projectSettings
                , _wsActiveProjectKey = wsActiveProjectKey'
                , _wsActivePackFile   = wsActivePackFile'
                , _wsActiveComponent  = wsfActiveComponent ws
                , _packageVcsConf     = wsfPackageVcsConf ws
                }
    return (workspace', deferredKeys)

--emptyWorkspace :: Workspace
--emptyWorkspace =  Workspace {
--    _wsVersion            =   workspaceVersion
--,   _wsSaveTime           =   ""
--,   _wsName               =   ""
--,   _wsFile               =   ""
--,   _wsProjects           =   []
--,   _wsActiveProjectFile  =   Nothing
--,   _wsActivePackFile     =   Nothing
--,   _wsActiveComponent    =   Nothing
--,   _packageVcsConf       =   Map.empty
--}

emptyWorkspaceFile :: WorkspaceFile
emptyWorkspaceFile =  WorkspaceFile {
    wsfVersion           =   workspaceVersion
,   wsfSaveTime          =   ""
,   wsfName              =   ""
,   wsfProjectKeys       =   Nothing
,   wsfProjectFiles      =   []
,   wsfActiveProjectKey  =   Nothing
,   wsfActiveProjectFile =   Nothing
,   wsfActivePackFile    =   Nothing
,   wsfActiveComponent   =   Nothing
,   wsfPackageVcsConf    =   Map.empty
,   wsfProjectSettings   =   Nothing
}

getProject :: ProjectKey -> [Project] -> Maybe Project
getProject pk projects =
    case filter (\ p -> pjKey p == pk) projects of
        [p] -> Just p
        _   -> Nothing

getPackage :: FilePath -> [IDEPackage] -> Maybe IDEPackage
getPackage fp packages =
    case filter (\ p -> ipdCabalFile p == fp) packages of
        [p] -> Just p
        _   -> Nothing

-- ---------------------------------------------------------------------
-- This needs to be incremented, when the workspace format changes
--
workspaceVersion :: Int
workspaceVersion = 4

setWorkspace :: Maybe Workspace -> IDEAction
setWorkspace mbWs = do
    liftIO $ debugM "leksah" "setWorkspace"
    ideR <- ask
--    mbOldWs <- readIDE workspace
    modifyIDE_ $ workspace .~ mbWs
    let packFileAndExe =  case mbWs of
                            Nothing -> Nothing
                            Just ws -> Just (ws ^. wsActiveProjectKey, ws ^. wsActivePackFile, ws ^. wsActiveComponent)
--    let oldPackFileAndExe = case mbOldWs of
--                            Nothing -> Nothing
--                            Just ws -> Just (ws ^. wsActiveProjectFile, ws ^. wsActivePackFile , ws ^. wsActiveComponent)
    case (packFileAndExe, mbWs) of
        (Just (Just pj, mbPackFile, mbExe), Just ws) ->
            case getProject pj (ws ^. wsProjects) of
                Just project ->
                    case (`getPackage` pjPackages project) =<< mbPackFile of
                        Just package -> void (activatePackage mbPackFile (Just project) (Just package) mbExe)
                        _ -> void (activatePackage Nothing (Just project) Nothing Nothing)
                _ -> deactivatePackage
        _ -> deactivatePackage
    case mbWs of
        Just ws -> do
#if !defined(ghcjs_HOST_OS)
            -- Only the (native-only) watcher callbacks below use these.
            fsn <- readIDE fsnotify
            tb <- readIDE triggerBuild
            extModsMVar <- readIDE externalModified
            let rebuild = void . liftIO $ tryPutMVar tb ()
#endif
            watchersMVar <- readIDE watchers
            liftIO $ do
                oldWatchers <- takeMVar watchersMVar
                let projectFiles = S.fromList $ map pjKey $ ws ^. wsProjects
                    packageFiles = S.fromList $ map ipdCabalFile $ pjPackages =<< ws ^. wsProjects
                    newProjects = filter (not . (`M.member` fst oldWatchers) . pjKey) $ ws ^. wsProjects
                    newPackages = filter (not . (`M.member` snd oldWatchers) . ipdCabalFile) $ pjPackages =<< ws ^. wsProjects
#if defined(ghcjs_HOST_OS)
                -- No file watching in the browser demo: shape-compatible
                -- no-op watchers (StopListening = pure ()).
                newProjectWatchers <- forM newProjects $ \project ->
                    return (pjKey project, return ())
                newPackageWatchers <- forM newPackages $ \package ->
                    return (ipdCabalFile package, return ())
#else
                newProjectWatchers <- forM newProjects $ \project ->
                  -- Remote projects: no local file watching (refresh is
                  -- event-driven); shape-compatible no-op watcher.
                  if isRemotePath (pjDir (pjKey project))
                   then return (pjKey project, return ())
                   else do
                    debugM "leksah" $ "Watching project " <> show (pjKey project)
                    stopMain <- watchDir fsn (pjDir $ pjKey project) (\case
                        Modified {} -> True
                        Added {} -> True
                        Removed {} -> True
                        _ -> False) $ \event -> do
                            let f = eventPath event
                            requestLocalRefresh f
                            case event of
                                Removed {} -> return ()  -- deleted: refresh only; don't stat it
                                _          -> void . (`reflectIDE` ideR) $ setModifiedOnDisk f
                            when (Just (takeFileName f) == (takeFileName <$> pjFile (pjKey project))) $
                                (`reflectIDE` ideR) $ postAsyncIDE $
                                    readWorkspace (ws ^. wsFile) >>= \case
                                        Left _ -> return ()
                                        Right (ws', deferred) -> do
                                            setWorkspace (Just ws')
                                            resolveDeferredProjects deferred
                    -- Also watch the repo's git metadata (index/HEAD/refs) so
                    -- external commits/checkouts/staging refresh Changes.
                    stopGit <- watchGitMeta fsn (pjDir $ pjKey project)
                    return (pjKey project, stopMain >> stopGit)
                newPackageWatchers <- forM newPackages $ \package ->
                  if isRemotePath (ipdCabalFile package)
                   then return (ipdCabalFile package, return ())
                   else do
                    debugM "leksah" $ "Watching package " <> show (ipdCabalFile package)
                    nonRootSrcPaths <- map (<>"/") . filter (/=ipdPackageDir package) <$>
                        mapM (myCanonicalizePath . (ipdPackageDir package </>)) (ipdSrcDirs package)

                    fmap (ipdCabalFile package,) <$> watchTree fsn (ipdPackageDir package) (\case
                        Modified {} -> True
                        Added {} -> True
                        Removed {} -> True
                        _ -> False) $ \event -> do
                            let f = eventPath event
                            requestLocalRefresh f
                            case event of
                              Removed {} -> return ()  -- deleted: refresh only; don't stat it
                              _ ->
                                (`reflectIDE` ideR) $ setModifiedOnDisk f >>= \case
                                  True -> rebuild
                                  False ->
                                    when (any (`isSourceIn` f) nonRootSrcPaths) $ do
                                        liftIO $ debugM "leksah" $ "Modified source file " <> f <> " in " <> T.unpack (ipdPackageName package)
                                        extMods <- liftIO $ takeMVar extModsMVar
                                        liftIO $ putMVar extModsMVar =<< evaluate (S.insert f extMods)
                                        rebuild
#endif
                let (keepProjectWatchers, discardProjectWatches) = M.partitionWithKey (\f _ -> f `S.member` projectFiles) $ fst oldWatchers
                    (keepPackageWatchers, discardPackageWatches) = M.partitionWithKey (\f _ -> f `S.member` packageFiles) $ snd oldWatchers
                forM_ discardProjectWatches id
                forM_ discardPackageWatches id
                putMVar watchersMVar (keepProjectWatchers <> M.fromList newProjectWatchers, keepPackageWatchers <> M.fromList newPackageWatchers)
        Nothing -> return ()
    return ()
  where
    isSourceIn srcDir f =
        case stripPrefix srcDir f of
            Just rest -> not $ any (`isPrefixOf` rest) ["dist/", "dist-", "."]
            _ -> False

#if !defined(ghcjs_HOST_OS)
-- | Watch a local project's git metadata (index, HEAD, refs) so external git
-- operations — commit, checkout, stage from a terminal — refresh the Changes
-- pane without polling.  The git-dir is resolved with @git rev-parse@ (handles
-- enclosing repos, worktrees and submodules); returns a combined
-- 'StopListening' (@return ()@ if @dir@ isn't in a git repo or git is
-- unavailable).  Only the @.git@ dir + its @refs@ subtree are watched, never
-- @objects@ (which churns on every fetch/gc).
watchGitMeta :: WatchManager -> FilePath -> IO StopListening
watchGitMeta fsn dir = do
    res <- runGit dir ["rev-parse", "--absolute-git-dir"]
             `catch` \(_ :: SomeException) -> return (ExitFailure 1, "", "")
    case res of
        (ExitSuccess, out, _)
          | (gd:_) <- lines (T.unpack out), not (null gd) -> do
              stopTop  <- watchDir fsn gd (const True) fire
              hasRefs  <- doesDirectoryExist (gd </> "refs")
              stopRefs <- if hasRefs
                            then watchTree fsn (gd </> "refs") (const True) fire
                            else return (return ())
              return (stopTop >> stopRefs)
        _ -> return (return ())
  where
    fire = requestLocalRefresh . eventPath
#endif

makeProjectKeyRelative :: FilePath -> ProjectKey -> IO ProjectKey
makeProjectKeyRelative wsFile' (StackTool (StackProject f)) =
    StackTool . StackProject <$> relativeTo wsFile' f
makeProjectKeyRelative wsFile' (CabalTool (CabalProject f)) =
    CabalTool . CabalProject <$> relativeTo wsFile' f
makeProjectKeyRelative wsFile' (CustomTool p) =
    CustomTool . (\dir -> p { pjCustomDir = dir }) <$> relativeTo wsFile' (pjCustomDir p)
makeProjectKeyRelative wsFile' (NixTool (NixProject f)) =
    NixTool . NixProject <$> relativeTo wsFile' f
makeProjectKeyRelative wsFile' (MakeTool (MakeProject f)) =
    MakeTool . MakeProject <$> relativeTo wsFile' f

-- Remote paths are stored verbatim in the .lkshw (they are not relative to
-- anything local); local ones are canonicalized and relativized to the
-- workspace-file directory as before.
relativeTo :: FilePath -> FilePath -> IO FilePath
relativeTo wsFile' f
    | isRemotePath f = return f
    | otherwise = makeRelative (dropFileName wsFile') <$> myCanonicalizePath f

makePathsRelative :: Workspace -> FilePath -> IO WorkspaceFile
makePathsRelative ws wsFile' = do
    wsActiveProjectKey' <- mapM (makeProjectKeyRelative wsFile') $ ws ^. wsActiveProjectKey
    wsActivePackFile' <- case ws ^. wsActivePackFile of
                            Nothing -> return Nothing
                            Just fp -> do
                                nfp <- liftIO $ myCanonicalizePath fp
                                return (Just (makeRelative (dropFileName wsFile') nfp))
    wsProjectKeys' <- mapM (makeProjectKeyRelative wsFile') $ ws ^. wsProjectKeys
    wsProjectSettings' <- forM (M.toList (ws ^. wsProjectSettings)) $ \(pk, s) -> do
        pk' <- makeProjectKeyRelative wsFile' pk
        return (pjFileOrDir pk', s)
    return WorkspaceFile
                { wsfVersion           = ws ^. wsVersion
                , wsfSaveTime          = ws ^. wsSaveTime
                , wsfName              = ws ^. wsName
                , wsfProjectKeys       = Just wsProjectKeys'
                , wsfProjectFiles      = mapMaybe pjFile wsProjectKeys'
                , wsfActiveProjectKey  = wsActiveProjectKey'
                , wsfActiveProjectFile = wsActiveProjectKey' >>= pjFile
                , wsfActivePackFile    = wsActivePackFile'
                , wsfActiveComponent   = ws ^. wsActiveComponent
                , wsfPackageVcsConf    = ws ^. packageVcsConf
                , wsfProjectSettings   = if null wsProjectSettings'
                                            then Nothing
                                            else Just (M.fromList wsProjectSettings')
                }

