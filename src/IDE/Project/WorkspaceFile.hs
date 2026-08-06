-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Reading, writing and mutating the workspace: the @.lkshw@ file (JSON,
-- format version 4 — unchanged, so existing workspaces keep loading), the
-- file watchers that keep the IDE in sync with on-disk edits, and the
-- workspace mutations (open\/activate\/remove project, per-project
-- settings).
--
-- Mostly carried from the old @IDE.Workspaces@\/@IDE.Workspaces.Writer@
-- (the JSON codec, deferred remote resolution and fsnotify watching are
-- recent work — see docs/relicensing.md); the reader-monad tower is gone:
-- mutations take the 'Workspace' (or read the current one) explicitly.
module IDE.Project.WorkspaceFile
  ( WorkspaceFile(..)
  , workspaceFileVersion
  , readWorkspace
  , writeWorkspace
  , installWorkspace
  , resolveDeferredProjects
  , makeProjectKeyAbsolute
    -- * Mutations
  , withWorkspace
  , projectOpenThis
  , projectOpenPath
  , dirProjectKey
  , workspaceActivatePackage
  , workspaceRemoveProject
  , setProjectSettings
    -- * Package activation side effects
  , activatePackage
  , deactivatePackage
  ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<|>))
import Control.Concurrent (putMVar, takeMVar, tryPutMVar)
import Control.Exception (evaluate)
import Control.Lens ((^.), (.~), (%~), (?~))
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson
       (FromJSON(..), ToJSON(..), eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
       (Options, defaultOptions, fieldLabelModifier, genericParseJSON,
        genericToEncoding, genericToJSON)
import Data.Char (toLower)
import Data.Function ((&))
import Data.List (find, partition, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
       (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S (fromList, insert, member)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import System.FilePath
       ((</>), (<.>), dropFileName, isAbsolute, makeRelative,
        takeExtension, takeFileName)
import System.Log.Logger (debugM)
import System.Time (getClockTime)

import IDE.Core.State
import IDE.Core.Types
import IDE.Gtk.State (postAsyncIDE)
import IDE.Project.Commands (resolveProject)
import IDE.Utils.Files (myCanonicalizePath)
import IDE.Utils.Project
       (CabalProject(..), CustomProject(..), MakeProject(..),
        NixProject(..), StackProject(..), filePathToProjectKey)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.FS
       (fsDoesFileExist, fsReadFileLazy, fsWriteFile, fsWriteFileLazy)
import IDE.Web.GhciMode (phaseSince)
import IDE.Web.RemoteRefresh (RefreshReason(..), requestRemoteRefresh)
import Data.Text.Encoding (encodeUtf8)
#if !defined(ghcjs_HOST_OS)
import Control.Exception (SomeException, catch)
import System.Directory
       (doesDirectoryExist, doesFileExist, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FSNotify
       (Event(..), StopListening, WatchManager, eventPath, watchDir,
        watchTree)
import IDE.Git (runGit)
import IDE.Pane.SourceBuffer (setModifiedOnDisk)
import IDE.Web.LocalRefresh (requestLocalRefresh)
#else
import System.Directory (doesDirectoryExist)
#endif

-- ---------------------------------------------------------------------
-- The on-disk format (.lkshw, JSON, version 4 — unchanged)
-- ---------------------------------------------------------------------

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

-- | Bumped when the on-disk shape changes incompatibly.
workspaceFileVersion :: Int
workspaceFileVersion = 4

-- ---------------------------------------------------------------------
-- Read / write
-- ---------------------------------------------------------------------

-- | Persist the workspace and install it as the live one.
writeWorkspace :: Workspace -> IDEAction
writeWorkspace ws = do
    timeNow <- liftIO getClockTime
    installWorkspace (Just ws)
    wsf <- liftIO $ makePathsRelative ws (ws ^. wsFile)
    liftIO . fsWriteFileLazy (ws ^. wsFile) . encodePretty $
        wsf { wsfVersion = workspaceFileVersion
            , wsfSaveTime = T.pack (show timeNow) }

-- | Read the workspace file.  Local projects are resolved here (globbing their
-- @packages:@ dirs and parsing each .cabal costs a few ms each); REMOTE
-- (@ssh://@) projects are NOT — they come back as package-less placeholders,
-- with their keys in the second component, for 'resolveDeferredProjects' to
-- fill in off the critical path.  Resolving a remote project needs ssh round
-- trips and this function runs before the UI is built — so doing it here
-- made every start and every ghci-mode reload wait on the network.
readWorkspace :: FilePath -> IDEM (Either String (Workspace, [ProjectKey]))
readWorkspace fp = do
    liftIO $ debugM "leksah" "readWorkspace"
    liftIO (eitherDecode <$> fsReadFileLazy fp) >>= \case
        Left pe -> error $ "Error reading file " ++ show fp ++ " " ++ show pe
        Right ws -> Right <$> makePathsAbsolute ws fp

-- | Resolve the projects 'readWorkspace' deferred, one thread each, patching
-- the LIVE workspace as each arrives (the user may have activated a different
-- package while we waited, so never write back a snapshot).  Going through
-- 'installWorkspace' means the file watchers and package activation catch up
-- exactly as they would have at boot; the web UI's project tree is derived
-- from IDE state, so rows fill in by themselves.
resolveDeferredProjects :: [ProjectKey] -> IDEAction
resolveDeferredProjects keys = forM_ keys $ \k -> forkIDE $ do
    t0 <- liftIO getCurrentTime
    mbProject <- resolveProject k
    liftIO . phaseSince t0 $ "workspace: deferred project " <> pjFileOrDir k
        <> " (" <> show (maybe 0 (length . pjPackages) mbProject) <> " pkgs)"
    case mbProject of
        Nothing -> ideMessage Normal $
            "Unable to load project : " <> T.pack (show k)
        Just project -> readIDE workspace >>= \case
            -- Still in the workspace (not closed/removed while we waited)?
            Just ws | any ((== k) . pjKey) (ws ^. wsProjects) ->
                installWorkspace . Just $ ws & wsProjects %~
                    map (\p -> if pjKey p == k then project else p)
            _ -> return ()

makeAbsolute' :: MonadIO m => FilePath -> FilePath -> m FilePath
makeAbsolute' basePath relativePath
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
    StackTool . StackProject <$> makeAbsolute' (dropFileName wsFile') f
makeProjectKeyAbsolute wsFile' (CabalTool (CabalProject f)) =
    CabalTool . CabalProject <$> makeAbsolute' (dropFileName wsFile') f
makeProjectKeyAbsolute wsFile' (CustomTool p) =
    CustomTool . (\dir -> p { pjCustomDir = dir })
        <$> makeAbsolute' (dropFileName wsFile') (pjCustomDir p)
makeProjectKeyAbsolute wsFile' (NixTool (NixProject f)) =
    NixTool . NixProject <$> makeAbsolute' (dropFileName wsFile') f
makeProjectKeyAbsolute wsFile' (MakeTool (MakeProject f)) =
    MakeTool . MakeProject <$> makeAbsolute' (dropFileName wsFile') f

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
                                Just fp -> Just <$> makeAbsolute' (dropFileName wsFile') fp
    let keys = fromMaybe (mapMaybe filePathToProjectKey (wsfProjectFiles ws)) $ wsfProjectKeys ws
    projectKeys      <- mapM (makeProjectKeyAbsolute wsFile') keys
    -- Local projects load here (a few ms each); remote ones are deferred to
    -- 'resolveDeferredProjects' — they need ssh round trips and this runs
    -- before the UI exists.  A deferred project is still IN the workspace,
    -- as a package-less placeholder, so it keeps its place in the project
    -- tree (and its per-project settings) while its packages are on the way.
    let (deferredKeys, localKeys) = partition (isRemotePath . pjFileOrDir) projectKeys
    localProjects     <- fmap catMaybes . forM localKeys $ \k -> do
        t0 <- liftIO getCurrentTime
        r <- resolveProject k
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
    projectSettings'  <- case wsfProjectSettings ws of
        Nothing -> return mempty
        Just m  -> fmap (M.fromList . catMaybes) . forM (M.toList m) $ \(k, s) -> do
            ak <- makeAbsolute' (dropFileName wsFile') k
            return $ (, s) <$> find ((== ak) . pjFileOrDir) projectKeys
    let workspace' = Workspace
                { _wsFile             = wsFile'
                , _wsProjects         = projects
                , _wsProjectSettings  = projectSettings'
                , _wsActiveProjectKey = wsActiveProjectKey'
                , _wsActivePackFile   = wsActivePackFile'
                , _wsActiveComponent  = wsfActiveComponent ws
                }
    return (workspace', deferredKeys)

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
    activeKey <- mapM (makeProjectKeyRelative wsFile') $ ws ^. wsActiveProjectKey
    activePackFile <- case ws ^. wsActivePackFile of
                            Nothing -> return Nothing
                            Just fp -> do
                                nfp <- myCanonicalizePath fp
                                return (Just (makeRelative (dropFileName wsFile') nfp))
    projectKeys' <- mapM (makeProjectKeyRelative wsFile') $ ws ^. wsProjectKeys
    settings' <- forM (M.toList (ws ^. wsProjectSettings)) $ \(pk, s) -> do
        pk' <- makeProjectKeyRelative wsFile' pk
        return (pjFileOrDir pk', s)
    return WorkspaceFile
                { wsfVersion           = workspaceFileVersion
                , wsfSaveTime          = ""
                , wsfName              = T.pack (takeFileName wsFile')
                , wsfProjectKeys       = Just projectKeys'
                , wsfProjectFiles      = mapMaybe pjFile projectKeys'
                , wsfActiveProjectKey  = activeKey
                , wsfActiveProjectFile = pjFile =<< activeKey
                , wsfActivePackFile    = activePackFile
                , wsfActiveComponent   = ws ^. wsActiveComponent
                , wsfProjectSettings   = Just (M.fromList settings')
                }

-- ---------------------------------------------------------------------
-- Package activation side effects
-- ---------------------------------------------------------------------

activatePackage :: MonadIDE m => Maybe FilePath -> Maybe Project -> Maybe Package -> Maybe Text -> m ()
activatePackage mbPath mbProject mbPack mbComponent = do
    liftIO $ debugM "leksah" $ "activatePackage " <> show (mbPath, pjKey <$> mbProject, ipdCabalFile <$> mbPack, mbComponent)
    case mbPath of
        -- A remote package's directory doesn't exist locally; leave the
        -- process cwd alone (remote runs cd on the far side).
#if !defined(ghcjs_HOST_OS)
        Just p | not (isRemotePath p) -> liftIO $ setCurrentDirectory (dropFileName p)
#endif
        _ -> return ()

deactivatePackage :: IDEAction
deactivatePackage = activatePackage Nothing Nothing Nothing Nothing

-- ---------------------------------------------------------------------
-- Install a workspace as the live one (state + activation + watchers)
-- ---------------------------------------------------------------------

-- | Make @mbWs@ the IDE's workspace: store it, re-apply the active-package
-- side effects, and reconcile the file watchers (projects gaining a watch on
-- their dir + git metadata, packages a recursive watch that feeds
-- external-modification tracking and the background build).
installWorkspace :: Maybe Workspace -> IDEAction
installWorkspace mbWs = do
    liftIO $ debugM "leksah" "installWorkspace"
    ideR <- ask
    modifyIDE_ $ workspace .~ mbWs
    case mbWs of
        Just ws | Just pj <- ws ^. wsActiveProjectKey
                , Just project <- wsLookupProject pj ws ->
            case (`pjLookupPackage` project) =<< (ws ^. wsActivePackFile) of
                Just package -> void $ activatePackage
                    (ws ^. wsActivePackFile) (Just project) (Just package)
                    (ws ^. wsActiveComponent)
                _ -> void $ activatePackage Nothing (Just project) Nothing Nothing
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
                                            installWorkspace (Just ws')
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
  where
    isSourceIn srcDir f =
        case stripPrefix srcDir f of
            Just rest -> not $ any (`isPrefixOf` rest) ["dist/", "dist-", "."]
            _ -> False
    isPrefixOf p s = take (length p) s == p

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

-- ---------------------------------------------------------------------
-- Mutations
-- ---------------------------------------------------------------------

-- | Run an action against the open workspace (a message when none is).
withWorkspace :: (Workspace -> IDEAction) -> IDEAction
withWorkspace f =
    readIDE workspace >>= \case
        Just ws -> f ws
        Nothing -> ideMessage Normal (__ "No workspace open")

-- | Add a project to the workspace by key, resolve it, make it (and its
-- first package) active, and persist.
projectOpenThis :: ProjectKey -> IDEAction
projectOpenThis projectKey = withWorkspace $ \ws -> do
    liftIO . debugM "leksah" $ "projectOpenThis " ++ show projectKey
    projectKey' <- makeProjectKeyAbsolute (ws ^. wsFile) projectKey
    resolveProject projectKey' >>= \case
        Nothing -> ideMessage Normal $ __ "Unable to load project : " <> T.pack (show projectKey')
        Just project -> do
            writeWorkspace $ ws
              & wsProjects %~ ((project :) . filter ((/= projectKey') . pjKey))
              & wsActiveProjectKey ?~ projectKey'
              & wsActivePackFile .~ (ipdCabalFile <$> listToMaybe (pjPackages project))
              & wsActiveComponent .~ Nothing
            when (isRemotePath (pjDir projectKey')) . liftIO $
                requestRemoteRefresh RefreshProjectOpened

-- | The 'ProjectKey' of a plain-directory project: a 'CustomTool' rooted at
-- @dir@ with no build/repl/doc commands.  Such a project carries no packages;
-- its files, git checkout and terminals all work from @dir@.
dirProjectKey :: FilePath -> ProjectKey
dirProjectKey dir = CustomTool (CustomProject dir [] Nothing Nothing Nothing Nothing)

-- | Add the project at a path to the workspace.  A directory is added as a
-- plain-directory project ('dirProjectKey'); anything else is treated as a
-- project file (cabal.project / stack.yaml / flake.nix / Makefile).  This is
-- the single entry shared by the Open Project / Open Folder panels and
-- @leksah-cmd project open@.
projectOpenPath :: FilePath -> IDEAction
projectOpenPath fp
    | isRemotePath fp = openFileKey fp   -- remote: can't stat; treat as a file
    | otherwise = liftIO (doesDirectoryExist fp) >>= \case
        True  -> projectOpenThis (dirProjectKey fp)
        False -> openFileKey fp
  where
    -- A Rust/Python project is identified by a marker file the user selects
    -- (Cargo.toml / pyproject.toml / setup.py); we open its CONTAINING directory
    -- as a plain-directory (CustomTool) project — the tree introspects the marker
    -- and the LSP (rust-analyzer / pyright) keys off the file's directory.
    openFileKey f
      | takeFileName f `elem` ["Cargo.toml", "pyproject.toml", "setup.py"]
          = projectOpenThis (dirProjectKey (dropFileName f))
      | otherwise = case filePathToProjectKey f of
          Just pk -> projectOpenThis pk
          Nothing -> ideMessage Normal $
              __ "Not a project file or folder : " <> T.pack f

-- | Set (and persist) the per-project settings for a project in the
-- workspace — e.g. the remote command prefix (@nix develop -c@).
setProjectSettings :: ProjectKey -> ProjectSettings -> IDEAction
setProjectSettings pk settings = withWorkspace $ \ws ->
    writeWorkspace $ ws
        & wsProjectSettings %~
            (if settings == defaultProjectSettings
                then M.delete pk
                else M.insert pk settings)

workspaceRemoveProject :: ProjectKey -> IDEAction
workspaceRemoveProject projectKey = withWorkspace $ \ws ->
    when (any ((/= projectKey) . pjKey) $ ws ^. wsProjects) $
        writeWorkspace $ ws & wsProjects %~ filter ((/= projectKey) . pjKey)

-- | Make a project (and optionally one of its packages, and optionally one
-- of that package's components) active, reorder it to the front, persist.
workspaceActivatePackage :: Project -> Maybe Package -> Maybe Text -> IDEAction
workspaceActivatePackage project mbPack exe = withWorkspace $ \ws -> do
    (mbPackFile, mbExe) <- case mbPack of
        Just pack' | ipdCabalFile pack' `elem` map ipdCabalFile (pjPackages project) -> do
            activatePackage (Just (ipdCabalFile pack')) (Just project) (Just pack') exe
            return (Just (ipdCabalFile pack'), exe)
        _ -> do
            activatePackage Nothing (Just project) Nothing Nothing
            return (Nothing, Nothing)
    writeWorkspace $ ws
             & wsProjects %~ ((project :) . filter ((/= pjKey project) . pjKey))
             & wsActiveProjectKey ?~ pjKey project
             & wsActivePackFile .~ mbPackFile
             & wsActiveComponent .~ mbExe
