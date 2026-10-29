-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The live workspace: which projects are open, what enumerating them
-- found, and what is active.  One 'Cell' holds the whole 'Ws' value; the
-- spec half ('wsSpec') mirrors the @\<name\>.leksah-workspace@ file and is
-- saved through 'IDE.Ws.File' whenever an operation changes it.
module IDE.Workspace
  ( -- * State
    ActiveTarget(..)
  , Ws
  , wsPath
  , wsSpec
  , wsProjectsMap
  , wsActive
  , emptyWs
    -- * Pure views
  , wsProjects
  , wsProjectFor
  , wsSpecFor
  , wsCmdPrefix
  , activeProject
  , activePackage
  , activeComponent
  , prDir
  , prFileOrDir
  , lookupPackage
  , packageIdText
  , wsProjectKey
  , wsIsTemporary
  , projectForPath
    -- * Service
  , WorkspaceService(..)
  , newWorkspaceService
  , wsOpenFile
  , wsSaveFile
  , projectOpenPath
  , projectOpenKey
  , workspaceRemoveProject
  , workspaceActivatePackage
  , setProjectCmdPrefix
  , reenumerateProject
  , workspaceFollowPath
  , workspaceSweepTemporary
  , workspacePinProject
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Lens (makeLenses, over, set, view)
import Control.Monad (unless, when)
import Data.List (find, isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (addTrailingPathSeparator, dropTrailingPathSeparator)

import IDE.Reactive (Cell, modifyCell, newCell, readCell)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.Coalesce (newSharedCoalescer)
import IDE.Ws.File
       (WsActive(..), WsProject(..), readWorkspaceFile, writeWorkspaceFile)
import qualified IDE.Ws.File as WF
import IDE.Ws.Registry (detectProject, discoverProject, enumerateProject)
import IDE.Ws.Types
       (Component(..), Effects, Package(..), Project(..), ProjectKey(..))

-- | What the user is building by default.
data ActiveTarget = ActiveTarget
    { atProject   :: ProjectKey
    , atPackage   :: Maybe FilePath   -- ^ manifest path
    , atComponent :: Maybe Component
    } deriving (Eq, Show)

-- | The live workspace value held in the cell.
data Ws = Ws
    { _wsPath        :: Maybe FilePath          -- ^ the @.leksah-workspace@
    , _wsSpec        :: WF.Workspace            -- ^ what the file says
    , _wsProjectsMap :: Map ProjectKey Project  -- ^ enumeration results
    , _wsActive      :: Maybe ActiveTarget
    , _wsRemembered  :: Map ProjectKey ActiveTarget
      -- ^ each project's last active target, so following panes back into a
      -- project restores the package/component chosen there
    } deriving (Eq, Show)

makeLenses ''Ws

emptyWs :: Ws
emptyWs = Ws Nothing (WF.Workspace "" [] Nothing) M.empty Nothing M.empty

-- | The key a workspace-file project entry denotes.
wsProjectKey :: WsProject -> ProjectKey
wsProjectKey p = ProjectKey (wpType p) (wpRoot p) (wpFile p)

-- | Enumerated projects, in the spec's order (unenumerated ones appear as
-- empty projects so the tree can show them immediately).
wsProjects :: Ws -> [Project]
wsProjects ws =
    [ fromMaybe (Project k []) (M.lookup k (view wsProjectsMap ws))
    | k <- map wsProjectKey (WF.wsProjects (view wsSpec ws)) ]

wsProjectFor :: ProjectKey -> Ws -> Maybe Project
wsProjectFor k = M.lookup k . view wsProjectsMap

-- | The spec entry for a key (source of overrides and command prefix).
wsSpecFor :: ProjectKey -> Ws -> Maybe WsProject
wsSpecFor k = find ((== k) . wsProjectKey) . WF.wsProjects . view wsSpec

-- | Was this project opened only because a pane showed something in it?
wsIsTemporary :: ProjectKey -> Ws -> Bool
wsIsTemporary k = maybe False wpTemporary . wsSpecFor k

-- | The workspace project a path is in — the deepest root containing it, so
-- a project nested inside another (a worktree under its repo) wins.
projectForPath :: Ws -> FilePath -> Maybe ProjectKey
projectForPath ws path =
    case sortOn (Down . length . pkRoot)
           [ k | k <- map wsProjectKey (WF.wsProjects (view wsSpec ws))
               , under (pkRoot k) ] of
        (k:_) -> Just k
        []    -> Nothing
  where
    p = dropTrailingPathSeparator path
    under root = let r = dropTrailingPathSeparator root
                 in p == r || addTrailingPathSeparator r `isPrefixOf` p

wsCmdPrefix :: ProjectKey -> Ws -> Maybe Text
wsCmdPrefix k ws = wsSpecFor k ws >>= wpCmdPrefix

activeProject :: Ws -> Maybe Project
activeProject ws = view wsActive ws >>= \t -> wsProjectFor (atProject t) ws

activePackage :: Ws -> Maybe Package
activePackage ws = do
    t <- view wsActive ws
    pr <- wsProjectFor (atProject t) ws
    case atPackage t of
        Just m  -> lookupPackage m pr
        Nothing -> case prPackages pr of
            [p] -> Just p
            _   -> Nothing

activeComponent :: Ws -> Maybe Component
activeComponent ws = view wsActive ws >>= atComponent

prDir :: Project -> FilePath
prDir = pkRoot . prKey

prFileOrDir :: Project -> FilePath
prFileOrDir p = fromMaybe (prDir p) (pkFile (prKey p))

lookupPackage :: FilePath -> Project -> Maybe Package
lookupPackage manifest = find ((== manifest) . pkgManifest) . prPackages

-- | @name-version@, the display id.
packageIdText :: Package -> Text
packageIdText p
    | T.null (pkgVersion p) = pkgName p
    | otherwise             = pkgName p <> "-" <> pkgVersion p

-- | The workspace service: the cell plus the effects used to detect and
-- enumerate, a note sink for user-visible messages, and a per-project
-- coalesced re-enumeration (watcher storms collapse to one live rescan).
data WorkspaceService = WorkspaceService
    { wsCell      :: Cell Ws
    , wsEffects   :: Effects
    , wsNote      :: Text -> IO ()
    , wsReqEnum   :: ProjectKey -> IO ()
    , wsFollowLock :: MVar ()
      -- ^ serialises 'workspaceFollowPath' / 'workspaceSweepTemporary' (one
      -- per window), so two can't both add the same discovered project
    , wsUsedPaths :: IORef (Maybe [FilePath])
      -- ^ the latest 'workspaceSweepTemporary' input: a sweep reads it when it
      -- runs, not when it was asked for, so an older set queued behind a
      -- follow can't drop the project that follow just opened
    }

newWorkspaceService :: Effects -> (Text -> IO ()) -> IO WorkspaceService
newWorkspaceService eff note = do
    cell <- newCell emptyWs
    req <- newSharedCoalescer $ \k -> do
        r <- enumerateProject eff k
        case r of
            Left err -> do
                note ("enumerate " <> T.pack (pkRoot k) <> ": " <> err)
                modifyCell cell . over wsProjectsMap $
                    M.insertWith (\_ old -> old) k (Project k [])
            Right pr ->
                modifyCell cell (over wsProjectsMap (M.insert k pr))
    lock <- newMVar ()
    used <- newIORef Nothing
    return (WorkspaceService cell eff note req lock used)

-- | Load a workspace file.  Every project in it starts enumerating in the
-- background; the cell updates as results land.
wsOpenFile :: WorkspaceService -> FilePath -> IO ()
wsOpenFile svc path =
    readWorkspaceFile (wsEffects svc) path >>= \case
        Left err -> wsNote svc ("workspace " <> T.pack path <> ": " <> err)
        Right spec -> do
            let keys = map wsProjectKey (WF.wsProjects spec)
                act = do
                    a <- WF.wsActive spec
                    k <- find ((== waProject a) . pkRoot) keys
                    return (ActiveTarget k (waPackage a) (waComponent a))
            modifyCell (wsCell svc) $
                set wsPath (Just path)
                . set wsSpec spec
                . set wsActive act
                . set wsRemembered (maybe M.empty (\a -> M.singleton (atProject a) a) act)
                . set wsProjectsMap M.empty
            mapM_ (wsReqEnum svc) keys

-- | Persist the current spec (no-op until a path exists).
wsSaveFile :: WorkspaceService -> IO ()
wsSaveFile svc = do
    ws <- readCell (wsCell svc)
    case view wsPath ws of
        Nothing -> return ()
        Just p  -> writeWorkspaceFile p (view wsSpec ws)

-- | Mutate the spec, save, and return the new value.
overSpec :: WorkspaceService -> (WF.Workspace -> WF.Workspace) -> IO ()
overSpec svc f = do
    modifyCell (wsCell svc) (over wsSpec f)
    wsSaveFile svc

-- | Detect the project type of a path (a directory, or a project file
-- like a @cabal.project@'s directory) and add it to the workspace.
projectOpenPath :: WorkspaceService -> FilePath -> IO ()
projectOpenPath svc path =
    detectProject (wsEffects svc) path >>= \case
        Nothing -> wsNote svc ("no project type claims " <> T.pack path)
        Just k  -> projectOpenKey svc k

-- | Add a known project key to the workspace (idempotent) and enumerate.
-- Opening one that is only there temporarily pins it.
projectOpenKey :: WorkspaceService -> ProjectKey -> IO ()
projectOpenKey = addProject False

addProject :: Bool -> WorkspaceService -> ProjectKey -> IO ()
addProject temp svc k = do
    ws <- readCell (wsCell svc)
    let present = any ((== k) . wsProjectKey) (WF.wsProjects (view wsSpec ws))
    if present
      then unless temp (workspacePinProject svc k)
      else overSpec svc $ \s -> s
        { WF.wsProjects = WF.wsProjects s
            <> [WsProject (pkType k) (pkRoot k) (pkFile k) M.empty Nothing temp] }
    -- First project in becomes active by default.
    when (not present && null (WF.wsProjects (view wsSpec ws))) $
        workspaceActivatePackage svc k Nothing Nothing
    unless present $ wsReqEnum svc k

-- | Make a temporary project a regular one (written to the file, and kept
-- when no pane shows it any more).
workspacePinProject :: WorkspaceService -> ProjectKey -> IO ()
workspacePinProject svc k = do
    ws <- readCell (wsCell svc)
    when (wsIsTemporary k ws) $ do
        overSpec svc $ \s -> s
            { WF.wsProjects =
                [ if wsProjectKey p == k then p { wpTemporary = False } else p
                | p <- WF.wsProjects s ] }
        -- It may be the active one, which the file couldn't record until now.
        mapM_ (\a -> when (atProject a == k) (saveActive svc a)) (view wsActive ws)

-- | A pane showing @path@ came to the front: make its project the active
-- one.  A path in no workspace project opens the project it belongs to
-- ('discoverProject', never claiming the directories in @stops@) as a
-- TEMPORARY one — local paths only: walking up a remote tree is a round
-- trip per directory.  A path in no project at all changes nothing.
workspaceFollowPath :: WorkspaceService -> [FilePath] -> FilePath -> IO ()
workspaceFollowPath svc stops path = withMVar (wsFollowLock svc) $ \_ -> do
    ws <- readCell (wsCell svc)
    mk <- case projectForPath ws path of
        Just k  -> pure (Just k)
        Nothing | isRemotePath path -> pure Nothing
                | otherwise -> discoverProject (wsEffects svc) stops path >>= \case
                    Nothing -> pure Nothing
                    Just k  -> Just k <$ addProject True svc k
    ws' <- readCell (wsCell svc)
    case mk of
        Just k | (atProject <$> view wsActive ws') /= Just k -> do
            let ActiveTarget _ mbManifest mbComp =
                    M.findWithDefault (ActiveTarget k Nothing Nothing) k
                        (view wsRemembered ws')
            workspaceActivatePackage svc k mbManifest mbComp
            -- The project it moved away from may have been kept only for
            -- being active.
            sweep svc
        _ -> return ()

-- | Drop the temporary projects none of @paths@ (everything the open panes
-- show: files, checkouts, terminal directories) is in any more — except the
-- active one, which goes when the active project next moves on.
workspaceSweepTemporary :: WorkspaceService -> [FilePath] -> IO ()
workspaceSweepTemporary svc paths = do
    writeIORef (wsUsedPaths svc) (Just paths)
    withMVar (wsFollowLock svc) $ \_ -> sweep svc

-- | The sweep itself (under the lock), against the latest paths.
sweep :: WorkspaceService -> IO ()
sweep svc = readIORef (wsUsedPaths svc) >>= mapM_ (\paths -> do
    ws <- readCell (wsCell svc)
    let used = (atProject <$> view wsActive ws)
               : [ Just k | p <- paths, Just k <- [projectForPath ws p] ]
    mapM_ (workspaceRemoveProject svc)
        [ k | p <- WF.wsProjects (view wsSpec ws), wpTemporary p
            , let k = wsProjectKey p, Just k `notElem` used ])

workspaceRemoveProject :: WorkspaceService -> ProjectKey -> IO ()
workspaceRemoveProject svc k = do
    overSpec svc $ \s -> s
        { WF.wsProjects = filter ((/= k) . wsProjectKey) (WF.wsProjects s) }
    modifyCell (wsCell svc) $ \ws ->
        over wsProjectsMap (M.delete k)
        . over wsRemembered (M.delete k)
        . (if (atProject <$> view wsActive ws) == Just k
              then set wsActive Nothing else id)
        $ ws

-- | Set the active target (project, optional package manifest, optional
-- component) — mirrored into the spec so it survives restarts.
workspaceActivatePackage
    :: WorkspaceService -> ProjectKey -> Maybe FilePath -> Maybe Component -> IO ()
workspaceActivatePackage svc k mbManifest mbComp = do
    let a = ActiveTarget k mbManifest mbComp
    modifyCell (wsCell svc) $
        set wsActive (Just a) . over wsRemembered (M.insert k a)
    -- The file keeps the last REGULAR project that was active: a temporary
    -- one won't be there to activate when the workspace is next opened.
    temp <- wsIsTemporary k <$> readCell (wsCell svc)
    unless temp $ saveActive svc a

saveActive :: WorkspaceService -> ActiveTarget -> IO ()
saveActive svc (ActiveTarget k mbManifest mbComp) =
    overSpec svc $ \s ->
        s { WF.wsActive = Just (WsActive (pkRoot k) mbManifest mbComp) }

setProjectCmdPrefix :: WorkspaceService -> ProjectKey -> Maybe Text -> IO ()
setProjectCmdPrefix svc k pre = overSpec svc $ \s -> s
    { WF.wsProjects =
        [ if wsProjectKey p == k then p { wpCmdPrefix = pre } else p
        | p <- WF.wsProjects s ] }

-- | Ask for a (coalesced) re-enumeration — watchers call this freely.
reenumerateProject :: WorkspaceService -> ProjectKey -> IO ()
reenumerateProject = wsReqEnum
