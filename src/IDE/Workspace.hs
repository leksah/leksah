-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The live workspace: which projects are open, what enumerating them
-- found, and what is active.  One 'Cell' holds the whole 'Ws' value; the
-- spec half ('wsSpec') mirrors the @\<name\>.leksah.json@ file and is
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
  ) where

import Control.Lens (makeLenses, over, set, view)
import Control.Monad (unless, when)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import IDE.Reactive (Cell, modifyCell, newCell, readCell)
import IDE.Web.Coalesce (newSharedCoalescer)
import IDE.Ws.File
       (WsActive(..), WsProject(..), readWorkspaceFile, writeWorkspaceFile)
import qualified IDE.Ws.File as WF
import IDE.Ws.Registry (detectProject, enumerateProject)
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
    { _wsPath        :: Maybe FilePath          -- ^ the @.leksah.json@
    , _wsSpec        :: WF.Workspace            -- ^ what the file says
    , _wsProjectsMap :: Map ProjectKey Project  -- ^ enumeration results
    , _wsActive      :: Maybe ActiveTarget
    } deriving (Eq, Show)

makeLenses ''Ws

emptyWs :: Ws
emptyWs = Ws Nothing (WF.Workspace "" [] Nothing) M.empty Nothing

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
    return (WorkspaceService cell eff note req)

-- | Load a workspace file.  Every project in it starts enumerating in the
-- background; the cell updates as results land.
wsOpenFile :: WorkspaceService -> FilePath -> IO ()
wsOpenFile svc path =
    readWorkspaceFile path >>= \case
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
projectOpenKey :: WorkspaceService -> ProjectKey -> IO ()
projectOpenKey svc k = do
    ws <- readCell (wsCell svc)
    let present = any ((== k) . wsProjectKey) (WF.wsProjects (view wsSpec ws))
    unless present . overSpec svc $ \s -> s
        { WF.wsProjects = WF.wsProjects s
            <> [WsProject (pkType k) (pkRoot k) (pkFile k) M.empty Nothing] }
    -- First project in becomes active by default.
    when (not present && null (WF.wsProjects (view wsSpec ws))) $
        workspaceActivatePackage svc k Nothing Nothing
    wsReqEnum svc k

workspaceRemoveProject :: WorkspaceService -> ProjectKey -> IO ()
workspaceRemoveProject svc k = do
    overSpec svc $ \s -> s
        { WF.wsProjects = filter ((/= k) . wsProjectKey) (WF.wsProjects s) }
    modifyCell (wsCell svc) $ \ws ->
        over wsProjectsMap (M.delete k)
        . (if (atProject <$> view wsActive ws) == Just k
              then set wsActive Nothing else id)
        $ ws

-- | Set the active target (project, optional package manifest, optional
-- component) — mirrored into the spec so it survives restarts.
workspaceActivatePackage
    :: WorkspaceService -> ProjectKey -> Maybe FilePath -> Maybe Component -> IO ()
workspaceActivatePackage svc k mbManifest mbComp = do
    modifyCell (wsCell svc) $
        set wsActive (Just (ActiveTarget k mbManifest mbComp))
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
