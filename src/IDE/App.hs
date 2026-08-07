-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}

-- | The wiring record: one handle per service, created once at boot and
-- passed to whoever needs it.  This is not a state monolith — every field
-- is an independent service owning its own cells; 'App' itself is
-- immutable and carries no data of its own.  There is deliberately no
-- reader monad over it: functions name the services they use, and only
-- boot-level code takes the whole record.
module IDE.App
  ( App(..)
  , RunState(..)
  , newApp
  , appNote
  , appJSM
  , appJSMResults
  , registerJsContext
  , unregisterJsContext
    -- * Actions
  , AppAction
  , setGlobalApp
  , getGlobalApp
  , withApp
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.IORef
       (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Language.Javascript.JSaddle (JSContextRef, JSM, runJSM)
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)

import IDE.BuildLog (BuildLog, blNote, newBuildLog)
import IDE.Builder (Builder, newBuilder)
import IDE.Config (ConfigService, newConfigService)
import IDE.Problems (Problems, newProblems)
import IDE.Reactive (Cell, modifyCell, newCell, readCell)
import IDE.Watch (WatchService, newWatchService)
import IDE.Web.Model (WebUi, WindowId, newWebUi)
import IDE.Workspace (WorkspaceService, newWorkspaceService)
import IDE.Ws.Types (defaultEffects)

-- | Boot lifecycle, mostly for the status bar.
data RunState = IsStartingUp | IsRunning | IsShuttingDown
    deriving (Eq, Show)

data App = App
    { appConfig      :: ConfigService
    , appConfigError :: Maybe Text     -- ^ settings.json parse error at boot
    , appBuildLog    :: BuildLog
    , appProblems    :: Problems
    , appWorkspace   :: WorkspaceService
    , appBuilder     :: Builder
    , appWatch       :: WatchService
    , appUi          :: Cell WebUi
    , appRunState    :: Cell RunState
    , appExit        :: IORef ExitCode
    , appJsContexts  :: Cell [(WindowId, JSContextRef)]
    , appDevelopLeksah :: Bool
    }

-- | Assemble the services.  Pure wiring: nothing here reads a workspace
-- or opens a window yet.
newApp :: Bool -> IO App
newApp develop = do
    buildLog <- newBuildLog
    (config, cfgErr) <- newConfigService
    problems <- newProblems
    workspace <- newWorkspaceService defaultEffects (blNote buildLog)
    builder <- newBuilder buildLog problems config workspace
    watch <- newWatchService
    ui <- newCell newWebUi
    runState <- newCell IsStartingUp
    exitRef <- newIORef ExitSuccess
    ctxs <- newCell []
    return App
        { appConfig      = config
        , appConfigError = cfgErr
        , appBuildLog    = buildLog
        , appProblems    = problems
        , appWorkspace   = workspace
        , appBuilder     = builder
        , appWatch       = watch
        , appUi          = ui
        , appRunState    = runState
        , appExit        = exitRef
        , appJsContexts  = ctxs
        , appDevelopLeksah = develop
        }

-- | A user-visible one-liner in the build log.
appNote :: App -> Text -> IO ()
appNote = blNote . appBuildLog

-- | Run a JSM in every live window, asynchronously per window — the
-- @ideJSM@ successor.  Never blocks the caller and never runs one
-- window's work on another's thread (the no-cross-window-sync rule).
appJSM :: App -> JSM () -> IO ()
appJSM app act = do
    ctxs <- readCell (appJsContexts app)
    mapM_ (\(_, ctx) -> void . forkIO $ runJSM act ctx) ctxs

-- | Run a JSM in every live window and collect the results (the @js eval@
-- backend).  Sequential; call from a worker thread, never from a window's
-- own frame.
appJSMResults :: App -> JSM a -> IO [a]
appJSMResults app act = do
    ctxs <- readCell (appJsContexts app)
    mapM (\(_, ctx) -> runJSM act ctx) ctxs

registerJsContext :: App -> WindowId -> JSContextRef -> IO ()
registerJsContext app wid ctx =
    modifyCell (appJsContexts app) (((wid, ctx) :) . filter ((/= wid) . fst))

unregisterJsContext :: App -> WindowId -> IO ()
unregisterJsContext app wid =
    modifyCell (appJsContexts app) (filter ((/= wid) . fst))

-- | One IDE-shaped action: everything a menu item, socket verb or widget
-- event ultimately runs.
type AppAction = App -> IO ()

{-# NOINLINE globalApp #-}
globalApp :: IORef (Maybe App)
globalApp = unsafePerformIO (newIORef Nothing)

-- | Stored once at boot for the out-of-band entry points (native menus,
-- the control socket's process-global drains).
setGlobalApp :: App -> IO ()
setGlobalApp = writeIORef globalApp . Just

getGlobalApp :: IO (Maybe App)
getGlobalApp = readIORef globalApp

-- | Run an action against the booted app; silently a no-op before boot
-- (native callbacks can fire during startup).
withApp :: AppAction -> IO ()
withApp act = getGlobalApp >>= mapM_ act
