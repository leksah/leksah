-- SPDX-License-Identifier: Apache-2.0

-- | The restart seam: the build system announces \"the leksah binary was
-- rebuilt, restart now\" and the front end decides what that means (exit 2
-- for the leksah.sh loop, tear down to the prompt in ghci mode, or hand off
-- to a successor instance).  A process-global handler cell in the style of
-- the other request bridges ('IDE.Web.CloseRequest', …): the front end
-- installs its policy at boot; 'requestRestart' is a no-op until then.
module IDE.Web.RestartRequest
  ( setRestartHandler
  , requestRestart
  ) where

import Control.Monad (join)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE restartHandler #-}
restartHandler :: IORef (IO ())
restartHandler = unsafePerformIO $ newIORef (return ())

-- | Install the front end's restart policy (called once at boot, and only
-- when running with @--develop-leksah@ — otherwise a rebuild never restarts).
setRestartHandler :: IO () -> IO ()
setRestartHandler = writeIORef restartHandler

-- | Fired by the in-IDE build system after a successful self-rebuild.
requestRestart :: IO ()
requestRestart = join (readIORef restartHandler)
