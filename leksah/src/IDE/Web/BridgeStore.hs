-- | A process-global slot for this process's ends of the frontend↔backend
-- bridge (see "IDE.Web.Bridge").
--
-- In dev/demo builds both halves live in one process, so 'newDirectBridge'
-- runs in @newIDE@ and BOTH ends land here.  In a split deployment each
-- process holds only its own end ('newJsBridge') and the other getter
-- returns 'Nothing'.  Same pattern as "IDE.Web.IDERefStore".
module IDE.Web.BridgeStore
  ( setFrontendBridge
  , setBackendBridge
  , getFrontendBridge
  , getBackendBridge
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.Bridge (Bridge)

{-# NOINLINE frontendEnd #-}
frontendEnd :: IORef (Maybe Bridge)
frontendEnd = unsafePerformIO (newIORef Nothing)

{-# NOINLINE backendEnd #-}
backendEnd :: IORef (Maybe Bridge)
backendEnd = unsafePerformIO (newIORef Nothing)

setFrontendBridge :: Bridge -> IO ()
setFrontendBridge = writeIORef frontendEnd . Just

setBackendBridge :: Bridge -> IO ()
setBackendBridge = writeIORef backendEnd . Just

-- | The frontend half's end (calls the backend's endpoints).
getFrontendBridge :: IO (Maybe Bridge)
getFrontendBridge = readIORef frontendEnd

-- | The backend half's end (calls the frontend's endpoints).
getBackendBridge :: IO (Maybe Bridge)
getBackendBridge = readIORef backendEnd
