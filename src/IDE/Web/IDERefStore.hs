-- | A process-global slot for the current 'IDERef'.
--
-- The web front ends create the 'IDERef' deep inside 'IDE.Web.Main.newIDE' and
-- don't otherwise expose it.  The native macOS menu (built before/around the
-- app's run loop) needs it to run commands when a menu item is chosen, so
-- 'newIDE' publishes it here and the menu reads it back.
module IDE.Web.IDERefStore
  ( setGlobalIDERef
  , getGlobalIDERef
  ) where

import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Core.State (IDERef)

{-# NOINLINE globalIDERef #-}
globalIDERef :: IORef (Maybe IDERef)
globalIDERef = unsafePerformIO (newIORef Nothing)

setGlobalIDERef :: IDERef -> IO ()
setGlobalIDERef = writeIORef globalIDERef . Just

getGlobalIDERef :: IO (Maybe IDERef)
getGlobalIDERef = readIORef globalIDERef
