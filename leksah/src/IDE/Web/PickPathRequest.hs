-- | A process-global queue of paths chosen in the Add Project… dialog's native
-- browse panel.
--
-- Deliberately NOT 'IDE.Web.OpenFileRequest': that queue's consumer opens an
-- editor tab, and a path picked while adding a project must do nothing of the
-- sort.  Two queues and (on macOS) two native callbacks mean neither flow can
-- ever leak into the other.
--
-- Every result carries the token of the dialog that asked for it.  The panel is
-- fire-and-forget — the dialog that opened it may be gone by the time the user
-- dismisses the sheet, and with several OS windows the bridge routes results to
-- whichever window is frontmost — so a dialog keeps only its own token's results
-- and drops the rest.  That turns every one of those races into a no-op instead
-- of a path landing in a dialog that never asked for it.
module IDE.Web.PickPathRequest
  ( newPickToken
  , deliverPickedPath
  , nextPickedPath
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE pickedPathChan #-}
pickedPathChan :: Chan (Int, FilePath)
pickedPathChan = unsafePerformIO newChan

{-# NOINLINE nextTokenRef #-}
nextTokenRef :: IORef Int
nextTokenRef = unsafePerformIO (newIORef 1)

-- | Claim an id for one dialog's browse round trips.  Monotonic, so a token is
-- never reused by a later dialog.
newPickToken :: IO Int
newPickToken = atomicModifyIORef' nextTokenRef $ \n -> (n + 1, n)

-- | Record a path the user chose in the native browse panel, tagged with the
-- token the panel was opened with.
deliverPickedPath :: Int -> FilePath -> IO ()
deliverPickedPath tok path = writeChan pickedPathChan (tok, path)

-- | Block until the next chosen path is available (drained by the reflex bridge).
nextPickedPath :: IO (Int, FilePath)
nextPickedPath = readChan pickedPathChan
