-- | A process-global queue of files chosen in the native "Open File" dialog.
--
-- File ▸ Open on leksah-wkwebview shows a native @NSOpenPanel@ (driven from the
-- menu glue); the dialog runs outside the reflex network, so the chosen path is
-- dropped here and 'IDE.Web.Main' drains the queue from a background thread,
-- turning it into a reflex 'Event' that opens the file in an editor — mirroring
-- how the GTK UI's @fileOpen@ opens the file the chooser returned.
module IDE.Web.OpenFileRequest
  ( deliverOpenedFile
  , nextOpenedFile
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE openedFileChan #-}
openedFileChan :: Chan FilePath
openedFileChan = unsafePerformIO newChan

-- | Record a file the user chose in the native open dialog.
deliverOpenedFile :: FilePath -> IO ()
deliverOpenedFile = writeChan openedFileChan

-- | Block until the next chosen file is available (drained by the reflex bridge).
nextOpenedFile :: IO FilePath
nextOpenedFile = readChan openedFileChan
