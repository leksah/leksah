{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.DirectoryUtils
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.DirectoryUtils (
    setModificationTimeOnOSX
) where

import Data.Time.Clock (UTCTime(..))
#ifdef darwin_HOST_OS
import Data.Tuple (swap)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.IO.Error (modifyIOError, ioeSetLocation, ioeSetFileName)
import System.FilePath (normalise)
import System.Posix.Files as Posix (getFileStatus)
# if MIN_VERSION_unix(2, 7, 0)
import System.Posix.Files as Posix (setFileTimesHiRes, accessTimeHiRes, modificationTimeHiRes)
#endif
#endif

-- HFS+ has a 1s resolution so we may need to fudge the mtime
-- This code is copied from the directory package to
-- avoid depending on the latest version.  Once we no longer
-- support ghc 7.10 we should remove it and use
-- setModificationTime from directory instead.
#ifdef darwin_HOST_OS
setModificationTimeOnOSX :: FilePath -> UTCTime -> IO ()
setModificationTimeOnOSX path =
  modifyIOError (`ioeSetLocation` "setModificationTime") .
  setFileTime True path

setFileTime :: Bool -> FilePath -> UTCTime -> IO ()
setFileTime isMtime path = modifyIOError (`ioeSetFileName` path) .
                           setTime . utcTimeToPOSIXSeconds
  where
    path'  = normalise path             -- handle empty paths
    setTime time = do
      stat <- Posix.getFileStatus path'
      uncurry (setFileTimes path') $
        swapIf isMtime (convertTime time, otherTime stat)
# if MIN_VERSION_unix(2, 7, 0)
    setFileTimes = Posix.setFileTimesHiRes
    convertTime  = id
    otherTime    = if isMtime
                   then Posix.accessTimeHiRes
                   else Posix.modificationTimeHiRes
#  else
    setFileTimes = Posix.setFileTimes
    convertTime  = fromInteger . truncate
    otherTime    = if isMtime
                   then Posix.accessTime
                   else Posix.modificationTime
# endif

swapIf :: Bool -> (a, a) -> (a, a)
swapIf True  = swap
swapIf False = id

#else

setModificationTimeOnOSX :: FilePath -> UTCTime -> IO ()
setModificationTimeOnOSX _path _time = return ()

#endif
