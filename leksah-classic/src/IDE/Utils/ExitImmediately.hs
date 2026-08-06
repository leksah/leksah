{-# LANGUAGE CPP #-}
-- | A portable 'exitImmediately': the develop-mode relaunch and
-- @leksah-cmd restart@ must exit the whole process at once from a forked
-- thread ('System.Exit.exitWith' only throws in the calling thread).  On
-- POSIX this is @unix@'s 'System.Posix.Process.exitImmediately'; on Windows
-- the C runtime's @exit@.
module IDE.Utils.ExitImmediately
  ( exitImmediately
  ) where

#ifdef mingw32_HOST_OS
import Foreign.C.Types (CInt(..))
import System.Exit (ExitCode(..))

foreign import ccall unsafe "exit" c_exit :: CInt -> IO ()

exitImmediately :: ExitCode -> IO ()
exitImmediately ExitSuccess     = c_exit 0
exitImmediately (ExitFailure n) = c_exit (fromIntegral n)
#else
import System.Posix.Process (exitImmediately)
#endif
