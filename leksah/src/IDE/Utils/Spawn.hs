{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
-- | "System.Process", but on macOS a bare command name (@ps@, @tmux@, @ssh@…)
-- is looked up on @PATH@ once, then spawned by its absolute path.  Import
-- this instead of "System.Process" anywhere leksah starts a process.
--
-- Why: the process library spawns with @posix_spawnp@, and macOS's
-- @posix_spawnp@ tries each @PATH@ entry in turn with a full @posix_spawn@.
-- Every miss creates a short-lived child process (a copy of leksah that fails
-- to load the image and dies within ~20µs), and syspolicyd security-scans
-- every one of those.  Leksah's @PATH@ is often 100+ entries (a nix develop
-- shell, nested), so one @ps@ poll cost ~70 such processes and leksah's
-- ordinary polling drove ~200 processes/s and syspolicyd to several cores.
-- With an absolute path it's one @posix_spawn@.
--
-- Left as they were (so behaviour can't change):
--
--  * names containing a @/@, and 'ShellCommand's (those exec @/bin/sh@);
--  * a 'CreateProcess' with its own 'env' — @posix_spawnp@ searches the
--    /parent's/ @PATH@ but the process library's fork fallback the child's,
--    so we don't second-guess which one the caller relied on;
--  * names not found, or found only via a relative @PATH@ entry (which would
--    resolve against the child's 'cwd') — those go to @posix_spawnp@ as-is;
--  * non-macOS: glibc's @posix_spawnp@ searches @PATH@ in the child, and
--    Windows' @CreateProcess@ doesn't fork at all.
--
-- Hits are cached per (@PATH@, name), and re-checked with one @stat@ before
-- use, so a removed binary is looked up again; misses are not cached, so a
-- tool installed later is picked up.
module IDE.Utils.Spawn
  ( module System.Process
  , createProcess
  , createProcess_
  , withCreateProcess
  , readCreateProcess
  , readCreateProcessWithExitCode
  , readProcess
  , readProcessWithExitCode
  , callProcess
  , spawnProcess
  , resolveCommand
  , resolveCreateProcess
  ) where

import System.Process hiding
  ( createProcess, createProcess_, withCreateProcess
  , readCreateProcess, readCreateProcessWithExitCode
  , readProcess, readProcessWithExitCode
  , callProcess, spawnProcess
  , rawSystem, runProcess, runInteractiveProcess )
import qualified System.Process as P
import System.Exit (ExitCode)
import System.IO (Handle)
#if defined(darwin_HOST_OS)
import Control.Exception (SomeException, try)
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist, executable, getPermissions)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, splitSearchPath, (</>))
import System.IO.Unsafe (unsafePerformIO)
#endif

-- | The absolute path @posix_spawnp@ would run for this command name, when
-- that can be decided without spawning (see the module header); otherwise
-- the name unchanged.
resolveCommand :: FilePath -> IO FilePath
#if defined(darwin_HOST_OS)
resolveCommand exe
  | null exe || '/' `elem` exe = return exe
  | otherwise = lookupEnv "PATH" >>= \case
      Nothing   -> return exe
      Just path -> do
        let key = (path, exe)
        cached <- Map.lookup key <$> readIORef resolveCache
        stillThere <- maybe (return False) existsQuietly cached
        case cached of
          Just full | stillThere -> return full
          _ -> searchPath (splitSearchPath path) >>= \case
            Nothing   -> return exe
            Just full -> do
              atomicModifyIORef' resolveCache (\m -> (Map.insert key full m, ()))
              return full
  where
    -- First entry holding an executable file, as posix_spawnp would pick;
    -- give up (Nothing) on reaching a relative entry, whose meaning depends
    -- on the child's cwd.
    searchPath [] = return Nothing
    searchPath (d:ds)
      | null d || not (isAbsolute d) = return Nothing
      | otherwise = do
          let full = d </> exe
          ok <- isExecutableFile full
          if ok then return (Just full) else searchPath ds
    isExecutableFile f = do
      r <- try (do e <- doesFileExist f
                   if e then executable <$> getPermissions f else return False)
      return (either (const False :: SomeException -> Bool) id r)
    existsQuietly f =
      either (const False :: SomeException -> Bool) id <$> try (doesFileExist f)

resolveCache :: IORef (Map.Map (String, String) FilePath)
resolveCache = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE resolveCache #-}
#else
resolveCommand = return
#endif

-- | 'resolveCommand' applied to a 'RawCommand' without its own 'env'.
resolveCreateProcess :: CreateProcess -> IO CreateProcess
resolveCreateProcess cp = case (cmdspec cp, env cp) of
  (RawCommand exe args, Nothing) -> do
    exe' <- resolveCommand exe
    return cp { cmdspec = RawCommand exe' args }
  _ -> return cp

createProcess :: CreateProcess
              -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = resolveCreateProcess cp >>= P.createProcess

createProcess_ :: String -> CreateProcess
               -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ fun cp = resolveCreateProcess cp >>= P.createProcess_ fun

withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess cp k = resolveCreateProcess cp >>= \cp' -> P.withCreateProcess cp' k

readCreateProcess :: CreateProcess -> String -> IO String
readCreateProcess cp input = resolveCreateProcess cp >>= \cp' -> P.readCreateProcess cp' input

readCreateProcessWithExitCode :: CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithExitCode cp input =
  resolveCreateProcess cp >>= \cp' -> P.readCreateProcessWithExitCode cp' input

readProcess :: FilePath -> [String] -> String -> IO String
readProcess exe args input = resolveCommand exe >>= \e -> P.readProcess e args input

readProcessWithExitCode :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode exe args input =
  resolveCommand exe >>= \e -> P.readProcessWithExitCode e args input

callProcess :: FilePath -> [String] -> IO ()
callProcess exe args = resolveCommand exe >>= \e -> P.callProcess e args

spawnProcess :: FilePath -> [String] -> IO ProcessHandle
spawnProcess exe args = resolveCommand exe >>= \e -> P.spawnProcess e args
