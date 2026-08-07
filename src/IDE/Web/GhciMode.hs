{-# LANGUAGE LambdaCase #-}
-- | Support for running leksah inside a cabal repl (@leksah.sh --ghci@).
--
-- In ghci mode the process must never 'IDE.Utils.ExitImmediately.exitImmediately'
-- — that would kill the ghci session whose whole point is surviving the app so
-- @:reload@ + a fresh @:main@ replace the rebuild-and-relaunch loop.  Instead
-- the exit sites call 'stopForGhci': run the registered cleanups (close the
-- listening sockets and native windows a fresh @:main@ must be able to
-- recreate), then stop the front end's run loop so @main@ returns to the
-- prompt.  Every thread the run forked is then reaped ('killAppThreads'):
-- leaving them as "inert debris" retained the whole previous IDE state through
-- their captured 'IDERef' — ~100MB of live heap per reload, and eventually a
-- multi-GB session that the OS kills.
module IDE.Web.GhciMode
  ( ghciMode
  , registerGhciCleanup
  , registerGhciCleanupNamed
  , registerGhciQuiesceNamed
  , setGhciStop
  , stopForGhci
  , suspendForGhci
  , phaseLog
  , phaseSince
  , censusLog
  , recordPreRunThreads
  ) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, killThread, threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forM_, join, unless, void, when)
import Data.IORef (IORef, newIORef, atomicModifyIORef', writeIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as S (fromList, member)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Conc.Sync
       (ThreadStatus(..), listThreads, threadLabel, threadStatus)
import GHC.Stack.CloneStack (cloneThreadStack, decode, StackEntry(..))
import GHC.Stats (GCDetails(..), RTSStats(..), getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | True when running under @leksah.sh --ghci@ (@LEKSAH_GHCI@ set non-empty
-- in the environment).
{-# NOINLINE ghciMode #-}
ghciMode :: Bool
ghciMode = unsafePerformIO $ maybe False (not . null) <$> lookupEnv "LEKSAH_GHCI"

-- | Timestamped phase mark on the same stderr as 'IDE.DebugLog.metaLog'
-- (@LEK <time> [phase] …@), for attributing reload-cycle wall clock to its
-- stages: teardown steps here, boot steps at the 'boot:' sites.  Cheap and
-- unconditional — a reload cycle emits a couple of dozen of these, and they are
-- the only record of where a 20-reload session's time actually went.
phaseLog :: String -> IO ()
phaseLog msg = do
  t <- getCurrentTime
  hPutStrLn stderr ("LEK " <> (takeWhile (/= ' ') . drop 11 $ show t)
                    <> " [phase] " <> msg)

-- | 'phaseLog' with the elapsed time since a start stamp appended in ms.
phaseSince :: UTCTime -> String -> IO ()
phaseSince t0 msg = do
  t <- getCurrentTime
  let ms = round (realToFrac (diffUTCTime t t0) * 1000 :: Double) :: Int
  phaseLog (msg <> " (+" <> show ms <> "ms)")

-- | One line of "how much debris has this ghci session accumulated": live
-- threads (each :main leaves the previous run's unlabelled pollers behind — see
-- 'killAppThreads') plus GC live set, cumulative allocation and major-GC count.
-- Logged at every teardown, so a session's reload series can be read straight
-- off the log: if reloads get slower, this says whether it is debris.
censusLog :: String -> IO ()
censusLog tag = do
  n <- length <$> listThreads
  enabled <- getRTSStatsEnabled
  extra <- if not enabled then return "" else do
    s <- getRTSStats
    return $ " liveMB=" <> show (gcdetails_live_bytes (gc s) `div` (1024 * 1024))
             <> " allocGB=" <> show (allocated_bytes s `div` (1024 * 1024 * 1024))
             <> " majorGCs=" <> show (major_gcs s)
             <> " gcCPUs=" <> show (gc_cpu_ns s `div` 1000000000)
  phaseLog ("census " <> tag <> ": threads=" <> show n <> extra)

{-# NOINLINE cleanupsRef #-}
cleanupsRef :: IORef [(String, IO ())]
cleanupsRef = unsafePerformIO (newIORef [])

-- | Register the teardown of a resource a fresh @:main@ must be able to
-- recreate (the warp listener, the cmd-socket listener, native windows).
-- Each registration runs at most once, in 'stopForGhci'.
registerGhciCleanup :: IO () -> IO ()
registerGhciCleanup = registerGhciCleanupNamed "unnamed"

-- | 'registerGhciCleanup' with a label, so the teardown timing in the log says
-- WHICH cleanup took the time.
registerGhciCleanupNamed :: String -> IO () -> IO ()
registerGhciCleanupNamed n c = atomicModifyIORef' cleanupsRef (\cs -> ((n, c) : cs, ()))

-- | Actions that stop the OUTSIDE WORLD from calling into this run — run first
-- in 'stopForGhci', before any thread is reaped.  Ordering matters and cost us
-- a leak: while the webviews' jsaddle contexts were still live, the pages kept
-- delivering results, and jsaddle forks a thread per delivery.  Threads forked
-- between the reap and the invalidation survived it AND then counted as
-- "pre-existing" at the next boot, so they were exempt from every later reap —
-- a ratchet that grew the exempt set 12 → 5418 threads over five reloads.
{-# NOINLINE quiescesRef #-}
quiescesRef :: IORef [(String, IO ())]
quiescesRef = unsafePerformIO (newIORef [])

-- | Register a "stop external callbacks" action (see 'quiescesRef').
registerGhciQuiesceNamed :: String -> IO () -> IO ()
registerGhciQuiesceNamed n c = atomicModifyIORef' quiescesRef (\cs -> ((n, c) : cs, ()))

{-# NOINLINE stopRef #-}
stopRef :: IORef (IO ())
stopRef = unsafePerformIO (newIORef (return ()))

-- | How the front end's run loop is made to return control to the ghci
-- prompt (macOS: 'IDE.Web.MacGlue.stopApp').  Registered at startup by the
-- platform layer.
setGhciStop :: IO () -> IO ()
setGhciStop = writeIORef stopRef

-- | The threads that already existed when this run's @main@ started: ghci's own
-- evaluation thread (which IS @main@ here), the RTS's IOManager \/ TimerManager
-- \/ finalizer threads, and anything a previous run left behind that we could
-- not reap.  Everything NOT in this set was forked by this run, so
-- 'killAppThreads' may reap it.  Snapshotting is what makes reaping safe: no
-- label list to keep in sync, and ghci's own threads can never be caught.
{-# NOINLINE preRunThreads #-}
preRunThreads :: IORef (Maybe (Set ThreadId))
preRunThreads = unsafePerformIO (newIORef Nothing)

-- | Record the pre-existing threads.  Call once, as early in @main@ as
-- possible, before anything is forked (see 'IDE.Web.Main.newIDE').  An
-- interpreted-module CAF, so each @:reload@ + @:main@ takes its own snapshot.
--
-- Only LIVE threads go in the set.  A 'ThreadId' keeps its thread's TSO (and
-- stack) reachable, so snapshotting the finished ones too would both pin them
-- forever and — since they would then count as \"pre-existing\" next time —
-- permanently exempt them from reaping.  That is a leak in its own right: the
-- snapshot grew 380 → 2070 entries over 8 reloads before this filter.
recordPreRunThreads :: IO ()
recordPreRunThreads = when ghciMode $ do
  ts   <- listThreads
  live <- filterM (fmap not . threadIsDead) ts
  writeIORef preRunThreads (Just (S.fromList live))
  phaseLog $ "boot: pre-run thread snapshot: " <> show (length live)
             <> " live (of " <> show (length ts) <> " listed)"

-- | Has this thread finished or died?  (A dead thread needs no killing, and
-- 'killThread' on one is a wasted forkIO — thousands of them, per reload.)
threadIsDead :: ThreadId -> IO Bool
threadIsDead t =
  (try (threadStatus t) :: IO (Either SomeException ThreadStatus)) >>= \case
    Left _               -> return True
    Right ThreadFinished -> return True
    Right ThreadDied     -> return True
    Right _              -> return False

-- | Kill the threads this run forked, before a @:reload@ + fresh @:main@.
--
-- Correctness: a stale per-window frame thread (or a duplicated bridge drain
-- routing to a dead window) blocks the reloaded windows' initial build on the
-- single Cocoa main queue and they freeze.
--
-- Space: this is also THE fix for the reload space leak.  Every surviving
-- thread of the old run — the git\/status pollers, tool-output readers, the RTS
-- stats logger, the build trigger, terminal readers — captures that run's
-- 'IDERef', which retains its whole IDE state: workspace with every parsed
-- package, the metadata caches, every open editor's contents.  Leaving them as
-- "harmless inert debris" (as this did) cost ~100MB of live heap PER RELOAD
-- (measured: 719MB → 1723MB over 8 reloads, RSS to ~6GB, one session killed
-- outright), because inert is not the same as unreachable.
--
-- Each kill runs in its own thread: 'killThread' blocks until the exception is
-- delivered, and a thread inside a safe FFI call (or masked) would otherwise
-- stall teardown indefinitely.  The label-matching fallback keeps the old
-- behaviour if no snapshot was taken.
killAppThreads :: IO ()
killAppThreads = do
  me <- myThreadId
  ts <- listThreads
  readIORef preRunThreads >>= \case
    Just pre -> do
      -- Only live threads this run forked.  Skipping the already-dead ones is
      -- what keeps this cheap: most of the thousands 'listThreads' reports are
      -- finished TSOs awaiting collection, and forking a killer for each was
      -- both pointless and its own source of debris.
      doomed <- filterM (fmap not . threadIsDead)
                  [ t | t <- ts, t /= me, not (t `S.member` pre) ]
      forM_ doomed $ \t -> forkIO $
        void (try (killThread t) :: IO (Either SomeException ()))
      -- WAIT for the kills to land (bounded): the caller collects right after,
      -- and a thread that is still running is still holding this run's state
      -- reachable, so collecting before they die frees nothing.
      let settle :: Int -> IO ()
          settle 0 = return ()
          settle n = do
            alive <- filterM (fmap not . threadIsDead) doomed
            unless (null alive) $ threadDelay 100000 >> settle (n - 1)
      settle (50 :: Int)   -- up to ~5s
      stuck <- filterM (fmap not . threadIsDead) doomed
      phaseLog $ "teardown: reaped " <> show (length doomed - length stuck) <> "/"
                 <> show (length doomed) <> " thread(s) forked by this run"
                 <> (if null stuck then ""
                     else " (" <> show (length stuck) <> " would not die)")
      -- A survivor is not cosmetic: a live thread is a GC ROOT, so whatever its
      -- stack references — for an app thread, this run's IDERef and the whole
      -- object graph under it — stays live for the rest of the session.  One
      -- unkillable thread per reload was ~150MB of live heap per reload.  Name
      -- it (label, status, top of stack) so it can be tracked down rather than
      -- counted.
      forM_ stuck $ \t -> do
        lbl <- threadLabel t
        st  <- try (threadStatus t) :: IO (Either SomeException ThreadStatus)
        frames <- either (const []) (take 6 . map prettyFrame)
                    <$> (try (cloneThreadStack t >>= decode)
                           :: IO (Either SomeException [StackEntry]))
        phaseLog $ "teardown: SURVIVOR " <> show t
                   <> " label=" <> show lbl
                   <> " status=" <> either (const "?") show st
                   <> " stack=" <> show frames
    -- No snapshot (shouldn't happen): fall back to the labels we know.
    Nothing -> forM_ ts $ \t -> when (t /= me) $ do
      mlbl <- threadLabel t
      case mlbl of
        Just lbl | any (`isPrefixOf` lbl) appThreadPrefixes ->
          void (try (killThread t) :: IO (Either SomeException ()))
        _ -> return ()
  where appThreadPrefixes =
          ["reflex-frames-", "resync-notifier-", "bridge-drain-"]
        prettyFrame e = functionName e <> " (" <> moduleName e <> ")"

-- | Tear down for a @:reload@: kill the old run's network threads, run the
-- registered cleanups (emptying the list, so a re-registering fresh run starts
-- clean), then stop the run loop so @main@ returns to the ghci prompt.
-- Exceptions in individual cleanups are swallowed — teardown must always reach
-- the stop.
stopForGhci :: IO ()
stopForGhci = do
  t0 <- getCurrentTime
  phaseLog "teardown: start"
  censusLog "pre-teardown"
  -- FIRST cut off inbound callbacks (jsaddle contexts), so nothing new is forked
  -- while we reap; only then kill this run's threads.
  qs <- atomicModifyIORef' quiescesRef (\pending -> ([], pending))
  forM_ qs $ \(n, c) -> do
    tq <- getCurrentTime
    void (try c :: IO (Either SomeException ()))
    phaseSince tq ("teardown: quiesce " <> n)
  when ghciMode $ void (try killAppThreads :: IO (Either SomeException ()))
  phaseSince t0 "teardown: killAppThreads done"
  cs <- atomicModifyIORef' cleanupsRef (\pending -> ([], pending))
  forM_ cs $ \(n, c) -> do
    tc <- getCurrentTime
    void (try c :: IO (Either SomeException ()))
    phaseSince tc ("teardown: cleanup " <> n)
  phaseSince t0 "teardown: cleanups done, stopping run loop"
  -- The reaped threads' heap is only actually released by a major collection,
  -- and doing it here (rather than leaving it to the fresh run, which would
  -- then peak at both runs' live sets) is what keeps RSS flat across reloads.
  when ghciMode $ do
    performMajorGC
    censusLog "post-teardown-gc"
  suspendForGhci
  phaseSince t0 "teardown: run loop stopped"

-- | Stop only the run loop, with NO cleanups: @leksah-cmd hs eval@'s suspend.
-- Windows, sockets and all state stay; 'IDE.Web.MacGlue.resumeApp' evaluated
-- at the prompt takes the UI straight back to where it was.
suspendForGhci :: IO ()
suspendForGhci = join (readIORef stopRef)
