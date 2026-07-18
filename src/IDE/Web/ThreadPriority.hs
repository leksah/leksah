{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Best-effort OS-thread priority bumps so leksah's interactive threads keep
-- getting CPU when background compilations (separate @ghc@/@cabal@ processes)
-- saturate the machine.  We raise the threads that drive the UI, the reflex
-- frame processing and the tmux control-mode I/O — the compilers are left
-- alone; the interactive threads simply out-schedule them.
--
-- * __macOS__: sets the calling pthread's Quality-of-Service class via
--   @pthread_set_qos_class_self_np@ (libSystem).  QoS is honoured by the
--   scheduler system-wide, so a @USER_INTERACTIVE@ leksah thread wins CPU over
--   the default/utility threads of separate build processes.  Works
--   unprivileged — this is the effective path.
--
-- * __Linux__: sets the calling thread's nice value with @setpriority(2)@ on
--   the kernel tid (@gettid@).  NOTE: an unprivileged process may only /raise/
--   nice (lower priority); /lowering/ nice needs @RLIMIT_NICE@ / @CAP_SYS_NICE@.
--   So this is best-effort: it silently no-ops when the kernel refuses (it only
--   bites if leksah's @RLIMIT_NICE@ is raised, e.g. via @limits.conf@).
--
-- * __Other platforms__ (Windows, GHC JS backend): no-op.
--
-- Priority lives on the OS thread, and GHC green threads migrate across the
-- @-threaded@ capability pool, so 'raiseCurrentThreadPriority' only sticks on a
-- thread that owns its OS thread: the process main thread, a reflex network's
-- (single) frame thread, or a bound thread.  Use 'forkPriorityThread' — a
-- 'forkOS' that sets the class before running — for worker threads.
module IDE.Web.ThreadPriority
  ( ThreadPriority(..)
  , raiseCurrentThreadPriority
  , forkPriorityThread
  ) where

import Control.Concurrent (ThreadId, forkIO, forkOS, rtsSupportsBoundThreads)

#if defined(darwin_HOST_OS)
import Control.Monad (void)
import Foreign.C.Types (CInt(..))
#elif defined(linux_HOST_OS)
import Control.Monad (void)
import Foreign.C.Types (CInt(..), CUInt(..))
#endif

-- | Coarse priority tiers, mapped to a platform mechanism per constructor.
data ThreadPriority
  = Interactive  -- ^ UI-critical: the main/Cocoa thread and reflex frame threads.
  | High         -- ^ Latency-sensitive I/O: the tmux control-mode reader/drain.
  | Normal       -- ^ Baseline (the default the RTS would give any thread).
  deriving (Eq, Show)

#if defined(darwin_HOST_OS)
-- qos_class_t values from <pthread/qos.h>; relative priority 0.
foreign import ccall unsafe "pthread_set_qos_class_self_np"
  c_pthread_set_qos_class_self_np :: CInt -> CInt -> IO CInt

qosClass :: ThreadPriority -> CInt
qosClass Interactive = 0x21  -- QOS_CLASS_USER_INTERACTIVE
qosClass High        = 0x19  -- QOS_CLASS_USER_INITIATED
qosClass Normal      = 0x15  -- QOS_CLASS_DEFAULT

-- | Raise the calling OS thread's priority (see the module header).
raiseCurrentThreadPriority :: ThreadPriority -> IO ()
raiseCurrentThreadPriority p = void $ c_pthread_set_qos_class_self_np (qosClass p) 0
#elif defined(linux_HOST_OS)
foreign import ccall unsafe "gettid"      c_gettid      :: IO CInt
-- setpriority(int which, id_t who, int prio); PRIO_PROCESS = 0 applies to a
-- single kernel task (thread) when `who` is a tid.  Return code is ignored:
-- EACCES/EPERM (unprivileged nice-lowering refused) is a best-effort no-op.
foreign import ccall unsafe "setpriority" c_setpriority :: CInt -> CUInt -> CInt -> IO CInt

niceOf :: ThreadPriority -> CInt
niceOf Interactive = -10
niceOf High        = -5
niceOf Normal      = 0

-- | Raise the calling OS thread's priority (see the module header).
raiseCurrentThreadPriority :: ThreadPriority -> IO ()
raiseCurrentThreadPriority p = do
    tid <- c_gettid
    void $ c_setpriority 0 (fromIntegral tid) (niceOf p)
#else
-- | No-op on platforms without a per-thread priority mechanism we use.
raiseCurrentThreadPriority :: ThreadPriority -> IO ()
raiseCurrentThreadPriority _ = pure ()
#endif

-- | Fork a worker thread that raises its own priority before running.  Uses a
-- bound thread ('forkOS') so the class sticks to a dedicated OS thread that the
-- Haskell thread never migrates off; falls back to 'forkIO' where the RTS has
-- no bound-thread support (non-threaded RTS, GHC JS backend) — there the
-- priority set is best-effort but nothing crashes.  The returned 'ThreadId'
-- works with 'Control.Concurrent.killThread' either way.
forkPriorityThread :: ThreadPriority -> IO () -> IO ThreadId
forkPriorityThread p act
  | rtsSupportsBoundThreads = forkOS run
  | otherwise               = forkIO run
  where run = raiseCurrentThreadPriority p >> act
