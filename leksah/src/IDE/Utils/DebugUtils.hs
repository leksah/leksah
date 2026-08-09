{-# LANGUAGE CPP #-}
module IDE.Utils.DebugUtils (
    traceTimeTaken
) where

import Prelude ()
import Prelude.Compat
import Control.Monad (void, forever, when)
import Control.Monad.IO.Class (MonadIO(..))
#if defined(ghcjs_HOST_OS)
-- criterion-measurement's cycle-counter cbits don't build for the JS
-- backend; wall-clock time and a simple seconds format are plenty here.
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (printf)
#else
import Criterion.Measurement (secs, getTime)
#endif
import System.Log.Logger (debugM)
import GHC.Stack (callStack, getCallStack, HasCallStack)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (writeChan, readChan, forkIO, newChan)

fastDebugM :: String -> IO ()
fastDebugM = unsafePerformIO $ do
  c <- newChan
  void . forkIO . forever $ debugM "leksah" =<< readChan c
  return $ writeChan c
{-# NOINLINE fastDebugM #-}

#if defined(ghcjs_HOST_OS)
getTime :: IO Double
getTime = realToFrac <$> getPOSIXTime

secs :: Double -> String
secs = printf "%.3f s"
#endif

traceTimeTaken :: (MonadIO m, HasCallStack) => String -> m a -> m a
traceTimeTaken s f = do
    start    <- liftIO getTime
    r        <- f
    duration <- subtract start <$> liftIO getTime
    when (duration > 0.001) . liftIO $ do
      let cs = getCallStack callStack
      fastDebugM $ "SLOW: " <> s <> "  " <> secs duration <> " called from " <> show cs
    return r



