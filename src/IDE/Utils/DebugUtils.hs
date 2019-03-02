module IDE.Utils.DebugUtils (
    traceTimeTaken
) where

import Prelude ()
import Prelude.Compat
import Control.Monad (void, forever, when)
import Control.Monad.IO.Class (MonadIO(..))
import Criterion.Measurement (secs, getTime)
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

traceTimeTaken :: (MonadIO m, HasCallStack) => String -> m a -> m a
traceTimeTaken s f = do
    start    <- liftIO getTime
    r        <- f
    duration <- subtract start <$> liftIO getTime
    when (duration > 0.001) . liftIO $ do
      let cs = getCallStack callStack
      fastDebugM $ "SLOW: " <> s <> "  " <> secs duration <> " called from " <> show cs
    return r



