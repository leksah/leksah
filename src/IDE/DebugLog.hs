-- SPDX-License-Identifier: Apache-2.0

-- | Developer trace channels, off unless their environment variable is
-- set at launch (@LEKSAH_FOCUS_LOG@, @LEKSAH_META_LOG@).  They write
-- single stderr lines and exist so latency\/focus investigations have a
-- stable tap that is free when disabled.
module IDE.DebugLog
  ( focusLog
  , metaLog
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE focusLogOn #-}
focusLogOn :: Bool
focusLogOn = unsafePerformIO $ maybe False (/= "") <$> lookupEnv "LEKSAH_FOCUS_LOG"

{-# NOINLINE metaLogOn #-}
metaLogOn :: Bool
metaLogOn = unsafePerformIO $ maybe False (/= "") <$> lookupEnv "LEKSAH_META_LOG"

-- | Focus\/keyboard investigation channel.
focusLog :: MonadIO m => String -> m ()
focusLog t = liftIO . when focusLogOn $ hPutStrLn stderr ("FOCUS " <> t)

-- | Everything-else investigation channel.
metaLog :: MonadIO m => String -> m ()
metaLog t = liftIO . when metaLogOn $ hPutStrLn stderr ("META " <> t)
