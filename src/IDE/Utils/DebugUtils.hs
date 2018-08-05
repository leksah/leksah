module IDE.Utils.DebugUtils (
    traceTimeTaken
) where

import Prelude ()
import Prelude.Compat
import Control.Monad.IO.Class (MonadIO(..))
import Criterion.Measurement (secs, getTime)
import System.Log.Logger (debugM)

traceTimeTaken :: MonadIO m => String -> m a -> m a
traceTimeTaken s f = do
    start    <- liftIO getTime
    r        <- f
    duration <- subtract start <$> liftIO getTime
    liftIO $ debugM "leksah" $ s <> " " <> secs duration
    return r



