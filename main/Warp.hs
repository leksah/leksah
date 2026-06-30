module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import IDE.Web.Main (develMain)

-- `develMain` uses jsaddle-warp's `debugWrapper`, which forks the Warp server
-- (and a reload restarter) into background threads and returns — it's designed
-- to be run from GHCi with `:fork`, where the session keeps the process alive.
-- As a compiled executable we must block the main thread ourselves, otherwise
-- the process exits immediately and the forked server is torn down.
main :: IO ()
main = do
  develMain
  forever $ threadDelay maxBound
