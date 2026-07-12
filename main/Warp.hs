module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.Environment (getArgs)
import IDE.Web.Main (develMain, splitFrontendMain)

-- `develMain` uses jsaddle-warp's `debugWrapper`, which forks the Warp server
-- (and a reload restarter) into background threads and returns — it's designed
-- to be run from GHCi with `:fork`, where the session keeps the process alive.
-- As a compiled executable we must block the main thread ourselves, otherwise
-- the process exits immediately and the forked server is torn down.
--
-- `--split-frontend` instead serves the ghcjs demo page in split mode and
-- runs only the BACKEND half of the frontend↔backend bridge in this process
-- (UI-split stage 1 proof — see IDE.Web.Bridge); its `runSettings` blocks.
main :: IO ()
main = do
  args <- getArgs
  if "--split-frontend" `elem` args
    then splitFrontendMain
    else do
      develMain
      forever $ threadDelay maxBound
