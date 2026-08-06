-- SPDX-License-Identifier: Apache-2.0

-- | Where leksah's things live on disk.
module IDE.Paths
  ( getDataDir
  , sidecarPath
  , isSubPath
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory
       (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (lookupEnv)
import System.FilePath
       ((</>), addTrailingPathSeparator, normalise)
import Data.List (isPrefixOf)

import qualified Paths_leksah

-- | The data directory (@cm6/@, @xterm/@, @fonts/@, @pics/@…).  The
-- launcher exports @leksah_datadir@ pointing at the source checkout so a
-- directly-executed binary finds assets without cabal's install layout;
-- otherwise fall back to the package's own idea.
getDataDir :: MonadIO m => m FilePath
getDataDir = liftIO $
    lookupEnv "leksah_datadir" >>= maybe Paths_leksah.getDataDir return

-- | A per-user sidecar file (@~\/.leksah-0.17\/\<name\>@), directory
-- ensured.  Session state, agent registry, queue files and friends live
-- here — state, not configuration, hence not under XDG config with
-- @settings.json@.
sidecarPath :: MonadIO m => FilePath -> m FilePath
sidecarPath name = liftIO $ do
    home <- getHomeDirectory
    let dir = home </> ".leksah-0.17"
    createDirectoryIfMissing True dir
    return (dir </> name)

-- | Is @child@ inside (or equal to) @parent@?  Both sides get a trailing
-- separator before the prefix test, so @\/a\/bc@ is not inside @\/a\/b@
-- and a path is inside itself.
isSubPath :: FilePath -> FilePath -> Bool
isSubPath parent child =
    addTrailingPathSeparator (normalise parent)
        `isPrefixOf` addTrailingPathSeparator (normalise child)
