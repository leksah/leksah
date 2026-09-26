-- SPDX-License-Identifier: Apache-2.0

-- | Where leksah's things live on disk.
module IDE.Paths
  ( getDataDir
  , sidecarPath
  , isSubPath
  ) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath
       ((</>), addTrailingPathSeparator, normalise, takeDirectory)
import Data.List (isPrefixOf)

import qualified Paths_leksah

-- | The data directory (@cm6/@, @xterm/@, @fonts/@, @pics/@…).  The
-- launcher exports @leksah_datadir@ pointing at the source checkout so a
-- directly-executed binary finds assets without cabal's install layout;
-- otherwise look beside the executable, and only then fall back to the
-- package's own idea.
getDataDir :: MonadIO m => m FilePath
getDataDir = liftIO $
    lookupEnv "leksah_datadir" >>= maybe packagedDataDir return

-- | The datadir of an *installed* Leksah, found relative to the running
-- executable.
--
-- This is not a nicety: @Paths_leksah@ cannot serve a packaged build.  The web
-- assets — @cm6\/@, @xterm\/@, @fonts\/@, @pics\/@ — are deliberately not cabal
-- @data-files@ (see leksah.cabal), so the directory @Paths_leksah@ reports
-- contains only the licence, the icons and the desktop entry, and that is true
-- even on the machine that built it.  Every installer therefore stages the
-- assets next to the executable, and this is what finds them.
--
-- The .app bundle and the Windows installer used to rely on
-- @IDE.Core.State.leksahSubDir@ for exactly this; that module was deleted in
-- the round-2 relicensing purge and nothing replaced it, which left both
-- installers shipping a datadir that nothing read.
packagedDataDir :: IO FilePath
packagedDataDir = do
    exeDir <- takeDirectory <$> getExecutablePath
    let candidates =
          [ exeDir </> ".." </> "Resources" </> "leksah"  -- macOS .app bundle
          , exeDir </> ".." </> "leksah"                  -- Windows installer
          , exeDir </> ".." </> "share" </> "leksah"      -- tarball / unix prefix
          ]
    found <- filterM looksLikeDataDir candidates
    case found of
        (d:_) -> return (normalise d)
        []    -> Paths_leksah.getDataDir
  where
    -- Probe for an asset only a staged datadir has.  A bare existence check
    -- would let Paths_leksah's own directory — which does exist, and holds
    -- only the cabal data-files — masquerade as the real one.
    looksLikeDataDir d = doesDirectoryExist (d </> "cm6")

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
