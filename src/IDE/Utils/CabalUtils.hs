-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.CabalUtils
-- Copyright   :  2007-2016 Juergen Nicklisch-Franken, Hamish Mackenzie, Jacco Krijnen, JP Moresmau
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.CabalUtils (
    findProjectRoot
) where

import System.FilePath (takeDirectory, isDrive, (</>))
import System.Directory (doesFileExist, getHomeDirectory)

findProjectRoot :: FilePath -> IO FilePath
findProjectRoot curdir = do

    -- Copied from cabal-install as it is not exposed
    -- and until features like `cabal run` exist for `cabal new-build`
    homedir <- getHomeDirectory

    -- Search upwards. If we get to the users home dir or the filesystem root,
    -- then use the current dir
    let probe dir | isDrive dir || dir == homedir
                  = return curdir -- implicit project root
        probe dir = do
          exists <- doesFileExist (dir </> "cabal.project")
          if exists
            then return dir       -- explicit project root
            else probe (takeDirectory dir)

    probe curdir
   --TODO: [nice to have] add compat support for old style sandboxes


