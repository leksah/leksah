{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.CabalProject
-- Copyright   :  2016-2016 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.CabalProject (
    findProjectRoot
  , projectPackages
  , getCabalProjectPackages
) where

import Prelude hiding (readFile)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath (takeDirectory, isDrive, (</>))
import Control.Exception (catch, SomeException(..))
import System.IO.Strict (readFile)

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

-- |
-- >>> :{
--  projectPackages $ unlines
--    [ "something befor"
--    , "--comment"
--    , "packages:"
--    , "--comment2"
--    , " package1"
--    , ""
--    , " "
--    , " --comment3"
--    , " package2 --comment4"
--    , "notpackage"
--    , " also notpackage" ]
-- :}
-- ["package1","package2"]
projectPackages :: String -> [FilePath]
projectPackages = map (dropWhile (==' ')) .
                  takeWhile (not . isNewSection) .
                  drop 1 .
                  dropWhile (/="packages:") .
                  filter (not . null) .
                  map (trimTrailing . dropComment) .
                  lines
  where
    dropComment ('-':'-':_) = ""
    dropComment (x:xs) = x:dropComment xs
    dropComment "" = ""

    trimTrailing = reverse . dropWhile (==' ') . reverse

    isNewSection "" = False
    isNewSection (' ':_) = False
    isNewSection _ = True

getCabalProjectPackages :: FilePath -> IO [FilePath]
getCabalProjectPackages dir = do
    projectRoot <- findProjectRoot dir
    let projectFile = projectRoot </> "cabal.project"
    doesFileExist projectFile >>= \case
        False -> return []
        True  -> (map (projectRoot </>) . projectPackages <$> readFile projectFile)
                    `catch` (\(_ :: SomeException) -> return [])

