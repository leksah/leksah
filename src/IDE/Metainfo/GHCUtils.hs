-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.GHCUtils
-- Copyright   :  2007-2009 Jürgen Nicklisch-Franken
-- License     :  GPL
--
-- Maintainer  :  Jutaro <jutaro@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------module IDE.Metainfo.GHCUtils (

    inGhc
,   inGhcIO
,   getInstalledPackageInfos
,   findFittingPackages

) where

import GHC (setSessionDynFlags,getSessionDynFlags,runGhc,Ghc(..))
import IDE.Core.State (IDEM(..))
import IDE.FileUtils (getSysLibDir)
import Control.Monad.Trans (liftIO)
import PackageConfig (PackageConfig(..))
import DynFlags (pkgDatabase,DynFlag(..),flags)
import UniqFM (eltsUFM)
import Distribution.Simple (withinRange,PackageIdentifier(..),Dependency(..))
import qualified Distribution.InstalledPackageInfo as IPI  (package)
import Data.List (maximumBy)
import Distribution.Package (pkgVersion)

inGhc :: Ghc a -> IDEM a
inGhc = liftIO . inGhcIO

inGhcIO :: Ghc a -> IO a
inGhcIO f = do
    libDir          <-   getSysLibDir
    runGhc (Just libDir) $ do
        getSessionDynFlags >>= setSessionDynFlags
        f

getInstalledPackageInfos :: Ghc [PackageConfig]
getInstalledPackageInfos = do
    dflags1         <-  getSessionDynFlags
    setSessionDynFlags dflags1{flags = Opt_ReadUserPackageConf : (flags dflags1)}
    pkgInfos        <-  case pkgDatabase dflags1 of
                            Nothing -> return []
                            Just fm -> return (eltsUFM fm)
    return pkgInfos

findFittingPackages :: [Dependency] -> Ghc [PackageIdentifier]
findFittingPackages dependencyList = do
    knownPackages   <-  getInstalledPackageInfos
    let packages    =   map IPI.package knownPackages
    return (concatMap (fittingKnown packages) dependencyList)
    where
    fittingKnown packages (Dependency dname versionRange) =
        let filtered =  filter (\ (PackageIdentifier name version) ->
                                    name == dname && withinRange version versionRange)
                        packages
        in  if length filtered > 1
                then [maximumBy (\a b -> compare (pkgVersion a) (pkgVersion b)) filtered]
                else filtered

