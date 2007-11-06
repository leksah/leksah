-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Collector
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This modulle collects information for packages to make navigation and information
--  accessible to ghf
--
-------------------------------------------------------------------------------

module Ghf.Collector (
   collectInstalled
,   collectUninstalled
) where

import GHC
import Module
import TcRnMonad
import qualified Maybes as M
import ErrUtils
import HscTypes
import Finder
import LoadIface
import Outputable hiding(trace)
import qualified Pretty as P
import MkIface
import Config
import DynFlags hiding(Option)
import PrelNames

--import qualified Distribution.InstalledPackageInfo as IPI
import qualified PackageConfig as DP
import Distribution.Simple.Configure
import Distribution.PackageDescription
import Distribution.InstalledPackageInfo hiding (package)
import Distribution.Package
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Monad.Reader
import UniqFM
import BinIface
import Panic
--import Packages hiding (package,exposedModules)
import System.IO
import System.Process
import Data.Maybe


import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import System.FilePath
import System.Directory
import Data.Char
import Data.List(isSuffixOf,zip4)
import Debug.Trace
import Data.Binary

import Ghf.Core hiding(trace)
import Ghf.Extractor
import Ghf.File
import Ghf.Info


collectInstalled :: Session -> String -> Bool -> IO()
collectInstalled session version forceRebuild = do
    when forceRebuild $ do
            collectorPath   <-  getCollectorPath version
            removeDirectoryRecursive collectorPath
    collectorPath   <-  getCollectorPath version
    knownPackages   <-  findKnownPackages collectorPath
    putStrLn $ "found known packages" ++ " " ++ show knownPackages
    packageInfos    <-  getInstalledPackageInfos session
--    putStrLn $ "get installed package infos" ++ " " ++ concatMap (\pi -> show (pkgName (package pi)))
--                                                            packageInfos
    let newPackages =   filter (\pi -> not $Set.member (showPackageId $ fromDPid $ DP.package pi)
                                                        knownPackages)
                                    packageInfos
--    putStrLn $ "after new Package"
    let newPackages2 =  filter (\pi -> True {--pkgName (package pi) == "base"--}) newPackages
--    putStrLn $ "after new Package2, Remaining " ++ concatMap (\ pi -> show (pkgName (package pi)))
--                                                            newPackages2
    exportedIfaceInfos <-  mapM (\ info -> getIFaceInfos (fromDPid $ DP.package info)
                                            (DP.exposedModules info) session) newPackages2
    hiddenIfaceInfos   <-  mapM (\ info -> getIFaceInfos (fromDPid $DP.package info)
                                        (DP.hiddenModules info) session) newPackages2
--    putStrLn $ "getIfaceInfos completed hidden lengtgh: " ++ show (map length hiddenIfaceInfos)
    let extracted   =   map extractInfo (zip4 exportedIfaceInfos hiddenIfaceInfos
                                            (map (fromDPid . DP.package) newPackages2)
                                                (map (\p -> map fromDPid (DP.depends p)) newPackages2))
--    putStrLn $ "extracted " ++ concatMap (\ pi -> packageId pi) extracted
    mapM_ (writeExtracted collectorPath) extracted


collectUninstalled :: Session -> String -> FilePath -> IO ()
collectUninstalled session version cabalPath = do
    allHiFiles      <-  allHiFiles (dropFileName cabalPath)
--    putStrLn $ "\nallModules " ++ show allHiFiles
    pd              <-  readPackageDescription normal cabalPath
                            >>= return . flattenPackageDescription
    allIfaceInfos   <-  getIFaceInfos2 allHiFiles session
    deps            <-  findFittingPackages session (buildDepends pd)
    let extracted   =   extractInfo (allIfaceInfos,[], package pd, deps)
    collectorPath   <-  getCollectorPath version
    writeExtracted collectorPath extracted
    putStrLn $ "\nExtracted infos for " ++ cabalPath

getIFaceInfos :: PackageIdentifier -> [String] -> Session -> IO [(ModIface, FilePath)]
getIFaceInfos pckg modules session = do
    let isBase          =   pkgName pckg == "base"
    let ifaces          =   mapM (\ mn -> findAndReadIface empty
                                          (if isBase
                                                then mkBaseModule_ (mkModuleName mn)
                                                else mkModule (DP.mkPackageId (asDPid pckg))
                                                              (mkModuleName mn))
                                          False) modules
    hscEnv              <-  sessionHscEnv session
    let gblEnv          =   IfGblEnv { if_rec_types = Nothing }
    maybes              <-  initTcRnIf  'i' hscEnv gblEnv () ifaces
    let res             =   catMaybes (map handleErr maybes)
    return res
    where
        handleErr (M.Succeeded val)   =   Just val
        handleErr (M.Failed mess)     =   trace (P.render (mess defaultErrStyle)) Nothing

getIFaceInfos2 :: [String] -> Session -> IO [(ModIface, FilePath)]
getIFaceInfos2 filePaths session = do
#if __GHC__ > 670
    let ifaces          =   mapM readBinIface filePaths
    hscEnv              <-  sessionHscEnv session
    let gblEnv          =   IfGblEnv { if_rec_types = Nothing }
    res                 <-  initTcRnIf  'i' hscEnv gblEnv () ifaces
#else
    res                 <-   mapM readBinIface filePaths
#endif
    return (zip res filePaths)


findKnownPackages :: FilePath -> IO (Set String)
findKnownPackages filePath = do
    paths <- getDirectoryContents filePath
    let nameList = map dropExtension  $filter (\s -> ".pack" `isSuffixOf` s) paths
    return (Set.fromList nameList)

writeExtracted :: FilePath -> PackageDescr -> IO ()
writeExtracted dirPath pd = do
    let filePath = dirPath </> showPackageId (packagePD pd) ++ ".pack"
    encodeFile filePath pd
