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

import Ghf.Core.State
import Ghf.Extractor
import Ghf.File
import Ghf.Info
import Ghf.SourceCollector



collectInstalled :: Bool -> Session -> String -> Bool -> IO()
collectInstalled writeAscii session version forceRebuild = do
    when forceRebuild $ do
            collectorPath   <-  getCollectorPath version
            removeDirectoryRecursive collectorPath
    collectorPath   <-  getCollectorPath version
    knownPackages   <-  findKnownPackages collectorPath
--    putStrLn $ "found known packages" ++ " " ++ show knownPackages
    packageInfos    <-  getInstalledPackageInfos session
    let newPackages =   filter (\pi -> not $Set.member (showPackageId $ fromDPid $ DP.package pi)
                                                        knownPackages)
                                    packageInfos
    exportedIfaceInfos <-  mapM (\ info -> getIFaceInfos (fromDPid $ DP.package info)
                                            (DP.exposedModules info) session) newPackages
    hiddenIfaceInfos   <-  mapM (\ info -> getIFaceInfos (fromDPid $DP.package info)
                                        (DP.hiddenModules info) session) newPackages
    let extracted   =   map extractInfo $ zip4 exportedIfaceInfos
                                                hiddenIfaceInfos
                                                (map (fromDPid . DP.package) newPackages)
                                                ((map (\p -> map fromDPid (DP.depends p)))
                                                   newPackages)
    sources         <-  getSourcesMap
    extractedWithSources    <-  mapM (collectSources sources) extracted
    mapM_ (writeExtracted collectorPath writeAscii) extractedWithSources


collectUninstalled :: Bool -> Session -> String -> FilePath -> IO ()
collectUninstalled writeAscii session version cabalPath = do
    allHiFiles      <-  allHiFiles (dropFileName cabalPath)
--    putStrLn $ "\nallModules " ++ show allHiFiles
    pd              <-  readPackageDescription normal cabalPath
                            >>= return . flattenPackageDescription
    allIfaceInfos   <-  getIFaceInfos2 allHiFiles session
    deps            <-  findFittingPackages session (buildDepends pd)
    let extracted   =   extractInfo (allIfaceInfos,[], package pd, deps)
    let sources     =   Map.fromList [(package pd,[cabalPath])]
    extractedWithSources    <-  collectSources sources extracted
    collectorPath   <-  getCollectorPath version
    writeExtracted collectorPath writeAscii extractedWithSources
    writeExtracted collectorPath True extractedWithSources
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

writeExtracted :: FilePath -> Bool -> PackageDescr -> IO ()
writeExtracted dirPath writeAscii pd = do
    let filePath = dirPath </> showPackageId (packagePD pd) ++ ".pack"
    if writeAscii
        then writeFile (filePath ++ "dpg") (show pd)
        else encodeFile filePath pd
