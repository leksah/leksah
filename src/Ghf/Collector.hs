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
import PackageConfig as IPI
import Distribution.Simple.Configure
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity

import Control.Monad.Reader
import UniqFM
import BinIface
import Panic
--import Packages hiding (package,exposedModules)
import System.IO
import System.Process
import Data.Maybe
import System.Console.GetOpt
import System.Environment
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


data Flag =  UninstalledProject String | Rebuild
       deriving (Show,Eq)

options :: [OptDescr Flag]
options =   [Option ['r'] ["Rebuild"] (NoArg Rebuild)
                "Cleans all .pack files and rebuild everything"
         ,   Option ['u'] ["Uninstalled"] (ReqArg UninstalledProject "FILE")
                "Gather info about an uninstalled package"]

ghfOpts :: [String] -> IO ([Flag], [String])
ghfOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $userError $concat errs ++ usageInfo header options
    where header = "Usage: ghf [OPTION...] files..."

-- |Build the main window
main = defaultErrorHandler defaultDynFlags $do
    args            <-  getArgs
    (o,fl)          <-  ghfOpts args
    libDir          <-  getSysLibDir
--    putStrLn $"libdir '" ++ normalise libDir ++ "'"
#if __GHC__ > 670
    session     <-  newSession (Just libDir)
#else
    session     <-  newSession JustTypecheck (Just libDir)
#endif
    dflags0         <-  getSessionDynFlags session
    setSessionDynFlags session dflags0
    let version     =   cProjectVersion
    let uninstalled =   filter (\x -> case x of UninstalledProject _ -> True
                                                otherwise -> False) o
    if length uninstalled > 0
        then mapM_ (collectUninstalled session version)
                $ map (\ (UninstalledProject x) -> x) uninstalled
        else collectInstalled session version (elem Rebuild o)

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
    let newPackages =   filter (\pi -> not $Set.member (showPackageId $ IPI.package pi) knownPackages)
                                    packageInfos
--    putStrLn $ "after new Package"
    let newPackages2 =  filter (\pi -> True {--pkgName (package pi) == "base"--}) newPackages
--    putStrLn $ "after new Package2, Remaining " ++ concatMap (\ pi -> show (pkgName (package pi)))
--                                                            newPackages2
    exportedIfaceInfos <-  mapM (\ info -> getIFaceInfos (IPI.package info)  (IPI.exposedModules info)
                                        session) newPackages2
    hiddenIfaceInfos   <-  mapM (\ info -> getIFaceInfos (IPI.package info)  (IPI.hiddenModules info)
                                        session) newPackages2
--    putStrLn $ "getIfaceInfos completed hidden lengtgh: " ++ show (map length hiddenIfaceInfos)
    let extracted   =   map extractInfo (zip4 exportedIfaceInfos hiddenIfaceInfos
                                            (map IPI.package newPackages2) (map depends newPackages2))
--    putStrLn $ "extracted " ++ concatMap (\ pi -> packageId pi) extracted
    mapM_ (writeExtracted collectorPath) extracted


collectUninstalled :: Session -> String -> FilePath -> IO ()
collectUninstalled session version cabalPath = do
    allHiFiles      <-  allHiFiles (dropFileName cabalPath)
--    putStrLn $ "\nallModules " ++ show allHiFiles
    pd              <-  PD.readPackageDescription normal cabalPath >>= return . PD.flattenPackageDescription
    allIfaceInfos   <-  getIFaceInfos2 allHiFiles session
    deps            <-  findFittingPackages session (PD.buildDepends pd)
    let extracted   =   extractInfo (allIfaceInfos,[], fromDPid (PD.package pd), deps)
    collectorPath   <-  getCollectorPath version
    writeExtracted collectorPath extracted
    putStrLn $ "\nExtracted infos for " ++ cabalPath

getIFaceInfos :: PackageIdentifier -> [String] -> Session -> IO [(ModIface, FilePath)]
getIFaceInfos pckg modules session = do
    let isBase          =   pkgName pckg == "base"
    let ifaces          =   mapM (\ mn -> findAndReadIface empty
                                          (if isBase
                                                then mkBaseModule_ (mkModuleName mn)
                                                else mkModule (mkPackageId pckg)
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

{--
writeExtracted :: FilePath -> PackageDescr -> IO ()
writeExtracted dirPath pd = do
    let filePath = dirPath </> packagePD pd ++ ".packs"
    hdl <- openFile filePath WriteMode
    hPutStr hdl (show pd)
    hClose hdl
--}

writeExtracted :: FilePath -> PackageDescr -> IO ()
writeExtracted dirPath pd = do
    let filePath = dirPath </> packagePD pd ++ ".pack"
    encodeFile filePath pd




