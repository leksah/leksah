-----------------------------------------------------------------------------
--
-- Module      :  Ghf.InterfaceCollector
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This modulle extracts information from .hi files for installed packages
--
-------------------------------------------------------------------------------

module Ghf.InterfaceCollector (
    collectInstalled
,   collectUninstalled
) where


import GHC hiding(Id)
import Module
import TcRnMonad
import qualified Maybes as M
import ErrUtils
import HscTypes
import Finder
import LoadIface
import Outputable hiding(trace)
import qualified Pretty as P
import Config
import IfaceSyn
import OccName
import FastString
import Outputable hiding(trace)
import UniqFM
import qualified PackageConfig as DP
import Name
import Unique
import SrcLoc
import MkIface
import DynFlags hiding(Option)
import PrelNames
import UniqFM
import BinIface
import Panic

import Data.Char (isSpace)
import Distribution.Version
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable (maximumBy)
import Text.ParserCombinators.ReadP
import Control.Monad
import System.Directory
import qualified PackageConfig as DP
import Distribution.Simple.Configure
import Distribution.PackageDescription
import Distribution.InstalledPackageInfo hiding (package)
import Distribution.Package
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import qualified Text.PrettyPrint.HughesPJ as PP
import Control.Monad.Reader
import System.IO
import System.Process
import Data.Maybe
import System.FilePath
import System.Directory
import Data.List(isSuffixOf,zip4,nub)
import Debug.Trace
import Data.Binary
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)


import Ghf.Core.State
import Ghf.File
import Ghf.Info


import Ghf.SourceCollector


data CollectStatistics = CollectStatistics {
    packagesTotal       ::   Int
,   packagesWithSource  ::   Int
,   modulesTotal        ::   Int
,   modulesWithSource   ::   Int
,   parseFailures       ::   Int
} deriving Show

emptyCollectStatistics = CollectStatistics 0 0 0 0 0

collectInstalled :: Bool -> Session -> String -> Bool -> IO()
collectInstalled writeAscii session version forceRebuild = do
    collectorPath       <-  getCollectorPath version
    when forceRebuild $ do
        removeDirectoryRecursive collectorPath
        getCollectorPath version
        return ()
    knownPackages       <-  findKnownPackages collectorPath
--    putStrLn $ "found known packages" ++ " " ++ show knownPackages
    packageInfos        <-  getInstalledPackageInfos session
    let newPackages     =   filter (\pi -> not $Set.member (showPackageId $ fromDPid $ DP.package pi)
                                                        knownPackages)
                                    packageInfos
    exportedIfaceInfos  <-  mapM (\ info -> getIFaceInfos (fromDPid $ DP.package info)
                                            (DP.exposedModules info) session) newPackages
    hiddenIfaceInfos    <-  mapM (\ info -> getIFaceInfos (fromDPid $DP.package info)
                                        (DP.hiddenModules info) session) newPackages
    let extracted       =   map extractInfo $ zip4 exportedIfaceInfos
                                                hiddenIfaceInfos
                                                (map (fromDPid . DP.package) newPackages)
                                                ((map (\p -> map fromDPid (DP.depends p)))
                                                   newPackages)
    sources             <-  getSourcesMap
    (extracted',failedToParse) <- collectAllSources session sources extracted
    let statistic       =   CollectStatistics {
        packagesTotal       =   length newPackages
    ,   packagesWithSource  =   length $ filter (\p -> isJust (mbSourcePathPD p)) extracted'
    ,   modulesTotal        =   foldl (\n p -> n + length (exposedModulesPD p)) 0 extracted'
    ,   modulesWithSource   =   foldl (\n p -> n + length (
                                    filter (\p -> isJust (mbSourcePathMD p)) (exposedModulesPD p)))
                                        0 extracted'
    ,   parseFailures       =   failedToParse}
    putStrLn $ show statistic
    when (modulesWithSource statistic > 0) $
        putStrLn $ "failure percentage " ++ show (round ((fromIntegral   (parseFailures statistic)) /
                                (fromIntegral   (modulesWithSource statistic)) * 100.0))
    mapM_ (writeExtracted collectorPath writeAscii) extracted'


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


extractInfo :: ([(ModIface, FilePath)],[(ModIface, FilePath)],PackageIdentifier,
                    [PackageIdentifier]) -> PackageDescr
extractInfo (ifacesExp,ifacesHid,pi,depends) =
    let hiddenDescrs        =   foldr (extractExportedDescrH pi) Map.empty (map fst ifacesHid)
        (ids,mods)          =   --trace  ("\nhidden: " ++ show (Map.keysSet hiddenDescrs))
                                foldr (extractExportedDescrR pi hiddenDescrs)
                                        (Map.empty,[]) (map fst ifacesExp)
    in PackageDescr {
        packagePD           =   pi
    ,   exposedModulesPD    =   mods
    ,   buildDependsPD      =   depends
    ,   mbSourcePathPD      =   Nothing
    ,   idDescriptionsPD    =   nub $ concat $ Map.elems ids}

extractExportedDescrH :: PackageIdentifier -> ModIface -> SymbolTable -> SymbolTable
extractExportedDescrH pid iface amap =
    let exportedNames       = Set.fromList
                                $ map occNameString
                                    $ concatMap availNames
                                        $ concatMap snd (mi_exports iface)
        exportedDecls       =   filter (\ ifdecl -> (occNameString $ ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
    in  foldr (extractIdentifierDescr' pid []) amap exportedDecls

extractExportedDescrR :: PackageIdentifier -> SymbolTable -> ModIface ->
                            (SymbolTable,[ModuleDescr]) -> (SymbolTable,[ModuleDescr])
extractExportedDescrR pid hidden iface (imap,mdList) =
    let mid             =   moduleNameString $moduleName (mi_module iface)
        exportedNames   =   Set.fromList
                                $map occNameString
                                    $concatMap availNames
                                        $concatMap snd (mi_exports iface)
        exportedDecls   =   filter (\ ifdecl -> (occNameString $ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
        mapWithOwnDecls =   foldr (extractIdentifierDescr' pid [mid]) imap exportedDecls
        otherDecls      =   exportedNames `Set.difference` (Map.keysSet mapWithOwnDecls)
        reexported      =   Map.map (\v -> map (\id -> id{moduleIdID = (PM pid mid)}) v)
                                                                    {--: [moduleIdID id]--}
                                $Map.filterWithKey (\k v -> k `Set.member` otherDecls)
                                    hidden
        inst            =   concatMap extractInstances (mi_insts iface)
        uses            =   Map.fromList $ map extractUsages (mi_usages iface)
        mdescr          =   ModuleDescr {
                    moduleIdMD        =   PM pid mid
                ,   exportedNamesMD   =   exportedNames
                ,   mbSourcePathMD    =   Nothing
                ,   instancesMD       =   inst
                ,   usagesMD          =   uses}
        newids          =   Map.unionWith (\ v1 v2 -> error "impossible: extractExported")
                                mapWithOwnDecls reexported

    in  (newids, mdescr : mdList)

extractIdentifierDescr' :: PackageIdentifier -> [ModuleIdentifier] -> IfaceDecl ->
                                SymbolTable -> SymbolTable
extractIdentifierDescr' pid mods ifDecl amap =
    let descrs =  extractIdentifierDescr ifDecl mods pid
    in foldr addToMap amap descrs
        where
            addToMap :: IdentifierDescr -> SymbolTable -> SymbolTable
            addToMap iddescr amap =
                let allIds  =   identifierID iddescr : (constructorsID iddescr
                                ++ fieldsID iddescr
                                ++ classOpsID iddescr)
                in foldl (\ st id -> Map.insertWith (++) id [iddescr] st) amap allIds

extractIdentifierDescr :: IfaceDecl -> [ModuleIdentifier] -> PackageIdentifier
                            -> [IdentifierDescr]
extractIdentifierDescr decl modules package
       = [IdentifierDescr{
    identifierID        =   unpackFS $occNameFS (ifName decl)
,   typeInfoID          =   BS.pack $ filterExtras $ showSDocUnqual $ppr decl
,   identifierTypeID    =   case decl of
                                (IfaceId _ _ _ )            ->  Function
                                (IfaceData _ _ _ ifCons _ _ _ _)
                                    -> case ifCons of
                                        IfDataTyCon _       ->  Data
                                        IfNewTyCon _        ->  Newtype
                                        IfAbstractTyCon     ->  AbstractData
                                (IfaceSyn _ _ _ _ _ )       ->  Synonym
                                (IfaceClass _ _ _ _ _ _ _ ) ->  Class
                                (IfaceForeign _ _)          ->  Foreign
,   moduleIdID          =   PM package (last modules)
,   constructorsID      =   case decl of
                                (IfaceData _ _ _ ifCons _ _ _ _)
                                    -> map extractConstructorName
                                        (visibleIfConDecls ifCons)
                                otherwise -> []
,   fieldsID            =   case decl of
                                (IfaceData _ _ _ ifCons _ _ _ _)
                                    -> concatMap extractFieldNames
                                        (visibleIfConDecls ifCons)
                                otherwise -> []
,   classOpsID          =   case decl of
                                (IfaceClass _ _ _ _ _ ifSigs _ )
                                    -> map (extractClassOpName) ifSigs
                                otherwise -> []
,   mbLocation          =   Nothing}]

extractConstructorName ::  IfaceConDecl -> Symbol
extractConstructorName  decl    =   unpackFS $occNameFS (ifConOcc decl)

extractFieldNames ::  IfaceConDecl -> [Symbol]
extractFieldNames  decl    =   map (extractFieldNames') (ifConFields decl)

extractFieldNames' :: OccName -> Symbol
extractFieldNames' occName = unpackFS $occNameFS occName

extractClassOpName :: IfaceClassOp -> Symbol
extractClassOpName (IfaceClassOp occName _ _) = unpackFS $occNameFS occName

extractInstances :: IfaceInst -> [(ClassId, DataId)]
extractInstances ifaceInst  =
    let className   =   showSDocUnqual $ ppr $ ifInstCls ifaceInst
        dataNames   =   map (\iftc -> showSDocUnqual $ ppr iftc)
                            $ map fromJust
                                $ filter isJust
                                    $ ifInstTys ifaceInst
    in map (\dn -> (className, dn)) dataNames

extractUsages :: Usage -> (ModuleIdentifier, Set Symbol)
extractUsages usage =
    let name    =   (showSDoc . ppr) $  usg_name usage
        ids     =   map (showSDocUnqual . ppr . fst) $ usg_entities usage
    in (name, Set.fromList ids)

filterExtras :: String -> String
filterExtras ('{':'-':r)                =   filterExtras' r
filterExtras ('R':'e':'c':'F':'l':'a':'g':r)
                                        =   filterExtras (skipNextWord r)
filterExtras ('G':'e':'n':'e':'r':'i':'c':'s':':':r)
                                        =   filterExtras (skipNextWord r)
filterExtras ('F':'a':'m':'i':'l':'y':'I':'n':'s':'t':'a':'n':'c':'e':':':r)
                                        =   filterExtras (skipNextWord r)
filterExtras (c:r)                      =   c : filterExtras r
filterExtras []                         =   []

filterExtras' ('-':'}':r)   =   filterExtras r
filterExtras' (c:r)         =   filterExtras' r
filterExtras' []            =   []

skipNextWord (a:r)
    | isSpace a             =   skipNextWord r
    | otherwise             =   skipNextWord' r
skipNextWord []             =   []

skipNextWord'(a:r)
        | a == '\n'         =   r
        | isSpace a         =   a:r
        | otherwise         =   skipNextWord' r
skipNextWord' []            =   []

writeExtracted :: FilePath -> Bool -> PackageDescr -> IO ()
writeExtracted dirPath writeAscii pd = do
    let filePath    = dirPath </> showPackageId (packagePD pd) ++ ".pack"
    if writeAscii
        then writeFile (filePath ++ "dpg") (show pd)
        else encodeFile filePath pd

-------------------------------------------------------------------------

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
    (extractedWithSources,_)    <-  collectSources session sources extracted
    collectorPath   <-  getCollectorPath version
    writeExtracted collectorPath writeAscii extractedWithSources
    writeExtracted collectorPath True extractedWithSources
    putStrLn $ "\nExtracted infos for " ++ cabalPath

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
    paths           <-  getDirectoryContents filePath
    let nameList    =   map dropExtension  $filter (\s -> ".pack" `isSuffixOf` s) paths
    return (Set.fromList nameList)




