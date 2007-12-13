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
import HscTypes
import LoadIface
import Outputable hiding(trace)
import qualified Pretty as P
import IfaceSyn
import FastString
import Outputable hiding(trace)
import qualified PackageConfig as DP
import Name
import PrelNames
import PackageConfig(mainPackageId,unpackPackageId,mkPackageId)

import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import System.Directory
import qualified PackageConfig as DP
import Distribution.PackageDescription
import Distribution.InstalledPackageInfo hiding (package)
import Distribution.Package
import Distribution.Verbosity
import Control.Monad.Reader
import System.IO
import Data.Maybe
import System.FilePath
import System.Directory
import Data.List(zip4,nub)
import Data.Binary
import qualified Data.ByteString.Char8 as BS


import Data.Ghf.Default
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

instance Default CollectStatistics where
    getDefault          =   CollectStatistics getDefault getDefault getDefault getDefault
                                getDefault

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
    exportedIfaceInfos  <-  mapM (\ info -> getIFaceInfos (DP.mkPackageId $ DP.package info)
                                            (DP.exposedModules info) session) newPackages
    hiddenIfaceInfos    <-  mapM (\ info -> getIFaceInfos (DP.mkPackageId $ DP.package info)
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
        putStrLn $ "failure percentage "
            ++ show ((round (((fromIntegral   (parseFailures statistic)) :: Double) /
                       (fromIntegral   (modulesWithSource statistic)) * 100.0)):: Integer)
    mapM_ (writeExtracted collectorPath writeAscii) extracted'

-------------------------------------------------------------------------

collectUninstalled :: Bool -> Session -> String -> FilePath -> IO ()
collectUninstalled writeAscii session version cabalPath = do
    pd                  <-  readPackageDescription normal cabalPath
                                        >>= return . flattenPackageDescription
    let modules         =   nub $ exeModules pd ++ libModules pd
    let basePath        =   takeDirectory cabalPath
    let srcPaths        =   concatMap (\ exe -> hsSourceDirs $ buildInfo exe)
                                                    $ executables pd
    let srcPath         =   case library pd of
                                Just (Library _ bi)    -> hsSourceDirs bi
                                Nothing                -> []
    dflags0         <-  getSessionDynFlags session
    setSessionDynFlags session dflags0
        {   topDir      =   basePath
        ,   importPaths =   srcPath ++ srcPaths
        --,   thisPackage =   mkPackageId (package pd)
        --,   ghcMode    =   OneShot
        }
    allIfaceInfos   <-  getIFaceInfos mainPackageId modules session
    deps            <-  findFittingPackages session (buildDepends pd)
    let extracted   =   extractInfo (allIfaceInfos,[], package pd, deps)
    let sources     =   Map.fromList [(package pd,[cabalPath])]
    (extractedWithSources,_)    <-  collectSources session sources extracted
    collectorPath   <-  getCollectorPath version
    writeExtracted collectorPath writeAscii extractedWithSources
    writeExtracted collectorPath True extractedWithSources
    putStrLn $ "\nExtracted infos for " ++ cabalPath

--getIFaceInfos2 :: PackageIdentifier -> [String] -> Session -> IO [(ModIface, FilePath)]
--getIFaceInfos2 modules session = do
--    let ifaces          =    mapM (\ m -> findAndReadIface empty
--                                            (mkModule (DP.mkPackageId (asDPid pckg))
--                                                              (mkModuleName mn)) False)
--                                                modules
--    hscEnv              <-  sessionHscEnv session
--    let gblEnv          =   IfGblEnv { if_rec_types = Nothing }
--    maybes              <-  initTcRnIf  'i' hscEnv gblEnv () ifaces
--    let res             =   catMaybes (map handleErr maybes)
--    return res
--    where
--        handleErr (M.Succeeded val)   =   Just val
--        handleErr (M.Failed mess)     =   trace (P.render (mess defaultErrStyle)) Nothing

--findAndReadIface :: SDoc -> Module
--		 -> IsBootInterface	-- True  <=> Look for a .hi-boot file
--					-- False <=> Look for .hi file
--		 -> TcRnIf gbl lcl (MaybeErr Message (ModIface, FilePath))


{--
#if __GHC__ > 670
    let ifaces          =   mapM readBinIface filePaths
    hscEnv              <-  sessionHscEnv session
    let gblEnv          =   IfGblEnv { if_rec_types = Nothing }
    res                 <-  initTcRnIf  'i' hscEnv gblEnv () ifaces
#else
    res                 <-   mapM readBinIface filePaths
#endif
    return (zip res filePaths)
--}
-------------------------------------------------------------------------

getIFaceInfos :: PackageId -> [String] -> Session -> IO [(ModIface, FilePath)]
getIFaceInfos pckg modules session =
    case unpackPackageId pckg of
        Nothing -> return []
        Just pid -> do
            let isBase          =   pkgName pid == "base"
            let ifaces          =   mapM (\ mn -> findAndReadIface empty
                                                  (if isBase
                                                        then mkBaseModule_ (mkModuleName mn)
                                                        else mkModule pckg
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
                                $Map.filterWithKey (\k _ -> k `Set.member` otherDecls)
                                    hidden
        inst            =   concatMap extractInstances (mi_insts iface)
        uses            =   Map.fromList $ map extractUsages (mi_usages iface)
        mdescr          =   ModuleDescr {
                    moduleIdMD        =   PM pid mid
                ,   exportedNamesMD   =   exportedNames
                ,   mbSourcePathMD    =   Nothing
                ,   instancesMD       =   inst
                ,   usagesMD          =   uses}
        newids          =   Map.unionWith (\ _ _ -> error "impossible: extractExported")
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
       = if null modules
          then []
          else [IdentifierDescr{
    identifierID        =   unpackFS $occNameFS (ifName decl)
,   typeInfoID          =   BS.pack $ filterExtras $ showSDocUnqual $ppr decl
,   identifierTypeID    =   case decl of
                                (IfaceId _ _ _ )            ->  Function
                                (IfaceData _ _ _ ifCons _ _ _ _)
                                    -> case ifCons of
                                        IfDataTyCon _       ->  Data
                                        IfNewTyCon _        ->  Newtype
                                        IfAbstractTyCon     ->  AbstractData
                                        IfOpenDataTyCon     ->  OpenData
                                (IfaceSyn _ _ _ _ _ )       ->  Synonym
                                (IfaceClass _ _ _ _ _ _ _ ) ->  Class
                                (IfaceForeign _ _)          ->  Foreign
,   moduleIdID          =   PM package (last modules)
,   constructorsID      =   case decl of
                                (IfaceData _ _ _ ifCons _ _ _ _)
                                    -> map extractConstructorName
                                        (visibleIfConDecls ifCons)
                                _   -> []
,   fieldsID            =   case decl of
                                (IfaceData _ _ _ ifCons _ _ _ _)
                                    -> concatMap extractFieldNames
                                        (visibleIfConDecls ifCons)
                                _   -> []
,   classOpsID          =   case decl of
                                (IfaceClass _ _ _ _ _ ifSigs _ )
                                    -> map (extractClassOpName) ifSigs
                                _   -> []
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

filterExtras, filterExtras' :: String -> String
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
filterExtras' (_:r)         =   filterExtras' r
filterExtras' []            =   []

skipNextWord, skipNextWord' :: String -> String
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




