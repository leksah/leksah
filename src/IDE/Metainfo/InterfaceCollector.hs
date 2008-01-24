{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.InterfaceCollector
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

module IDE.Metainfo.InterfaceCollector (
    collectInstalled
,   collectInstalledI
,   collectInstalled'
,   collectUninstalled
,   getInstalledPackageInfos
,   asDPid
,   fromDPid
,   findFittingPackages

) where


import GHC hiding(Id,Failed,Succeeded)
import Module
import TcRnMonad
import qualified Maybes as M
import HscTypes
import LoadIface
import Outputable hiding(trace)
import IfaceSyn
import FastString
import Outputable hiding(trace)
import qualified PackageConfig as DP
import Name
import PrelNames
import PackageConfig(mainPackageId,unpackPackageId,mkPackageId)
import Maybes
import TcRnTypes
import Finder
import qualified FastString as FS
import ErrUtils
import Config(cProjectVersion)
import UniqFM

import Data.List (maximumBy)
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import System.Directory
import qualified PackageConfig as DP
import Distribution.PackageDescription
import Distribution.InstalledPackageInfo hiding (package)
import Distribution.Package
import Distribution.Verbosity
import Distribution.Version
import Control.Monad.Reader
import System.IO
import Data.Maybe
import System.FilePath
import System.Directory
import Data.List(zip4,nub)
import Data.Binary
import qualified Data.ByteString.Char8 as BS


import IDE.Utils.Default
import IDE.Core.State
import IDE.Utils.File
--import IDE.Metainfo.Info
import IDE.Metainfo.SourceCollector

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

collectInstalledI :: Bool -> IDEAction
collectInstalledI b      =   do
    session'            <-  readIDE session
    sources             <-  lift $ getSourcesMap
    lift $ collectInstalled' False session' cProjectVersion b sources


collectInstalled :: Session -> Bool -> IO ()
collectInstalled session b     =   do
    sources             <-  getSourcesMap
    collectInstalled' False session cProjectVersion b sources

collectInstalled' :: Bool -> Session -> String -> Bool -> Map PackageIdentifier [FilePath] -> IO()
collectInstalled' writeAscii session version forceRebuild sources = do
    collectorPath       <-  getCollectorPath version
    when forceRebuild $ do
        removeDirectoryRecursive collectorPath
        getCollectorPath version
        return ()
    knownPackages       <-  findKnownPackages collectorPath
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
    (extracted',failedToParse) <- collectAllSources session sources extracted
    let statistic       =   CollectStatistics {
        packagesTotal       =   length newPackages
    ,   packagesWithSource  =   length $ filter (\p -> isJust (mbSourcePathPD p)) extracted'
    ,   modulesTotal        =   foldl (\n p -> n + length (exposedModulesPD p)) 0 extracted'
    ,   modulesWithSource   =   foldl (\n p -> n + length (
                                    filter (\p -> isJust (mbSourcePathMD p)) (exposedModulesPD p)))
                                        0 extracted'
    ,   parseFailures       =   failedToParse}
    sysMessage Normal $ show statistic
    when (modulesWithSource statistic > 0) $
        sysMessage Normal $ "failure percentage "
            ++ show ((round (((fromIntegral   (parseFailures statistic)) :: Double) /
                       (fromIntegral   (modulesWithSource statistic)) * 100.0)):: Integer)
    mapM_ (writeExtracted collectorPath writeAscii) extracted'

collectUninstalled :: Bool -> Session -> String -> FilePath -> IO ()
collectUninstalled writeAscii session version cabalPath = do
    pd                  <-  readPackageDescription normal cabalPath
                                        >>= return . flattenPackageDescription
    let modules         =   nub $ exeModules pd ++ libModules pd
    let basePath        =   takeDirectory cabalPath
    let buildPath       =   "dist" </> "build" </> pkgName (package pd) </>
                                (pkgName (package pd) ++ "-tmp/")
    dflags0             <-  getSessionDynFlags session
    setSessionDynFlags session
        dflags0
        {   topDir      =   basePath
        ,   importPaths =   [buildPath]
        --,   thisPackage =   mkPackageId (package pd)
        ,   ghcMode    =   OneShot
        }
    dflags1         <-  getSessionDynFlags session
    (dflags2,_)     <-  parseDynamicFlags dflags1 ["-fglasgow-exts","-haddock"]
    setSessionDynFlags session dflags2
    allIfaceInfos   <-  getIFaceInfos2 modules session
    deps            <-  findFittingPackages session (buildDepends pd)
    let extracted   =   extractInfo (allIfaceInfos,[], package pd, deps)
    let sources     =   Map.fromList [(package pd,[cabalPath])]
    (extractedWithSources,_)    <-  collectSources session sources extracted
    collectorPath   <-  getCollectorPath version
    writeExtracted collectorPath writeAscii extractedWithSources
    writeExtracted collectorPath True extractedWithSources
    sysMessage Normal $ "\nExtracted infos for " ++ cabalPath

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
                handleErr (M.Failed mess)     =   Nothing

getIFaceInfos2 :: [String] -> Session -> IO [(ModIface, FilePath)]
getIFaceInfos2 modules session = do
    let ifaces          =   mapM (\ mn -> findAndReadIface2 mn
                                          (mkModule mainPackageId (mkModuleName mn))) modules
    hscEnv              <-  sessionHscEnv session
    let gblEnv          =   IfGblEnv { if_rec_types = Nothing }
    maybes              <-  initTcRnIf  'i' hscEnv gblEnv () ifaces
    let res             =   catMaybes (map handleErr maybes)
    return res
    where
        handleErr (M.Succeeded val)   =   Just val
        handleErr (M.Failed mess)     =   Nothing

findAndReadIface2 :: String -> Module -> TcRnIf gbl lcl (MaybeErr Message (ModIface, FilePath))
findAndReadIface2  doc mod =   do
    hsc_env     <-  getTopEnv
    mb_found    <-  ioToIOEnv (findExactModule hsc_env mod)
    case mb_found of
        Found loc mod   ->  do
            let file_path   =   ml_hi_file loc
            read_result     <-  readIface mod file_path False
            case read_result of
	            Failed _  ->  returnM (Failed (text $ "can't read iface " ++
                                                    doc ++ " at " ++ file_path))
	            Succeeded iface
		            | mi_module iface /= mod
                        ->  return (Failed (text $ "read but not equal" ++ doc))
		            | otherwise
                        ->  returnM (Succeeded (iface, file_path))
        _               ->  return (Failed (text $ "can't locate " ++ doc))

-------------------------------------------------------------------------

extractInfo :: ([(ModIface, FilePath)],[(ModIface, FilePath)],PackageIdentifier,
                    [PackageIdentifier]) -> PackageDescr
extractInfo (ifacesExp,ifacesHid,pi,depends) =
    let hiddenDescrs        =   foldr (extractExportedDescrH pi) Map.empty (map fst ifacesHid)
        mods                =   --trace  ("\nhidden: " ++ show (Map.keysSet hiddenDescrs))
                                map (extractExportedDescrR pi hiddenDescrs) (map fst ifacesExp)
    in PackageDescr {
        packagePD           =   pi
    ,   exposedModulesPD    =   mods
    ,   buildDependsPD      =   depends
    ,   mbSourcePathPD      =   Nothing}

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

extractExportedDescrR :: PackageIdentifier
    -> SymbolTable
    -> ModIface
    -> ModuleDescr
extractExportedDescrR pid hidden iface =
    let mid             =   moduleNameString $moduleName (mi_module iface)
        exportedNames   =   Set.fromList
                                $map occNameString
                                    $concatMap availNames
                                        $concatMap snd (mi_exports iface)
        exportedDecls   =   filter (\ ifdecl -> (occNameString $ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
        ownDecls        =   concatMap (extractIdentifierDescr pid [mid]) exportedDecls
--        otherDecls      =   exportedNames `Set.difference` (Map.keysSet mapWithOwnDecls)
--        reexported      =   Map.map (\v -> map (\id -> id{moduleIdID = (PM pid mid)}) v)
--                                                                    {--: [moduleIdID id]--}
--                                $Map.filterWithKey (\k _ -> k `Set.member` otherDecls)
--                                    hidden
        inst            =   concatMap (extractInstances (PM pid mid)) (mi_insts iface)
        uses            =   Map.fromList $ map extractUsages (mi_usages iface)
    in  ModuleDescr {
                    moduleIdMD          =   PM pid mid
                ,   exportedNamesMD     =   exportedNames
                ,   mbSourcePathMD      =   Nothing
                ,   usagesMD            =   uses
                ,   idDescriptionsMD    =   ownDecls ++ inst}

extractIdentifierDescr' :: PackageIdentifier -> [ModuleIdentifier] -> IfaceDecl ->
                                SymbolTable -> SymbolTable
extractIdentifierDescr' pid mods ifDecl amap =
    let descrs =  extractIdentifierDescr pid  mods ifDecl
    in foldr addToMap amap descrs
        where
            addToMap :: IdentifierDescr -> SymbolTable -> SymbolTable
            addToMap iddescr amap =
                let allIds  =   identifierID iddescr : (constructorsID iddescr
                                ++ fieldsID iddescr
                                ++ classOpsID iddescr)
                in foldl (\ st id -> Map.insertWith (++) id [iddescr] st) amap allIds

extractIdentifierDescr :: PackageIdentifier -> [ModuleIdentifier] -> IfaceDecl
                            -> [IdentifierDescr]
extractIdentifierDescr package modules decl
       = if null modules
          then []
          else case decl of
            (IfaceId _ _ _ )
                -> [SimpleDescr{
                        identifierID        =   unpackFS $occNameFS (ifName decl)
                    ,   typeInfoID          =   BS.pack $ filterExtras $ showSDocUnqual $ppr decl
                    ,   identifierTypeID    =   FunctionS
                    ,   moduleIdID          =   PM package (last modules)
                    ,   mbLocation          =   Nothing
                    ,   mbComment           =   Nothing
                    }]
            (IfaceData _ _ _ ifCons _ _ _ _)
                -> case ifCons of
                    IfDataTyCon _
                        ->  [DataDescr{
                                identifierID        =   unpackFS $occNameFS (ifName decl)
                            ,   typeInfoID          =   BS.pack $ filterExtras
                                                                $ showSDocUnqual $ppr decl
                            ,   moduleIdID          =   PM package (last modules)
                            ,   constructorsID      =   map extractConstructorName
                                                            (visibleIfConDecls ifCons)
                            ,   fieldsID            =   concatMap extractFieldNames
                                                            (visibleIfConDecls ifCons)
                            ,   mbLocation          =   Nothing
                            ,   mbComment           =   Nothing
                            }]
                    other -> [SimpleDescr{
                                identifierID        =   unpackFS $occNameFS (ifName decl)
                            ,   typeInfoID          =   BS.pack $ filterExtras
                                                                $ showSDocUnqual $ppr decl
                            ,   identifierTypeID    =   case other of
                                                            IfNewTyCon _    ->  NewtypeS
                                                            IfAbstractTyCon ->  AbstractDataS
                                                            IfOpenDataTyCon ->  OpenDataS
                                                            _               ->  throwIDE
                                                                "Impossible"
                            ,   moduleIdID          =   PM package (last modules)
                            ,   mbLocation          =   Nothing
                            ,   mbComment           =   Nothing
                            }]
            (IfaceClass _ _ _ _ _ ifSigs _ )
                        ->  [ClassDescr{
                                identifierID        =   unpackFS $occNameFS (ifName decl)
                            ,   typeInfoID          =   BS.pack $ filterExtras
                                                                $ showSDocUnqual $ppr decl
                            ,   moduleIdID          =   PM package (last modules)
                            ,   classOpsID          =   map (extractClassOpName) ifSigs
                            ,   mbLocation          =   Nothing
                            ,   mbComment           =   Nothing
                            }]
            (IfaceSyn _ _ _ _ _ )
                        ->  [SimpleDescr{
                                identifierID        =   unpackFS $occNameFS (ifName decl)
                            ,   typeInfoID          =   BS.pack $ filterExtras
                                                                $ showSDocUnqual $ppr decl
                            ,   identifierTypeID    =   SynonymS
                            ,   moduleIdID          =   PM package (last modules)
                            ,   mbLocation          =   Nothing
                            ,   mbComment           =   Nothing
                            }]
            (IfaceForeign _ _)
                        ->  [SimpleDescr{
                                identifierID        =   unpackFS $occNameFS (ifName decl)
                            ,   typeInfoID          =   BS.pack $ filterExtras
                                                                $ showSDocUnqual $ppr decl
                            ,   identifierTypeID    =   ForeignS
                            ,   moduleIdID          =   PM package (last modules)
                            ,   mbLocation          =   Nothing
                            ,   mbComment           =   Nothing
                            }]

extractConstructorName ::  IfaceConDecl -> Symbol
extractConstructorName  decl    =   unpackFS $occNameFS (ifConOcc decl)

extractFieldNames ::  IfaceConDecl -> [Symbol]
extractFieldNames  decl    =   map (extractFieldNames') (ifConFields decl)

extractFieldNames' :: OccName -> Symbol
extractFieldNames' occName = unpackFS $occNameFS occName

extractClassOpName :: IfaceClassOp -> Symbol
extractClassOpName (IfaceClassOp occName _ _) = unpackFS $occNameFS occName

extractInstances :: PackModule -> IfaceInst -> [IdentifierDescr]
extractInstances pm ifaceInst  =
    let className   =   showSDocUnqual $ ppr $ ifInstCls ifaceInst
        dataNames   =   map (\iftc -> showSDocUnqual $ ppr iftc)
                            $ map fromJust
                                $ filter isJust
                                    $ ifInstTys ifaceInst
    in [InstanceDescr
                    {   identifierID    =   className
                    ,   binds           =   dataNames
                    ,   moduleIdID      =   pm
                    ,   mbLocation      =   Nothing
                    ,   mbComment       =   Nothing}]

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

-- ---------------------------------------------------------------------
-- The (mothers) little helpers
--

getInstalledPackageInfos :: Session -> IO [DP.InstalledPackageInfo]
getInstalledPackageInfos session = do
    dflags1         <-  getSessionDynFlags session
    pkgInfos        <-  case pkgDatabase dflags1 of
                            Nothing -> return []
                            Just fm -> return (eltsUFM fm)
    return pkgInfos

asDPid :: PackageIdentifier -> DP.PackageIdentifier
asDPid (PackageIdentifier name version) = DP.PackageIdentifier name version

fromDPid :: DP.PackageIdentifier -> PackageIdentifier
fromDPid (DP.PackageIdentifier name version) = PackageIdentifier name version

findFittingPackages :: Session -> [Dependency] -> IO  [PackageIdentifier]
findFittingPackages session dependencyList = do
    knownPackages   <-  getInstalledPackageInfos session
    let packages    =   map (fromDPid . DP.package) knownPackages
    return (concatMap (fittingKnown packages) dependencyList)
    where
    fittingKnown packages (Dependency dname versionRange) =
        let filtered =  filter (\ (PackageIdentifier name version) ->
                                    name == dname && withinRange version versionRange)
                        packages
        in  if length filtered > 1
                then [maximumBy (\a b -> compare (pkgVersion a) (pkgVersion b)) filtered]
                else filtered

findFittingPackagesDP :: Session -> [Dependency] -> IO  [PackageIdentifier]
findFittingPackagesDP session dependencyList =  do
        fp <- (findFittingPackages session dependencyList)
        return fp

-- ---------------------------------------------------------------------
-- Binary Instances for linear storage
--

instance Binary PackModule where
    put (PM pack' modu')
        =   do  put pack'
                put modu'
    get =   do  pack'                <- get
                modu'                <- get
                return (PM pack' modu')

instance Binary PackageIdentifier where
    put (PackageIdentifier name' version')
        =   do  put name'
                put version'
    get =   do  name'                <- get
                version'             <- get
                return (PackageIdentifier name' version')

instance Binary Version where
    put (Version branch' tags')
        =   do  put branch'
                put tags'
    get =   do  branch'              <- get
                tags'                <- get
                return (Version branch' tags')


instance Binary PackageDescr where
    put (PackageDescr packagePD' exposedModulesPD' buildDependsPD' mbSourcePathPD')
        =   do  put packagePD'
                put exposedModulesPD'
                put buildDependsPD'
                put mbSourcePathPD'
    get =   do  packagePD'           <- get
                exposedModulesPD'    <- get
                buildDependsPD'      <- get
                mbSourcePathPD'      <- get
                return (PackageDescr packagePD' exposedModulesPD' buildDependsPD'
                                        mbSourcePathPD')

instance Binary ModuleDescr where
    put (ModuleDescr moduleIdMD' exportedNamesMD' mbSourcePathMD' usagesMD'
                idDescriptionsMD')
        = do    put moduleIdMD'
                put exportedNamesMD'
                put mbSourcePathMD'
                put usagesMD'
                put idDescriptionsMD'
    get = do    moduleIdMD'          <- get
                exportedNamesMD'     <- get
                mbSourcePathMD'      <- get
                usagesMD'            <- get
                idDescriptionsMD'    <- get
                return (ModuleDescr moduleIdMD' exportedNamesMD' mbSourcePathMD'
                                    usagesMD' idDescriptionsMD')

instance Binary IdentifierDescr where
    put (SimpleDescr identifierID' identifierTypeID' typeInfoID' moduleIdID'
                            mbLocation' mbComment')
        = do    put (1::Int)
                put identifierID'
                put identifierTypeID'
                put typeInfoID'
                put moduleIdID'
                put mbLocation'
                put mbComment'
    put (DataDescr identifierID' typeInfoID' moduleIdID'
                            constructorsID' fieldsID' mbLocation' mbComment')
        = do    put (2::Int)
                put identifierID'
                put typeInfoID'
                put moduleIdID'
                put constructorsID'
                put fieldsID'
                put mbLocation'
                put mbComment'
    put (ClassDescr identifierID' typeInfoID' moduleIdID'
                            classOpsID' mbLocation' mbComment')
        = do    put (3::Int)
                put identifierID'
                put typeInfoID'
                put moduleIdID'
                put classOpsID'
                put mbLocation'
                put mbComment'
    put (InstanceDescr identifierID' classID' moduleIdID' mbLocation' mbComment')
        = do    put (4::Int)
                put identifierID'
                put classID'
                put moduleIdID'
                put mbLocation'
                put mbComment'
    get = do    (typeHint :: Int)           <- get
                case typeHint of
                    1 -> do
                            identifierID'        <- get
                            identifierTypeID'    <- get
                            typeInfoID'          <- get
                            moduleIdID'          <- get
                            mbLocation'          <- get
                            mbComment'           <- get
                            return (SimpleDescr identifierID' identifierTypeID' typeInfoID'
                                       moduleIdID' mbLocation' mbComment')
                    2 -> do
                            identifierID'        <- get
                            typeInfoID'          <- get
                            moduleIdID'          <- get
                            constructorsID'      <- get
                            fieldsID'            <- get
                            mbLocation'          <- get
                            mbComment'           <- get
                            return (DataDescr identifierID' typeInfoID' moduleIdID'
                                        constructorsID' fieldsID' mbLocation' mbComment')
                    3 -> do
                            identifierID'        <- get
                            typeInfoID'          <- get
                            moduleIdID'          <- get
                            classOpsID'          <- get
                            mbLocation'          <- get
                            mbComment'           <- get
                            return (ClassDescr identifierID' typeInfoID' moduleIdID'
                                        classOpsID' mbLocation' mbComment')
                    4 -> do
                            identifierID'        <- get
                            classID'             <- get
                            moduleIdID'          <- get
                            mbLocation'          <- get
                            mbComment'           <- get
                            return (InstanceDescr identifierID' classID' moduleIdID'
                                        mbLocation' mbComment')
                    _ -> throwIDE "Impossible in Binary IdentifierDescr get"


instance Binary IdTypeS where
    put it  =   do  put (fromEnum it)
    get     =   do  code         <- get
                    return (toEnum code)

instance Binary Location where
    put (Location locationSLine' locationSCol' locationELine' locationECol')
        = do    put locationSLine'
                put locationSCol'
                put locationELine'
                put locationECol'
    get = do    locationSLine'       <-  get
                locationSCol'        <-  get
                locationELine'       <-  get
                locationECol'        <-  get
                return (Location locationSLine' locationSCol' locationELine' locationECol')


