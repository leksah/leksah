{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Ghf.Info
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This module provides the infos collected by the extractor before
--   and an info pane to present some of them to the user
--
---------------------------------------------------------------------------------

module Ghf.Info (
    initInfo
,   loadAccessibleInfo
,   updateAccessibleInfo

,   clearCurrentInfo
,   buildCurrentInfo
,   buildActiveInfo
,   buildSymbolTable

,   getIdentifierDescr

,   getInstalledPackageInfos
,   findFittingPackages
,   findFittingPackagesDP

,   asDPid
,   fromDPid

) where

import System.IO
import qualified Data.Map as Map
import Config
import Control.Monad
import Control.Monad.Trans
import System.FilePath
import System.Directory
import qualified Data.Map as Map
import GHC
import System.IO
import Distribution.Version
import Data.List
import UniqFM
import qualified PackageConfig as DP
import Data.Maybe
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Distribution.Package
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Data.ByteString.Char8 (ByteString)




import Ghf.Utils.DeepSeq
import Ghf.File
import Ghf.Core.State
import {-# SOURCE #-} Ghf.InterfaceCollector
--import Ghf.Extractor

-- ---------------------------------------------------------------------
-- Binary Instances
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
    put (ModuleDescr moduleIdMD' exportedNamesMD' mbSourcePathMD' instancesMD' usagesMD'
                idDescriptionsMD')
        = do    put moduleIdMD'
                put exportedNamesMD'
                put mbSourcePathMD'
                put instancesMD'
                put usagesMD'
                put idDescriptionsMD'
    get = do    moduleIdMD'          <- get
                exportedNamesMD'     <- get
                mbSourcePathMD'      <- get
                instancesMD'         <- get
                usagesMD'            <- get
                idDescriptionsMD'    <- get
                return (ModuleDescr moduleIdMD' exportedNamesMD' mbSourcePathMD'
                                    instancesMD' usagesMD' idDescriptionsMD')

instance Binary IdentifierDescr where
    put (IdentifierDescr identifierID' identifierTypeID' typeInfoID' moduleIdID'
                            constructorsID' fieldsID' classOpsID' mbLocation' mbComment')
        = do    put identifierID'
                put identifierTypeID'
                put typeInfoID'
                put moduleIdID'
                put constructorsID'
                put fieldsID'
                put classOpsID'
                put mbLocation'
                put mbComment'
    get = do    identifierID'        <- get
                identifierTypeID'    <- get
                typeInfoID'          <- get
                moduleIdID'          <- get
                constructorsID'      <- get
                fieldsID'            <- get
                classOpsID'          <- get
                mbLocation'          <- get
                mbComment'           <- get
                return (IdentifierDescr identifierID' identifierTypeID' typeInfoID'
                           moduleIdID' constructorsID' fieldsID' classOpsID' mbLocation' mbComment')

instance Binary IdType where
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

initInfo :: GhfAction
initInfo = do
    session' <- readGhf session
    let version     =   cProjectVersion
    lift $ putStrLn "Before running collector"
    lift $ collectInstalled False session' version False
    lift $ putStrLn "After running collector"
    lift $ putStrLn "Before loading infos"
    loadAccessibleInfo
    lift $ putStrLn "After loading infos"

--
-- | Load all infos for all installed and exposed packages
--   (see shell command: ghc-pkg list)
--
loadAccessibleInfo :: GhfAction
loadAccessibleInfo =
    let version     =   cProjectVersion in do
        session'        <-  readGhf session

        collectorPath   <-  lift $ getCollectorPath version
        packageInfos    <-  lift $ getInstalledPackageInfos session'
        packageList     <-  lift $ mapM (loadInfosForPackage collectorPath)
                                                    (map (fromDPid . DP.package) packageInfos)
        let scope       =   foldr buildScope (Map.empty,Map.empty)
                                $ map fromJust
                                    $ filter isJust packageList
        modifyGhf_ (\ghf -> return (ghf{accessibleInfo = (Just scope)}))

--
-- | Clears the current info, not the world infos
--
clearCurrentInfo :: GhfAction
clearCurrentInfo = do
    modifyGhf_ (\ghf    ->  return (ghf{currentInfo = Nothing}))

--
-- | Builds the current info for a package
--
buildCurrentInfo :: [Dependency] -> GhfAction
buildCurrentInfo depends = do
    session'            <-  readGhf session
    fp                  <-  lift $findFittingPackages session' depends
    mbActive            <-  buildActiveInfo'
    case mbActive of
        Nothing         -> modifyGhf_ (\ghf -> return (ghf{currentInfo = Nothing}))
        Just active     -> do
            accessibleInfo'     <-  readGhf accessibleInfo
            case accessibleInfo' of
                Nothing         ->  modifyGhf_ (\ghf -> return (ghf{currentInfo = Nothing}))
                Just (pdmap,_)  ->  do
                    let packageList =   map (\ pin -> pin `Map.lookup` pdmap) fp
                    let scope       =   foldr buildScope (Map.empty,Map.empty)
                                            $ map fromJust
                                                $ filter isJust packageList
                    modifyGhf_ (\ghf -> return (ghf{currentInfo = Just (active, scope)}))

--
-- | Builds the current info for the activePackage
--
buildActiveInfo :: GhfAction
buildActiveInfo = do
    currentInfo         <-  readGhf currentInfo
    case currentInfo of
        Nothing                 -> return ()
        Just (active, scope)    -> do
            newActive   <-  buildActiveInfo'
            case newActive of
                Nothing         -> return ()
                Just newActive  ->
                    modifyGhf_ (\ghf -> return (ghf{currentInfo = Just (newActive, scope)}))


--
-- | Builds the current info for the activePackage
--
buildActiveInfo' :: GhfM (Maybe PackageScope)
buildActiveInfo' =
    let version         =   cProjectVersion in do
    activePack          <-  readGhf activePack
    session             <-  readGhf session
    case activePack of
        Nothing         ->  return Nothing
        Just ghfPackage ->  do
            lift $ collectUninstalled False session cProjectVersion (cabalFile ghfPackage)
            lift $ putStrLn "uninstalled collected"
            collectorPath   <-  lift $ getCollectorPath cProjectVersion
            packageDescr    <-  lift $ loadInfosForPackage collectorPath
                                            (packageId ghfPackage)
            case packageDescr of
                Nothing     -> return Nothing
                Just pd     -> do
                    let scope       =   buildScope pd (Map.empty,Map.empty)
                    return (Just scope)

--
-- | Updates the world info (it is the responsibility of the caller to rebuild
--   the current info)
--
updateAccessibleInfo :: GhfAction
updateAccessibleInfo = do
    wi              <-  readGhf accessibleInfo
    session         <-  readGhf session
    let version     =   cProjectVersion
    case wi of
        Nothing -> loadAccessibleInfo
        Just (psmap,psst) -> do
            packageInfos        <-  lift $ getInstalledPackageInfos session
            let packageIds      =   map (fromDPid . DP.package) packageInfos
            let newPackages     =   filter (\ pi -> Map.member pi psmap) packageIds
            let trashPackages   =   filter (\ e  -> not (elem e packageIds))(Map.keys psmap)
            if null newPackages && null trashPackages
                then return ()
                else do
                    collectorPath   <-  lift $ getCollectorPath version
                    newPackageInfos <-  lift $ mapM (\pid -> loadInfosForPackage collectorPath pid)
                                                        newPackages
                    let psamp2      =   foldr (\e m -> Map.insert (packagePD e) e m)
                                                psmap
                                                (map fromJust
                                                    $ filter isJust newPackageInfos)
                    let psamp3      =   foldr (\e m -> Map.delete e m) psmap trashPackages
                    let scope       =   foldr buildScope (Map.empty,Map.empty)
                                            (Map.elems psamp3)
                    modifyGhf_ (\ghf -> return (ghf{accessibleInfo = Just scope}))


--
-- | Loads the infos for the given packages
--
loadInfosForPackage :: FilePath -> PackageIdentifier -> IO (Maybe PackageDescr)
loadInfosForPackage dirPath pid = do
    let filePath = dirPath </> showPackageId pid ++ ".pack"
    exists <- doesFileExist filePath
    if exists
        then catch (do
            file            <-  openBinaryFile filePath ReadMode
            bs              <-  BS.hGetContents file
--            putStrLn $ "Now reading iface " ++ showPackageId pid
            let packageInfo = decode bs
            packageInfo `deepSeq` (hClose file)
            return (Just packageInfo))
            (\e -> do putStrLn (show e); return Nothing)
        else do
            message $"packageInfo not found for " ++ showPackageId pid
            return Nothing

--
-- | Loads the infos for the given packages (has an collecting argument)
--
buildScope :: PackageDescr -> PackageScope -> PackageScope
buildScope packageD (packageMap, symbolTable) =
    let pid = packagePD packageD
    in if pid `Map.member` packageMap
        then trace  ("package already in world " ++ (showPackageId $ packagePD packageD))
                    (packageMap, symbolTable)
        else (Map.insert pid packageD packageMap,
              buildSymbolTable packageD symbolTable)

buildSymbolTable :: PackageDescr -> SymbolTable -> SymbolTable
buildSymbolTable pDescr symbolTable =
     foldl (\ st idDescr ->  let allIds = identifierID idDescr : (constructorsID idDescr
                                                        ++ fieldsID idDescr ++ classOpsID idDescr)
                        in foldl (\ st2 id -> Map.insertWith (++) id [idDescr] st2) st allIds)
        symbolTable (idDescriptionsPD pDescr)

--
-- | Lookup of the identifier description
--
getIdentifierDescr :: String -> SymbolTable -> SymbolTable -> [IdentifierDescr]
getIdentifierDescr str st1 st2 =
    case str `Map.lookup` st1 of
        Nothing -> case str `Map.lookup` st2 of
                        Nothing -> []
                        Just list -> list
        Just list -> case str `Map.lookup` st2 of
                        Nothing -> list
                        Just list2 -> list ++ list2

{--
typeDescription :: String -> SymbolTable -> String
typeDescription str st =
    case str `Map.lookup` st of
        Nothing -> "No info found -- Testing for scoped symbols missing \n"
        Just list -> concatMap generateText list
    where
        ttString Function   =   "identifies a function of type "
        ttString Data       =   "identifies data definition"
        ttString Newtype    =   "identifies a Newtype"
        ttString Synonym    =   "identifies a synonym type for"
        ttString AbstractData = "identifies an abstract data type"
        ttString Constructor =  "identifies a constructor of data type"
        ttString Field      =   "identifies a field in a record with type"
        ttString Class      =   "identifies a class"
        ttString ClassOp    =   "identifies a class operation with type "
        ttString Foreign    =   "identifies something strange"
        generateText (IdentifierDescr _ tt ti m) =
            str ++ " "  ++   (ttString tt) ++ "\n   "
                ++   ti ++  "\n   "
                ++   "exported by modules "  ++   show m ++ "\n"
--}

-- ---------------------------------------------------------------------
-- The little helpers
--

getInstalledPackageInfos :: Session -> IO [DP.InstalledPackageInfo]
getInstalledPackageInfos session = do
    dflags1         <-  getSessionDynFlags session
    pkgInfos        <-  case pkgDatabase dflags1 of
                            Nothing -> return []
                            Just fm -> return (eltsUFM fm)
    return pkgInfos

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

asDPid :: PackageIdentifier -> DP.PackageIdentifier
asDPid (PackageIdentifier name version) = DP.PackageIdentifier name version

fromDPid :: DP.PackageIdentifier -> PackageIdentifier
fromDPid (DP.PackageIdentifier name version) = PackageIdentifier name version

-- ---------------------------------------------------------------------
-- Forcing Evaluation
--

instance DeepSeq Location where
    deepSeq pd =  deepSeq (locationSLine pd)
                    $   deepSeq (locationSCol pd)
                    $   deepSeq (locationELine pd)
                    $   deepSeq (locationECol pd)

instance DeepSeq PackageDescr where
    deepSeq pd =  deepSeq (packagePD pd)
                    $   deepSeq (mbSourcePathPD pd)
                    $   deepSeq (exposedModulesPD pd)
                    $   deepSeq (buildDependsPD pd)
                    $   deepSeq (idDescriptionsPD pd)

instance DeepSeq ModuleDescr where
    deepSeq pd =  deepSeq (moduleIdMD pd)
                    $   deepSeq (mbSourcePathMD pd)
                    $   deepSeq (exportedNamesMD pd)
                    $   deepSeq (instancesMD pd)
                    $   deepSeq (usagesMD pd)

instance DeepSeq IdentifierDescr where
    deepSeq pd =  deepSeq (identifierID pd)
                    $   deepSeq (identifierTypeID pd)
                    $   deepSeq (typeInfoID pd)
                    $   deepSeq (moduleIdID pd)
                    $   deepSeq (constructorsID pd)
                    $   deepSeq (fieldsID pd)
                    $   deepSeq (classOpsID pd)
                    $   deepSeq (mbLocation pd)

instance DeepSeq PackageIdentifier where
    deepSeq pd =  deepSeq (pkgName pd)
                    $   deepSeq (pkgVersion pd)

instance DeepSeq alpha  => DeepSeq (Set alpha) where
    deepSeq s =  deepSeq (Set.elems s)

instance (DeepSeq alpha, DeepSeq beta) => DeepSeq (Map alpha beta) where
    deepSeq s =  deepSeq (Map.toList s)

instance DeepSeq IdType where  deepSeq = seq

instance DeepSeq ByteString where  deepSeq = seq

instance DeepSeq Version where  deepSeq = seq

instance DeepSeq PackModule where
    deepSeq pd =  deepSeq (pack pd)
                    $   deepSeq (modu pd)
