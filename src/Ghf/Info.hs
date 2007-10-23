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

,   getIdentifierDescr

,   getInstalledPackageInfos
,   findFittingPackages
,   findFittingPackagesDP
,   fromPackageIdentifier
,   toPackageIdentifier

,   asDPid
,   fromDPid
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.ModelView as New
import Graphics.UI.Gtk.Multiline.TextView
import Control.Monad.Reader
import Data.IORef
import System.IO
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Config
import Control.Monad
import Control.Monad.Trans
import System.FilePath
import System.Directory
import Data.Map (Map)
import qualified Data.Map as Map
import GHC
import System.IO
import Control.Concurrent
import qualified Distribution.Package as DP
import Distribution.PackageDescription hiding (package)
import Distribution.InstalledPackageInfo hiding (package,InstalledPackageInfo)
import Distribution.Version
import Data.List
import UniqFM
import PackageConfig
import Data.Maybe
import Text.ParserCombinators.ReadP
import Data.Binary
import System.Process

import Ghf.File
import Ghf.Core
import Ghf.SourceCandy
import Ghf.ViewFrame
import Ghf.PropertyEditor
import Ghf.SpecialEditors
import Ghf.Log

initInfo :: GhfAction
initInfo = do
    lift $ putStrLn "Before running collector"
    pid <- lift $ runProcess "dist/build/ghf-collector/ghf-collector"  [] Nothing Nothing Nothing Nothing Nothing
    lift $ waitForProcess pid
    lift $ putStrLn "After running collector"
    loadAccessibleInfo

--
-- | Load all infos for all installed and exposed packages
--   (see shell command: ghc-pkg list)
--
loadAccessibleInfo :: GhfAction
loadAccessibleInfo =
    let version     =   cProjectVersion in do
        session         <-  readGhf session

        collectorPath   <-  lift $ getCollectorPath version
        packageInfos    <-  lift $ getInstalledPackageInfos session
        packageList     <-  lift $ mapM (loadInfosForPackage collectorPath)
                                                    (map package packageInfos)
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
    session             <-  readGhf session
    fp                  <-  lift $findFittingPackages session depends
    active              <-  buildActiveInfo'
    case active of
        Nothing         -> modifyGhf_ (\ghf -> return (ghf{currentInfo = Nothing}))
        Just active     -> do
            accessibleInfo      <-  readGhf accessibleInfo
            case accessibleInfo of
                Nothing         ->  modifyGhf_ (\ghf -> return (ghf{currentInfo = Nothing}))
                Just (pdmap,_)  ->  do
                    let packageList =   map (\ pi -> pi `Map.lookup` pdmap)
                                            $ map fromPackageIdentifier fp
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
    log                 <-  getLog
    case activePack of
        Nothing         ->  return Nothing
        Just ghfPackage ->  do
            pid <- lift $ runProcess "dist/build/ghf-collector/ghf-collector"
                                        ["--Uninstalled=" ++ cabalFile ghfPackage]
                                        Nothing Nothing Nothing Nothing Nothing
            lift $ waitForProcess pid
            collectorPath   <-  lift $ getCollectorPath version
            packageDescr    <-  lift $ loadInfosForPackage collectorPath
                                            (fromDPid $ packageId ghfPackage)
            case packageDescr of
                Nothing     -> return Nothing
                Just pd     -> do
                    let scope       =   buildScope pd (Map.empty,Map.empty)
                    return (Just scope)

--
-- | Updates the world info (it is the responsibility of the caller to rebuild
--   the current info
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
            let packageIds      =   map (fromPackageIdentifier . package) packageInfos
            let newPackages     =   filter (\ pi -> Map.member pi psmap) packageIds
            let trashPackages   =   filter (\ e  -> not (elem e packageIds))(Map.keys psmap)
            if null newPackages && null trashPackages
                then return ()
                else do
                    collectorPath   <-  lift $ getCollectorPath version
                    newPackageInfos <-  lift $ mapM (\pid -> loadInfosForPackage collectorPath pid)
                                                $ map toPackageIdentifier newPackages
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
            putStrLn $ "Now reading iface " ++ showPackageId pid
            packageInfo <- decodeFile filePath
            return (Just packageInfo))
            (\e -> do putStrLn (show e); return Nothing)
        else do
            message $"packaeInfo not found for " ++ showPackageId pid
            return Nothing

--
-- | Loads the infos for the given packages (has an collecting argument)
--
buildScope :: PackageDescr -> PackageScope -> PackageScope
buildScope packageD (packageMap, symbolTable) =
    let pid = packagePD packageD
    in if pid `Map.member` packageMap
        then trace  ("package already in world " ++ packagePD packageD)
                    (packageMap, symbolTable)
        else (Map.insert pid packageD packageMap,
              Map.unionWith (++) symbolTable (idDescriptionsPD packageD))

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

getInstalledPackageInfos :: Session -> IO [InstalledPackageInfo]
getInstalledPackageInfos session = do
    dflags1         <-  getSessionDynFlags session
    pkgInfos        <-  case pkgDatabase dflags1 of
                            Nothing -> return []
                            Just fm -> return (eltsUFM fm)
    return pkgInfos

findFittingPackages :: Session -> [Dependency] -> IO  [PackageIdentifier]
findFittingPackages session dependencyList = do
    knownPackages   <-  getInstalledPackageInfos session
    let packages    =   map package knownPackages
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

fromPackageIdentifier :: PackageIdentifier -> PackIdentifier
fromPackageIdentifier   =   showPackageId

toPackageIdentifier :: PackIdentifier -> PackageIdentifier
toPackageIdentifier pd    =   case readP_to_S DP.parsePackageId pd of
                                [(ps,_)]  -> fromDPid ps
                                _         -> error "cannot parse package identifier"


