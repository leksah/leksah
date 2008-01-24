{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.Info
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

module IDE.Metainfo.Info (
    InfoAction(..)
,   getIdentifierDescr
,   findFittingPackages
) where

import System.IO
import qualified Data.Map as Map
import Config
import Control.Monad
import Control.Monad.Trans
import System.FilePath
import System.Directory
import qualified Data.Map as Map
import System.IO
import Distribution.Version
import Data.List
import qualified PackageConfig as DP
import Data.Maybe
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Distribution.Package
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import Data.ByteString.Char8 (ByteString)
import System.Process(ProcessHandle,getProcessExitCode)
import System.Glib.MainLoop(timeoutAddFull,priorityDefaultIdle)
import Control.Monad.Reader(runReaderT,ask)

import IDE.Utils.DeepSeq
import IDE.Utils.File
import IDE.Core.State
import IDE.Metainfo.InterfaceCollector

class InfoAction alpha where
    -- | Update and initialize metadata for the world
    initInfo            ::   alpha
    -- | Builds the current info for a package
    -- Called when a package gets active
    buildCurrentInfo    ::   [Dependency] -> alpha
    -- | Builds the current info for the activePackage in the background.
    -- If a process handle is given, it waits for this process to finish before building
    -- Called after a build
    rebuildInBackground ::   Maybe ProcessHandle -> alpha
    -- | Manually rebuild current info from menu
    buildActiveInfo     ::   alpha

instance InfoAction IDEAction where
    initInfo            =   initInfo'
    buildCurrentInfo    =   buildCurrentInfo'
    rebuildInBackground =   rebuildInBackground'
    buildActiveInfo     =   buildActiveInfo'

--
-- | Update and initialize metadata for the world
--
initInfo' :: IDEAction
initInfo' = do
    session'        <-  readIDE session
    ideMessage Normal "Now updating metadata ..."
    lift $ collectInstalled session' False
    ideMessage Normal "Now loading metadata ..."
    loadAccessibleInfo
    ideMessage Normal "Finished loading ..."

--
-- | Load all infos for all installed and exposed packages
--   (see shell command: ghc-pkg list)
--
loadAccessibleInfo :: IDEAction
loadAccessibleInfo =
    let version     =   cProjectVersion in do
        session'        <-  readIDE session

        collectorPath   <-  lift $ getCollectorPath version
        packageInfos    <-  lift $ getInstalledPackageInfos session'
        packageList     <-  lift $ mapM (loadInfosForPackage collectorPath)
                                                    (map (fromDPid . DP.package) packageInfos)
        let scope       =   foldr buildScope (Map.empty,Map.empty)
                                $ map fromJust
                                    $ filter isJust packageList
        modifyIDE_ (\ide -> return (ide{accessibleInfo = (Just scope)}))

--
-- | Clears the current info, not the world infos
--
clearCurrentInfo :: IDEAction
clearCurrentInfo = do
    modifyIDE_ (\ide    ->  return (ide{currentInfo = Nothing}))
    ide <- getIDE
    triggerEvent ide CurrentInfo
    return ()

--
-- | Builds the current info for a package
--
buildCurrentInfo' :: [Dependency] -> IDEAction
buildCurrentInfo' depends = do
    session'            <-  readIDE session
    fp                  <-  lift $findFittingPackages session' depends
    mbActive            <-  loadActiveInfo
    case mbActive of
        Nothing         ->  modifyIDE_ (\ide -> return (ide{currentInfo = Nothing}))
        Just active     -> do
            accessibleInfo'     <-  readIDE accessibleInfo
            case accessibleInfo' of
                Nothing         ->  modifyIDE_ (\ide -> return (ide{currentInfo = Nothing}))
                Just (pdmap,_)  ->  do
                    let packageList =   map (\ pin -> pin `Map.lookup` pdmap) fp
                    let scope       =   foldr buildScope (Map.empty,Map.empty)
                                            $ map fromJust
                                                $ filter isJust packageList
                    modifyIDE_ (\ide -> return (ide{currentInfo = Just (active, scope)}))
    ide <- getIDE
    triggerEvent ide CurrentInfo
    return ()


--
-- | Builds the current info for the activePackage in the background
--   If a process handle is given, it waits for this process to finish before building
--
rebuildInBackground' :: Maybe ProcessHandle -> IDEAction
rebuildInBackground' mbHandle = do
    ideR <- ask
    lift $ do
        timeoutAddFull (myRebuild ideR) priorityDefaultIdle 500
        return ()
    where
        myRebuild :: IDERef -> IO Bool
        myRebuild ideR =
            case mbHandle of
                Just hdl -> do
                    mbExitCode  <-  getProcessExitCode hdl
                    case mbExitCode of
                        --process not finished, wait for next call
                        Nothing ->  return True
                        Just _  ->  doIt ideR
                Nothing -> doIt ideR
        doIt :: IDERef -> IO Bool
        doIt ideR = do
                runReaderT (do
                    errs <- readIDE errors
                    when (length (filter isError errs) == 0) $ do
                        ideMessage Normal "Update meta info for active package"
                        buildActiveInfo') ideR
                return False

--
-- | Builds the current info for the activePackage
--
buildActiveInfo' :: IDEAction
buildActiveInfo' = do
    currentInfo         <-  readIDE currentInfo
    case currentInfo of
        Nothing                 -> return ()
        Just (active, scope)    -> do
            newActive   <-  buildActiveInfo2
            case newActive of
                Nothing         -> return ()
                Just newActive  -> do
                    modifyIDE_ (\ide -> return (ide{currentInfo = Just (newActive, scope)}))
                    ide <- getIDE
                    triggerEvent ide CurrentInfo
                    return ()



--
-- | Loads the current info for the activePackage
--
loadActiveInfo :: IDEM (Maybe PackageScope)
loadActiveInfo =
    let version         =   cProjectVersion in do
    activePack          <-  readIDE activePack
    session             <-  readIDE session
    case activePack of
        Nothing         ->  return Nothing
        Just idePackage ->  do
            collectorPath   <-  lift $ getCollectorPath cProjectVersion
            packageDescr    <-  lift $ loadInfosForPackage collectorPath
                                            (packageId idePackage)
            case packageDescr of
                Nothing     -> return Nothing
                Just pd     -> do
                    let scope       =   buildScope pd (Map.empty,Map.empty)
                    return (Just scope)

--
-- | Builds the current info for the activePackage
--
buildActiveInfo2 :: IDEM (Maybe PackageScope)
buildActiveInfo2 =
    let version         =   cProjectVersion in do
    activePack          <-  readIDE activePack
    session             <-  readIDE session
    case activePack of
        Nothing         ->  return Nothing
        Just idePackage ->  do
            lift $ collectUninstalled False session cProjectVersion (cabalFile idePackage)
            -- ideMessage Normal "uninstalled collected"
            collectorPath   <-  lift $ getCollectorPath cProjectVersion
            packageDescr    <-  lift $ loadInfosForPackage collectorPath
                                            (packageId idePackage)
            case packageDescr of
                Nothing     -> return Nothing
                Just pd     -> do
                    let scope       =   buildScope pd (Map.empty,Map.empty)
                    return (Just scope)


--
-- | Updates the world info (it is the responsibility of the caller to rebuild
--   the current info)
--
updateAccessibleInfo :: IDEAction
updateAccessibleInfo = do
    wi              <-  readIDE accessibleInfo
    session         <-  readIDE session
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
                    modifyIDE_ (\ide -> return (ide{accessibleInfo = Just scope}))


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
            let packageInfo = decode bs
            packageInfo `deepSeq` (hClose file)
            return (Just packageInfo))
            (\e -> do sysMessage Normal (show e); return Nothing)
        else do
            sysMessage Normal $"packageInfo not found for " ++ showPackageId pid
            return Nothing

--
-- | Loads the infos for the given packages (has an collecting argument)
--
buildScope :: PackageDescr -> PackageScope -> PackageScope
buildScope packageD (packageMap, symbolTable) =
    let pid = packagePD packageD
    in if pid `Map.member` packageMap
        then (packageMap, symbolTable)
        else (Map.insert pid packageD packageMap,
              buildSymbolTable packageD symbolTable)

buildSymbolTable :: PackageDescr -> SymbolTable -> SymbolTable
buildSymbolTable pDescr symbolTable =
     foldl (\ st idDescr ->  let allIds = identifierID idDescr : (allConstructorsID idDescr
                                                        ++ allFieldsID idDescr ++ allClassOpsID idDescr)
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


-- ---------------------------------------------------------------------
-- DeepSeq instances for forcing evaluation
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
                    $   deepSeq (usagesMD pd)

instance DeepSeq IdentifierDescr where
    deepSeq (SimpleDescr identifierID' identifierTypeID' typeInfoID' moduleIdID'
        mbLocation' mbComment')  =  deepSeq identifierID'
                    $   deepSeq identifierTypeID'
                    $   deepSeq typeInfoID'
                    $   deepSeq moduleIdID'
                    $   deepSeq mbLocation'
                    $   deepSeq mbComment'
    deepSeq (DataDescr identifierID' typeInfoID' moduleIdID' constructorsID' fieldsID'
        mbLocation' mbComment')  =  deepSeq identifierID'
                    $   deepSeq typeInfoID'
                    $   deepSeq moduleIdID'
                    $   deepSeq constructorsID'
                    $   deepSeq fieldsID'
                    $   deepSeq mbLocation'
                    $   deepSeq mbComment'
    deepSeq (ClassDescr identifierID'  typeInfoID' classOpsID' moduleIdID'
        mbLocation' mbComment')  =  deepSeq identifierID'
                    $   deepSeq typeInfoID'
                    $   deepSeq moduleIdID'
                    $   deepSeq classOpsID'
                    $   deepSeq mbLocation'
                    $   deepSeq mbComment'
    deepSeq (InstanceDescr identifierID' classID' moduleIdID'
        mbLocation' mbComment')  =  deepSeq identifierID'
                    $   deepSeq classID'
                    $   deepSeq moduleIdID'
                    $   deepSeq mbLocation'
                    $   deepSeq mbComment'

instance DeepSeq PackageIdentifier where
    deepSeq pd =  deepSeq (pkgName pd)
                    $   deepSeq (pkgVersion pd)

instance DeepSeq alpha  => DeepSeq (Set alpha) where
    deepSeq s =  deepSeq (Set.elems s)

instance (DeepSeq alpha, DeepSeq beta) => DeepSeq (Map alpha beta) where
    deepSeq s =  deepSeq (Map.toList s)

instance DeepSeq IdType where  deepSeq = seq

instance DeepSeq IdTypeS where  deepSeq = seq

instance DeepSeq ByteString where  deepSeq = seq

instance DeepSeq Version where  deepSeq = seq

instance DeepSeq PackModule where
    deepSeq pd =  deepSeq (pack pd)
                    $   deepSeq (modu pd)

