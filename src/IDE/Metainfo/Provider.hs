{-# LANGUAGE CPP, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.Provider
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | This module provides the infos collected by the server before
--
---------------------------------------------------------------------------------

module IDE.Metainfo.Provider (
    getIdentifierDescr
,   getIdentifiersStartingWith
,   getCompletionOptions
,   getDescription
,   getActivePackageDescr
,   searchMeta

,   initInfo       -- Update and rebuild
,   updateSystemInfo
,   rebuildSystemInfo
,   updateWorkspaceInfo
,   rebuildWorkspaceInfo

,   getPackageInfo  -- Just retreive from State
,   getWorkspaceInfo
,   getSystemInfo

,   getPackageImportInfo -- Scope for the import tool
) where

import System.IO (hClose, openBinaryFile, IOMode(..))
import System.IO.Strict (readFile)
import qualified Data.Map as Map
import Control.Monad
import System.FilePath
import System.Directory
import Data.List
import Data.Maybe
import Distribution.Package hiding (depends,packageId)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Distribution.Version
import Distribution.ModuleName

import Control.DeepSeq
import IDE.Utils.FileUtils
import IDE.Core.State
import Data.Char (toLower,isUpper,toUpper,isLower)
import Text.Regex.TDFA
import qualified Text.Regex.TDFA as Regex
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.TDFA.String (execute,compile)
import Data.Binary.Shared (decodeSer)
#if MIN_VERSION_Cabal(1,16,0)
import Language.Haskell.Extension (KnownExtension)
#else
import Language.Haskell.Extension (knownExtensions)
#endif
import Distribution.Text (display)
import IDE.Core.Serializable ()
import Data.Map (Map(..))
import Control.Exception (SomeException(..), catch)
import Prelude hiding(catch, readFile)
import IDE.Utils.ServerConnection(doServerCommand)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Distribution.PackageDescription (hsSourceDirs)

trace a b = b

-- ---------------------------------------------------------------------
-- Updating metadata
--

--
-- | Update and initialize metadata for the world -- Called at startup
--
initInfo :: IDEAction -> IDEAction
initInfo continuation = do
    prefs  <- readIDE prefs
    if collectAtStart prefs
        then do
            ideMessage Normal "Now updating system metadata ..."
            callCollector False True True $ \ _ -> do
                ideMessage Normal "Now loading metadata ..."
                loadSystemInfo
                ideMessage Normal "Now updating workspace metadata ..."
                updateWorkspaceInfo' False $ \ _ -> do
                    ideMessage Normal "Finished"
                    triggerEventIDE (InfoChanged True) >> return ()

                    trace "blah" $ continuation
        else do
            ideMessage Normal "Now loading metadata ..."
            loadSystemInfo
            ideMessage Normal "Now updating workspace metadata ..."
            updateWorkspaceInfo' False $ \ _ -> do
                ideMessage Normal "Finished"
                triggerEventIDE (InfoChanged True) >> return ()
                continuation

updateSystemInfo :: IDEAction
updateSystemInfo     = trace "update sys info called" $ do
    updateSystemInfo' False $ \ _ ->
        updateWorkspaceInfo' False $ \ _ -> do
            triggerEventIDE (InfoChanged False) >> return ()

rebuildSystemInfo :: IDEAction
rebuildSystemInfo    =  trace "rebuild sys info called" $ do
    updateSystemInfo' True $ \ _ ->
        updateWorkspaceInfo' True $ \ _ ->
            triggerEventIDE (InfoChanged False) >> return ()

updateWorkspaceInfo :: IDEAction
updateWorkspaceInfo = trace "update workspace info called" $ do
    currentState' <- readIDE currentState
    case currentState' of
        IsStartingUp -> return ()
        _ -> do
            updateWorkspaceInfo' False $ \ _ -> do
                triggerEventIDE (InfoChanged False) >> return ()

rebuildWorkspaceInfo :: IDEAction
rebuildWorkspaceInfo = trace "rebuild workspace info called" $ do
    updateWorkspaceInfo' True $ \ _ -> do
        triggerEventIDE (InfoChanged False) >> return ()

--
-- | Load all infos for all installed and exposed packages
--   (see shell command: ghc-pkg list)
--
loadSystemInfo :: IDEAction
loadSystemInfo = do
    collectorPath   <-  liftIO $ getCollectorPath
    packageIds      <-  liftM nub $ liftIO $ getInstalledPackageIds
    packageList     <-  liftIO $ mapM (loadInfosForPackage collectorPath)
                                                packageIds
    let scope       =   foldr buildScope (PackScope Map.empty getEmptyDefaultScope)
                            $ catMaybes packageList
--    liftIO performGC
    modifyIDE_ (\ide -> ide{systemInfo = (Just (GenScopeC (addOtherToScope scope False)))})

    return ()

--
-- | Updates the system info
--
updateSystemInfo' :: Bool -> (Bool -> IDEAction) -> IDEAction
updateSystemInfo' rebuild continuation = do
    wi              <-  getSystemInfo
    case wi of
        Nothing -> loadSystemInfo
        Just (GenScopeC (PackScope psmap psst)) -> do
            packageIds          <-  liftIO $ getInstalledPackageIds
            let newPackages     =   filter (\ pi -> Map.member pi psmap) packageIds
            let trashPackages   =   filter (\ e  -> not (elem e packageIds))(Map.keys psmap)
            if null newPackages && null trashPackages
                then continuation True
                else do
                    callCollector rebuild True True $ \ _ -> do
                        collectorPath   <-  lift $ getCollectorPath
                        newPackageInfos <-  liftIO $ mapM (\pid -> loadInfosForPackage collectorPath pid)
                                                            newPackages
                        let psmap2      =   foldr (\e m -> Map.insert (pdPackage e) e m)
                                                    psmap
                                                    (map fromJust
                                                        $ filter isJust newPackageInfos)
                        let psmap3      =   foldr (\e m -> Map.delete e m) psmap2 trashPackages
                        let scope :: PackScope (Map String [Descr])
                                        =   foldr buildScope (PackScope Map.empty symEmpty)
                                                (Map.elems psmap3)
                        modifyIDE_ (\ide -> ide{systemInfo = Just (GenScopeC (addOtherToScope scope False))})
                        continuation True

getEmptyDefaultScope :: Map String [Descr]
getEmptyDefaultScope = symEmpty
--
-- | Rebuilds system info
--
rebuildSystemInfo' :: (Bool -> IDEAction) -> IDEAction
rebuildSystemInfo' continuation = do
    callCollector True True True $ \ _ -> do
        loadSystemInfo
        continuation True

-- ---------------------------------------------------------------------
-- Metadata for the workspace and active package
--


updateWorkspaceInfo' :: Bool -> (Bool -> IDEAction) -> IDEAction
updateWorkspaceInfo' rebuild continuation = do
    mbWorkspace         <- readIDE workspace
    systemInfo'         <- getSystemInfo
    case mbWorkspace of
        Nothing ->  do
            trace "no workspace" $ modifyIDE_ (\ide -> ide{workspaceInfo = Nothing, packageInfo = Nothing})
            continuation False
        Just ws -> do
            updatePackageInfos rebuild (wsPackages ws) $ \ _ packDescrs -> do
                let dependPackIds = (nub $ concatMap pdBuildDepends packDescrs)
                                        \\ map pdPackage packDescrs
                let packDescrsI =   case systemInfo' of
                                        Nothing -> []
                                        Just (GenScopeC (PackScope pdmap _)) ->
                                            catMaybes $ map (\ pid -> pid `Map.lookup` pdmap)  dependPackIds
                let scope1 :: PackScope (Map String [Descr])
                                =   foldr buildScope (PackScope Map.empty symEmpty) packDescrs
                let scope2 :: PackScope (Map String [Descr])
                                =   foldr buildScope (PackScope Map.empty symEmpty) packDescrsI
                modifyIDE_ (\ide -> ide{workspaceInfo = Just
                    (GenScopeC (addOtherToScope scope1 True), GenScopeC(addOtherToScope scope2 False))})
                -- Now care about active package
                activePack      <-  readIDE activePack
                case activePack of
                    Nothing -> do
                        modifyIDE_ (\ide -> ide{packageInfo = Nothing})
                    Just pack -> do
                        case filter (\pd -> pdPackage pd == ipdPackageId pack) packDescrs of
                            [pd] -> let impPackDescrs =
                                            case systemInfo' of
                                                Nothing -> []
                                                Just (GenScopeC (PackScope pdmap _)) ->
                                                     catMaybes $ map (\ pid -> pid `Map.lookup` pdmap)
                                                        (pdBuildDepends pd)
                                        -- The imported from the workspace should be treated different
                                        workspacePackageIds = map ipdPackageId (wsPackages ws)
                                        impPackDescrs' = filter (\pd -> not (elem (pdPackage pd)
                                                                    workspacePackageIds)) impPackDescrs
                                        impPackDescrs'' = catMaybes $ map (\pd ->
                                            if (elem (pdPackage pd) workspacePackageIds)
                                                then find (\pd' -> pdPackage pd == pdPackage pd') packDescrs
                                                else Nothing) impPackDescrs
                                        scope1 :: PackScope (Map String [Descr])
                                                =   buildScope pd (PackScope Map.empty symEmpty)
                                        scope2 :: PackScope (Map String [Descr])
                                                =   foldr buildScope (PackScope Map.empty symEmpty)
                                                        (impPackDescrs' ++ impPackDescrs'')
                                        in modifyIDE_ (\ide -> ide{packageInfo = Just
                                                            (GenScopeC (addOtherToScope scope1 False),
                                                            GenScopeC(addOtherToScope scope2 False))})
                            _    -> modifyIDE_ (\ide -> ide{packageInfo = Nothing})
                continuation True

updatePackageInfos :: Bool -> [IDEPackage] -> (Bool -> [PackageDescr] -> IDEAction) -> IDEAction
updatePackageInfos rebuild packs conts = updatePackageInfos' [] rebuild packs conts
    where
        updatePackageInfos' collector _ [] continuation =  continuation True collector
        updatePackageInfos' collector rebuild (hd:tail) continuation =
            updatePackageInfo rebuild hd $ \ _ packDescr ->
                updatePackageInfos' (packDescr : collector) rebuild tail continuation

updatePackageInfo :: Bool -> IDEPackage -> (Bool -> PackageDescr -> IDEAction) -> IDEAction
updatePackageInfo rebuild idePack continuation =
    trace ("updatePackageInfo " ++ show (ipdPackageId idePack)) $ do
    workspInfoCache'     <- readIDE workspInfoCache
    let (packageMap, ic) =  case pi  `Map.lookup` workspInfoCache' of
                                Nothing -> (Map.empty,True)
                                Just m  -> (m,False)
    modPairsMb <- liftIO $ mapM (\(modName, bi) -> do
            sf <- case  modName `Map.lookup` packageMap of
                        Nothing            -> findSourceFile (srcDirs' bi) haskellSrcExts modName
                        Just (_,Nothing,_) -> findSourceFile (srcDirs' bi) haskellSrcExts modName
                        Just (_,Just fp,_) -> return (Just fp)
            return (modName, sf))
                $ Map.toList $ ipdModules idePack
    mainModules <- liftIO $ mapM (\(fn, bi, isTest) -> do
                                    mbFn <- findSourceFile' (srcDirs' bi) fn
                                    return (main,mbFn))
                            (ipdMain idePack)
    let modPairsMb' = case mainModules of
                        [] -> modPairsMb
                        hd:_ -> hd : modPairsMb
    let (modWith,modWithout) = partition (\(x,y) -> isJust y) modPairsMb'
    let modWithSources       = map (\(f,s) -> (f,fromJust s)) modWith
    let modWithoutSources    = map fst $ modWithout
    -- Now see which modules have to be truely updated
    modToUpdate <- if rebuild
                            then return modWithSources
                            else liftIO $ figureOutRealSources idePack modWithSources
    trace ("updatePackageInfo modToUpdate " ++ show (map (display.fst) modToUpdate)) $
     callCollectorWorkspace
        rebuild
        (dropFileName (ipdCabalFile idePack))
        (ipdPackageId idePack)
        (map (\(x,y) -> (display x,y)) modToUpdate)
        (\ b -> do
            buildDepends         <- liftIO $ findFittingPackages (ipdDepends idePack)
            collectorPath        <- liftIO $ getCollectorPath
            let packageCollectorPath = collectorPath </> packageIdentifierToString pi
            (moduleDescrs,packageMap, changed, modWithout)
                                 <- liftIO $ foldM
                                        (getModuleDescr packageCollectorPath)
                                        ([],packageMap,False,modWithoutSources)
                                        modPairsMb'
            when changed $ modifyIDE_ (\ide -> ide{workspInfoCache =
                                            Map.insert pi packageMap workspInfoCache'})
            continuation True $ (PackageDescr {
                pdPackage        = pi,
                pdMbSourcePath   = Just $ ipdCabalFile idePack,
                pdModules        = moduleDescrs,
                pdBuildDepends   = buildDepends}))
    where
        basePath =  normalise $ (takeDirectory (ipdCabalFile idePack))
        srcDirs' bi =  map (basePath </>) (hsSourceDirs bi)
        pi = ipdPackageId idePack

figureOutRealSources :: IDEPackage -> [(ModuleName,FilePath)] -> IO [(ModuleName,FilePath)]
figureOutRealSources idePack modWithSources = do
    collectorPath <- getCollectorPath
    let packageCollectorPath = collectorPath </> packageIdentifierToString (ipdPackageId idePack)
    filterM (ff packageCollectorPath) modWithSources
    where
        ff packageCollectorPath (md ,fp) =  do
                let modId = display md
                let collectorModulePath = packageCollectorPath </> modId <.> leksahMetadataWorkspaceFileExtension
                existCollectorFile <- doesFileExist collectorModulePath
                existSourceFile    <- doesFileExist fp
                if (not existSourceFile)
                    then return True -- Maybe with preprocessing
                    else if not existCollectorFile
                        then return True
                        else do
                            sourceModTime <-  getModificationTime fp
                            collModTime   <-  getModificationTime collectorModulePath
                            return (sourceModTime > collModTime)


getModuleDescr :: FilePath
    -> ([ModuleDescr],ModuleDescrCache,Bool,[ModuleName])
    -> (ModuleName, Maybe FilePath)
    -> IO ([ModuleDescr],ModuleDescrCache,Bool,[ModuleName])
getModuleDescr packageCollectorPath (modDescrs,packageMap,changed,problemMods) (modName,mbFilePath) =
    case modName `Map.lookup` packageMap of
        Just (eTime,mbFp,mdescr) -> do
            existMetadataFile <- doesFileExist moduleCollectorPath
            if existMetadataFile
                then do
                    modificationTime <- liftIO $ getModificationTime moduleCollectorPath
                    if modificationTime == eTime
                        then return (mdescr:modDescrs,packageMap,changed,problemMods)
                        else do
                            mbNewDescr  <- trace ("loadInfo: " ++ display modName) $
                                                loadInfosForModule moduleCollectorPath
                            case mbNewDescr of
                                Just newDescr -> return (newDescr:modDescrs,
                                                    Map.insert modName (modificationTime,mbFilePath,newDescr) packageMap,
                                                    True, problemMods)
                                Nothing       -> return (mdescr:modDescrs,packageMap,changed,
                                                    modName : problemMods)
                else return (mdescr:modDescrs,packageMap,changed, modName : problemMods)
        Nothing -> do
            existMetadataFile <- doesFileExist moduleCollectorPath
            if existMetadataFile
                then do
                    modificationTime <- liftIO $ getModificationTime moduleCollectorPath
                    mbNewDescr       <- loadInfosForModule moduleCollectorPath
                    case mbNewDescr of
                        Just newDescr -> return (newDescr:modDescrs,
                                    Map.insert modName (modificationTime,mbFilePath,newDescr) packageMap,
                                        True, problemMods)
                        Nothing       -> return (modDescrs,packageMap,changed,
                                        modName : problemMods)
                else return (modDescrs,packageMap,changed, modName : problemMods)
    where
        moduleCollectorPath = packageCollectorPath </> display modName <.>  leksahMetadataWorkspaceFileExtension

-- ---------------------------------------------------------------------
-- Low level helpers for loading metadata
--

--
-- | Loads the infos for the given packages
--
loadInfosForPackage :: FilePath -> PackageIdentifier -> IO (Maybe PackageDescr)
loadInfosForPackage dirPath pid = do
    let filePath = dirPath </> packageIdentifierToString pid ++ leksahMetadataSystemFileExtension
    let filePath2 = dirPath </> packageIdentifierToString pid ++ leksahMetadataPathFileExtension
    exists <- doesFileExist filePath
    if exists
        then catch (do
            file            <-  openBinaryFile filePath ReadMode
            trace ("now loading metadata for package " ++ packageIdentifierToString pid) return ()
            bs              <-  BSL.hGetContents file
            let (metadataVersion'::Integer, packageInfo::PackageDescr) = decodeSer bs
            if metadataVersion /= metadataVersion'
                then do
                    hClose file
                    throwIDE ("Metadata has a wrong version."
                            ++  " Consider rebuilding metadata with: leksah-server -osb +RTS -N2 -RTS")
                else do
                    packageInfo `deepseq` (hClose file)
                    exists'  <-  doesFileExist filePath2
                    sourcePath <- if exists'
                                    then liftM Just (readFile filePath2)
                                    else return Nothing
                    let packageInfo' = injectSourceInPack sourcePath packageInfo
                    return (Just packageInfo'))
            (\ (e :: SomeException) -> do
                sysMessage Normal
                    ("loadInfosForPackage: " ++ packageIdentifierToString pid ++ " Exception: " ++ show e)
                return Nothing)
        else do
            sysMessage Normal $"packageInfo not found for " ++ packageIdentifierToString pid
            return Nothing

injectSourceInPack :: Maybe FilePath -> PackageDescr -> PackageDescr
injectSourceInPack Nothing pd = pd{
    pdMbSourcePath = Nothing,
    pdModules      = map (injectSourceInMod Nothing) (pdModules pd)}
injectSourceInPack (Just pp) pd = pd{
    pdMbSourcePath = (Just pp),
    pdModules      = map (injectSourceInMod (Just (dropFileName pp))) (pdModules pd)}

injectSourceInMod :: Maybe FilePath -> ModuleDescr -> ModuleDescr
injectSourceInMod Nothing md = md{mdMbSourcePath = Nothing}
injectSourceInMod (Just bp) md =
    case mdMbSourcePath md of
        Just sp -> md{mdMbSourcePath = Just (bp </> sp)}
        Nothing -> md

--
-- | Loads the infos for the given module
--
loadInfosForModule :: FilePath -> IO (Maybe ModuleDescr)
loadInfosForModule filePath  = do
    exists <- doesFileExist filePath
    if exists
        then catch (do
            file            <-  openBinaryFile filePath ReadMode
            bs              <-  BSL.hGetContents file
            let (metadataVersion'::Integer, moduleInfo::ModuleDescr) = decodeSer bs
            if metadataVersion /= metadataVersion'
                then do
                    hClose file
                    throwIDE ("Metadata has a wrong version."
                            ++  " Consider rebuilding metadata with -r option")
                else do
                    moduleInfo `deepseq` (hClose file)
                    return (Just moduleInfo))
            (\ (e :: SomeException) -> do sysMessage Normal ("loadInfosForModule: " ++ show e); return Nothing)
        else do
            sysMessage Normal $"moduleInfo not found for " ++ filePath
            return Nothing

findFittingPackages :: [Dependency] -> IO [PackageIdentifier]
findFittingPackages dependencyList = do
    knownPackages   <-  getInstalledPackageIds
    return (concatMap (fittingKnown knownPackages) dependencyList)
    where
    fittingKnown packages (Dependency dname versionRange) =
        let filtered =  filter (\ (PackageIdentifier name version) ->
                                    name == dname && withinRange version versionRange)
                        packages
        in  if length filtered > 1
                then [maximumBy (\a b -> compare (pkgVersion a) (pkgVersion b)) filtered]
                else filtered

-- ---------------------------------------------------------------------
-- Looking up and searching metadata
--

getActivePackageDescr :: IDEM (Maybe PackageDescr)
getActivePackageDescr = do
    mbActive <- readIDE activePack
    case mbActive of
        Nothing -> return Nothing
        Just pack -> do
            packageInfo' <- getPackageInfo
            case packageInfo' of
                Nothing -> return Nothing
                Just (GenScopeC (PackScope map _),(GenScopeC (PackScope _ _))) ->
                    return (ipdPackageId pack `Map.lookup` map)

--
-- | Lookup of an identifier description
--
getIdentifierDescr :: (SymbolTable alpha, SymbolTable beta)  => String -> alpha   -> beta   -> [Descr]
getIdentifierDescr str st1 st2 =
    let r1 = str `symLookup` st1
        r2 = str `symLookup` st2
    in r1 ++ r2

--
-- | Lookup of an identifiers starting with the specified prefix and return a list.
--
getIdentifiersStartingWith :: (SymbolTable alpha , SymbolTable beta)  => String -> alpha   -> beta   -> [String]
getIdentifiersStartingWith prefix st1 st2 =
    takeWhile (isPrefixOf prefix) $
        if memberLocal || memberGlobal then
            prefix : Set.toAscList names
            else
            Set.toAscList names
    where
        (_, memberLocal, localNames) = Set.splitMember prefix (symbols st1)
        (_, memberGlobal, globalNames) = Set.splitMember prefix (symbols st2)
        names = Set.union globalNames localNames

getCompletionOptions :: String -> IDEM [String]
getCompletionOptions prefix = do
    workspaceInfo' <- getWorkspaceInfo
    case workspaceInfo' of
        Nothing -> return []
        Just ((GenScopeC (PackScope _ symbolTable1)),(GenScopeC (PackScope _ symbolTable2))) ->
            return $ getIdentifiersStartingWith prefix symbolTable1 symbolTable2

getDescription :: String -> IDEM String
getDescription name = do
    workspaceInfo' <- getWorkspaceInfo
    case workspaceInfo' of
        Nothing -> return ""
        Just ((GenScopeC (PackScope _ symbolTable1)),(GenScopeC (PackScope _ symbolTable2))) ->
            return ((foldr (\d f -> shows (Present d) .  showChar '\n' . f) id
                (getIdentifierDescr name symbolTable1 symbolTable2)) "")

getPackageInfo :: IDEM (Maybe (GenScope, GenScope))
getPackageInfo   =  readIDE packageInfo

getWorkspaceInfo :: IDEM (Maybe (GenScope, GenScope))
getWorkspaceInfo =  readIDE workspaceInfo

getSystemInfo :: IDEM (Maybe GenScope)
getSystemInfo    =  readIDE systemInfo

-- | Only exported items
getPackageImportInfo :: IDEPackage -> IDEM (Maybe (GenScope,GenScope))
getPackageImportInfo idePack = do
    mbActivePack  <- readIDE activePack
    systemInfo'   <- getSystemInfo
    if isJust mbActivePack && ipdPackageId (fromJust mbActivePack) == ipdPackageId idePack
        then do
            packageInfo' <- getPackageInfo
            case packageInfo' of
                Nothing -> trace "getPackageImportInfo: no package info" $ return Nothing
                Just ((GenScopeC (PackScope pdmap _)),_) -> do
                     case Map.lookup (ipdPackageId idePack) pdmap of
                        Nothing -> trace "getPackageImportInfo: package not found in package" $ return Nothing
                        Just pd -> buildIt pd systemInfo'
        else do
            workspaceInfo <- getWorkspaceInfo
            case workspaceInfo of
                Nothing -> trace "getPackageImportInfo: no workspace info" $ return Nothing
                Just ((GenScopeC (PackScope pdmap _)),_) ->
                    case Map.lookup (ipdPackageId idePack) pdmap of
                        Nothing -> trace "getPackageImportInfo: package not found in workspace" $ return Nothing
                        Just pd -> buildIt pd systemInfo'

    where
        filterPrivate :: ModuleDescr -> ModuleDescr
        filterPrivate md = md{mdIdDescriptions = filter dscExported (mdIdDescriptions md)}
        buildIt pd systemInfo' =
                case systemInfo' of
                    Nothing -> trace "getPackageImportInfo: no system info" $ return Nothing
                    Just (GenScopeC (PackScope pdmap' _)) ->
                        let impPackDescrs = catMaybes $ map (\ pid -> pid `Map.lookup` pdmap')
                                                (pdBuildDepends pd)
                            pd' = pd{pdModules = map filterPrivate (pdModules pd)}
                            scope1 :: PackScope (Map String [Descr])
                                            =   buildScope pd (PackScope Map.empty symEmpty)
                            scope2 :: PackScope (Map String [Descr])
                                =   foldr buildScope (PackScope Map.empty symEmpty) impPackDescrs
                        in return (Just (GenScopeC scope1, GenScopeC scope2))
--
-- | Searching of metadata
--

searchMeta :: Scope -> String -> SearchMode -> IDEM [Descr]
searchMeta _ "" _ = return []
searchMeta (PackageScope False) searchString searchType = do
    packageInfo'    <- getPackageInfo
    case packageInfo' of
        Nothing    -> return []
        Just ((GenScopeC (PackScope _ rl)),_) -> return (searchInScope searchType searchString rl)
searchMeta (PackageScope True) searchString searchType = do
    packageInfo'    <- getPackageInfo
    case packageInfo' of
        Nothing    -> return []
        Just ((GenScopeC (PackScope _ rl)),(GenScopeC (PackScope _ rr))) ->
            return (searchInScope searchType searchString rl
                                ++  searchInScope searchType searchString rr)
searchMeta (WorkspaceScope False) searchString searchType = do
    workspaceInfo'    <- getWorkspaceInfo
    case workspaceInfo' of
        Nothing    -> return []
        Just ((GenScopeC (PackScope _ rl)),_) -> return (searchInScope searchType searchString rl)
searchMeta (WorkspaceScope True) searchString searchType = do
    workspaceInfo'    <- getWorkspaceInfo
    case workspaceInfo' of
        Nothing    -> return []
        Just ((GenScopeC (PackScope _ rl)),(GenScopeC (PackScope _ rr))) ->
            return (searchInScope searchType searchString rl
                                ++  searchInScope searchType searchString rr)
searchMeta SystemScope searchString searchType = do
    systemInfo'  <- getSystemInfo
    packageInfo' <- getPackageInfo
    case systemInfo' of
        Nothing ->
            case packageInfo' of
                        Nothing    -> return []
                        Just ((GenScopeC (PackScope _ rl)),_) ->
                                return (searchInScope searchType searchString rl)
        Just (GenScopeC (PackScope _ s)) ->
            case packageInfo' of
                Nothing    -> return (searchInScope searchType searchString s)
                Just ((GenScopeC (PackScope _ rl)),_) -> return (searchInScope searchType searchString rl
                                        ++  searchInScope searchType searchString s)

searchInScope :: SymbolTable alpha =>  SearchMode -> String -> alpha  -> [Descr]
searchInScope (Exact _)  l st      = searchInScopeExact l st
searchInScope (Prefix True) l st   = (concat . symElems) (searchInScopePrefix l st)
searchInScope (Prefix False) [] _  = []
searchInScope (Prefix False) l st  = (concat . symElems) (searchInScopeCaseIns l st "")
searchInScope (Regex b) l st       = searchRegex l st b


searchInScopeExact :: SymbolTable alpha =>  String -> alpha  -> [Descr]
searchInScopeExact = symLookup

searchInScopePrefix :: SymbolTable alpha   =>  String -> alpha  -> alpha
searchInScopePrefix searchString symbolTable =
    let (_, exact, mapR)   = symSplitLookup searchString symbolTable
        (mbL, _, _)        = symSplitLookup (searchString ++ "{") mapR
    in case exact of
            Nothing -> mbL
            Just e  -> symInsert searchString e mbL

searchInScopeCaseIns :: SymbolTable alpha => String -> alpha -> String -> alpha
searchInScopeCaseIns [] st _                    =  st
searchInScopeCaseIns (a:l)  st pre | isLower a  =
    let s1 = pre ++ [a]
        s2 = pre ++ [toUpper a]
    in  (symUnion (searchInScopeCaseIns l (searchInScopePrefix s1 st) s1)
                   (searchInScopeCaseIns l (searchInScopePrefix s2 st) s2))
                                   | isUpper a  =
    let s1 = pre ++ [a]
        s2 = pre ++ [toLower a]
    in  (symUnion (searchInScopeCaseIns l (searchInScopePrefix s1 st) s1)
                   (searchInScopeCaseIns l (searchInScopePrefix s2 st) s2))
                                    | otherwise =
    let s =  pre ++ [a]
    in searchInScopeCaseIns l (searchInScopePrefix s st) s


searchRegex :: SymbolTable alpha => String -> alpha  -> Bool -> [Descr]
searchRegex searchString st caseSense =
    case compileRegex caseSense searchString of
        Left err ->
            unsafePerformIO $ sysMessage Normal (show err) >> return []
        Right regex ->
            filter (\e -> do
                case execute regex (dscName e) of
                    Left e        -> False
                    Right Nothing -> False
                    _             -> True)
                        (concat (symElems st))

compileRegex :: Bool -> String -> Either String Regex
compileRegex caseSense searchString =
    let compOption = defaultCompOpt {
                            Regex.caseSensitive = caseSense
                        ,   multiline = True } in
    compile compOption defaultExecOpt searchString

-- ---------------------------------------------------------------------
-- Handling of scopes
--

--
-- | Loads the infos for the given packages (has an collecting argument)
--
buildScope :: SymbolTable alpha  =>  PackageDescr -> PackScope alpha  -> PackScope alpha
buildScope packageD (PackScope packageMap symbolTable) =
    let pid = pdPackage packageD
    in if pid `Map.member` packageMap
        then (PackScope packageMap symbolTable)
        else (PackScope (Map.insert pid packageD packageMap)
                  (buildSymbolTable packageD symbolTable))

buildSymbolTable :: SymbolTable alpha  =>  PackageDescr -> alpha  -> alpha
buildSymbolTable pDescr symbolTable =
     foldl' buildScope'
            symbolTable allDescriptions
    where
        allDescriptions =  concatMap mdIdDescriptions (pdModules pDescr)
        buildScope' st idDescr =
            let allDescrs = allDescrsFrom idDescr
            in  foldl' (\ map descr -> symInsert (dscName descr) [descr] map)
                        st allDescrs
        allDescrsFrom descr | isReexported descr = [descr]
                            | otherwise =
            case dscTypeHint descr of
                DataDescr constructors fields ->
                    descr : (map (\(SimpleDescr fn ty loc comm exp) ->
                        Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                            dscMbModu' = dscMbModu descr, dscMbLocation' = loc,
                            dscMbComment' = comm, dscTypeHint' = FieldDescr descr, dscExported' = exp})
                            fields)
                            ++  (map (\(SimpleDescr fn ty loc comm exp) ->
                        Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                            dscMbModu' = dscMbModu descr, dscMbLocation' = loc,
                            dscMbComment' = comm, dscTypeHint' = ConstructorDescr descr, dscExported' = exp})
                                constructors)
                ClassDescr _ methods ->
                    descr : (map (\(SimpleDescr fn ty loc comm exp) ->
                        Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                            dscMbModu' = dscMbModu descr, dscMbLocation' = loc,
                            dscMbComment' = comm, dscTypeHint' = MethodDescr descr, dscExported' = exp})
                            methods)
                NewtypeDescr (SimpleDescr fn ty loc comm exp) mbField ->
                    descr : (Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                            dscMbModu' = dscMbModu descr, dscMbLocation' = loc,
                            dscMbComment' = comm, dscTypeHint' = ConstructorDescr descr, dscExported' = exp})
                             : case mbField of
                                    Just (SimpleDescr fn ty loc comm exp) ->
                                        [Real $ RealDescr{dscName' = fn, dscMbTypeStr' = ty,
                                        dscMbModu' = dscMbModu descr, dscMbLocation' = loc,
                                        dscMbComment' = comm, dscTypeHint' = FieldDescr descr, dscExported' = exp}]
                                    Nothing -> []
                InstanceDescr _ -> []
                _ -> [descr]


-- ---------------------------------------------------------------------
-- Low level functions for calling the collector
--

callCollector :: Bool -> Bool -> Bool -> (Bool -> IDEAction) -> IDEAction
callCollector rebuild sources extract cont = trace "callCollector" $ do
    doServerCommand command $ \ res ->
        case res of
            ServerOK         -> trace "callCollector finished" $ cont True
            ServerFailed str -> trace str $ cont False
            _                -> trace "impossible server answer" $ cont False
    where command = SystemCommand {
            scRebuild = rebuild,
            scSources = sources,
            scExtract = extract}

callCollectorWorkspace :: Bool -> FilePath -> PackageIdentifier -> [(String,FilePath)] ->
    (Bool -> IDEAction) -> IDEAction
callCollectorWorkspace rebuild fp  pi modList cont = trace "callCollectorWorkspace" $
    if null modList
        then trace "callCollectorWorkspace: Nothing to do" $ cont True
        else do
            doServerCommand command  $ \ res ->
                case res of
                    ServerOK         -> trace "callCollectorWorkspace finished" $ cont True
                    ServerFailed str -> trace str $ cont False
                    _                -> trace "impossible server answer" $ cont False
    where command = WorkspaceCommand {
            wcRebuild = rebuild,
            wcPackage = pi,
            wcPath    = fp,
            wcModList = modList}

-- ---------------------------------------------------------------------
-- Additions for completion
--

keywords :: [String]
keywords = [
        "as"
    ,   "case"
    ,   "of"
    ,   "class"
    ,   "data"
    ,   "default"
    ,   "deriving"
    ,   "do"
    ,   "forall"
    ,   "foreign"
    ,   "hiding"
    ,   "if"
    ,   "then"
    ,   "else"
    ,   "import"
    ,   "infix"
    ,   "infixl"
    ,   "infixr"
    ,   "instance"
    ,   "let"
    ,   "in"
    ,   "mdo"
    ,   "module"
    ,   "newtype"
    ,   "qualified"
    ,   "type"
    ,   "where"]

keywordDescrs :: [Descr]
keywordDescrs = map (\s -> Real $ RealDescr
                                s
                                Nothing
                                Nothing
                                Nothing
                                (Just (BS.pack " Haskell keyword"))
                                KeywordDescr
                                True) keywords

extensionDescrs :: [Descr]
extensionDescrs =  map (\ext -> Real $ RealDescr
                                    ("X" ++ show ext)
                                    Nothing
                                    Nothing
                                    Nothing
                                    (Just (BS.pack " Haskell language extension"))
                                    ExtensionDescr
                                    True)
#if MIN_VERSION_Cabal(1,16,0)
                                ([minBound..maxBound]::[KnownExtension])
#else
                                knownExtensions
#endif

moduleNameDescrs :: PackageDescr -> [Descr]
moduleNameDescrs pd = map (\md -> Real $ RealDescr
                                    ((display . modu . mdModuleId) md)
                                    Nothing
                                    (Just (mdModuleId md))
                                    Nothing
                                    (Just (BS.pack " Module name"))
                                    ModNameDescr
                                    True) (pdModules pd)

addOtherToScope ::  SymbolTable alpha  =>  PackScope alpha -> Bool -> PackScope alpha
addOtherToScope (PackScope packageMap symbolTable) addAll = (PackScope packageMap newSymbolTable)
    where newSymbolTable = foldl' (\ map descr -> symInsert (dscName descr) [descr] map)
                        symbolTable (if addAll
                                        then keywordDescrs ++ extensionDescrs ++ modNameDescrs
                                        else modNameDescrs)
          modNameDescrs = concatMap moduleNameDescrs (Map.elems packageMap)

