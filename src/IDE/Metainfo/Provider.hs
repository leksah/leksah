{-# OPTIONS_GHC -XTypeSynonymInstances -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.Provider
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | This module provides the infos collected by the extractor before
--   and an info pane to present some of them to the user
--
---------------------------------------------------------------------------------

module IDE.Metainfo.Provider (
    getIdentifierDescr
,   getIdentifiersStartingWith
,   getCompletionOptions
,   getDescription

,   initInfo
,   updateAccessibleInfo
,   infoForActivePackage
,   mayRebuildInBackground
,   rebuildActiveInfo
,   searchMeta
,   rebuildLibInfo
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
import Data.List
import qualified PackageConfig as DP
import Data.Maybe
import Data.Binary
import Distribution.Package hiding (depends,packageId)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import System.Process (ProcessHandle(..),getProcessExitCode)
import System.Glib.MainLoop(timeoutAddFull,priorityDefaultIdle)
import Control.Monad.Reader(ask)
import Distribution.Version
import Distribution.ModuleName
import GHC (runGhc)

import DeepSeq
import IDE.FileUtils
import IDE.Core.State
import IDE.Metainfo.InterfaceCollector
import Control.Event
import Data.Char (toLower,isUpper,toUpper,isLower)
import Text.Regex.Posix
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Posix.String (execute,compile)
import IDE.Metainfo.GHCUtils (findFittingPackages,getInstalledPackageInfos,inGhc)
--import Debug.Trace

--
-- | Lookup of an identifier description
--
getIdentifierDescr :: String -> SymbolTable -> SymbolTable -> [Descr]
getIdentifierDescr str st1 st2 =
    let r1 = case str `Map.lookup` st1 of
                Nothing -> []
                Just r -> r
        r2 = case str `Map.lookup` st2 of
                Nothing -> []
                Just r -> r
    in r1 ++ r2

--
-- | Lookup of an identifiers starting with the specified prefix and return a list.
--
getIdentifiersStartingWith :: String -> SymbolTable -> SymbolTable -> [String]
getIdentifiersStartingWith prefix st1 st2 =
    takeWhile (isPrefixOf prefix) $
        if memberLocal || memberGlobal then
            prefix : Set.toAscList names
            else
            Set.toAscList names
    where
        (_, memberLocal, localNames) = Set.splitMember prefix (Map.keysSet st1)
        (_, memberGlobal, globalNames) = Set.splitMember prefix (Map.keysSet st2)
        names = Set.union globalNames localNames

getCompletionOptions :: String -> IDEM [String]
getCompletionOptions prefix = do
    currentInfo' <- readIDE currentInfo
    case currentInfo' of
        Nothing -> return []
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            return $ getIdentifiersStartingWith prefix symbolTable1 symbolTable2

getDescription :: String -> IDEM String
getDescription name = do
    currentInfo' <- readIDE currentInfo
    case currentInfo' of
        Nothing -> return ""
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            return $ foldl (\result description ->
                result
                ++ case description of
                    Descr _ _ _ _ _ _ ->
                        "-- " ++ showPackModule (descrModu description) ++ "\n"
                        ++ case mbComment description of
                            Just comment -> unlines $ map ((++) "-- ") $ nonemptyLines (BS.unpack comment)
                            Nothing -> ""
                        ++ (unlines $ nonemptyLines (BS.unpack $ typeInfo description))
                        ++ "\n"
                    _ -> ""
                ) "" $ getIdentifierDescr name symbolTable1 symbolTable2
    where
        nonemptyLines text = filter (\line -> isJust $ find notSpace line) $ lines text
        notSpace char = char /= ' ' && char /= '\t'

--
-- | Searching of metadata
--
searchMeta :: Scope -> String -> SearchMode -> IDEM [Descr]
searchMeta Local searchString searchType = do
    currentInfo'    <- readIDE currentInfo
    case currentInfo' of
        Nothing    -> return []
        Just (l,_) -> return (searchInScope searchType searchString (snd l))
searchMeta Package searchString searchType = do
    currentInfo'    <- readIDE currentInfo
    case currentInfo' of
        Nothing    -> return []
        Just (l,p) -> return (searchInScope searchType searchString (snd l)
                                ++  searchInScope searchType searchString (snd p))
searchMeta System searchString searchType = do
    accessibleInfo' <- readIDE accessibleInfo
    let s = case accessibleInfo' of
                Nothing        -> Map.empty
                Just (_,scope) -> scope
    currentInfo'    <- readIDE currentInfo
    case currentInfo' of
        Nothing    -> return (searchInScope searchType searchString s)
        Just (l,_) -> return (searchInScope searchType searchString (snd l)
                                ++  searchInScope searchType searchString s)


searchInScope :: SearchMode -> String -> SymbolTable -> [Descr]
searchInScope (Exact _)  l st      = searchInScopeExact l st
searchInScope (Prefix True) l st   = (concat . Map.elems) (searchInScopePrefix l st)
searchInScope (Prefix False) [] _  = []
searchInScope (Prefix False) l st  = (concat . Map.elems) (searchInScopeCaseIns l st "")
searchInScope (Regex b) l st       = searchRegex l st b


searchInScopeExact :: String -> SymbolTable -> [Descr]
searchInScopeExact searchString symbolTable =
    case Map.lookup searchString symbolTable of
        Nothing -> []
        Just l  -> l

searchInScopePrefix :: String -> SymbolTable -> SymbolTable
searchInScopePrefix searchString symbolTable =
    let (_, exact, mapR)   = Map.splitLookup searchString symbolTable
        (mbL, _, _)        = Map.splitLookup (searchString ++ "{") mapR
    in case exact of
            Nothing -> mbL
            Just e  -> Map.insert searchString e mbL

searchInScopeCaseIns :: String -> SymbolTable -> String -> SymbolTable
searchInScopeCaseIns [] st _                    =  st
searchInScopeCaseIns (a:l)  st pre | isLower a  =
    let s1 = pre ++ [a]
        s2 = pre ++ [toUpper a]
    in  (Map.union (searchInScopeCaseIns l (searchInScopePrefix s1 st) s1)
                   (searchInScopeCaseIns l (searchInScopePrefix s2 st) s2))
                                   | isUpper a  =
    let s1 = pre ++ [a]
        s2 = pre ++ [toLower a]
    in  (Map.union (searchInScopeCaseIns l (searchInScopePrefix s1 st) s1)
                   (searchInScopeCaseIns l (searchInScopePrefix s2 st) s2))
                                    | otherwise =
    let s =  pre ++ [a]
    in searchInScopeCaseIns l (searchInScopePrefix s st) s

searchRegex :: String -> SymbolTable -> Bool -> [Descr]
searchRegex searchString st caseSense =
    unsafePerformIO $ do
        res <- compile (if caseSense then compBlank else compIgnoreCase)
                    execBlank searchString
        case res of
            Left err -> do
                sysMessage Normal (show err)
                return []
            Right regex ->
                filterM (\e -> do
                    res <- execute regex (descrName e)
                    case res of
                        Left e        -> return False
                        Right Nothing -> return False
                        _             -> return True)
                            (concat (Map.elems st))

--
-- | Update and initialize metadata for the world
--
initInfo :: IDEAction
initInfo = do
    prefs           <- readIDE prefs
    when (collectAtStart prefs) $ do
        ideMessage Normal "Now updating metadata ..."
        collectInstalled prefs False
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
        collectorPath   <-  lift $ getCollectorPath version
        packageInfos    <-  inGhc $ getInstalledPackageInfos
 --       trace ("allInfos " ++ show (map (DP.unpackPackageId . DP.packageConfigId) packageInfos)) $ return ()
        packageList     <-  liftIO $ mapM (loadInfosForPackage collectorPath False)
                                                    (map DP.package packageInfos)
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
    ideR <- ask
    triggerEvent ideR CurrentInfo
    return ()

--
-- | Set info for package
--
infoForActivePackage :: IDEAction
infoForActivePackage  = do
    activePack          <-  readIDE activePack
    case activePack of
        Nothing         ->  do
            modifyIDE_ (\ide -> return (ide{currentInfo = Nothing}))
        Just pack       ->  do
            let depends'         =  depends pack
            fp                  <-  inGhc $ findFittingPackages depends'
            mbActive            <-  loadOrBuildActiveInfo
            case mbActive of
                Nothing         ->  do
                    -- no meta info available
                    modifyIDE_ (\ide -> return (ide{currentInfo = Nothing}))
                Just active     ->  do

                    accessibleInfo'     <-  readIDE accessibleInfo
                    case accessibleInfo' of
                        Nothing         ->  modifyIDE_ (\ide -> return (ide{currentInfo = Nothing}))
                        Just (pdmap,_)  ->  do
                            let packageList =   map (\ pin -> pin `Map.lookup` pdmap) fp
                            let scope       =   foldr buildScope (Map.empty,Map.empty)
                                                    $ map fromJust
                                                        $ filter isJust packageList
                            modifyIDE_ (\ide -> return (ide{currentInfo = Just (active, scope)}))
    ideR <- ask
    triggerEvent ideR CurrentInfo
    return ()


--
-- | Builds the current info for the activePackage in the background
--   If a process handle is given, it waits for this process to finish before building
--
mayRebuildInBackground :: Maybe ProcessHandle -> IDEAction
mayRebuildInBackground mbHandle = reifyIDE $ \ ideR  ->
    let
        myRebuild :: IO Bool
        myRebuild =
            case mbHandle of
                Just hdl -> do
                    mbExitCode  <-  getProcessExitCode hdl
                    case mbExitCode of
                        --process not finished, wait for next call
                        Nothing ->  return True
                        Just _  ->  doIt
                Nothing -> doIt
        doIt :: IO Bool
        doIt = do
                reflectIDE (do
                    errs <- readIDE errors
                    when (length (filter isError errs) == 0) $ do
                        ideMessage Normal "Update meta info for active package"
                        setInfo buildActiveInfo) ideR
                return False
    in do
        timeoutAddFull myRebuild priorityDefaultIdle 500
        return ()

rebuildActiveInfo :: IDEAction
rebuildActiveInfo       =   setInfo buildActiveInfo

--
-- | Builds the current info for the activePackage
--
setInfo :: IDEM (Maybe PackageScope) -> IDEAction
setInfo f = do
    currentInfo         <-  readIDE currentInfo
    case currentInfo of
        Nothing                 -> return ()
        Just (active, scope)    -> do
            newActive   <-  f
            case newActive of
                Nothing         -> return ()
                Just newActive  -> do
                    modifyIDE_ (\ide -> return (ide{currentInfo = Just (newActive, scope)}))
                    ideR <- ask
                    triggerEvent ideR CurrentInfo
                    return ()

--
-- | Loads the current info for the activePackage, or builds it if not available
--
loadOrBuildActiveInfo :: IDEM (Maybe PackageScope)
loadOrBuildActiveInfo = do
    mbActiveInfo        <-  loadActiveInfo
    case mbActiveInfo of
        Just ai         ->  return (Just ai)
        Nothing         ->  buildActiveInfo

--
-- | Loads the current info for the activePackage
--
loadActiveInfo :: IDEM (Maybe PackageScope)
loadActiveInfo =
    let version         =   cProjectVersion in do
    activePack          <-  readIDE activePack
    case activePack of
        Nothing         ->  return Nothing
        Just idePackage ->  do
            collectorPath   <-  lift $ getCollectorPath cProjectVersion
            packageDescr    <-  liftIO $ loadInfosForPackage collectorPath True
                                            (packageId idePackage)
            case packageDescr of
                Nothing     -> return Nothing
                Just pd     -> do
                    let scope       =   buildScope pd (Map.empty,Map.empty)
                    return (Just scope)

--
-- | Builds the current info for the activePackage
--
buildActiveInfo :: IDEM (Maybe PackageScope)
buildActiveInfo =
    let version         =   cProjectVersion in do
    activePack          <-  readIDE activePack
    case activePack of
        Nothing         ->  return Nothing
        Just idePackage ->  do
            libDir          <-   liftIO $ getSysLibDir
            liftIO $ runGhc (Just libDir)
                        $ collectUninstalled False cProjectVersion (takeFileName $ cabalFile idePackage)
            -- ideMessage Normal "uninstalled collected"
            collectorPath   <-  lift $ getCollectorPath cProjectVersion
            packageDescr    <-  liftIO $ loadInfosForPackage collectorPath True
                                            (packageId idePackage)
            case packageDescr of
                Nothing     -> return Nothing
                Just pd     -> do
                    let scope       =   buildScope pd (Map.empty,Map.empty)
                    return (Just scope)

rebuildLibInfo :: IDEAction
rebuildLibInfo = do
    updateAccessibleInfo
    rebuildActiveInfo

--
-- | Updates the world info (it is the responsibility of the caller to rebuild
--   the current info)
--
updateAccessibleInfo :: IDEAction
updateAccessibleInfo = do
    wi              <-  readIDE accessibleInfo
    let version     =   cProjectVersion
    case wi of
        Nothing -> loadAccessibleInfo
        Just (psmap,psst) -> do
            packageInfos        <-  inGhc getInstalledPackageInfos
            let packageIds      =   map DP.package packageInfos
            let newPackages     =   filter (\ pi -> Map.member pi psmap) packageIds
            let trashPackages   =   filter (\ e  -> not (elem e packageIds))(Map.keys psmap)
            if null newPackages && null trashPackages
                then return ()
                else do
                    collectorPath   <-  lift $ getCollectorPath version
                    newPackageInfos <-  liftIO $ mapM (\pid -> loadInfosForPackage collectorPath False pid)
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
loadInfosForPackage :: FilePath -> Bool -> PackageIdentifier -> IO (Maybe PackageDescr)
loadInfosForPackage dirPath isWorkingPackage pid = do
    let filePath = dirPath </> fromPackageIdentifier pid ++ (if isWorkingPackage then ".packw" else ".pack")
    exists <- doesFileExist filePath
    if exists
        then catch (do
            file            <-  openBinaryFile filePath ReadMode
            bs              <-  BSL.hGetContents file
            let (metadataVersion', packageInfo) =   decode bs
            if metadataVersion /= metadataVersion'
                then do
                    hClose file
                    throwIDE ("Metadata has a wrong version."
                            ++  " Consider rebuilding metadata with -r option")
                else do
                    packageInfo `deepSeq` (hClose file)
                    return (Just packageInfo))
            (\e -> do sysMessage Normal (show e); return Nothing)
        else do
            sysMessage Normal $"packageInfo not found for " ++ fromPackageIdentifier pid
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
     foldl' buildScope symbolTable allDescriptions
    where
        allDescriptions =  concatMap idDescriptionsMD (exposedModulesPD pDescr)
        buildScope st idDescr =
            let allDescrs = allDescrsFrom idDescr
            in  foldl' (\ map descr -> Map.insertWith (++) (descrName descr) [descr] map)
                        st allDescrs
        allDescrsFrom descr | isReexported descr = [descr]
                            | otherwise =
            case details descr of
                DataDescr constructors fields ->
                    descr : (map (\(fn,ty) -> Descr{descrName' = fn, typeInfo' = ty,
                        descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                        mbComment' = mbComment descr, details' = FieldDescr {typeDescrF = descr}})
                            fields
                            ++  (map (\(fn,ty) -> Descr{descrName' = fn, typeInfo' = ty,
                            descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                            mbComment' = mbComment descr, details' = ConstructorDescr {typeDescrC = descr}})
                                constructors))
                ClassDescr _ methods ->
                    descr : (map (\(fn,ty) -> Descr{descrName' = fn, typeInfo' = ty,
                        descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                        mbComment' = mbComment descr, details' = MethodDescr {classDescrM = descr}})
                            methods)
                NewtypeDescr constr mbField ->
                    descr : Descr{descrName' = fst constr, typeInfo' = snd constr,
                            descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                            mbComment' = mbComment descr, details' = ConstructorDescr {typeDescrC = descr}}
                             : case mbField of
                                    Just fld ->
                                        [Descr{descrName' = fst fld, typeInfo' = snd fld,
                                        descrModu' = descrModu descr, mbLocation' = mbLocation descr,
                                        mbComment' = mbComment descr, details' = FieldDescr {typeDescrF = descr}}]
                                    Nothing -> []
                InstanceDescr _ -> []
                _ -> [descr]



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

instance DeepSeq ModuleDescr where
    deepSeq pd =  deepSeq (moduleIdMD pd)
                    $   deepSeq (mbSourcePathMD pd)
                    $   deepSeq (exportedNamesMD pd)
                    $   deepSeq (usagesMD pd)

instance DeepSeq Descr where
    deepSeq (Descr descrName' typeInfo' descrModu'
        mbLocation' mbComment' details')  =  deepSeq descrName'
                    $   deepSeq typeInfo'
                    $   deepSeq descrModu'
                    $   deepSeq mbLocation'
                    $   deepSeq mbComment'
                    $   deepSeq details'
    deepSeq (Reexported reexpModu' impDescr') = deepSeq reexpModu'
                    $   deepSeq impDescr'

instance DeepSeq SpDescr where
    deepSeq (FieldDescr typeDescrF')              =   deepSeq typeDescrF'
    deepSeq (ConstructorDescr typeDescrC')        =  deepSeq typeDescrC'
    deepSeq (DataDescr constructors' fields')     =   deepSeq constructors'
                    $   deepSeq fields'
    deepSeq (NewtypeDescr constructor' mbField')  =   deepSeq constructor'
                    $   deepSeq mbField'
    deepSeq (ClassDescr super' methods')          =   deepSeq super'
                    $   deepSeq methods'
    deepSeq (MethodDescr classDescrM')            =   deepSeq classDescrM'
    deepSeq (InstanceDescr binds')                =   deepSeq binds'
    deepSeq a                                     =   seq a


instance DeepSeq PackageIdentifier where
    deepSeq pd =  deepSeq (pkgName pd)
                    $   deepSeq (pkgVersion pd)

instance DeepSeq alpha  => DeepSeq (Set alpha) where
    deepSeq s =  deepSeq (Set.elems s)

instance (DeepSeq alpha, DeepSeq beta) => DeepSeq (Map alpha beta) where
    deepSeq s =  deepSeq (Map.toList s)

instance DeepSeq DescrType where  deepSeq = seq

instance DeepSeq BS.ByteString where  deepSeq = seq

instance DeepSeq Version where  deepSeq = seq

instance DeepSeq PackModule where
    deepSeq pd =  deepSeq (pack pd)
                    $   deepSeq (modu pd)

instance DeepSeq ModuleName where
    deepSeq =  deepSeq . components

instance DeepSeq PackageName where
    deepSeq (PackageName s) =  deepSeq s

