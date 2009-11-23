{-# OPTIONS_GHC -XTypeSynonymInstances -XScopedTypeVariables #-}
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
-- | This module provides the infos collected by the extractor before
--   and an info pane to present some of them to the user
--
---------------------------------------------------------------------------------

module IDE.Metainfo.Provider (
    getIdentifierDescr
,   getIdentifiersStartingWith
,   getCompletionOptions
,   getDescription
,   getActivePackageDescr
,   searchMeta

,   initInfo
,   rebuildPackageInfo
,   buildSystemInfo
,   updateInfo
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
import Distribution.Package hiding (depends,packageId)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Distribution.Version
import Distribution.ModuleName
import GHC (runGhc)

import Control.DeepSeq
import IDE.FileUtils
import IDE.Core.State
import IDE.Metainfo.InterfaceCollector
import Data.Char (toLower,isUpper,toUpper,isLower)
import Text.Regex.Posix
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.Posix.String (execute,compile)
import IDE.Metainfo.GHCUtils (findFittingPackages,getInstalledPackageInfos,inGhc)
import Data.Binary.Shared (decodeSer)
import System.Mem (performGC)
import Language.Haskell.Extension (knownExtensions)
import Distribution.Text (display)

getActivePackageDescr :: IDEM (Maybe PackageDescr)
getActivePackageDescr = do
    mbActive <- readIDE activePack
    case mbActive of
        Nothing -> return Nothing
        Just pack -> do
            packageInfo' <- readIDE packageInfo
            case packageInfo' of
                Nothing -> return Nothing
                Just ((map,_),(_,_)) -> return (packageId pack `Map.lookup` map)

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
    packageInfo' <- readIDE packageInfo
    case packageInfo' of
        Nothing -> return []
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            return $ getIdentifiersStartingWith prefix symbolTable1 symbolTable2

getDescription :: String -> IDEM String
getDescription name = do
    packageInfo' <- readIDE packageInfo
    case packageInfo' of
        Nothing -> return ""
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            return ((foldr (\d f -> shows (Present d) .  showChar '\n' . f) id
                (getIdentifierDescr name symbolTable1 symbolTable2)) "")

--
-- | Searching of metadata
--

searchMeta :: Scope -> String -> SearchMode -> IDEM [Descr]
searchMeta _ "" _ = return []
searchMeta (PackageScope False) searchString searchType = do
    packageInfo'    <- readIDE packageInfo
    case packageInfo' of
        Nothing    -> return []
        Just (l,_) -> return (searchInScope searchType searchString (snd l))
searchMeta (PackageScope True) searchString searchType = do
    packageInfo'    <- readIDE packageInfo
    case packageInfo' of
        Nothing    -> return []
        Just (l,p) -> return (searchInScope searchType searchString (snd l)
                                ++  searchInScope searchType searchString (snd p))
searchMeta (WorkspaceScope False) searchString searchType = do
    workspaceInfo'    <- readIDE workspaceInfo
    case workspaceInfo' of
        Nothing    -> return []
        Just (l,_) -> return (searchInScope searchType searchString (snd l))
searchMeta (WorkspaceScope True) searchString searchType = do
    workspaceInfo'    <- readIDE workspaceInfo
    case workspaceInfo' of
        Nothing    -> return []
        Just (l,p) -> return (searchInScope searchType searchString (snd l)
                                ++  searchInScope searchType searchString (snd p))
searchMeta SystemScope searchString searchType = do
    systemInfo' <- readIDE systemInfo
    let s = case systemInfo' of
                Nothing        -> Map.empty
                Just (_,scope) -> scope
    packageInfo'    <- readIDE packageInfo
    case packageInfo' of
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
-- | Update and initialize metadata for the world -- Called at startup
--
initInfo :: IDEAction
initInfo = do
    prefs           <- readIDE prefs
    when (collectAtStart prefs) $ do
        ideMessage Normal "Now updating metadata ..."
        collectInstalled prefs False
    ideMessage Normal "Now loading metadata ..."
    loadSystemInfo
    ideMessage Normal "Finished loading ..."

--
-- | Load all infos for all installed and exposed packages
--   (see shell command: ghc-pkg list)
--
loadSystemInfo :: IDEAction
loadSystemInfo =
    let version     =   cProjectVersion in do
        collectorPath   <-  lift $ getCollectorPath version
        packageInfos    <-  inGhc $ getInstalledPackageInfos
        packageList     <-  liftIO $ mapM (loadInfosForPackage collectorPath False)
                                                    (map DP.package packageInfos)
        let scope       =   foldr buildScope (Map.empty,Map.empty)
                                $ map fromJust
                                    $ filter isJust packageList
        liftIO performGC
        modifyIDE_ (\ide -> ide{systemInfo = (Just (addOtherToScope scope True))})
        triggerEventIDE InfoChanged
        return ()

--
-- | Clears the current info, not the world infos
--
clearPackageInfo :: IDEAction
clearPackageInfo = do
    modifyIDE_ (\ide    ->  ide{packageInfo = Nothing})
    infoForWorkspace
    triggerEventIDE InfoChanged
    return ()

updateInfo = do
    infoForActivePackage False
    infoForWorkspace
    triggerEventIDE InfoChanged
    return ()

--
-- | Set info for workspace
--
infoForWorkspace :: IDEAction
infoForWorkspace  = do
    mbWorkspace         <-  readIDE workspace
    systemInfo'         <- readIDE systemInfo
    case (mbWorkspace, systemInfo') of
        (Just ws, Just (pdmap,_))  ->  do
            let depends'         =  nub $ concatMap depends (wsPackages ws)
            importPacks          <- inGhc $ findFittingPackages depends'
            let importPacks'     =  filter (\ i -> isNothing (find (\ e -> packageId e == i) (wsPackages ws)))
                                        importPacks
            packages1   <-  mapM loadOrBuildPackageInfo (wsPackages ws)
            let packages2   =   map (\ pin -> pin `Map.lookup` pdmap) importPacks'
            let scope1      =   foldr buildScope (Map.empty,Map.empty)
                                            $ map fromJust
                                                $ filter isJust packages1
            let scope2      =   foldr buildScope (Map.empty,Map.empty)
                                            $ map fromJust
                                                $ filter isJust packages2
            modifyIDE_ (\ide -> ide{workspaceInfo = Just
                (addOtherToScope scope1 False, addOtherToScope scope2 False)})
        otherwise     ->  modifyIDE_ (\ide -> ide{workspaceInfo = Nothing})
    return ()

--
-- | Set info for package
--
infoForActivePackage :: Bool -> IDEAction
infoForActivePackage rebuild = do
    activePack          <-  readIDE activePack
    systemInfo'     <-  readIDE systemInfo
    case (activePack, systemInfo') of
        (Just pack,Just (pdmap,_)) ->  do
            let depends'         =  depends pack
            packs1               <-  inGhc $ findFittingPackages depends'
            -- we need to sort out current workspace packages and get the 'current' format
            mbWs <- readIDE workspace
            let (opacks,wpacks) =   case mbWs of
                                        Nothing -> (packs1,[])
                                        Just ws -> foldl' (func (wsPackages ws)) ([],[]) packs1
            mbActive            <-  if rebuild
                                        then buildPackageInfo pack
                                        else loadOrBuildPackageInfo pack
            case mbActive of
                Nothing         ->  do
                    modifyIDE_ (\ide -> ide{packageInfo = Nothing})
                Just active     ->  do
                    let packageList      =   map (\ pin -> pin `Map.lookup` pdmap) opacks
                    packages2    <-  mapM loadOrBuildPackageInfo wpacks
                    let scope    =   foldr buildScope (Map.empty,Map.empty)
                                            $ map fromJust
                                                $ filter isJust (packageList ++ packages2)
                    modifyIDE_ (\ide -> ide{packageInfo = Just
                         (addOtherToScope (buildScope active (Map.empty,Map.empty)) True,
                             addOtherToScope scope False)})
        otherwise         ->  modifyIDE_ (\ide -> ide{packageInfo = Nothing})

    where func packages (l,r) pid  =  case find (\ a -> packageId a == pid) packages of
                                Nothing   -> (pid:l,r)
                                Just pack ->  (l,pack:r)

--
-- | Builds the current info for the activePackage
--
rebuildPackageInfo :: IDEAction
rebuildPackageInfo       =   do
    infoForActivePackage True
    infoForWorkspace
    triggerEventIDE InfoChanged
    return ()

--
-- | Builds the current info for the activePackage
--
buildSystemInfo :: IDEAction
buildSystemInfo = do
    updateSystemInfo
    infoForActivePackage True
    infoForWorkspace
    triggerEventIDE InfoChanged
    return ()

--
-- | Updates the world info (it is the responsibility of the caller to rebuild
--   the current info)
--
updateSystemInfo :: IDEAction
updateSystemInfo = do
    wi              <-  readIDE systemInfo
    let version     =   cProjectVersion
    case wi of
        Nothing -> loadSystemInfo
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
                    modifyIDE_ (\ide -> ide{systemInfo = Just scope})


--
-- | Builds the current info for the activePackage
--
setInfo :: IDEM (Maybe PackScope) -> IDEAction
setInfo f = do
    packageInfo         <-  readIDE packageInfo
    case packageInfo of
        Nothing                 -> return ()
        Just (active, scope)    -> do
            newActive   <-  f
            case newActive of
                Nothing         -> return ()
                Just newActive  -> do
                    modifyIDE_ (\ide -> ide{packageInfo = Just
                        (addOtherToScope newActive True, scope)})
                    return ()

--
-- | Loads the current info for the activePackage, or builds it if not available
--
loadOrBuildPackageInfo :: IDEPackage -> IDEM (Maybe PackageDescr)
loadOrBuildPackageInfo pack = do
    mbActiveInfo        <-  loadPackageInfoFor  pack
    case mbActiveInfo of
        Just ai         ->  return (Just ai)
        Nothing         ->  buildPackageInfo pack

--
-- | Loads the current info for the activePackage
--
loadPackageInfoFor :: IDEPackage -> IDEM (Maybe PackageDescr)
loadPackageInfoFor idePackage  = do
    collectorPath   <-  lift $ getCollectorPath cProjectVersion
    liftIO $ loadInfosForPackage collectorPath True
                                    (packageId idePackage)



--
-- | Builds the current info for the activePackage
--
buildPackageInfo :: IDEPackage -> IDEM (Maybe PackageDescr)
buildPackageInfo idePackage = do
    libDir          <-   liftIO $ getSysLibDir
    liftIO $ runGhc (Just libDir)
                $ collectUninstalled False cProjectVersion (cabalFile idePackage)
    -- ideMessage Normal "uninstalled collected"
    collectorPath   <-  lift $ getCollectorPath cProjectVersion
    liftIO $ loadInfosForPackage collectorPath True
                                    (packageId idePackage)


--
-- | Loads the infos for the given packages
--
loadInfosForPackage :: FilePath -> Bool -> PackageIdentifier -> IO (Maybe PackageDescr)
loadInfosForPackage dirPath isWorkingPackage pid = do
    let filePath = dirPath </> fromPackageIdentifier pid ++
             (if isWorkingPackage then leksahCurrentMetaExtension else leksahMetadataFileExtension)
    exists <- doesFileExist filePath
    if exists
        then catch (do
            file            <-  openBinaryFile filePath ReadMode
            -- trace ("now loading metadata for package" ++ filePath) return ()
            bs              <-  BSL.hGetContents file
            let (metadataVersion', packageInfo) =   decodeSer bs
            if metadataVersion /= metadataVersion'
                then do
                    hClose file
                    throwIDE ("Metadata has a wrong version."
                            ++  " Consider rebuilding metadata with -r option")
                else do
                    packageInfo `deepseq` (hClose file)
                    return (Just packageInfo))
            (\e -> do sysMessage Normal ("loadInfosForPackage: " ++ show e); return Nothing)
        else do
            sysMessage Normal $"packageInfo not found for " ++ fromPackageIdentifier pid
            return Nothing

--
-- | Loads the infos for the given packages (has an collecting argument)
--
buildScope :: PackageDescr -> PackScope -> PackScope
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
keywordDescrs = map (\s -> Descr
                                s
                                Nothing
                                Nothing
                                Nothing
                                (Just (BS.pack "| Haskell keyword"))
                                KeywordDescr) keywords

extensionDescrs :: [Descr]
extensionDescrs =  map (\ext -> Descr
                                    ("X" ++ show ext)
                                    Nothing
                                    Nothing
                                    Nothing
                                    (Just (BS.pack "| Haskell language extension"))
                                    ExtensionDescr) knownExtensions

moduleNameDescrs :: PackageDescr -> [Descr]
moduleNameDescrs pd = map (\md -> Descr
                                    ((display . modu . moduleIdMD) md)
                                    Nothing
                                    (Just (moduleIdMD md))
                                    Nothing
                                    (Just (BS.pack "| Module name"))
                                    ModNameDescr) (exposedModulesPD pd)

addOtherToScope ::  PackScope -> Bool -> PackScope
addOtherToScope (packageMap, symbolTable) addAll = (packageMap, newSymbolTable)
    where newSymbolTable = foldl' (\ map descr -> Map.insertWith (++) (descrName descr) [descr] map)
                        symbolTable (if addAll
                                        then keywordDescrs ++ extensionDescrs ++ modNameDescrs
                                        else modNameDescrs)
          modNameDescrs = concatMap moduleNameDescrs (Map.elems packageMap)

-- ---------------------------------------------------------------------
-- NFData instances for forcing evaluation
--

instance NFData Location where
    rnf pd =  rnf (locationSLine pd)
                    `seq`    rnf (locationSCol pd)
                    `seq`    rnf (locationELine pd)
                    `seq`    rnf (locationECol pd)

instance NFData PackageDescr where
    rnf pd =  rnf (packagePD pd)
                    `seq`    rnf (mbSourcePathPD pd)
                    `seq`    rnf (exposedModulesPD pd)
                    `seq`    rnf (buildDependsPD pd)

instance NFData ModuleDescr where
    rnf pd =  rnf (moduleIdMD pd)
                    `seq`    rnf (mbSourcePathMD pd)
                    `seq`    rnf (exportedNamesMD pd)
                    `seq`    rnf (referencesMD pd)

instance NFData Descr where
    rnf (Descr descrName' typeInfo' descrModu'
        mbLocation' mbComment' details')  =  rnf descrName'
                    `seq`    rnf typeInfo'
                    `seq`    rnf descrModu'
                    `seq`    rnf mbLocation'
                    `seq`    rnf mbComment'
                    `seq`    rnf details'

    rnf (Reexported reexpModu' impDescr') = rnf reexpModu'
                    `seq`    rnf impDescr'

instance NFData SpDescr where
    rnf (FieldDescr typeDescrF')              =   rnf typeDescrF'
    rnf (ConstructorDescr typeDescrC')        =   rnf typeDescrC'
    rnf (DataDescr constructors' fields')     =   constructors'
                    `seq` rnf fields'
    rnf (NewtypeDescr constructor' mbField')  =   rnf constructor'
                    `seq`    rnf mbField'
    rnf (ClassDescr super' methods')          =   rnf super'
                    `seq`    rnf methods'
    rnf (MethodDescr classDescrM')            =   rnf classDescrM'
    rnf (InstanceDescr binds')                =   rnf binds'
    rnf a                                     =   seq a ()


instance NFData PackageIdentifier where
    rnf pd =  rnf (pkgName pd)
                    `seq`    rnf (pkgVersion pd)

instance NFData DescrType where  rnf a = seq a ()

instance NFData BS.ByteString where  rnf b = seq b ()

instance NFData Version where  rnf v = seq v ()

instance NFData PackModule where
    rnf pd =  rnf (pack pd)
                    `seq`   rnf (modu pd)

instance NFData ModuleName where
    rnf =  rnf . components

instance NFData PackageName where
    rnf (PackageName s) =  rnf s

