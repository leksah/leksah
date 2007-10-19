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
    loadInfos
,   clearInfos
,   typeDescription
,   getIdentifierDescr

,   initInfo
,   setInfo
,   isInfo

,   findFittingPackages
,   findFittingPackagesDP
,   fromDPid
,   asDPid
--,   clearInfo
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
import qualified Distribution.Package as DP
import Distribution.PackageDescription hiding (package)
--import Distribution.InstalledPackageInfo
import Distribution.Version
import Data.List
import UniqFM
import PackageConfig

import Ghf.File
import Ghf.Core
import Ghf.SourceCandy
import Ghf.ViewFrame
import Ghf.PropertyEditor
import Ghf.SpecialEditors

--
-- | Load all infos for all installed and exposed packages (see shell command: ghc-pkg list)
--
loadInfos :: GhfAction
loadInfos = do
    session         <-  readGhf session
    let version     =   cProjectVersion
    collectorPath   <-  lift $ getCollectorPath version
    packageInfos    <-  lift $ getInstalledPackageInfos session
    packageList     <-  lift $mapM (loadInfosForPackage collectorPath)
                                                (map package packageInfos)
    scope           <-  foldr buildScope (Map.empty,Map.empty) packageList
    modifyGhf_ (\ghf -> return (ghf{worldInfo = (Just scope)}))

--
-- | Clears the current info, not the world infos
--
clearInfos :: GhfAction
clearInfos = do
    modifyGhf_ (\ghf -> return (ghf{currentInfo = Nothing}))

--
-- | Updates the world info and rebuilds or clears the current info
--
updateInfos :: GhfAction
updateInfos = do
    wi <- readGhf worldInfo
    case wi of
        Nothing -> loadInfos
        Just (psmap,psst) -> do
            packageInfos    <-  lift $ getInstalledPackageInfos session
            let packageIds  =   map package packageInfos
            newPackages     <-  filter (\pi -> Map.member pi psmap)
            trashPackages   <-  filter (\e -> not List.elem e packageIds)(Map.keys psmap)
            if null newPackages && null trashPackages
                then return ()
                else do
                    newPackageInfos <- lift $mapM (loadInfosForPackage collectorPath)
                                                (map package newPackages)
                    let psamp2 = foldr (\e m -> Map.insert (packageIdW e) e m) psmap psmap
                    let psamp3 = foldr (\e m -> Map.delete e m) psmap psmap

                    return (psamp3,




--
-- | Loads the infos for the given packages (has an collecting argument)
--
loadInfosForPackage :: FilePath -> PackageIdentifier -> IO (Maybe PackageDescr)
loadInfosForPackage dirPath (packageList, symbolTable) pid = do
    let filePath = dirPath </> showPackageId pid ++ ".pack"
    exists <- doesFileExist filePath
    if exists
        then do
            hdl <- openFile filePath ReadMode
            str <- hGetContents hdl
            packageInfo <- readIO str
            hClose hdl
            return packageInfo
        else do
            message $"packaeInfo not found for " ++ showPackageId pid
            return Nothing

--
-- | Loads the infos for the given packages (has an collecting argument)
--
buildScope :: PackageDescr -> PackageScope -> PackageScope
buildScope packageD (packageMap, symbolTable) =
    let pid = packageIdW packageD
    in if pid `Map.member` packageMap
        then trace  ("package already in world " ++ showPackageId (packageIdW packageD))
                    (packageMap, symbolTable)
        else (Map.insert pid packageD packageMap,
              Map.unionWith (++) symbolTable (idDescriptions packageD))





--
-- | Lookup of the identifier description
--
getIdentifierDescr :: String -> SymbolTable -> [IdentifierDescr]
getIdentifierDescr str st =
    case str `Map.lookup` st of
        Nothing -> []
        Just list -> list

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
        generateText (IdentifierDescr _ tt ti m p) =
            str ++ " "  ++   (ttString tt) ++ "\n   "
                ++   ti ++  "\n   "
                ++   "exported by modules "  ++   show m ++ " in package " ++ show p ++ "\n   "
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

findFittingPackagesDP :: Session -> [Dependency] -> IO  [DP.PackageIdentifier]
findFittingPackagesDP session dependencyList =  do
        fp <- (findFittingPackages session dependencyList)
        return (map asDPid fp)

asDPid :: PackageIdentifier -> DP.PackageIdentifier
asDPid (PackageIdentifier name version) = DP.PackageIdentifier name version

fromDPid :: DP.PackageIdentifier -> PackageIdentifier
fromDPid (DP.PackageIdentifier name version) = PackageIdentifier name version



-- ---------------------------------------------------------------------
-- The GUI stuff for infos
--


infoPaneName = "Info"

idDescrDescr :: [FieldDescriptionE IdentifierDescr]
idDescrDescr = [
        mkFieldE (emptyParams
            {   paraName = Just "Symbol"})
            identifierW
            (\ b a -> a{identifierW = b})
            stringEditor
    ,   mkFieldE (emptyParams
            {   paraName = Just "Modules exporting"})
            moduleIdI
            (\ b a -> a{moduleIdI = b})
            (multisetEditor (ColumnDescr False [("",(\row -> [New.cellText := row]))])
                (stringEditor, emptyParams))
    ,   mkFieldE (emptyParams
            {  paraName = Just "From Package"})
            packageIdI
            (\b a -> a{packageIdI = b})
            packageEditor
    ,   mkFieldE (emptyParams
            {paraName = Just "Sort of symbol"})
            identifierType
            (\b a -> a{identifierType = b})
            (staticSelectionEditor allIdTypes)
    ,   mkFieldE (emptyParams
            {paraName = Just "Type Info"})
            typeInfo
            (\b a -> a{typeInfo = b})
            multilineStringEditor
{--    ,   mkField (emptyParams
            {paraName = Just "Documentation"})
            typeInfo
            (\b a -> a{typeInfo = b})
            multilineStringEditor--}]

allIdTypes = [Function,Data,Newtype,Synonym,AbstractData,Constructor,Field,Class,ClassOp,Foreign]

initInfo :: PanePath -> Notebook -> IdentifierDescr -> GhfAction
initInfo panePath nb idDescr = do
    ghfR <- ask
    panes <- readGhf panes
    paneMap <- readGhf paneMap
    prefs <- readGhf prefs
    (pane,cids) <- lift $ do
            nbbox       <- vBoxNew False 0
            bb          <- hButtonBoxNew
            definitionB <- buttonNewWithLabel "Definition"
            docuB       <- buttonNewWithLabel "Docu"
            usesB       <- buttonNewWithLabel "Uses"
            boxPackStart bb definitionB PackNatural 0
            boxPackStart bb docuB PackNatural 0
            boxPackStart bb usesB PackNatural 0
            resList <- mapM (\ fd -> (fieldEditor fd) idDescr) idDescrDescr
            let (widgets, setInjs, getExts, notifiers) = unzip4 resList
            mapM_ (\ w -> boxPackStart nbbox w PackNatural 0) widgets
            boxPackEnd nbbox bb PackNatural 0
            --openType
            let info = GhfInfo nbbox setInjs
            notebookPrependPage nb nbbox infoPaneName
            widgetShowAll (box info)
            return (info,[])
    let newPaneMap  =  Map.insert (uniquePaneName (InfoPane pane))
                            (panePath, BufConnections [] [] cids) paneMap
    let newPanes = Map.insert infoPaneName (InfoPane pane) panes
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes,
                                    paneMap = newPaneMap}))
    lift $widgetGrabFocus (box pane)

makeInfoActive :: GhfInfo -> GhfAction
makeInfoActive info = do
    activatePane (InfoPane info) (BufConnections[][][])

setInfo :: IdentifierDescr -> GhfM ()
setInfo identifierDescr = do
    panesST <- readGhf panes
    prefs   <- readGhf prefs
    layout  <- readGhf layout
    let infos = map (\ (InfoPane b) -> b) $filter isInfo $Map.elems panesST
    if null infos || length infos > 1
        then do
            let pp  =  getStandardPanePath (infoPanePath prefs) layout
            lift $ message $ "panePath " ++ show pp
            nb      <- getNotebook pp
            initInfo pp nb identifierDescr
            panesST <- readGhf panes
            let logs = map (\ (InfoPane b) -> b) $filter isInfo $Map.elems panesST
            if null logs || length logs > 1
                then error "Can't init info"
                else return ()
        else do
            let inj = injectors (head infos)
            mapM_ (\ a -> lift $ a identifierDescr)  inj
            return ()

isInfo :: GhfPane -> Bool
isInfo (InfoPane _) = True
isInfo _            = False


