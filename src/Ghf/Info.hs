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
    loadInfosForPackages
,   clearInfosForPackages
,   findFittingPackages
,   typeDescription
,   getIdentifierDescr

,   initInfo
,   setInfo
,   isInfo
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
import Distribution.Package
import Config
import Control.Monad
import Control.Monad.Trans
import System.FilePath
import System.Directory
import Data.Map (Map)
import qualified Data.Map as Map
import GHC
import Distribution.PackageDescription hiding (package)
import Distribution.InstalledPackageInfo
import Distribution.Version
import Data.List

import Ghf.File
import Ghf.Extractor
import Ghf.Core
import Ghf.SourceCandy
import Ghf.ViewFrame
import Ghf.PropertyEditor
import Ghf.SpecialEditors

loadInfosForPackages :: [PackageIdentifier] -> GhfAction
loadInfosForPackages packages = do
    let version     =   cProjectVersion
    collectorPath   <-  lift $getCollectorPath version
    (packageList,symbolTable) <-  lift $foldM (loadInfosForPackage collectorPath)
                                                ([],Map.empty) packages
    modifyGhf_ (\ghf -> return (ghf{packWorld = (Just (packageList,symbolTable))}))

clearInfosForPackages :: GhfAction
clearInfosForPackages = do
    modifyGhf_ (\ghf -> return (ghf{packWorld = Nothing}))

loadInfosForPackage :: FilePath -> ([PackageDescr],SymbolTable) -> PackageIdentifier ->
                            IO ([PackageDescr],SymbolTable)
loadInfosForPackage dirPath (packageList, symbolTable) pid = do
    let filePath = dirPath </> showPackageId pid ++ ".pack"
    exists <- doesFileExist filePath
    if exists
        then do
            hdl <- openFile filePath ReadMode
            str <- hGetContents hdl
            packageInfo <- readIO str
            hClose hdl
            let newSymbols = Map.unionWith (++) symbolTable (idDescriptions packageInfo)
            return (packageInfo:packageList, newSymbols)
        else do
            message $"packaeInfo not found for " ++ showPackageId pid
            return (packageList, symbolTable)


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

getIdentifierDescr :: String -> SymbolTable -> [IdentifierDescr]
getIdentifierDescr str st =
    case str `Map.lookup` st of
        Nothing -> []
        Just list -> list

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

{--
openType :: IdentifierDescription -> IO ()
openType
    mapM_ (\ setInj -> setInj prefs) setInjs
--}
