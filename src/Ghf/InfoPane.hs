-----------------------------------------------------------------------------
--
-- Module      :  Ghf.InfoPane
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The GUI stuff for infos
--
-------------------------------------------------------------------------------

module Ghf.InfoPane (
    initInfo
,   setInfo
,   isInfo
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
--import Distribution.InstalledPackageInfo
import Distribution.Version
import Data.List
import UniqFM
import PackageConfig
import Data.Maybe

import Ghf.File
import Ghf.Core
import Ghf.SourceCandy
import Ghf.ViewFrame
import Ghf.PropertyEditor
import Ghf.SpecialEditors
import Ghf.Log

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


