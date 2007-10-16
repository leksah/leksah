-----------------------------------------------------------------------------
--
-- Module      :  Ghf.TypeInfo
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The type info pane of ghf
--
---------------------------------------------------------------------------------

module Ghf.TypeInfo (
    initTypeInfo
,   getTypeInfo
,   isTypeInfo
,   openType
,   clearType
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Control.Monad.Reader
import Data.IORef
import System.IO
import qualified Data.Map as Map
import Data.Map (Map,(!))

import Ghf.Core
import Ghf.SourceCandy
import Ghf.ViewFrame
import Ghf.PropertyEditor
import Ghf.SpecialEditors

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
            stringEditor
    ,   mkFieldE (emptyParams
            {  paraName = Just "From Package"})
            packageIdM
            (\b a -> a{packageIdM = b})
            packageEditor
    ,   mkFieldE (emptyParams
            {paraName = Just "Sort of symbol"})
            identifierType
            (\b a -> a{identifierType = b})
            staticSelectionEditor
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

initTypeInfo :: PanePath -> Notebook -> IdentifierDescr -> GhfAction
initTypeInfo panePath nb idDescr = do
    ghfR <- ask
    panes <- readGhf panes
    paneMap <- readGhf paneMap
    prefs <- readGhf prefs
    (buf,cids) <- lift $ do
            nbbox       <- vBoxNew False 0
            bb          <- hButtonBoxNew
            definitionB <- buttonNew "Definition"
            docuB       <- buttonNew "Docu"
            usesB       <- buttonNew "Uses"
            boxPackStart bb definitionB PackNatural 0
            boxPackStart bb docuB PackNatural 0
            boxPackStart bb usesB PackNatural 0
            (widgets, setInjs, getExts, notifiers)
                <- mapM (\ fd -> (fieldEditor fd) prefs) idDescr idDescrDescr
            mapM_ (\ w -> boxPackStart nbbox w PackNatural 0) widgets
            boxPackEnd nbbox bb PackNatural
            openType
            let info = GhfInfo nbbox setInjs
            notebookPrependPage nb nbbox infoPaneName
            widgetShowAll (box pane)
            return (info,[])
    let newPaneMap  =  Map.insert (uniquePaneName (InfoPane pane))
                            (panePath, BufConnections [] [] cids) paneMap
    let newPanes = Map.insert infoPaneName (InfoPane pane) panes
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes,
                                    paneMap = newPaneMap}))
    lift $widgetGrabFocus (bb pane)

makeInfoActive :: GhfInfo -> GhfAction
makeInfoActive info = do
    activatePane (InfoPane info) (BufConnections[][][])

getInfo :: GhfM GhfInfo
getInfo = do
    panesST <- readGhf panes
    prefs   <- readGhf prefs
    layout  <- readGhf layout
    let infos = map (\ (InfoPane b) -> b) $filter isInfo $Map.elems panesST
    if null infos || length infos > 1
        then do
            let pp  =  getStandardPanePath (infoPanePath prefs) layout
            nb      <- getNotebook pp
            initInfo pp nb
            panesST <- readGhf panes
            let logs = map (\ (InfoPane b) -> b) $filter isInfo $Map.elems panesST
            if null logs || length logs > 1
                then error "Can't init info"
                else return (head infos)
        else return (head infos)

isInfo :: GhfPane -> Bool
isInfo (InfoPane _) = True
isInfo _            = False

openType :: IdentifierDescription -> IO ()
openType
    mapM_ (\ setInj -> setInj prefs) setInjs
