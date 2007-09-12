--
-- | The log pane og ghf
--

module Ghf.GUI.Log (
    initLog
,   getLog
,   isLog
,   appendLog
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
import Ghf.GUI.ViewFrame
import Ghf.GUI.SourceCandy

import Ghf.Core

logBufferName = "Log"

initLog :: GhfAction
initLog = do
    ghfR <- ask
    panePath <- getActivePanePathOrTop
    nb <- getActiveOrTopNotebook
    panes <- readGhf panes
    paneMap <- readGhf paneMap
    prefs <- readGhf prefs
    (buf,cids) <- lift $ do
        tv <- textViewNew
        textViewSetEditable tv False
        fd <- case logviewFont prefs of
            Just str -> do
                fontDescriptionFromString str
            Nothing -> do
                f <- fontDescriptionNew
                fontDescriptionSetFamily f "Sans"
                return f
        widgetModifyFont tv (Just fd)
        sw <- scrolledWindowNew Nothing Nothing
        containerAdd sw tv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        scrolledWindowSetShadowType sw ShadowIn

        let buf = GhfLog tv sw
        notebookPrependPage nb sw logBufferName
        widgetShowAll (scrolledWindowL buf)
        mbPn <- notebookPageNum nb sw
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        return (buf,[])
    let newPaneMap  =  Map.insert (LogBuf buf) (panePath,cids) paneMap
    let newPanes = Map.insert logBufferName (LogBuf buf) panes
    modifyGhf_ (\ghf -> return (ghf{panes = newPanes,
                                    paneMap = newPaneMap}))
    lift $widgetGrabFocus (textView buf)

getLog :: GhfM GhfLog
getLog = do
    panesST <- readGhf panes
    let logs = map (\ (LogBuf b) -> b) $filter isLog $Map.elems panesST
    if null logs || length logs > 1
        then error "no log buf or more then one log buf"
        else return (head logs)

isLog :: GhfPane -> Bool
isLog (LogBuf _)    = True
isLog _             = False

appendLog :: GhfLog -> String -> IO ()
appendLog (GhfLog tv _) string = do
    buf <- textViewGetBuffer tv
    iter <- textBufferGetEndIter buf
    textBufferInsert buf iter string
    iter <- textBufferGetEndIter buf
    textViewScrollToIter tv iter 0.25 Nothing
    return ()



