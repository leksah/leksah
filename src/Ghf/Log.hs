{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- | The log pane og ghf
--

module Ghf.Log (
    getLog
,   appendLog
,   LogTag(..)
,   markErrorInLog
,   clearLog

) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Control.Monad.Reader
import Data.Maybe

import Ghf.Core.State
import Ghf.SourceCandy
import Ghf.SourceEditor
import Ghf.ViewFrame

instance Pane GhfLog
    where
    primPaneName _  =   "Log"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledWindowL
    paneId b        =   "*Log"
    makeActive log  =   do
        activatePane log (BufConnections[][] [])
    close pane     =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  lift $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  lift $ do
                putStrLn "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                lift $notebookRemovePage nb i
                removePaneAdmin pane

instance ModelPane GhfLog LogState where
    saveState p     =   return (Just (StateC LogState))

    recoverState pp LogState = do
        nb <- getNotebook pp
        initLog pp nb

data LogTag = LogTag | ErrorTag | FrameTag

initLog :: PanePath -> Notebook -> GhfAction
initLog panePath nb = do
    ghfR <- ask
    panes <- readGhf panes
    paneMap <- readGhf paneMap
    prefs <- readGhf prefs
    (buf,cids) <- lift $ do
        tv <- textViewNew
        buf <- textViewGetBuffer tv
        iter <- textBufferGetEndIter buf
        textBufferCreateMark buf (Just "end") iter True

        tags <- textBufferGetTagTable buf
        errtag <- textTagNew (Just "err")
        set errtag[textTagForeground := "red"]
        textTagTableAdd tags errtag
        frametag <- textTagNew (Just "frame")
        set frametag[textTagForeground := "green"]
        textTagTableAdd tags frametag
        activeErrtag <- textTagNew (Just "activeErr")
        set activeErrtag[textTagBackground := "yellow"]
        textTagTableAdd tags activeErrtag

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
        notebookPrependPage nb sw (paneName buf)
        widgetShowAll (scrolledWindowL buf)
        mbPn <- notebookPageNum nb sw
        case mbPn of
            Just i -> notebookSetCurrentPage nb i
            Nothing -> putStrLn "Notebook page not found"
        cid1 <- tv `afterFocusIn`
            (\_ -> do runReaderT (makeActive buf) ghfR; return True)
        cid2 <- tv `onButtonPress`
            (\ b -> do runReaderT (clicked b buf) ghfR; return True)
        return (buf,[cid1])
    addPaneAdmin buf (BufConnections [] [] cids) panePath
    lift $widgetGrabFocus (textView buf)

clicked :: Event -> GhfLog -> GhfAction
clicked (Button _ SingleClick _ _ _ _ LeftButton x y) ghfLog = do
    lift $ putStrLn "double clicked"
    errors'     <-  readGhf errors
    line' <- lift $ do
        (x,y)       <-  widgetGetPointer (textView ghfLog)
        (_,y')      <-  textViewWindowToBufferCoords (textView ghfLog) TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY (textView ghfLog) y'
        textIterGetLine iter
    case filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
            (zip errors' [0..(length errors')]) of
        [(thisErr,n)] -> do
            lift $ putStrLn "error found"
            succ <- selectSourceBuf (filePath thisErr)
            if succ
                then markErrorInSourceBuf (line thisErr) (column thisErr)
                        (errDescription thisErr)
                else return ()
            markErrorInLog (logLines thisErr)
            modifyGhf_ (\ghf -> return (ghf{currentErr = Just n}))
        otherwise   -> return ()
clicked _ _ = return ()


getLog :: GhfM GhfLog
getLog = do
    mbPane <- getPane LogCasting
    case mbPane of
        Nothing -> do
            prefs   <- readGhf prefs
            layout  <- readGhf layout
            let pp  =  getStandardPanePath (logPanePath prefs) layout
            nb      <- getNotebook pp
            initLog pp nb
            mbPane <- getPane LogCasting
            case mbPane of
                Nothing ->  error "Can't init log"
                Just l  ->  return l
        Just p -> return p

appendLog :: GhfLog -> String -> LogTag -> IO Int
appendLog l@(GhfLog tv _) string tag = do
    buf <- textViewGetBuffer tv
    iter <- textBufferGetEndIter buf
    textBufferSelectRange buf iter iter
    textBufferInsert buf iter string
    iter2 <- textBufferGetEndIter buf
    case tag of
        LogTag -> return ()
        ErrorTag -> do
            len <- textBufferGetCharCount buf
            strti <- textBufferGetIterAtOffset buf (len - length string)
            textBufferApplyTagByName buf "err" iter2 strti
        FrameTag -> do
            len <- textBufferGetCharCount buf
            strti <- textBufferGetIterAtOffset buf (len - length string)
            textBufferApplyTagByName buf "frame" iter2 strti
    textBufferMoveMarkByName buf "end" iter2
    mbMark <- textBufferGetMark buf "end"
    line <- textIterGetLine iter2
    case mbMark of
        Nothing -> return ()
        Just mark -> textViewScrollMarkOnscreen tv mark
    bringPaneToFront l
    return line

markErrorInLog :: (Int,Int) -> GhfAction
markErrorInLog (l1,l2) = do
    (GhfLog tv _) <- getLog
    lift $ do
        buf <- textViewGetBuffer tv
        iter <- textBufferGetIterAtLineOffset buf (l1-1) 0
        iter2 <- textBufferGetIterAtLineOffset buf l2 0
        textBufferSelectRange buf iter iter2
--        textBufferApplyTagByName buf "activeErr" iter iter2
        textBufferMoveMarkByName buf "end" iter
        mbMark <- textBufferGetMark buf "end"
        case mbMark of
            Nothing -> return ()
            Just mark ->  textViewScrollToMark tv  mark 0.0 (Just (0.3,0.3))

clearLog :: GhfAction
clearLog = do
    log <- getLog
    buf <- lift$ textViewGetBuffer $textView log
    lift $textBufferSetText buf ""
    modifyGhf_ (\ghf -> return (ghf{errors = [], currentErr = Nothing}))

