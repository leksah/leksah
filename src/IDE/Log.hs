{-# OPTIONS_GHC -fglasgow-exts #-}
--
-- Module      :  IDE.Log
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Log pane
--
-------------------------------------------------------------------------------


module IDE.Log (
    LogView(..)
,   LogAction(..)
,   IDELog(..)
,   LogState
,   LogTag(..)
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.Multiline.TextView
import Control.Monad.Reader
import Data.Maybe

import IDE.Core.State
import IDE.SourceEditor
import IDE.Framework.ViewFrame


-------------------------------------------------------------------------------
--
-- * Interface
--

--
-- | The Log Viev
--

class LogAction alpha where
    clearLog        ::   alpha

instance LogAction IDEAction where
    clearLog        =   clearLog'

class IDEPaneC alpha => LogView alpha where
    getLog          ::   IDEM alpha
    appendLog       ::   alpha  -> String -> LogTag -> IO Int
    markErrorInLog  ::   alpha  -> (Int, Int) -> IO ()

instance IDEObject IDELog
instance IDEPaneC IDELog

instance LogView IDELog
    where
    getLog          =   getLog'
    appendLog       =   appendLog'
    markErrorInLog  =   markErrorInLog'

instance CastablePane IDELog where
    casting _               =   LogCasting
    downCast _ (PaneC a)    =   case casting a of
                                    LogCasting -> Just a
                                    _          -> Nothing


instance Recoverable LogState where
    toPaneState a           =   LogSt a

instance Pane IDELog
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
                sysMessage Normal "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                lift $notebookRemovePage nb i
                removePaneAdmin pane

instance RecoverablePane IDELog LogState where
    saveState p     =   return (Just (StateC LogState))
    recoverState pp LogState = do
        nb <- getNotebook pp
        initLog pp nb


-------------------------------------------------------------------------------
--
-- * Implementation
--


initLog :: PanePath -> Notebook -> IDEAction
initLog panePath nb = do
    ideR <- ask
    panes <- readIDE panes
    paneMap <- readIDE paneMap
    prefs <- readIDE prefs
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

        textViewSetEditable tv True
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

        let buf = IDELog tv sw
        notebookInsertOrdered nb sw (paneName buf)
        widgetShowAll (scrolledWindowL buf)
        cid1 <- tv `afterFocusIn`
            (\_ -> do runReaderT (makeActive buf) ideR; return True)
        cid2 <- tv `onButtonPress`
            (\ b -> do runReaderT (clicked b buf) ideR; return True)
        return (buf,[cid1])
    addPaneAdmin buf (BufConnections [] [] cids) panePath
    lift $widgetGrabFocus (textView buf)

clicked :: Event -> IDELog -> IDEAction
clicked (Button _ SingleClick _ _ _ _ LeftButton x y) ideLog = do
    errors'     <-  readIDE errors
    line' <- lift $ do
        (x,y)       <-  widgetGetPointer (textView ideLog)
        (_,y')      <-  textViewWindowToBufferCoords (textView ideLog) TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY (textView ideLog) y'
        textIterGetLine iter
    case filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
            (zip errors' [0..(length errors')]) of
        [(thisErr,n)] -> do
            succ <- selectSourceBuf (filePath thisErr)
            if succ
                then markErrorInSourceBuf (line thisErr) (column thisErr)
                        (errDescription thisErr)
                else return ()
            log :: IDELog <- getLog
            lift $ markErrorInLog log (logLines thisErr)
            modifyIDE_ (\ide -> return (ide{currentErr = Just n}))
        otherwise   -> return ()
clicked _ _ = return ()


getLog' :: IDEM IDELog
getLog' = do
    mbPane <- getPane LogCasting
    case mbPane of
        Nothing -> do
            prefs   <- readIDE prefs
            layout  <- readIDE layout
            let pp  =  getStandardPanePath (logPanePath prefs) layout
            nb      <- getNotebook pp
            initLog pp nb
            mbPane <- getPane LogCasting
            case mbPane of
                Nothing ->  throwIDE "Can't init log"
                Just l  ->  return l
        Just p -> return p

appendLog' :: IDELog -> String -> LogTag -> IO Int
appendLog' l@(IDELog tv _) string tag = do
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

markErrorInLog' :: IDELog -> (Int,Int) -> IO ()
markErrorInLog' (IDELog tv _) (l1,l2) = do
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

clearLog' :: IDEAction
clearLog' = do
    log <- getLog
    buf <- lift$ textViewGetBuffer $textView log
    lift $textBufferSetText buf ""
    modifyIDE_ (\ide -> return (ide{errors = [], currentErr = Nothing}))

