{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances -XParallelListComp #-}
--
-- Module      :  IDE.Pane.Log
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Log pane
--
-------------------------------------------------------------------------------


module IDE.Pane.Log (
    IDELog(..)
,   LogView(..)
,   clearLog
,   LogState
,   LogTag(..)

,   readOut
,   readErr
,   runExternal
) where

import Data.Typeable (Typeable(..))
import IDE.Core.State
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad.Trans (liftIO)
import IDE.Pane.SourceBuffer (markRefInSourceBuf,selectSourceBuf)
import System.IO
import Prelude hiding (catch)
import Control.Exception hiding (try)
import IDE.ImportTool (addAllImports,addImport,parseNotInScope)
import System.Process
    (runInteractiveProcess, ProcessHandle(..))

-------------------------------------------------------------------------------
--
-- * Interface
--

--
-- | The Log pane
--


data IDELog         =   IDELog {
    textView        ::   TextView
,   scrolledWindowL ::   ScrolledWindow
} deriving Typeable

data LogState               =   LogState
    deriving(Eq,Ord,Read,Show,Typeable)


class Pane alpha beta => LogView alpha beta where
    getLog          ::   beta alpha
    appendLog       ::   alpha  -> String -> LogTag -> IO Int
    markErrorInLog  ::   alpha  -> (Int, Int) -> IO ()

instance IDEObject IDELog

instance LogView IDELog IDEM
    where
    getLog          =   getLog'
    appendLog       =   appendLog'
    markErrorInLog  =   markErrorInLog'

instance Pane IDELog IDEM
    where
    primPaneName  _ =   "Log"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledWindowL
    paneId b        =   "*Log"
    makeActive log  =   activatePane log []
    close           =   closePane

instance RecoverablePane IDELog LogState IDEM where
    saveState p     =   return (Just LogState)
    recoverState pp LogState = do
        nb <- getNotebook pp
        initLog pp nb


-------------------------------------------------------------------------------
--
-- * Implementation
--


initLog :: PanePath -> Notebook -> IDEAction
initLog panePath nb = do
    panes      <- readIDE panes
    paneMap    <- readIDE paneMap
    prefs      <- readIDE prefs
    (buf,cids) <- reifyIDE $ \ideR  ->  do
        tv           <- textViewNew
        buf          <- textViewGetBuffer tv
        iter         <- textBufferGetEndIter buf
        textBufferCreateMark buf (Just "end") iter True

        tags         <- textBufferGetTagTable buf
        errtag       <- textTagNew (Just "err")
        set errtag[textTagForeground := "red"]
        textTagTableAdd tags errtag
        frametag     <- textTagNew (Just "frame")
        set frametag[textTagForeground := "green"]
        textTagTableAdd tags frametag
        activeErrtag <- textTagNew (Just "activeErr")
        set activeErrtag[textTagBackground := "yellow"]
        textTagTableAdd tags activeErrtag
        intputTag <- textTagNew (Just "input")
        set intputTag[textTagForeground := "blue"]
        textTagTableAdd tags intputTag
        infoTag <- textTagNew (Just "info")
        set infoTag[textTagForeground := "grey"]
        textTagTableAdd tags infoTag

        textViewSetEditable tv True
        fd           <- case logviewFont prefs of
            Just str -> do
                fontDescriptionFromString str
            Nothing  -> do
                f    <- fontDescriptionNew
                fontDescriptionSetFamily f "Sans"
                return f
        widgetModifyFont tv (Just fd)
        sw           <- scrolledWindowNew Nothing Nothing
        containerAdd sw tv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        scrolledWindowSetShadowType sw ShadowIn

        let buf = IDELog tv sw
        notebookInsertOrdered nb sw (paneName buf) Nothing
        widgetShowAll (scrolledWindowL buf)
        cid1         <- tv `afterFocusIn`
            (\_      -> do reflectIDE (makeActive buf) ideR ; return True)
        cid2         <- tv `onButtonPress`
            (\ b     -> do reflectIDE (clicked b buf) ideR ; return True)
        return (buf,[ConnectC cid1, ConnectC cid2])
    addPaneAdmin buf cids panePath
    liftIO $widgetGrabFocus (textView buf)

clicked :: Event -> IDELog -> IDEAction
clicked (Button _ SingleClick _ _ _ _ LeftButton x y) ideLog = do
    logRefs'     <-  readIDE allLogRefs
    line' <- liftIO $ do
        (x,y)       <-  widgetGetPointer (textView ideLog)
        (_,y')      <-  textViewWindowToBufferCoords (textView ideLog) TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY (textView ideLog) y'
        textIterGetLine iter
    case filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
            (zip logRefs' [0..(length logRefs')]) of
        [(thisRef,n)] -> do
            mbBuf <- selectSourceBuf (filePath thisRef)
            case mbBuf of
                Just buf -> markRefInSourceBuf n buf thisRef True
                Nothing -> return ()
            log :: IDELog <- getLog
            liftIO $ markErrorInLog log (logLines thisRef)
            case logRefType thisRef of
                BreakpointRef -> modifyIDE_ (\ide -> return (ide{currentBreak = Just thisRef}))
                _             -> modifyIDE_ (\ide -> return (ide{currentError = Just thisRef}))
        otherwise   -> return ()
clicked (Button _ SingleClick _ _ _ _ RightButton x y) ideLog = do
    logRefs'    <-  readIDE allLogRefs
    line'       <-  reifyIDE $ \ideR  ->  do
        (x,y)       <-  widgetGetPointer (textView ideLog)
        (_,y')      <-  textViewWindowToBufferCoords (textView ideLog) TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY (textView ideLog) y'
        textIterGetLine iter
    case filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
            (zip logRefs' [0..(length logRefs')]) of
        [(thisRef,n)] -> reifyIDE $ \ideR  ->  do
            theMenu         <-  menuNew
            item0           <-  menuItemNewWithLabel "Add all imports"
            item0 `onActivateLeaf` do
                reflectIDE addAllImports ideR
            menuShellAppend theMenu item0
            case parseNotInScope (refDescription thisRef) of
                Nothing   -> do
                    return ()
                Just _  -> do
                    item1   <-  menuItemNewWithLabel "Add import"
                    item1 `onActivateLeaf` do
                        reflectIDE (addImport thisRef [] >> return()) ideR
                    menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return ()
        otherwise   -> return ()
clicked _ _ = return ()

getLog' :: IDEM IDELog
getLog' = do
    mbPane <- getPane
    case mbPane of
        Nothing -> do
            prefs   <- readIDE prefs
            layout  <- readIDE layout
            let pp  =  getStandardPanePath (logPanePath prefs) layout
            nb      <- getNotebook pp
            initLog pp nb
            mbPane <- getPane
            case mbPane of
                Nothing ->  throwIDE "Can't init log"
                Just l  ->  return l
        Just p -> return p

simpleLog :: String -> IDEAction
simpleLog str = do
    log :: IDELog <- getLog
    liftIO $ appendLog log str LogTag
    return ()

appendLog' :: IDELog -> String -> LogTag -> IO Int
appendLog' l@(IDELog tv _) string tag = do
    buf   <- textViewGetBuffer tv
    iter  <- textBufferGetEndIter buf
    textBufferSelectRange buf iter iter
    textBufferInsert buf iter string
    iter2 <- textBufferGetEndIter buf
    let tagName = case tag of
                    LogTag   -> Nothing
                    ErrorTag -> Just "err"
                    FrameTag -> Just "frame"
                    InputTag -> Just "input"
                    InfoTag  -> Just "info"

    case tagName of
        Nothing   -> return ()
        Just name -> do
            len   <- textBufferGetCharCount buf
            strti <- textBufferGetIterAtOffset buf (len - length string)
            textBufferApplyTagByName buf name iter2 strti

    textBufferMoveMarkByName buf "end" iter2
    mbMark <- textBufferGetMark buf "end"
    line   <- textIterGetLine iter2
    case mbMark of
        Nothing   -> return ()
        Just mark -> textViewScrollMarkOnscreen tv mark
    return line

markErrorInLog' :: IDELog -> (Int,Int) -> IO ()
markErrorInLog' (IDELog tv _) (l1,l2) = do
    idleAdd  (do
        buf    <- textViewGetBuffer tv
        iter   <- textBufferGetIterAtLineOffset buf (l1-1) 0
        iter2  <- textBufferGetIterAtLineOffset buf l2 0
        textBufferSelectRange buf iter iter2
        textBufferMoveMarkByName buf "end" iter
        mbMark <- textBufferGetMark buf "end"
        case mbMark of
            Nothing   -> return ()
            Just mark ->  do
                    textViewScrollToMark tv  mark 0.0 (Just (0.3,0.3))
                    return ()
        return False) priorityDefaultIdle
    return ()


clearLog :: IDEAction
clearLog = do
    log <- getLog
    buf <- liftIO$ textViewGetBuffer $textView log
    liftIO $textBufferSetText buf ""
    modifyIDE_ (\ide -> return (ide{allLogRefs = [], currentError = Nothing, currentBreak = Nothing}))



-- ---------------------------------------------------------------------
-- ** Spawning external processes
--

readOut :: IDELog -> Handle -> IO ()
readOut log hndl =
     catch (readAndShow)
       (\(e :: SomeException) -> do
        --appendLog log ("----------------------------------------\n") FrameTag
        hClose hndl
        return ())
    where
    readAndShow = do
        line <- hGetLine hndl
        appendLog log (line ++ "\n") LogTag
        readAndShow

readErr :: IDELog -> Handle -> IO ()
readErr log hndl =
     catch (readAndShow)
       (\(e :: SomeException) -> do
        hClose hndl
        return ())
    where
    readAndShow = do
        line <- hGetLine hndl
        appendLog log (line ++ "\n") ErrorTag
        readAndShow

runExternal :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runExternal path args = do
    putStrLn $ "Run external called with args " ++ show args
    hndls@(inp, out, err, _) <- runInteractiveProcess path args Nothing Nothing
    sysMessage Normal $ "Starting external tool: " ++ path ++ " with args " ++ (show args)
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    hSetBuffering inp NoBuffering
    hSetBinaryMode out True
    hSetBinaryMode err True
    return hndls

