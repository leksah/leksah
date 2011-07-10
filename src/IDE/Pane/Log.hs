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
,   LogState
,   LogTag(..)
,   showLog
,   clearLog
,   getLog          -- ::   beta alpha
,   appendLog       -- ::   alpha  -> String -> LogTag -> IO Int
,   markErrorInLog  -- ::   alpha  -> (Int, Int) -> IO ()
,   getActiveOrDefaultLogLaunch
,   getDefaultLogLaunch
,   getOrBuildLogLaunchByName
,   getOrBuildLogLaunchByPackage
,   getOrBuildLogLaunchByPackageId

,   readOut
,   readErr
--,   runExternal
) where

import Data.Typeable (Typeable(..))
import IDE.Core.State
import IDE.Core.Types(LogLaunch)
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader
import IDE.Pane.SourceBuffer (markRefInSourceBuf,selectSourceBuf)
import System.IO
import Prelude hiding (catch)
import Control.Exception hiding (try)
import IDE.ImportTool
       (addPackage, parseHiddenModule, addImport, parseNotInScope,
        resolveErrors)
import IDE.System.Process (runInteractiveProcess, ProcessHandle)
import Graphics.UI.Gtk
       (textBufferSetText, textViewScrollToMark,
        textBufferGetIterAtLineOffset, textViewScrollMarkOnscreen,
        textBufferGetMark, textBufferMoveMarkByName,
        textBufferApplyTagByName, textBufferGetIterAtOffset,
        textBufferGetCharCount, textBufferInsert, textBufferSelectRange,
        widgetHide, widgetShowAll, menuShellAppend, onActivateLeaf,
        menuItemNewWithLabel, containerGetChildren, textIterGetLine,
        textViewGetLineAtY, textViewWindowToBufferCoords, widgetGetPointer,
#if MIN_VERSION_gtk(0,10,5)
        on, populatePopup,
#else
        onPopulatePopup,
#endif
        onButtonPress, afterFocusIn,
        scrolledWindowSetShadowType, scrolledWindowSetPolicy, containerAdd,
        containerForeach, containerRemove, changed,
        scrolledWindowNew, widgetModifyFont, fontDescriptionSetFamily,
        fontDescriptionNew, fontDescriptionFromString, textViewSetEditable,
        textTagBackground, textTagTableAdd, textTagForeground, textTagNew,
        textBufferGetTagTable, textBufferCreateMark, textBufferGetEndIter,
        textViewGetBuffer, textViewNew, Window, Notebook, castToWidget,
        ScrolledWindow, TextView, Container, ComboBox, HBox, VBox, Menu, AttrOp(..), set,
        TextWindowType(..), ShadowType(..), PolicyType(..), hBoxNew, buttonNewWithLabel,
        boxPackStartDefaults, vBoxNew, comboBoxNewText, boxPackEndDefaults,
        comboBoxAppendText, comboBoxSetActive, comboBoxGetActiveText,
        priorityDefaultIdle, idleAdd)
import qualified Data.Map as Map
import Data.Maybe
import Distribution.Package

-------------------------------------------------------------------------------
--
-- * Interface
--

--
-- | The Log pane
--


data IDELog = IDELog {
    logVBox :: VBox
,   logButtons :: HBox
,   logLaunchBox :: ComboBox
} deriving Typeable

getActiveOrDefaultLogLaunch :: IDEM LogLaunch
getActiveOrDefaultLogLaunch = do
                         (IDELog _ _ comboBox) <- getLog
                         launches <- readIDE logLaunches
                         active <- liftIO $ comboBoxGetActiveText comboBox
                         case active of
                            Nothing -> getDefaultLogLaunch
                            Just key -> return $ launches Map.! key
--getActiveOrDefaultLogLaunch = getDefaultLogLaunch

getDefaultLogLaunch :: IDEM LogLaunch
getDefaultLogLaunch = do
    launches <- readIDE logLaunches
    return $ launches Map.! defaultLogName

getOrBuildLogLaunchByPackage :: IDEPackage
                             -> IDEM LogLaunch
getOrBuildLogLaunchByPackage = getOrBuildLogLaunchByShownPackageId . getLogLaunchNameByPackage

getOrBuildLogLaunchByPackageId :: PackageIdentifier
                               -> IDEM LogLaunch
getOrBuildLogLaunchByPackageId = getOrBuildLogLaunchByShownPackageId . getLogLaunchNameByPackageId

getOrBuildLogLaunchByShownPackageId :: String
                               -> IDEM LogLaunch
getOrBuildLogLaunchByShownPackageId = getOrBuildLogLaunchByName

getOrBuildLogLaunchByName :: String
                          -> IDEM LogLaunch
getOrBuildLogLaunchByName logName = do
                                                        log@(IDELog _ _ comboBox) <- getLog
                                                        launches <- readIDE logLaunches
                                                        let mbLogLaunch = Map.lookup logName launches
                                                        case mbLogLaunch of
                                                            Just value -> return value
                                                            Nothing -> do
                                                                        newLogLaunch <- createNewLogLaunch
                                                                        liftIO $ comboBoxAppendText comboBox logName
                                                                        let newLaunches = Map.insert logName newLogLaunch launches
                                                                        modifyIDE_ (\ide -> ide {logLaunches = newLaunches})
                                                                        return newLogLaunch

getLogLaunchNameByPackage :: IDEPackage -> String
getLogLaunchNameByPackage package = getLogLaunchNameByPackageId (ipdPackageId package)

getLogLaunchNameByPackageId :: PackageIdentifier -> String
getLogLaunchNameByPackageId (PackageIdentifier pkgName pkgVersion) = show pkgName ++ show pkgVersion

defaultLogName = "default"
--removeLogLaunch :: IDELog
--                -> String
--                -> LogLaunch
--                ->




data LogState               =   LogState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDELog IDEM
    where
    primPaneName  _ =   "Log"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . logVBox
    paneId b        =   "*Log"

instance RecoverablePane IDELog LogState IDEM where
    saveState p     =   return (Just LogState)
    recoverState pp LogState = do
        mbPane :: Maybe IDELog <- getPane
        case mbPane of
            Nothing -> do
                nb <- getNotebook pp
                prefs' <- readIDE prefs
                buildPane pp nb builder
            Just p -> return (Just p)
    builder = builder'

-------------------------------------------------------------------------------
--
-- * Implementation
--

createNewLogLaunch :: IDEM LogLaunch
createNewLogLaunch = do
    idePrefs <- readIDE prefs
    liftIO $ do
        tv           <- textViewNew
        buf          <- textViewGetBuffer tv
        iter         <- textBufferGetEndIter buf
        textBufferCreateMark buf (Just "end") iter True
        tags         <- textBufferGetTagTable buf
        errtag       <- textTagNew (Just "err")
        set errtag[textTagForeground := "red"]
        textTagTableAdd tags errtag
        frametag     <- textTagNew (Just "frame")
        set frametag[textTagForeground := "dark green"]
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

        textViewSetEditable tv False
        fd           <- case logviewFont idePrefs of
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

        return $ LogLaunch tv sw

builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDELog,Connections)
builder' pp nb windows = do
    prefs <- readIDE prefs
    logLaunch <- createNewLogLaunch
    let emptyMap = Map.empty :: Map.Map String LogLaunch
    let map = Map.insert defaultLogName logLaunch emptyMap
    modifyIDE_ $ \ide -> ide { logLaunches = map}

    ideR <- ask
    reifyIDE $  \ideR -> do
--        mainContainer <- vBoxNew False 0

        -- launch container
        container <- vBoxNew False 0


        -- top
        hBox <- hBoxNew False 0
        --containerAdd mainContainer hBox
        containerAdd container hBox
        btn <- buttonNewWithLabel "sample button"
        boxPackStartDefaults hBox btn
        comboBox <- comboBoxNewText
        boxPackEndDefaults hBox comboBox


        --containerAdd mainContainer container


        containerAdd container $ scrolledWindowL logLaunch
        index <- comboBoxAppendText comboBox defaultLogName
        comboBoxSetActive comboBox index


        on comboBox changed $ do
                mbTitle <- comboBoxGetActiveText comboBox
                let title = fromJust mbTitle
                reflectIDE (
                    do
                        launches <- readIDE logLaunches
                        let logLaunch = fromJust $ Map.lookup title launches
                        let sw = scrolledWindowL logLaunch
                        liftIO $ containerForeach container
                                         (\widget -> containerRemove container widget)
                        liftIO $ containerAdd container sw
                        )
                        ideR


        let buf = IDELog container hBox comboBox
--        cid1         <- container `afterFocusIn`
--            (\_      -> do reflectIDE (makeActive buf) ideR ; return False)
        cid2         <- container `onButtonPress`
                    (\ b     -> do reflectIDE (clicked b buf) ideR ; return False)
--        return (Just buf,[ConnectC cid1, ConnectC cid2])
        return (Just buf,[ConnectC cid2])

{--TODO
--#if MIN_VERSION_gtk(0,10,5)
--        cid3         <- container `on` populatePopup $ populatePopupMenu buf ideR
--#else
--        cid3         <- container `onPopulatePopup` (populatePopupMenu buf ideR)
--#endif
-}

clicked :: Event -> IDELog -> IDEAction
clicked (Button _ SingleClick _ _ _ _ LeftButton x y) log = do
    logRefs'     <-  readIDE allLogRefs
    activeLogLaunch <- getActiveOrDefaultLogLaunch -- TODO srp get active log launch here
    line' <- liftIO $ do
        (x,y)       <-  widgetGetPointer (textView activeLogLaunch)
        (_,y')      <-  textViewWindowToBufferCoords (textView activeLogLaunch) TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY (textView activeLogLaunch) y'
        textIterGetLine iter
    case filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
            (zip logRefs' [0..(length logRefs')]) of
        [(thisRef,n)] -> do
            mbBuf <- selectSourceBuf (logRefFullFilePath thisRef)
            case mbBuf of
                Just buf -> markRefInSourceBuf n buf thisRef True
                Nothing -> return ()
            log :: IDELog <- getLog
            markErrorInLog log (logLines thisRef)
            case logRefType thisRef of
                BreakpointRef -> setCurrentBreak (Just thisRef)
                _             -> setCurrentError (Just thisRef)
        otherwise   -> return ()
clicked _ _ = return ()

populatePopupMenu :: IDELog -> IDERef -> Menu -> IO ()
populatePopupMenu log ideR menu = do
    items <- containerGetChildren menu
    item0           <-  menuItemNewWithLabel "Resolve Errors"
    item0 `onActivateLeaf` do
        reflectIDE resolveErrors ideR
    menuShellAppend menu item0
    res <- reflectIDE (do
        logRefs'    <-  readIDE allLogRefs
        activeLogLaunch <- getActiveOrDefaultLogLaunch -- TODO srp get active log launch here
        line'       <-  reifyIDE $ \ideR  ->  do
            (x,y)       <-  widgetGetPointer (textView activeLogLaunch)
            (_,y')      <-  textViewWindowToBufferCoords (textView activeLogLaunch) TextWindowWidget (x,y)
            (iter,_)    <-  textViewGetLineAtY (textView activeLogLaunch) y'
            textIterGetLine iter
        return $ filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
                (zip logRefs' [0..(length logRefs')])) ideR
    case res of
        [(thisRef,n)] -> do
            case parseNotInScope (refDescription thisRef) of
                Nothing   -> do
                    return ()
                Just _  -> do
                    item1   <-  menuItemNewWithLabel "Add Import"
                    item1 `onActivateLeaf` do
                        reflectIDE (addImport thisRef [] (\_ -> return ())) ideR
                    menuShellAppend menu item1
            case parseHiddenModule (refDescription thisRef) of
                Nothing   -> do
                    return ()
                Just _  -> do
                    item2   <-  menuItemNewWithLabel "Add Package"
                    item2 `onActivateLeaf` do
                        reflectIDE (addPackage thisRef >> return ()) ideR
                    menuShellAppend menu item2
            widgetShowAll menu
            return ()
        otherwise   -> return ()
    mapM_ widgetHide $ take 2 (reverse items)

getLog :: IDEM IDELog
getLog = do
    mbPane <- getOrBuildPane (Right "*Log")
    case mbPane of
        Nothing ->  throwIDE "Can't init log"
        Just p -> return p

showLog :: IDEAction
showLog = do
    l <- getLog
    displayPane l False

--simpleLog :: String --
--          -> String
--          -> IDEAction
--simpleLog str = do
--    log :: IDELog <- getLog
--    liftIO $ appendLog log str LogTag
--    return ()

{- the workhorse for logging: appends given text with given tag to given loglaunch -}
appendLog :: LogLaunch
          -> String
          -> LogTag
          -> IO Int
appendLog logLaunch string tag = do
    let tv = textView logLaunch
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

markErrorInLog :: IDELog -> (Int,Int) -> IDEAction
markErrorInLog log (l1,l2) = do
    activeLogLaunch <- getActiveOrDefaultLogLaunch -- TODO srp get active log launch here
    let tv = textView activeLogLaunch
    liftIO $ idleAdd  (do
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
    activeLogLaunch <- getActiveOrDefaultLogLaunch -- TODO srp get active log launch here
    buf <- liftIO$ textViewGetBuffer $ textView activeLogLaunch
    liftIO $textBufferSetText buf ""
--    modifyIDE_ (\ide -> ide{allLogRefs = []})
--    setCurrentError Nothing
--    setCurrentBreak Nothing TODO: Check with Hamish



-- ---------------------------------------------------------------------
-- ** Spawning external processes
--

readOut :: IDELog -> Handle -> IDEAction
readOut log hndl = do
     ideRef <- ask
     liftIO $ catch (readAndShow ideRef)
       (\(e :: SomeException) -> do
        --appendLog log ("----------------------------------------\n") FrameTag
        hClose hndl
        return ())
    where
    readAndShow :: IDERef -> IO()
    readAndShow ideRef = do
        line <- hGetLine hndl
        defaultLogLaunch <- runReaderT getDefaultLogLaunch ideRef --TODO srp use default log here ?
        appendLog defaultLogLaunch (line ++ "\n") LogTag
        readAndShow ideRef

readErr :: IDELog -> Handle -> IDEAction
readErr log hndl = do
    ideRef <- ask
    liftIO $ catch (readAndShow ideRef)
       (\(e :: SomeException) -> do
        hClose hndl
        return ())
    where
    readAndShow ideRef = do
        line <- hGetLine hndl
        defaultLogLaunch <- runReaderT getDefaultLogLaunch ideRef --TODO srp use default log here ?
        appendLog defaultLogLaunch (line ++ "\n") LogTag
        readAndShow ideRef

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

