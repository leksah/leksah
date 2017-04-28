{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
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
,   LogState(..)
,   LogTag(..)
,   showLog
,   clearLog
,   getLog          -- ::   beta alpha
,   appendLog       -- ::   alpha  -> Text -> LogTag -> IO Int
,   markErrorInLog  -- ::   alpha  -> (Int, Int) -> IO ()
,   getActiveOrDefaultLogLaunch
,   getDefaultLogLaunch
,   buildLogLaunchByName
,   buildLogLaunchByPackage
,   buildLogLaunchByPackageId
,   addLogLaunchData
,   showLogLaunch
,   showDefaultLogLaunch
,   showDefaultLogLaunch'
) where

import Prelude ()
import Prelude.Compat
import Data.Typeable (Typeable(..))
import IDE.Core.State
import IDE.Core.Types(LogLaunch)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask, unless)
import IDE.Pane.SourceBuffer (markRefInSourceBuf,selectSourceBuf)
import System.IO
import Control.Exception hiding (try)
import IDE.ImportTool
       (resolveErrors, addResolveMenuItems)
import IDE.Utils.Tool
       (terminateProcess, runInteractiveProcess, ProcessHandle)
import qualified Data.Map as Map
import Data.Maybe
import Distribution.Package
import Data.Char
import IDE.Utils.GUIUtils (__)
import Text.Printf (printf)
import Data.Text (Text)
import qualified Data.Text as T
       (null, all, drop, tail, init, take, reverse, isSuffixOf,
        isPrefixOf, pack, length, unpack)
import Data.Monoid (Monoid(..), (<>))
import Data.List (elemIndex, isPrefixOf, isSuffixOf, findIndex)
import qualified Data.Foldable as F (toList, forM_)
import qualified Data.Sequence as Seq (empty)
import GI.Gtk.Objects.Box (boxNew, Box(..))
import GI.Gtk.Objects.TextView
       (textViewScrollToMark, textViewGetBuffer,
        textViewScrollMarkOnscreen, textViewGetLineAtY,
        textViewWindowToBufferCoords, onTextViewPopulatePopup,
        textViewSetBuffer, textViewSetEditable, textViewNew, TextView(..))
import GI.Gtk.Objects.ComboBox
       (onComboBoxChanged, comboBoxSetActive, comboBoxGetActive,
        ComboBox(..))
import Data.GI.Gtk.ComboBox
       (comboBoxNewText, comboBoxGetModelText, comboBoxRemoveText,
        comboBoxAppendText, comboBoxGetActiveText)
import Data.GI.Gtk.ModelView.SeqStore (seqStoreToList)
import GI.Gtk.Objects.Widget
       (widgetHide, widgetShowAll, widgetGetPointer,
        onWidgetButtonPressEvent, afterWidgetFocusInEvent,
        widgetModifyFont, toWidget)
import GI.Gtk.Objects.TextBuffer
       (textBufferSetText, textBufferGetIterAtLineOffset,
        textBufferGetMark, textBufferMoveMarkByName,
        textBufferApplyTagByName, textBufferGetIterAtOffset,
        textBufferGetCharCount, textBufferInsert, textBufferSelectRange,
        textBufferGetTagTable, textBufferCreateMark, textBufferGetEndIter,
        textBufferNew)
import GI.Gtk.Objects.TextTag
       (setTextTagBackground, setTextTagForeground, textTagNew)
import Data.GI.Base (unsafeCastTo, set)
import GI.Gtk.Objects.TextTagTable
       (noTextTagTable, textTagTableAdd)
import GI.Gtk.Objects.Notebook (Notebook(..))
import GI.Gtk.Objects.Window (Window(..))
import Graphics.UI.Editor.Parameters
       (Packing(..), boxPackEnd', boxPackStart')
import GI.Gtk.Objects.Button (onButtonClicked, buttonNewWithLabel)
import GI.Pango.Structs.FontDescription
       (fontDescriptionSetFamily, fontDescriptionNew,
        fontDescriptionFromString)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetShadowType, scrolledWindowSetPolicy,
        scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container
       (containerGetChildren, containerAdd)
import GI.Gtk.Enums
       (TextWindowType(..), ShadowType(..), PolicyType(..), Orientation(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import GI.Gdk.Structs.EventButton
       (getEventButtonY, getEventButtonX, getEventButtonButton,
        getEventButtonType)
import GI.Gdk.Enums (EventType(..), EventType)
import Data.Word (Word32)
import GI.Gdk.Constants (pattern BUTTON_PRIMARY)
import GI.Gtk.Structs.TextIter (textIterGetLine)
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Objects.MenuItem
       (onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.MenuShell (menuShellAppend)
import GI.GLib.Functions (idleAdd)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT)
import Data.Int (Int32)
import Control.Monad.IO.Class (MonadIO)

-------------------------------------------------------------------------------
--
-- * Interface
--

--
-- | The Log pane
--


data IDELog = IDELog {
    logMainContainer :: Box
,   logLaunchTextView :: TextView
,   logButtons :: Box
,   logLaunchBox :: ComboBox
} deriving Typeable

getActiveOrDefaultLogLaunch :: IDEM LogLaunch
getActiveOrDefaultLogLaunch = do
                         log <- getLog
                         let comboBox = logLaunchBox log
                         launches <- readIDE logLaunches
                         active <- comboBoxGetActiveText comboBox
                         case active of
                            Nothing -> getDefaultLogLaunch
                            Just key -> return $ logLaunch $ launches Map.! key

getDefaultLogLaunch :: MonadIDE m => m LogLaunch
getDefaultLogLaunch = do
    launches <- readIDE logLaunches
    return $ logLaunch $ launches Map.! defaultLogName

buildLogLaunchByPackage :: IDEPackage
                             -> IDEM (LogLaunch, Text)
buildLogLaunchByPackage = buildLogLaunchByShownPackageId . getLogLaunchNameByPackage

buildLogLaunchByPackageId :: PackageIdentifier
                               -> IDEM (LogLaunch, Text)
buildLogLaunchByPackageId = buildLogLaunchByShownPackageId . getLogLaunchNameByPackageId

buildLogLaunchByShownPackageId :: Text
                               -> IDEM (LogLaunch, Text)
buildLogLaunchByShownPackageId = buildLogLaunchByName

buildLogLaunchByName :: Text
                          -> IDEM (LogLaunch, Text)
buildLogLaunchByName logName = do
        log <- getLog
        launches <- readIDE logLaunches
        let mbLogLaunch = Map.lookup logName launches
        let name = getNextFreeName logName launches
        newLogLaunch <- liftIO createNewLogLaunch
        return (newLogLaunch, name)
        where
        getNextFreeName prevName launches = case Map.lookup prevName launches of
                            Nothing -> prevName
                            Just _  -> getNextFreeName (incrementName prevName) launches
        incrementName name = case parseName name of
                                    Nothing -> createNewName name 0
                                    Just (number,name) -> createNewName name number
        createNewName name number = mconcat [name, " (", T.pack (show $ number+1), ")"]
        parseName name = if surroundedByParenth (getLaunchString name) &&
                               isNumberAndNotEmpty (T.init $ T.tail $ getLaunchString name)
                            then Just
                                    (read $ T.unpack $ T.init $ T.tail $ getLaunchString name,
                                     T.reverse $ T.drop 4 $ T.reverse name)
                            else Nothing
        surroundedByParenth string = ("(" `T.isPrefixOf` string ) && (")" `T.isSuffixOf` string) && not (T.null string)
        isNumberAndNotEmpty string = T.all isNumber string && not (T.null string) -- check if
        getLaunchString name = T.reverse $ T.take 3 $ T.reverse name


getLogLaunchNameByPackage :: IDEPackage -> Text
getLogLaunchNameByPackage package = getLogLaunchNameByPackageId (ipdPackageId package)

getLogLaunchNameByPackageId :: PackageIdentifier -> Text
getLogLaunchNameByPackageId (PackageIdentifier pkgName pkgVersion) = T.pack $ show pkgName ++ show pkgVersion

defaultLogName = "default"

-- ^ adds arguments to ide to process them later.
-- ^ e.g. using processhandle to kill process and name to switch between view
addLogLaunchData :: Text -> LogLaunch -> ProcessHandle -> IDEM ()
addLogLaunchData name logLaunch pid = do
    log <- getLog
    let comboBox = logLaunchBox log
    comboBoxAppendText comboBox name
    launches <- readIDE logLaunches
    let newLaunches = Map.insert name (LogLaunchData logLaunch (Just pid)) launches
    modifyIDE_ (\ide -> ide {logLaunches = newLaunches})
    showLogLaunch name


removeActiveLogLaunchData :: IDEM ()
removeActiveLogLaunchData = do
--                liftIO $ putStrLn $ "Attempting to remove active log launchdata from ide" --TODO remove logging
                log <- getLog
                let comboBox = logLaunchBox log

                index <- comboBoxGetActive comboBox
                mbTitle <- comboBoxGetActiveText comboBox
--                liftIO $ putStrLn $ "Lauch to remove: index " ++ (show index) ++ ", mbTitle: "++ (show mbTitle)
                let title = fromJust mbTitle

--                model <- comboBoxGetModelText comboBox
--                list <- seqStoreToList model
--                liftIO $ putStrLn $ "Underlying model " ++ (show list)


                showDefaultLogLaunch comboBox
                comboBoxRemoveText comboBox index
--                liftIO $ putStrLn $ "Removed launch from combobox."
                launches <- readIDE logLaunches
--                liftIO $ putStrLn $ "Number of available launches: "++(show $ length $ Map.toList launches)
                let newLaunches = Map.delete title launches
                modifyIDE_ (\ide -> ide {logLaunches = newLaunches})
--                liftIO $ putStrLn $ "Removed log launch data successfully from ide"

showDefaultLogLaunch :: MonadIO m => ComboBox -> m ()
showDefaultLogLaunch comboBox = comboBoxSetActive comboBox 0

showDefaultLogLaunch' :: IDEM ()
showDefaultLogLaunch' = do
        log <- getLog
        let comboBox = logLaunchBox log

        showDefaultLogLaunch comboBox

showLogLaunch :: Text -> IDEM ()
showLogLaunch name = do
    liftIO $ putStrLn $ "showLogLaunch: name = " <> T.unpack name
    log <- getLog
    let comboBox = logLaunchBox log

    model <- comboBoxGetModelText comboBox
    list <- seqStoreToList model
    let mbIndex = elemIndex name list

    liftIO $ putStrLn $ "showLogLaunch: mbIndex = " ++ show mbIndex

    case mbIndex of
        Nothing -> return() -- TODO errorCalls
        Just index -> comboBoxSetActive comboBox (fromIntegral index)
    liftIO $ putStrLn "switched to loglaunch"

data LogState               =   LogState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDELog IDEM
    where
    primPaneName  _ =   __ "Log"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . toWidget . logMainContainer
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

createNewLogLaunch :: IO LogLaunch
createNewLogLaunch = do
    buf          <- textBufferNew noTextTagTable
    iter         <- textBufferGetEndIter buf
    textBufferCreateMark buf (Just "end") iter True
    tags         <- textBufferGetTagTable buf

    errtag       <- textTagNew (Just "err")
    setTextTagForeground errtag "red"
    textTagTableAdd tags errtag

    frametag     <- textTagNew (Just "frame")
    setTextTagForeground frametag "dark green"
    textTagTableAdd tags frametag

    activeErrtag <- textTagNew (Just "activeErr")
    setTextTagBackground activeErrtag "yellow"
    textTagTableAdd tags activeErrtag

    intputTag <- textTagNew (Just "input")
    setTextTagForeground intputTag "blue"
    textTagTableAdd tags intputTag

    infoTag <- textTagNew (Just "info")
    setTextTagForeground infoTag "grey"
    textTagTableAdd tags infoTag

    return $ LogLaunch buf

builder' :: PanePath ->
    Notebook ->
    Window ->
    IDEM (Maybe IDELog,Connections)
builder' pp nb windows = do
    prefs <- readIDE prefs
    newLogLaunch <- liftIO createNewLogLaunch
    let emptyMap = Map.empty :: Map.Map Text LogLaunchData
    let map = Map.insert defaultLogName (LogLaunchData newLogLaunch Nothing) emptyMap
    modifyIDE_ $ \ide -> ide { logLaunches = map}

    ideR <- ask
    mainContainer <- boxNew OrientationVertical 0

    -- top, buttons and combobox
    hBox <- boxNew OrientationHorizontal 0
    boxPackStart' mainContainer hBox PackNatural 0

    terminateBtn <- buttonNewWithLabel (__ "Terminate process")
    boxPackStart' hBox terminateBtn PackNatural 0
    removeBtn <- buttonNewWithLabel (__ "Remove launch")
    boxPackStart' hBox removeBtn PackNatural 0
    comboBox <- comboBoxNewText
    boxPackEnd' hBox comboBox PackGrow 0

    -- bot, launch textview in a scrolled window
    tv           <- textViewNew
    textViewSetEditable tv False
    fd           <- case logviewFont prefs of
        (True, Just str) ->  fontDescriptionFromString str
        _  -> do
            f    <- fontDescriptionNew
            fontDescriptionSetFamily f "Monospace"
            return f
    widgetModifyFont tv (Just fd)
    sw           <- scrolledWindowNew noAdjustment noAdjustment
    containerAdd sw tv
    scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
    scrolledWindowSetShadowType sw ShadowTypeIn

    boxPackEnd' mainContainer sw PackGrow 0

    -- add default launch
    textViewSetBuffer tv (Just $ logBuffer newLogLaunch)
    index <- comboBoxAppendText comboBox defaultLogName
    comboBoxSetActive comboBox index

    onComboBoxChanged comboBox $ do
            mbTitle <- comboBoxGetActiveText comboBox
            case mbTitle of
                Nothing -> showDefaultLogLaunch comboBox
                Just title -> reflectIDE (
                                do
                                    launches <- readIDE logLaunches
                                    log <- getLog
                                    let tv = logLaunchTextView log
                                    let logL = logLaunch $ (Map.!) launches title
                                    let buf = logBuffer logL

                                    textViewSetBuffer tv (Just buf)
                                    )
                                    ideR

    onButtonClicked terminateBtn $ do
            mbTitle <- comboBoxGetActiveText comboBox
            case mbTitle of
                Nothing -> return()
                Just title -> reflectIDE (
                                do
                                    launches <- readIDE logLaunches
                                    terminateLogLaunch title launches
                                    )
                                    ideR

    onButtonClicked removeBtn $ do
            mbTitle <- comboBoxGetActiveText comboBox
            case mbTitle of
                Nothing -> return()
                Just title -> unless (title == defaultLogName) $
                                 reflectIDE
                                   (do launches <- readIDE logLaunches
                                       removeActiveLogLaunchData
                                       terminateLogLaunch title launches)
                                   ideR


    let buf = IDELog mainContainer tv hBox comboBox
    cid1 <- onIDE afterWidgetFocusInEvent tv $ do
        liftIDE $ makeActive buf
        return False
    cid2 <- onIDE onWidgetButtonPressEvent tv $ do
        e <- lift ask
        click <- getEventButtonType e
        button <- getEventButtonButton e
        x <- getEventButtonX e
        y <- getEventButtonY e
        liftIDE $ clicked click (fromIntegral button) x y buf
        return False
    cid3 <- ConnectC tv <$> onTextViewPopulatePopup tv (\w ->
        unsafeCastTo Menu w >>= populatePopupMenu buf ideR)
    return (Just buf, [cid1, cid2, cid3])
  where
    terminateLogLaunch title launches = do
        let mbPH = mbPid $ fromJust $ Map.lookup title launches
        case mbPH of
            Nothing -> return ()
            Just ph -> liftIO $ terminateProcess ph


clicked :: EventType -> Int32 -> Double -> Double -> IDELog -> IDEAction
clicked EventTypeButtonPress BUTTON_PRIMARY x y log = do
    logRefs'     <-  readIDE allLogRefs
    log <- getLog
    let tv = logLaunchTextView log
    (x,y)       <-  widgetGetPointer tv
    (_,y')      <-  textViewWindowToBufferCoords tv TextWindowTypeWidget x y
    (iter,_)    <-  textViewGetLineAtY tv y'
    line' <- fromIntegral <$> textIterGetLine iter
    case [(s,e,es) | es@LogRef{logLines = Just (s, e)} <- F.toList logRefs', s <= (line'+1) && e >= (line'+1)] of
        [(s,e,thisRef)] -> do
            mbBuf <- selectSourceBuf (logRefFullFilePath thisRef)
            case mbBuf of
                Just buf -> markRefInSourceBuf buf thisRef True
                Nothing -> return ()
            log :: IDELog <- getLog
            markErrorInLog log (s, e)
            case logRefType thisRef of
                BreakpointRef -> setCurrentBreak (Just thisRef)
                _             -> setCurrentError (Just thisRef)
        _ -> return ()
clicked _ _ _ _ _ = return ()

populatePopupMenu :: IDELog -> IDERef -> Menu -> IO ()
populatePopupMenu log ideR menu = do
    items <- containerGetChildren menu
    item0 <-  menuItemNewWithLabel (__ "Resolve Errors")
    onMenuItemActivate item0 $ reflectIDE resolveErrors ideR
    menuShellAppend menu item0
    res <- reflectIDE (do
        log <- getLog
        logRefs'    <-  readIDE allLogRefs
        activeLogLaunch <- getActiveOrDefaultLogLaunch -- TODO srp get active log launch here
        line'       <-  reifyIDE $ \ideR  ->  do
            let tv = logLaunchTextView log
            (x,y)       <-  widgetGetPointer tv
            (_,y')      <-  textViewWindowToBufferCoords tv TextWindowTypeWidget x y
            (iter,_)    <-  textViewGetLineAtY tv y'
            fromIntegral <$> textIterGetLine iter
        return [es | es@LogRef{logLines = Just (s, e)} <- F.toList logRefs', s <= (line'+1) && e >= (line'+1)]) ideR
    case res of
        [thisRef] -> do
            addResolveMenuItems ideR menu thisRef
            widgetShowAll menu
            return ()
        otherwise   -> return ()
    mapM_ widgetHide $ take 2 (reverse items)

getLog :: IDEM IDELog
getLog = do
    mbPane <- getOrBuildPane (Right "*Log")
    case mbPane of
        Nothing ->  throwIDE (__ "Can't init log")
        Just p -> return p

showLog :: IDEAction
showLog = do
    l <- getLog
    displayPane l False

{- the workhorse for logging: appends given text with given tag to given loglaunch -}
appendLog :: IDELog
          -> LogLaunch
          -> Text
          -> LogTag
          -> IO Int
appendLog log logLaunch text tag = do
    let buf = logBuffer logLaunch
    iter  <- textBufferGetEndIter buf
    textBufferSelectRange buf iter iter
    textBufferInsert buf iter text (-1)
    iter2 <- textBufferGetEndIter buf
    let tagName = case tag of
                    LogTag   -> Nothing
                    ErrorTag -> Just "err"
                    FrameTag -> Just "frame"
                    InputTag -> Just "input"
                    InfoTag  -> Just "info"
    let tv = logLaunchTextView log
    case tagName of
        Nothing   -> return ()
        Just name -> do
            len   <- textBufferGetCharCount buf
            strti <- textBufferGetIterAtOffset buf (len - fromIntegral (T.length text))
            textBufferApplyTagByName buf name iter2 strti

    textBufferMoveMarkByName buf "end" iter2
    mbMark <- textBufferGetMark buf "end"
    line   <- textIterGetLine iter2
    F.forM_ mbMark (textViewScrollMarkOnscreen tv)
    return $ fromIntegral line

markErrorInLog :: IDELog -> (Int,Int) -> IDEAction
markErrorInLog log (l1,l2) = do
    let tv = logLaunchTextView log
    idleAdd PRIORITY_DEFAULT (do
        buf    <- textViewGetBuffer tv
        iter   <- textBufferGetIterAtLineOffset buf (fromIntegral l1-1) 0
        iter2  <- textBufferGetIterAtLineOffset buf (fromIntegral l2) 0
        textBufferSelectRange buf iter iter2
        textBufferMoveMarkByName buf "end" iter
        textBufferGetMark buf "end" >>= \case
            Nothing   -> return ()
            Just mark -> textViewScrollToMark tv mark 0.0 True 0.3 0.3
        return False)
    return ()


clearLog :: IDEAction
clearLog = do
    log <- getLog
    buf <- textViewGetBuffer $ logLaunchTextView log
    textBufferSetText buf "" 0
    modifyIDE_ (\ide -> ide{allLogRefs = Seq.empty})
    setCurrentError Nothing
    setCurrentBreak Nothing


