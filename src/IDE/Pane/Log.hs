{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Data.Typeable (Typeable(..))
import IDE.Core.State
import IDE.Core.Types(LogLaunch)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask, unless)
import IDE.Pane.SourceBuffer (markRefInSourceBuf,selectSourceBuf)
import System.IO
import Prelude hiding (catch)
import Control.Exception hiding (try)
import IDE.ImportTool
       (resolveErrors, addResolveMenuItems)
import IDE.Utils.Tool
       (terminateProcess, runInteractiveProcess, ProcessHandle)
import Graphics.UI.Gtk
       (textBufferSetText, textViewScrollToMark,
        textBufferGetIterAtLineOffset, textViewScrollMarkOnscreen, textViewSetBuffer,
        textBufferGetMark, textBufferMoveMarkByName,
        textBufferApplyTagByName, textBufferGetIterAtOffset,
        textBufferGetCharCount, textBufferInsert, textBufferSelectRange,
        widgetHide, widgetShowAll, menuShellAppend,
        menuItemNewWithLabel, containerGetChildren, textIterGetLine,
        textViewGetLineAtY, textViewWindowToBufferCoords, widgetGetPointer,
        on, populatePopup, eventCoordinates, eventClick, eventButton,
        buttonPressEvent, focusInEvent, textBufferNew,
        scrolledWindowSetShadowType, scrolledWindowSetPolicy, containerAdd,
        containerForeach, containerRemove, changed, Click(..), MouseButton(..),
        scrolledWindowNew, widgetModifyFont, fontDescriptionSetFamily,
        fontDescriptionNew, fontDescriptionFromString, textViewSetEditable,
        textTagBackground, textTagTableAdd, textTagForeground, textTagNew,
        textBufferGetTagTable, textBufferCreateMark, textBufferGetEndIter,
        textViewGetBuffer, textViewNew, Window, Notebook, castToWidget,
        ScrolledWindow, TextView, Container, ComboBox, HBox, VBox, Menu, AttrOp(..), set,
        TextWindowType(..), ShadowType(..), PolicyType(..), hBoxNew, buttonNewWithLabel,
        vBoxNew, comboBoxNewText, menuItemActivate,
        comboBoxAppendText, comboBoxSetActive, comboBoxGetActiveText,
        priorityDefaultIdle, idleAdd,Frame, frameNew,buttonActivated,
        boxPackStart, boxPackEnd, Packing(..), comboBoxGetActive, comboBoxRemoveText,
        comboBoxGetModelText, listStoreToList, after) --TODO remove import for logging only
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
import Data.Foldable (forM_)

-------------------------------------------------------------------------------
--
-- * Interface
--

--
-- | The Log pane
--


data IDELog = IDELog {
    logMainContainer :: VBox
,   logLaunchTextView :: TextView
,   logButtons :: HBox
,   logLaunchBox :: ComboBox
} deriving Typeable

getActiveOrDefaultLogLaunch :: IDEM LogLaunch
getActiveOrDefaultLogLaunch = do
                         log <- getLog
                         let comboBox = logLaunchBox log
                         launches <- readIDE logLaunches
                         active <- liftIO $ comboBoxGetActiveText comboBox
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
    liftIO $ comboBoxAppendText comboBox name
    launches <- readIDE logLaunches
    let newLaunches = Map.insert name (LogLaunchData logLaunch (Just pid)) launches
    modifyIDE_ (\ide -> ide {logLaunches = newLaunches})
    showLogLaunch name


removeActiveLogLaunchData :: IDEM ()
removeActiveLogLaunchData = do
--                liftIO $ putStrLn $ "Attempting to remove active log launchdata from ide" --TODO remove logging
                log <- getLog
                let comboBox = logLaunchBox log

                index <- liftIO $ comboBoxGetActive comboBox
                mbTitle <- liftIO $ comboBoxGetActiveText comboBox
--                liftIO $ putStrLn $ "Lauch to remove: index " ++ (show index) ++ ", mbTitle: "++ (show mbTitle)
                let title = fromJust mbTitle

--                model <- liftIO $ comboBoxGetModelText comboBox
--                list <- liftIO $ listStoreToList model
--                liftIO $ putStrLn $ "Underlying model " ++ (show list)


                liftIO $ showDefaultLogLaunch comboBox
                liftIO $ comboBoxRemoveText comboBox index
--                liftIO $ putStrLn $ "Removed launch from combobox."
                launches <- readIDE logLaunches
--                liftIO $ putStrLn $ "Number of available launches: "++(show $ length $ Map.toList launches)
                let newLaunches = Map.delete title launches
                modifyIDE_ (\ide -> ide {logLaunches = newLaunches})
--                liftIO $ putStrLn $ "Removed log launch data successfully from ide"

showDefaultLogLaunch :: ComboBox -> IO()
showDefaultLogLaunch comboBox = comboBoxSetActive comboBox 0

showDefaultLogLaunch' :: IDEM ()
showDefaultLogLaunch' = do
        log <- getLog
        let comboBox = logLaunchBox log

        liftIO $ showDefaultLogLaunch comboBox

showLogLaunch :: Text -> IDEM ()
showLogLaunch name = do
    liftIO $ putStrLn $ "showLogLaunch: name = " <> T.unpack name
    log <- getLog
    let comboBox = logLaunchBox log

    model <- liftIO $ comboBoxGetModelText comboBox
    list <- liftIO $ listStoreToList model
    let mbIndex = elemIndex name list

    liftIO $ putStrLn $ "showLogLaunch: mbIndex = " ++ show mbIndex

    case mbIndex of
        Nothing -> return() -- TODO errorCalls
        Just index -> liftIO $ comboBoxSetActive comboBox index
    liftIO $ putStrLn "switched to loglaunch"

data LogState               =   LogState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDELog IDEM
    where
    primPaneName  _ =   __ "Log"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . logMainContainer
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
    buf          <- textBufferNew Nothing
    iter         <- textBufferGetEndIter buf
    textBufferCreateMark buf (Just "end") iter True
    tags         <- textBufferGetTagTable buf

    errtag       <- textTagNew (Just "err")
    set errtag[textTagForeground := ("red" :: Text)]
    textTagTableAdd tags errtag

    frametag     <- textTagNew (Just "frame")
    set frametag[textTagForeground := ("dark green" :: Text)]
    textTagTableAdd tags frametag

    activeErrtag <- textTagNew (Just "activeErr")
    set activeErrtag[textTagBackground := ("yellow" :: Text)]
    textTagTableAdd tags activeErrtag

    intputTag <- textTagNew (Just "input")
    set intputTag[textTagForeground := ("blue" :: Text)]
    textTagTableAdd tags intputTag

    infoTag <- textTagNew (Just "info")
    set infoTag[textTagForeground := ("grey" :: Text)]
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
    reifyIDE $  \ideR -> do
        mainContainer <- vBoxNew False 0

        -- top, buttons and combobox
        hBox <- hBoxNew False 0
        boxPackStart mainContainer hBox PackNatural 0

        terminateBtn <- buttonNewWithLabel (__ "Terminate process")
        boxPackStart hBox terminateBtn PackNatural 0
        removeBtn <- buttonNewWithLabel (__ "Remove launch")
        boxPackStart hBox removeBtn PackNatural 0
        comboBox <- comboBoxNewText
        boxPackEnd hBox comboBox PackGrow 0

        -- bot, launch textview in a scrolled window
        tv           <- textViewNew
        textViewSetEditable tv False
        fd           <- case logviewFont prefs of
            Just str ->  fontDescriptionFromString str
            Nothing  -> do
                f    <- fontDescriptionNew
                fontDescriptionSetFamily f ("Sans" :: Text)
                return f
        widgetModifyFont tv (Just fd)
        sw           <- scrolledWindowNew Nothing Nothing
        containerAdd sw tv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        scrolledWindowSetShadowType sw ShadowIn

        boxPackEnd mainContainer sw PackGrow 0

        -- add default launch
        textViewSetBuffer tv (logBuffer newLogLaunch)
        index <- comboBoxAppendText comboBox defaultLogName
        comboBoxSetActive comboBox index

        on comboBox changed $ do
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

                                        liftIO $ textViewSetBuffer tv buf
                                        )
                                        ideR

        on terminateBtn buttonActivated $ do
                mbTitle <- comboBoxGetActiveText comboBox
                case mbTitle of
                    Nothing -> return()
                    Just title -> reflectIDE (
                                    do
                                        launches <- readIDE logLaunches
                                        terminateLogLaunch title launches
                                        )
                                        ideR

        on removeBtn buttonActivated $ do
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
        cid1 <- after tv focusInEvent $ do
            liftIO $ reflectIDE (makeActive buf) ideR
            return False
        cid2 <- on tv buttonPressEvent $ do
            click <- eventClick
            button <- eventButton
            (x, y) <- eventCoordinates
            liftIO $ reflectIDE (clicked click button x y buf) ideR
            return False
        cid3 <- on tv populatePopup $ populatePopupMenu buf ideR
        return (Just buf, [ConnectC cid1, ConnectC cid2])
        where
        terminateLogLaunch title launches = do
            let mbPH = mbPid $ fromJust $ Map.lookup title launches
            case mbPH of
                Nothing -> return ()
                Just ph -> liftIO $ terminateProcess ph


clicked :: Click -> MouseButton -> Double -> Double -> IDELog -> IDEAction
clicked SingleClick LeftButton x y log = do
    logRefs'     <-  readIDE allLogRefs
    log <- getLog
    line' <- liftIO $ do
        let tv = logLaunchTextView log
        (x,y)       <-  widgetGetPointer tv
        (_,y')      <-  textViewWindowToBufferCoords tv TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY tv y'
        textIterGetLine iter
    case [(s,e,es) | es@LogRef{logLines = Just (s, e)} <- logRefs', s <= (line'+1) && e >= (line'+1)] of
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
    item0           <-  menuItemNewWithLabel (__ "Resolve Errors")
    item0 `on` menuItemActivate $ reflectIDE resolveErrors ideR
    menuShellAppend menu item0
    res <- reflectIDE (do
        log <- getLog
        logRefs'    <-  readIDE allLogRefs
        activeLogLaunch <- getActiveOrDefaultLogLaunch -- TODO srp get active log launch here
        line'       <-  reifyIDE $ \ideR  ->  do
            let tv = logLaunchTextView log
            (x,y)       <-  widgetGetPointer tv
            (_,y')      <-  textViewWindowToBufferCoords tv TextWindowWidget (x,y)
            (iter,_)    <-  textViewGetLineAtY tv y'
            textIterGetLine iter
        return [es | es@LogRef{logLines = Just (s, e)} <- logRefs', s <= (line'+1) && e >= (line'+1)]) ideR
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
    textBufferInsert buf iter text
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
            strti <- textBufferGetIterAtOffset buf (len - T.length text)
            textBufferApplyTagByName buf name iter2 strti

    textBufferMoveMarkByName buf "end" iter2
    mbMark <- textBufferGetMark buf "end"
    line   <- textIterGetLine iter2
    forM_ mbMark (textViewScrollMarkOnscreen tv)
    return line

markErrorInLog :: IDELog -> (Int,Int) -> IDEAction
markErrorInLog log (l1,l2) = do
    let tv = logLaunchTextView log
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
    log <- getLog
    buf <- liftIO $ textViewGetBuffer $ logLaunchTextView log
    liftIO $ textBufferSetText buf ("" :: Text)
    modifyIDE_ (\ide -> ide{allLogRefs = []})
    setCurrentError Nothing
    setCurrentBreak Nothing


