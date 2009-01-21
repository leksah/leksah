{-# OPTIONS_GHC -XDeriveDataTypeable -XMultiParamTypeClasses
    -XScopedTypeVariables -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.Info
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | The GUI stuff for infos
--
-------------------------------------------------------------------------------

module IDE.Pane.Info (
    IDEInfo
,   InfoState
,   setInfo
,   setSymbol
,   replayInfoHistory
,   getIdentifierDescr
,   showInfo
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Control.Monad.Reader
import System.IO
import Control.Monad
import Control.Monad.Trans
import System.IO
--import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.IORef
import qualified Data.Map as Map
import Data.Typeable

import IDE.Core.State
import Control.Event
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Simple
import Graphics.UI.Editor.Composite
import Graphics.UI.Editor.Parameters
import IDE.Pane.SourceBuffer
import IDE.Pane.Callers
import Graphics.UI.Editor.Basics
import MyMissing



-- | An info pane description
--
data IDEInfo        =   IDEInfo {
    sw              ::   ScrolledWindow
,   currentDescr    ::   IORef Descr
,   injector        ::   Descr -> IO()
,   extractor       ::   Descr -> Extractor Descr
} deriving Typeable

data InfoState              =   InfoState Descr
    deriving(Eq,Ord,Read,Show,Typeable)


instance IDEObject IDEInfo

instance Pane IDEInfo IDEM
    where
    primPaneName _  =   "Info"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . sw
    paneId b        =   "*Info"
    makeActive pane =   activatePane pane []
    close pane     =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  liftIO $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  liftIO $ do
                sysMessage Normal "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                liftIO $ do
                    notebookRemovePage nb i
                    widgetDestroy (getTopWidget pane)
                removePaneAdmin pane

instance RecoverablePane IDEInfo InfoState IDEM where
    saveState p     =   do
        currentDescr' <-  liftIO $ readIORef (currentDescr p)
        return (Just (InfoState currentDescr'))
    recoverState pp (InfoState descr) =   do
        setInfo descr

showInfo :: IDEAction
showInfo = do
    mbInfo :: Maybe IDEInfo <- getPane
    case mbInfo of
        Nothing -> return ()
        Just p  -> liftIO $ bringPaneToFront p

idDescrDescr :: IDERef -> Session -> FieldDescription Descr
idDescrDescr ideR session = VFD emptyParams [
    HFD emptyParams [
         mkField
            (paraName <<<- ParaName "Identifier"
                $ paraName <<<- ParaPack PackGrow
                    $ emptyParams)
            descrName
            (\ b a -> if isReexported a then a else a{descrName' = b})
            (symbolEditor ideR session)
    ,    mkField
            emptyParams
            (stockIdFromType . descrType . details)
            (\b a -> a)
            imageEditor
    ,    mkField
            (paraName <<<- ParaName "Sort"
                    $ emptyParams)
            (descrType . details)
            (\b a -> a)
            (comboSelectionEditor allOf)
    ,   mkField
            (paraName <<<- ParaName "Exported by"
                $ paraPack <<<- ParaPack PackGrow
                        $ emptyParams)
            (\l -> showPackModule (descrModu l))
            (\ b a -> if isReexported a then a else a{descrModu' = parsePackModule b})
            (stringEditor (const True))]
    ,   mkField
            (paraPack <<<- ParaPack PackGrow $ emptyParams)
            (\ a -> (BS.unpack $ typeInfo a,
                (case mbComment a of
                    Nothing -> ""
                    Just s -> BS.unpack s)))
            (\ b a ->
                (if isReexported a
                    then a
                    else a {typeInfo'  = BS.pack (fst b),
                            mbComment' = case snd b of
                                            "" -> Nothing
                                            s  -> Just (BS.pack s)}))
            (typeAndCommentEditor ideR session)]

--allIdTypes =  [Function, Newtype, Type, AbstractData, OpenData, Foreign
--    , Data, Class, Instance, Constructor, Field, Method, OrphanedInstance]

typeEditor :: IDERef -> Session -> Editor String
typeEditor ideR session para noti = do
    ed@(wid,inj,ext) <- multilineStringEditor para noti
    registerEvent noti ButtonRelease $Left (\e -> do
        res <- showInfoHandler wid ideR session
        return e {gtkReturn = res})
    return ed

typeAndCommentEditor :: IDERef -> Session -> Editor (String,String)
typeAndCommentEditor ideR session para noti =
    splitEditor (typeEditor ideR session,
                paraName  <<<- ParaName "Type"
                $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                $ emptyParams)
            (multilineStringEditor,
                paraName <<<- ParaName "Comment"
                $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                $ emptyParams)
                    (paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                    $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                    $ para) noti

symbolEditor :: IDERef -> Session -> Editor String
symbolEditor ideR session parameters notifier = do
    window       <- reflectIDE (readIDE window) ideR session
    ed@(w,i,ext) <- stringEditor (\_ -> True) parameters notifier
    registerEvent notifier AfterKeyRelease (Left (\ event -> do
        mbText   <- ext
        case mbText of
            Just t  -> do
                reflectIDE (do
                    triggerEvent ideR (SearchMeta t)
                    return ()) ideR session
                rw <- liftIO $ getRealWidget w
                when (isJust rw) $ liftIO $ do
                    widgetGrabFocus (fromJust rw)
                    editableSelectRegion (castToEditable (fromJust rw)) 0 0
                    editableSetPosition (castToEditable (fromJust rw)) (-1)
            Nothing -> return ()
        return event{gtkReturn=False}))
    return ed

showInfoHandler :: Widget -> IDERef -> Session -> IO Bool
showInfoHandler wid ideR session = do
    mbFrame    <- binGetChild (castToAlignment wid)
    mbInner    <- binGetChild (castToFrame (forceJust mbFrame "InfoPane>>typeEditor: Can't find child"))
    mbScrolled <- binGetChild (castToAlignment (forceJust mbInner "InfoPane>>typeEditor: Can't find child2"))	
    mbTV       <- binGetChild (castToScrolledWindow (forceJust mbScrolled "InfoPane>>typeEditor: Can't find child3"))	
    buf        <- textViewGetBuffer (castToTextView (forceJust mbTV "InfoPane>>typeEditor: Can't find child4"))
    (l,r)      <- textBufferGetSelectionBounds buf
    symbol     <- textBufferGetText buf l r True
    reflectIDE (triggerEvent ideR (SelectInfo symbol)) ideR session
    return False

initInfo :: PanePath -> Notebook -> Descr -> IDEAction
initInfo panePath nb idDescr = do
    panes       <- readIDE panes
    paneMap     <- readIDE paneMap
    prefs       <- readIDE prefs
    (pane,cids) <- reifyIDE $ \ideR session ->  do
            ibox        <- vBoxNew False 0
            bb          <- hButtonBoxNew
            buttonBoxSetLayout bb ButtonboxSpread
            definitionB <- buttonNewWithLabel "Source"
            moduB       <- buttonNewWithLabel "Modules"
            usesB       <- buttonNewWithLabel "Usage"
            docuB       <- buttonNewWithLabel "Docu"
            searchB     <- buttonNewWithLabel "Find"
            widgetSetSensitivity docuB False
            boxPackStartDefaults bb definitionB
            boxPackStartDefaults bb moduB
            boxPackStartDefaults bb usesB
            boxPackStartDefaults bb docuB
            boxPackStartDefaults bb searchB
            (widget,injb,ext,notifier) <-  buildEditor (idDescrDescr ideR session) idDescr
            boxPackStart ibox widget PackGrow 0
            boxPackEnd ibox bb PackNatural 0
            --openType
            sw            <- scrolledWindowNew Nothing Nothing
            scrolledWindowAddWithViewport sw ibox
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            currentDescr' <- newIORef idDescr
            let info = IDEInfo sw currentDescr' injb ext
            -- mapM_ (\w -> widgetSetExtensionEvents w [ExtensionEventsAll]) widgets
--            widget `onFocus` (\_ ->  do reflectIDE (makeActive info) ideR session
--                                        return False)
            definitionB `onClicked` (reflectIDE gotoSource ideR session)
            moduB `onClicked` (reflectIDE gotoModule' ideR session)
            usesB `onClicked` (reflectIDE calledBy' ideR session)
            searchB `onClicked` (do
                mbDescr <- ext idDescr
                case mbDescr of
                    Nothing -> return ()
                    Just descr -> reflectIDE (do
                                    triggerEvent ideR (SearchMeta (descrName' descr))
                                    showInfo) ideR session)
            notebookInsertOrdered nb sw (paneName info) Nothing
            widgetShowAll sw
            return (info,[])
    addPaneAdmin pane [] panePath
    liftIO $widgetGrabFocus (sw pane)
    liftIO $bringPaneToFront pane

gotoSource :: IDEAction
gotoSource = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  do  ideMessage Normal "gotoSource:noDefition"
                            return ()
        Just info   ->  goToDefinition info >> return ()

gotoModule' :: IDEAction
gotoModule' = do
    mbInfo  <-  getInfoCont
    ideR    <-  ask
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  triggerEvent ideR (SelectIdent info) >> return ()

calledBy' :: IDEAction
calledBy' = do
    mbInfo <- getInfoCont
    case mbInfo of
        Nothing     ->  return ()
        Just info   ->  calledBy info  >> return ()

setSymbol :: String -> IDEAction
setSymbol symbol = do
    ideR         <- ask
    currentInfo' <- readIDE currentInfo
    case currentInfo' of
        Nothing -> return ()
        Just ((_,symbolTable1),(_,symbolTable2)) ->
            case getIdentifierDescr symbol symbolTable1 symbolTable2 of
                []     -> return ()
                (a:r)  ->  do
                    setInfo a
                    showInfo
                    if length (a:r) > 1
                        then triggerEvent ideR (DescrChoice (a:r)) >> return ()
                        else triggerEvent ideR (SelectIdent a) >> return ()


setInfo :: Descr -> IDEAction
setInfo identifierDescr = do
    mbPane <-  getPane
    case mbPane of
        Nothing -> do
            prefs   <- readIDE prefs
            layout  <- readIDE layout
            let pp  =  getStandardPanePath (logPanePath prefs) layout
            nb      <- getNotebook pp
            initInfo pp nb identifierDescr
        Just info -> do
            oldDescr <- liftIO $ readIORef (currentDescr info)
            liftIO $ do
                writeIORef (currentDescr info) identifierDescr
                (injector info) identifierDescr
            recordInfoHistory identifierDescr oldDescr

getInfoCont ::  IDEM (Maybe (Descr))
getInfoCont = do
    mbPane <- getPane
    case mbPane of
        Nothing ->  return Nothing
        Just p  ->  liftIO $ readIORef (currentDescr p) >>= return . Just

--
-- | Lookup of an identifier description
--
getIdentifierDescr :: String -> SymbolTable -> SymbolTable -> [Descr]
getIdentifierDescr str st1 st2 =
    let r1 = case str `Map.lookup` st1 of
                Nothing -> []
                Just r -> r
        r2 = case str `Map.lookup` st2 of
                Nothing -> []
                Just r -> r
    in r1 ++ r2

-- * GUI History

recordInfoHistory :: Descr -> Descr -> IDEAction
recordInfoHistory  descr oldDescr = do
    ideR        <- ask
    triggerEvent ideR (RecordHistory
        ((InfoElementSelected descr),
         (InfoElementSelected oldDescr)))
    return ()

replayInfoHistory :: Descr -> IDEAction
replayInfoHistory descr = do
    setInfo descr


