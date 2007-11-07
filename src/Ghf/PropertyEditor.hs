-----------------------------------------------------------------------------
--
-- Module      :  Ghf.PropertyEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for editing options in a GUI dialog
--
-----------------------------------------------------------------------------------

module Ghf.PropertyEditor (
-- * Types
    Getter
,   Setter
,   Injector
,   Extractor
,   Editor
--,   Applicator
,   Notifier

,   FieldDescriptionE(..)
,   mkFieldE
,   Default(..)
,   emptyNotifier
,   EventSelector(..)

,   Parameters(..)
,   emptyParams
,   getParameter

,   extractAndValidate

,   boolEditor
,   stringEditor
,   multilineStringEditor
,   intEditor
,   genericEditor
,   fontEditor

,   maybeEditor
,   pairEditor
,   eitherOrEditor
,   multisetEditor
,   ColumnDescr(..)

,   staticSelectionEditor
,   staticMultiselectionEditor
,   multiselectionEditor

,   fileEditor
,   otherEditor
) where

import Control.Monad(foldM)
import Graphics.UI.Gtk hiding (afterToggleOverwrite,Focus,onChanged)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.ModelView as New
import Text.ParserCombinators.Parsec hiding (Parser)
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map(Map,(!))
import Data.Maybe(isNothing,fromJust,catMaybes)
import Data.Version
import Data.IORef
import Data.Maybe(isJust)
import System.Directory
import Data.List(unzip4,elemIndex,filter)
import Data.Unique
import Text.ParserCombinators.ReadP(readP_to_S)
import Debug.Trace
import System.FilePath

import qualified Text.PrettyPrint.HughesPJ as PP

import Ghf.Core

-- ---------------------------------------------------------------------
-- * Basic Types
--

--
-- | A type for getting a field of a record
--
type Getter alpha beta     =   alpha -> beta
--
-- | A type for setting the field of a record
--
type Setter alpha beta     =   beta -> alpha -> alpha

--
-- | A type for injecting a value into an editor
--
type Injector beta     =   beta -> IO()
--
-- | A type for extracting a value from an editor
--
type Extractor beta    =   IO(Maybe (beta))

--
-- | A type for handling a gtk event
-- | Returning True: The event has been handles
-- | Returning False: Usual handling should proceed
type Handler        =   Event -> IO Bool

--
-- | A type to register or unregister a handler
-- | If the second argument is Left Handler the handler gets registered
-- | If the second argument is Right Unique the handler will be removed
-- | The returned unique value can be used for unregistering an event
type Notifier       =   EventSelector -> Either Handler Unique -> IO (Unique)

--
-- | A type to describe an editor.
-- | beta is the type of the individual field of the record
type Editor beta       =   Parameters -> IO(Widget, Injector beta, Extractor beta, Notifier)

--
-- | A constructor type for a field desciption
--
type MkFieldDescriptionE alpha beta =
    Parameters ->
    (Getter alpha beta) ->
    (Setter alpha beta) ->
    (Editor beta) ->
    FieldDescriptionE alpha

--
-- | A type to describe a field of a record, which can be edited
-- | alpha is the type of the individual field of the record
data FieldDescriptionE alpha =  FDE {
        parameters  ::  Parameters
    ,   fieldEditor ::  alpha -> IO (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
    }

data EventSelector  =   Clicked
                    |   FocusOut
                    |   FocusIn
                    |   SelectionChanged
    deriving (Eq,Ord,Show)
--
-- | A class for providing default values for certain types of editors
--
class Default a where
    getDefault      ::  a

instance Default Int where
    getDefault = 1

instance Default a => Default (Either a b)
    where
        getDefault =  Left(getDefault)

instance (Default alpha, Default beta) => Default (alpha,beta)
    where getDefault = (getDefault,getDefault)

instance Default [a] where
    getDefault = []


--type Applicator beta =   beta -> GhfAction ()

--
-- | A type for parameters for editors
--
data Parameters     =   Parameters  {
                        paraName        :: Maybe String
                    ,   synopsisP       :: Maybe String
                    ,   direction       :: Maybe Direction
                    ,   shadow          :: Maybe ShadowType
                    ,   outerAlignment  :: Maybe (Float,Float,Float,Float)
                                               -- | xalign yalign xscale yscale
                    ,   outerPadding    :: Maybe (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    ,   innerAlignment  :: Maybe (Float,Float,Float,Float)
                                                -- | xalign yalign xscale yscale
                    ,   innerPadding    :: Maybe (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    ,   minSize         :: Maybe (Int, Int)
                    ,   horizontal      :: Maybe Bool
    }

emptyParams     =   Parameters Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                        Nothing Nothing

--
-- | Convenience method to get a parameter, or if not set the default parameter
--
getParameter :: (Parameters -> (Maybe beta)) -> Parameters -> beta
getParameter selector parameters =
    let individual = selector parameters in
        case individual of
            Just it ->  it
            Nothing ->  case selector getDefault of
                            Just it -> it
                            Nothing -> error "default parameter should not be Nothing"

instance Default Parameters where
    getDefault      =   Parameters (Just "") (Just "") (Just Horizontal) (Just ShadowNone)
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3))
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3))
                            (Just (-1,-1)) Nothing

--
-- | Convenience method to validate and extract fields
--
extractAndValidate :: alpha -> [alpha -> Extractor alpha] -> [String] -> IO (Maybe alpha)
extractAndValidate val getExts fieldNames = do
    (newVal,errors) <- foldM (\ (val,errs) (ext,fn) -> do
        extVal <- ext val
        case extVal of
            Just nval -> return (nval,errs)
            Nothing -> return (val, (' ' : fn) : errs))
                (val,[]) (zip getExts fieldNames)
    if null errors
        then return (Just newVal)
        else do
            md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                        $ "The follwoing fields have invalid values." ++ concat (reverse errors)
            dialogRun md
            widgetDestroy md
            return Nothing

-- ------------------------------------------------------------
-- * Implementation of notifications
-- ------------------------------------------------------------

--
-- | A type for a function to register an event
--
type RegFunc        =   Widget -> Handler -> IO (ConnectId Widget)

--
-- | A type for the state of the notification system
--
type NotifierSt     =   IORef (Map EventSelector
                            (Maybe Widget,RegFunc,Maybe (ConnectId Widget),[(Unique, Handler)]))

--
-- | Initial state of the notification system
--
emptyNotifier        ::  IO (NotifierSt)
emptyNotifier        =   newIORef(Map.empty)

--
-- | Declare that the event can be thrown from this editor
--
declareEvent :: EventSelector -> RegFunc -> NotifierSt -> IO()
declareEvent eventSel regFunc notifierState = do
    noti <- readIORef notifierState
    case Map.lookup eventSel noti of
        Nothing -> do
             let noti2 = Map.insert eventSel (Nothing, regFunc, Nothing,[]) noti
             writeIORef notifierState noti2
        Just _ -> error $"editor has already declared event " ++ show eventSel

--
-- | Activate the event after the widget has been constructed
--
activateEvent :: Widget -> EventSelector -> NotifierSt -> IO()
activateEvent widget eventSel notifierState = do
    noti <- readIORef notifierState
    case Map.lookup eventSel noti of
        Nothing -> error $"editor has not declared event before activating it " ++ show eventSel
        Just (Nothing,registerFunc,Nothing,handlers) -> do
            cid <- registerFunc widget (\ e -> do
                noti <- readIORef notifierState
                case Map.lookup eventSel noti of
                    Nothing -> return False
                    Just (_,_,_,[]) -> return False
                    Just (_,_,_,handlers2) -> do
                        boolList <- mapM (\f -> f e) (map snd handlers2)
                        return (foldl (&&) True boolList))
            let noti2 = Map.insert eventSel (Just widget,registerFunc,Just cid,handlers) noti
            writeIORef notifierState noti2
        Just _ -> error $"editor has already been activated " ++ show eventSel

--
-- | Propagate the event with the selector from notifier to notifierst
--
propagateEvent :: EventSelector -> Notifier -> NotifierSt -> IO()
propagateEvent eventSel notiFrom notifierState = do
    noti <- readIORef notifierState
    case Map.lookup eventSel noti of
        Nothing -> error $"can't propagte event which is not activated " ++ show eventSel
        Just (mbWidget,registerFunc,Nothing,handlers) -> do
            cid <- notiFrom eventSel (Left (\ e -> do
                noti <- readIORef notifierState
                case Map.lookup eventSel noti of
                    Nothing -> return False
                    Just (_,_,_,[]) -> return False
                    Just (_,_,_,handlers2) -> do
                        boolList <- mapM (\f -> f e) (map snd handlers2)
                        return (foldl (&&) True boolList)))
            let noti2 = Map.insert eventSel (mbWidget,registerFunc,Nothing,handlers) noti
            writeIORef notifierState noti2
        Just _ -> error $"editor has already been activated " ++ show eventSel

--
-- | Constructor for a notifier
--
mkNotifier :: NotifierSt -> Notifier
mkNotifier notifierState = notFunc
    where
    notFunc :: EventSelector -> Either Handler Unique -> IO (Unique)
    notFunc eventSel (Left handler) = do
        noti <- readIORef notifierState
        uni <- newUnique
        case Map.lookup eventSel noti of
            Nothing -> error $"editor does not support event " ++ show eventSel
            Just (Just widget,registerFunc,Nothing,handlers)
                    -> error $"mkNotifier for activated event" ++ show eventSel
            Just (mbWidget, registerFunc, mbUnique, handlers)
                    -> do   unique <- newUnique
                            let noti2 = Map.insert eventSel
                                    (mbWidget,registerFunc,mbUnique,handlers++[(uni,handler)]) noti
                            writeIORef notifierState noti2
                            return unique
    notFunc eventSel (Right uni) = do
        noti <- readIORef notifierState
        case Map.lookup eventSel noti of
            Nothing -> error $"editor does not support event " ++ show eventSel
            Just (mbWidget,regFunc,Just cid,l) -> do
                let l2 = filter (\(u,_) -> u /= uni) l
                if null l2
                    then do
                        signalDisconnect cid
                        let noti2 = Map.insert eventSel (mbWidget,regFunc,Nothing,[]) noti
                        writeIORef notifierState noti2
                    else do
                        let noti2 = Map.insert eventSel (mbWidget,regFunc,Just cid,l2) noti
                        writeIORef notifierState noti2
        return uni

-- ------------------------------------------------------------
-- * Implementation of editing
-- ------------------------------------------------------------

--
-- | Function to construct a field description
--
mkFieldE :: Eq beta => MkFieldDescriptionE alpha beta
mkFieldE parameters getter setter editor =
    FDE parameters
        (\ dat -> do
            (widget, inj,ext,noti) <- editor parameters
            inj (getter dat)
            noti FocusOut (Left (\e -> do
                v <- ext
                case v of
                    Just _ -> do
                        return False
                    Nothing -> do
                        putStrLn "Validation Failure"
                        let message = case paraName parameters of
                                        Just s -> "in field " ++ s
                                        Nothing -> "in unnamed field"
                        dia <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                            ("Validation Failure " ++ message)
                        dialogRun dia
                        widgetDestroy dia
                        return False))
            return (widget,
                    (\a -> inj (getter a)),
                    (\a -> do
                        b <- ext
                        case b of
                            Just b -> return (Just (setter b a))
                            Nothing -> return Nothing),
                    noti))

-- | Function to construct an editor
--
mkEditor :: (Container -> Injector alpha) -> Extractor alpha -> Notifier -> Editor alpha
mkEditor injectorC extractor notifier parameters = do
    let (xalign, yalign, xscale, yscale) = getParameter outerAlignment parameters
    outerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter outerPadding parameters
    alignmentSetPadding outerAlig paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew
    frameSetShadowType frame (getParameter shadow parameters)
    case getParameter paraName parameters of
        "" -> return ()
        str -> frameSetLabel frame str
    containerAdd outerAlig frame
    let (xalign, yalign, xscale, yscale) =  getParameter innerAlignment parameters
    innerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter innerPadding parameters
    alignmentSetPadding innerAlig paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlig
    let (x,y) = getParameter minSize parameters
    widgetSetSizeRequest outerAlig x y
    return ((castToWidget) outerAlig, injectorC (castToContainer innerAlig), extractor, notifier)

-- ------------------------------------------------------------
-- * Simple Editors
-- ------------------------------------------------------------

instance ContainerClass Widget
instance BinClass Widget
instance ButtonClass Widget

--
-- | Editor for a boolean value in the form of a check button
--
boolEditor :: Editor Bool
boolEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent Clicked (\w h -> w `onClicked` do  h (Event True); return ()) notifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- checkButtonNewWithLabel (getParameter paraName parameters)
                    containerAdd widget button
                    toggleButtonSetActive button bool
                    activateEvent (castToWidget button) Clicked notifier
                    activateEvent (castToWidget button) FocusOut notifier
                    writeIORef coreRef (Just button)
                Just button -> toggleButtonSetActive button bool)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just button -> do
                    r <- toggleButtonGetActive button
                    return (Just r))
        (mkNotifier notifier)
        parameters {paraName = Just " "}

--
-- | Editor for a boolean value in the form of two radio buttons
--
boolEditor2 :: String -> Editor Bool
boolEditor2 label2 parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent Clicked (\w h -> w `onClicked` do  h (Event True); return ()) notifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- vBoxNew True 2
                    radio1 <- radioButtonNewWithLabel (getParameter paraName parameters)
                    radio2 <- radioButtonNewWithLabelFromWidget radio1 label2
                    boxPackStart box radio1 PackGrow 2
                    boxPackStart box radio2 PackGrow 2
                    containerAdd widget box
                    if bool
                        then do
                            toggleButtonSetActive radio1 True
                        else do
                            toggleButtonSetActive radio2 True
                    activateEvent (castToWidget radio1) Clicked notifier
                    activateEvent (castToWidget radio1) FocusOut notifier
                    writeIORef coreRef (Just (radio1,radio2))
                Just (radio1,radio2) ->
                    if bool
                        then do
                            toggleButtonSetActive radio1 True
                        else do
                            toggleButtonSetActive radio2 True)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (radio1,radio2) -> do
                    r <- toggleButtonGetActive radio1
                    return (Just r))
        (mkNotifier notifier)
        parameters {paraName = Just " "}

--
-- | Editor for a string in the form of a text entry
--
stringEditor :: Editor String
stringEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    entry   <-  entryNew
                    activateEvent (castToWidget entry) FocusOut notifier
                    containerAdd widget entry
                    entrySetText entry string
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry string)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    r <- entryGetText entry
                    return (Just r))
        (mkNotifier notifier)
        parameters

--
-- | Editor for a multiline string in the form of a multiline text entry
--
multilineStringEditor :: Editor String
multilineStringEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    textView   <-  textViewNew
                    containerAdd widget textView
                    activateEvent (castToWidget textView) FocusOut notifier
                    buffer <- textViewGetBuffer textView
                    textBufferSetText buffer string
                    writeIORef coreRef (Just textView)
                Just textView -> do
                    buffer <- textViewGetBuffer textView
                    textBufferSetText buffer string)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just textView -> do
                    buffer <- textViewGetBuffer textView
                    start <- textBufferGetStartIter buffer
                    end <- textBufferGetEndIter buffer
                    r <- textBufferGetText buffer start end False
                    return (Just r))
        (mkNotifier notifier)
        parameters

--
-- | Editor for an integer in the form of a spin entry
--
intEditor :: (Double,Double,Double) -> Editor Int
intEditor (min, max, step) parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    spin <- spinButtonNewWithRange min max step
                    activateEvent (castToWidget spin) FocusOut notifier
                    containerAdd widget spin
                    spinButtonSetValue spin (fromIntegral v)
                    writeIORef coreRef (Just spin)
                Just spin -> spinButtonSetValue spin (fromIntegral v))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just spin -> do
                    newNum <- spinButtonGetValue spin
                    return (Just (truncate newNum)))
        (mkNotifier notifier)
        parameters

--
-- | Editor for for any value which is an instance of Read and Show in the form of a
-- | text entry
genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor parameters = do
    (wid,inj,ext,notif) <- stringEditor parameters
    let ginj v = inj (show v)
    let gext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s ->
                let l = read s in
                if null l then
                    return Nothing
                    else return (Just (head l))
    return (wid,ginj,gext,notif)

#ifdef _Newgtk
--
-- | Editor for the selection of an element from a static list of elements in the
-- | form of a combo box
staticSelectionEditor :: (Show beta, Eq beta) => [beta] -> Editor beta
staticSelectionEditor list parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    (combo,_)   <-  New.comboBoxNewText show list
                    activateEvent (castToWidget combo) FocusOut notifier
                    New.comboBoxSetActive combo 1
                    containerAdd widget combo
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> New.comboBoxSetActive combo i
                        Nothing -> return ()
                    writeIORef coreRef (Just combo)
                Just combo -> do
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> New.comboBoxSetActive combo i
                        Nothing -> return ())
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just combo -> do
                    ind <- New.comboBoxGetActive combo
                    case ind of
                        i | i >= 0  -> return (Just (list !! i))
                        otherwise   -> return Nothing)
        (mkNotifier notifier)
        parameters

#else
staticSelectionEditor :: (Show beta, Eq beta) => [beta] -> Editor beta
staticSelectionEditor list parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    combo   <-  New.comboBoxNewText
                    activateEvent (castToWidget combo) FocusOut notifier
                    New.comboBoxSetActive combo 1
                    containerAdd widget combo
                    mapM_ (\t -> New.comboBoxAppendText combo (show t)) list
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> New.comboBoxSetActive combo i
                        Nothing -> return ()
                    writeIORef coreRef (Just combo)
                Just combo -> do
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> New.comboBoxSetActive combo i
                        Nothing -> return ())
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just combo -> do
                    mbInd <- New.comboBoxGetActive combo
                    case mbInd of
                        Just i  -> return (Just (list !! i))
                        otherwise   -> return Nothing)
        (mkNotifier notifier)
        parameters
#endif


--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box

multiselectionEditor :: (Show beta, Eq beta) => Editor [beta]
multiselectionEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore   <- New.listStoreNew ([]:: [alpha])
                    listView    <- New.treeViewNewWithModel listStore
                    activateEvent (castToWidget listView) FocusOut notifier
                    sel         <- New.treeViewGetSelection listView
                    New.treeSelectionSetMode sel SelectionMultiple
                    renderer    <- New.cellRendererTextNew
                    col         <- New.treeViewColumnNew
                    New.treeViewAppendColumn listView col
                    New.cellLayoutPackStart col renderer True
                    New.cellLayoutSetAttributes col renderer listStore $ \row -> [ New.cellText := show row ]
                    New.treeViewSetHeadersVisible listView False
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) objs
                    containerAdd widget listView
                    New.treeSelectionUnselectAll sel
                    --let inds = catMaybes $map (\obj -> elemIndex obj list) objs
                    --mapM_ (\i -> New.treeSelectionSelectPath sel [i]) inds
                    writeIORef coreRef (Just (listView,listStore))
                Just (listView,listStore) -> do
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) objs)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listView,listStore) -> do
                    sel         <- New.treeViewGetSelection listView
                    treePath    <- New.treeSelectionGetSelectedRows sel
                    values      <- mapM (\[i] -> listStoreGetValue listStore i) treePath
                    return (Just values))
        (mkNotifier notifier)
        parameters

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box

staticMultiselectionEditor :: (Show beta, Eq beta) => [beta] -> Editor [beta]
staticMultiselectionEditor list parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore <- New.listStoreNew ([]:: [alpha])
                    listView <- New.treeViewNewWithModel listStore
                    activateEvent (castToWidget listView) FocusOut notifier
                    sel <- New.treeViewGetSelection listView
                    New.treeSelectionSetMode sel SelectionMultiple
                    renderer <- New.cellRendererTextNew
                    col <- New.treeViewColumnNew
                    New.treeViewAppendColumn listView col
                    New.cellLayoutPackStart col renderer True
                    New.cellLayoutSetAttributes col renderer listStore $ \row -> [ New.cellText := show row ]
                    New.treeViewSetHeadersVisible listView False
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) list
                    containerAdd widget listView
                    New.treeSelectionUnselectAll sel
                    let inds = catMaybes $map (\obj -> elemIndex obj list) objs
                    mapM_ (\i -> New.treeSelectionSelectPath sel [i]) inds
                    writeIORef coreRef (Just listView)
                Just listView -> do
                    sel <- New.treeViewGetSelection listView
                    New.treeSelectionUnselectAll sel
                    let inds = catMaybes $map (\obj -> elemIndex obj list) objs
                    mapM_ (\i -> New.treeSelectionSelectPath sel [i]) inds)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listView -> do
                    sel <- New.treeViewGetSelection listView
                    treePath <- New.treeSelectionGetSelectedRows sel
                    return (Just (map (\[i] -> list !! i) treePath)))
        (mkNotifier notifier)
        parameters

--
-- | Editor for the selection of a file path in the form of a text entry and a button,
-- | which opens a gtk file chooser
fileEditor :: Maybe FilePath -> FileChooserAction -> String -> Editor FilePath
fileEditor mbFilePath action buttonName parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent Clicked
        (\widget handler -> do  widget `onClicked` do
                                    handler (Event True)
                                    return ()) notifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget filePath -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel buttonName
                    entry   <-  entryNew
                    set entry [ entryEditable := False ]
                    activateEvent (castToWidget button) Clicked notifier
                    (mkNotifier notifier) Clicked (Left (buttonHandler entry))
                    box <- case getParameter direction parameters of
                                Horizontal  -> do
                                    r <- hBoxNew False 1
                                    return (castToBox r)
                                Vertical    -> do
                                    r <- vBoxNew False 1
                                    return (castToBox r)
                    activateEvent (castToWidget button) FocusOut notifier
                    boxPackStart box entry PackGrow 0
                    boxPackEnd box button PackNatural 0
                    containerAdd widget box
                    entrySetText entry filePath
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry filePath)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    str <- entryGetText entry
                    return (Just str))
        (mkNotifier notifier)
        parameters
    where
    buttonHandler entry = (\ e -> do
        mbFileName <- do
            dialog <- fileChooserDialogNew
                            (Just $ "Select File")
                            Nothing
                        action
                        [("gtk-cancel"
                        ,ResponseCancel)
                        ,("gtk-open"
                        ,ResponseAccept)]
            widgetShow dialog
            response <- dialogRun dialog
            case response of
                ResponseAccept -> do
                    f <- fileChooserGetFilename dialog
                    widgetDestroy dialog
                    return f
                ResponseCancel -> do
                    widgetDestroy dialog
                    return Nothing
                ResponseDeleteEvent-> do
                    widgetDestroy dialog
                    return Nothing
        case mbFileName of
            Nothing -> return True
            Just fn -> do
                let relative = case mbFilePath of
                                Nothing -> fn
                                Just rel -> makeRelative rel fn
                entrySetText entry relative
                return True)

--
-- | Editor for a font selection
--
fontEditor :: Editor (Maybe String)
fontEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` h) notifier
    mkEditor
        (\widget mbValue -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    fs <- fontButtonNew
                    activateEvent (castToWidget fs) FocusOut notifier
                    containerAdd widget fs
                    case mbValue of
                        Nothing -> return True
                        Just s  -> fontButtonSetFontName fs s
                    writeIORef coreRef (Just fs)
                Just fs ->   case mbValue of
                                Nothing -> return ()
                                Just s  -> do
                                    fontButtonSetFontName fs s
                                    return ())
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just fs -> do
                    f <- fontButtonGetFontName fs
                    return (Just (Just f)))
        (mkNotifier notifier)
        parameters

--
-- | An editor, which opens another editor
--   You have to inject a value before the button can be clicked.
--
otherEditor :: (alpha  -> String -> IO (Maybe alpha)) -> Editor alpha
otherEditor func parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent Clicked (\w h -> w `onClicked` do  h (Event True); return ()) notifier
    declareEvent FocusIn (\w h -> w `onFocusIn` do  h) notifier
    declareEvent FocusOut (\w h -> w `onFocusOut` do  h) notifier
    mkEditor
        (\widget val -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName parameters)
                    containerAdd widget button
                    activateEvent (castToWidget button) Clicked notifier
                    activateEvent (castToWidget button) FocusIn notifier
                    activateEvent (castToWidget button) FocusOut notifier
                    (mkNotifier notifier) Clicked (Left (buttonHandler coreRef))
                    writeIORef coreRef (Just (button,val))
                Just (button, oldval) -> writeIORef coreRef (Just (button, val)))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,val) -> return (Just val))
        (mkNotifier notifier)
        parameters {paraName = Just " "}
    where
    buttonHandler coreRef = (\ e -> do
        core <- readIORef coreRef
        case core of
            Nothing -> error "You have to inject a value before the button can be clicked"
            Just (b,val) -> do
                res <- func val (getParameter paraName parameters)
                case res of
                    Nothing     -> return True
                    Just nval   -> do
                        writeIORef coreRef (Just (b, nval))
                        return True)



-- ------------------------------------------------------------
-- * Composition editors
-- ------------------------------------------------------------

--
-- | An editor which composes two subeditors
--
pairEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
pairEditor (fstEd,fstPara) (sndEd,sndPara) parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` do  h) notifier
    mkEditor
        (\widget (v1,v2) -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    fst@(fstFrame,inj1,ext1,noti1) <- fstEd fstPara
                    snd@(sndFrame,inj2,ext2,noti2) <- sndEd sndPara
                    box <- case getParameter direction parameters of
                        Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box fstFrame PackGrow 0
                    boxPackStart box sndFrame PackGrow 0
                    containerAdd widget box
                    propagateEvent FocusOut noti2 notifier
                    inj1 v1
                    inj2 v2
                    writeIORef coreRef (Just (fst,snd))
                Just ((_,inj1,_,_),(_,inj2,_,_)) -> do
                    inj1 v1
                    inj2 v2)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1,_),(_,_,ext2,_)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)
        (mkNotifier notifier)
        parameters

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- | or deselected (if the positive Argument is False)
--
maybeEditor :: Default beta => (Editor beta, Parameters) -> Bool -> String -> Editor (Maybe beta)
maybeEditor (childEdit, childParams) positive boolLabel parameters = do
    coreRef <- newIORef Nothing
    childRef  <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` do  h) notifier
    mkEditor
        (\widget mbVal -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    be@(boolFrame,inj1,ext1,notifierBool) <- boolEditor
                            emptyParams {paraName = Just boolLabel}
                    propagateEvent FocusOut notifierBool notifier
                    box <- case getParameter direction parameters of
                        Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box boolFrame PackNatural 0
                    containerAdd widget box
                    notifierBool Clicked (Left (onClickedHandler widget coreRef childRef notifier))
                    case mbVal of
                        Nothing -> do
                            inj1 (if positive then False else True)
                        Just val -> do
                            (childWidget,inj2,ext2,noti2) <- getChildEditor childRef childEdit childParams notifier
                            boxPackEnd box childWidget PackNatural 0
                            widgetShowAll childWidget
                            inj1 (if positive then True else False)
                            inj2 val
                    writeIORef coreRef (Just (be,box))
                Just (be@(boolFrame,inj1,ext1,notiRef1),box) -> do
                    hasChild <- hasChildEditor childRef
                    case mbVal of
                        Nothing -> do
                            if hasChild
                                then do
                                    (childWidget,_,_,_) <- getChildEditor childRef childEdit childParams notifier
                                    inj1 (if positive then False else True)
                                    widgetHideAll childWidget
                                else inj1 (if positive then False else True)
                        Just val -> do
                            if hasChild
                                then do
                                    (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams notifier
                                    widgetShowAll childWidget
                                    inj2 val
                                else do
                                    (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams notifier
                                    boxPackEnd box childWidget PackNatural 0
                                    widgetShowAll childWidget
                                    inj2 val)
        (do
            core <- readIORef coreRef
            case core of
                Nothing  -> return Nothing
                Just (be@(boolFrame,inj1,ext1,notiRef1),_) -> do
                    bool <- ext1
                    case bool of
                        Nothing -> return Nothing
                        Just bv | bv == positive -> do
                            (_,_,ext2,_) <- getChildEditor childRef childEdit childParams notifier
                            value <- ext2
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Just value))
                        otherwise -> return (Just Nothing))
        (mkNotifier notifier)
        parameters
    where
    onClickedHandler widget coreRef childRef notifier = (\ event -> do
        core <- readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just (be@(boolFrame,inj1,ext1,notiRef1),vBox) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                        if not (bool == positive)
                            then do
                                hasChild <- hasChildEditor childRef
                                if hasChild
                                    then do
                                        (childWidget,_,_,_) <- getChildEditor childRef childEdit childParams notifier
                                        widgetHideAll childWidget
                                    else return ()
                            else do
                                hasChild <- hasChildEditor childRef
                                if hasChild
                                    then do
                                        (childWidget,_,_,_) <- getChildEditor childRef childEdit childParams notifier
                                        widgetShowAll childWidget
                                    else do
                                        (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams notifier
                                        boxPackEnd vBox childWidget PackNatural 0
                                        inj2 getDefault
                                        widgetShowAll childWidget
                    Nothing -> return ()
                return True)
    getChildEditor childRef childEditor childParams notifier =  do
        mb <- readIORef childRef
        case mb of
            Just editor -> return editor
            Nothing -> do
                let val = childEditor
                editor@(_,_,_,cnoti) <- childEditor childParams
                propagateEvent FocusOut cnoti notifier
                writeIORef childRef (Just editor)
                return editor
    hasChildEditor childRef =  do
        mb <- readIORef childRef
        return (isJust mb)

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- | or deselected (if the positive Argument is False)
eitherOrEditor :: (Default alpha, Default beta) => (Editor alpha, Parameters) ->
                        (Editor beta, Parameters) -> String -> Editor (Either alpha beta)
eitherOrEditor (leftEditor,leftParams) (rightEditor,rightParams) label2 parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` do  h) notifier
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    be@(boolFrame,inj1,ext1,noti1) <- boolEditor2  label2 parameters
                    le@(leftFrame,inj2,ext2,noti2) <- leftEditor leftParams
                    re@(rightFrame,inj3,ext3,noti3) <- rightEditor rightParams
                    noti1 Clicked (Left (onClickedHandler widget coreRef))
                    propagateEvent FocusOut noti2 notifier
                    propagateEvent FocusOut noti3 notifier
                    box <- case getParameter direction parameters of
                        Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box boolFrame PackNatural 0
                    activateEvent (castToWidget box) FocusOut notifier
                    containerAdd widget box
                    case v of
                        Left vl -> do
                          boxPackStart box leftFrame PackNatural 0
                          inj2 vl
                          inj3 getDefault
                          inj1 True
                        Right vr  -> do
                          boxPackStart box rightFrame PackNatural 0
                          inj3 vr
                          inj2 getDefault
                          inj1 False
                    writeIORef coreRef (Just (be,le,re,box))
                Just ((_,inj1,_,_),(leftFrame,inj2,_,_),(rightFrame,inj3,_,_),box) -> do
                    case v of
                            Left vl -> do
                              containerRemove box rightFrame
                              boxPackStart box leftFrame PackNatural 0
                              inj2 vl
                              inj3 getDefault
                              inj1 True
                            Right vr  -> do
                              containerRemove box leftFrame
                              boxPackStart box rightFrame PackNatural 0
                              inj3 vr
                              inj2 getDefault
                              inj1 False)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1,_),(_,_,ext2,_),(_,_,ext3,_),_) -> do
                    mbbool <- ext1
                    case mbbool of
                        Nothing -> return Nothing
                        Just True   ->  do
                            value <- ext2
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Left value))
                        Just False -> do
                            value <- ext3
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Right value)))
        (mkNotifier notifier)
        parameters {paraName = Just " "}
    where
    onClickedHandler widget coreRef = (\ event -> do
        core <- readIORef coreRef
        case core of
            Nothing  -> error "Impossible"
            Just (be@(_,_,ext1,_),(leftFrame,_,_,_),(rightFrame,_,_,_),box) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                            if bool then do
                              containerRemove box rightFrame
                              boxPackStart box leftFrame PackNatural 0
                              widgetShowAll box
                            else do
                              containerRemove box leftFrame
                              boxPackStart box rightFrame PackNatural 0
                              widgetShowAll box
                    Nothing -> return ()
                return True)


-- a trivial example: (ColumnDescr False [("",(\row -> [New.cellText := show row]))])
-- and a nontrivial:
--  [("Package",\(Dependency str _) -> [New.cellText := str])
--  ,("Version",\(Dependency _ vers) -> [New.cellText := showVersionRange vers])])
data ColumnDescr row = ColumnDescr Bool [(String,(row -> [AttrOp CellRendererText]))]

--
-- | An editor with a subeditor, of which a list of items can be selected
multisetEditor :: (Show alpha, Default alpha) =>
                    ColumnDescr alpha -> (Editor alpha, Parameters) -> Editor [alpha]
multisetEditor (ColumnDescr showHeaders columnsDD) (singleEditor, sParams) parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent FocusOut (\w h -> w `onFocusOut` do  h) notifier
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    (box,buttonBox) <- case getParameter direction parameters of
                        Horizontal -> do
                            b  <- hBoxNew False 1
                            bb <- vButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                        Vertical -> do
                            b  <- vBoxNew False 1
                            bb <- hButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                    (frameS,injS,extS,notS) <- singleEditor sParams
                    addButton <- buttonNewWithLabel "Add"
                    removeButton <- buttonNewWithLabel "Remove"
                    containerAdd buttonBox addButton
                    containerAdd buttonBox removeButton
                    listStore <- New.listStoreNew ([]:: [alpha])
                    list <- New.treeViewNewWithModel listStore
                    sel <- New.treeViewGetSelection list
                    New.treeSelectionSetMode sel SelectionSingle
                    mapM_ (\(str,func) -> do
                            col <- New.treeViewColumnNew
                            New.treeViewColumnSetTitle col str
                            New.treeViewColumnSetResizable col True
                            New.treeViewAppendColumn list col
                            renderer <- New.cellRendererTextNew
                            New.cellLayoutPackStart col renderer True
                            New.cellLayoutSetAttributes col renderer listStore func
                        ) columnsDD
                    New.treeViewSetHeadersVisible list showHeaders
                    sel  `New.onSelectionChanged` (selectionHandler sel listStore injS)
                    boxPackStart box list PackNatural 0
                    boxPackStart box buttonBox PackNatural 0
                    boxPackEnd box frameS PackGrow 0
                    activateEvent (castToWidget list) FocusOut notifier
                    containerAdd widget box
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) v
                    addButton `onClicked` do
                        mbv <- extS
                        case mbv of
                            Just v -> do
                              New.listStoreAppend listStore v
                              return ()
                            Nothing -> return ()
                    removeButton `onClicked` do
                        mbi <- New.treeSelectionGetSelected sel
                        case mbi of
                            Nothing -> return ()
                            Just iter -> do
                                [i] <- New.treeModelGetPath listStore iter
                                New.listStoreRemove listStore i
                    writeIORef coreRef (Just listStore)
                    injS getDefault
                Just listStore -> do
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) v)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listStore -> do
                    v <- listStoreGetValues listStore
                    return (Just v))
        (mkNotifier notifier)
        parameters
    where
    listStoreGetValues :: New.ListStore a -> IO [a]
    listStoreGetValues listStore = do
        mbi <- New.treeModelGetIterFirst listStore
        getTail mbi
        where getTail mbi = case mbi of
                                Nothing -> return []
                                Just iter -> do
                                    [i] <- New.treeModelGetPath listStore iter
                                    v <- New.listStoreGetValue listStore i
                                    mbi2 <- New.treeModelIterNext listStore iter
                                    rest <- getTail mbi2
                                    return (v : rest)
    selectionHandler :: New.TreeSelection -> New.ListStore a -> Injector a -> IO ()
    selectionHandler sel listStore inj = do
        ts <- New.treeSelectionGetSelected sel
        case ts of
            Nothing -> return ()
            Just iter -> do
                [i] <- New.treeModelGetPath listStore iter
                v <- New.listStoreGetValue listStore i
                inj v
                return ()

