-----------------------------------------------------------------------------
--
-- Module      :  GUI.Ghf.SimpleEditors
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for making simple editors
--
-----------------------------------------------------------------------------------

module GUI.Ghf.SimpleEditors (
    boolEditor
,   boolEditor2
,   stringEditor
,   multilineStringEditor
,   intEditor
,   genericEditor
,   fontEditor
,   staticSelectionEditor
,   staticMultiselectionEditor
,   multiselectionEditor
,   fileEditor
,   otherEditor
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import System.FilePath.Posix

import GUI.Ghf.Parameters
import GUI.Ghf.EditorBasics
import GUI.Ghf.MakeEditor

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
        (paraName <<<- (ParaName "") $ parameters)

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
        (paraName <<<- (ParaName "") $ parameters)

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

#ifdef False
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
                    box <- case getParameter paraDirection parameters of
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
                _   -> return Nothing
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
        (paraName <<<- (ParaName "") $ parameters)
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
