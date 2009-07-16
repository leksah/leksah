-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Simple
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making simple editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Simple (
    boolEditor
,   boolEditor2
,   clickEditor
,   stringEditor
,   multilineStringEditor
,   intEditor
,   genericEditor
,   fontEditor
,   comboSelectionEditor
,   staticListEditor
,   staticListMultiEditor
,   multiselectionEditor
,   fileEditor
,   otherEditor
,   imageEditor

,   okCancelFields
) where

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk as Gtk
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import System.FilePath.Posix

import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Control.Event
import Graphics.UI.Gtk.Gdk.Events (Event(..))

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
boolEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- checkButtonNewWithLabel (getParameter paraName parameters)
                    widgetSetName button (getParameter paraName parameters)
                    containerAdd widget button
                    toggleButtonSetActive button bool
                    mapM_ (activateEvent (castToWidget button) notifier Nothing)
                        [Clicked,FocusOut,FocusIn]
                    writeIORef coreRef (Just button)
                Just button -> toggleButtonSetActive button bool)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just button -> do
                    r <- toggleButtonGetActive button
                    return (Just r))
        (paraName <<<- ParaName "" $ parameters)
        notifier

--
-- | Editor for a boolean value in the form of two radio buttons
--
boolEditor2 :: String -> Editor Bool
boolEditor2 label2 parameters notifier = do
    coreRef <- newIORef Nothing
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
                    widgetSetName radio1 $ getParameter paraName parameters ++ ".1"
                    widgetSetName radio2 $ getParameter paraName parameters ++ ".2"
                    containerAdd widget box
                    if bool
                        then toggleButtonSetActive radio1 True
                        else toggleButtonSetActive radio2 True
                    mapM_ (activateEvent (castToWidget radio1) notifier Nothing)
                                [Clicked,FocusOut,FocusIn]
                    mapM_ (activateEvent (castToWidget radio2) notifier Nothing)
                                [Clicked,FocusOut,FocusIn]
                    writeIORef coreRef (Just (radio1,radio2))
                Just (radio1,radio2) ->
                    if bool
                        then toggleButtonSetActive radio1 True
                        else toggleButtonSetActive radio2 True)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (radio1,radio2) -> do
                    r <- toggleButtonGetActive radio1
                    return (Just r))
        (paraName <<<- ParaName "" $ parameters)
        notifier

-- | An Editor for nothing (which may report a click) in the form of a button
--
clickEditor :: Editor ()
clickEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget bool -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- case getParameter paraStockId parameters of
                        "" ->   buttonNewWithLabel (getParameter paraName parameters)
                        st ->   buttonNewFromStock st
                    widgetSetName button (getParameter paraName parameters)
                    containerAdd widget button
                    activateEvent (castToWidget button) notifier Nothing Clicked
                    writeIORef coreRef (Just button)
                Just button -> return ())
        (return (Just ()))
        (paraName <<<- ParaName "" $ parameters)
        notifier

-- | An Editor to display an image
--
imageEditor :: Editor StockId
imageEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget stockId -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    image <- imageNewFromStock stockId IconSizeLargeToolbar
                    widgetSetName image (getParameter paraName parameters)
                    containerAdd widget image
                    writeIORef coreRef (Just (image,stockId))
                Just (image,stockId2) -> imageSetFromStock image stockId IconSizeLargeToolbar)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,stockId3) -> return (Just stockId3))
        parameters
        notifier

--
-- | Editor for a string in the form of a text entry
--
stringEditor :: (String -> Bool) -> Editor String
stringEditor validation parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    entry   <-  entryNew
                    widgetSetName entry (getParameter paraName parameters)
                    mapM_ (activateEvent (castToWidget entry) notifier Nothing)
                                [FocusOut,FocusIn,AfterKeyRelease]
                    containerAdd widget entry
                    entrySetText entry string
                    writeIORef coreRef (Just entry)
                Just entry -> entrySetText entry string)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just entry -> do
                    r <- entryGetText entry
                    if validation r
                        then return (Just r)
                        else return Nothing)
        parameters
        notifier

--
-- | Editor for a multiline string in the form of a multiline text entry
--
multilineStringEditor :: Editor String
multilineStringEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget string -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    aTextView       <-  textViewNew
                    widgetSetName aTextView (getParameter paraName parameters)
                    aScrolledWindow <-  scrolledWindowNew Nothing Nothing
                    scrolledWindowSetPolicy aScrolledWindow PolicyAutomatic PolicyAutomatic
                    containerAdd aScrolledWindow aTextView
                    containerAdd widget aScrolledWindow
                    mapM_ (activateEvent (castToWidget aTextView) notifier Nothing)
                        [ButtonRelease,FocusOut,FocusIn]
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string
                    writeIORef coreRef (Just (aScrolledWindow,aTextView))
                Just (aScrolledWindow,aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    textBufferSetText buffer string)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (aScrolledWindow, aTextView) -> do
                    buffer          <-  textViewGetBuffer aTextView
                    start           <-  textBufferGetStartIter buffer
                    end             <-  textBufferGetEndIter buffer
                    r               <-  textBufferGetText buffer start end False
                    return (Just r))
        parameters
        notifier

--
-- | Editor for an integer in the form of a spin entry
--
intEditor :: (Double,Double,Double) -> Editor Int
intEditor (min, max, step) parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    spin <- spinButtonNewWithRange min max step
                    widgetSetName spin (getParameter paraName parameters)
                    mapM_ (activateEvent (castToWidget spin) notifier Nothing)
                                [FocusOut,FocusIn]
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
        parameters
        notifier

--
-- | Editor for for any value which is an instance of Read and Show in the form of a
-- | text entry
genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor parameters notifier = do
    (wid,inj,ext) <- stringEditor (const True) parameters notifier
    let ginj = inj . show
    let gext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> catch (liftM Just (readIO s))
                            (\e -> do
                                putStrLn ("Generic editor no parse for " ++ s ++ " " ++ show e)
                                return Nothing)
    return (wid,ginj,gext)

--
-- | Editor for no value, it only emtis a clicked event and has the form of a check button
--
buttonEditor :: Editor ()
buttonEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget _ -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName parameters)
                    widgetSetName button (getParameter paraName parameters)
                    containerAdd widget button
                    mapM_ (activateEvent (castToWidget button) notifier Nothing)
                                [Clicked,FocusIn]
                    writeIORef coreRef (Just button)
                Just button -> return ())
        (return (Just ()))
        parameters
        notifier

--
-- | Editor for the selection of some element from a static list of elements in the
-- | form of a combo box

comboSelectionEditor :: Eq beta => [beta] -> (beta -> String) -> Editor beta
comboSelectionEditor list showF parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    combo <- comboBoxNewText
                    mapM_ (\o -> comboBoxAppendText combo (showF o)) list
                    widgetSetName combo (getParameter paraName parameters)
                    mapM_ (activateEvent (castToWidget combo) notifier Nothing)
                            [FocusOut,FocusIn]
                    comboBoxSetActive combo 1
                    containerAdd widget combo
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo i
                        Nothing -> return ()
                    writeIORef coreRef (Just combo)
                Just combo -> do
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> comboBoxSetActive combo i
                        Nothing -> return ())
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just combo -> do
                    ind <- comboBoxGetActive combo
                    case ind of
                        (-1)   -> return Nothing
                        otherwise  -> return (Just (list !! ind)))
        parameters
        notifier

--
-- | Editor for the selection of some elements from a list of elements in the
-- | form of a list box
multiselectionEditor :: (Show beta, Eq beta) => Editor [beta]
multiselectionEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore   <- listStoreNew ([]:: [alpha])
                    listView    <- treeViewNewWithModel listStore
                    widgetSetName listView (getParameter paraName parameters)
                    mapM_ (activateEvent (castToWidget listView) notifier Nothing)
                            [FocusOut,FocusIn]
                    sel         <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionMultiple
                    renderer    <- cellRendererTextNew
                    col         <- treeViewColumnNew
                    treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetAttributes col renderer listStore
                        $ \row -> [ cellText := show row ]
                    treeViewSetHeadersVisible listView False
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) objs
                    containerAdd widget listView
                    treeSelectionUnselectAll sel
                    --let inds = catMaybes $map (\obj -> elemIndex obj list) objs
                    --mapM_ (\i -> treeSelectionSelectPath sel [i]) inds
                    writeIORef coreRef (Just (listView,listStore))
                Just (listView,listStore) -> do
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) objs)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listView,listStore) -> do
                    sel         <- treeViewGetSelection listView
                    treePath    <- treeSelectionGetSelectedRows sel
                    values      <- mapM (\[i] -> listStoreGetValue listStore i) treePath
                    return (Just values))
        parameters
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box with toggle elements

staticListMultiEditor :: (Eq beta) => [beta] -> (beta -> String) -> Editor [beta]
staticListMultiEditor list showF parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget objs -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore <- listStoreNew ([]:: [(Bool,beta)])
                    listView <- treeViewNewWithModel listStore
                    widgetSetName listView (getParameter paraName parameters)
                    mapM_ (activateEvent (castToWidget listView) notifier Nothing)
                            [FocusOut,FocusIn]
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel SelectionSingle
                    rendererToggle <- cellRendererToggleNew
                    set rendererToggle [cellToggleActivatable := True]
                    rendererText <- cellRendererTextNew
                    col1 <- treeViewColumnNew
                    treeViewAppendColumn listView col1
                    cellLayoutPackStart col1 rendererToggle True
                    cellLayoutSetAttributes col1 rendererToggle listStore
                        $ \row -> [ cellToggleActive := fst row]
                    col2 <- treeViewColumnNew
                    treeViewAppendColumn listView col2
                    cellLayoutPackStart col2 rendererText True
                    cellLayoutSetAttributes col2 rendererText listStore
                        $ \row -> [ cellText := showF (snd row)]
                    treeViewSetHeadersVisible listView False
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) $ map (\e -> (elem e objs,e)) list
                    let minSize =   getParameter paraMinSize parameters
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw          <-  scrolledWindowNew Nothing Nothing
                    containerAdd sw listView
                    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                    containerAdd widget sw
                      -- update the model when the toggle buttons are activated
                    on rendererToggle cellToggled $ \pathStr -> do
                        let (i:_) = stringToTreePath pathStr
                        val <- listStoreGetValue listStore i
                        listStoreSetValue listStore i (not (fst val),snd val)
                    listView `onKeyPress` (\event -> do
                        let Key { eventKeyName = name, eventModifier = modifier, eventKeyChar = char } = event
                        case (name, modifier, char) of
                            ("Return", _, _) -> do
                                sel <- treeViewGetSelection listView
                                rows <- treeSelectionGetSelectedRows sel
                                mapM_ (\ (i:_) -> do
                                    val <- listStoreGetValue listStore i
                                    listStoreSetValue listStore i (not (fst val),snd val)) rows
                                return True
                            _ -> return False)
                    writeIORef coreRef (Just (listView,listStore))
                Just (listView,listStore) -> do
                    let model = map (\e -> (elem e objs,e)) list
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) $ map (\e -> (elem e objs,e)) list)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (listView,listStore) -> do
                    model <- listStoreToList listStore
                    return (Just (map snd $ filter (\e -> fst e) model)))
        parameters
        notifier

--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box

staticListEditor :: (Eq beta) => [beta] -> (beta -> String) -> Editor beta
staticListEditor list showF parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget obj -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    listStore <- listStoreNew ([]:: [alpha])
                    listView <- treeViewNewWithModel listStore
                    widgetSetName listView (getParameter paraName parameters)
                    mapM_ (activateEvent (castToWidget listView) notifier Nothing)
                            [FocusOut,FocusIn]
                    sel <- treeViewGetSelection listView
                    treeSelectionSetMode sel
                        (case getParameter paraMultiSel parameters of
                            True  -> SelectionMultiple
                            False -> SelectionSingle)
                    renderer <- cellRendererTextNew
                    col <- treeViewColumnNew
                    treeViewAppendColumn listView col
                    cellLayoutPackStart col renderer True
                    cellLayoutSetAttributes col renderer listStore
                        $ \row -> [ cellText := showF row ]
                    treeViewSetHeadersVisible listView False
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) list
                    let minSize =   getParameter paraMinSize parameters
                    uncurry (widgetSetSizeRequest listView) minSize
                    sw          <-  scrolledWindowNew Nothing Nothing
                    containerAdd sw listView
                    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                    containerAdd widget sw
                    treeSelectionUnselectAll sel
                    let mbInd = elemIndex obj list
                    case mbInd of
                        Nothing -> return ()
                        Just ind -> treeSelectionSelectPath sel [ind]
                    writeIORef coreRef (Just listView)
                Just listView -> do
                    sel <- treeViewGetSelection listView
                    treeSelectionUnselectAll sel
                    let mbInd = elemIndex obj list
                    case mbInd of
                        Nothing -> return ()
                        Just ind -> treeSelectionSelectPath sel [ind])
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listView -> do
                    sel <- treeViewGetSelection listView
                    treePaths <- treeSelectionGetSelectedRows sel
                    case treePaths of
                        [[i]] -> return (Just (list !! i))
                        _ -> return Nothing)
        parameters
        notifier


--
-- | Editor for the selection of a file path in the form of a text entry and a button,
-- | which opens a gtk file chooser
fileEditor :: Maybe FilePath -> FileChooserAction -> String -> Editor FilePath
fileEditor mbFilePath action buttonName parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget filePath -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel buttonName
                    widgetSetName button $ getParameter paraName parameters ++ "-button"
                    mapM_ (activateEvent (castToWidget button) notifier Nothing)
                            [FocusOut,FocusIn,Clicked]
                    entry   <-  entryNew
                    widgetSetName entry $ getParameter paraName parameters ++ "-entry"
                    -- set entry [ entryEditable := False ]
                    mapM_ (activateEvent (castToWidget entry) notifier Nothing)
                            [FocusOut,FocusIn]
                    registerEvent notifier Clicked (Left (buttonHandler entry))
                    box <- case getParameter paraDirection parameters of
                                Horizontal  -> do
                                    r <- hBoxNew False 1
                                    return (castToBox r)
                                Vertical    -> do
                                    r <- vBoxNew False 1
                                    return (castToBox r)
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
        parameters
        notifier
    where
    buttonHandler entry e =  do
        mbFileName <- do
            dialog <- fileChooserDialogNew
                            (Just "Select File")
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
            Nothing -> return (e{gtkReturn=True})
            Just fn -> do
                let relative = case mbFilePath of
                                Nothing -> fn
                                Just rel -> makeRelative rel fn
                entrySetText entry relative
--                triggerEvent notifier SelectionChanged...
                return (e{gtkReturn=True})

--
-- | Editor for a font selection
--
fontEditor :: Editor (Maybe String)
fontEditor parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget mbValue -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    fs <- fontButtonNew
                    widgetSetName fs $ getParameter paraName parameters
                    mapM_ (activateEvent (castToWidget fs) notifier Nothing)
                            [FocusOut,FocusIn,Clicked]
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
        parameters
        notifier

--
-- | An editor, which opens another editor
--   You have to inject a value before the button can be clicked.
--
otherEditor :: (alpha  -> String -> IO (Maybe alpha)) -> Editor alpha
otherEditor func parameters notifier = do
    coreRef <- newIORef Nothing
    mkEditor
        (\widget val -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    button <- buttonNewWithLabel (getParameter paraName parameters)
                    widgetSetName button $ getParameter paraName parameters
                    containerAdd widget button
                    mapM_ (activateEvent (castToWidget button) notifier Nothing)
                            [FocusOut,FocusIn,Clicked]
                    registerEvent notifier Clicked (Left (buttonHandler coreRef))
                    writeIORef coreRef (Just (button,val))
                Just (button, oldval) -> writeIORef coreRef (Just (button, val)))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just (_,val) -> return (Just val))
        (paraName <<<- ParaName "" $ parameters)
        notifier
    where
    buttonHandler coreRef e = do
        core <- readIORef coreRef
        case core of
            Nothing -> error "You have to inject a value before the button can be clicked"
            Just (b,val) -> do
                res <- func val (getParameter paraName parameters)
                case res of
                    Nothing     -> return (e{gtkReturn=True})
                    Just nval   -> do
                        writeIORef coreRef (Just (b, nval))
                        return (e{gtkReturn=True})

okCancelFields :: FieldDescription ()
okCancelFields = HFD emptyParams [
        mkField
            (paraStockId <<<- ParaStockId stockOk
                $ paraName <<<- ParaName "Ok"
                    $ emptyParams)
            (const ())
            (\ a b -> b)
            clickEditor
    ,   mkField
            (paraStockId <<<- ParaStockId stockCancel
                $ paraName <<<- ParaName "Cancel"
                    $ emptyParams)
            (const ())
            (\ _ b -> b)
            clickEditor]

