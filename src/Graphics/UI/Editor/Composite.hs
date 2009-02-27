-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Composite
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info at leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for making composite editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Composite (
    maybeEditor
,   pairEditor
,   splitEditor
,   eitherOrEditor
,   multisetEditor
,   ColumnDescr(..)
) where

import Graphics.UI.Gtk
import Control.Monad
import Data.IORef
import Data.Maybe

import Default
import IDE.Exception
import Control.Event
import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Simple

--
-- | An editor which composes two subeditors
--
pairEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
pairEditor (fstEd,fstPara) (sndEd,sndPara) parameters notifier = do
    coreRef <- newIORef Nothing
    noti1   <- emptyNotifier
    noti2   <- emptyNotifier
    mapM_ (propagateEvent notifier [noti2]) allGUIEvents
    fst@(fstFrame,inj1,ext1) <- fstEd fstPara noti1
    snd@(sndFrame,inj2,ext2) <- sndEd sndPara noti2
    mkEditor
        (\widget (v1,v2) -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- case getParameter paraDirection parameters of
                        Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box fstFrame PackGrow 0
                    boxPackStart box sndFrame PackGrow 0
                    containerAdd widget box
                    inj1 v1
                    inj2 v2
                    writeIORef coreRef (Just (fst,snd))
                Just ((_,inj1,_),(_,inj2,_)) -> do
                    inj1 v1
                    inj2 v2)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)
        parameters
        notifier

--
-- | Like a pair editor, but with a moveable split
--
splitEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
splitEditor (fstEd,fstPara) (sndEd,sndPara) parameters notifier = do
    coreRef <- newIORef Nothing
    noti1   <- emptyNotifier
    noti2   <- emptyNotifier
    mapM_ (propagateEvent notifier [noti1,noti2]) allGUIEvents
    fst@(fstFrame,inj1,ext1) <- fstEd fstPara noti1
    snd@(sndFrame,inj2,ext2) <- sndEd sndPara noti2
    mkEditor
        (\widget (v1,v2) -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    paned <- case getParameter paraDirection parameters of
                        Horizontal  -> do  h <- vPanedNew
                                           return (castToPaned h)
                        Vertical    -> do  v <- hPanedNew
                                           return (castToPaned v)
                    panedPack1 paned fstFrame True True
                    panedPack2 paned sndFrame True True
                    containerAdd widget paned
                    inj1 v1
                    inj2 v2
                    writeIORef coreRef (Just (fst,snd))
                Just ((_,inj1,_),(_,inj2,_)) -> do
                    inj1 v1
                    inj2 v2)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)
        parameters
        notifier

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or deselected (if the positive Argument is False)
--
maybeEditor :: Default beta => (Editor beta, Parameters) -> Bool -> String -> Editor (Maybe beta)
maybeEditor (childEdit, childParams) positive boolLabel parameters notifier = do
    coreRef <- newIORef Nothing
    childRef  <- newIORef Nothing
    notifierBool <- emptyNotifier
    cNoti   <- emptyNotifier
    mapM_ (propagateEvent notifier [notifierBool]) [Clicked,FocusIn]
    mapM_ (propagateEvent notifier [cNoti]) allGUIEvents

    mkEditor
        (\widget mbVal -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    box <- case getParameter paraDirection parameters of
                        Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    be@(boolFrame,inj1,ext1) <- boolEditor
                        (paraName <<<- ParaName boolLabel $ emptyParams)
                        notifierBool
                    boxPackStart box boolFrame PackNatural 0
                    containerAdd widget box
                    registerEvent notifierBool Clicked (Left (onClickedHandler widget coreRef childRef cNoti))
                    case mbVal of
                        Nothing -> inj1 (not positive)
                        Just val -> do
                            (childWidget,inj2,ext2) <- getChildEditor childRef childEdit childParams cNoti
                            boxPackEnd box childWidget PackGrow 0
                            widgetShowAll childWidget
                            inj1 positive
                            inj2 val
                    writeIORef coreRef (Just (be,box))
                Just (be@(boolFrame,inj1,extt),box) -> do
                    hasChild <- hasChildEditor childRef
                    case mbVal of
                        Nothing ->
                            if hasChild
                                then do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    inj1 (not positive)
                                    widgetHideAll childWidget
                                else inj1 (not positive)
                        Just val ->
                            if hasChild
                                then do
                                    (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                    widgetShowAll childWidget
                                    inj2 val
                                else do
                                    (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                    boxPackEnd box childWidget PackGrow 0
                                    widgetShowAll childWidget
                                    inj2 val)
        (do
            core <- readIORef coreRef
            case core of
                Nothing  -> return Nothing
                Just (be@(boolFrame,inj1,ext1),_) -> do
                    bool <- ext1
                    case bool of
                        Nothing -> return Nothing
                        Just bv | bv == positive -> do
                            (_,_,ext2) <- getChildEditor childRef childEdit childParams cNoti
                            value <- ext2
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Just value))
                        otherwise -> return (Just Nothing))
        parameters
        notifier
    where
    onClickedHandler widget coreRef childRef cNoti event = do
        core <- readIORef coreRef
        case core of
            Nothing  -> throwIDE "Impossible"
            Just (be@(boolFrame,inj1,ext1),vBox) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                        if bool /= positive
                            then do
                                hasChild <- hasChildEditor childRef
                                when hasChild $ do
                                    (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                    widgetHideAll childWidget
                            else do
                                hasChild <- hasChildEditor childRef
                                if hasChild
                                    then do
                                        (childWidget,_,_) <- getChildEditor childRef childEdit childParams cNoti
                                        widgetShowAll childWidget
                                    else do
                                        (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                        boxPackEnd vBox childWidget PackNatural 0
                                        inj2 getDefault
                                        widgetShowAll childWidget
                    Nothing -> return ()
                return (event {gtkReturn=True})
    getChildEditor childRef childEditor childParams cNoti =  do
        mb <- readIORef childRef
        case mb of
            Just editor -> return editor
            Nothing -> do
                let val = childEditor
                editor@(_,_,_) <- childEditor childParams cNoti
                writeIORef childRef (Just editor)
                return editor
    hasChildEditor childRef =  do
        mb <- readIORef childRef
        return (isJust mb)

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- or deselected (if the positive Argument is False)
eitherOrEditor :: (Default alpha, Default beta) => (Editor alpha, Parameters) ->
                        (Editor beta, Parameters) -> String -> Editor (Either alpha beta)
eitherOrEditor (leftEditor,leftParams) (rightEditor,rightParams)
            label2 parameters notifier = do
    coreRef <- newIORef Nothing
    noti1 <- emptyNotifier
    noti2 <- emptyNotifier
    noti3 <- emptyNotifier
    mapM_ (propagateEvent notifier [noti1,noti2,noti3]) allGUIEvents
    be@(boolFrame,inj1,ext1) <- boolEditor2  label2 parameters noti1
    le@(leftFrame,inj2,ext2) <- leftEditor leftParams noti2
    re@(rightFrame,inj3,ext3) <- rightEditor rightParams noti3
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    registerEvent noti1 Clicked (Left (onClickedHandler widget coreRef))
                    box <- case getParameter paraDirection parameters of
                        Horizontal -> do
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box boolFrame PackNatural 0
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
                Just ((_,inj1,_),(leftFrame,inj2,_),(rightFrame,inj3,_),box) ->
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
                Just ((_,_,ext1),(_,_,ext2),(_,_,ext3),_) -> do
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
        (paraName <<<- ParaName "" $ parameters)
        notifier
    where
    onClickedHandler widget coreRef event =  do
        core <- readIORef coreRef
        case core of
            Nothing  -> throwIDE "Impossible"
            Just (be@(_,_,ext1),(leftFrame,_,_),(rightFrame,_,_),box) -> do
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
                return event{gtkReturn=True}


-- a trivial example: (ColumnDescr False [("",(\row -> [cellText := show row]))])
-- and a nontrivial:
--  [("Package",\(Dependency str _) -> [cellText := str])
--  ,("Version",\(Dependency _ vers) -> [cellText := showVersionRange vers])])
data ColumnDescr row = ColumnDescr Bool [(String,(row -> [AttrOp CellRendererText]))]

--
-- | An editor with a subeditor, of which a list of items can be selected
multisetEditor :: (Show alpha, Default alpha) =>
                    ColumnDescr alpha -> (Editor alpha, Parameters) -> Editor [alpha]
multisetEditor (ColumnDescr showHeaders columnsDD) (singleEditor, sParams)
        parameters notifier = do
    coreRef <- newIORef Nothing
    cnoti   <- emptyNotifier
    mapM_ (propagateEvent notifier [cnoti]) allGUIEvents
    mkEditor
        (\widget v -> do
            core <- readIORef coreRef
            case core of
                Nothing  -> do
                    (box,buttonBox) <- case getParameter paraDirection parameters of
                        Horizontal -> do
                            b  <- hBoxNew False 1
                            bb <- vButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                        Vertical -> do
                            b  <- vBoxNew False 1
                            bb <- hButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                    (frameS,injS,extS) <- singleEditor sParams cnoti
                    addButton   <- buttonNewWithLabel "Add"
                    removeButton <- buttonNewWithLabel "Remove"
                    containerAdd buttonBox addButton
                    containerAdd buttonBox removeButton
                    listStore   <-  listStoreNew ([]:: [alpha])
                    list        <-  treeViewNewWithModel listStore
                    let minSize =   getParameter paraMinSize parameters
                    uncurry (widgetSetSizeRequest list) minSize
                    sw          <-  scrolledWindowNew Nothing Nothing
                    containerAdd sw list
                    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                    sel         <-  treeViewGetSelection list
                    treeSelectionSetMode sel SelectionSingle
                    mapM_ (\(str,func) -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col str
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn list col
                            renderer <- cellRendererTextNew
                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStore func
                        ) columnsDD
                    treeViewSetHeadersVisible list showHeaders
                    sel  `onSelectionChanged` selectionHandler sel listStore injS
                    boxPackStart box sw PackGrow 0
                    boxPackStart box buttonBox PackNatural 0
                    boxPackStart box frameS PackNatural 0
                    activateEvent (castToWidget list) notifier Nothing FocusOut
                    containerAdd widget box
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) v
                    addButton `onClicked` do
                        mbv <- extS
                        case mbv of
                            Just v -> do
                              seq <- listStoreAppend listStore v
                              treeSelectionSelectPath sel [seq]
                              mbCol <- treeViewGetColumn list 0
                              case mbCol of
                                Nothing -> return ()
                                Just col -> treeViewScrollToCell list [seq] col Nothing
                              return ()
                            Nothing -> return ()
                    removeButton `onClicked` do
                        mbi <- treeSelectionGetSelected sel
                        case mbi of
                            Nothing -> return ()
                            Just iter -> do
                                [i] <- treeModelGetPath listStore iter
                                listStoreRemove listStore i
                    writeIORef coreRef (Just listStore)
                    injS getDefault
                Just listStore -> do
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore) v)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listStore -> do
                    v <- listStoreGetValues listStore
                    return (Just v))
        (paraMinSize <<<- ParaMinSize (-1,-1) $ parameters)
        notifier
    where
    listStoreGetValues :: ListStore a -> IO [a]
    listStoreGetValues listStore = treeModelGetIterFirst listStore >>= getTail
        where getTail mbi = case mbi of
                                Nothing -> return []
                                Just iter -> do
                                    [i] <- treeModelGetPath listStore iter
                                    v <- listStoreGetValue listStore i
                                    mbi2 <- treeModelIterNext listStore iter
                                    rest <- getTail mbi2
                                    return (v : rest)
    selectionHandler :: TreeSelection -> ListStore a -> Injector a -> IO ()
    selectionHandler sel listStore inj = do
        ts <- treeSelectionGetSelected sel
        case ts of
            Nothing -> return ()
            Just iter -> do
                [i] <- treeModelGetPath listStore iter
                v <- listStoreGetValue listStore i
                inj v
                return ()

