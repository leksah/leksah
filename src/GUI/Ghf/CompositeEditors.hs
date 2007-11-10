-----------------------------------------------------------------------------
--
-- Module      :  GUI.Ghf.CompositeEditors
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for making composite editors
--
-----------------------------------------------------------------------------------

module GUI.Ghf.CompositeEditors (
    maybeEditor
,   pairEditor
,   eitherOrEditor
,   multisetEditor
,   ColumnDescr(..)
) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView as New
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe

import Data.Ghf.Default
import GUI.Ghf.Parameters
import GUI.Ghf.EditorBasics
import GUI.Ghf.MakeEditor
import GUI.Ghf.SimpleEditors

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
                            (paraName <<<- (ParaName boolLabel) $ emptyParams)
                    propagateEvent FocusOut notifierBool notifier
                    box <- case getParameter paraDirection parameters of
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
                    box <- case getParameter paraDirection parameters of
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
        (paraName <<<- (ParaName "") $ parameters)
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
                    (box,buttonBox) <- case getParameter paraDirection parameters of
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

