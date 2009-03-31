-----------------------------------------------------------------------------
--
-- Module      :  Graphics.UI.Editor.Composite
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | Module for making composite editors
--
-----------------------------------------------------------------------------------

module Graphics.UI.Editor.Composite (
    maybeEditor
,   pairEditor
,   tupel3Editor
--,   tupel4Editor
--,   tupel5Editor
--,   tupel6Editor
--,   tupel7Editor
,   splitEditor
,   eitherOrEditor
,   multisetEditor
,   ColumnDescr(..)

,   filesEditor
,   stringsEditor

,   versionEditor
,   versionRangeEditor
,   dependencyEditor
,   dependenciesEditor
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
import Data.List (sortBy, nub, sort, elemIndex)
import Distribution.Simple
    (orEarlierVersion,
     orLaterVersion,
     VersionRange(..),
     PackageName(..),
     Dependency(..),
     PackageIdentifier(..))
import Distribution.Text (simpleParse, display)
import Distribution.Package (pkgName)
import Distribution.Version
    (VersionRange(..))
import Data.Version (Version(..))
import MyMissing (forceJust)

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

tupel3Editor :: (Editor alpha, Parameters)
    -> (Editor beta, Parameters)
    -> (Editor gamma, Parameters)
    -> Editor (alpha,beta,gamma)
tupel3Editor p1 p2 p3 parameters notifier = do
    coreRef <- newIORef Nothing
    noti1   <- emptyNotifier
    noti2   <- emptyNotifier
    noti3   <- emptyNotifier
    mapM_ (propagateEvent notifier [noti2]) allGUIEvents
    r1@(frame1,inj1,ext1) <- (fst p1) (snd p1) noti1
    r2@(frame2,inj2,ext2) <- (fst p2) (snd p2) noti2
    r3@(frame3,inj3,ext3) <- (fst p3) (snd p3) noti3
    mkEditor
        (\widget (v1,v2,v3) -> do
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
                    boxPackStart box frame1 PackGrow 0
                    boxPackStart box frame2 PackGrow 0
                    boxPackStart box frame3 PackGrow 0
                    containerAdd widget box
                    inj1 v1
                    inj2 v2
                    inj3 v3
                    writeIORef coreRef (Just (r1,r2,r3))
                Just ((_,inj1,_),(_,inj2,_),(_,inj3,_)) -> do
                    inj1 v1
                    inj2 v2
                    inj3 v3)
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just ((_,_,ext1),(_,_,ext2),(_,_,ext3)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    r3 <- ext3
                    if isJust r1 && isJust r2 && isJust r3
                        then return (Just (fromJust r1,fromJust r2, fromJust r3))
                        else return Nothing)
        parameters
        notifier

{--
tupel4Editor :: (Editor alpha, Parameters)
    -> (Editor beta, Parameters)
    -> (Editor gamma, Parameters)
    -> (Editor delta, Parameters)
    -> Editor (alpha,beta,gamma,delta)
tupel4Editor p1 p2 p3 p4 parameters notifier = do
    (widg,inj,ext) <- pairEditor ((tupel3Editor p1 p2 p3), parameters) p4 parameters notifier
    return (widg,
        (\ (a, b, c, d) -> inj ((a,b,c),d)),
        (do
            mb <- ext
            case mb of
                Nothing        -> return Nothing
                Just ((a,b,c),d) -> return (Just (a,b,c,d))))

tupel5Editor :: (Editor alpha, Parameters)
    -> (Editor beta, Parameters)
    -> (Editor gamma, Parameters)
    -> (Editor delta, Parameters)
    -> (Editor epsilon, Parameters)
    -> Editor (alpha,beta,gamma,delta,epsilon)
tupel5Editor p1 p2 p3 p4 p5 parameters notifier = do
    (widg,inj,ext) <- pairEditor ((tupel4Editor p1 p2 p3 p4), parameters) p5 parameters notifier
    return (widg,
        (\ (a, b, c, d, e) -> inj ((a,b,c,d),e)),
        (do
            mb <- ext
            case mb of
                Nothing        -> return Nothing
                Just ((a,b,c,d),e) -> return (Just (a,b,c,d,e))))

tupel6Editor :: (Editor alpha, Parameters)
    -> (Editor beta, Parameters)
    -> (Editor gamma, Parameters)
    -> (Editor delta, Parameters)
    -> (Editor epsilon, Parameters)
    -> (Editor zeta, Parameters)
    -> Editor (alpha,beta,gamma,delta,epsilon,zeta)
tupel6Editor p1 p2 p3 p4 p5 p6 parameters notifier = do
    (widg,inj,ext) <- pairEditor ((tupel5Editor p1 p2 p3 p4 p5), parameters) p6 parameters notifier
    return (widg,
        (\ (a, b, c, d, e, f) -> inj ((a,b,c,d,e),f)),
        (do
            mb <- ext
            case mb of
                Nothing        -> return Nothing
                Just ((a,b,c,d,e),f) -> return (Just (a,b,c,d,e,f))))

tupel7Editor :: (Editor alpha, Parameters)
    -> (Editor beta, Parameters)
    -> (Editor gamma, Parameters)
    -> (Editor delta, Parameters)
    -> (Editor epsilon, Parameters)
    -> (Editor zeta, Parameters)
    -> (Editor eta, Parameters)
    -> Editor (alpha,beta,gamma,delta,epsilon,zeta,eta)
tupel7Editor p1 p2 p3 p4 p5 p6 p7 parameters notifier = do
    (widg,inj,ext) <- pairEditor ((tupel6Editor p1 p2 p3 p4 p5 p6), parameters) p7 parameters notifier
    return (widg,
        (\ (a, b, c, d, e, f, g) -> inj ((a,b,c,d,e,f),g)),
        (do
            mb <- ext
            case mb of
                Nothing        -> return Nothing
                Just ((a,b,c,d,e,f),g) -> return (Just (a,b,c,d,e,f,g))))
--}

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
                                    inj1 positive
                                    (childWidget,inj2,_) <- getChildEditor childRef childEdit childParams cNoti
                                    widgetShowAll childWidget
                                    inj2 val
                                else do
                                    inj1 positive
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
    be@(boolFrame,inj1,ext1) <- boolEditor2  (getParameter paraName rightParams) leftParams noti1
    le@(leftFrame,inj2,ext2) <- leftEditor (paraName <<<- ParaName "" $ leftParams) noti2
    re@(rightFrame,inj3,ext3) <- rightEditor (paraName <<<- ParaName "" $ rightParams) noti3
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
multisetEditor :: (Show alpha, Default alpha, Eq alpha) => ColumnDescr alpha
    -> (Editor alpha, Parameters)
    -> Maybe ([alpha] -> [alpha]) -- ^ The 'mbSort' arg, a sort function if desired
    -> Maybe (alpha -> alpha -> Bool) -- ^ The 'mbReplace' arg, a function which is a criteria for removing an
                              --   old entry when adding a new value
    -> Editor [alpha]
multisetEditor (ColumnDescr showHeaders columnsDD) (singleEditor, sParams) mbSort mbReplace
        parameters notifier = do
    coreRef <- newIORef Nothing
    cnoti   <- emptyNotifier
    mapM_ (propagateEvent notifier [cnoti]) allGUIEvents
    mkEditor
        (\widget vs -> do
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
                    treeView        <-  treeViewNewWithModel listStore
                    let minSize =   getParameter paraMinSize parameters
                    uncurry (widgetSetSizeRequest treeView) minSize
                    sw          <-  scrolledWindowNew Nothing Nothing
                    containerAdd sw treeView
                    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
                    sel         <-  treeViewGetSelection treeView
                    treeSelectionSetMode sel SelectionSingle
                    mapM_ (\(str,func) -> do
                            col <- treeViewColumnNew
                            treeViewColumnSetTitle  col str
                            treeViewColumnSetResizable col True
                            treeViewAppendColumn treeView col
                            renderer <- cellRendererTextNew
                            cellLayoutPackStart col renderer True
                            cellLayoutSetAttributes col renderer listStore func
                        ) columnsDD
                    treeViewSetHeadersVisible treeView showHeaders
                    sel  `onSelectionChanged` selectionHandler sel listStore injS
                    boxPackStart box sw PackGrow 0
                    boxPackStart box buttonBox PackNatural 0
                    boxPackStart box frameS PackNatural 0
                    activateEvent (castToWidget treeView) notifier Nothing FocusOut
                    containerAdd widget box
                    listStoreClear listStore
                    mapM_ (listStoreAppend listStore)
                        (case mbSort of
                            Nothing -> vs
                            Just sortF -> sortF vs)
                    addButton `onClicked` do
                        mbv <- extS
                        case mbv of
                            Just v -> do
                                case mbReplace of
                                    Nothing         -> return ()
                                    Just replaceF   -> do
                                         cont <- listStoreToList listStore
                                         mapM_ (listStoreRemove listStore)
                                            $ map fst
                                                $ filter (\(_,e) -> replaceF v e)
                                                    $ zip [0..] cont
                                case mbSort of
                                    Nothing    -> do
                                        listStoreAppend listStore v
                                        return ()
                                    Just sortF -> do
                                        cont <- listStoreToList listStore
                                        listStoreClear listStore
                                        mapM_ (listStoreAppend listStore) (sortF (v:cont))
                                cont <- listStoreToList listStore
                                case elemIndex v cont of
                                    Just idx -> do
                                        treeSelectionSelectPath sel [idx]
                                        mbCol <- treeViewGetColumn treeView 0
                                        case mbCol of
                                            Nothing  -> return ()
                                            Just col -> treeViewScrollToCell treeView [idx] col Nothing
                                    Nothing -> return ()
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
                    mapM_ (listStoreAppend listStore)
                        (case mbSort of
                            Nothing -> vs
                            Just sortF -> sortF vs))
        (do core <- readIORef coreRef
            case core of
                Nothing -> return Nothing
                Just listStore -> do
                    v <- listStoreToList listStore
                    return (Just v))
        (paraMinSize <<<- ParaMinSize (-1,-1) $ parameters)
        notifier
    where
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


filesEditor :: Maybe FilePath -> FileChooserAction -> String -> Editor [FilePath]
filesEditor fp act label p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [cellText := row]))])
        (fileEditor fp act label, emptyParams)
        (Just sort)
        (Just (==))
        (paraShadow <<<- ParaShadow ShadowIn $
            paraDirection  <<<- ParaDirection Vertical $ p)

stringsEditor :: (String -> Bool) -> Editor [String]
stringsEditor validation p =
    multisetEditor
        (ColumnDescr False [("",(\row -> [cellText := row]))])
        (stringEditor validation, emptyParams)
        (Just sort)
        (Just (==))
        (paraShadow <<<- ParaShadow ShadowIn $ p)

dependencyEditor :: [PackageIdentifier] -> Editor Dependency
dependencyEditor packages para noti = do
    (wid,inj,ext) <- pairEditor
        ((eitherOrEditor (comboSelectionEditor ((sort . nub) (map (display . pkgName) packages)) id
            , paraName <<<- ParaName "Select" $ emptyParams)
            (stringEditor (const True), paraName <<<- ParaName "Enter" $ emptyParams)
            "Select from list?"), paraName <<<- ParaName "Name"$ emptyParams)
        (versionRangeEditor,paraName <<<- ParaName "Version" $ emptyParams)
        (paraDirection <<<- ParaDirection Vertical $ para)
        noti
    let pinj (Dependency pn@(PackageName s) v) = if elem s (map (display . pkgName) packages)
                                                    then inj (Left s,v)
                                                    else inj (Right s,v)
    let pext = do
        mbp <- ext
        case mbp of
            Nothing -> return Nothing
            Just (Left "",v) -> return Nothing
            Just (Left s,v) -> return (Just $ Dependency (PackageName s) v)
            Just (Right "",v) -> return Nothing
            Just (Right s,v) -> return (Just $ Dependency (PackageName s) v)
    return (wid,pinj,pext)

dependenciesEditor :: [PackageIdentifier] -> Editor [Dependency]
dependenciesEditor packages p noti =
    multisetEditor
        (ColumnDescr True [("Package",\(Dependency (PackageName str) _) -> [cellText := str])
                           ,("Version",\(Dependency _ vers) -> [cellText := display vers])])
        (dependencyEditor packages,
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                   $ emptyParams)
        (Just (sortBy (\ (Dependency p1 _) (Dependency p2 _) -> compare p1 p2)))
        (Just (\ (Dependency p1 _) (Dependency p2 _) -> p1 == p2))
        (paraShadow <<<- ParaShadow ShadowIn $
            paraOuterAlignment <<<- ParaInnerAlignment (0.0, 0.5, 1.0, 1.0)
                $ paraInnerAlignment <<<- ParaOuterAlignment (0.0, 0.5, 1.0, 1.0)
                    $ paraDirection  <<<-  ParaDirection Vertical
                        $ paraPack <<<- ParaPack PackGrow
                            $ p)
        noti

versionRangeEditor :: Editor VersionRange
versionRangeEditor para noti = do
    (wid,inj,ext) <-
        maybeEditor
            ((eitherOrEditor
                (pairEditor
                    (comboSelectionEditor v1 show, emptyParams)
                    (versionEditor, paraName <<<- ParaName "Enter Version" $ emptyParams),
                        (paraDirection <<<- ParaDirection Vertical
                            $ paraName <<<- ParaName "Simple"
                            $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                            $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                            $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                            $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                            $ emptyParams))
                (tupel3Editor
                    (comboSelectionEditor v2 show, emptyParams)
                    (versionRangeEditor, paraShadow <<<- ParaShadow ShadowIn $ emptyParams)
                    (versionRangeEditor, paraShadow <<<- ParaShadow ShadowIn $ emptyParams),
                        paraName <<<- ParaName "Complex"
                        $    paraDirection <<<- ParaDirection Vertical
                        $ paraOuterAlignment <<<- ParaOuterAlignment  (0.0, 0.0, 0.0, 0.0)
                        $ paraOuterPadding <<<- ParaOuterPadding    (0, 0, 0, 0)
                        $ paraInnerAlignment <<<- ParaInnerAlignment  (0.0, 0.0, 0.0, 0.0)
                        $ paraInnerPadding <<<- ParaInnerPadding   (0, 0, 0, 0)
                        $ emptyParams) "Select version range"), emptyParams)
            False "Any Version"
            (paraDirection <<<- ParaDirection Vertical $ para)
            noti
    let vrinj AnyVersion                =   inj Nothing
        vrinj (ThisVersion v)           =   inj (Just (Left (ThisVersionS,v)))
        vrinj (LaterVersion v)          =   inj (Just (Left (LaterVersionS,v)))
        vrinj (EarlierVersion v)        =   inj (Just (Left (EarlierVersionS,v)))
        vrinj (UnionVersionRanges (ThisVersion v1) (LaterVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrLaterVersionS,v1)))
        vrinj (UnionVersionRanges (LaterVersion v1) (ThisVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrLaterVersionS,v1)))
        vrinj (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrEarlierVersionS,v1)))
        vrinj (UnionVersionRanges (EarlierVersion v1) (ThisVersion v2)) | v1 == v2
                                        =  inj (Just (Left (ThisOrEarlierVersionS,v1)))
        vrinj (UnionVersionRanges v1 v2)=  inj (Just (Right (UnionVersionRangesS,v1,v2)))
        vrinj (IntersectVersionRanges v1 v2)
                                        =    inj (Just (Right (IntersectVersionRangesS,v1,v2)))
    let vrext = do  mvr <- ext
                    case mvr of
                        Nothing -> return (Just AnyVersion)
                        Just Nothing -> return (Just AnyVersion)
                        Just (Just (Left (ThisVersionS,v)))     -> return (Just (ThisVersion v))
                        Just (Just (Left (LaterVersionS,v)))    -> return (Just (LaterVersion v))
                        Just (Just (Left (EarlierVersionS,v)))   -> return (Just (EarlierVersion v))

                        Just (Just (Left (ThisOrLaterVersionS,v)))   -> return (Just (orLaterVersion  v))
                        Just (Just (Left (ThisOrEarlierVersionS,v)))   -> return (Just (orEarlierVersion  v))
                        Just (Just (Right (UnionVersionRangesS,v1,v2)))
                                                        -> return (Just (UnionVersionRanges v1 v2))
                        Just (Just (Right (IntersectVersionRangesS,v1,v2)))
                                                        -> return (Just (IntersectVersionRanges v1 v2))
    return (wid,vrinj,vrext)
        where
            v1 = [ThisVersionS,LaterVersionS,ThisOrLaterVersionS,EarlierVersionS,ThisOrEarlierVersionS]
            v2 = [UnionVersionRangesS,IntersectVersionRangesS]

data Version1 = ThisVersionS | LaterVersionS | ThisOrLaterVersionS | EarlierVersionS | ThisOrEarlierVersionS
    deriving (Eq)
instance Show Version1 where
    show ThisVersionS   =  "This Version"
    show LaterVersionS  =  "Later Version"
    show ThisOrLaterVersionS = "This or later Version"
    show EarlierVersionS =  "Earlier Version"
    show ThisOrEarlierVersionS = "This or earlier Version"

data Version2 = UnionVersionRangesS | IntersectVersionRangesS
    deriving (Eq)
instance Show Version2 where
    show UnionVersionRangesS =  "Union Version Ranges"
    show IntersectVersionRangesS =  "Intersect Version Ranges"

versionEditor :: Editor Version
versionEditor para noti = do
    (wid,inj,ext) <- stringEditor (\s -> not (null s)) para noti
    let pinj v = inj (display v)
    let pext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> return (simpleParse s)
    return (wid, pinj, pext)

instance Default Version1
    where getDefault = ThisVersionS

instance Default Version2
    where getDefault = UnionVersionRangesS

instance Default Version
    where getDefault = forceJust (simpleParse "0") "PackageEditor>>default version"

instance Default VersionRange
    where getDefault = AnyVersion

instance Default Dependency
    where getDefault = Dependency getDefault getDefault

instance Default PackageName
    where getDefault = PackageName getDefault





