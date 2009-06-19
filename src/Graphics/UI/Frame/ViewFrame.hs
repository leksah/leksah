{-# OPTIONS_GHC
    -XFunctionalDependencies
    -XNoMonomorphismRestriction
    -XFlexibleInstances
    -XMultiParamTypeClasses
    -XUndecidableInstances
    -XDeriveDataTypeable #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.ViewFrame
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Splittable panes containing notebooks with any widgets
--
---------------------------------------------------------------------------------

module Graphics.UI.Frame.ViewFrame (
    removePaneAdmin
,   addPaneAdmin
,   notebookInsertOrdered
,   markLabel

-- * Convenience methods for accesing Pane state
,   posTypeToPaneDirection
,   paneDirectionToPosType
,   paneFromName
,   mbPaneFromName
,   guiPropertiesFromName

-- * View Actions
,   viewMove
,   viewSplitHorizontal
,   viewSplitVertical
,   viewSplit
,   viewSplit'
,   viewNewGroup
,   newGroupOrBringToFront
,   bringGroupToFront
,   viewNest
,   viewNest'
,   viewDetach
,   viewDetach'
,   handleNotebookSwitch
,   viewCollapse
,   viewCollapse'
,   viewTabsPos
,   viewSwitchTabs

-- * View Queries
,   getBestPanePath
,   getBestPathForId
,   getActivePanePath
,   getActivePanePathOrStandard
,   figureOutPaneName
,   getNotebook
,   getPaned
,   getActiveNotebook
,   getActivePane
,   setActivePane
,   getUiManager
,   getWindows
,   getMainWindow
,   getLayout
,   getPanesSt
,   getPaneMapSt
,   getPane
,   getPanes

-- * View Actions
,   bringPaneToFront
,   newNotebook
,   newNotebook'

-- * Accessing GUI elements
--,   widgetFromPath
,   getUIAction
,   widgetGet


) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite,onToggleOverwrite)
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Data.Unique
import Data.Typeable

import Graphics.UI.Frame.Panes
import Graphics.UI.Editor.Parameters
import System.Glib (isA)
import Graphics.UI.Gtk.Types (gTypeNotebook)
import System.CPUTime (getCPUTime)
import Graphics.UI.Gtk.Gdk.Enums (Modifier(..))
import MyMissing
import Graphics.UI.Gtk.Gdk.EventM (TimeStamp(..))
import Graphics.UI.Editor.MakeEditor
    (mkField, FieldDescription(..), buildEditor)
import Graphics.UI.Editor.Simple (stringEditor, okCancelFields)
import Control.Event (registerEvent)
import Graphics.UI.Editor.Basics
    (eventPaneName, GUIEventSelector(..))
import qualified Data.Set as  Set (unions, member)
import Data.Set (Set(..))
import Graphics.UI.Gtk.Gdk.Events (Event(..))
-- import Debug.Trace (trace)
trace a b = b

removePaneAdmin :: Pane alpha beta => alpha -> beta ()
removePaneAdmin pane = do
    panes'          <-  getPanesSt
    paneMap'        <-  getPaneMapSt
    setPanesSt      (Map.delete (paneName pane) panes')
    setPaneMapSt    (Map.delete (paneName pane) paneMap')

addPaneAdmin :: RecoverablePane alpha beta delta => alpha -> Connections -> PanePath -> delta ()
addPaneAdmin pane conn pp = do
    panes'          <-  getPanesSt
    paneMap'        <-  getPaneMapSt
    unique          <-  liftIO newUnique
    liftIO $ widgetSetName (getTopWidget pane) (paneName pane)
    trace ("addPaneAdmin name: " ++ paneName pane ++ " path: " ++ show pp) $
        setPaneMapSt  (Map.insert (paneName pane) (pp, conn) paneMap')
    setPanesSt          (Map.insert (paneName pane) (PaneC pane) panes')

getPane ::  RecoverablePane alpha beta delta => delta (Maybe alpha)
getPane = do
    selectedPanes <- getPanes
    if null selectedPanes || length selectedPanes > 1
        then return Nothing
        else (return (Just $ head selectedPanes))

getPanes ::  RecoverablePane alpha beta delta => delta ([alpha])
getPanes = do
    panes' <- getPanesSt
    return (catMaybes
                $ map (\(PaneC p) -> cast p)
                    $ Map.elems panes')

notebookInsertOrdered :: PaneMonad alpha => (NotebookClass self, WidgetClass child)		
    => self	
    -> child	-- child - the Widget to use as the contents of the page.
    -> String
    -> Maybe Label	-- the label for the page as String or Label
    -> alpha ()
notebookInsertOrdered nb widget labelStr mbLabel = do
    label	    <-  case mbLabel of
                        Nothing  -> liftIO $ labelNew (Just labelStr)
                        Just l  -> return l
    menuLabel   <-  liftIO $ labelNew (Just labelStr)
    numPages    <-  liftIO $ notebookGetNPages nb
    mbWidgets   <-  liftIO $ mapM (notebookGetNthPage nb) [0 .. (numPages-1)]
                    -- trace ("numPages " ++ show numPages) $

    let widgets =   map (\v -> forceJust v "ViewFrame.notebookInsertOrdered: no widget") mbWidgets
    labelStrs   <-  liftIO $ mapM (widgetGetName) widgets
    let pos     =   case findIndex (\s -> s > labelStr) labelStrs of
                        Just i  ->  i
                        Nothing ->  -1
    labelBox    <-  mkLabelBox label labelStr
    liftIO $ do
        markLabel nb labelBox False
        realPos     <-  notebookInsertPageMenu nb widget labelBox menuLabel pos
        widgetShowAll labelBox
        notebookSetCurrentPage nb realPos

-- | Returns a label box
mkLabelBox :: PaneMonad alpha => Label -> String -> alpha EventBox
mkLabelBox lbl paneName = do
    (tb,lb) <- liftIO $ do
        miscSetAlignment (castToMisc lbl) 0.0 0.0
        miscSetPadding  (castToMisc lbl) 0 0

        labelBox  <- eventBoxNew
        eventBoxSetVisibleWindow labelBox False
        innerBox  <- hBoxNew False 0

        tabButton <- buttonNew
        widgetSetName tabButton "leksah-close-button"
        buttonSetFocusOnClick tabButton False
        buttonSetRelief tabButton ReliefNone
        buttonSetAlignment tabButton (0.0,0.0)

        image     <- imageNewFromStock stockClose IconSizeMenu
        mbPB <- widgetRenderIcon tabButton stockClose IconSizeMenu ""
        (height,width)   <-  case mbPB of
                                Nothing -> trace "no real size" return (14,14)
                                Just pb -> do
                                h <- pixbufGetHeight pb
                                w <- pixbufGetWidth pb
                                return (h,w)
        on tabButton styleSet (\style -> do
            widgetSetSizeRequest tabButton (height + 2) (width + 2))
        containerSetBorderWidth tabButton 0
        containerAdd tabButton image

        boxPackStart innerBox lbl PackNatural 0
        boxPackEnd innerBox tabButton PackNatural 0

        containerAdd labelBox innerBox
        dragSourceSet labelBox [Button1] [ActionCopy,ActionMove]
        tl        <- targetListNew
        targetListAddTextTargets tl 0
        dragSourceSetTargetList labelBox tl
        on labelBox dragDataGet (\ cont id timeStamp -> do
            selectionDataSetText paneName
            trace ("DragGet data: " ++ paneName) $ return ())
        return (tabButton,labelBox)
    cl <- runInIO closeHandler
    liftIO $ onClicked tb (cl ())

    return lb
    where
        closeHandler :: PaneMonad alpha => () -> alpha ()
        closeHandler _ =    case "group_" `stripPrefix` paneName of
                                Just group  -> do
                                    closeGroup group
                                Nothing -> do
                                    (PaneC pane) <- paneFromName paneName
                                    close pane
                                    return ()

-- | Add the change mark or removes it
markLabel :: (WidgetClass alpha, NotebookClass beta) => beta -> alpha -> Bool -> IO ()
markLabel nb topWidget modified = do
    mbBox   <- notebookGetTabLabel nb topWidget
    case mbBox of
        Nothing  -> return ()
        Just box -> do
            mbContainer <- binGetChild (castToBin box)
            case mbContainer of
                Nothing -> return ()
                Just container -> do
                    children <- containerGetChildren container
                    let label = castToLabel $ forceHead children "ViewFrame>>markLabel: empty children"
                    text <- widgetGetName topWidget
                    labelSetUseMarkup (castToLabel label) True
                    labelSetMarkup (castToLabel label)
                        (if modified
                              then "<span foreground=\"red\">" ++ text ++ "</span>"
                          else text)

-- | Constructs a unique pane name, which is an index and a string
figureOutPaneName :: PaneMonad alpha => String -> Int -> alpha (Int,String)
figureOutPaneName bn ind = do
    bufs <- getPanesSt
    let ind = foldr (\(PaneC buf) ind ->
                if primPaneName buf == bn
                    then max ind ((getAddedIndex buf) + 1)
                    else ind)
                0 (Map.elems bufs)
    if ind == 0
        then return (0,bn)
        else return (ind,bn ++ "(" ++ show ind ++ ")")

paneFromName :: PaneMonad alpha => PaneName -> alpha (IDEPane alpha)
paneFromName pn = do
    mbPane <- mbPaneFromName pn
    case mbPane of
        Just p -> return p
        Nothing -> error $ "ViewFrame>>paneFromName:Can't find pane from unique name " ++ pn

mbPaneFromName :: PaneMonad alpha => PaneName -> alpha (Maybe (IDEPane alpha))
mbPaneFromName pn = do
    panes  <- getPanesSt
    return (Map.lookup pn panes)

-- |
guiPropertiesFromName :: PaneMonad alpha => PaneName -> alpha (PanePath, Connections)
guiPropertiesFromName pn = do
    paneMap <- getPaneMapSt
    case Map.lookup pn paneMap of
            Just it -> return it
            otherwise  -> error $"Cant't find guiProperties from unique name " ++ pn

posTypeToPaneDirection PosLeft      =   LeftP
posTypeToPaneDirection PosRight     =   RightP	
posTypeToPaneDirection PosTop       =   TopP
posTypeToPaneDirection PosBottom    =   BottomP	

paneDirectionToPosType LeftP        =   PosLeft
paneDirectionToPosType RightP       =   PosRight   	
paneDirectionToPosType TopP         =   PosTop
paneDirectionToPosType BottomP      =   PosBottom

--
-- | Toggle the tabs of the current notebook
--
viewSwitchTabs :: PaneMonad alpha => alpha ()
viewSwitchTabs = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> liftIO $ do
            b <- notebookGetShowTabs nb
            notebookSetShowTabs nb (not b)

--
-- | Sets the tab position in the current notebook
--
viewTabsPos :: PaneMonad alpha => PositionType -> alpha ()
viewTabsPos pos = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> liftIO $notebookSetTabPos nb pos

--
-- | Split the currently active pane in horizontal direction
--
viewSplitHorizontal     :: PaneMonad alpha => alpha ()
viewSplitHorizontal     = viewSplit Horizontal

--
-- | Split the currently active pane in vertical direction
--
viewSplitVertical :: PaneMonad alpha => alpha ()
viewSplitVertical = viewSplit Vertical

--
-- | The active view can be split in two (horizontal or vertical)
--
viewSplit :: PaneMonad alpha => Direction -> alpha ()
viewSplit dir = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewSplit' panePath dir

viewSplit' :: PaneMonad alpha => PanePath -> Direction -> alpha ()
viewSplit' panePath dir = do
    l <- getLayout
    case layoutFromPath panePath l of
        (TerminalP _ _ _ (Just _) _) -> trace ("ViewFrame>>viewSplit': can't split detached: ")
                                            return ()
        _                            -> do
            activeNotebook  <- getNotebook panePath
            mbPD <- do
                mbParent  <- liftIO $ widgetGetParent activeNotebook
                case mbParent of
                    Nothing -> trace ("ViewFrame>>viewSplit': parent not found: ") return Nothing
                    Just parent -> do
                        (nb,paneDir) <- do
                            let (name,altname,paneDir,
                                 oldPath,newPath) =  case dir of
                                                        Horizontal  -> ("top",
                                                                        "bottom",
                                                                        TopP,
                                                                        panePath ++ [SplitP TopP],
                                                                        panePath ++ [SplitP BottomP])
                                                        Vertical    -> ("left",
                                                                        "right",
                                                                        LeftP,
                                                                        panePath ++ [SplitP LeftP],
                                                                        panePath ++ [SplitP RightP])
                            nb  <- newNotebook newPath
                            st  <- getFrameState
                            setFrameState st{panePathFromNB = Map.insert activeNotebook oldPath (panePathFromNB st)}
                            (np,nbi) <- liftIO $ do
                                newpane <- case dir of
                                              Horizontal  -> do  h <- vPanedNew
                                                                 return (castToPaned h)
                                              Vertical    -> do  v <- hPanedNew
                                                                 return (castToPaned v)
                                rName <- widgetGetName activeNotebook
                                widgetSetName newpane rName
                                widgetSetName nb altname
                                panedPack2 newpane nb True True
                                nbIndex <- if parent `isA` gTypeNotebook
                                            then notebookPageNum (castToNotebook parent) activeNotebook
                                            else trace ("ViewFrame>>viewSplit': parent not a notebook: ") return Nothing
                                containerRemove (castToContainer parent) activeNotebook
                                widgetSetName activeNotebook name
                                panedPack1 newpane activeNotebook True True
                                return (newpane,nbIndex)
                            case (reverse panePath, nbi) of
                                (SplitP dir:_, _)
                                    | dir `elem` [TopP, LeftP] -> liftIO $ panedPack1 (castToPaned parent) np True True
                                    | otherwise                -> liftIO $ panedPack2 (castToPaned parent) np True True
                                (GroupP group:_, Just n) -> do
                                    liftIO $ notebookInsertPage (castToNotebook parent) np group n
                                    label <- groupLabel group
                                    liftIO $ notebookSetTabLabel (castToNotebook parent) np label
                                    return ()
                                ([], _) -> do
                                    liftIO $ boxPackStart (castToBox parent) np PackGrow 0
                                    liftIO $ boxReorderChild (castToVBox parent) np 2
                                _ -> error "No notebook index found in viewSplit"
                            liftIO $ do
                                widgetShowAll np
                                widgetGrabFocus activeNotebook
                                case nbi of
                                    Just n -> do
                                        notebookSetCurrentPage (castToNotebook parent) n
                                        return ()
                                    _      -> trace ("ViewFrame>>viewSplit': parent not a notebook: ")return ()
                                return (nb,paneDir)
                        handleFunc <-  runInIO (handleNotebookSwitch nb)
                        liftIO $ afterSwitchPage nb handleFunc
                        return (Just (paneDir,dir))
            case mbPD of
              Just (paneDir,pdir) -> do
                  let toPane = panePath ++ [SplitP paneDir]
                  adjustPanes panePath toPane
                  adjustLayoutForSplit paneDir panePath
                  viewMove (otherDirection paneDir)
              Nothing -> return ()

viewNewGroup :: PaneMonad alpha => alpha ()
viewNewGroup = do
    mainWindow <- getMainWindow
    mbGroupName <- liftIO $ groupNameDialog mainWindow
    case
     mbGroupName of
        Just groupName -> do
            layout <- getLayoutSt
            if groupName `Set.member` allGroupNames layout
                then liftIO $ do
                    md <- messageDialogNew (Just mainWindow) [] MessageWarning ButtonsClose
                        ("Group name not unique " ++ groupName)
                    dialogRun md
                    widgetDestroy md
                    return ()
                else viewNest groupName
        Nothing -> return ()

newGroupOrBringToFront :: PaneMonad alpha => String -> PanePath -> alpha (Maybe PanePath,Bool)
newGroupOrBringToFront groupName pp = do
    layout <- getLayoutSt
    if groupName `Set.member` allGroupNames layout
        then do
            mbPP <- bringGroupToFront groupName
            return (mbPP,False)
        else let realPath = getBestPanePath pp layout in do
            viewNest' realPath groupName
            return (Just (realPath ++ [GroupP groupName]),True)

bringGroupToFront :: PaneMonad alpha => String -> alpha (Maybe PanePath)
bringGroupToFront groupName = do
    layout <- getLayoutSt
    case findGroupPath groupName layout   of
        Just path -> do
            widget <- getNotebookOrPaned path castToWidget
            liftIO $ setCurrentNotebookPages widget
            return (Just path)
        Nothing -> return Nothing


--  Yet another stupid little dialog

groupNameDialog :: Window -> IO (Maybe String)
groupNameDialog parent =  liftIO $ do
    dia                        <-   dialogNew
    windowSetTransientFor dia parent
    windowSetTitle dia "Enter group name"
    upper                      <-   dialogGetUpper dia
    lower                      <-   dialogGetActionArea dia
    (widget,inj,ext,_)         <-   buildEditor moduleFields ""
    (widget2,_,_,notifier)     <-   buildEditor okCancelFields ()
    registerEvent notifier Clicked (Left (\e -> do
            case eventPaneName e of
                "Ok"    ->  dialogResponse dia ResponseOk
                _       ->  dialogResponse dia ResponseCancel
            return e))
    boxPackStart upper widget PackGrow 7
    boxPackStart lower widget2 PackNatural 7
    widgetShowAll dia
    resp <- dialogRun dia
    value                      <- ext ("")
    widgetDestroy dia
    case resp of
        ResponseOk | value /= Just ""  -> return value
        _                             -> return Nothing
    where
        moduleFields :: FieldDescription String
        moduleFields = VFD emptyParams [
                mkField
                    (paraName <<<- ParaName ("New group ")
                            $ emptyParams)
                    id
                    (\ a b -> a)
            (stringEditor (\s -> True))]

viewNest :: PaneMonad alpha => String -> alpha ()
viewNest group = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewNest' panePath group

viewNest' :: PaneMonad alpha => PanePath -> String -> alpha ()
viewNest' panePath group = do
    activeNotebook  <- getNotebook panePath
    mbParent  <- liftIO $ widgetGetParent activeNotebook
    case mbParent of
        Nothing -> return ()
        Just parent -> do
            layout          <-  getLayoutSt
            let paneLayout  =   layoutFromPath panePath layout
            case paneLayout of
                (TerminalP {}) -> do
                    nb <- newNotebook (panePath ++ [GroupP group])
                    liftIO $ do
                        widgetSetName nb ("group_" ++ group)
                        notebookAppendPage activeNotebook nb group
                    label <- groupLabel group
                    liftIO $ do
                        notebookSetTabLabel activeNotebook nb label
                        widgetShowAll nb
                        widgetGrabFocus activeNotebook
                        return nb
                    handleFunc <-  runInIO (handleNotebookSwitch nb)
                    liftIO $ afterSwitchPage nb handleFunc
                    adjustLayoutForNest group panePath
                _ -> return ()

closeGroup :: PaneMonad alpha => String -> alpha ()
closeGroup groupName = do
    layout <- getLayout
    let mbPath = findGroupPath groupName layout
    mainWindow <- getMainWindow
    case mbPath of
        Nothing -> trace ("ViewFrame>>closeGroup: Group path not found: " ++ groupName) return ()
        Just path -> do
            panesMap <- getPaneMapSt
            let nameAndpathList  = filter (\(a,pp) -> path `isPrefixOf` pp)
                            $ map (\(a,b) -> (a,fst b)) (Map.assocs panesMap)
            continue <- case nameAndpathList of
                            (_:_) -> liftIO $ do
                                md <- messageDialogNew (Just mainWindow) [] MessageQuestion ButtonsYesNo
                                    ("Group " ++ groupName ++ " not empty. Close with all contents?")
                                rid <- dialogRun md
                                widgetDestroy md
                                case rid of
                                    ResponseYes ->  return True
                                    otherwise   ->  return False
                            []  -> return True
            when continue $ do
                panes <- mapM paneFromName $ map fst nameAndpathList
                results <- mapM (\ (PaneC p) -> close p) panes
                when (foldr (&&) True results) $ do
                    nbOrPaned  <- getNotebookOrPaned path castToWidget
                    mbParent <- liftIO $ widgetGetParent nbOrPaned
                    case mbParent of
                        Nothing -> error "ViewFrame>>closeGroup: closeGroup: no parent"
                        Just parent -> liftIO $ containerRemove (castToContainer parent) nbOrPaned
                    setLayoutSt (removeGL path layout)
                    ppMap <- getPanePathFromNB
                    setPanePathFromNB (Map.filter (\pa -> path `isPrefixOf` pa) ppMap)
                    return ()

viewDetach :: PaneMonad alpha => alpha (Maybe (Window,Widget))
viewDetach = do
    id <- liftIO $ fmap show getCPUTime
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return Nothing
        Just panePath -> do
            viewDetach' panePath id

viewDetach' :: PaneMonad alpha => PanePath -> String -> alpha (Maybe (Window,Widget))
viewDetach' panePath id = do
    activeNotebook  <- getNotebook panePath
    mbParent  <- liftIO $ widgetGetParent activeNotebook
    case mbParent of
        Nothing -> return Nothing
        Just parent -> do
            layout          <-  getLayoutSt
            let paneLayout  =   layoutFromPath panePath layout
            case paneLayout of
                (TerminalP{detachedSize = size}) -> do
                    window <- liftIO $ do
                        window <- windowNew
                        windowSetTitle window "leksah detached window"
                        widgetSetName window id
                        case size of
                            Just (width, height) -> do
                                windowSetDefaultSize window width height
                            Nothing -> do
                                (curWidth, curHeight) <- widgetGetSize activeNotebook
                                windowSetDefaultSize window curWidth curHeight
                        containerRemove (castToContainer parent) activeNotebook
                        containerAdd window activeNotebook
                        widgetShowAll window
                        return window
                    handleFunc <-  runInIO (handleReattach id window)
                    liftIO $ window `onDelete` handleFunc
                    windows <- getWindowsSt
                    setWindowsSt $ windows ++ [window]
                    adjustLayoutForDetach id panePath
                    return (Just (window, castToWidget activeNotebook))
                _ -> return Nothing



handleReattach :: PaneMonad alpha => String -> Window -> Event -> alpha Bool
handleReattach windowId window _ = do
    layout <- getLayout
    case findDetachedPath windowId layout of
        Nothing -> trace ("ViewFrame>>handleReattach: panePath for id not found: " ++ windowId)
                $ do
            windows <- getWindowsSt
            setWindowsSt $ delete window windows
            return False
        Just pp -> do
            nb      <- getNotebook pp
            parent  <- getNotebookOrPaned (init pp) castToContainer
            liftIO $ containerRemove (castToContainer window) nb
            liftIO $ containerAdd parent nb
            adjustLayoutForReattach pp
            windows <- getWindowsSt
            setWindowsSt $ delete window windows
            case last pp of
                GroupP groupName -> do
                    label <- groupLabel groupName
                    liftIO $ notebookSetTabLabel (castToNotebook parent) nb label
                otherwise       -> return ()
            return False -- "now destroy the window"

groupLabel :: PaneMonad beta => String -> beta EventBox
groupLabel group = do
    label <- liftIO $ labelNew Nothing
    liftIO $ labelSetUseMarkup label True
    liftIO $ labelSetMarkup label ("<b>" ++ group ++ "</b>")
    labelBox <- mkLabelBox label ("group_" ++ group)
    liftIO $ widgetShowAll labelBox
    return labelBox

handleNotebookSwitch :: PaneMonad beta => Notebook -> Int -> beta ()
handleNotebookSwitch nb index = do
    mbW <- liftIO $ notebookGetNthPage nb index
    case mbW of
        Nothing -> error "ViewFrame/handleNotebookSwitch: Can't find widget"
        Just w  -> do
            name   <-  liftIO $ widgetGetName w
            mbPane <-  findPaneFor name
            case mbPane of
                Nothing         ->  return () -- trace ("ViewFrame>>handleNotebookSwitch: can't find pane " ++ name)

                Just (PaneC p)  ->  makeActive p -- trace ("ViewFrame>>handleNotebookSwitch: switch to pane " ++ name)

    where
        findPaneFor :: PaneMonad beta => String -> beta (Maybe (IDEPane beta))
        findPaneFor n1   =   do
            panes'      <-  getPanesSt
            foldM (\r (PaneC p) -> do
                n2 <- liftIO $ widgetGetName (getTopWidget p)
                return (if n1 == n2 then (Just (PaneC p)) else r))
                        Nothing (Map.elems panes')

--
-- | Two notebooks can be collapsed to one
--
viewCollapse :: PaneMonad alpha => alpha ()
viewCollapse = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewCollapse' panePath

viewCollapse' :: PaneMonad alpha => PanePath -> alpha ()
viewCollapse' panePath = do
    trace ("Collapse : " ++ (show panePath)) $ return ()
    layout1           <- getLayoutSt
    case layoutFromPath panePath layout1 of
        (TerminalP _ _ _ (Just _) _) -> trace ("ViewFrame>>viewCollapse': can't collapse detached: ")
                                            return ()
        _                            -> do
            let newPanePath   = init panePath
            let mbOtherSidePath = otherSide panePath
            case mbOtherSidePath of
                Nothing -> return ()
                Just otherSidePath ->
                    let sp1 = getGroupPathForCollapse panePath layout1
                        sp2 = getGroupPathForCollapse otherSidePath layout1
                    in do
                    case sp1 of
                        Nothing -> return ()
                        Just sp -> viewCollapse' sp
                    case  sp2 of
                        Nothing -> return ()
                        Just sp -> viewCollapse' sp
                    paneMap         <- getPaneMapSt
                    activeNotebook  <- getNotebook panePath
                    st  <- getFrameState
                    otherSideNotebook <- getNotebook otherSidePath
                    setFrameState st{panePathFromNB = (Map.delete otherSideNotebook
                                                        (Map.insert activeNotebook newPanePath (panePathFromNB st)))}
                    let paneNamesToMove = map (\(w,(p,_)) -> w)
                                            $filter (\(w,(p,_)) -> otherSidePath `isPrefixOf` p)
                                                $Map.toList paneMap
                    panesToMove <- mapM paneFromName paneNamesToMove
                    mapM_ (\(PaneC p) -> move panePath p) panesToMove
                    mbParent <- liftIO $ widgetGetParent activeNotebook
                    case mbParent of
                        Nothing -> error "collapse: no parent"
                        Just parent -> do
                            mbGrandparent <- liftIO $ widgetGetParent parent
                            case mbGrandparent of
                                Nothing -> error "collapse: no grandparent"
                                Just grandparent -> do
                                    nbIndex <- if grandparent `isA` gTypeNotebook
                                        then liftIO $ notebookPageNum (castToNotebook grandparent) parent
                                        else return Nothing
                                    liftIO $ containerRemove (castToContainer grandparent) parent
                                    liftIO $ containerRemove (castToContainer parent) activeNotebook
                                    if length panePath > 1
                                        then do
                                            let lasPathElem = last newPanePath
                                            case (lasPathElem, nbIndex) of
                                                (SplitP dir, _) | dir == TopP || dir == LeftP ->
                                                    liftIO $ panedPack1 (castToPaned grandparent) activeNotebook True True
                                                (SplitP dir, _) | dir == BottomP || dir == RightP ->
                                                    liftIO $ panedPack2 (castToPaned grandparent) activeNotebook True True
                                                (GroupP group, Just n) -> do
                                                    liftIO $ notebookInsertPage (castToNotebook grandparent) activeNotebook group n
                                                    label <- groupLabel group
                                                    liftIO $ do
                                                        notebookSetTabLabel (castToNotebook grandparent) activeNotebook label
                                                        notebookSetCurrentPage (castToNotebook grandparent) n
                                                        return ()
                                                _ -> error "collapse: Unable to find page index"
                                            liftIO $ widgetSetName activeNotebook $panePathElementToWidgetName lasPathElem
                                        else liftIO $ do
                                            boxPackStart (castToVBox grandparent) activeNotebook PackGrow 0
                                            boxReorderChild (castToVBox grandparent) activeNotebook 2
                                            widgetSetName activeNotebook "root"
                    adjustPanes panePath newPanePath
                    adjustPanes otherSidePath newPanePath
                    adjustLayoutForCollapse panePath

getGroupPathForCollapse :: PanePath -> PaneLayout -> Maybe PanePath
getGroupPathForCollapse path layout =
    case layoutFromPath path layout of
        TerminalP {}   -> Nothing
        HorizontalP _ _ _   -> Just (path ++ [SplitP TopP])
        VerticalP _ _ _     -> Just (path ++ [SplitP LeftP])



--
-- | Moves the activePane in the given direction, if possible
-- | If their are many possibilities choose the leftmost and topmost
--
viewMove :: PaneMonad beta => PaneDirection -> beta  ()
viewMove direction = do
    mbPane <- getActivePaneSt
    case mbPane of
        Nothing -> do
            return ()
        Just (paneName,_) -> do
            (PaneC pane) <- paneFromName paneName
            mbPanePath <- getActivePanePath
            case mbPanePath of
                Nothing -> do
                    return ()
                Just panePath -> do
                  layout <- getLayoutSt
                  case findMoveTarget panePath layout direction of
                      Nothing -> do
                        return ()
                      Just moveTo -> move moveTo pane

--
-- | Find the target for a move
--
findMoveTarget :: PanePath -> PaneLayout -> PaneDirection -> Maybe PanePath
findMoveTarget panePath layout direction=
    let oppositeDir          = otherDirection direction
        canMove []           = []
        canMove reversedPath =
            case head reversedPath of
                SplitP d | d == oppositeDir
                    -> SplitP direction : (tail reversedPath)
                GroupP group -> []
                _                     -> canMove (tail reversedPath)
        basePath = reverse (canMove $ reverse panePath)
    in case basePath of
        [] -> Nothing
        _  -> let layoutP  = layoutFromPath basePath layout
             in  trace (show basePath) Just $basePath ++ findAppropriate layoutP oppositeDir

dragMove :: PaneMonad alpha => (PaneName,Notebook) -> alpha ()
dragMove (paneName,toNB) = do
    paneMap         <-  getPaneMapSt
    panes           <-  getPanesSt
    layout          <-  getLayout
    frameState      <-  getFrameState
    case "group_" `stripPrefix` paneName of
        Just group  -> do
            trace ("dragMove a group Layout before: " ++ show layout) return ()
            case findGroupPath group layout of
                Nothing -> trace ("ViewFrame>>dragMove: group not found: " ++ group) return ()
                Just fromPath -> do
                    groupNBOrPaned <- getNotebookOrPaned fromPath castToWidget
                    fromNB  <- getNotebook (init fromPath)
                    case toNB `Map.lookup` (panePathFromNB frameState) of
                        Nothing -> trace "ViewFrame>>dragMove: panepath for Notebook not found" return ()
                        Just toPath -> do
                            when (fromNB /= toNB && not (isPrefixOf fromPath toPath)) $ do
                                mbNum <- liftIO $ notebookPageNum fromNB groupNBOrPaned
                                case mbNum of
                                    Nothing ->  trace "ViewFrame>>dragMove: group notebook not found" return ()
                                    Just num -> do
                                        liftIO $ notebookRemovePage fromNB num
                                        label <- groupLabel group
                                        notebookInsertOrdered toNB groupNBOrPaned group Nothing
                                        liftIO $ notebookSetTabLabel toNB groupNBOrPaned label
                                        adjustPanes fromPath (toPath ++ [GroupP group])
                                        adjustLayoutForGroupMove fromPath toPath group
                                        adjustNotebooks fromPath (toPath ++ [GroupP group])
                                        layout2          <-  getLayout
                                        trace ("dragMove a group Layout after: " ++ show layout2) return ()
                                        return ()
        Nothing     ->
            case paneName `Map.lookup` panes of
                Nothing -> trace ("ViewFrame>>dragMove: pane not found: " ++ paneName) return ()
                Just (PaneC pane) -> do
                    case toNB `Map.lookup` (panePathFromNB frameState) of
                        Nothing -> trace "ViewFrame>>dragMove: panepath for Notebook not found" return ()
                        Just toPath ->
                            case paneName `Map.lookup`paneMap of
                                Nothing -> trace ("ViewFrame>>dragMove: pane data not found: " ++ paneName)
                                            return ()
                                Just (fromPath,_) -> do
                                    let child = getTopWidget pane
                                    (fromPane,cid)  <-  guiPropertiesFromName paneName
                                    fromNB          <-  trace ("dragMove: pane name: " ++ paneName ++ " path: " ++ show fromPane)
                                                            getNotebook fromPane
                                    when (fromNB /= toNB) $ do
                                        mbNum <- liftIO $ notebookPageNum fromNB child
                                        case mbNum of
                                            Nothing ->  trace "ViewFrame>>dragMove: widget not found" return ()
                                            Just num -> do
                                                liftIO $ notebookRemovePage fromNB num
                                                notebookInsertOrdered toNB child paneName Nothing
                                                let paneMap1    =   Map.delete paneName paneMap
                                                trace ("drag move name: " ++ paneName ++ " path: " ++ show toPath) $
                                                    setPaneMapSt    $   Map.insert paneName (toPath,cid) paneMap1

--
-- | Moves the given Pane to the given path
--   TODO use adjust
move ::  Pane alpha beta => PanePath -> alpha -> beta ()
move toPane idew  = do
    paneMap         <-  getPaneMapSt
    let child       =   getTopWidget idew
    (fromPane,cid)  <-  guiPropertiesFromName (paneName idew)
    fromNB          <-  getNotebook fromPane
    toNB            <-  getNotebook toPane
    mbNum           <-  liftIO $ notebookPageNum fromNB child
    case mbNum of
        Nothing -> return ()
        Just pn -> do
            mbBox   <- liftIO $ notebookGetTabLabel fromNB child
            theText <- liftIO $ widgetGetName child
            liftIO $ notebookRemovePage fromNB pn
            notebookInsertOrdered toNB child theText Nothing
    let paneMap1    =   Map.delete (paneName idew) paneMap
    trace ("move name: " ++ (paneName idew) ++ " path: " ++ show toPane) $
        setPaneMapSt    $   Map.insert (paneName idew) (toPane,cid) paneMap1

findAppropriate :: PaneLayout -> PaneDirection -> PanePath
findAppropriate  (TerminalP {}) _ =   []
findAppropriate  (HorizontalP t b _) LeftP     =   SplitP TopP    :  findAppropriate t LeftP
findAppropriate  (HorizontalP t b _) RightP    =   SplitP TopP    :  findAppropriate t RightP
findAppropriate  (HorizontalP t b _) BottomP   =   SplitP BottomP :  findAppropriate b BottomP
findAppropriate  (HorizontalP t b _) TopP      =   SplitP TopP    :  findAppropriate b TopP
findAppropriate  (VerticalP l r _) LeftP       =   SplitP LeftP   :  findAppropriate l LeftP
findAppropriate  (VerticalP l r _) RightP      =   SplitP RightP  :  findAppropriate r RightP
findAppropriate  (VerticalP l r _) BottomP     =   SplitP LeftP   :  findAppropriate l BottomP
findAppropriate  (VerticalP l r _) TopP        =   SplitP LeftP   :  findAppropriate l TopP

--
-- | Bring the pane to the front position in its notebook
--
bringPaneToFront :: Pane alpha beta => alpha -> IO ()
bringPaneToFront pane = do
    let tv = getTopWidget pane
    setCurrentNotebookPages tv


setCurrentNotebookPages widget = do
    mbParent <- widgetGetParent widget
    case mbParent of
        Just parent -> do
            setCurrentNotebookPages parent
            if parent `isA` gTypeNotebook
                then do
                    mbPageNum <- notebookPageNum (castToNotebook parent) widget
                    case mbPageNum of
                        Just pageNum -> do
                            notebookSetCurrentPage (castToNotebook parent) pageNum
                            return ()
                        Nothing      -> return ()
                else return ()
        Nothing -> return ()
                    --do -- TODO use this?
            -- widgetShowAll widget
            -- windowPresent (castToWindow widget)

--
-- | Get a valid panePath from a standard path.
--
getBestPanePath :: StandardPath -> PaneLayout -> PanePath
getBestPanePath sp pl = let res = reverse $ getStandard' sp pl []
    in trace ("ViewFrame>>getBestPanePath in: " ++ show sp ++ " out: " ++ show res) res
    where
    getStandard' (GroupP group:sp) (TerminalP {paneGroups = groups}) p
        | group `Map.member` groups                 =   getStandard' sp (groups Map.! group) (GroupP group:p)
    getStandard' _ (TerminalP {}) p              =   p
    getStandard' (SplitP LeftP:sp) (VerticalP l r _) p     =   getStandard' sp l (SplitP LeftP:p)
    getStandard' (SplitP RightP:sp) (VerticalP l r _) p    =   getStandard' sp r (SplitP RightP:p)
    getStandard' (SplitP TopP:sp) (HorizontalP t b _) p    =   getStandard' sp t (SplitP TopP:p)
    getStandard' (SplitP BottomP:sp) (HorizontalP t b _) p =   getStandard' sp b (SplitP BottomP:p)
    -- if no match get leftmost topmost
    getStandard' _ (VerticalP l r _) p              =   getStandard' [] l (SplitP LeftP:p)
    getStandard' _ (HorizontalP t b _) p            =   getStandard' [] t (SplitP TopP:p)

--
-- | Get a standard path.
--
getBestPathForId :: PaneMonad alpha => String -> alpha PanePath
getBestPathForId  id = do
    p <- panePathForGroup id
    l <- getLayout
    return (getBestPanePath p l)
		
--
-- | Construct a new notebook
--
newNotebook' :: IO Notebook
newNotebook' = do
    nb <- notebookNew
    notebookSetTabPos nb PosTop
    notebookSetShowTabs nb True
    notebookSetScrollable nb True
    notebookSetPopup nb True
    return nb

--
-- | Construct a new notebook,
--
newNotebook :: PaneMonad alpha => PanePath -> alpha Notebook
newNotebook pp = do
    st  <- trace ("newNotebook path: " ++ show pp)
                getFrameState
    nb  <- liftIO newNotebook'
    setFrameState st{panePathFromNB = Map.insert nb pp (panePathFromNB st)}
    func <- runInIO (dragMove)
    liftIO $ do
        tl <- targetListNew
        targetListAddTextTargets tl 0
        dragDestSet nb [DestDefaultAll] [ActionCopy, ActionMove]
        dragDestSetTargetList nb tl
        on nb dragDataReceived (dragFunc nb func)
        return nb
    where
        dragFunc ::
            Notebook ->
            ((PaneName,Notebook) -> IO ()) ->
            DragContext ->
            Point ->
            InfoId ->
            TimeStamp ->
            (SelectionDataM ())
        dragFunc nb func cont point id timeStamp = do
            mbText <- selectionDataGetText
            case mbText of
                Nothing -> return ()
                Just str -> do
                    liftIO $ func (str,nb)
                    return ()

terminalsWithPanePath :: PaneLayout -> [(PanePath,PaneLayout)]
terminalsWithPanePath pl = map (\ (pp,l) -> (reverse pp,l)) $ terminalsWithPP [] pl
    where
        terminalsWithPP pp t@(TerminalP groups _ _ _ _) =  [(pp,t)]
                                            ++ concatMap (terminalsFromGroup pp) (Map.toList groups)
        terminalsWithPP pp (VerticalP l r _)       =  terminalsWithPP (SplitP LeftP : pp) l
                                                        ++ terminalsWithPP (SplitP RightP : pp) r
        terminalsWithPP pp (HorizontalP t b _)     =  terminalsWithPP (SplitP TopP : pp) t
                                                        ++ terminalsWithPP (SplitP BottomP : pp) b
        terminalsFromGroup pp (name,layout)        =  terminalsWithPP (GroupP name : pp) layout

findGroupPath :: String -> PaneLayout -> Maybe PanePath
findGroupPath group layout =
    let terminalPairs = terminalsWithPanePath layout
    in case (filter filterFunc terminalPairs) of
        [] -> Nothing
        (pp,_) : [] -> Just (pp ++ [GroupP group])
        _ -> error ("ViewFrame>>group name not unique: " ++ group)
    where
        filterFunc (_,(TerminalP groups _ _ _ _)) =  group  `Set.member` Map.keysSet groups
        filterFunc _                              =  error "ViewFrame>>findGroupPath: impossible"

findDetachedPath :: String -> PaneLayout -> Maybe PanePath
findDetachedPath id layout =
    let terminalPairs = terminalsWithPanePath layout
    in case (filter filterFunc terminalPairs) of
        [] -> Nothing
        (pp,_) : [] -> Just pp
        _ -> error ("ViewFrame>>window id not unique: " ++ id)
    where
        filterFunc (_,(TerminalP _ _ _ (Just lid) _)) = lid == id
        filterFunc _                                  = False


allGroupNames :: PaneLayout -> Set String
allGroupNames pl = Set.unions $ map getFunc (terminalsWithPanePath pl)
    where
        getFunc (_,(TerminalP groups _ _ _ _)) =  Map.keysSet groups
        getFunc _                              =  error "ViewFrame>>allGroupNames: impossible"


--
-- | Get another pane path which points to the other side at the same level
--
otherSide :: PanePath -> Maybe PanePath
otherSide []    =   Nothing
otherSide p     =   let rp = reverse p
                    in case head rp of
                        SplitP d -> Just (reverse $ SplitP (otherDirection d) : tail rp)
                        _        -> Nothing

--
-- | Get the opposite direction of a pane direction
--
otherDirection :: PaneDirection -> PaneDirection
otherDirection LeftP    = RightP
otherDirection RightP   = LeftP
otherDirection TopP     = BottomP
otherDirection BottomP  = TopP

--
-- | Get the layout at the given pane path
--
layoutFromPath :: PanePath -> PaneLayout -> PaneLayout
layoutFromPath [] l                                   = l
layoutFromPath (GroupP group:r) (TerminalP {paneGroups = groups})
    | group `Map.member` groups                       = layoutFromPath r (groups Map.! group)
layoutFromPath (SplitP TopP:r) (HorizontalP t _ _)    = layoutFromPath r t
layoutFromPath (SplitP BottomP:r) (HorizontalP _ b _) = layoutFromPath r b
layoutFromPath (SplitP LeftP:r) (VerticalP l _ _)     = layoutFromPath r l
layoutFromPath (SplitP RightP:r) (VerticalP _ ri _)   = layoutFromPath r ri
layoutFromPath pp l                                   = error
    $"inconsistent layout (layoutFromPath) " ++ show pp ++ " " ++ show l

layoutsFromPath :: PanePath -> PaneLayout -> [PaneLayout]
layoutsFromPath (GroupP group:r) layout@(TerminalP {paneGroups = groups})
    | group `Map.member` groups
        = layout:layoutsFromPath r (groups Map.! group)
layoutsFromPath [] layout                                     =   [layout]
layoutsFromPath (SplitP TopP:r) layout@(HorizontalP t b _)    =   layout:layoutsFromPath r t
layoutsFromPath (SplitP BottomP:r) layout@(HorizontalP t b _) =   layout:layoutsFromPath r b
layoutsFromPath (SplitP LeftP:r) layout@(VerticalP l ri _)    =   layout:layoutsFromPath r l
layoutsFromPath (SplitP RightP:r) layout@(VerticalP l ri _)   =   layout:layoutsFromPath r ri
layoutsFromPath pp l                                      = error
    $"inconsistent layout (layoutsFromPath) " ++ show pp ++ " " ++ show l

getWidgetNameList :: PanePath -> PaneLayout -> [String]
getWidgetNameList path layout = reverse $ nameList (reverse path) (reverse $ layoutsFromPath path layout)
    where
        nameList [] _ = reverse ["Leksah Main Window","topBox","root"]
        nameList (pe:_) (TerminalP{detachedId = Just id}:_) = [panePathElementToWidgetName pe, id]
        nameList (pe:rpath) (_:rlayout) = panePathElementToWidgetName pe : nameList rpath rlayout
        nameList _ _ = error $ "inconsistent layout (getWidgetNameList) " ++ show path ++ " " ++ show layout

getNotebookOrPaned :: PaneMonad alpha => PanePath -> (Widget -> beta) -> alpha beta
getNotebookOrPaned p cf = do
    layout <- getLayout
    (widgetGet $ getWidgetNameList p layout) cf

--
-- | Get the notebook widget for the given pane path
--
getNotebook :: PaneMonad alpha => PanePath -> alpha  Notebook
getNotebook p = getNotebookOrPaned p castToNotebook

--
-- | Get the (gtk) Paned widget for a given path
--
getPaned :: PaneMonad alpha => PanePath -> alpha Paned
getPaned p = getNotebookOrPaned p castToPaned

--
-- | Get the path to the active pane
--
getActivePanePath :: PaneMonad alpha => alpha  (Maybe PanePath)
getActivePanePath = do
    mbPane   <- getActivePaneSt
    case mbPane of
        Nothing -> return Nothing
        Just (paneName,_) -> do
            (pp,_)  <- guiPropertiesFromName paneName
            return (Just (pp))

getActivePanePathOrStandard :: PaneMonad alpha => StandardPath -> alpha  (PanePath)
getActivePanePathOrStandard sp = do
    mbApp <- getActivePanePath
    case mbApp of
        Just app -> return app
        Nothing -> do
            layout <- getLayoutSt
            return (getBestPanePath sp layout)

--
-- | Get the active notebook
--
getActiveNotebook :: PaneMonad alpha => alpha  (Maybe Notebook)
getActiveNotebook = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Just panePath -> do
            nb <- getNotebook panePath
            return (Just nb)
        Nothing -> return Nothing


--
-- | Translates a pane direction to the widget name
--
paneDirectionToWidgetName           :: PaneDirection -> String
paneDirectionToWidgetName TopP      =  "top"
paneDirectionToWidgetName BottomP   =  "bottom"
paneDirectionToWidgetName LeftP     =  "left"
paneDirectionToWidgetName RightP    =  "right"

panePathElementToWidgetName :: PanePathElement -> String
panePathElementToWidgetName (SplitP dir)   = paneDirectionToWidgetName dir
panePathElementToWidgetName (GroupP group) = "group_" ++ group

--
-- | Changes a pane path in the pane map
--
adjustPanes :: PaneMonad alpha => PanePath -> PanePath -> alpha ()
adjustPanes fromPane toPane  = do
    trace ("adjustPanes: " ++ show fromPane ++ " -> " ++ show toPane) $ return ()
    paneMap     <- getPaneMapSt
    setPaneMapSt (Map.map (\(pp,other) ->
        case stripPrefix fromPane pp of
            Just rest -> (toPane++rest,other)
            _         -> (pp,other)) paneMap)

adjustNotebooks :: PaneMonad alpha => PanePath -> PanePath -> alpha ()
adjustNotebooks fromPane toPane  = do
    npMap <- getPanePathFromNB
    setPanePathFromNB  (Map.map (\pp ->
        case stripPrefix fromPane pp of
            Just rest -> toPane++rest
            _         -> pp) npMap)

--
-- | Changes the layout for a split
--
adjustLayoutForSplit :: PaneMonad alpha => PaneDirection -> PanePath -> alpha ()
adjustLayoutForSplit  dir path  = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newLayout   =   TerminalP Map.empty Nothing 0 Nothing Nothing
        newTerm     =   case dir of
                            LeftP   -> VerticalP paneLayout newLayout 0
                            RightP  -> VerticalP newLayout paneLayout 0
                            TopP    -> HorizontalP paneLayout newLayout 0
                            BottomP -> HorizontalP newLayout paneLayout 0
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a nest
--
adjustLayoutForNest :: PaneMonad alpha => String -> PanePath -> alpha ()
adjustLayoutForNest group path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {paneGroups = groups}) -> paneLayout {
                                paneGroups = Map.insert group (TerminalP Map.empty Nothing 0 Nothing Nothing) groups}
                            _          -> error "Unexpected layout type in adjustLayoutForNest"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a detach
--
adjustLayoutForDetach :: PaneMonad alpha => String -> PanePath -> alpha ()
adjustLayoutForDetach id path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {}) -> paneLayout {detachedId = Just id}
                            _              -> error "Unexpected layout type in adjustLayoutForDetach"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a reattach
--
adjustLayoutForReattach :: PaneMonad alpha => PanePath -> alpha ()
adjustLayoutForReattach path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {}) -> paneLayout {detachedId = Nothing, detachedSize = Nothing}
                            _   -> error "Unexpected layout type in adjustLayoutForReattach"
    setLayoutSt     $   adjustLayout path layout newTerm

--
-- | Changes the layout for a collapse
--
adjustLayoutForCollapse :: PaneMonad alpha => PanePath -> alpha ()
adjustLayoutForCollapse oldPath = do
    layout          <-  getLayoutSt
    let oldLayout   =   layoutFromPath oldPath layout
    setLayoutSt     $   adjustLayout (init oldPath) layout oldLayout

--
-- | Changes the layout for a move
--
adjustLayoutForGroupMove :: PaneMonad alpha => PanePath -> PanePath -> String -> alpha ()
adjustLayoutForGroupMove fromPath toPath group = do
    layout <- getLayout
    let layoutToMove = layoutFromPath fromPath layout
    let newLayout = removeGL fromPath layout
    setLayoutSt (addGL layoutToMove (toPath ++ [GroupP group])  newLayout)

--
-- | Changes the layout for a remove
--
adjustLayoutForGroupRemove :: PaneMonad alpha => PanePath -> String -> alpha ()
adjustLayoutForGroupRemove fromPath group = do
    layout <- getLayout
    setLayoutSt (removeGL fromPath layout)

--
-- | Remove group layout at a certain path
--
removeGL :: PanePath -> PaneLayout -> PaneLayout
removeGL [GroupP group] t@(TerminalP oldGroups _ _ _ _)
    | group `Map.member` oldGroups                        =  t{paneGroups = group `Map.delete` oldGroups}
removeGL (GroupP group:r)  old@(TerminalP {paneGroups = groups})
    | group `Map.member` groups                             = old{paneGroups = Map.adjust (removeGL r) group groups}
removeGL (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (removeGL r tp) bp 0
removeGL (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (removeGL r bp) 0
removeGL (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (removeGL r lp) rp 0
removeGL (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (removeGL r rp) 0
removeGL p l = error $"ViewFrame>>removeGL: inconsistent layout" ++ show p ++ " " ++ show l

--
-- | Add group layout at a certain path
--
addGL :: PaneLayout -> PanePath -> PaneLayout -> PaneLayout
addGL toAdd [GroupP group] t@(TerminalP oldGroups _ _ _ _)  =  t{paneGroups = Map.insert group toAdd oldGroups}
addGL toAdd (GroupP group:r)  old@(TerminalP {paneGroups = groups})
    | group `Map.member` groups = old{paneGroups       = Map.adjust (addGL toAdd r) group groups}
addGL toAdd (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (addGL toAdd r tp) bp 0
addGL toAdd (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (addGL toAdd r bp) 0
addGL toAdd (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (addGL toAdd r lp) rp 0
addGL toAdd (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (addGL toAdd r rp) 0
addGL _ p l = error $"ViewFrame>>addGL: inconsistent layout" ++ show p ++ " " ++ show l

--
-- | Changes the layout by replacing element at pane path (pp) with replace
--
adjustLayout :: PanePath -> PaneLayout -> PaneLayout -> PaneLayout
adjustLayout pp layout replace    = adjust' pp layout
    where
    adjust' [] _                                       = replace
    adjust' (GroupP group:r)  old@(TerminalP {paneGroups = groups})
        | group `Map.member` groups =
            old{paneGroups = Map.adjust (adjustPaneGroupLayout r) group groups}
    adjust' (SplitP TopP:r)  (HorizontalP tp bp _)     = HorizontalP (adjust' r tp) bp 0
    adjust' (SplitP BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (adjust' r bp) 0
    adjust' (SplitP LeftP:r)  (VerticalP lp rp _)      = VerticalP (adjust' r lp) rp 0
    adjust' (SplitP RightP:r)  (VerticalP lp rp _)     = VerticalP lp (adjust' r rp) 0
    adjust' p l = error $"inconsistent layout (adjust) " ++ show p ++ " " ++ show l
    adjustPaneGroupLayout p group = adjust' p group

--
-- | Get the widget from a list of strings
--
widgetFromPath :: Widget -> [String] -> IO (Widget)
widgetFromPath w [] = return w
widgetFromPath w path = do
    children    <- containerGetChildren (castToContainer w)
    chooseWidgetFromPath children path

chooseWidgetFromPath :: [Widget] -> [String] -> IO (Widget)
chooseWidgetFromPath _ [] = error $"Cant't find widget (empty path)"
chooseWidgetFromPath widgets (h:t) = do
    names       <- mapM widgetGetName widgets
    let mbiInd  =  findIndex (== h) names
    case mbiInd of
        Nothing     -> error $"Cant't find widget path " ++ show (h:t) ++ " found only " ++ show names
        Just ind    -> widgetFromPath (widgets !! ind) t

widgetGet :: PaneMonad alpha => [String] -> (Widget -> b) -> alpha  (b)
widgetGet strL cf = do
    windows <- getWindowsSt
    r <- liftIO $chooseWidgetFromPath (map castToWidget windows) strL
    return (cf r)

widgetGetRel :: Widget -> [String] -> (Widget -> b) -> IO (b)
widgetGetRel w sl cf = do
    r <- widgetFromPath w sl
    return (cf r)

getUIAction :: PaneMonad alpha => String -> (Action -> a) -> alpha (a)
getUIAction str f = do
    uiManager <- getUiManagerSt
    liftIO $ do
        findAction <- uiManagerGetAction uiManager str
        case findAction of
            Just act -> return (f act)
            Nothing  -> error $"getUIAction can't find action " ++ str

getThis :: PaneMonad delta =>  (FrameState delta -> alpha) -> delta alpha
getThis sel = do
    st <- getFrameState
    return (sel st)
setThis :: PaneMonad delta =>  (FrameState delta -> alpha -> FrameState delta) -> alpha -> delta ()
setThis sel value = do
    st <- getFrameState
    setFrameState (sel st value)

getWindowsSt    = getThis windows
setWindowsSt    = setThis (\st value -> st{windows = value})
getUiManagerSt  = getThis uiManager
getPanesSt      =  getThis panes
setPanesSt      = setThis (\st value -> st{panes = value})
getPaneMapSt    = getThis paneMap
setPaneMapSt    = setThis (\st value -> st{paneMap = value})
getActivePaneSt = getThis activePane
setActivePaneSt = setThis (\st value -> st{activePane = value})
getLayoutSt     = getThis layout
setLayoutSt     = setThis (\st value -> st{layout = value})
getPanePathFromNB  = getThis panePathFromNB
setPanePathFromNB  = setThis (\st value -> st{panePathFromNB = value})


getActivePane   = getActivePaneSt
setActivePane   = setActivePaneSt
getUiManager    = getUiManagerSt
getWindows      = getWindowsSt
getMainWindow   = liftM head getWindows
getLayout       = getLayoutSt
