{-# OPTIONS_GHC  #-}
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
,   getStandardPanePath
,   getActivePanePath
,   getActivePanePathOrStandard
,   figureOutPaneName
,   getNotebook
,   getPaned
,   getActiveNotebook
,   getPane
,   getPanes

-- * View Actions
,   bringPaneToFront
,   newNotebook

-- * Accessing GUI elements
--,   widgetFromPath
,   getUIAction
,   widgetGet


) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite,onToggleOverwrite)
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import Control.OldException(evaluate,catch)
import Prelude hiding(catch)
import Data.Unique
import Data.Typeable

import Graphics.UI.Frame.Panes
import Graphics.UI.Editor.Parameters
import Debug.Trace (trace)
import System.Glib (isA)
import Graphics.UI.Gtk.Types (gTypeNotebook)
import System.CPUTime (getCPUTime)

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
    liftIO $ widgetSetName (getTopWidget pane) (show (hashUnique unique))
    setPaneMapSt        (Map.insert (paneName pane) (pp, conn) paneMap')
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

notebookInsertOrdered :: (NotebookClass self, WidgetClass child)		
    => self	
    -> child	-- child - the Widget to use as the contents of the page.
    -> String	-- tabLabel - the label for the page
    -> Maybe Label
    -> IO ()
notebookInsertOrdered nb widget label mbTabLabel = do
    menuLabel   <-  labelNew (Just label)
    tabLabel    <-  case mbTabLabel of
                        Nothing -> labelNew (Just label)
                        Just l  -> return l
    numPages    <-  notebookGetNPages nb
    mbWidgets   <-  mapM (notebookGetNthPage nb) [0 .. (numPages-1)]
    widgets     <-  catch (evaluate (map fromJust mbWidgets))
                        (\e -> error "ViewFrame.notebookInsertOrdered: no widget")
    mbLabels    <-  mapM (notebookGetTabLabelText nb) widgets
    labels      <-  catch (evaluate (map fromJust mbLabels))
                        (\e -> error "ViewFrame.notebookInsertOrdered: no label")
    let pos     =   case findIndex (\s -> s > label) labels of
                        Just i  ->  i
                        Nothing ->  -1
    realPos     <-  notebookInsertPageMenu nb widget tabLabel menuLabel pos
    notebookSetCurrentPage nb realPos

-- | Constructs a unique pane name, which is an index and a string
figureOutPaneName :: Map String (IDEPane alpha) -> String -> Int -> (Int,String)
figureOutPaneName bufs bn ind =
    let ind = foldr (\(PaneC buf) ind ->
                if primPaneName buf == bn
                    then max ind ((getAddedIndex buf) + 1)
                    else ind)
                0 (Map.elems bufs)
    in  if ind == 0
            then (0,bn)
            else (ind,bn ++ "(" ++ show ind ++ ")")

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
    activeNotebook  <- getNotebook panePath

    mbPD <- do
      mbParent  <- liftIO $ widgetGetParent activeNotebook
      case mbParent of
          Nothing -> return Nothing
          Just parent -> do
              (nb,paneDir) <- liftIO $ do
                  trace ("Pane path " ++ show panePath) return ()
                  newpane <- case dir of
                                  Horizontal  -> do  h <- vPanedNew
                                                     return (castToPaned h)
                                  Vertical    -> do  v <- hPanedNew
                                                     return (castToPaned v)
                  let (name,altname,paneDir) = case dir of
                              Horizontal  -> ("top","bottom",TopP)
                              Vertical    -> ("left","right",LeftP)
                  rName <- widgetGetName activeNotebook
                  widgetSetName newpane rName
                  nb <- newNotebook
                  widgetSetName nb altname
                  panedPack2 newpane nb True True
                  nbIndex <- if parent `isA` gTypeNotebook
                      then notebookPageNum (castToNotebook parent) activeNotebook
                      else return Nothing
                  containerRemove (castToContainer parent) activeNotebook
                  widgetSetName activeNotebook name
                  panedPack1 newpane activeNotebook True True
                  case (reverse panePath, nbIndex) of
                      (SplitP dir:_, _)
                          | dir `elem` [TopP, LeftP] -> panedPack1 (castToPaned parent) newpane True True
                          | otherwise                -> panedPack2 (castToPaned parent) newpane True True
                      (GroupP group:_, Just n) -> do
                          notebookInsertPage (castToNotebook parent) newpane group n
                          label <- groupLabel group
                          notebookSetTabLabel (castToNotebook parent) newpane label
                          return ()
                      ([], _) -> do
                          boxPackStart (castToBox parent) newpane PackGrow 0
                          boxReorderChild (castToVBox parent) newpane 2
                      _ -> error "No notebook index found in viewSplit"
                  widgetShowAll newpane
                  widgetGrabFocus activeNotebook
                  case nbIndex of
                      Just n -> do
                          notebookSetCurrentPage (castToNotebook parent) n
                          return ()
                      _      -> return ()
                  return (nb,paneDir)
              handleFunc <-  runInIO (handleNotebookSwitch nb)
              liftIO $ afterSwitchPage nb handleFunc
              return (Just (paneDir,dir))
    case mbPD of
      Just (paneDir,pdir) -> do
          let toPane = panePath ++ [SplitP paneDir]
          adjustPane panePath toPane
          adjustLayoutForSplit paneDir panePath
          viewMove (otherDirection paneDir)
      Nothing -> return ()

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
                    nb <- liftIO $ do
                        nb <- newNotebook
                        widgetSetName nb ("group_"++group)
                        notebookAppendPage activeNotebook nb group
                        label <- groupLabel group
                        notebookSetTabLabel activeNotebook nb label
                        widgetShowAll nb
                        widgetGrabFocus activeNotebook
                        return nb
                    handleFunc <-  runInIO (handleNotebookSwitch nb)
                    liftIO $ afterSwitchPage nb handleFunc
                    adjustLayoutForNest group panePath
                _ -> return ()

viewDetach :: PaneMonad alpha => alpha ()
viewDetach = do
    id <- liftIO $ fmap show getCPUTime
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewDetach' panePath id

viewDetach' :: PaneMonad alpha => PanePath -> String -> alpha ()
viewDetach' panePath id = do
    activeNotebook  <- getNotebook panePath

    mbParent  <- liftIO $ widgetGetParent activeNotebook
    case mbParent of
        Nothing -> return ()
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
                    windows <- getWindowsSt
                    setWindowsSt $ windows ++ [window]
                    adjustLayoutForDetach id panePath
                _ -> return ()


groupLabel :: String -> IO Label
groupLabel group = do
    label <- labelNew Nothing
    labelSetUseMarkup label True
    labelSetMarkup label ("<b>" ++ group ++ "</b>")
    return label

handleNotebookSwitch :: PaneMonad beta => Notebook -> Int -> beta ()
handleNotebookSwitch nb index = do
    mbW <- liftIO $ notebookGetNthPage nb index
    case mbW of
        Nothing -> error "ViewFrame/handleNotebookSwitch: Can't find widget"
        Just w  -> do
            mbPane <- findPaneFor w
            case mbPane of
                Nothing         ->  return ()
                Just (PaneC p)  ->  makeActive p
    where
        findPaneFor :: PaneMonad beta => Widget -> beta (Maybe (IDEPane beta))
        findPaneFor w   =   do
            panes'      <-  getPanesSt
            n1          <-  liftIO $ widgetGetName w
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
    let newPanePath   = init panePath
    let mbOtherSidePath = otherSide panePath
    case mbOtherSidePath of
        Nothing -> return ()
        Just otherSidePath ->
            let sp1 = getSubpath panePath layout1
                sp2 = getSubpath otherSidePath layout1
            in do
            case sp1 of
                Nothing -> return ()
                Just sp -> viewCollapse' sp
            case  sp2 of
                Nothing -> return ()
                Just sp -> viewCollapse' sp
            paneMap         <- getPaneMapSt
            activeNotebook  <- getNotebook panePath
            let paneNamesToMove = map (\(w,(p,_)) -> w)
                                    $filter (\(w,(p,_)) -> otherSidePath `isPrefixOf` p)
                                        $Map.toList paneMap
            panesToMove <- mapM paneFromName paneNamesToMove
            mapM_ (\(PaneC p) -> move panePath p) panesToMove
            liftIO $ do
                mbParent <- widgetGetParent activeNotebook
                case mbParent of
                    Nothing -> error "collapse: no parent"
                    Just parent -> do
                        mbGrandparent <- widgetGetParent parent
                        case mbGrandparent of
                            Nothing -> error "collapse: no grandparent"
                            Just grandparent -> do
                                nbIndex <- if grandparent `isA` gTypeNotebook
                                    then notebookPageNum (castToNotebook grandparent) parent
                                    else return Nothing
                                containerRemove (castToContainer grandparent) parent
                                containerRemove (castToContainer parent) activeNotebook
                                if length panePath > 1
                                    then do
                                        let lasPathElem = last newPanePath
                                        case (lasPathElem, nbIndex) of
                                            (SplitP dir, _) | dir == TopP || dir == LeftP ->
                                                panedPack1 (castToPaned grandparent) activeNotebook True True
                                            (SplitP dir, _) | dir == BottomP || dir == RightP ->
                                                panedPack2 (castToPaned grandparent) activeNotebook True True
                                            (GroupP group, Just n) -> do
                                                notebookInsertPage (castToNotebook grandparent) activeNotebook group n
                                                label <- groupLabel group
                                                notebookSetTabLabel (castToNotebook grandparent) activeNotebook label
                                                notebookSetCurrentPage (castToNotebook grandparent) n
                                                return ()
                                            _ -> error "collapse: Unable to find page index"
                                        widgetSetName activeNotebook $panePathElementToWidgetName lasPathElem
                                    else do
                                        boxPackStart (castToVBox grandparent) activeNotebook PackGrow 0
                                        boxReorderChild (castToVBox grandparent) activeNotebook 2
                                        widgetSetName activeNotebook "root"
            adjustLayoutForCollapse panePath
            adjustPane panePath newPanePath
                --adjustPane otherSidePath newPanePath

--
-- | Moves the given Pane to the given path
--
move ::  Pane alpha beta => PanePath -> alpha -> beta ()
move toPane idew  = do
    paneMap         <-  getPaneMapSt
    let child       =   getTopWidget idew
    (fromPane,cid)  <-  guiPropertiesFromName (paneName idew)
    fromNB          <-  getNotebook fromPane
    toNB            <-  getNotebook toPane
    liftIO $ do
        mbNum <- notebookPageNum fromNB child
        case mbNum of
            Nothing -> return ()
            Just pn -> do
                mbText <- notebookGetTabLabelText fromNB child
                mbLabel <- notebookGetTabLabel fromNB child
                case (mbText,mbLabel) of
                    (Just text, Just label) -> do
                        notebookRemovePage fromNB pn
                        notebookInsertOrdered toNB child text (Just (castToLabel label))
                    _ -> return ()
    let paneMap1    =   Map.delete (paneName idew) paneMap
    setPaneMapSt    $   Map.insert (paneName idew) (toPane,cid) paneMap1

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
--                    case layoutFromPath (reverse (tail reversedPath)) layout of
--                        TerminalP groups _ _
--                            | group `Map.member` groups -> do
--                                let (before, after) = Map.split group groups
--                                case (reverse (Map.keys before), Map.keys after) of
--                                    (previous:_, _) | direction `elem` [LeftP, TopP] ->
--                                        GroupP previous : (tail reversedPath)
--                                    (_, next:_) | direction `elem` [RightP, BottomP] ->
--                                        GroupP next : (tail reversedPath)
--                                    _ -> canMove (tail reversedPath)
--                        _             -> canMove (tail reversedPath)
                _                     -> canMove (tail reversedPath)
        basePath = reverse (canMove $ reverse panePath)
    in case basePath of
        [] -> Nothing
        _  -> let layoutP  = layoutFromPath basePath layout
             in  trace (show basePath) Just $basePath ++ findAppropriate layoutP oppositeDir

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
--    mbParent <- widgetGetParent tv
--    case mbParent of
--        Just parent -> do
--            let nb = castToNotebook parent
--            n <- notebookGetNPages nb
--            r <- filterM (\i -> do
--                        mbp <-  notebookGetNthPage nb i
--                        case mbp of
--                            Nothing -> return False
--                            Just p -> do
--                                mbs <- notebookGetTabLabelText nb p
--                                case mbs of
--                                    Nothing -> return False
--                                    Just s -> return (s == paneName pane))
--                                    [0..(n-1)]
--            case r of
--                [i] -> notebookSetCurrentPage nb i
--                otherwise -> return ()
--        Nothing -> return ()

setCurrentNotebookPages widget = do
    mbParent <- widgetGetParent widget
    case mbParent of
        Just parent -> do
            if parent `isA` gTypeNotebook
                then do
                    mbPageNum <- notebookPageNum (castToNotebook parent) widget
                    case mbPageNum of
                        Just pageNum -> do
                            notebookSetCurrentPage (castToNotebook parent) pageNum
                            return ()
                        Nothing      -> return ()
                else return ()
            setCurrentNotebookPages parent
        Nothing -> do
            name <- widgetGetName widget
            putStrLn $ "Bringing to front " ++ name
            widgetShowAll widget
            windowPresent (castToWindow widget)


--
-- | Get a valid panePath from a standard path.
--
getStandardPanePath :: StandardPath -> PaneLayout -> PanePath
getStandardPanePath sp pl = reverse $ getStandard' sp pl []
    where
    getStandard' (GroupP group:sp) (TerminalP {paneGroups = groups}) p
        | group `Map.member` groups                 =   getStandard' sp (paneGroupLayout (groups Map.! group)) (GroupP group:p)
    getStandard' _ (TerminalP {}) p              =   p
    getStandard' (SplitP LeftP:sp) (VerticalP l r _) p     =   getStandard' sp l (SplitP LeftP:p)
    getStandard' (SplitP RightP:sp) (VerticalP l r _) p    =   getStandard' sp r (SplitP RightP:p)
    getStandard' (SplitP TopP:sp) (HorizontalP t b _) p    =   getStandard' sp t (SplitP TopP:p)
    getStandard' (SplitP BottomP:sp) (HorizontalP t b _) p =   getStandard' sp b (SplitP BottomP:p)
    -- if no match get leftmost topmost
    getStandard' _ (VerticalP l r _) p              =   getStandard' [] l (SplitP LeftP:p)
    getStandard' _ (HorizontalP t b _) p            =   getStandard' [] t (SplitP TopP:p)

--
-- | Construct a new notebook
--
newNotebook :: IO Notebook
newNotebook = do
    nb <- notebookNew
    notebookSetTabPos nb PosTop
    notebookSetShowTabs nb True
    notebookSetScrollable nb True
    notebookSetPopup nb True
    return nb


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
    | group `Map.member` groups                       = layoutFromPath r (paneGroupLayout (groups Map.! group))
layoutFromPath (SplitP TopP:r) (HorizontalP t _ _)    = layoutFromPath r t
layoutFromPath (SplitP BottomP:r) (HorizontalP _ b _) = layoutFromPath r b
layoutFromPath (SplitP LeftP:r) (VerticalP l _ _)     = layoutFromPath r l
layoutFromPath (SplitP RightP:r) (VerticalP _ ri _)   = layoutFromPath r ri
layoutFromPath pp l                                   = error
    $"inconsistent layout (layoutFromPath) " ++ show pp ++ " " ++ show l

layoutsFromPath :: PanePath -> PaneLayout -> [PaneLayout]
layoutsFromPath (GroupP group:r) layout@(TerminalP {paneGroups = groups})
    | group `Map.member` groups
        = layout:layoutsFromPath r (paneGroupLayout (groups Map.! group))
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
    layout <- getLayoutSt
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
            return (getStandardPanePath sp layout)


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
adjustPane :: PaneMonad alpha => PanePath -> PanePath -> alpha ()
adjustPane fromPane toPane  = do
    trace ("adjustPane: " ++ show fromPane ++ " -> " ++ show toPane) $ return ()
    paneMap     <- getPaneMapSt
    let newMap  = Map.map (\(pp,other) -> case stripPrefix fromPane pp of
                                                Just rest -> (toPane++rest,other)
                                                _         -> (pp,other)) paneMap
    setPaneMapSt newMap

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
    setLayoutSt     $   adjust path layout newTerm

--
-- | Changes the layout for a nest
--
adjustLayoutForNest :: PaneMonad alpha => String -> PanePath -> alpha ()
adjustLayoutForNest group path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {paneGroups = groups}) -> paneLayout {
                                paneGroups = Map.insert group (PaneGroup (TerminalP Map.empty Nothing 0 Nothing Nothing)) groups}
                            _                                   -> error "Unexpected layout type in adjustLayoutForNest"
    setLayoutSt     $   adjust path layout newTerm

--
-- | Changes the layout for a detach
--
adjustLayoutForDetach :: PaneMonad alpha => String -> PanePath -> alpha ()
adjustLayoutForDetach id path = do
    layout          <-  getLayoutSt
    let paneLayout  =   layoutFromPath path layout
        newTerm     =   case paneLayout of
                            (TerminalP {}) -> paneLayout {detachedId = Just id}
                            _                                   -> error "Unexpected layout type in adjustLayoutForDetach"
    setLayoutSt     $   adjust path layout newTerm

--
-- | Changes the layout for a collapse

--
adjustLayoutForCollapse :: PaneMonad alpha => PanePath -> alpha ()
adjustLayoutForCollapse oldPath = do
    layout          <-  getLayoutSt
    let oldLayout   =   layoutFromPath oldPath layout
    setLayoutSt     $   adjust (init oldPath) layout oldLayout

getSubpath :: PanePath -> PaneLayout -> Maybe PanePath
getSubpath path layout =
    case layoutFromPath path layout of
        TerminalP {}   -> Nothing
        HorizontalP _ _ _   -> Just (path ++ [SplitP TopP])
        VerticalP _ _ _     -> Just (path ++ [SplitP LeftP])

--
-- | Changes the layout by replacing element at pane path with replace
--
adjust :: PanePath -> PaneLayout -> PaneLayout -> PaneLayout
adjust pp layout replace    = adjust' pp layout
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
    adjustPaneGroupLayout p group = group {paneGroupLayout = adjust' p (paneGroupLayout group)}

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
    uiManager <- getUIManagerSt
    liftIO $ do
        findAction <- uiManagerGetAction uiManager str
        case findAction of
            Just act -> return (f act)
            Nothing  -> error $"getUIAction can't find action " ++ str
