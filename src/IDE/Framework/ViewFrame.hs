-----------------------------------------------------------------------------
--
-- Module      :  IDE.Framework.ViewFrame
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | Splittable panes containing notebooks with any widgets
--
---------------------------------------------------------------------------------

module IDE.Framework.ViewFrame (
    notebookInsertOrdered

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
,   viewCollapse
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
,   activatePane
,   deactivatePane
,   deactivatePaneIfActive

-- * Accessing GUI elements
,   widgetFromPath
,   getCandyState
,   setCandyState

,   getSBSpecialKeys
,   getSBActivePane
,   getSBActivePackage
,   getSBErrors
,   getStatusbarIO
,   getStatusbarLC

) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import Data.Maybe
import GHC.Exception(evaluate,catch)

import IDE.Core.State
import IDE.Core.Types
import IDE.Core.Panes
import IDE.Framework.Parameters

notebookInsertOrdered :: (NotebookClass self, WidgetClass child)	
    => self	
    -> child	-- child - the Widget to use as the contents of the page.
    -> String	-- tabLabel - the label for the page
    -> IO ()
notebookInsertOrdered nb widget label = do
    numPages    <-  notebookGetNPages nb
    mbWidgets   <-  mapM (notebookGetNthPage nb) [0 .. (numPages-1)]
    widgets     <-  catch (evaluate (map fromJust mbWidgets))
                        (\e -> throwIDE "ViewFrame.notebookInsertOrdered: no widget")
    mbLabels    <-  mapM (notebookGetTabLabelText nb) widgets
    labels      <-  catch (evaluate (map fromJust mbLabels))
                        (\e -> throwIDE "ViewFrame.notebookInsertOrdered: no label")
    let pos     =   case findIndex (\s -> s > label) labels of
                        Just i  ->  i
                        Nothing ->  -1
    realPos     <-  notebookInsertPage  nb widget label pos
    notebookSetCurrentPage nb realPos
getPane ::  CastablePane alpha => Casting alpha -> IDEM (Maybe alpha)
getPane casting = do
    selectedPanes <- getPanes casting
    if null selectedPanes || length selectedPanes > 1
        then return Nothing
        else (return (Just $head selectedPanes))

getPanes ::  CastablePane alpha => Casting alpha -> IDEM ([alpha])
getPanes casting = do
    panes' <- readIDE panes
    return (catMaybes $ map (downCast casting) $ Map.elems panes')

-- | Constructs a unique pane name, which is an index and a string
figureOutPaneName :: Pane alpha => Map String alpha -> String -> Int -> (Int,String)
figureOutPaneName bufs bn ind =
    let ind = foldr (\buf ind ->
                if primPaneName buf == bn
                    then max ind ((getAddedIndex buf) + 1)
                    else ind)
                0 (Map.elems bufs)
    in  if ind == 0
            then (0,bn)
            else (ind,bn ++ "(" ++ show ind ++ ")")

paneFromName :: PaneName -> IDEM IDEPane
paneFromName pn = do
    mbPane <- mbPaneFromName pn
    case mbPane of
        Just p -> return p
        Nothing -> throwIDE $"Cant't find pane from unique name " ++ pn

mbPaneFromName :: PaneName -> IDEM (Maybe IDEPane)
mbPaneFromName pn = do
    panes  <- readIDE panes
    return (Map.lookup pn panes)

-- |
guiPropertiesFromName :: PaneName -> IDEM (PanePath, Connections)
guiPropertiesFromName pn = do
    paneMap <- readIDE paneMap
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

activatePane :: Pane alpha => alpha -> Connections -> IDEAction
activatePane pane conn = do
    deactivatePane
    sb <- getSBActivePane
    lift $statusbarPop sb 1
    lift $statusbarPush sb 1 (paneName pane)
    modifyIDE_ $ \ide -> do
        bringPaneToFront pane
        return (ide{activePane = Just (paneName pane,conn)})

deactivatePane :: IDEAction
deactivatePane = do
    sb <- getSBActivePane
    lift $statusbarPop sb 1
    lift $statusbarPush sb 1 ""
    mbAP    <-  readIDE activePane
    case mbAP of
        Just (_,BufConnections signals signals2 signals3) -> lift $do
            mapM_ signalDisconnect signals
            mapM_ signalDisconnect signals2
            mapM_ signalDisconnect signals3
        Nothing -> return ()
    modifyIDE_ $ \ide -> do
        return (ide{activePane = Nothing})

deactivatePaneIfActive :: Pane alpha => alpha -> IDEAction
deactivatePaneIfActive pane = do
    mbActive <- readIDE activePane
    case mbActive of
        Nothing -> return ()
        Just (n,_) -> if n == paneName pane
                        then deactivatePane
                        else return ()
--
-- | Toggle the tabs of the current notebook
--
viewSwitchTabs :: IDEAction
viewSwitchTabs = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> lift $do
            b <- notebookGetShowTabs nb
            notebookSetShowTabs nb (not b)

--
-- | Sets the tab position in the current notebook
--
viewTabsPos :: PositionType -> IDEAction
viewTabsPos pos = do
    mbNb <- getActiveNotebook
    case mbNb of
        Nothing -> return ()
        Just nb -> lift $notebookSetTabPos nb pos

--
-- | Split the currently active pane in horizontal direction
--
viewSplitHorizontal     :: IDEAction
viewSplitHorizontal     = viewSplit Horizontal

--
-- | Split the currently active pane in vertical direction
--
viewSplitVertical :: IDEAction
viewSplitVertical = viewSplit Vertical

--
-- | The active view can be split in two (horizontal or vertical)
--
viewSplit :: Direction -> IDEAction
viewSplit dir = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> viewSplit' panePath dir

viewSplit' :: PanePath -> Direction -> IDEAction
viewSplit' panePath dir = do
  activeNotebook  <- getNotebook panePath
  mbPD <- lift $ do
      mbParent  <- widgetGetParent activeNotebook
      case mbParent of
          Nothing -> return Nothing
          Just parent -> do
              --trace ("Pane path " ++ show panePath) return ()
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
              containerRemove (castToContainer parent) activeNotebook
              widgetSetName activeNotebook name
              panedPack1 newpane activeNotebook True True
              if not (null panePath)
                  then
                      if (last panePath == TopP || last panePath == LeftP)
                          then  panedPack1 (castToPaned parent) newpane True True
                          else  panedPack2 (castToPaned parent) newpane True True
                  else do
                      boxPackStart (castToBox parent) newpane PackGrow 0
                      boxReorderChild (castToVBox parent) newpane 2
              widgetShowAll newpane
              widgetGrabFocus activeNotebook
              return (Just (paneDir,dir))
  case mbPD of
      Just (paneDir,dir) -> do
          let toPane = panePath ++ [paneDir]
          adjustPane panePath toPane
          adjustLayoutForSplit dir panePath
      Nothing -> return ()

--
-- | Two notebooks can be collapsed to one
--
viewCollapse :: IDEAction
viewCollapse = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewCollapse' panePath

viewCollapse' :: PanePath -> IDEAction
viewCollapse' panePath = do
    layout1           <- readIDE layout
    let newPanePath   = reverse $tail $reverse panePath
    let mbOtherSidePath = otherSide panePath
    case mbOtherSidePath of
        Nothing -> lift $putStrLn "Can't collapse top level"
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
            paneMap         <- readIDE paneMap
            activeNotebook  <- getNotebook panePath
            let paneNamesToMove = map (\(w,(p,_)) -> w)
                                    $filter (\(w,(p,_)) -> p == otherSidePath)
                                        $Map.toList paneMap
            panesToMove <- mapM paneFromName paneNamesToMove
            mapM_ (move panePath) panesToMove
            lift $ do
                mbParent <- widgetGetParent activeNotebook
                case mbParent of
                    Nothing -> error "collapse: no parent"
                    Just parent -> do
                        mbGrandparent <- widgetGetParent parent
                        case mbGrandparent of
                            Nothing -> error "collapse: no grandparent"
                            Just grandparent -> do
                                containerRemove (castToContainer grandparent) parent
                                containerRemove (castToContainer parent) activeNotebook
                                if length panePath > 1
                                    then do
                                        let dir = last newPanePath
                                        if (dir == TopP || dir == LeftP)
                                            then panedPack1 (castToPaned grandparent)
                                                    activeNotebook True True
                                            else panedPack2 (castToPaned grandparent)
                                                    activeNotebook True True
                                        widgetSetName activeNotebook $paneDirectionToWidgetName dir
                                    else do
                                        boxPackStart (castToVBox grandparent) activeNotebook PackGrow 0
                                        boxReorderChild (castToVBox grandparent) activeNotebook 2
                                        widgetSetName activeNotebook "root"
            adjustLayoutForCollapse newPanePath
            adjustPane panePath newPanePath
                --adjustPane otherSidePath newPanePath

--
-- | Moves the given Pane to the given path
--
move ::  PanePath -> IDEPane -> IDEAction
move toPane idew  = do
    paneMap         <-  readIDE paneMap
    let child       =   getTopWidget idew
    (fromPane,cid)  <-  guiPropertiesFromName (paneName idew)
    fromNB          <-  getNotebook fromPane
    toNB            <-  getNotebook toPane
    lift $ do
        mbNum <- notebookPageNum fromNB child
        case mbNum of
            Nothing -> return ()
            Just pn -> do
                mbText <- notebookGetTabLabelText fromNB child
                case mbText of
                    Nothing -> return ()
                    Just text -> do
                        notebookRemovePage fromNB pn
                        notebookInsertOrdered toNB child text
    let paneMap1    =  Map.delete (paneName idew) paneMap
    let newPaneMap  =  Map.insert (paneName idew) (toPane,cid) paneMap1
    modifyIDE_ (\ide -> return (ide{paneMap = newPaneMap}))

--
-- | Moves the activePane in the given direction, if possible
-- | If their are many possibilities choose the leftmost and topmost
--
viewMove :: PaneDirection -> IDEAction
viewMove direction = do
    mbPane <- readIDE activePane
    case mbPane of
        Nothing -> do
            lift $putStrLn "no active pane"
            return ()
        Just (paneName,_) -> do
            pane <- paneFromName paneName
            mbPanePath <- getActivePanePath
            case mbPanePath of
                Nothing -> do
                    lift $putStrLn "no active pane path"
                    return ()
                Just panePath -> do
                  layout <- readIDE layout
                  case findMoveTarget panePath layout direction of
                      Nothing -> do
                        lift $putStrLn "no target found"
                        return ()
                      Just moveTo -> trace ("move target: " ++ show moveTo)
                                        move moveTo pane

--
-- | Find the target for a move
--
findMoveTarget :: PanePath -> PaneLayout -> PaneDirection -> Maybe PanePath
findMoveTarget panePath layout direction=
    let reversedPath    = reverse panePath
        oppositeDir     = otherDirection direction
        cutPath         = dropWhile (\d -> d /= oppositeDir) reversedPath
    in if null cutPath
        then Nothing
        else let basePath = reverse (direction : tail cutPath)
                 layoutP  = layoutFromPath basePath layout
             in  Just $basePath ++ findAppropriate layoutP oppositeDir

findAppropriate :: PaneLayout -> PaneDirection -> PanePath
findAppropriate  (TerminalP _ _) _ =   []
findAppropriate  (HorizontalP t b _) LeftP     =   TopP    :  findAppropriate t LeftP
findAppropriate  (HorizontalP t b _) RightP    =   TopP    :  findAppropriate t RightP
findAppropriate  (HorizontalP t b _) BottomP   =   BottomP :  findAppropriate b BottomP
findAppropriate  (HorizontalP t b _) TopP      =   TopP    :  findAppropriate b TopP
findAppropriate  (VerticalP l r _) LeftP       =   LeftP   :  findAppropriate l LeftP
findAppropriate  (VerticalP l r _) RightP      =   RightP  :  findAppropriate r RightP
findAppropriate  (VerticalP l r _) BottomP     =   LeftP   :  findAppropriate l BottomP
findAppropriate  (VerticalP l r _) TopP        =   LeftP   :  findAppropriate l TopP

--
-- | Bring the pane to the front position in its notebook
--
bringPaneToFront :: Pane alpha => alpha -> IO ()
bringPaneToFront pane = do
    let tv = getTopWidget pane
    mbParent <- widgetGetParent tv
    case mbParent of
        Just parent -> do
            let nb = castToNotebook parent
            n <- notebookGetNPages nb
            r <- filterM (\i -> do
                        mbp <-  notebookGetNthPage nb i
                        case mbp of
                            Nothing -> return False
                            Just p -> do
                                mbs <- notebookGetTabLabelText nb p
                                case mbs of
                                    Nothing -> return False
                                    Just s -> return (s == paneName pane))
                                    [0..(n-1)]
            case r of
                [i] -> notebookSetCurrentPage nb i
                otherwise -> return ()
        Nothing -> return ()

--
-- | Get a concrete panePath from a standard path.
-- Standard path is for example left top.
--
getStandardPanePath :: StandardPath -> PaneLayout -> PanePath
getStandardPanePath sp pl = reverse $ getStandard' sp pl []
    where
    getStandard' _ (TerminalP _ _) p                =   p
    getStandard' LeftTop (VerticalP l r _) p        =   getStandard' LeftTop l (LeftP:p)
    getStandard' LeftBottom (VerticalP l r _) p     =   getStandard' LeftBottom l (LeftP:p)
    getStandard' RightTop (VerticalP l r _) p       =   getStandard' RightTop r (RightP:p)
    getStandard' RightBottom (VerticalP l r _) p    =   getStandard' RightBottom r (RightP:p)
    getStandard' LeftTop (HorizontalP t b _) p      =   getStandard' LeftTop t (TopP:p)
    getStandard' LeftBottom (HorizontalP t b _) p   =   getStandard' LeftBottom b (BottomP:p)
    getStandard' RightTop (HorizontalP t b _) p     =   getStandard' RightTop t (TopP:p)
    getStandard' RightBottom (HorizontalP t b _) p  =   getStandard' RightBottom b (BottomP:p)

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
                        ae = otherDirection $head rp
                    in Just (reverse $ae : tail rp)

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
layoutFromPath [] l                             = l
layoutFromPath (TopP:r) (HorizontalP t _ _)     = layoutFromPath r t
layoutFromPath (BottomP:r) (HorizontalP _ b _)  = layoutFromPath r b
layoutFromPath (LeftP:r) (VerticalP l _ _)      = layoutFromPath r l
layoutFromPath (RightP:r) (VerticalP _ ri _)    = layoutFromPath r ri
layoutFromPath pp l                             = error
    $"inconsistent layout " ++ show pp ++ " " ++ show l


getNotebookOrPaned :: PanePath -> (Widget -> beta) -> IDEM beta
getNotebookOrPaned p cf = (widgetGet $["topBox","root"] ++ map paneDirectionToWidgetName p) cf

--
-- | Get the notebook widget for the given pane path
--
getNotebook :: PanePath -> IDEM Notebook
getNotebook p = getNotebookOrPaned p castToNotebook

--
-- | Get the (gtk) Paned widget for a given path
--
getPaned :: PanePath -> IDEM Paned
getPaned p = getNotebookOrPaned p castToPaned

--
-- | Get the path to the active pane
--
getActivePanePath :: IDEM (Maybe PanePath)
getActivePanePath = do
    mbPane   <- readIDE activePane
    case mbPane of
        Nothing -> return Nothing
        Just (paneName,_) -> do
            (pp,_)  <- guiPropertiesFromName paneName
            return (Just (pp))

getActivePanePathOrStandard :: StandardPath -> IDEM (PanePath)
getActivePanePathOrStandard sp = do
    mbApp <- getActivePanePath
    case mbApp of
        Just app -> return app
        Nothing -> do
            layout <- readIDE layout
            return (getStandardPanePath sp layout)


--
-- | Get the active notebook
--
getActiveNotebook :: IDEM (Maybe Notebook)
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

--
-- | Changes a pane path in the pane map
--
adjustPane :: PanePath -> PanePath -> IDEAction
adjustPane fromPane toPane  = do
    trace ("adjust pane from: " ++ show fromPane ++ " to: " ++ show toPane) return ()
    paneMap     <- readIDE paneMap
    let newMap  = Map.map (\(pp,other) -> do
        if pp == fromPane
            then (toPane,other)
            else (pp,other)) paneMap
    modifyIDE_ $ \ide -> return (ide{paneMap = newMap})

--
-- | Changes the layout for a split
--
adjustLayoutForSplit            :: Direction -> PanePath -> IDEAction
adjustLayoutForSplit  dir path  = do
    layout          <- readIDE layout
    let newTerm     = case dir of
                        Horizontal -> HorizontalP (TerminalP Nothing 0) (TerminalP Nothing 0) 0
                        Vertical   -> VerticalP (TerminalP Nothing 0) (TerminalP Nothing 0) 0
    let newLayout   = adjust path layout newTerm
    modifyIDE_ $ \ide -> return (ide{layout = newLayout})

--
-- | Changes the layout for a collapse (HorizontalP TerminalP (VerticalP (HorizontalP TerminalP TerminalP) TerminalP))

--
adjustLayoutForCollapse :: PanePath -> IDEAction
adjustLayoutForCollapse path = do
    layout          <- readIDE layout
    let newLayout   = adjust path layout (TerminalP Nothing 0)
    modifyIDE_ $ \ide -> return (ide{layout = newLayout})

getSubpath :: PanePath -> PaneLayout -> Maybe PanePath
getSubpath path layout =
    case layoutFromPath path layout of
        TerminalP _ _       -> Nothing
        HorizontalP _ _ _   -> Just (path ++ [TopP])
        VerticalP _ _ _     -> Just (path ++ [LeftP])

--
-- | Changes the layout by replacing element at pane path with replace
--
adjust :: PanePath -> PaneLayout -> PaneLayout -> PaneLayout
adjust pp layout replace    = adjust' pp layout
    where
    adjust' [] _                                = replace
    adjust' (TopP:r)  (HorizontalP tp bp _)     = HorizontalP (adjust' r tp) bp 0
    adjust' (BottomP:r)  (HorizontalP tp bp _)  = HorizontalP tp (adjust' r bp) 0
    adjust' (LeftP:r)  (VerticalP lp rp _)      = VerticalP (adjust' r lp) rp 0
    adjust' (RightP:r)  (VerticalP lp rp _)     = VerticalP lp (adjust' r rp) 0
    adjust' p l = error $"inconsistent layout " ++ show p ++ " " ++ show l

--
-- | Get the widget from a list of strings
--
widgetFromPath :: Widget -> [String] -> IO (Widget)
widgetFromPath w [] = return w
widgetFromPath w (h:t) = do
    children    <- containerGetChildren (castToContainer w)
    names       <- mapM widgetGetName children
    let mbiInd  =  findIndex (== h) names
    case mbiInd of
        Nothing     -> error $"Cant't find widget path " ++ show (h:t)
        Just ind    -> widgetFromPath (children !! ind) t


widgetGet :: [String] -> (Widget -> b) -> IDEM (b)
widgetGet strL cf = do
    w <- readIDE window
    r <- lift $widgetFromPath (castToWidget w) strL
    return (cf r)

widgetGetRel :: Widget -> [String] -> (Widget -> b) -> IO (b)
widgetGetRel w sl cf = do
    r <- widgetFromPath w sl
    return (cf r)

getUIAction :: String -> (Action -> a) -> IDEM(a)
getUIAction str f = do
    uiManager <- readIDE uiManager
    lift $ do
        findAction <- uiManagerGetAction uiManager str
        case findAction of
            Just act -> return (f act)
            Nothing  -> error $"getUIAction can't find action " ++ str

--get widget elements

getCandyState :: IDEM (Bool)
getCandyState = do
    ui <- getUIAction "ui/menubar/_Edit/Source Candy" castToToggleAction
    lift $toggleActionGetActive ui

setCandyState :: Bool -> IDEAction
setCandyState b = do
    ui <- getUIAction "ui/menubar/_Edit/Source Candy" castToToggleAction
    lift $toggleActionSetActive ui b
--

getSBSpecialKeys :: IDEM (Statusbar)
getSBSpecialKeys = widgetGet ["topBox","statusBox","statusBarSpecialKeys"] castToStatusbar

getSBActivePane :: IDEM (Statusbar)
getSBActivePane = widgetGet ["topBox","statusBox","statusBarActivePane"] castToStatusbar

getSBActivePackage :: IDEM (Statusbar)
getSBActivePackage = widgetGet ["topBox","statusBox","statusBarActiveProject"] castToStatusbar

getSBErrors :: IDEM (Statusbar)
getSBErrors = widgetGet ["topBox","statusBox","statusBarErrors"] castToStatusbar

getStatusbarIO :: IDEM (Statusbar)
getStatusbarIO =  widgetGet ["topBox","statusBox","statusBarInsertOverwrite"] castToStatusbar

getStatusbarLC :: IDEM (Statusbar)
getStatusbarLC = widgetGet ["topBox","statusBox","statusBarLineColumn"] castToStatusbar


