module Ghf.GUI.ViewFrame (
    viewMove
,   viewSplitHorizontal
,   viewSplitVertical
,   viewCollapse
,   figureOutPaneName
,   getNotebook
,   getActiveOrTopNotebook
,   getActivePanePath
,   getActivePanePathOrTop
,   maybeActiveBuf
,   guessNewActiveBuffer
,   newNotebook
,   viewTabsPos
,   viewSwitchTabs
,   widgetFromPath

,   getFindEntry
,   getFindBar
,   getStatusbarIO
,   getStatusbarLC
,   getCaseSensitive
,   getGotoLineSpin
--,   getFindAction
,   getWrapAround
,   getEntireWord
,   getSpecialKeys

,   getCandyState
,   setCandyState
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
--import Graphics.UI.Gtk.Glade
import System.Glib.GObject
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import Data.Maybe ( fromMaybe, isJust)
import Text.Printf
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Data.List(findIndex)
import Debug.Trace

import Ghf.Core

newNotebook :: IO Notebook
newNotebook = do
    nb <- notebookNew
    notebookSetTabPos nb PosTop
    notebookSetShowTabs nb True
    notebookSetScrollable nb True
    notebookSetPopup nb True
    return nb 

viewSwitchTabs :: GhfAction
viewSwitchTabs = do
    nb <- getActiveOrTopNotebook
    lift $do
        b <- notebookGetShowTabs nb
        notebookSetShowTabs nb (not b)

viewTabsPos :: PositionType -> GhfAction
viewTabsPos pos = do
    nb <- getActiveOrTopNotebook
    lift $notebookSetTabPos nb pos
        
--
-- | Split the currently active pane in horizontal direction
--
viewSplitHorizontal     :: GhfAction
viewSplitHorizontal     = viewSplit Horizontal

--
-- | Split the currently active pane in vertical direction
--
viewSplitVertical :: GhfAction
viewSplitVertical = viewSplit Vertical

--
-- | The main view can be split in two (horizontal or vertical)
--
viewSplit :: Direction -> GhfAction
viewSplit dir = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do 
          activeNotebook  <- getNotebook panePath
          mbPD <- lift $ do
              mbParent  <- widgetGetParent activeNotebook
              case mbParent of
                  Nothing -> return Nothing
                  Just parent -> do
                      --trace ("Pane path " ++ show panePath) return ()
                      newpane <- case dir of
                                      Horizontal  ->  do  h <- vPanedNew
                                                          return (castToPaned h)
                                      Vertical ->     do  v <- hPanedNew
                                                          return (castToPaned v)
                      let (name,altname,paneDir) = case dir of
                                  Horizontal  ->  ("top","bottom",TopP)
                                  Vertical    ->  ("left","right",LeftP)
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
viewCollapse :: GhfAction
viewCollapse = do
    mbPanePath        <- getActivePanePath
    case mbPanePath of
        Nothing -> return ()
        Just panePath -> do
            viewCollapse' panePath

viewCollapse' :: PanePath -> GhfAction
viewCollapse' panePath = do
    layout1           <- readGhf layout
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
            paneMap         <- readGhf paneMap
            activeNotebook  <- getNotebook panePath
            let windowsToMove = map (\(w,(p,_)) -> w)
                                    $filter (\(w,(p,_)) -> p == otherSidePath)
                                        $Map.toList paneMap
            mapM_ (move panePath) windowsToMove
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
move ::  PanePath -> GhfPane -> GhfAction
move toPane ghfw  = do
    paneMap <- readGhf paneMap
    let child = getTopWidget ghfw
    let (fromPane,cid) =  paneMap ! ghfw
    fromNB  <- getNotebook fromPane
    toNB    <- getNotebook toPane
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
                        pn2 <- notebookPrependPage toNB child text
                        notebookSetCurrentPage toNB pn2
    let paneMap1    =  Map.delete ghfw paneMap
    let newPaneMap  =  Map.insert ghfw (toPane,cid) paneMap1
    modifyGhf_ (\ghf -> return (ghf{paneMap = newPaneMap}))

--
-- | Moves the activePane in the given direction, if possible 
--   If their are many possibilities choose the leftmost and topmost
--
viewMove :: PaneDirection -> GhfAction
viewMove direction = do
    paneMap <- readGhf paneMap
    mbPane <- readGhf activePane
    case mbPane of
        Nothing -> return ()
        Just (pane,_) -> do
            mbPanePath <- getActivePanePath
            case mbPanePath of
                Nothing -> return ()
                Just panePath -> do 
                  layout <- readGhf layout
                  case findMoveTarget panePath layout direction of
                      Nothing -> return ()
                      Just moveTo -> trace ("move target: " ++ show moveTo)
                                        move moveTo pane

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
findAppropriate  TerminalP _ =   []
findAppropriate  (HorizontalP t b) LeftP     =   TopP    :  findAppropriate t LeftP
findAppropriate  (HorizontalP t b) RightP    =   TopP    :  findAppropriate t RightP
findAppropriate  (HorizontalP t b) BottomP   =   BottomP :  findAppropriate b BottomP
findAppropriate  (HorizontalP t b) TopP      =   TopP    :  findAppropriate b TopP
findAppropriate  (VerticalP l r) LeftP       =   LeftP   :  findAppropriate l LeftP
findAppropriate  (VerticalP l r) RightP      =   RightP  :  findAppropriate r RightP
findAppropriate  (VerticalP l r) BottomP     =   LeftP   :  findAppropriate l BottomP
findAppropriate  (VerticalP l r) TopP        =   LeftP   :  findAppropriate l TopP

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
layoutFromPath (TopP:r) (HorizontalP t _)       = layoutFromPath r t
layoutFromPath (BottomP:r) (HorizontalP _ b)    = layoutFromPath r b
layoutFromPath (LeftP:r) (VerticalP l _)        = layoutFromPath r l
layoutFromPath (RightP:r) (VerticalP _ ri)      = layoutFromPath r ri
layoutFromPath pp l                             = error
    $"inconsistent layout " ++ show pp ++ " " ++ show l

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

--
-- | Get the concrete notebook widget for the active pane
--
getNotebook :: PanePath -> GhfM Notebook
getNotebook p = (widgetGet $["topBox","root"] ++ map paneDirectionToWidgetName p) castToNotebook

--
-- | Get the concrete notebook widget for the active pane
--   If their is no active pane assume that their is just one pane
-- (HorizontalP TerminalP (VerticalP (HorizontalP TerminalP TerminalP) TerminalP))

getActiveOrTopNotebook :: GhfM Notebook
getActiveOrTopNotebook = do
    mbPanePath <- getActivePanePath
    case mbPanePath of
        Just panePath -> getNotebook panePath
        Nothing -> do
            layout <- readGhf layout
            if layout == TerminalP     
                then getNotebook []
                else error "getActiveOrTopNotebook: No active notebook and not collapsed"

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
adjustPane :: PanePath -> PanePath -> GhfAction
adjustPane fromPane toPane  = do
    trace ("adjust pane from: " ++ show fromPane ++ " to: " ++ show toPane) return () 
    paneMap     <- readGhf paneMap
    let newMap  = Map.map (\(pane,other) -> do
        if pane == fromPane
            then (toPane,other)
            else (pane,other)) paneMap
    modifyGhf_ $ \ghf -> return (ghf{paneMap = newMap})

--
-- | Changes the layout for a split
--
adjustLayoutForSplit            :: Direction -> PanePath -> GhfAction
adjustLayoutForSplit  dir path  = do
    layout          <- readGhf layout
    let newTerm     = case dir of
                        Horizontal -> HorizontalP TerminalP TerminalP
                        Vertical -> VerticalP TerminalP TerminalP
    let newLayout   = adjust path layout newTerm
    modifyGhf_ $ \ghf -> return (ghf{layout = newLayout})

--
-- | Changes the layout for a collapse (HorizontalP TerminalP (VerticalP (HorizontalP TerminalP TerminalP) TerminalP))

--
adjustLayoutForCollapse :: PanePath -> GhfAction
adjustLayoutForCollapse path = do
    layout          <- readGhf layout
    let newLayout   = adjust path layout TerminalP  
    modifyGhf_ $ \ghf -> return (ghf{layout = newLayout})

getSubpath :: PanePath -> PaneLayout -> Maybe PanePath
getSubpath path layout = 
    case layoutFromPath path layout of
        TerminalP -> Nothing
        HorizontalP _ _ -> Just (path ++ [TopP])
        VerticalP _ _ -> Just (path ++ [LeftP])

--
-- | Changes the layout by replacing element at pane path with replace
--
adjust :: PanePath -> PaneLayout -> PaneLayout -> PaneLayout
adjust pp layout replace    = adjust' pp layout
    where
    adjust' [] _                                = replace
    adjust' (TopP:r)  (HorizontalP tp bp)       = HorizontalP (adjust' r tp) bp
    adjust' (BottomP:r)  (HorizontalP tp bp)    = HorizontalP tp (adjust' r bp)
    adjust' (LeftP:r)  (VerticalP lp rp)        = VerticalP (adjust' r lp) rp
    adjust' (RightP:r)  (VerticalP lp rp)       = VerticalP lp (adjust' r rp)
    adjust' p l = error $"inconsistent layout " ++ show p ++ " " ++ show l

getActivePanePath :: GhfM (Maybe PanePath)
getActivePanePath = do
    mbPane   <- readGhf activePane
    case mbPane of
        Nothing -> return Nothing
        Just (pane,_) -> do
            paneMap  <- readGhf paneMap
            return (Just (fst $paneMap ! pane))

getActivePanePathOrTop :: GhfM PanePath
getActivePanePathOrTop = do
    mbPP <- getActivePanePath
    case mbPP of
        Just pp -> return pp
        Nothing -> do            
            layout <- readGhf layout
            if layout == TerminalP     
                then return []
                else error "getActivePanePathOrTop: No active notebook and not collapsed"

maybeActiveBuf :: GhfM (Maybe (GhfBuffer,Connections))
maybeActiveBuf = do
    mbPane   <- readGhf activePane
    case mbPane of
        Nothing -> return Nothing
        Just (pane,signals) -> do
            case pane of
                PaneBuf buf -> return (Just (buf,signals))
              --  otherwise   -> return Nothing
    



figureOutPaneName :: Map String GhfPane -> String -> Int -> (Int,String)
figureOutPaneName bufs bn ind =
    let ind = foldr (\buf ind ->
                if getBufferName buf == bn
                    then max ind ((getAddedIndex buf) + 1)
                    else ind)
                0 (Map.elems bufs)
    in  if ind == 0
            then (0,bn)
            else (ind,bn ++ "(" ++ show ind ++ ")")


widgetGet :: [String] -> (Widget -> b) -> GhfM (b)
widgetGet strL cf = do
    w <- readGhf window
    r <- lift $widgetFromPath (castToWidget w) strL
    return (cf r)

widgetGetRel :: Widget -> [String] -> (Widget -> b) -> IO (b)
widgetGetRel w sl cf = do
    r <- widgetFromPath w sl
    return (cf r) 

getUIAction :: String -> (Action -> a) -> GhfM(a)    
getUIAction str f = do
    uiManager <- readGhf uiManager
    lift $ do
        findAction <- uiManagerGetAction uiManager str
        case findAction of
            Just act -> return (f act)
            Nothing  -> error $"getUIAction can't find action " ++ str

guessNewActiveBuffer :: Notebook -> GhfAction
guessNewActiveBuffer nb = do
    panes <- readGhf panes
    mbBuf  <- lift $do
        mbI <- notebookGetCurrentPage nb
        case mbI of
            -1 -> return Nothing
            i  -> do
                mbPage <- notebookGetNthPage nb i
                case mbPage of
                    Nothing -> return Nothing
                    Just page -> do
                        mbKey <- notebookGetMenuLabelText nb page
                        case mbKey of
                            Nothing -> return Nothing
                            Just key -> return (Map.lookup key panes)
    modifyGhf_ $ \ghf -> 
        let newActiveBuf =  case mbBuf of
                              Just b  ->  Just (b,BufConnections [][])
                              Nothing ->  if Map.null panes
                                              then Nothing
                                              else Just (head (Map.elems panes),
                                                            BufConnections [][])
        in return (ghf{activePane = newActiveBuf})

--get widget elements

--getFindState = toggleActionGetActive
--    $getUIAction "ui/menubar/_Edit/_Find" castToToggleAction

getCandyState :: GhfM (Bool)
getCandyState = do 
    ui <- getUIAction "ui/menubar/_Edit/Source Candy" castToToggleAction
    lift $toggleActionGetActive ui 

setCandyState :: Bool -> GhfAction
setCandyState b = do
    ui <- getUIAction "ui/menubar/_Edit/Source Candy" castToToggleAction
    lift $toggleActionSetActive ui b
--

getFindEntry :: GhfM (Entry)
getFindEntry =  widgetGet ["topBox","statusBox","searchBox","searchEntry"] castToEntry

getFindBar :: GhfM (HBox)
getFindBar =  widgetGet ["topBox","statusBox","searchBox"] castToHBox 

getStatusbarIO :: GhfM (Statusbar)
getStatusbarIO =  widgetGet ["topBox","statusBox","statusBarInsertOverwrite"] castToStatusbar 

getStatusbarLC :: GhfM (Statusbar)
getStatusbarLC = widgetGet ["topBox","statusBox","statusBarLineColumn"] castToStatusbar

getCaseSensitive :: GhfM (ToggleButton)
getCaseSensitive = widgetGet ["topBox","statusBox","searchBox","caseSensitiveButton"] 
                        castToToggleButton

getWrapAround :: GhfM (ToggleButton)
getWrapAround = widgetGet ["topBox","statusBox","searchBox","wrapAroundButton"] 
                        castToToggleButton

getEntireWord :: GhfM (ToggleButton)
getEntireWord = widgetGet ["topBox","statusBox","searchBox","entireWordButton"] 
                        castToToggleButton

getGotoLineSpin :: GhfM (SpinButton)
getGotoLineSpin = widgetGet ["topBox","statusBox","gotoLineEntry"] castToSpinButton

getSpecialKeys :: GhfM (Statusbar)
getSpecialKeys = widgetGet ["topBox","statusBox","statusBarSpecialKeys"] castToStatusbar
