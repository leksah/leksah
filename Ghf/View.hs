module Ghf.View (
    viewMove
,   viewMoveUp
,   viewSplitHorizontal
,   viewSplitVertical
,   viewCollapse
,   figureOutPaneName
,   getNotebook
,   getActivePanePath
,   maybeActiveBuf
,   realBufferName
,   guessNewActiveBuffer

,   getFindEntry
,   getFindBar
,   getStatusbarIO
,   getStatusbarLC
,   getCaseSensitive
,   getGotoLineSpin
,   getFindAction
,   getWrapAround
,   getEntireWord
) where

import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Multiline.TextView
import Graphics.UI.Gtk.Glade
import System.Glib.GObject
import Control.Monad.Reader
import Data.IORef
import System.FilePath
import System.Directory
import Data.Maybe ( fromMaybe, isJust, fromJust )
import Text.Printf
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Data.List(findIndex)

import Ghf.Core


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
-- | The main view can be split in to (horizontal or vertical)
--
viewSplit :: Direction -> GhfAction
viewSplit dir = do
    panePath        <- getActivePanePath
    activeNotebook  <- getNotebook panePath
    mbPD <- lift $ do
        mbParent  <- widgetGetParent activeNotebook
        case mbParent of
            Nothing -> return Nothing
            Just parent -> do
                newpane <- case dir of
                                Horizontal  ->  do  h <- hPanedNew
                                                    return (castToPaned h)
                                Vertical ->     do  v <- hPanedNew
                                                    return (castToPaned v)
                let (name,nbname,altnbname,paneDir,dir) = case dir of
                            Horizontal  ->  ("hpane","nb-top","nb-bottom",TopP,Horizontal)
                            Vertical    ->  ("vpane","nb-left","nb-right",LeftP,Vertical)
                widgetSetName newpane name
                nb <- notebookNew
                widgetSetName nb nbname
                panedPack1 newpane nb True True
                containerRemove (castToContainer parent) activeNotebook
                widgetSetName activeNotebook altnbname
                panedPack2 newpane activeNotebook True True
                if not (null panePath)
                    then panedPack2 newpane parent True True
                    else do
                        boxPackStart (castToVBox parent) newpane PackGrow 0
                        boxReorderChild (castToVBox parent) newpane 2
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
    paneMap <- readGhf paneMap
    panePath        <- getActivePanePath
    activeNotebook <- getNotebook panePath
    layout <- readGhf layout
    let otherSidePath = otherSide panePath
    let newPanePath = reverse $tail $reverse panePath
    b <- if isJust otherSidePath
            && (layoutFromPath panePath layout /= TerminalP
            || layoutFromPath (fromJust otherSidePath) layout /= TerminalP)
            then return False
            else do
                adjustPane panePath newPanePath
                adjustPane (fromJust otherSidePath) newPanePath
                lift $ do
                  mbParent <- widgetGetParent activeNotebook
                  case mbParent of
                      Nothing -> return False
                      Just parent -> do
                          mbGrandparent <- widgetGetParent parent
                          case mbGrandparent of
                              Nothing -> return False
                              Just grandparent -> do
                                  containerRemove (castToContainer grandparent) parent
                                  if length panePath > 1
                                    then do
                                        panedPack2 (castToPaned grandparent) activeNotebook True True
                                        return True
                                    else do
                                        boxPackStart (castToVBox grandparent) activeNotebook PackGrow 0
                                        boxReorderChild (castToVBox grandparent) activeNotebook 2 {--###--}
                                        return True
    if b
        then let windowsToMove =
                    map (\(w,(p,_)) -> w)
                        $ filter (\(w,(p,_)) -> p == (fromJust otherSidePath))
                            $ Map.toList paneMap
             in do
                adjustLayoutForCollapse newPanePath
                mapM_ (move newPanePath) windowsToMove
        else return ()

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
-- | Moves the activePane to the other side, if their are many solutions
--   choose the rightmost and topmost
--
viewMove :: GhfAction
viewMove = do
    paneMap <- readGhf paneMap
    (pane,_) <- readGhf activePane
    panePath <- getActivePanePath
    layout <- readGhf layout
    case moveTarget panePath layout of
        Nothing -> return ()
        Just moveTo -> move moveTo pane

--
-- | Find the target Path, choose the rightmost and topmost
--
moveTarget :: PanePath -> PaneLayout -> Maybe PanePath
moveTarget panePath layout =
    let mbTarget = otherSide panePath in
    case mbTarget of
        Nothing -> Nothing
        Just target ->
            Just (target ++ chooseRightmostTopmost (layoutFromPath panePath layout))
    where
    chooseRightmostTopmost          ::  PaneLayout -> PanePath
    chooseRightmostTopmost  TerminalP  = []
    chooseRightmostTopmost  (HorizontalP t _)   =   TopP :  chooseRightmostTopmost t
    chooseRightmostTopmost  (VerticalP _ r)     =   RightP :  chooseRightmostTopmost r

--
-- | Moves the activePaneone level up
--
viewMoveUp :: GhfAction
viewMoveUp = return ()


--
-- | Get another pane path which points to the other side at the same level
--
otherSide :: PanePath -> Maybe PanePath
otherSide []    =   Nothing
otherSide p     =   let rp = reverse p
                        ae = case head rp of
                                LeftP -> RightP
                                RightP -> LeftP
                                TopP -> BottomP
                                BottomP -> TopP
                    in Just (reverse $ae : tail rp)

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
-- | Get the concrete notebook widget from a logical path
--
getNotebook :: PanePath -> GhfM Notebook
getNotebook [] = widgetGet ["topBox","nb"] castToNotebook
getNotebook p = widgetGet (["topBox"] ++ (concatMap paneDirectionToWidgetName p))
                    castToNotebook
    where
    paneDirectionToWidgetName           :: PaneDirection -> [String]
    paneDirectionToWidgetName TopP      = ["hpane","nb-top"]
    paneDirectionToWidgetName BottomP   = ["hpane","nb-bottom"]
    paneDirectionToWidgetName LeftP     = ["vpane","nb-left"]
    paneDirectionToWidgetName RightP    = ["vpane","nb-right"]

--
-- | Changes a pane path in the pane map
--
adjustPane                  :: PanePath -> PanePath -> GhfAction
adjustPane fromPane toPane  = do
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
-- | Changes the layout for a collapse
--
adjustLayoutForCollapse :: PanePath -> GhfAction
adjustLayoutForCollapse path = do
    layout          <- readGhf layout
    let newLayout   = adjust path layout TerminalP  
    modifyGhf_ $ \ghf -> return (ghf{layout = newLayout})

--
-- | Changes the layout by replacing element at pane path with replace
--
adjust                      :: PanePath -> PaneLayout -> PaneLayout -> PaneLayout
adjust pp layout replace    = adjust' pp layout
    where
    adjust' [] _                                = replace
    adjust' (TopP:r)  (HorizontalP tp bp)       = HorizontalP (adjust' r tp) bp
    adjust' (BottomP:r)  (HorizontalP tp bp)    = HorizontalP bp (adjust' r bp)
    adjust' (LeftP:r)  (VerticalP lp rp)        = VerticalP (adjust' r lp) rp
    adjust' (RightP:r)  (VerticalP lp rp)       = VerticalP lp (adjust' r rp)
    adjust' p l = error $"inconsistent layout " ++ show p ++ " " ++ show l

getActivePanePath :: GhfM PanePath
getActivePanePath = do
    (pane,_) <- readGhf activePane
    paneMap  <- readGhf paneMap
    return (fst $paneMap ! pane)

maybeActiveBuf :: GhfM (Maybe (GhfBuffer,Connections))
maybeActiveBuf = do
    (pane,signals) <- readGhf activePane
    case pane of
        PaneBuf buf -> return (Just (buf,signals))
        otherwise   -> return Nothing
    

getTopWidget :: GhfPane -> Widget
getTopWidget (PaneBuf buf) = castToWidget(scrolledWindow buf)

getBufferName :: GhfPane -> String
getBufferName (PaneBuf buf) = bufferName buf

getAddedIndex :: GhfPane -> Int
getAddedIndex (PaneBuf buf) = addedIndex buf

figureOutPaneName :: Map String GhfPane -> String -> Int -> (Int,String)
figureOutPaneName bufs bn ind =
    let ind = foldr (\buf ind ->
                if getBufferName buf == bn
                    then max ind ((getAddedIndex buf) + 1)
                    else ind)
                0 (Map.elems bufs)
    in  if ind == 0
            then (0,bn)
            else (ind,bn ++ "<" ++ show ind ++ ">")

realBufferName :: GhfBuffer -> String
realBufferName buf =
    if addedIndex buf == 0
        then bufferName buf
        else bufferName buf ++ "(" ++ show (addedIndex buf) ++ ")"

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
        return (f (fromJust findAction))

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
                              Just b  ->  (b,BufConnections [][])
                              Nothing ->  if Map.null panes
                                              then error $"guessNewActiveBuffer ->" ++
                                                            "panes are empty"
                                              else (head (Map.elems panes),BufConnections [][])
        in return (ghf{activePane = newActiveBuf})

--actions
getFindAction = getUIAction "ui/menubar/_Edit/_Find" castToToggleAction


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