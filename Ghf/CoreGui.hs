module Ghf.CoreGui (
    figureOutBufferName
,   realBufferName
,   getNotebook
,   getActiveBufferPNotebookOrDefault
,   getActiveBufferPNotebook
,   guessNewActiveBuffer
,   makeBufferActive

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
import Graphics.UI.Gtk
import Control.Monad.Reader
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Ghf.Core


figureOutBufferName :: Map String GhfBuffer -> String -> Int -> (Int,String)
figureOutBufferName bufs bn ind =
    let ind = foldr (\buf ind -> if bufferName buf == bn
                    then max ind (addedIndex buf + 1)
                    else ind) 0 (Map.elems bufs) in
    if ind == 0 then (0,bn) else (ind,bn ++ "(" ++ show ind ++ ")")

realBufferName :: GhfBuffer -> String
realBufferName buf =
    if addedIndex buf == 0
        then bufferName buf
        else bufferName buf ++ "(" ++ show (addedIndex buf) ++ ")"

widgetFromPath :: Widget -> [String] -> IO (Widget)
widgetFromPath w [] = return w
widgetFromPath w (h:t) = do
    children <- containerGetChildren (castToContainer w)
    names <- mapM widgetGetName children
    let mbiInd = findIndex (== h) names
    case mbiInd of
        Nothing -> error $"Cant't find widget path " ++ show (h:t)
        Just ind -> widgetFromPath (children !! ind) t

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

getActiveBufferPNotebook :: GhfM (Maybe (Pane,Notebook))
getActiveBufferPNotebook = do
    mbBuf <- readGhf mbActiveBuf
    case mbBuf of
        Nothing -> return Nothing
        Just buf -> do
            paneMap <- readGhf paneMap
            let (pane,_) = paneMap Map.! WindowBuf buf
            nb <- getNotebook pane
            return (Just (pane,nb))

getActiveBufferPNotebookOrDefault :: GhfM (Pane,Notebook)
getActiveBufferPNotebookOrDefault = do
    mbNotebook <- getActiveBufferPNotebook
    case mbNotebook of
        Nothing -> do
            nb <- getNotebook RightTop
            return (RightTop,nb)
        Just p  -> return p

guessNewActiveBuffer :: Notebook -> GhfAction
guessNewActiveBuffer nb = do
    bufs <- readGhf buffers
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
                            Just key -> return (Map.lookup key bufs)
    modifyGhf_ $ \ghf -> return (ghf{mbActiveBuf =
        case mbBuf of
            Just _  -> mbBuf
            Nothing -> if Map.null bufs
                            then Nothing
                            else (Just (head (Map.elems bufs)))})

makeBufferActive :: GhfBuffer -> GhfAction
makeBufferActive buf = modifyGhf_ $ \ghf -> return (ghf{mbActiveBuf = Just buf})

getNotebook :: Pane -> GhfM (Notebook)
getNotebook RightTop = widgetGet ["topBox","paneLeftRight","paneRight", "notebook0" ] castToNotebook
getNotebook RightBottom = widgetGet ["topBox","paneLeftRight","paneRight", "notebook1" ] castToNotebook 
getNotebook LeftBottom = widgetGet ["topBox","paneLeftRight","paneLeft", "notebook2" ]  castToNotebook
getNotebook LeftTop = widgetGet ["topBox","paneLeftRight","paneLeft", "notebook3" ]  castToNotebook


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

--actions
getFindAction = getUIAction "ui/menubar/_Edit/_Find" castToToggleAction