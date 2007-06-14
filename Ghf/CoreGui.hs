module Ghf.CoreGui (
    figureOutBufferName
,   realBufferName
,   getPane
,   getActivePane

,   getNotebook
,   getFindEntry
,   getFindBar
,   getStatusbarIO
,   getStatusbarLC
,   getCaseSensitive
,   getGotoLineSpin
,   getFindAction
,   getWrapAround
,   getEntireWord

,   PaneNum
) where
import Graphics.UI.Gtk
import Control.Monad.Reader
import Data.List
import Ghf.Core
import Data.Maybe

-- | 1: right top 2: right botton 3: left bottom 4: left top
type PaneNum = Int

figureOutBufferName :: [GhfBuffer] -> String -> Int -> (Int,String)
figureOutBufferName bufs bn ind =
    let ind = foldr (\buf ind -> if bufferName buf == bn
                    then max ind (addedIndex buf + 1)
                    else ind) 0 bufs in
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

-------------------------
--convinience methods

--widgets upper
getActivePane = getPane 1

getPane :: PaneNum -> GhfM (Widget)
getPane 1 = widgetGet ["topBox","paneLeftRight","paneRight", "notebookBox1" ] id 
getPane 2 = widgetGet ["topBox","paneLeftRight","paneRight", "notebookBox2" ] id 
getPane 3 = widgetGet ["topBox","paneLeftRight","paneLeft", "notebookBox3" ]  id
getPane 4 = widgetGet ["topBox","paneLeftRight","paneLeft", "notebookBox4" ]  id

getNotebook :: Widget -> IO (Notebook)
getNotebook nb =  widgetGetRel nb ["notebook"] castToNotebook


getFindEntry :: Widget -> IO (Entry)
getFindEntry nb =  widgetGetRel nb ["statusBox","searchBox","searchEntry"] castToEntry

getFindBar :: Widget -> IO (HBox)
getFindBar nb =  widgetGetRel nb ["statusBox","searchBox"] castToHBox 

getStatusbarIO :: Widget -> IO (Statusbar)
getStatusbarIO nb =  widgetGetRel nb ["statusBox","statusBarInsertOverwrite"] castToStatusbar 

getStatusbarLC :: Widget -> IO (Statusbar)
getStatusbarLC nb = widgetGetRel nb ["statusBox","statusBarLineColumn"] castToStatusbar

getCaseSensitive :: Widget -> IO (ToggleButton)
getCaseSensitive nb = widgetGetRel nb ["statusBox","searchBox","caseSensitiveButton"] 
                        castToToggleButton

getWrapAround :: Widget -> IO (ToggleButton)
getWrapAround nb = widgetGetRel nb ["statusBox","searchBox","wrapAroundButton"] 
                        castToToggleButton

getEntireWord :: Widget -> IO (ToggleButton)
getEntireWord nb = widgetGetRel nb ["statusBox","searchBox","entireWordButton"] 
                        castToToggleButton

getGotoLineSpin :: Widget -> IO (SpinButton)
getGotoLineSpin nb = widgetGetRel nb ["statusBox","gotoLineEntry"] castToSpinButton

--actions
getFindAction = getUIAction "ui/menubar/_Edit/_Find" castToToggleAction