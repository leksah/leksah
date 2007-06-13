module Ghf.CoreGui (
    figureOutBufferName
,   realBufferName
,   getMainBuffersNotebook
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
import Ghf.Core
import Data.Maybe

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

getUIAction :: String -> (Action -> a) -> GhfM(a)    
getUIAction str f = do
    uiManager <- readGhf uiManager
    lift $ do
        findAction <- uiManagerGetAction uiManager str
        return (f (fromJust findAction))

-------------------------
--convinience methods

--widgets upper
getMainBuffersNotebook :: GhfM (Notebook)
getMainBuffersNotebook = widgetGet ["topBox","upperBox","mainBuffers"] castToNotebook 

getFindEntry :: GhfM (Entry)
getFindEntry = widgetGet ["topBox","upperBox","statusBox","searchBox","searchEntry"] castToEntry

getFindBar :: GhfM (HBox)
getFindBar = widgetGet ["topBox","upperBox","statusBox","searchBox"] castToHBox

getStatusbarIO :: GhfM (Statusbar)
getStatusbarIO = widgetGet ["topBox","upperBox","statusBox","statusBarInsertOverwrite"] castToStatusbar

getStatusbarLC :: GhfM (Statusbar)
getStatusbarLC = widgetGet ["topBox","upperBox","statusBox","statusBarLineColumn"] castToStatusbar

getCaseSensitive :: GhfM (ToggleButton)
getCaseSensitive = widgetGet ["topBox","upperBox","statusBox","searchBox","caseSensitiveButton"] 
                        castToToggleButton

getWrapAround :: GhfM (ToggleButton)
getWrapAround = widgetGet ["topBox","upperBox","statusBox","searchBox","wrapAroundButton"] 
                        castToToggleButton
getEntireWord :: GhfM (ToggleButton)
getEntireWord = widgetGet ["topBox","upperBox","statusBox","searchBox","entireWordButton"] 
                        castToToggleButton

getGotoLineSpin :: GhfM (SpinButton)
getGotoLineSpin = widgetGet ["topBox","upperBox","statusBox","gotoLineEntry"] castToSpinButton
--------
{--
--widgets lower
getMainBuffersNotebook2 :: GhfM (Notebook)
getMainBuffersNotebook2 = widgetGet ["topBox","lowerBox","mainBuffers"] castToNotebook 

getFindEntry2 :: GhfM (Entry)
getFindEntry2 = widgetGet ["topBox","lowerBox","statusBox","searchBox","searchEntry"] castToEntry

getFindBar2 :: GhfM (HBox)
getFindBar2 = widgetGet ["topBox","lowerBox","statusBox","searchBox"] castToHBox

getStatusbarIO2 :: GhfM (Statusbar)
getStatusbarIO2 = widgetGet ["topBox","lowerBox","statusBox","statusBarInsertOverwrite"] castToStatusbar

getStatusbarLC2 :: GhfM (Statusbar)
getStatusbarLC2 = widgetGet ["topBox","lowerBox","statusBox","statusBarLineColumn"] castToStatusbar

getCaseSensitive2 :: GhfM (ToggleButton)
getCaseSensitive2 = widgetGet ["topBox","lowerBox","statusBox","searchBox","caseSensitiveButton"] 
                        castToToggleButton

getWrapAround2 :: GhfM (ToggleButton)
getWrapAround2 = widgetGet ["topBox","lowerBox","statusBox","searchBox","wrapAroundButton"] 
                        castToToggleButton
getEntireWord2 :: GhfM (ToggleButton)
getEntireWord2 = widgetGet ["topBox","lowerBox","statusBox","searchBox","entireWordButton"] 
                        castToToggleButton

getGotoLineSpin2 :: GhfM (SpinButton)
getGotoLineSpin2 = widgetGet ["topBox","lowerBox","statusBox","gotoLineEntry"] castToSpinButton
--}

-------------------
--actions
getFindAction = getUIAction "ui/menubar/_Edit/_Find" castToToggleAction