module Ghf.CoreGui (
    figureOutBufferName
,   realBufferName
,   getMainBuffersNotebook
,   getFindEntry
,   getFindBar
,   getStatusbarIO
,   getStatusbarLC
,   getCaseSensitive
) where
import Graphics.UI.Gtk
import Control.Monad.Reader
import Data.List
import Ghf.Core

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
    

getMainBuffersNotebook :: GhfM (Notebook)
getMainBuffersNotebook = widgetGet ["topBox","mainBuffers"] castToNotebook 

getFindEntry :: GhfM (Entry)
getFindEntry = widgetGet ["topBox","statusBox","searchBox","searchEntry"] castToEntry
getFindBar :: GhfM (HBox)
getFindBar = widgetGet ["topBox","statusBox","searchBox"] castToHBox

getStatusbarIO :: GhfM (Statusbar)
getStatusbarIO = widgetGet ["topBox","statusBox","statusBarInsertOverwrite"] castToStatusbar

getStatusbarLC :: GhfM (Statusbar)
getStatusbarLC = widgetGet ["topBox","statusBox","statusBarLineColumn"] castToStatusbar

getCaseSensitive :: GhfM (ToggleButton)
getCaseSensitive = widgetGet ["topBox","statusBox","searchBox","caseSensitiveButton"] 
                        castToToggleButton
