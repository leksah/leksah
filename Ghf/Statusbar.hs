module Ghf.Statusbar (
    buildStatusbar
,   getFindEntry
,   getFindBar
,   getStatusbarIO
,   getStatusbarLC
,   getCaseSensitive
,   getGotoLineSpin
,   getFindAction
,   getWrapAround
,   getEntireWord

)where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.Types

buildStatusbar :: IO (HBox)
buildStatusbar =
    sb <- statusbarNew
    statusbarSetHasResizeGrip sb False

    sblc <- statusbarNew
    widgetSetName sblc $"statusBarLineColumn" 
    statusbarSetHasResizeGrip sblc False
    widgetSetSizeRequest sblc 140 (-1)

    sbio <- statusbarNew
    widgetSetName sbio $"statusBarInsertOverwrite" 
    statusbarSetHasResizeGrip sbio False
    widgetSetSizeRequest sbio 40 (-1)

    entry <- entryNew
    widgetSetName entry $"searchEntry"

    caseSensitiveButton <- checkButtonNewWithLabel "Case sensitive"
    widgetSetName caseSensitiveButton $"caseSensitiveButton" 

    entireWordButton <- checkButtonNewWithLabel "Entire word"
    widgetSetName entireWordButton $"entireWordButton" 

    wrapAroundButton <- checkButtonNewWithLabel "Warp around"
    widgetSetName wrapAroundButton $"wrapAroundButton" 

    dummy <- hBoxNew False 1
    widgetSetName dummy $"dummyBox" 

    spinL <- spinButtonNewWithRange 1.0 100.0 10.0
    widgetSetName spinL $"gotoLineEntry"

    hbf <- hBoxNew False 1
    widgetSetName hbf $"searchBox" 
    boxPackStart hbf entry PackGrow 0
    boxPackStart hbf caseSensitiveButton PackNatural 0
    boxPackStart hbf entireWordButton PackNatural 0
    boxPackStart hbf wrapAroundButton PackNatural 0

    hb <- hBoxNew False 1
    widgetSetName hb $ "statusBox"
    boxPackStart hb dummy PackGrow 0
    boxPackStart hb spinL PackGrow 0
    boxPackStart hb hbf PackGrow 0
    boxPackStart hb sblc PackNatural 0
    boxPackStart hb sbio PackNatural 0

    entry `afterInsertText` (\_ _ -> do runReaderT (editFindInc Insert) ghfR; 
                                        t <- entryGetText entry
                                        return (length t))
    entry `afterDeleteText` (\_ _ -> do runReaderT (editFindInc Delete) ghfR; return ())
    entry `afterKeyPress`  (\e -> do runReaderT (editFindKey e) ghfR; return True)
    entry `onEntryActivate` runReaderT (editFindHide) ghfR

    spinL `afterKeyPress`  (\e -> do runReaderT (editGotoLineKey e) ghfR; return True)
    spinL `afterEntryActivate` runReaderT editGotoLineEnd ghfR
    spinL `afterFocusOut` (\_ -> do runReaderT editGotoLineEnd ghfR; return False)
    return hb

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