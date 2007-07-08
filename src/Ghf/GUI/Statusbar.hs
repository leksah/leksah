module Ghf.GUI.Statusbar (
    buildStatusbar

) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Types
import qualified Data.Map as Map
import Data.Map (Map,(!))
import Control.Monad.Reader

import Ghf.Core
import Ghf.Editor.SourceEditor
import GUI.ViewFrame
import Ghf.Dialogs
import {-# SOURCE #-} Ghf.Preferences(editPrefs)
import Ghf.Package

buildStatusbar ghfR = do
    sb <- statusbarNew
    statusbarSetHasResizeGrip sb False

    sblk <- statusbarNew
    widgetSetName sblk $"statusBarSpecialKeys" $
    statusbarSetHasResizeGrip sblk False
    widgetSetSizeRequest sblk 210 (-1)

    sblc <- statusbarNew
    widgetSetName sblc $"statusBarLineColumn" $
    statusbarSetHasResizeGrip sblc False
    widgetSetSizeRequest sblc 140 (-1)

    sbio <- statusbarNew
    widgetSetName sbio $"statusBarInsertOverwrite" $
    statusbarSetHasResizeGrip sbio False
    widgetSetSizeRequest sbio 40 (-1)

    entry <- entryNew
    widgetSetName entry $"searchEntry"

    caseSensitiveButton <- checkButtonNewWithLabel "Case sensitive"
    widgetSetName caseSensitiveButton $"caseSensitiveButton" $

    entireWordButton <- checkButtonNewWithLabel "Entire word"
    widgetSetName entireWordButton $"entireWordButton" $

    wrapAroundButton <- checkButtonNewWithLabel "Warp around"
    widgetSetName wrapAroundButton $"wrapAroundButton" $

    dummy <- hBoxNew False 1
    widgetSetName dummy $"dummyBox" $

    spinL <- spinButtonNewWithRange 1.0 100.0 10.0
    widgetSetName spinL $"gotoLineEntry"

    hbf <- hBoxNew False 1
    widgetSetName hbf $"searchBox" $
    boxPackStart hbf entry PackGrow 0
    boxPackStart hbf caseSensitiveButton PackNatural 0
    boxPackStart hbf entireWordButton PackNatural 0
    boxPackStart hbf wrapAroundButton PackNatural 0

    hb <- hBoxNew False 1
    widgetSetName hb $ "statusBox"
    boxPackStart hb sblk PackNatural 0
    boxPackStart hb dummy PackGrow 0
    boxPackStart hb spinL PackGrow 0
    boxPackStart hb hbf PackGrow 0
    boxPackStart hb sblc PackNatural 0
    boxPackStart hb sbio PackNatural 0

    entry `afterInsertText` (\ _ _ -> do $
        runReaderT (editFindInc Insert) ghfR
        t <- entryGetText entry
        return (length t))
    entry `afterDeleteText` (\ _ _ -> do runReaderT (editFindInc Delete) ghfR; return ())
    entry `afterKeyPress`  (\ e -> do runReaderT (editFindKey e) ghfR; return True)
    entry `onEntryActivate` runReaderT (editFindHide) ghfR

    spinL `afterKeyPress`  (\ e -> do runReaderT (editGotoLineKey e) ghfR; return True)
    spinL `afterEntryActivate` runReaderT editGotoLineEnd ghfR
    spinL `afterFocusOut` (\ _ -> do runReaderT editGotoLineEnd ghfR; return False)
    return hb
