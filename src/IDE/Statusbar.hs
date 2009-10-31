-----------------------------------------------------------------------------
--
-- Module      :  IDE.Statusbar
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Builds and updates the Statusbar, To update the bar triiger the Statusbar changed event.
--
-----------------------------------------------------------------------------

module IDE.Statusbar (
    changeStatusbar
,   buildStatusbar
) where
import IDE.Core.State
    (getMainWindow,
     widgetGet,
     PaneMonad(..),
     IDEAction(..),
     StatusbarCompartment(..))
import IDE.Core.Types (StatusbarCompartment(..))
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
    (windowSetTitle,
     castToStatusbar,
     Statusbar(..),
     boxPackStart,
     hBoxNew,
     widgetSetSizeRequest,
     widgetSetName,
     statusbarSetHasResizeGrip,
     statusbarNew,
     HBox(..),
     statusbarPush,
     statusbarPop,
     Packing(..),
     boxPackEnd)
import Graphics.UI.Frame.Panes (IDEPane(..), paneName)
import Text.Printf (printf)


changeStatusbar :: [StatusbarCompartment] -> IDEAction
changeStatusbar = mapM_ changeStatusbar'
    where
    changeStatusbar' (CompartmentCommand accStr) =  do
        sb <- getSBSpecialKeys
        liftIO $statusbarPop sb 1
        liftIO $statusbarPush sb 1 accStr
        return ()
    changeStatusbar' (CompartmentPane (Just (PaneC pane))) =  do
        sb <- getSBActivePane
        liftIO $ statusbarPop sb 1
        liftIO $ statusbarPush sb 1 (paneName pane)
        return ()
    changeStatusbar' (CompartmentPane Nothing) =  do
        sb <- getSBActivePane
        liftIO $ statusbarPop sb 1
        liftIO $ statusbarPush sb 1 ""
        return ()
    changeStatusbar' (CompartmentState string) =  do
        let realStr = if '\n' `elem` string then (head $ lines string) ++ " ..." else string
        sb <- getSBErrors
        liftIO $ statusbarPop sb 1
        liftIO $ statusbarPush sb 1 realStr
        return ()
    changeStatusbar' (CompartmentPackage string) =  do
        sb <- getSBActivePackage
        window <- getMainWindow
        liftIO $ statusbarPop sb 1
        liftIO $ statusbarPush sb 1 string
        liftIO $ windowSetTitle window $ "Leksah: " ++  string
        return ()
    changeStatusbar' (CompartmentBufferPos (line,col)) =  do
        sb <- getStatusbarLC
        liftIO $ statusbarPop sb 1
        liftIO $ statusbarPush sb 1 $ printf "Ln %4d, Col %3d" (line + 1) (col + 1)
        return ()
    changeStatusbar' (CompartmentOverlay modi) =  do
        sb <- getStatusbarIO
        liftIO $ statusbarPop sb 1
        liftIO $ statusbarPush sb 1 $ if modi then "OVR" else "INS"
        return ()


buildStatusbar :: IO HBox
buildStatusbar = do
    sb <- statusbarNew
    statusbarSetHasResizeGrip sb False

    sblk <- statusbarNew
    widgetSetName sblk "statusBarSpecialKeys"
    statusbarSetHasResizeGrip sblk False
    widgetSetSizeRequest sblk 150 (-1)

    sbap <- statusbarNew
    widgetSetName sbap "statusBarActivePane"
    statusbarSetHasResizeGrip sbap False
    widgetSetSizeRequest sbap 150 (-1)

    sbapr <- statusbarNew
    widgetSetName sbapr "statusBarActiveProject"
    statusbarSetHasResizeGrip sbapr False
    widgetSetSizeRequest sbapr 150 (-1)

    sbe <- statusbarNew
    widgetSetName sbe "statusBarErrors"
    statusbarSetHasResizeGrip sbe False
    widgetSetSizeRequest sbe 150 (-1)

    sblc <- statusbarNew
    widgetSetName sblc "statusBarLineColumn"
    statusbarSetHasResizeGrip sblc True
    widgetSetSizeRequest sblc 150 (-1)

    sbio <- statusbarNew
    widgetSetName sbio "statusBarInsertOverwrite"
    statusbarSetHasResizeGrip sbio False
    widgetSetSizeRequest sbio 60 (-1)

    dummy <- hBoxNew False 1
    widgetSetName dummy "dummyBox"


    hb <- hBoxNew False 1
    widgetSetName hb "statusBox"
    boxPackStart hb sblk PackGrow 0
    boxPackStart hb sbap PackGrow 0
    boxPackStart hb sbapr PackGrow 0
    --boxPackStart hb dummy PackGrow 0
    boxPackEnd hb sblc PackNatural 0
    boxPackEnd hb sbio PackNatural 0
    boxPackEnd hb sbe PackNatural 0

    return hb

getSBSpecialKeys :: PaneMonad alpha => alpha Statusbar
getSBSpecialKeys   = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarSpecialKeys"] castToStatusbar

getSBActivePane :: PaneMonad alpha => alpha Statusbar
getSBActivePane    = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarActivePane"] castToStatusbar

getSBActivePackage :: PaneMonad alpha => alpha Statusbar
getSBActivePackage = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarActiveProject"] castToStatusbar

getSBErrors :: PaneMonad alpha => alpha Statusbar
getSBErrors        = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarErrors"] castToStatusbar

getStatusbarIO :: PaneMonad alpha => alpha Statusbar
getStatusbarIO     =  widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarInsertOverwrite"] castToStatusbar

getStatusbarLC :: PaneMonad alpha => alpha Statusbar
getStatusbarLC     = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarLineColumn"] castToStatusbar
