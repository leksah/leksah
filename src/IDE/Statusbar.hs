{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Statusbar
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
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
       (postAsyncIDE, getMainWindow, widgetGet, PaneMonad(..),
        IDEAction(..), StatusbarCompartment(..))
import Graphics.UI.Frame.Panes (IDEPane(..), paneName)
import Text.Printf (printf)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, lines, unpack)
import Data.Monoid ((<>))
import GI.Gtk.Objects.Statusbar
       (Statusbar(..), statusbarNew, statusbarPush, statusbarPop)
import GI.Gtk.Objects.Window (setWindowTitle)
import GI.Gtk.Objects.Image
       (Image(..), imageSetPixelSize, imageNewFromIconName,
        imageSetFromIconName)
import GI.Gtk.Enums (IconSize(..), Orientation(..))
import GI.Gtk.Objects.Box (boxNew, Box(..))
import GI.Gtk.Objects.Widget (widgetSetSizeRequest, widgetSetName)
import Graphics.UI.Editor.Parameters
       (Packing(..), boxPackEnd', boxPackStart')
import Data.GI.Base.ManagedPtr (unsafeCastTo)

changeStatusbar :: [StatusbarCompartment] -> IDEAction
changeStatusbar = postAsyncIDE . mapM_ changeStatusbar'
    where
    changeStatusbar' (CompartmentCommand accStr) =  do
        sb <- getSBSpecialKeys
        statusbarPop sb 1
        statusbarPush sb 1 accStr
        return ()
    changeStatusbar' (CompartmentPane (Just (PaneC pane))) =  do
        sb <- getSBActivePane
        statusbarPop sb 1
        statusbarPush sb 1 (paneName pane)
        return ()
    changeStatusbar' (CompartmentPane Nothing) =  do
        sb <- getSBActivePane
        statusbarPop sb 1
        statusbarPush sb 1 ""
        return ()
    changeStatusbar' (CompartmentState string) =  do
        let realStr = if '\n' `elem` T.unpack string then head (T.lines string) <> " ..." else string
        sb <- getSBErrors
        statusbarPop sb 1
        statusbarPush sb 1 realStr
        return ()
    changeStatusbar' (CompartmentPackage string) =  do
        sb <- getSBActivePackage
        window <- getMainWindow
        statusbarPop sb 1
        statusbarPush sb 1 string
        setWindowTitle window $ "Leksah: " <> string
        return ()
    changeStatusbar' (CompartmentBufferPos (line,col)) =  do
        sb <- getStatusbarLC
        statusbarPop sb 1
        statusbarPush sb 1 (T.pack $ printf "Ln %4d, Col %3d" (line + 1) (col + 1))
        return ()
    changeStatusbar' (CompartmentOverlay modi) =  do
        sb <- getStatusbarIO
        statusbarPop sb 1
        statusbarPush sb 1 $ if modi then "OVR" else "INS"
        return ()
    changeStatusbar' (CompartmentBuild bool) =  do
        im <- getImBuild
        imageSetFromIconName im (Just $ if bool then "ide_build" else "ide_empty") (fromIntegral . fromEnum $ IconSizeMenu)
        return ()
    changeStatusbar' (CompartmentCollect bool) =  do
        im <- getImCollect
        imageSetFromIconName im (Just $ if bool then "ide_rebuild_meta" else "ide_empty") (fromIntegral . fromEnum $ IconSizeMenu)
        return ()


buildStatusbar :: MonadIO m => m Box
buildStatusbar = do
    sblk <- statusbarNew
    widgetSetName sblk "statusBarSpecialKeys"
    widgetSetSizeRequest sblk 150 (-1)

    sbap <- statusbarNew
    widgetSetName sbap "statusBarActivePane"
    widgetSetSizeRequest sbap 150 (-1)

    sbapr <- statusbarNew
    widgetSetName sbapr "statusBarActiveProject"
    widgetSetSizeRequest sbapr 150 (-1)

    sbe <- statusbarNew
    widgetSetName sbe "statusBarErrors"
    widgetSetSizeRequest sbe 150 (-1)

    sblc <- statusbarNew
    widgetSetName sblc "statusBarLineColumn"
    widgetSetSizeRequest sblc 150 (-1)

    sbio <- statusbarNew
    widgetSetName sbio "statusBarInsertOverwrite"
    widgetSetSizeRequest sbio 60 (-1)

    buildImage <- imageNewFromIconName (Just "ide_empty") (fromIntegral $ fromEnum IconSizeMenu)
    widgetSetName buildImage "buildImage"
    imageSetPixelSize buildImage 16

    collectImage <- imageNewFromIconName (Just "ide_empty") (fromIntegral $ fromEnum IconSizeMenu)
    widgetSetName collectImage "collectImage"
    imageSetPixelSize collectImage 16

    hb <- boxNew OrientationHorizontal 1
    widgetSetName hb "statusBox"
    boxPackStart' hb sblk PackGrow 0
    boxPackStart' hb sbap PackGrow 0
    boxPackStart' hb sbapr PackGrow 0
    --boxPackStart' hb dummy PackGrow 0
    boxPackEnd' hb sblc PackNatural 0
    boxPackEnd' hb sbio PackNatural 0
    boxPackEnd' hb collectImage PackNatural 0
    boxPackEnd' hb buildImage PackNatural 0
    boxPackEnd' hb sbe PackNatural 0

    return hb

getSBSpecialKeys :: PaneMonad alpha => alpha Statusbar
getSBSpecialKeys   = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarSpecialKeys"] (unsafeCastTo Statusbar)

getSBActivePane :: PaneMonad alpha => alpha Statusbar
getSBActivePane    = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarActivePane"] (unsafeCastTo Statusbar)

getSBActivePackage :: PaneMonad alpha => alpha Statusbar
getSBActivePackage = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarActiveProject"] (unsafeCastTo Statusbar)

getSBErrors :: PaneMonad alpha => alpha Statusbar
getSBErrors        = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarErrors"] (unsafeCastTo Statusbar)

getStatusbarIO :: PaneMonad alpha => alpha Statusbar
getStatusbarIO     =  widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarInsertOverwrite"] (unsafeCastTo Statusbar)

getStatusbarLC :: PaneMonad alpha => alpha Statusbar
getStatusbarLC     = widgetGet ["Leksah Main Window", "topBox","statusBox","statusBarLineColumn"] (unsafeCastTo Statusbar)

getImBuild :: PaneMonad alpha => alpha Image
getImBuild        = widgetGet ["Leksah Main Window", "topBox","statusBox","buildImage"] (unsafeCastTo Image)

getImCollect :: PaneMonad alpha => alpha Image
getImCollect        = widgetGet ["Leksah Main Window", "topBox","statusBox","collectImage"] (unsafeCastTo Image)
