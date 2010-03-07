-----------------------------------------------------------------------------
--
-- Module      :  IDE.NotebookFlipper
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  <maintainer@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.NotebookFlipper (
    flipDown
,   flipUp
) where

import Graphics.UI.Gtk
import IDE.Core.State
import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Control.Monad (when)
import IDE.Pane.SourceBuffer(recentSourceBuffers)

flipDown :: IDEAction
flipDown = do
    currentState' <- readIDE currentState
    case currentState' of
        IsFlipping tv -> moveFlipperDown tv
        IsRunning     -> initFlipper True
        _             -> return ()

flipUp :: IDEAction
flipUp = do
    currentState' <- readIDE currentState
    case currentState' of
        IsFlipping  tv -> moveFlipperUp tv
        IsRunning      -> initFlipper False
        _              -> return ()

-- | Moves down in the Flipper state
moveFlipperDown :: TreeViewClass alpha => alpha -> IDEAction
moveFlipperDown tree = liftIO $ do
    mbStore <- treeViewGetModel tree
    case mbStore of
        Nothing -> throwIDE "NotebookFlipper>>setFlipper: no store"
        Just store -> do
            n <- treeModelIterNChildren store Nothing
            when (n /= 0) $ do
                (cl, _) <- treeViewGetCursor tree
                case cl of
                    (current:_) ->  let next =  if current == n - 1
                                                    then 0
                                                    else current + 1
                                    in treeViewSetCursor tree [min (n-1) next] Nothing
                    []          ->  treeViewSetCursor tree [1] Nothing

-- | Moves up in the Flipper state
moveFlipperUp :: TreeViewClass alpha => alpha  -> IDEAction
moveFlipperUp tree = liftIO $ do
    mbStore <- treeViewGetModel tree
    case mbStore of
        Nothing -> throwIDE "NotebookFlipper>>setFlipper: no store"
        Just store -> do
            n <- treeModelIterNChildren store Nothing
            when (n /= 0) $ do
                (cl, _) <- treeViewGetCursor tree
                case cl of
                    (current:_) ->  let next =  if current == 0
                                                    then n - 1
                                                    else current - 1
                                    in treeViewSetCursor tree [min (n-1) next] Nothing
                    []          ->  treeViewSetCursor tree [n-1] Nothing

-- | Initiate Filpper , If True moves down, if false up
initFlipper :: Bool -> IDEAction
initFlipper direction = do
    mainWindow   <- getMainWindow
    recentPanes' <-  recentSourceBuffers
    tree' <- reifyIDE $ \ideR -> do
        window <- windowNewPopup
        windowSetTransientFor window mainWindow
        set window [windowResizable := True]
        frame <- frameNew
        containerAdd window frame
        tree <- treeViewNew

        containerAdd frame tree
        store <- listStoreNew recentPanes'
        treeViewSetModel tree store
        column <- treeViewColumnNew
        treeViewAppendColumn tree column
        renderer <- cellRendererTextNew
        treeViewColumnPackStart column renderer True
        cellLayoutSetAttributes column renderer store
            (\str -> [ cellText := str])

        set tree [treeViewHeadersVisible := False]

        cid <- onKeyRelease mainWindow $ handleKeyRelease tree ideR

        onRowActivated tree (\treePath column -> do
            signalDisconnect cid
            let [row] = treePath
            string <- listStoreGetValue store row
            reflectIDE (do
                mbPane <- mbPaneFromName string
                case mbPane of
                    Just (PaneC pane) -> makeActive pane
                    Nothing   -> return ()) ideR
            widgetHideAll window
            widgetDestroy window)
        set window [windowWindowPosition := WinPosCenterOnParent]
        n <- treeModelIterNChildren store Nothing
        treeViewSetCursor tree [if direction then min 1 (n-1) else (n-1)] Nothing
        widgetShowAll window
        return tree
    modifyIDE_ (\ide -> ide{currentState = IsFlipping tree'})
    return ()

handleKeyRelease :: TreeViewClass alpha => alpha -> IDERef -> Event -> IO (Bool)
handleKeyRelease tree ideR Key{eventKeyName = name, eventModifier = modifier, eventKeyChar = char} = do
    case (name, modifier, char) of
        (ctrl, _, _) | (ctrl == "Control_L") || (ctrl == "Control_R") -> do
            currentState' <- reflectIDE (readIDE currentState) ideR
            case currentState' of
                IsFlipping  tv -> do
                    (treePath, _) <- treeViewGetCursor tree
                    Just column <- treeViewGetColumn tree 0
                    treeViewRowActivated tree treePath column
                    reflectIDE (modifyIDE_ (\ide -> ide{currentState = IsRunning})) ideR
                    return False
                _ -> return False
        (_,_,_) -> return False
handleKeyRelease tree ideR _ = return False


