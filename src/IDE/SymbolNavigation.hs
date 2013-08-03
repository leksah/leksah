{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.SymbolNavigation
-- Copyright   :  (c) Sanny Sannof, Juergen Nicklisch-Franken
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | The source editor part of Leksah
--
-----------------------------------------------------------------------------------

module IDE.SymbolNavigation (
    createHyperLinkSupport,
    mapControlCommand
) where

import Graphics.UI.Gtk
       (cursorNew, eventRootCoordinates, widgetAddEvents,
        buttonPressEvent, eventIsHint, motionNotifyEvent, eventModifier,
        drawWindowGetPointer, eventCoordinates, EventM, DrawWindow,
        pointerGrab, screenGetDefault,
        widgetGetAllocation, scrolledWindowGetVAdjustment,
        adjustmentGetValue, scrolledWindowGetHAdjustment, pointerUngrab,
        eventTime, leaveNotifyEvent, ScrolledWindow, Modifier(..),
        Rectangle(..), EventMask(..), Underline(..),
#if MIN_VERSION_gtk(0,13,0) || defined(MIN_VERSION_gtk3)
        widgetGetWindow
#else
        widgetGetDrawWindow
#endif
        )
import System.Glib.Signals (on)
import IDE.TextEditor (TextEditor(..), EditorView(..), EditorIter(..))
import IDE.Core.Types (IDEM)
import Graphics.UI.Editor.Basics (Connection(..), Connection)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk.Gdk.Cursor (CursorType(..))
import IDE.Utils.GUIUtils (mapControlCommand)
import Data.IORef (writeIORef, readIORef, newIORef)
import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import Control.Monad.Reader.Class (MonadReader(..))
import IDE.Core.State (reflectIDE)
import Control.Applicative ((<$>))

data Locality = LocalityPackage  | LocalityWorkspace | LocalitySystem  -- in which category symbol is located
    deriving (Ord,Eq,Show)

-- |
createHyperLinkSupport
    :: TextEditor editor
    => EditorView editor -- ^ source buffer view
    -> ScrolledWindow    -- ^ container window
    -> (Bool -> Bool -> EditorIter editor -> IDEM (EditorIter editor, EditorIter editor)) -- ^ identifiermapper (bools=control,shift)
    -> (Bool -> Bool -> String -> IDEM ()) -- ^ click handler
    -> IDEM [Connection]
createHyperLinkSupport sv sw identifierMapper clickHandler = do
    tv <- getEditorWidget sv
    tvb <- getBuffer sv
    ttt <- getTagTable tvb
    linkTag <- newTag ttt "link"
    underline linkTag UnderlineSingle
    cursor <- liftIO $ cursorNew Hand2

    id1 <- liftIO $ sw `on` leaveNotifyEvent $ do
        eventTime >>= (liftIO . pointerUngrab)
        return True

    let moveOrClick eventX eventY mods eventTime click = do
        sx <- liftIO $ scrolledWindowGetHAdjustment sw >>= adjustmentGetValue
        sy <- liftIO $ scrolledWindowGetVAdjustment sw >>= adjustmentGetValue

        let ex = eventX + sx
            ey = eventY + sy
            ctrlPressed = (mapControlCommand Control) `elem` mods
            shiftPressed = Shift `elem` mods
        iter <- getIterAtLocation sv (round ex) (round ey)
        (Rectangle _ _ szx szy) <- liftIO $ widgetGetAllocation sw
        if eventX < 0 || eventY < 0
            || round eventX > szx || round eventY > szy then do
                liftIO $ pointerUngrab eventTime
                return True
          else do
            (beg, en) <- identifierMapper ctrlPressed shiftPressed iter
            slice <- getSlice tvb beg en True
            removeTagByName tvb "link"
            offs <- getLineOffset beg
            offsc <- getLineOffset iter
            if (length slice > 1) then do
                if (click) then do
                        liftIO $ pointerUngrab eventTime
                        clickHandler ctrlPressed shiftPressed slice
                    else do
                        applyTagByName tvb "link" beg en
                        Just screen <- liftIO $ screenGetDefault
#if MIN_VERSION_gtk(0,13,0) || defined(MIN_VERSION_gtk3)
                        mbDW <- liftIO $ widgetGetWindow tv
#else
                        mbDW <- liftIO $ Just <$> widgetGetDrawWindow tv
#endif
                        case mbDW of
                            Nothing -> return ()
                            Just dw -> do
                                liftIO $ pointerGrab dw False [PointerMotionMask,ButtonPressMask,LeaveNotifyMask] (Nothing  :: Maybe DrawWindow) (Just cursor) eventTime
                                return ()
              else do
                liftIO $ pointerUngrab eventTime
                return ()
            return True
    lineNumberBugFix <- liftIO $ newIORef Nothing
    let fixBugWithX mods isHint (eventX, eventY) ptrx = do
            let hasNoControlModifier = not $ (mapControlCommand Control) `elem` mods
            lnbf <- readIORef lineNumberBugFix
            -- print ("ishint?, adjusted, event.x, ptr.x, adjustment,hasControl?",isHint,ptrx - fromMaybe (-1000) lnbf , eventX, ptrx, lnbf, hasNoControlModifier)
            when (isHint && hasNoControlModifier) $ do
                -- get difference between event X and pointer x
                -- event X is in coordinates of sourceView text
                -- pointer X is in coordinates of window (remember "show line numbers" ?)
                liftIO $ writeIORef lineNumberBugFix $ Just (ptrx - eventX)   -- captured difference
            -- When control key is pressed, mostly NON-HINT events come,
            -- GTK gives (mistakenly?) X in window coordinates in such cases
            let nx = if (isJust lnbf && not isHint)
                        then ptrx - fromJust lnbf    -- translate X back
                        else eventX
            return (nx, eventY)
    ideR <- ask
    id2 <- liftIO $ sw `on` motionNotifyEvent $ do
        isHint <- eventIsHint
        eventTime <- eventTime
        mods <- eventModifier
        (oldX, oldY) <- eventCoordinates
        (rootX, _) <- eventRootCoordinates
        (eventX, eventY) <- liftIO $ fixBugWithX mods isHint (oldX, oldY) rootX
        liftIO $ do
            -- print ("move adjustment: isHint, old, new root", isHint, eventX, oldX, rootX)
            (`reflectIDE` ideR) $ moveOrClick eventX eventY mods eventTime False
        return True
    id3 <- liftIO $ sw `on` buttonPressEvent $ do
        eventTime <- eventTime
        mods <- eventModifier
        -- liftIO $ print ("button press")
        (oldX, oldY) <- eventCoordinates
        (rootX, _) <- eventRootCoordinates
        (eventX, eventY) <- liftIO $ fixBugWithX mods False (oldX, oldY) rootX
        -- liftIO $ print ("click adjustment: old, new", eventX, oldX)
        liftIO $ (`reflectIDE` ideR) $ moveOrClick eventX eventY mods eventTime True

    return $ [ConnectC id1, ConnectC id2, ConnectC id3]


