{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
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

import IDE.TextEditor (TextEditor(..), EditorView(..), EditorIter(..))
import IDE.Core.Types (IDEM)
import Control.Monad.IO.Class (MonadIO(..))
import IDE.Utils.GUIUtils (mapControlCommand)
import Data.IORef (writeIORef, readIORef, newIORef)
import Control.Monad (void, join, when)
import Data.Maybe (fromJust, isJust)
import Control.Monad.Reader.Class (MonadReader(..))
import IDE.Core.State (reflectIDE)
import Control.Applicative ((<$>))
import Data.Text (Text)
import qualified Data.Text as T (length)
import GI.Gtk.Objects.ScrolledWindow
       (getScrolledWindowVadjustment, getScrolledWindowHadjustment,
        ScrolledWindow(..))
import Graphics.UI.Editor.Basics (Connection(..), Connection)
import GI.Pango.Enums (Underline(..))
import GI.Gdk.Objects.Cursor (cursorNew)
import GI.Gdk.Enums (GrabOwnership(..), CursorType(..))
import GI.Gtk.Objects.Widget
       (onWidgetButtonPressEvent, onWidgetMotionNotifyEvent,
        widgetGetWindow, widgetGetAllocation, onWidgetLeaveNotifyEvent)
import GI.Gdk.Structs.EventCrossing (getEventCrossingTime)
import GI.Gdk.Functions (pointerGrab, pointerUngrab)
import GI.Gtk.Objects.Adjustment (adjustmentGetValue)
import Graphics.UI.Frame.Rectangle
       (getRectangleHeight, getRectangleWidth)
import GI.Gdk.Objects.Screen (screenGetDefault)
import qualified GI.Gdk.Objects.Window as Gdk (noWindow)
import GI.Gdk.Structs.EventMotion
       (getEventMotionXRoot, getEventMotionY, getEventMotionX,
        getEventMotionState, getEventMotionTime, getEventMotionIsHint)
import GI.Gdk.Structs.EventButton
       (getEventButtonXRoot, getEventButtonY, getEventButtonX,
        getEventButtonState, getEventButtonTime)
import GI.Gdk.Objects.Window (windowGetOrigin)
import GI.Gdk
       (getEventButtonDevice, getEventMotionDevice,
        deviceGetDisplay, cursorNewForDisplay, displayGetDefault)
import Data.Foldable (forM_)
-- TODO fix seat code
#undef MIN_VERSION_GTK_3_20
#ifdef MIN_VERSION_GTK_3_20
import GI.Gdk.Flags
       (SeatCapabilities(..), EventMask(..), ModifierType(..))
import GI.Gdk.Objects.Device (deviceGetSeat)
import GI.Gdk.Objects.Seat (seatGrab, seatUngrab)
#else
import GI.Gdk.Flags
       (EventMask(..), ModifierType(..))
import GI.Gdk.Objects.Device (deviceGrab, deviceUngrab)
import IDE.TypeTip (setTypeTip)
import qualified IDE.TextEditor.Class as E (TextEditor(..))
#endif

data Locality = LocalityPackage  | LocalityWorkspace | LocalitySystem  -- in which category symbol is located
    deriving (Ord,Eq,Show)

-- |
createHyperLinkSupport
    :: TextEditor editor
    => EditorView editor -- ^ source buffer view
    -> ScrolledWindow    -- ^ container window
    -> (Bool -> Bool -> EditorIter editor -> IDEM (EditorIter editor, EditorIter editor)) -- ^ identifiermapper (bools=control,shift)
    -> (Bool -> Bool -> (EditorIter editor, EditorIter editor) -> IDEM ()) -- ^ click handler
    -> IDEM [Connection]
createHyperLinkSupport sv sw identifierMapper clickHandler = do
    tv <- getEditorWidget sv
    tvb <- getBuffer sv
    ttt <- getTagTable tvb
    linkTag <- newTag ttt "link"
    underline linkTag UnderlineSingle Nothing
    linkLocIORef <- liftIO $ newIORef Nothing

--    id1 <- ConnectC sw <$> onWidgetLeaveNotifyEvent sw (\e -> do
--        getEventCrossingTime e >>= pointerUngrab
--        return True)

    let moveOrClick mbDevice eventX eventY mods eventTime mbMotion = do
#ifdef MIN_VERSION_GTK_3_20
            mbSeat <- mapM deviceGetSeat mbDevice
            let ungrab = mapM_ seatUngrab mbSeat
#else
            let ungrab = mapM_ (`deviceUngrab` eventTime) mbDevice
#endif
            mbHand <- mapM deviceGetDisplay mbDevice
                >>= mapM (`cursorNewForDisplay` CursorTypeHand2)
            sx <- getScrolledWindowHadjustment sw >>= adjustmentGetValue
            sy <- getScrolledWindowVadjustment sw >>= adjustmentGetValue

            let ex = eventX + sx
                ey = eventY + sy
                ctrlPressed = mapControlCommand ModifierTypeControlMask `elem` mods
                shiftPressed = ModifierTypeShiftMask `elem` mods
            iter <- getIterAtLocation sv (round ex) (round ey)
            rect <- widgetGetAllocation sw
            szx <- getRectangleWidth rect
            szy <- getRectangleHeight rect
            linkIters <- if not ctrlPressed || eventX < 0 || eventY < 0
                || round eventX > szx || round eventY > szy then do
                    ungrab
                    setTypeTip (0, 0) ""
                    return Nothing
              else do
                (beg, en) <- identifierMapper ctrlPressed shiftPressed iter
                slice <- getSlice tvb beg en True
                offs <- getLineOffset beg
                offsc <- getLineOffset iter
                if T.length slice > 1 then
                    case mbMotion of
                        Nothing -> do
                            ungrab
                            clickHandler ctrlPressed shiftPressed (beg, en)
                            setTypeTip (0, 0) ""
                            return Nothing
#ifdef MIN_VERSION_GTK_3_20
                        Just motion -> do
#else
                        Just _ -> do
#endif
                            screen <- screenGetDefault
                            widgetGetWindow tv >>= \case
                                Nothing -> return ()
                                Just dw ->
#ifdef MIN_VERSION_GTK_3_20
                                    forM_ mbSeat $ \seat ->
                                        seatGrab seat dw [SeatCapabilitiesPointer] False mbHand (_convertToEvent <$> motion) Nothing
#else
                                    forM_ mbDevice $ \device ->
                                        deviceGrab device dw GrabOwnershipNone False
                                            [EventMaskPointerMotionMask,EventMaskButtonPressMask,EventMaskLeaveNotifyMask]
                                            mbHand eventTime
#endif
                            return $ Just (beg, en)
                  else setTypeTip (0, 0) "" >> ungrab >> return Nothing
            oldLoc <- liftIO $ readIORef linkLocIORef
            case linkIters of
                Just (beg, en) -> do
                    linkLoc <- sequence [E.getLine beg, getLineOffset beg, E.getLine en, getLineOffset en]
                    when (Just linkLoc /= oldLoc) $ do
                        removeTagByName tvb "link"
                        applyTagByName tvb "link" beg en
                        liftIO $ writeIORef linkLocIORef (Just linkLoc)
                Nothing -> do
                    when (isJust oldLoc) $ removeTagByName tvb "link"
                    liftIO $ writeIORef linkLocIORef Nothing
            return True
    lineNumberBugFix <- liftIO $ newIORef Nothing
    let fixBugWithX mods isHint (eventX, eventY) ptrx' = do
            Just window <- widgetGetWindow sw
            (_, ox, _) <- windowGetOrigin window
            let ptrx = ptrx' - (fromIntegral ox)
            let hasNoControlModifier = mapControlCommand ModifierTypeControlMask `notElem` mods
            lnbf <- readIORef lineNumberBugFix
            -- print ("ishint?, adjusted, event.x, ptr.x, adjustment,hasControl?",isHint, eventX, ptrx, lnbf, ox, hasNoControlModifier)
            -- when (isHint && hasNoControlModifier) $
            when (abs (ptrx - eventX) > 2) $
                -- get difference between event X and pointer x
                -- event X is in coordinates of sourceView text
                -- pointer X is in coordinates of window (remember "show line numbers" ?)
                liftIO $ writeIORef lineNumberBugFix $ Just (ptrx - eventX)   -- captured difference
            -- When control key is pressed, mostly NON-HINT events come,
            -- GTK gives (mistakenly?) X in window coordinates in such cases
            let nx = if isJust lnbf && not isHint
                        then ptrx - fromJust lnbf    -- translate X back
                        else eventX
            return (nx, eventY)
    ideR <- ask
    id2 <- ConnectC sw <$> onWidgetMotionNotifyEvent sw (\e -> do
        isHint <- (/=0) <$> getEventMotionIsHint e
        eventTime <- getEventMotionTime e
        mods <- getEventMotionState e
        oldX <- getEventMotionX e
        oldY <- getEventMotionY e
        rootX <- getEventMotionXRoot e
        device <- getEventMotionDevice e
        (eventX, eventY) <- liftIO $ fixBugWithX mods isHint (oldX, oldY) rootX
        -- print ("move adjustment: isHint, old, new root", isHint, eventX, oldX, rootX)
        (`reflectIDE` ideR) $ moveOrClick device eventX eventY mods eventTime (Just e)
        return True)
    id3 <- ConnectC sw <$> onWidgetButtonPressEvent sw (\e -> do
        eventTime <- getEventButtonTime e
        mods <- getEventButtonState e
        -- liftIO $ print ("button press")
        oldX <- getEventButtonX e
        oldY <- getEventButtonY e
        rootX <- getEventButtonXRoot e
        device <- getEventButtonDevice e
        (eventX, eventY) <- liftIO $ fixBugWithX mods False (oldX, oldY) rootX
        -- liftIO $ print ("click adjustment: old, new", eventX, oldX)
        (`reflectIDE` ideR) $ moveOrClick device eventX eventY mods eventTime Nothing)

    return [{-id1,-} {- id2, -} id3]


