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

import Prelude ()
import Prelude.Compat

import Control.Monad (void)
import Control.Monad.IO.Class ()
import Control.Monad.Reader.Class (MonadReader(..))

import Data.Foldable (forM_)
import Data.GI.Base.ShortPrelude
import Data.IORef (writeIORef, readIORef, newIORef)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T (length)

import GI.Gdk.Enums (CursorType(..))
import GI.Gdk.Flags
       (SeatCapabilities(..), ModifierType(..))
import GI.Gdk.Objects.Window (windowGetOrigin)
import GI.Gdk
       (getEventButtonDevice, getEventMotionDevice,
        deviceGetDisplay, cursorNewForDisplay)
import GI.Gdk.Objects.Device (deviceGetSeat)
import GI.Gdk.Objects.Seat (seatGrab, seatUngrab)
import GI.Gdk.Unions.Event (getEventMotion)
import GI.Gdk.Structs.EventButton
       (getEventButtonXRoot, getEventButtonY, getEventButtonX,
        getEventButtonState, getEventButtonTime)
import GI.Gdk.Structs.EventMotion
       (getEventMotionXRoot, getEventMotionY, getEventMotionX,
        getEventMotionState, getEventMotionTime, getEventMotionIsHint)
import GI.Gdk.Callbacks (noSeatGrabPrepareFunc)
import qualified GI.Gdk.Unions.Event as Gdk.Event
import GI.Gtk.Objects.Widget
       (onWidgetButtonPressEvent,
        widgetGetWindow, widgetGetAllocation, IsWidget)
import GI.Gtk.Objects.Adjustment (adjustmentGetValue)
import GI.Gtk.Objects.ScrolledWindow
       (getScrolledWindowVadjustment, getScrolledWindowHadjustment,
        ScrolledWindow(..))
import GI.Pango.Enums (Underline(..))

import Graphics.UI.Editor.Basics (Connection(..), Connection)
import Graphics.UI.Frame.Rectangle
       (getRectangleHeight, getRectangleWidth)

import IDE.Core.State (reflectIDE)
import IDE.Core.Types (IDEM)
import IDE.TextEditor (TextEditor(..), EditorView(..), EditorIter(..))
import qualified IDE.TextEditor.Class as E (TextEditor(..))
import IDE.TypeTip (setTypeTip)
import IDE.Utils.GUIUtils (mapControlCommand)

data Locality = LocalityPackage  | LocalityWorkspace | LocalitySystem  -- in which category symbol is located
    deriving (Ord,Eq,Show)

type WidgetMotionNotifyEventCallback =
    Gdk.Event.Event
    -> IO Bool

-- | Type for the callback on the (unwrapped) C side.
type C_WidgetMotionNotifyEventCallback =
    Ptr () ->                               -- object
    Ptr Gdk.Event.Event ->
    Ptr () ->                               -- user_data
    IO CInt

-- | Generate a function pointer callable from C code, from a `C_WidgetMotionNotifyEventCallback`.
foreign import ccall "wrapper"
    mk_WidgetMotionNotifyEventCallback :: C_WidgetMotionNotifyEventCallback -> IO (FunPtr C_WidgetMotionNotifyEventCallback)

-- | Wrap a `WidgetMotionNotifyEventCallback` into a `C_WidgetMotionNotifyEventCallback`.
wrap_WidgetMotionNotifyEventCallback ::
    WidgetMotionNotifyEventCallback ->
    C_WidgetMotionNotifyEventCallback
wrap_WidgetMotionNotifyEventCallback _cb _ event _ = do
    event' <- newBoxed Gdk.Event.Event event
    result <- _cb  event'
    let result' = (fromIntegral . fromEnum) result
    return result'

onWidgetMotionNotifyEvent :: (IsWidget a, MonadIO m) => a -> WidgetMotionNotifyEventCallback -> m SignalHandlerId
onWidgetMotionNotifyEvent obj cb = liftIO $ do
    let cb' = wrap_WidgetMotionNotifyEventCallback cb
    cb'' <- mk_WidgetMotionNotifyEventCallback cb'
    connectSignalFunPtr obj "motion-notify-event" cb'' SignalConnectBefore Nothing

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

    let moveOrClick mbDevice eventX eventY mods _eventTime mbMotion = do
            mbSeat <- mapM deviceGetSeat mbDevice
            let ungrab = mapM_ seatUngrab mbSeat
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
                if T.length slice > 1 then
                    case mbMotion of
                        Nothing -> do
                            ungrab
                            clickHandler ctrlPressed shiftPressed (beg, en)
                            setTypeTip (0, 0) ""
                            return Nothing
                        Just event -> do
                            widgetGetWindow tv >>= \case
                                Nothing -> return ()
                                Just dw ->
                                    forM_ mbSeat $ \seat ->
                                        seatGrab seat dw [SeatCapabilitiesPointer] False (fromJust mbHand) (Just event) noSeatGrabPrepareFunc
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
    let fixBugWithX _mods isHint (eventX, eventY) ptrx' = do
            Just window <- widgetGetWindow sw
            (_, ox, _) <- windowGetOrigin window
            let ptrx = ptrx' - fromIntegral ox
            -- let hasNoControlModifier = mapControlCommand ModifierTypeControlMask `notElem` mods
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
        motion <- getEventMotion e
        isHint <- (/=0) <$> getEventMotionIsHint motion
        eventTime <- getEventMotionTime motion
        mods <- getEventMotionState motion
        oldX <- getEventMotionX motion
        oldY <- getEventMotionY motion
        rootX <- getEventMotionXRoot motion
        device <- getEventMotionDevice motion
        (eventX, eventY) <- liftIO $ fixBugWithX mods isHint (oldX, oldY) rootX
        -- print ("move adjustment: isHint, old, new root", isHint, eventX, oldX, rootX)
        void . (`reflectIDE` ideR) $ moveOrClick device eventX eventY mods eventTime (Just e)
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

    return [{-id1,-} id2, id3]


