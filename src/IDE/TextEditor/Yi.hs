{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor.Yi
-- Copyright   :  2007-2013 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.TextEditor.Yi (
    Yi(..)

#ifdef LEKSAH_WITH_YI
  , TextEditor(..)
  , EditorBuffer(..)
  , EditorView(..)
  , EditorIter(..)
  , EditorMark(..)
  , EditorTag(..)
  , EditorTagTable(..)

  , newYiBuffer
#endif
) where

import Data.Typeable (Typeable)

#ifdef LEKSAH_WITH_YI
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.GI.Base.BasicTypes (NullToNothing(..))
import GI.Gtk
       (noWidget, getCurrentEventTime, menuPopup, menuAttachToWidget,
        menuNew, onWidgetPopupMenu, onWidgetKeyReleaseEvent,
        onWidgetLeaveNotifyEvent, onWidgetMotionNotifyEvent,
        onWidgetKeyPressEvent, onWidgetButtonReleaseEvent,
        onWidgetButtonPressEvent, widgetAddEvents, afterWidgetFocusInEvent,
        widgetGrabFocus, toWidget, widgetGetWindow)
import GI.Gdk
       (getEventButtonState)
import Graphics.UI.Frame.Rectangle
       (Rectangle(..), newRectangle)
import Data.GI.Base.Attributes (AttrOp(..))
import GI.Pango (layoutSetFontDescription)
import Yi (nelemsB, readAtB, moveB)
import Yi.Buffer.TextUnit (TextUnit(..))
import GI.Gdk.Flags (ModifierType(..), EventMask(..))
import Control.Monad (void)
import qualified Yi.Rope as R (head)
import IDE.Core.Types (MonadIDE(..))
import Data.GI.Base.BasicConversions (gflagsToWord)

import IDE.TextEditor.Class (TextEditor(..))
import IDE.Core.Types (IDEM)
import IDE.Core.State (liftYi, onIDE, reflectIDE, liftYiControl)
import qualified Yi.UI.Pango.Control as Yi
       (getBuffer, setText, newView, getText, newBuffer)
import Yi.UI.Pango.Control
       (Control(..), View(..), iterFBufRef, Iter(..), fBufRef, Buffer(..),
        setBufferMode)
import Yi
       (moveToColB, gotoLn, atSol, atEof, atSof, curCol, curLn, readLnB,
        nextWordB, moveToEol, rightB, rightN,
        unitWord, atBoundaryB, moveToSol, prevWordB, leftB, readB,
        doUntilB_, prevPointB, Mode, modifyMode, insertingA, undoB,
        markSavedB, setSelectRegionB, redoB, markPointA, insertNAt,
        regionIsEmpty, regionEnd, regionStart, selMark, isUnchangedBuffer,
        Point(..), MarkValue(..), lineOf, pointOfLineColB,
        askMarks, insMark, sizeB, getRawestSelectRegionB, mkRegion,
        deleteRegionB, newMarkB, Mark, pointB, moveTo, savingPointB, Point,
        withGivenBuffer, withEditor, BufferM, BufferRef, Mode(..),
        IndentSettings(..), BufferId(..))
import qualified Yi.Rope as Yi (length, fromText)
import Control.Applicative ((<$>))
import Yi.Keymap.Cua (paste, cut, copy)
import Yi.Buffer.Basic (Direction(..))
import Control.Monad.State.Class (gets)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time (getCurrentTime)
import IDE.Utils.GUIUtils (fontDescription)
import Control.Monad.Reader.Class (MonadReader(..))
import Graphics.UI.Editor.Basics (Connection(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Lens (use)
#endif

data Yi = Yi deriving( Typeable, Show )

#ifdef LEKSAH_WITH_YI
newYiBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer Yi)
newYiBuffer mbFilename contents = do
    liftYiControl $ do
        let (filename, id) = case mbFilename of
                                Just fn -> (fn, FileBuffer fn)
                                Nothing -> ("Unknown.hs", MemBuffer "*leksah*")
        buffer <- Yi.newBuffer id $ Yi.fromText contents
        setBufferMode filename buffer
        return $ YiBuffer buffer

withYiBuffer' :: BufferRef -> BufferM a -> IDEM a
withYiBuffer' b f = liftYi $ withEditor $ withGivenBuffer b f

withYiBuffer :: Buffer -> BufferM a -> IDEM a
withYiBuffer b f = withYiBuffer' (fBufRef b) f

mkYiIter' :: BufferRef -> Point -> EditorIter Yi
mkYiIter' b p = YiIter $ Iter b p

mkYiIter :: Buffer -> Point -> EditorIter Yi
mkYiIter b p = mkYiIter' (fBufRef b) p

withYiIter :: Iter -> BufferM a -> IDEM a
withYiIter (Iter b p) f = withYiBuffer' b $ do
    savingPointB $ do
        moveTo p
        f

transformYiIter' :: Iter -> BufferM Point -> IDEM (EditorIter Yi)
transformYiIter' i f = mkYiIter' (iterFBufRef i) <$> withYiIter i f

transformYiIter :: Iter -> BufferM a -> IDEM (EditorIter Yi)
transformYiIter i f = transformYiIter' i (f >> pointB)

tryTransformYiIter' :: Iter -> BufferM Point -> IDEM (Maybe (EditorIter Yi))
tryTransformYiIter' i@(Iter b p) f = withYiIter i $ do
    newPoint <- f
    if p == newPoint
        then return Nothing
        else return . Just $ mkYiIter' b newPoint

tryTransformYiIter :: Iter -> BufferM a -> IDEM (Maybe (EditorIter Yi))
tryTransformYiIter i f = tryTransformYiIter' i (f >> pointB)

iterFromYiBuffer' :: BufferRef -> BufferM Point -> IDEM (EditorIter Yi)
iterFromYiBuffer' b f = mkYiIter' b <$> withYiBuffer' b f

iterFromYiBuffer :: Buffer -> BufferM Point -> IDEM (EditorIter Yi)
iterFromYiBuffer b f = iterFromYiBuffer' (fBufRef b) f

instance TextEditor Yi where
    data EditorBuffer Yi = YiBuffer Buffer
    data EditorView Yi = YiView View
    data EditorMark Yi = YiMark Mark
    data EditorIter Yi = YiIter Iter
    data EditorTagTable Yi = YiTagTable
    data EditorTag Yi = YiTag

    newBuffer = newYiBuffer
    applyTagByName (YiBuffer fb) name (YiIter first) (YiIter last) = return () -- TODO
    beginNotUndoableAction (YiBuffer fb) = return () -- TODO
    beginUserAction (YiBuffer fb) = return () -- TODO
    canRedo (YiBuffer fb) = return True -- TODO
    canUndo (YiBuffer fb) = return True -- TODO
    copyClipboard (YiBuffer fb) _ = liftYi $ withEditor $ copy
    createMark (YiView b) _name (YiIter (Iter _ p)) _toolTip = void . withYiBuffer (Yi.getBuffer b) $
        YiMark <$> newMarkB (MarkValue p Backward)
    cutClipboard (YiBuffer fb) clipboard defaultEditable = liftYi $ withEditor $ cut
    delete (YiBuffer b) (YiIter (Iter _ first)) (YiIter (Iter _ last)) =
        withYiBuffer b $ deleteRegionB $ mkRegion first last
    deleteSelection (YiBuffer b) = withYiBuffer b $ do
        region <- getRawestSelectRegionB
        deleteRegionB region -- TODO support flags
    endNotUndoableAction (YiBuffer fb) = return () -- TODO
    endUserAction (YiBuffer fb) = return () -- TODO
    getEndIter (YiBuffer b) = iterFromYiBuffer b sizeB
    getInsertMark (YiBuffer b) = YiMark <$> (withYiBuffer b $ insMark <$> askMarks)
    getIterAtLine (YiBuffer b) line = iterFromYiBuffer b $ pointOfLineColB line 1
    getIterAtMark (YiBuffer b) (YiMark m) = iterFromYiBuffer b $ (use . markPointA) m
    getIterAtOffset (YiBuffer b) offset = return $ mkYiIter b $ Point offset
    getLineCount (YiBuffer b) = withYiBuffer b $ sizeB >>= lineOf
    getModified (YiBuffer b) = not <$> (withYiBuffer b $ gets isUnchangedBuffer)
    getSelectionBoundMark (YiBuffer b) = YiMark . selMark <$> (withYiBuffer b $ askMarks)
    getSelectionBounds (YiBuffer b) = withYiBuffer b $ do
        region <- getRawestSelectRegionB
        return (mkYiIter b (regionStart region),
                mkYiIter b (regionEnd region))
    getInsertIter (YiBuffer b) = withYiBuffer b $ do
        insertMark <- insMark <$> askMarks
        mkYiIter b <$> (use . markPointA) insertMark
    getSlice (YiBuffer b) (YiIter first) (YiIter last) includeHidenChars = liftYiControl $
        Yi.getText b first last
    getStartIter (YiBuffer b) = return $ mkYiIter b $ Point 0
    getTagTable (YiBuffer b) = return YiTagTable -- TODO
    getText (YiBuffer b) (YiIter first) (YiIter last) includeHidenChars = liftYiControl $
        Yi.getText b first last
    hasSelection (YiBuffer b) = withYiBuffer b $ do
        region <- getRawestSelectRegionB
        return $ not $ regionIsEmpty region
    insert (YiBuffer b) (YiIter (Iter _ p)) text = withYiBuffer b $ insertNAt (Yi.fromText text) p
    newView (YiBuffer b) mbFontString = do
        fd <- fontDescription mbFontString
        liftYiControl $ fmap YiView $ Yi.newView b fd
    pasteClipboard (YiBuffer b) clipboard (YiIter (Iter _ p)) defaultEditable = liftYi $ withEditor $ paste
    placeCursor (YiBuffer b) (YiIter (Iter _ p)) = withYiBuffer b $ moveTo p
    redo (YiBuffer b) = withYiBuffer b redoB
    removeTagByName (YiBuffer b) name = return () -- TODO
    selectRange (YiBuffer b) (YiIter (Iter _ first)) (YiIter (Iter _ last)) = withYiBuffer b $
        setSelectRegionB $ mkRegion first last
    setModified (YiBuffer b) modified = unless modified $ do
        now <- liftIO $ getCurrentTime
        withYiBuffer b $ markSavedB now
    setStyle (YiBuffer b) mbStyle = return () -- TODO
    setText (YiBuffer b) text = liftYiControl $ Yi.setText b (Yi.fromText text)
    undo (YiBuffer b) =  withYiBuffer b undoB
    bufferToWindowCoords (YiView v) point = return point -- TODO
    drawTabs (YiView _) = return () -- TODO
    getBuffer (YiView v) = return $ YiBuffer $ Yi.getBuffer v
    getWindow (YiView v) = nullToNothing $ widgetGetWindow (drawArea v)
    getIterAtLocation (YiView View{viewFBufRef = b}) x y = return $ mkYiIter' b $ Point 0 -- TODO
    getIterLocation (YiView v) (YiIter i) = newRectangle 0 0 0 0 -- TODO
    getOverwrite (YiView View{viewFBufRef = b}) = withYiBuffer' b $ not <$> use insertingA
    getScrolledWindow (YiView v) = return $ scrollWin v
    getEditorWidget (YiView v) = liftIO . toWidget $ drawArea v
    grabFocus (YiView View{drawArea = da}) = widgetGrabFocus da
    scrollToMark (YiView v) (YiMark m) withMargin mbAlign = return () -- TODO
    scrollToIter (YiView v) (YiIter i) withMargin mbAlign = return () -- TODO
    setFont (YiView v) mbFontString = do
        fd <- fontDescription mbFontString
        layoutSetFontDescription (layout v) (Just fd)
    setIndentWidth (YiView View{viewFBufRef = b}) width =
        withYiBuffer' b $ modifyMode $
            \ (mode@Mode{modeIndentSettings = mis}) ->
                mode{modeIndentSettings = mis{shiftWidth = width}}
    setWrapMode (YiView View{viewFBufRef = b}) width = return ()
    setRightMargin (YiView v) mbRightMargin = return () -- TODO
    setShowLineNumbers (YiView v) show = return () -- TODO
    setTabWidth (YiView View{viewFBufRef = b}) width =
        withYiBuffer' b $ modifyMode $
            \ (mode@Mode{modeIndentSettings = mis}) ->
                mode{modeIndentSettings = mis{tabSize = width}}

    backwardCharC (YiIter i) = transformYiIter' i prevPointB
    backwardFindCharC (YiIter i) pred mbLimit = tryTransformYiIter i $
        doUntilB_ (pred <$> readB) leftB
    backwardWordStartC (YiIter i@(Iter b p)) = withYiIter i $ do
        prevWordB
        newPoint <- pointB
        if p == newPoint
            then return Nothing
            else return . Just $ mkYiIter' b newPoint
    backwardToLineStartC (YiIter i) = transformYiIter i moveToSol
    endsWord (YiIter i) = withYiIter i $ do
        atBoundaryB unitWord Forward
    forwardCharC i = forwardCharsC i 1
    forwardCharsC (YiIter i) n = transformYiIter i $ rightN n
    forwardFindCharC (YiIter i) pred mbLimit = tryTransformYiIter i $
        doUntilB_ (pred <$> readB) rightB
    forwardSearch (YiIter i) str pred mbLimit = return Nothing -- TODO
    forwardToLineEndC (YiIter i) = transformYiIter i moveToEol
    forwardWordEndC (YiIter i@(Iter b p)) = withYiIter i $ do
        nextWordB
        newPoint <- pointB
        if p == newPoint
            then return Nothing
            else return . Just $ mkYiIter' b newPoint
    getChar (YiIter (Iter b p)) = withYiBuffer' b $ R.head <$> nelemsB 1 p
    getCharsInLine (YiIter i) = withYiIter i $ Yi.length <$> readLnB
    getLine (YiIter i) = withYiIter i curLn
    getLineOffset (YiIter i) = withYiIter i curCol
    getOffset (YiIter (Iter _ (Point o))) = return o
    isStart (YiIter i) = withYiIter i atSof
    isEnd (YiIter i) = withYiIter i atEof
    iterEqual (YiIter (Iter _ p1)) (YiIter (Iter _ p2)) = return $ p1 == p2
    startsLine (YiIter i) = withYiIter i atSol
    startsWord (YiIter i) = withYiIter i atSol -- TODO
    atEnd (YiIter (Iter b _)) = iterFromYiBuffer' b sizeB
    atLine (YiIter i) line = transformYiIter i $ gotoLn line
    atLineOffset (YiIter i) column = transformYiIter i $ moveToColB column
    atOffset (YiIter (Iter b _)) offset = return $ YiIter $ Iter b (Point offset)
    atStart (YiIter (Iter b _)) = return $ mkYiIter' b $ Point 0
    newTag (YiTagTable) name = return YiTag -- TODO
    lookupTag (YiTagTable) name = return Nothing -- TODO
    background (YiTag) color = return () -- TODO
    underline (YiTag) value = return () -- TODO
    afterFocusIn (YiView v) f = do
        ideR <- ask
        id1 <- onIDE afterWidgetFocusInEvent (drawArea v) $ liftIDE f >> return True
        return [id1]
    afterModifiedChanged (YiBuffer b) f = return [] -- TODO
    afterMoveCursor (YiView v) f = return [] -- TODO
    afterToggleOverwrite (YiView v) f = return [] -- TODO
    onButtonPress (YiView v) f = do
        id1 <- onIDE onWidgetButtonPressEvent (drawArea v) f
        return [id1]
    onButtonRelease (YiView v) f = do
        id1 <- onIDE onWidgetButtonReleaseEvent (drawArea v) f
        return [id1]
    onCompletion (YiView v) start cancel = return [] -- TODO
    onKeyPress (YiView v) f = do
        id1 <- onIDE onWidgetKeyPressEvent (drawArea v) f
        return [id1]
    onMotionNotify (YiView v) f = do
        id1 <- onIDE onWidgetMotionNotifyEvent (drawArea v) f
        return [id1]
    onLeaveNotify (YiView v) f = do
        id1 <- onIDE onWidgetLeaveNotifyEvent (drawArea v) f
        return [id1]
    onKeyRelease (YiView v) f = do
        id1 <- onIDE onWidgetKeyReleaseEvent (drawArea v) f
        return [id1]
    onLookupInfo (YiView v) f = do
        widgetAddEvents (drawArea v) (gflagsToWord [EventMaskButtonReleaseMask])
        id1 <- onIDE onWidgetButtonReleaseEvent (drawArea v) $ do
            e <- lift ask
            mod <- getEventButtonState e
            case mod of
                [ModifierTypeControlMask] -> f >> return True
                _                         -> return False
        return [id1]
    onMotionNotifyEvent (YiView v) f = return [] -- TODO
    onPopulatePopup (YiView v) f = do
        ideR <- ask
        liftIO $ do
            id1 <- ConnectC (drawArea v) <$> onWidgetPopupMenu (drawArea v) (do
                 menu <- menuNew
                 menuAttachToWidget menu (drawArea v) Nothing
                 reflectIDE (f menu) ideR
                 menuPopup menu noWidget noWidget Nothing 0 =<< getCurrentEventTime
                 return True)
            return [id1]
    onSelectionChanged (YiBuffer b) handler = return [] -- TODO

#endif

