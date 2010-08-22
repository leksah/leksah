-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.TextEditor (

-- Data Types
    EditorBuffer(..)
,   EditorView(..)
,   EditorMark(..)
,   EditorIter(..)
,   EditorTagTable(..)
,   EditorTag(..)

-- Buffer
,   newGtkBuffer
,   newYiBuffer
,   simpleGtkBuffer
,   applyTagByName
,   beginNotUndoableAction
,   beginUserAction
,   canRedo
,   canUndo
,   copyClipboard
,   createMark
,   cutClipboard
,   delete
,   deleteSelection
,   endNotUndoableAction
,   endUserAction
,   getEndIter
,   getInsertMark
,   getIterAtLine
,   getIterAtMark
,   getIterAtOffset
,   getLineCount
,   getModified
,   getSelectionBoundMark
,   getSelectionBounds
,   getSlice
,   getStartIter
,   getTagTable
,   getText
,   hasSelection
,   insert
,   moveMark
,   newView
,   pasteClipboard
,   placeCursor
,   redo
,   removeTagByName
,   selectRange
,   setModified
,   setStyle
,   setText
,   undo

-- View
,   bufferToWindowCoords
,   getBuffer
,   getDrawWindow
,   getIterLocation
,   getOverwrite
,   getScrolledWindow
,   grabFocus
,   scrollToMark
,   scrollToIter
,   setFont
,   setIndentWidth
,   setRightMargin
,   setShowLineNumbers
,   setTabWidth

-- Iterator
,   backwardCharC
,   backwardFindCharC
,   backwardWordStartC
,   backwardToLineStartC
,   endsWord
,   forwardCharC
,   forwardCharsC
,   forwardFindCharC
,   forwardToLineEndC
,   forwardWordEndC
,   forwardSearch
,   getChar
,   getCharsInLine
,   getLine
,   getLineOffset
,   getOffset
,   isStart
,   isEnd
,   iterEqual
,   startsLine
,   atEnd
,   atLine
,   atLineOffset
,   atOffset
,   atStart

-- Tag Table
,   newTag
,   lookupTag

-- Tag
,   background
,   underline

-- Events
,   afterFocusIn
,   afterModifiedChanged
,   afterMoveCursor
,   afterToggleOverwrite
,   onButtonPress
,   onButtonRelease
,   onCompletion
,   onKeyPress
,   onKeyRelease
,   onLookupInfo
,   onPopulatePopup
) where

import Prelude hiding(getChar, getLine)
import Data.Char (isAlphaNum, isSymbol)
import Data.Maybe (fromJust)
import Control.Monad (when)
import Control.Monad.Reader (liftIO, ask)
import Control.Applicative ((<$>))

import qualified Graphics.UI.Gtk as Gtk hiding(afterToggleOverwrite)
import qualified Graphics.UI.Gtk.SourceView as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as GtkOld
import System.Glib.Attributes (AttrOp(..))

#ifdef LEKSAH_WITH_YI
import qualified Yi as Yi hiding(withBuffer)
import qualified Yi.UI.Pango.Control as Yi
import qualified Yi.Keymap.Cua as Yi
import Data.Time (getCurrentTime)
import Control.Monad (unless)
#endif

import IDE.Core.State
import IDE.Utils.GUIUtils(controlIsPressed)



-- Data types
data EditorBuffer = GtkEditorBuffer Gtk.SourceBuffer
#ifdef LEKSAH_WITH_YI
    | YiEditorBuffer Yi.Buffer
#endif

data EditorView = GtkEditorView Gtk.SourceView
#ifdef LEKSAH_WITH_YI
    | YiEditorView Yi.View
#endif

data EditorMark = GtkEditorMark Gtk.TextMark
#ifdef LEKSAH_WITH_YI
    | YiEditorMark Yi.Mark
#endif

data EditorIter = GtkEditorIter Gtk.TextIter
#ifdef LEKSAH_WITH_YI
    | YiEditorIter Yi.Iter
#endif

data EditorTagTable = GtkEditorTagTable Gtk.TextTagTable
#ifdef LEKSAH_WITH_YI
    | YiEditorTagTable
#endif

data EditorTag = GtkEditorTag Gtk.TextTag
#ifdef LEKSAH_WITH_YI
    | YiEditorTag
#endif

#ifdef LEKSAH_WITH_YI
withYiBuffer' :: Yi.BufferRef -> Yi.BufferM a -> IDEM a
withYiBuffer' b f = liftYi $ Yi.liftEditor $ Yi.withGivenBuffer0 b f

withYiBuffer :: Yi.Buffer -> Yi.BufferM a -> IDEM a
withYiBuffer b f = withYiBuffer' (Yi.fBufRef b) f

mkYiIter' :: Yi.BufferRef -> Yi.Point -> EditorIter
mkYiIter' b p = YiEditorIter $ Yi.Iter b p

mkYiIter :: Yi.Buffer -> Yi.Point -> EditorIter
mkYiIter b p = mkYiIter' (Yi.fBufRef b) p

iterFromYiBuffer' :: Yi.BufferRef -> Yi.BufferM Yi.Point -> IDEM EditorIter
iterFromYiBuffer' b f = mkYiIter' b <$> withYiBuffer' b f

iterFromYiBuffer :: Yi.Buffer -> Yi.BufferM Yi.Point -> IDEM EditorIter
iterFromYiBuffer b f = iterFromYiBuffer' (Yi.fBufRef b) f
#endif

-- Buffer
newGtkBuffer :: Maybe FilePath -> String -> IDEM EditorBuffer
newGtkBuffer mbFilename contents = liftIO $ do
    lm     <- Gtk.sourceLanguageManagerNew
    mbLang <- case mbFilename of
        Just filename -> Gtk.sourceLanguageManagerGuessLanguage lm (Just filename) Nothing
        Nothing       -> Gtk.sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
    buffer <- case mbLang of
        Just sLang -> Gtk.sourceBufferNewWithLanguage sLang
        Nothing -> Gtk.sourceBufferNew Nothing
    Gtk.sourceBufferSetMaxUndoLevels buffer (-1)
    Gtk.sourceBufferBeginNotUndoableAction buffer
    Gtk.textBufferSetText buffer contents
    Gtk.sourceBufferEndNotUndoableAction buffer
    return $ GtkEditorBuffer buffer

-- Buffer
simpleGtkBuffer :: String -> IDEM EditorBuffer
simpleGtkBuffer contents = liftIO $ do
    buffer <- Gtk.sourceBufferNew Nothing
    Gtk.textBufferSetText buffer contents
    return $ GtkEditorBuffer buffer


newYiBuffer :: Maybe FilePath -> String -> IDEM EditorBuffer
#ifdef LEKSAH_WITH_YI
newYiBuffer mbFilename contents = liftYiControl $ do
    let (filename, id) = case mbFilename of
                            Just fn -> (fn, Right fn)
                            Nothing -> ("Unknown.hs", Left "*leksah*")
    buffer <- Yi.newBuffer id contents
    Yi.setBufferMode filename buffer
    return $ YiEditorBuffer buffer
#else
newYiBuffer = newGtkBuffer
#endif

applyTagByName :: EditorBuffer
                  -> String
                  -> EditorIter
                  -> EditorIter
                  -> IDEM ()
applyTagByName (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferApplyTagByName sb name first last
#ifdef LEKSAH_WITH_YI
applyTagByName (YiEditorBuffer fb) name (YiEditorIter first) (YiEditorIter last) = return () -- TODO
applyTagByName _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in createMark"
#endif

beginNotUndoableAction :: EditorBuffer -> IDEM ()
beginNotUndoableAction (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferBeginNotUndoableAction sb
#ifdef LEKSAH_WITH_YI
beginNotUndoableAction (YiEditorBuffer fb) = return () -- TODO
#endif

beginUserAction :: EditorBuffer -> IDEM ()
beginUserAction (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferBeginUserAction sb
#ifdef LEKSAH_WITH_YI
beginUserAction (YiEditorBuffer fb) = return () -- TODO
#endif

canRedo :: EditorBuffer -> IDEM Bool
canRedo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferGetCanRedo sb
#ifdef LEKSAH_WITH_YI
canRedo (YiEditorBuffer fb) = return True -- TODO
#endif

canUndo :: EditorBuffer -> IDEM Bool
canUndo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferGetCanUndo sb
#ifdef LEKSAH_WITH_YI
canUndo (YiEditorBuffer fb) = return True -- TODO
#endif

copyClipboard :: EditorBuffer -> Gtk.Clipboard -> IDEM ()
copyClipboard (GtkEditorBuffer sb) clipboard = liftIO $ Gtk.textBufferCopyClipboard sb clipboard
#ifdef LEKSAH_WITH_YI
copyClipboard (YiEditorBuffer fb) _ = liftYi $ Yi.liftEditor $ Yi.copy
#endif

createMark :: EditorBuffer
              -> EditorIter
              -> Bool
              -> IDEM EditorMark
createMark (GtkEditorBuffer sb) (GtkEditorIter i) leftGravity = liftIO $
    GtkEditorMark <$> Gtk.textBufferCreateMark sb Nothing i leftGravity
#ifdef LEKSAH_WITH_YI
createMark (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ p)) leftGravity = withYiBuffer b $
    YiEditorMark <$> Yi.newMarkB (Yi.MarkValue p (if leftGravity then Yi.Backward else Yi.Forward))
createMark _ _ _ = liftIO $ fail "Mismatching TextEditor types in createMark"
#endif

cutClipboard :: EditorBuffer -> Gtk.Clipboard -> Bool -> IDEM ()
cutClipboard (GtkEditorBuffer sb) clipboard defaultEditable = liftIO $ Gtk.textBufferCutClipboard sb clipboard defaultEditable
#ifdef LEKSAH_WITH_YI
cutClipboard (YiEditorBuffer fb) clipboard defaultEditable = liftYi $ Yi.liftEditor $ Yi.cut
#endif

delete :: EditorBuffer -> EditorIter -> EditorIter -> IDEM ()
delete (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferDelete sb first last
#ifdef LEKSAH_WITH_YI
delete (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ first)) (YiEditorIter (Yi.Iter _ last)) =
    withYiBuffer b $ Yi.deleteRegionB $ Yi.mkRegion first last
delete _ _ _ = liftIO $ fail "Mismatching TextEditor types in delete"
#endif

deleteSelection :: EditorBuffer -> Bool -> Bool -> IDEM ()
deleteSelection (GtkEditorBuffer sb) interactive defaultEditable = liftIO $
    Gtk.textBufferDeleteSelection sb interactive defaultEditable >> return ()
#ifdef LEKSAH_WITH_YI
deleteSelection (YiEditorBuffer b) interactive defaultEditable = withYiBuffer b $ do
    region <- Yi.getRawestSelectRegionB
    Yi.deleteRegionB region -- TODO support flags
#endif

endNotUndoableAction :: EditorBuffer -> IDEM ()
endNotUndoableAction (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferEndNotUndoableAction sb
#ifdef LEKSAH_WITH_YI
endNotUndoableAction (YiEditorBuffer fb) = return () -- TODO
#endif

endUserAction :: EditorBuffer -> IDEM ()
endUserAction (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferEndUserAction sb
#ifdef LEKSAH_WITH_YI
endUserAction (YiEditorBuffer fb) = return () -- TODO
#endif

getEndIter :: EditorBuffer -> IDEM EditorIter
getEndIter (GtkEditorBuffer sb) = liftIO $ GtkEditorIter <$> Gtk.textBufferGetEndIter sb
#ifdef LEKSAH_WITH_YI
getEndIter (YiEditorBuffer b) = iterFromYiBuffer b Yi.sizeB
#endif

getInsertMark :: EditorBuffer -> IDEM EditorMark
getInsertMark (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorMark $ Gtk.textBufferGetInsert sb
#ifdef LEKSAH_WITH_YI
getInsertMark (YiEditorBuffer b) = YiEditorMark <$> (withYiBuffer b $ Yi.insMark <$> Yi.askMarks)
#endif

getIterAtLine :: EditorBuffer -> Int -> IDEM EditorIter
getIterAtLine (GtkEditorBuffer sb) line = liftIO $ GtkEditorIter <$> Gtk.textBufferGetIterAtLine sb line
#ifdef LEKSAH_WITH_YI
getIterAtLine (YiEditorBuffer b) line = iterFromYiBuffer b $ Yi.pointOfLineColB line 1
#endif

getIterAtMark :: EditorBuffer -> EditorMark -> IDEM EditorIter
getIterAtMark (GtkEditorBuffer sb) (GtkEditorMark m) = liftIO $ GtkEditorIter <$> Gtk.textBufferGetIterAtMark sb m
#ifdef LEKSAH_WITH_YI
getIterAtMark (YiEditorBuffer b) (YiEditorMark m) = iterFromYiBuffer b $ Yi.getMarkPointB m
getIterAtMark _ _ = liftIO $ fail "Mismatching TextEditor types in getIterAtMark"
#endif

getIterAtOffset :: EditorBuffer -> Int -> IDEM EditorIter
getIterAtOffset (GtkEditorBuffer sb) offset = liftIO $ GtkEditorIter <$> Gtk.textBufferGetIterAtOffset sb offset
#ifdef LEKSAH_WITH_YI
getIterAtOffset (YiEditorBuffer b) offset = return $ mkYiIter b $ Yi.Point offset
#endif

getLineCount :: EditorBuffer -> IDEM Int
getLineCount (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferGetLineCount sb
#ifdef LEKSAH_WITH_YI
getLineCount (YiEditorBuffer b) = withYiBuffer b $ Yi.sizeB >>= Yi.lineOf
#endif

getModified :: EditorBuffer -> IDEM Bool
getModified (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferGetModified sb
#ifdef LEKSAH_WITH_YI
getModified (YiEditorBuffer b) = not <$> (withYiBuffer b $ Yi.gets Yi.isUnchangedBuffer)
#endif

getSelectionBoundMark :: EditorBuffer -> IDEM EditorMark
getSelectionBoundMark (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorMark $ Gtk.textBufferGetSelectionBound sb
#ifdef LEKSAH_WITH_YI
getSelectionBoundMark (YiEditorBuffer b) = YiEditorMark . Yi.selMark <$> (withYiBuffer b $ Yi.askMarks)
#endif

getSelectionBounds :: EditorBuffer -> IDEM (EditorIter, EditorIter)
getSelectionBounds (GtkEditorBuffer sb) = liftIO $ do
    (first, last) <- Gtk.textBufferGetSelectionBounds sb
    return (GtkEditorIter first, GtkEditorIter last)
#ifdef LEKSAH_WITH_YI
getSelectionBounds (YiEditorBuffer b) = withYiBuffer b $ do
    region <- Yi.getRawestSelectRegionB
    return (mkYiIter b (Yi.regionStart region),
            mkYiIter b (Yi.regionEnd region))
#endif

getSlice :: EditorBuffer
            -> EditorIter
            -> EditorIter
            -> Bool
            -> IDEM String
getSlice (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars = liftIO $
    Gtk.textBufferGetSlice sb first last includeHidenChars
#ifdef LEKSAH_WITH_YI
getSlice (YiEditorBuffer b) (YiEditorIter first) (YiEditorIter last) includeHidenChars = liftYiControl $
    Yi.getText b first last
getSlice _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in getSlice"
#endif

getStartIter :: EditorBuffer -> IDEM EditorIter
getStartIter (GtkEditorBuffer sb) = liftIO $ GtkEditorIter <$> Gtk.textBufferGetStartIter sb
#ifdef LEKSAH_WITH_YI
getStartIter (YiEditorBuffer b) = return $ mkYiIter b $ Yi.Point 0
#endif

getTagTable :: EditorBuffer -> IDEM EditorTagTable
getTagTable (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorTagTable $ Gtk.textBufferGetTagTable sb
#ifdef LEKSAH_WITH_YI
getTagTable (YiEditorBuffer b) = return YiEditorTagTable -- TODO
#endif

getText :: EditorBuffer
           -> EditorIter
           -> EditorIter
           -> Bool
           -> IDEM String
getText (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars = liftIO $
    Gtk.textBufferGetText sb first last includeHidenChars
#ifdef LEKSAH_WITH_YI
getText (YiEditorBuffer b) (YiEditorIter first) (YiEditorIter last) includeHidenChars = liftYiControl $
    Yi.getText b first last
getText _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in getText"
#endif

hasSelection :: EditorBuffer -> IDEM Bool
hasSelection (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferHasSelection sb
#ifdef LEKSAH_WITH_YI
hasSelection (YiEditorBuffer b) = withYiBuffer b $ do
    region <- Yi.getRawestSelectRegionB
    return $ not $ Yi.regionIsEmpty region
#endif

insert :: EditorBuffer -> EditorIter -> String -> IDEM ()
insert (GtkEditorBuffer sb) (GtkEditorIter i) text = liftIO $ Gtk.textBufferInsert sb i text
#ifdef LEKSAH_WITH_YI
insert (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ p)) text = withYiBuffer b $ Yi.insertNAt text p
insert _ _ _ = liftIO $ fail "Mismatching TextEditor types in insert"
#endif

moveMark :: EditorBuffer -> EditorMark -> EditorIter -> IDEM ()
moveMark (GtkEditorBuffer sb) (GtkEditorMark m) (GtkEditorIter i) = liftIO $ Gtk.textBufferMoveMark sb m i
#ifdef LEKSAH_WITH_YI
moveMark (YiEditorBuffer b) (YiEditorMark m) (YiEditorIter (Yi.Iter _ p)) = withYiBuffer b $ Yi.setMarkPointB m p
moveMark _ _ _ = liftIO $ fail "Mismatching TextEditor types in moveMark"
#endif

newView :: EditorBuffer -> Maybe String -> IDEM EditorView
newView (GtkEditorBuffer sb) mbFontString = do
    fd <- fontDescription mbFontString
    liftIO $ do
        sv <- Gtk.sourceViewNewWithBuffer sb
        Gtk.sourceViewSetHighlightCurrentLine sv True
        Gtk.sourceViewSetInsertSpacesInsteadOfTabs sv True
        Gtk.sourceViewSetIndentOnTab sv True
        Gtk.sourceViewSetAutoIndent sv True
        Gtk.sourceViewSetSmartHomeEnd sv Gtk.SourceSmartHomeEndBefore
        -- TODO make this a configuration setting
        -- Gtk.textViewSetWrapMode sv Gtk.WrapChar
        sw <- Gtk.scrolledWindowNew Nothing Nothing
        Gtk.containerAdd sw sv
        Gtk.widgetModifyFont sv (Just fd)
        return (GtkEditorView sv)
#ifdef LEKSAH_WITH_YI
newView (YiEditorBuffer b) mbFontString = do
    fd <- fontDescription mbFontString
    liftYiControl $ fmap YiEditorView $ Yi.newView b fd
#endif

pasteClipboard :: EditorBuffer
                  -> Gtk.Clipboard
                  -> EditorIter
                  -> Bool
                  -> IDEM ()
pasteClipboard (GtkEditorBuffer sb) clipboard (GtkEditorIter i) defaultEditable = liftIO $
    Gtk.textBufferPasteClipboard sb clipboard i defaultEditable
#ifdef LEKSAH_WITH_YI
pasteClipboard (YiEditorBuffer b) clipboard (YiEditorIter (Yi.Iter _ p)) defaultEditable = liftYi $ Yi.liftEditor $ Yi.paste
pasteClipboard _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in pasteClipboard"
#endif

placeCursor :: EditorBuffer -> EditorIter -> IDEM ()
placeCursor (GtkEditorBuffer sb) (GtkEditorIter i) = liftIO $ Gtk.textBufferPlaceCursor sb i
#ifdef LEKSAH_WITH_YI
placeCursor (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ p)) = withYiBuffer b $ Yi.moveTo p
placeCursor _ _ = liftIO $ fail "Mismatching TextEditor types in placeCursor"
#endif

redo :: EditorBuffer -> IDEM ()
redo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferRedo sb
#ifdef LEKSAH_WITH_YI
redo (YiEditorBuffer b) = withYiBuffer b Yi.redoB
#endif

removeTagByName :: EditorBuffer
                   -> String
                   -> EditorIter
                   -> EditorIter
                   -> IDEM ()
removeTagByName (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferRemoveTagByName sb name first last
#ifdef LEKSAH_WITH_YI
removeTagByName (YiEditorBuffer b) name (YiEditorIter (Yi.Iter _ first)) (YiEditorIter (Yi.Iter _ last)) = return () -- TODO
removeTagByName _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in removeTagByName"
#endif

selectRange :: EditorBuffer -> EditorIter -> EditorIter -> IDEM ()
selectRange (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferSelectRange sb first last
#ifdef LEKSAH_WITH_YI
selectRange (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ first)) (YiEditorIter (Yi.Iter _ last)) = withYiBuffer b $
    Yi.setSelectRegionB $ Yi.mkRegion first last
selectRange _ _ _ = liftIO $ fail "Mismatching TextEditor types in selectRange"
#endif

setModified :: EditorBuffer -> Bool -> IDEM ()
setModified (GtkEditorBuffer sb) modified = liftIO $ Gtk.textBufferSetModified sb modified >> return ()
#ifdef LEKSAH_WITH_YI
setModified (YiEditorBuffer b) modified = unless modified $ do
    now <- liftIO $ getCurrentTime
    withYiBuffer b $ Yi.markSavedB now
#endif

setStyle :: EditorBuffer -> Maybe String -> IDEM ()
setStyle (GtkEditorBuffer sb) mbStyle = liftIO $ do
    case mbStyle of
        Nothing  -> return ()
        Just str -> do
            styleManager <- Gtk.sourceStyleSchemeManagerNew
            ids <- Gtk.sourceStyleSchemeManagerGetSchemeIds styleManager
            when (elem str ids) $ do
                scheme <- Gtk.sourceStyleSchemeManagerGetScheme styleManager str
                Gtk.sourceBufferSetStyleScheme sb scheme
#ifdef LEKSAH_WITH_YI
setStyle (YiEditorBuffer b) mbStyle = return () -- TODO
#endif

setText :: EditorBuffer -> String -> IDEM ()
setText (GtkEditorBuffer sb) text = liftIO $ Gtk.textBufferSetText sb text
#ifdef LEKSAH_WITH_YI
setText (YiEditorBuffer b) text = liftYiControl $ Yi.setText b text
#endif

undo :: EditorBuffer -> IDEM ()
undo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferUndo sb
#ifdef LEKSAH_WITH_YI
undo (YiEditorBuffer b) =  withYiBuffer b Yi.undoB
#endif

-- View
bufferToWindowCoords :: EditorView -> (Int, Int) -> IDEM (Int, Int)
bufferToWindowCoords (GtkEditorView sv) point = liftIO $ Gtk.textViewBufferToWindowCoords sv Gtk.TextWindowWidget point
#ifdef LEKSAH_WITH_YI
bufferToWindowCoords (YiEditorView v) point = return point -- TODO
#endif

getBuffer :: EditorView -> IDEM EditorBuffer
getBuffer (GtkEditorView sv) = liftIO $ fmap (GtkEditorBuffer . Gtk.castToSourceBuffer) $ sv `Gtk.get` Gtk.textViewBuffer
#ifdef LEKSAH_WITH_YI
getBuffer (YiEditorView v) = return $ YiEditorBuffer $ Yi.getBuffer v
#endif

getDrawWindow :: EditorView -> IDEM Gtk.DrawWindow
getDrawWindow (GtkEditorView sv) = liftIO $ Gtk.widgetGetDrawWindow sv
#ifdef LEKSAH_WITH_YI
getDrawWindow (YiEditorView v) = liftIO $ Gtk.widgetGetDrawWindow (Yi.drawArea v)
#endif

getIterLocation :: EditorView -> EditorIter -> IDEM Gtk.Rectangle
getIterLocation (GtkEditorView sv) (GtkEditorIter i) = liftIO $ Gtk.textViewGetIterLocation sv i
#ifdef LEKSAH_WITH_YI
getIterLocation (YiEditorView v) (YiEditorIter i) = return $ Gtk.Rectangle 0 0 0 0 -- TODO
getIterLocation _ _ = liftIO $ fail "Mismatching TextEditor types in getIterLocation"
#endif

getOverwrite :: EditorView -> IDEM Bool
getOverwrite (GtkEditorView sv) = liftIO $ Gtk.textViewGetOverwrite sv
#ifdef LEKSAH_WITH_YI
getOverwrite (YiEditorView Yi.View{Yi.viewFBufRef = b}) = withYiBuffer' b $ not <$> Yi.getA Yi.insertingA
#endif

getScrolledWindow :: EditorView -> IDEM Gtk.ScrolledWindow
getScrolledWindow (GtkEditorView sv) = liftIO $ fmap (Gtk.castToScrolledWindow . fromJust) $ Gtk.widgetGetParent sv
#ifdef LEKSAH_WITH_YI
getScrolledWindow (YiEditorView v) = return $ Yi.scrollWin v
#endif

grabFocus :: EditorView -> IDEM ()
grabFocus (GtkEditorView sv) = liftIO $ Gtk.widgetGrabFocus sv
#ifdef LEKSAH_WITH_YI
grabFocus (YiEditorView Yi.View{Yi.drawArea = da}) = liftIO $ Gtk.widgetGrabFocus da
#endif

scrollToMark :: EditorView
                -> EditorMark
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
scrollToMark (GtkEditorView sv) (GtkEditorMark m) withMargin mbAlign = liftIO $ Gtk.textViewScrollToMark sv m withMargin mbAlign
#ifdef LEKSAH_WITH_YI
scrollToMark (YiEditorView v) (YiEditorMark m) withMargin mbAlign = return () -- TODO
scrollToMark _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in scrollToMark"
#endif

scrollToIter :: EditorView
                -> EditorIter
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
scrollToIter (GtkEditorView sv) (GtkEditorIter i) withMargin mbAlign = liftIO $ Gtk.textViewScrollToIter sv i withMargin mbAlign >> return ()
#ifdef LEKSAH_WITH_YI
scrollToIter (YiEditorView v) (YiEditorIter i) withMargin mbAlign = return () -- TODO
scrollToIter _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in scrollToIter"
#endif

fontDescription :: Maybe String -> IDEM Gtk.FontDescription
fontDescription mbFontString = liftIO $ do
    case mbFontString of
        Just str -> do
            Gtk.fontDescriptionFromString str
        Nothing -> do
            f <- Gtk.fontDescriptionNew
            Gtk.fontDescriptionSetFamily f "Monospace"
            return f

setFont :: EditorView -> Maybe String -> IDEM ()
setFont (GtkEditorView sv) mbFontString = do
    fd <- fontDescription mbFontString
    liftIO $ Gtk.widgetModifyFont sv (Just fd)
#ifdef LEKSAH_WITH_YI
setFont (YiEditorView v) mbFontString = do
    fd <- fontDescription mbFontString
    liftIO $ Gtk.layoutSetFontDescription (Yi.layout v) (Just fd)
#endif

setIndentWidth :: EditorView -> Int -> IDEM ()
setIndentWidth (GtkEditorView sv) width = liftIO $ Gtk.sourceViewSetIndentWidth sv width
#ifdef LEKSAH_WITH_YI
setIndentWidth (YiEditorView Yi.View{Yi.viewFBufRef = b}) width =
    withYiBuffer' b $ Yi.modifyMode $
        \ (mode@Yi.Mode{Yi.modeIndentSettings = mis}) ->
            mode{Yi.modeIndentSettings = mis{Yi.shiftWidth = width}}
#endif

setRightMargin :: EditorView -> Maybe Int -> IDEM ()
setRightMargin (GtkEditorView sv) mbRightMargin = liftIO $ do
    case mbRightMargin of
        Just n -> do
            Gtk.sourceViewSetShowRightMargin sv True
            Gtk.sourceViewSetRightMarginPosition sv (fromIntegral n)
        Nothing -> Gtk.sourceViewSetShowRightMargin sv False
#ifdef LEKSAH_WITH_YI
setRightMargin (YiEditorView v) mbRightMargin = return () -- TODO
#endif

setShowLineNumbers :: EditorView -> Bool -> IDEM ()
setShowLineNumbers (GtkEditorView sv) show = liftIO $ Gtk.sourceViewSetShowLineNumbers sv show
#ifdef LEKSAH_WITH_YI
setShowLineNumbers (YiEditorView v) show = return () -- TODO
#endif

setTabWidth :: EditorView -> Int -> IDEM ()
setTabWidth (GtkEditorView sv) width = liftIO $ Gtk.sourceViewSetTabWidth sv width
#ifdef LEKSAH_WITH_YI
setTabWidth (YiEditorView Yi.View{Yi.viewFBufRef = b}) width =
    withYiBuffer' b $ Yi.modifyMode $
        \ (mode@Yi.Mode{Yi.modeIndentSettings = mis}) ->
            mode{Yi.modeIndentSettings = mis{Yi.tabSize = width}}
#endif

-- Iterator
transformGtkIter :: Gtk.TextIter -> (Gtk.TextIter -> IO a) -> IDEM EditorIter
transformGtkIter i f = do
    new <- liftIO $ Gtk.textIterCopy i
    liftIO $ f new
    return $ GtkEditorIter new

transformGtkIterMaybe :: Gtk.TextIter -> (Gtk.TextIter -> IO Bool) -> IDEM (Maybe EditorIter)
transformGtkIterMaybe i f = do
    new <- liftIO $ Gtk.textIterCopy i
    found <- liftIO $ f new
    return $ if found
        then Just $ GtkEditorIter new
        else Nothing

#ifdef LEKSAH_WITH_YI
withYiIter :: Yi.Iter -> Yi.BufferM a -> IDEM a
withYiIter (Yi.Iter b p) f = withYiBuffer' b $ do
    Yi.savingPointB $ do
        Yi.moveTo p
        f

transformYiIter' :: Yi.Iter -> Yi.BufferM Yi.Point -> IDEM EditorIter
transformYiIter' i f = mkYiIter' (Yi.iterFBufRef i) <$> withYiIter i f

transformYiIter :: Yi.Iter -> Yi.BufferM a -> IDEM EditorIter
transformYiIter i f = transformYiIter' i (f >> Yi.pointB)

tryTransformYiIter' :: Yi.Iter -> Yi.BufferM Yi.Point -> IDEM (Maybe EditorIter)
tryTransformYiIter' i@(Yi.Iter b p) f = withYiIter i $ do
    newPoint <- f
    if p == newPoint
        then return Nothing
        else return . Just $ mkYiIter' b newPoint

tryTransformYiIter :: Yi.Iter -> Yi.BufferM a -> IDEM (Maybe EditorIter)
tryTransformYiIter i f = tryTransformYiIter' i (f >> Yi.pointB)
#endif

backwardCharC :: EditorIter -> IDEM EditorIter
backwardCharC (GtkEditorIter i) = transformGtkIter i Gtk.textIterBackwardChar
#ifdef LEKSAH_WITH_YI
backwardCharC (YiEditorIter i) = transformYiIter' i Yi.prevPointB
#endif

backwardFindCharC :: EditorIter
                    -> (Char -> Bool)
                    -> Maybe EditorIter
                    -> IDEM (Maybe EditorIter)
backwardFindCharC (GtkEditorIter i) pred mbLimit = transformGtkIterMaybe i $ \x ->
    Gtk.textIterBackwardFindChar x pred $
        case mbLimit of
            Just (GtkEditorIter limit) -> Just limit
            Nothing                    -> Nothing
#ifdef LEKSAH_WITH_YI
            _                          -> fail "Mismatching TextEditor types in backwardFindChar"
#endif

#ifdef LEKSAH_WITH_YI
backwardFindCharC (YiEditorIter i) pred mbLimit = tryTransformYiIter i $
    Yi.doUntilB_ (pred <$> Yi.readB) Yi.leftB
#endif

backwardWordStartC :: EditorIter -> IDEM (Maybe EditorIter)
backwardWordStartC (GtkEditorIter i) = transformGtkIterMaybe i Gtk.textIterBackwardWordStart
#ifdef LEKSAH_WITH_YI
backwardWordStartC (YiEditorIter i@(Yi.Iter b p)) = withYiIter i $ do
    Yi.prevWordB
    newPoint <- Yi.pointB
    if p == newPoint
        then return Nothing
        else return . Just $ mkYiIter' b newPoint
#endif

backwardToLineStartC :: EditorIter -> IDEM EditorIter
backwardToLineStartC (GtkEditorIter i) = transformGtkIter i $ \new -> do
    n <- Gtk.textIterGetLineOffset new
    Gtk.textIterBackwardChars new n
    return ()
#ifdef LEKSAH_WITH_YI
backwardToLineStartC (YiEditorIter i) = transformYiIter i Yi.moveToSol
#endif

endsWord :: EditorIter -> IDEM Bool
endsWord (GtkEditorIter i) = liftIO $ Gtk.textIterEndsWord i
#ifdef LEKSAH_WITH_YI
endsWord (YiEditorIter i) = withYiIter i $ do
    Yi.atBoundaryB Yi.unitWord Yi.Forward
#endif

forwardCharC :: EditorIter -> IDEM EditorIter
forwardCharC (GtkEditorIter i) = transformGtkIter i Gtk.textIterForwardChar
#ifdef LEKSAH_WITH_YI
forwardCharC (YiEditorIter i) = transformYiIter' i Yi.nextPointB
#endif

forwardCharsC :: EditorIter -> Int -> IDEM EditorIter
forwardCharsC (GtkEditorIter i) n = transformGtkIter i $ flip Gtk.textIterForwardChars n
#ifdef LEKSAH_WITH_YI
forwardCharsC (YiEditorIter i) n = transformYiIter i $ Yi.rightN n
#endif

forwardFindCharC :: EditorIter
                   -> (Char -> Bool)
                   -> Maybe EditorIter
                   -> IDEM (Maybe EditorIter)
forwardFindCharC (GtkEditorIter i) pred mbLimit = transformGtkIterMaybe i $ \x ->
    Gtk.textIterForwardFindChar x pred $
        case mbLimit of
            Just (GtkEditorIter limit) -> Just limit
            Nothing                    -> Nothing
#ifdef LEKSAH_WITH_YI
            _                          -> fail "Mismatching TextEditor types in forwardFindChar"
#endif

#ifdef LEKSAH_WITH_YI
forwardFindCharC (YiEditorIter i) pred mbLimit = tryTransformYiIter i $
    Yi.doUntilB_ (pred <$> Yi.readB) Yi.rightB
#endif

forwardSearch :: EditorIter
                 -> String
                 -> [Gtk.TextSearchFlags]
                 -> Maybe EditorIter
                 -> IDEM (Maybe (EditorIter, EditorIter))
forwardSearch (GtkEditorIter i) str flags mbLimit = liftIO $ do
    fmap (fmap (\(start, end) -> (GtkEditorIter start, GtkEditorIter end))) $
        Gtk.textIterForwardSearch i str flags $
            case mbLimit of
                Just (GtkEditorIter limit) -> Just limit
                Nothing                    -> Nothing
#ifdef LEKSAH_WITH_YI
                _                          -> fail "Mismatching TextEditor types in forwardSearch"
#endif

#ifdef LEKSAH_WITH_YI
forwardSearch (YiEditorIter i) str pred mbLimit = return Nothing -- TODO
#endif

forwardToLineEndC :: EditorIter -> IDEM EditorIter
forwardToLineEndC (GtkEditorIter i) = transformGtkIter i Gtk.textIterForwardToLineEnd
#ifdef LEKSAH_WITH_YI
forwardToLineEndC (YiEditorIter i) = transformYiIter i Yi.moveToEol
#endif

forwardWordEndC :: EditorIter -> IDEM (Maybe EditorIter)
forwardWordEndC (GtkEditorIter i) = transformGtkIterMaybe i Gtk.textIterForwardWordEnd
#ifdef LEKSAH_WITH_YI
forwardWordEndC (YiEditorIter i@(Yi.Iter b p)) = withYiIter i $ do
    Yi.nextWordB
    newPoint <- Yi.pointB
    if p == newPoint
        then return Nothing
        else return . Just $ mkYiIter' b newPoint
#endif

getChar :: EditorIter -> IDEM (Maybe Char)
getChar (GtkEditorIter i) = liftIO $ Gtk.textIterGetChar i
#ifdef LEKSAH_WITH_YI
getChar (YiEditorIter i) = withYiIter i Yi.readCharB
#endif

getCharsInLine :: EditorIter -> IDEM Int
getCharsInLine (GtkEditorIter i) = liftIO $ Gtk.textIterGetCharsInLine i
#ifdef LEKSAH_WITH_YI
getCharsInLine (YiEditorIter i) = withYiIter i $ length <$> Yi.readLnB
#endif

getLine :: EditorIter -> IDEM Int
getLine (GtkEditorIter i) = liftIO $ Gtk.textIterGetLine i
#ifdef LEKSAH_WITH_YI
getLine (YiEditorIter i) = withYiIter i Yi.curLn
#endif

getLineOffset :: EditorIter -> IDEM Int
getLineOffset (GtkEditorIter i) = liftIO $ Gtk.textIterGetLineOffset i
#ifdef LEKSAH_WITH_YI
getLineOffset (YiEditorIter i) = withYiIter i Yi.curCol
#endif

getOffset :: EditorIter -> IDEM Int
getOffset (GtkEditorIter i) = liftIO $ Gtk.textIterGetOffset i
#ifdef LEKSAH_WITH_YI
getOffset (YiEditorIter (Yi.Iter _ (Yi.Point o))) = return o
#endif

isStart :: EditorIter -> IDEM Bool
isStart (GtkEditorIter i) = liftIO $ Gtk.textIterIsStart i
#ifdef LEKSAH_WITH_YI
isStart (YiEditorIter i) = withYiIter i Yi.atSof
#endif

isEnd :: EditorIter -> IDEM Bool
isEnd (GtkEditorIter i) = liftIO $ Gtk.textIterIsEnd i
#ifdef LEKSAH_WITH_YI
isEnd (YiEditorIter i) = withYiIter i Yi.atEof
#endif

iterEqual :: EditorIter -> EditorIter -> IDEM Bool
iterEqual (GtkEditorIter i1) (GtkEditorIter i2) = liftIO $ Gtk.textIterEqual i1 i2
#ifdef LEKSAH_WITH_YI
iterEqual (YiEditorIter (Yi.Iter _ p1)) (YiEditorIter (Yi.Iter _ p2)) = return $ p1 == p2
iterEqual _ _ = liftIO $ fail "Mismatching TextEditor types in iterEqual"
#endif

startsLine :: EditorIter -> IDEM Bool
startsLine (GtkEditorIter i) = liftIO $ Gtk.textIterStartsLine i
#ifdef LEKSAH_WITH_YI
startsLine (YiEditorIter i) = withYiIter i Yi.atSol
#endif

atEnd :: EditorIter -> IDEM EditorIter
atEnd (GtkEditorIter i) = liftIO $ GtkEditorIter <$> do
    buffer <- Gtk.textIterGetBuffer i
    Gtk.textBufferGetEndIter buffer
#ifdef LEKSAH_WITH_YI
atEnd (YiEditorIter (Yi.Iter b _)) = iterFromYiBuffer' b Yi.sizeB
#endif

atLine :: EditorIter -> Int -> IDEM EditorIter
atLine (GtkEditorIter i) line = transformGtkIter i $ flip Gtk.textIterSetLine line
#ifdef LEKSAH_WITH_YI
atLine (YiEditorIter i) line = transformYiIter i $ Yi.gotoLn line
#endif

atLineOffset :: EditorIter -> Int -> IDEM EditorIter
atLineOffset (GtkEditorIter i) column = transformGtkIter i $ flip Gtk.textIterSetLineOffset column
#ifdef LEKSAH_WITH_YI
atLineOffset (YiEditorIter i) column = transformYiIter i $ Yi.moveToColB column
#endif

atOffset :: EditorIter -> Int -> IDEM EditorIter
atOffset (GtkEditorIter i) offset = transformGtkIter i $ flip Gtk.textIterSetOffset offset
#ifdef LEKSAH_WITH_YI
atOffset (YiEditorIter (Yi.Iter b _)) offset = return $ YiEditorIter $ Yi.Iter b (Yi.Point offset)
#endif

atStart :: EditorIter -> IDEM EditorIter
atStart (GtkEditorIter i) = liftIO $ GtkEditorIter <$> do
    buffer <- Gtk.textIterGetBuffer i
    Gtk.textBufferGetEndIter buffer
#ifdef LEKSAH_WITH_YI
atStart (YiEditorIter (Yi.Iter b _)) = return $ mkYiIter' b $ Yi.Point 0
#endif

-- Tag Table
newTag :: EditorTagTable -> String -> IDEM EditorTag
newTag (GtkEditorTagTable tt) name = liftIO $ do
    t <- Gtk.textTagNew (Just name)
    Gtk.textTagTableAdd tt t
    return $ GtkEditorTag t
#ifdef LEKSAH_WITH_YI
newTag (YiEditorTagTable) name = return YiEditorTag -- TODO
#endif

lookupTag :: EditorTagTable -> String -> IDEM (Maybe EditorTag)
lookupTag (GtkEditorTagTable tt) name = liftIO $ fmap (fmap GtkEditorTag) $ Gtk.textTagTableLookup tt name
#ifdef LEKSAH_WITH_YI
lookupTag (YiEditorTagTable) name = return Nothing -- TODO
#endif

-- Tag
background :: EditorTag -> Gtk.Color -> IDEM ()
background (GtkEditorTag t) color = liftIO $ Gtk.set t [Gtk.textTagBackground := colorHexString color]
#ifdef LEKSAH_WITH_YI
background (YiEditorTag) color = return () -- TODO
#endif

underline :: EditorTag -> Gtk.Underline -> IDEM ()
underline (GtkEditorTag t) value = liftIO $ Gtk.set t [Gtk.textTagUnderline := value]
#ifdef LEKSAH_WITH_YI
underline (YiEditorTag) value = return () -- TODO
#endif

-- Events
afterFocusIn :: EditorView -> IDEM () -> IDEM [Connection]
afterFocusIn (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.afterFocusIn` \_ -> reflectIDE f ideR >> return False
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
afterFocusIn (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        id1 <- (Yi.drawArea v) `Gtk.afterFocusIn` \_ -> reflectIDE f ideR >> return False
        return [ConnectC id1]
#endif

afterModifiedChanged :: EditorBuffer -> IDEM () -> IDEM [Connection]
afterModifiedChanged (GtkEditorBuffer sb) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sb `Gtk.afterModifiedChanged` reflectIDE f ideR
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
afterModifiedChanged (YiEditorBuffer b) f = return [] -- TODO
#endif

afterMoveCursor :: EditorView -> IDEM () -> IDEM [Connection]
afterMoveCursor (GtkEditorView sv) f = do
    ideR <- ask
    GtkEditorBuffer sb <- getBuffer (GtkEditorView sv)
    liftIO $ do
#if MIN_VERSION_gtk(0,10,5)
        id1 <- sv `Gtk.after` Gtk.moveCursor $ \_ _ _ -> reflectIDE f ideR
#else
        id1 <- sv `Gtk.afterMoveCursor` \_ _ _ -> reflectIDE f ideR
#endif
        sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
        id2 <- sv `Gtk.onButtonRelease` \_ -> reflectIDE f ideR >> return False
        id3 <- sb `Gtk.afterEndUserAction` reflectIDE f ideR
        return [ConnectC id1, ConnectC id2, ConnectC id3]
#ifdef LEKSAH_WITH_YI
afterMoveCursor (YiEditorView v) f = return [] -- TODO
#endif

afterToggleOverwrite :: EditorView -> IDEM () -> IDEM [Connection]
afterToggleOverwrite (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
#if MIN_VERSION_gtk(0,10,5)
        id1 <- sv `Gtk.after` Gtk.toggleOverwrite $ reflectIDE f ideR
#else
        id1 <- sv `Gtk.afterToggleOverwrite` reflectIDE f ideR
#endif
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
afterToggleOverwrite (YiEditorView v) f = return [] -- TODO
#endif

onButtonPress :: EditorView
                 -> (GtkOld.Event -> IDEM Bool)
                 -> IDEM [Connection]
onButtonPress (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.onButtonPress` \event -> reflectIDE (f event) ideR
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
onButtonPress (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        id1 <- (Yi.drawArea v) `Gtk.onButtonPress` \event -> reflectIDE (f event) ideR
        return [ConnectC id1]
#endif

onButtonRelease :: EditorView
                 -> (GtkOld.Event -> IDEM Bool)
                 -> IDEM [Connection]
onButtonRelease (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.onButtonRelease` \event -> reflectIDE (f event) ideR
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
onButtonRelease (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        id1 <- (Yi.drawArea v) `Gtk.onButtonRelease` \event -> reflectIDE (f event) ideR
        return [ConnectC id1]
#endif

onCompletion :: EditorView -> IDEM () -> IDEM () -> IDEM [Connection]
onCompletion (GtkEditorView sv) start cancel = do
    ideR <- ask
    (GtkEditorBuffer sb) <- getBuffer (GtkEditorView sv)
    liftIO $ do
        id1 <- sb `Gtk.afterBufferInsertText` \iter text -> do
            let isIdent a = isAlphaNum a || a == '\'' || a == '_' || a == '.'
            let isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                         || a == '!'  || a == '@' || a == '%' || a == '&' || a == '?'
            if (all isIdent text) || (all isOp text)
                then do
                    hasSel <- Gtk.textBufferHasSelection sb
                    if not hasSel
                        then do
                            (iterC, _) <- Gtk.textBufferGetSelectionBounds sb
                            atC <- Gtk.textIterEqual iter iterC
                            when atC $ reflectIDE start ideR
                        else
                            reflectIDE cancel ideR
                else
                    reflectIDE cancel ideR
#if MIN_VERSION_gtk(0,10,5)
        id2 <- sv `Gtk.on` Gtk.moveCursor $ \_ _ _ -> reflectIDE cancel ideR
#else
        id2 <- sv `Gtk.onMoveCursor` \_ _ _ -> reflectIDE cancel ideR
#endif
        id3 <- sv `Gtk.onButtonPress` \_ -> reflectIDE cancel ideR >> return False
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
onCompletion (YiEditorView v) start cancel = return [] -- TODO
#endif

onKeyPress :: EditorView
              -> (String -> [Gtk.Modifier] -> Gtk.KeyVal -> IDEM Bool)
              -> IDEM [Connection]
onKeyPress (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.on` Gtk.keyPressEvent $ do
            name        <- Gtk.eventKeyName
            modifier    <- Gtk.eventModifier
            keyVal      <- Gtk.eventKeyVal
            liftIO $ reflectIDE (f name modifier keyVal) ideR
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
onKeyPress (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        id1 <- (Yi.drawArea v) `Gtk.on` Gtk.keyPressEvent $ do
            name        <- Gtk.eventKeyName
            modifier    <- Gtk.eventModifier
            keyVal      <- Gtk.eventKeyVal
            liftIO $ reflectIDE (f name modifier keyVal) ideR
        return [ConnectC id1]
#endif

onKeyRelease :: EditorView
                -> (String -> [Gtk.Modifier] -> Gtk.KeyVal -> IDEM Bool)
                -> IDEM [Connection]
onKeyRelease (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.on` Gtk.keyReleaseEvent $ do
            name        <- Gtk.eventKeyName
            modifier    <- Gtk.eventModifier
            keyVal      <- Gtk.eventKeyVal
            liftIO $ reflectIDE (f name modifier keyVal) ideR
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
onKeyRelease (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        id1 <- (Yi.drawArea v) `Gtk.on` Gtk.keyReleaseEvent $ do
            name        <- Gtk.eventKeyName
            modifier    <- Gtk.eventModifier
            keyVal      <- Gtk.eventKeyVal
            liftIO $ reflectIDE (f name modifier keyVal) ideR
        return [ConnectC id1]
#endif

onLookupInfo :: EditorView -> IDEM () -> IDEM [Connection]
onLookupInfo (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
        id1 <- sv `Gtk.onButtonRelease` \e -> when (controlIsPressed e) (reflectIDE f ideR) >> return False
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
onLookupInfo (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        (Yi.drawArea v) `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
        id1 <- (Yi.drawArea v) `Gtk.onButtonRelease` \e -> when (controlIsPressed e) (reflectIDE f ideR) >> return False
        return [ConnectC id1]
#endif

onPopulatePopup :: EditorView -> (Gtk.Menu -> IDEM ()) -> IDEM [Connection]
onPopulatePopup (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
#if MIN_VERSION_gtk(0,10,5)
        id1 <- sv `Gtk.on` Gtk.populatePopup $ \menu -> reflectIDE (f menu) ideR
#else
        id1 <- sv `Gtk.onPopulatePopup` \menu -> reflectIDE (f menu) ideR
#endif
        return [ConnectC id1]
#ifdef LEKSAH_WITH_YI
onPopulatePopup (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        id1 <- (Yi.drawArea v) `Gtk.onPopupMenu` do
             menu <- Gtk.menuNew
             reflectIDE (f menu) ideR
             Gtk.menuPopup menu Nothing
        return [ConnectC id1]
#endif



