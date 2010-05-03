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
import Data.Char (isAlphaNum)
import Data.Maybe (fromJust)
import Control.Monad (when)
import Control.Monad.Reader (liftIO, ask)
import Control.Applicative ((<$>))

import qualified Graphics.UI.Gtk as Gtk hiding(afterToggleOverwrite)
import qualified Graphics.UI.Gtk.SourceView as Gtk
import qualified Graphics.UI.Gtk.Multiline.TextView as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as GtkOld
import qualified Graphics.UI.Gtk.Gdk.EventM as Gtk
import System.Glib.Attributes (AttrOp(..))

#ifdef YI
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
#ifdef YI
    | YiEditorBuffer Yi.Buffer
#endif

data EditorView = GtkEditorView Gtk.SourceView
#ifdef YI
    | YiEditorView Yi.View
#endif

data EditorMark = GtkEditorMark Gtk.TextMark
#ifdef YI
    | YiEditorMark Yi.Mark
#endif

data EditorIter = GtkEditorIter Gtk.TextIter
#ifdef YI
    | YiEditorIter Yi.Iter
#endif

data EditorTagTable = GtkEditorTagTable Gtk.TextTagTable
#ifdef YI
    | YiEditorTagTable
#endif

data EditorTag = GtkEditorTag Gtk.TextTag
#ifdef YI
    | YiEditorTag
#endif

#ifdef YI
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
#ifdef YI
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
#ifdef YI
applyTagByName (YiEditorBuffer fb) name (YiEditorIter first) (YiEditorIter last) = return () -- TODO
applyTagByName _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in createMark"
#endif

beginNotUndoableAction :: EditorBuffer -> IDEM ()
beginNotUndoableAction (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferBeginNotUndoableAction sb
#ifdef YI
beginNotUndoableAction (YiEditorBuffer fb) = return () -- TODO
#endif

beginUserAction :: EditorBuffer -> IDEM ()
beginUserAction (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferBeginUserAction sb
#ifdef YI
beginUserAction (YiEditorBuffer fb) = return () -- TODO
#endif

canRedo :: EditorBuffer -> IDEM Bool
canRedo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferGetCanRedo sb
#ifdef YI
canRedo (YiEditorBuffer fb) = return True -- TODO
#endif

canUndo :: EditorBuffer -> IDEM Bool
canUndo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferGetCanUndo sb
#ifdef YI
canUndo (YiEditorBuffer fb) = return True -- TODO
#endif

copyClipboard :: EditorBuffer -> Gtk.Clipboard -> IDEM ()
copyClipboard (GtkEditorBuffer sb) clipboard = liftIO $ Gtk.textBufferCopyClipboard sb clipboard
#ifdef YI
copyClipboard (YiEditorBuffer fb) _ = liftYi $ Yi.liftEditor $ Yi.copy
#endif

createMark :: EditorBuffer
              -> EditorIter
              -> Bool
              -> IDEM EditorMark
createMark (GtkEditorBuffer sb) (GtkEditorIter i) leftGravity = liftIO $
    GtkEditorMark <$> Gtk.textBufferCreateMark sb Nothing i leftGravity
#ifdef YI
createMark (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ p)) leftGravity = withYiBuffer b $
    YiEditorMark <$> Yi.newMarkB (Yi.MarkValue p (if leftGravity then Yi.Backward else Yi.Forward))
createMark _ _ _ = liftIO $ fail "Mismatching TextEditor types in createMark"
#endif

cutClipboard :: EditorBuffer -> Gtk.Clipboard -> Bool -> IDEM ()
cutClipboard (GtkEditorBuffer sb) clipboard defaultEditable = liftIO $ Gtk.textBufferCutClipboard sb clipboard defaultEditable
#ifdef YI
cutClipboard (YiEditorBuffer fb) clipboard defaultEditable = liftYi $ Yi.liftEditor $ Yi.cut
#endif

delete :: EditorBuffer -> EditorIter -> EditorIter -> IDEM ()
delete (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferDelete sb first last
#ifdef YI
delete (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ first)) (YiEditorIter (Yi.Iter _ last)) =
    withYiBuffer b $ Yi.deleteRegionB $ Yi.mkRegion first last
delete _ _ _ = liftIO $ fail "Mismatching TextEditor types in delete"
#endif

deleteSelection :: EditorBuffer -> Bool -> Bool -> IDEM ()
deleteSelection (GtkEditorBuffer sb) interactive defaultEditable = liftIO $
    Gtk.textBufferDeleteSelection sb interactive defaultEditable >> return ()
#ifdef YI
deleteSelection (YiEditorBuffer b) interactive defaultEditable = withYiBuffer b $ do
    region <- Yi.getRawestSelectRegionB
    Yi.deleteRegionB region -- TODO support flags
#endif

endNotUndoableAction :: EditorBuffer -> IDEM ()
endNotUndoableAction (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferEndNotUndoableAction sb
#ifdef YI
endNotUndoableAction (YiEditorBuffer fb) = return () -- TODO
#endif

endUserAction :: EditorBuffer -> IDEM ()
endUserAction (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferEndUserAction sb
#ifdef YI
endUserAction (YiEditorBuffer fb) = return () -- TODO
#endif

getEndIter :: EditorBuffer -> IDEM EditorIter
getEndIter (GtkEditorBuffer sb) = liftIO $ GtkEditorIter <$> Gtk.textBufferGetEndIter sb
#ifdef YI
getEndIter (YiEditorBuffer b) = iterFromYiBuffer b Yi.sizeB
#endif

getInsertMark :: EditorBuffer -> IDEM EditorMark
getInsertMark (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorMark $ Gtk.textBufferGetInsert sb
#ifdef YI
getInsertMark (YiEditorBuffer b) = YiEditorMark <$> (withYiBuffer b $ Yi.insMark <$> Yi.askMarks)
#endif

getIterAtLine :: EditorBuffer -> Int -> IDEM EditorIter
getIterAtLine (GtkEditorBuffer sb) line = liftIO $ GtkEditorIter <$> Gtk.textBufferGetIterAtLine sb line
#ifdef YI
getIterAtLine (YiEditorBuffer b) line = iterFromYiBuffer b $ Yi.pointOfLineColB line 1
#endif

getIterAtMark :: EditorBuffer -> EditorMark -> IDEM EditorIter
getIterAtMark (GtkEditorBuffer sb) (GtkEditorMark m) = liftIO $ GtkEditorIter <$> Gtk.textBufferGetIterAtMark sb m
#ifdef YI
getIterAtMark (YiEditorBuffer b) (YiEditorMark m) = iterFromYiBuffer b $ Yi.getMarkPointB m
getIterAtMark _ _ = liftIO $ fail "Mismatching TextEditor types in getIterAtMark"
#endif

getIterAtOffset :: EditorBuffer -> Int -> IDEM EditorIter
getIterAtOffset (GtkEditorBuffer sb) offset = liftIO $ GtkEditorIter <$> Gtk.textBufferGetIterAtOffset sb offset
#ifdef YI
getIterAtOffset (YiEditorBuffer b) offset = return $ mkYiIter b $ Yi.Point offset
#endif

getLineCount :: EditorBuffer -> IDEM Int
getLineCount (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferGetLineCount sb
#ifdef YI
getLineCount (YiEditorBuffer b) = withYiBuffer b $ Yi.sizeB >>= Yi.lineOf
#endif

getModified :: EditorBuffer -> IDEM Bool
getModified (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferGetModified sb
#ifdef YI
getModified (YiEditorBuffer b) = not <$> (withYiBuffer b $ Yi.gets Yi.isUnchangedBuffer)
#endif

getSelectionBoundMark :: EditorBuffer -> IDEM EditorMark
getSelectionBoundMark (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorMark $ Gtk.textBufferGetSelectionBound sb
#ifdef YI
getSelectionBoundMark (YiEditorBuffer b) = YiEditorMark . Yi.selMark <$> (withYiBuffer b $ Yi.askMarks)
#endif

getSelectionBounds :: EditorBuffer -> IDEM (EditorIter, EditorIter)
getSelectionBounds (GtkEditorBuffer sb) = liftIO $ do
    (first, last) <- Gtk.textBufferGetSelectionBounds sb
    return (GtkEditorIter first, GtkEditorIter last)
#ifdef YI
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
#ifdef YI
getSlice (YiEditorBuffer b) (YiEditorIter first) (YiEditorIter last) includeHidenChars = liftYiControl $
    Yi.getText b first last
getSlice _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in getSlice"
#endif

getStartIter :: EditorBuffer -> IDEM EditorIter
getStartIter (GtkEditorBuffer sb) = liftIO $ GtkEditorIter <$> Gtk.textBufferGetStartIter sb
#ifdef YI
getStartIter (YiEditorBuffer b) = return $ mkYiIter b $ Yi.Point 0
#endif

getTagTable :: EditorBuffer -> IDEM EditorTagTable
getTagTable (GtkEditorBuffer sb) = liftIO $ fmap GtkEditorTagTable $ Gtk.textBufferGetTagTable sb
#ifdef YI
getTagTable (YiEditorBuffer b) = return YiEditorTagTable -- TODO
#endif

getText :: EditorBuffer
           -> EditorIter
           -> EditorIter
           -> Bool
           -> IDEM String
getText (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars = liftIO $
    Gtk.textBufferGetText sb first last includeHidenChars
#ifdef YI
getText (YiEditorBuffer b) (YiEditorIter first) (YiEditorIter last) includeHidenChars = liftYiControl $
    Yi.getText b first last
getText _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in getText"
#endif

hasSelection :: EditorBuffer -> IDEM Bool
hasSelection (GtkEditorBuffer sb) = liftIO $ Gtk.textBufferHasSelection sb
#ifdef YI
hasSelection (YiEditorBuffer b) = withYiBuffer b $ do
    region <- Yi.getRawestSelectRegionB
    return $ not $ Yi.regionIsEmpty region
#endif

insert :: EditorBuffer -> EditorIter -> String -> IDEM ()
insert (GtkEditorBuffer sb) (GtkEditorIter i) text = liftIO $ Gtk.textBufferInsert sb i text
#ifdef YI
insert (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ p)) text = withYiBuffer b $ Yi.insertNAt text p
insert _ _ _ = liftIO $ fail "Mismatching TextEditor types in insert"
#endif

moveMark :: EditorBuffer -> EditorMark -> EditorIter -> IDEM ()
moveMark (GtkEditorBuffer sb) (GtkEditorMark m) (GtkEditorIter i) = liftIO $ Gtk.textBufferMoveMark sb m i
#ifdef YI
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
        sw <- Gtk.scrolledWindowNew Nothing Nothing
        Gtk.containerAdd sw sv
        Gtk.widgetModifyFont sv (Just fd)
        return (GtkEditorView sv)
#ifdef YI
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
#ifdef YI
pasteClipboard (YiEditorBuffer b) clipboard (YiEditorIter (Yi.Iter _ p)) defaultEditable = liftYi $ Yi.liftEditor $ Yi.paste
pasteClipboard _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in pasteClipboard"
#endif

placeCursor :: EditorBuffer -> EditorIter -> IDEM ()
placeCursor (GtkEditorBuffer sb) (GtkEditorIter i) = liftIO $ Gtk.textBufferPlaceCursor sb i
#ifdef YI
placeCursor (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ p)) = withYiBuffer b $ Yi.moveTo p
placeCursor _ _ = liftIO $ fail "Mismatching TextEditor types in placeCursor"
#endif

redo :: EditorBuffer -> IDEM ()
redo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferRedo sb
#ifdef YI
redo (YiEditorBuffer b) = withYiBuffer b Yi.redoB
#endif

removeTagByName :: EditorBuffer
                   -> String
                   -> EditorIter
                   -> EditorIter
                   -> IDEM ()
removeTagByName (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferRemoveTagByName sb name first last
#ifdef YI
removeTagByName (YiEditorBuffer b) name (YiEditorIter (Yi.Iter _ first)) (YiEditorIter (Yi.Iter _ last)) = return () -- TODO
removeTagByName _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in removeTagByName"
#endif

selectRange :: EditorBuffer -> EditorIter -> EditorIter -> IDEM ()
selectRange (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) = liftIO $
    Gtk.textBufferSelectRange sb first last
#ifdef YI
selectRange (YiEditorBuffer b) (YiEditorIter (Yi.Iter _ first)) (YiEditorIter (Yi.Iter _ last)) = withYiBuffer b $
    Yi.setSelectRegionB $ Yi.mkRegion first last
selectRange _ _ _ = liftIO $ fail "Mismatching TextEditor types in selectRange"
#endif

setModified :: EditorBuffer -> Bool -> IDEM ()
setModified (GtkEditorBuffer sb) modified = liftIO $ Gtk.textBufferSetModified sb modified >> return ()
#ifdef YI
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
#ifdef YI
setStyle (YiEditorBuffer b) mbStyle = return () -- TODO
#endif

setText :: EditorBuffer -> String -> IDEM ()
setText (GtkEditorBuffer sb) text = liftIO $ Gtk.textBufferSetText sb text
#ifdef YI
setText (YiEditorBuffer b) text = liftYiControl $ Yi.setText b text
#endif

undo :: EditorBuffer -> IDEM ()
undo (GtkEditorBuffer sb) = liftIO $ Gtk.sourceBufferUndo sb
#ifdef YI
undo (YiEditorBuffer b) =  withYiBuffer b Yi.undoB
#endif

-- View
bufferToWindowCoords :: EditorView -> (Int, Int) -> IDEM (Int, Int)
bufferToWindowCoords (GtkEditorView sv) point = liftIO $ Gtk.textViewBufferToWindowCoords sv Gtk.TextWindowWidget point
#ifdef YI
bufferToWindowCoords (YiEditorView v) point = return point -- TODO
#endif

getBuffer :: EditorView -> IDEM EditorBuffer
getBuffer (GtkEditorView sv) = liftIO $ fmap (GtkEditorBuffer . Gtk.castToSourceBuffer) $ sv `Gtk.get` Gtk.textViewBuffer
#ifdef YI
getBuffer (YiEditorView v) = return $ YiEditorBuffer $ Yi.getBuffer v
#endif

getDrawWindow :: EditorView -> IDEM Gtk.DrawWindow
getDrawWindow (GtkEditorView sv) = liftIO $ Gtk.widgetGetDrawWindow sv
#ifdef YI
getDrawWindow (YiEditorView v) = liftIO $ Gtk.widgetGetDrawWindow (Yi.drawArea v)
#endif

getIterLocation :: EditorView -> EditorIter -> IDEM Gtk.Rectangle
getIterLocation (GtkEditorView sv) (GtkEditorIter i) = liftIO $ Gtk.textViewGetIterLocation sv i
#ifdef YI
getIterLocation (YiEditorView v) (YiEditorIter i) = return $ Gtk.Rectangle 0 0 0 0 -- TODO
getIterLocation _ _ = liftIO $ fail "Mismatching TextEditor types in getIterLocation"
#endif

getOverwrite :: EditorView -> IDEM Bool
getOverwrite (GtkEditorView sv) = liftIO $ Gtk.textViewGetOverwrite sv
#ifdef YI
getOverwrite (YiEditorView Yi.View{Yi.viewFBufRef = b}) = withYiBuffer' b $ not <$> Yi.getA Yi.insertingA
#endif

getScrolledWindow :: EditorView -> IDEM Gtk.ScrolledWindow
getScrolledWindow (GtkEditorView sv) = liftIO $ fmap (Gtk.castToScrolledWindow . fromJust) $ Gtk.widgetGetParent sv
#ifdef YI
getScrolledWindow (YiEditorView v) = return $ Yi.scrollWin v
#endif

grabFocus :: EditorView -> IDEM ()
grabFocus (GtkEditorView sv) = liftIO $ Gtk.widgetGrabFocus sv
#ifdef YI
grabFocus (YiEditorView Yi.View{Yi.drawArea = da}) = liftIO $ Gtk.widgetGrabFocus da
#endif

scrollToMark :: EditorView
                -> EditorMark
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
scrollToMark (GtkEditorView sv) (GtkEditorMark m) withMargin mbAlign = liftIO $ Gtk.textViewScrollToMark sv m withMargin mbAlign
#ifdef YI
scrollToMark (YiEditorView v) (YiEditorMark m) withMargin mbAlign = return () -- TODO
scrollToMark _ _ _ _ = liftIO $ fail "Mismatching TextEditor types in scrollToMark"
#endif

scrollToIter :: EditorView
                -> EditorIter
                -> Double
                -> Maybe (Double, Double)
                -> IDEM ()
scrollToIter (GtkEditorView sv) (GtkEditorIter i) withMargin mbAlign = liftIO $ Gtk.textViewScrollToIter sv i withMargin mbAlign >> return ()
#ifdef YI
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
#ifdef YI
setFont (YiEditorView v) mbFontString = do
    fd <- fontDescription mbFontString
    liftIO $ Gtk.layoutSetFontDescription (Yi.layout v) (Just fd)
#endif

setIndentWidth :: EditorView -> Int -> IDEM ()
setIndentWidth (GtkEditorView sv) width = liftIO $ Gtk.sourceViewSetIndentWidth sv width
#ifdef YI
setIndentWidth (YiEditorView v) width = return () -- TODO
#endif

setRightMargin :: EditorView -> Maybe Int -> IDEM ()
setRightMargin (GtkEditorView sv) mbRightMargin = liftIO $ do
    case mbRightMargin of
        Just n -> do
            Gtk.sourceViewSetShowRightMargin sv True
            Gtk.sourceViewSetRightMarginPosition sv (fromIntegral n)
        Nothing -> Gtk.sourceViewSetShowRightMargin sv False
#ifdef YI
setRightMargin (YiEditorView v) mbRightMargin = return () -- TODO
#endif

setShowLineNumbers :: EditorView -> Bool -> IDEM ()
setShowLineNumbers (GtkEditorView sv) show = liftIO $ Gtk.sourceViewSetShowLineNumbers sv show
#ifdef YI
setShowLineNumbers (YiEditorView v) show = return () -- TODO
#endif

setTabWidth :: EditorView -> Int -> IDEM ()
setTabWidth (GtkEditorView sv) width = liftIO $ Gtk.sourceViewSetTabWidth sv width
#ifdef YI
setTabWidth (YiEditorView v) width = return () -- TODO
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

#ifdef YI
withYiIter :: Yi.Iter -> Yi.BufferM a -> IDEM a
withYiIter (Yi.Iter b p) f = withYiBuffer' b $ do
    oldPoint <- Yi.pointB
    insertMark <- Yi.insMark <$> Yi.askMarks
    Yi.setMarkPointB insertMark p -- Using this becauls moveTo forgets the prefered column
    result <- f
    Yi.setMarkPointB insertMark oldPoint
    return result

transformYiIter' :: Yi.Iter -> Yi.BufferM Yi.Point -> IDEM EditorIter
transformYiIter' i f = mkYiIter' (Yi.iterFBufRef i) <$> withYiIter i f

transformYiIter :: Yi.Iter -> Yi.BufferM a -> IDEM EditorIter
transformYiIter i f = transformYiIter' i (f >> Yi.pointB)
#endif

backwardCharC :: EditorIter -> IDEM EditorIter
backwardCharC (GtkEditorIter i) = transformGtkIter i Gtk.textIterBackwardChar
#ifdef YI
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
#ifdef YI
            _                          -> fail "Mismatching TextEditor types in backwardFindChar"
#endif

#ifdef YI
backwardFindCharC (YiEditorIter i) pred mbLimit = return Nothing -- TODO
#endif

backwardWordStartC :: EditorIter -> IDEM (Maybe EditorIter)
backwardWordStartC (GtkEditorIter i) = transformGtkIterMaybe i Gtk.textIterBackwardWordStart
#ifdef YI
backwardWordStartC (YiEditorIter i) = return Nothing -- TODO
#endif

endsWord :: EditorIter -> IDEM Bool
endsWord (GtkEditorIter i) = liftIO $ Gtk.textIterEndsWord i
#ifdef YI
endsWord (YiEditorIter i) = return False -- TODO
#endif

forwardCharC :: EditorIter -> IDEM EditorIter
forwardCharC (GtkEditorIter i) = transformGtkIter i Gtk.textIterForwardChar
#ifdef YI
forwardCharC (YiEditorIter i) = transformYiIter' i Yi.nextPointB
#endif

forwardCharsC :: EditorIter -> Int -> IDEM EditorIter
forwardCharsC (GtkEditorIter i) n = transformGtkIter i $ flip Gtk.textIterForwardChars n
#ifdef YI
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
#ifdef YI
            _                          -> fail "Mismatching TextEditor types in forwardFindChar"
#endif

#ifdef YI
forwardFindCharC (YiEditorIter i) pred mbLimit = return Nothing -- TODO
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
#ifdef YI
                _                          -> fail "Mismatching TextEditor types in forwardSearch"
#endif

#ifdef YI
forwardSearch (YiEditorIter i) str pred mbLimit = return Nothing -- TODO
#endif

forwardToLineEndC :: EditorIter -> IDEM EditorIter
forwardToLineEndC (GtkEditorIter i) = transformGtkIter i Gtk.textIterForwardToLineEnd
#ifdef YI
forwardToLineEndC (YiEditorIter i) = transformYiIter i Yi.moveToEol
#endif

forwardWordEndC :: EditorIter -> IDEM (Maybe EditorIter)
forwardWordEndC (GtkEditorIter i) = transformGtkIterMaybe i Gtk.textIterForwardWordEnd
#ifdef YI
forwardWordEndC (YiEditorIter i) = return Nothing -- TODO
#endif

getChar :: EditorIter -> IDEM (Maybe Char)
getChar (GtkEditorIter i) = liftIO $ Gtk.textIterGetChar i
#ifdef YI
getChar (YiEditorIter i) = withYiIter i Yi.readCharB
#endif

getCharsInLine :: EditorIter -> IDEM Int
getCharsInLine (GtkEditorIter i) = liftIO $ Gtk.textIterGetCharsInLine i
#ifdef YI
getCharsInLine (YiEditorIter i) = withYiIter i $ length <$> Yi.readLnB
#endif

getLine :: EditorIter -> IDEM Int
getLine (GtkEditorIter i) = liftIO $ Gtk.textIterGetLine i
#ifdef YI
getLine (YiEditorIter i) = withYiIter i Yi.curLn
#endif

getLineOffset :: EditorIter -> IDEM Int
getLineOffset (GtkEditorIter i) = liftIO $ Gtk.textIterGetLineOffset i
#ifdef YI
getLineOffset (YiEditorIter i) = withYiIter i Yi.curCol
#endif

getOffset :: EditorIter -> IDEM Int
getOffset (GtkEditorIter i) = liftIO $ Gtk.textIterGetOffset i
#ifdef YI
getOffset (YiEditorIter (Yi.Iter _ (Yi.Point o))) = return o
#endif

isStart :: EditorIter -> IDEM Bool
isStart (GtkEditorIter i) = liftIO $ Gtk.textIterIsStart i
#ifdef YI
isStart (YiEditorIter i) = withYiIter i Yi.atSof
#endif

isEnd :: EditorIter -> IDEM Bool
isEnd (GtkEditorIter i) = liftIO $ Gtk.textIterIsEnd i
#ifdef YI
isEnd (YiEditorIter i) = withYiIter i Yi.atEof
#endif

iterEqual :: EditorIter -> EditorIter -> IDEM Bool
iterEqual (GtkEditorIter i1) (GtkEditorIter i2) = liftIO $ Gtk.textIterEqual i1 i2
#ifdef YI
iterEqual (YiEditorIter (Yi.Iter _ p1)) (YiEditorIter (Yi.Iter _ p2)) = return $ p1 == p2
iterEqual _ _ = liftIO $ fail "Mismatching TextEditor types in iterEqual"
#endif

startsLine :: EditorIter -> IDEM Bool
startsLine (GtkEditorIter i) = liftIO $ Gtk.textIterStartsLine i
#ifdef YI
startsLine (YiEditorIter i) = withYiIter i Yi.atSol
#endif

atEnd :: EditorIter -> IDEM EditorIter
atEnd (GtkEditorIter i) = liftIO $ GtkEditorIter <$> do
    buffer <- Gtk.textIterGetBuffer i
    Gtk.textBufferGetEndIter buffer
#ifdef YI
atEnd (YiEditorIter (Yi.Iter b _)) = iterFromYiBuffer' b Yi.sizeB
#endif

atLine :: EditorIter -> Int -> IDEM EditorIter
atLine (GtkEditorIter i) line = transformGtkIter i $ flip Gtk.textIterSetLine line
#ifdef YI
atLine (YiEditorIter i) line = transformYiIter i $ Yi.gotoLn line
#endif

atLineOffset :: EditorIter -> Int -> IDEM EditorIter
atLineOffset (GtkEditorIter i) column = transformGtkIter i $ flip Gtk.textIterSetLineOffset column
#ifdef YI
atLineOffset (YiEditorIter i) column = transformYiIter i $ Yi.moveToColB column
#endif

atOffset :: EditorIter -> Int -> IDEM EditorIter
atOffset (GtkEditorIter i) offset = transformGtkIter i $ flip Gtk.textIterSetOffset offset
#ifdef YI
atOffset (YiEditorIter (Yi.Iter b _)) offset = return $ YiEditorIter $ Yi.Iter b (Yi.Point offset)
#endif

atStart :: EditorIter -> IDEM EditorIter
atStart (GtkEditorIter i) = liftIO $ GtkEditorIter <$> do
    buffer <- Gtk.textIterGetBuffer i
    Gtk.textBufferGetEndIter buffer
#ifdef YI
atStart (YiEditorIter (Yi.Iter b _)) = return $ mkYiIter' b $ Yi.Point 0
#endif

-- Tag Table
newTag :: EditorTagTable -> String -> IDEM EditorTag
newTag (GtkEditorTagTable tt) name = liftIO $ do
    t <- Gtk.textTagNew (Just name)
    Gtk.textTagTableAdd tt t
    return $ GtkEditorTag t
#ifdef YI
newTag (YiEditorTagTable) name = return YiEditorTag -- TODO
#endif

lookupTag :: EditorTagTable -> String -> IDEM (Maybe EditorTag)
lookupTag (GtkEditorTagTable tt) name = liftIO $ fmap (fmap GtkEditorTag) $ Gtk.textTagTableLookup tt name
#ifdef YI
lookupTag (YiEditorTagTable) name = return Nothing -- TODO
#endif

-- Tag
background :: EditorTag -> Gtk.Color -> IDEM ()
background (GtkEditorTag t) color = liftIO $ Gtk.set t [Gtk.textTagBackground := colorHexString color]
#ifdef YI
background (YiEditorTag) color = return () -- TODO
#endif

underline :: EditorTag -> Gtk.Underline -> IDEM ()
underline (GtkEditorTag t) value = liftIO $ Gtk.set t [Gtk.textTagUnderline := value]
#ifdef YI
underline (YiEditorTag) value = return () -- TODO
#endif

-- Events
afterFocusIn :: EditorView -> IDEM () -> IDEM [Connection]
afterFocusIn (GtkEditorView sv) f = do
    ideR <- ask
    liftIO $ do
        id1 <- sv `Gtk.afterFocusIn` \_ -> reflectIDE f ideR >> return False
        return [ConnectC id1]
#ifdef YI
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
#ifdef YI
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
#ifdef YI
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
#ifdef YI
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
#ifdef YI
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
#ifdef YI
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
        id1 <- sb `Gtk.afterBufferInsertText` \iter text ->
            if (all (\c -> (isAlphaNum c) || (c == '.') || (c == '_')) text)
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
#ifdef YI
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
#ifdef YI
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
#ifdef YI
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
#ifdef YI
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
#ifdef YI
onPopulatePopup (YiEditorView v) f = do
    ideR <- ask
    liftIO $ do
        id1 <- (Yi.drawArea v) `Gtk.onPopupMenu` do
             menu <- Gtk.menuNew
             reflectIDE (f menu) ideR
             Gtk.menuPopup menu Nothing
        return [ConnectC id1]
#endif



