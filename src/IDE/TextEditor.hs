-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   backwardChar
,   backwardFindChar
,   backwardWordStart
,   copyIter
,   endsWord
,   forwardChar
,   forwardChars
,   forwardFindChar
,   forwardToLineEnd
,   forwardWordEnd
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
,   setLine
,   setLineOffset
,   setOffset

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
,   onCompletion
,   onKeyPress
,   onKeyRelease
,   onLookupInfo
,   onPopulatePopup
) where

import Prelude hiding(getChar, getLine)
import Data.Char (isAlphaNum)
import Data.Maybe (fromJust)
import Data.IORef (readIORef, writeIORef)
import Control.Monad (when)
import Control.Monad.Reader (liftIO)

import qualified Graphics.UI.Gtk as Gtk hiding(afterToggleOverwrite)
import qualified Graphics.UI.Gtk.SourceView as Gtk
import qualified Graphics.UI.Gtk.Multiline.TextView as Gtk
import qualified Graphics.UI.Gtk.Gdk.EventM as Gtk
import System.Glib.Attributes (AttrOp(..))

#ifdef YI
import qualified Yi as Yi
import qualified Yi.UI.Pango.Control as Yi
#endif

import Graphics.UI.Frame.Panes (Connection(..))
import IDE.Core.Types (colorHexString)
import IDE.Core.State (controlIsPressed)

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
    | YiEditorMark
#endif

data EditorIter = GtkEditorIter Gtk.TextIter
#ifdef YI
    | YiEditorIter
#endif

data EditorTagTable = GtkEditorTagTable Gtk.TextTagTable
#ifdef YI
    | YiEditorTagTable
#endif

data EditorTag = GtkEditorTag Gtk.TextTag
#ifdef YI
    | YiEditorTag
#endif

-- Buffer
newGtkBuffer mbFilename = do
    lm     <- Gtk.sourceLanguageManagerNew
    mbLang <- case mbFilename of
        Just filename -> Gtk.sourceLanguageManagerGuessLanguage lm (Just filename) Nothing
        Nothing       -> Gtk.sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
    buffer <- case mbLang of
        Just sLang -> Gtk.sourceBufferNewWithLanguage sLang
        Nothing -> Gtk.sourceBufferNew Nothing
    Gtk.sourceBufferSetMaxUndoLevels buffer (-1)
    return $ GtkEditorBuffer buffer

#ifdef YI
newYiBuffer mbFilename = fmap YiEditorBuffer $ Yi.newBuffer 0 (Left "*leksah*") (Yi.fromString "")
#else
newYiBuffer = newGtkBuffer
#endif

applyTagByName (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferApplyTagByName sb name first last
#ifdef YI
applyTagByName (YiEditorBuffer fb) name (YiEditorIter) (YiEditorIter) = return () -- TODO
applyTagByName _ _ _ _ = fail "Mismatching TextEditor types in createMark"
#endif

beginNotUndoableAction (GtkEditorBuffer sb) = Gtk.sourceBufferBeginNotUndoableAction sb
#ifdef YI
beginNotUndoableAction (YiEditorBuffer fb) = return () -- TODO
#endif

beginUserAction (GtkEditorBuffer sb) = Gtk.textBufferBeginUserAction sb
#ifdef YI
beginUserAction (YiEditorBuffer fb) = return () -- TODO
#endif

canRedo (GtkEditorBuffer sb) = Gtk.sourceBufferGetCanRedo sb
#ifdef YI
canRedo (YiEditorBuffer fb) = return False -- TODO
#endif

canUndo (GtkEditorBuffer sb) = Gtk.sourceBufferGetCanUndo sb
#ifdef YI
canUndo (YiEditorBuffer fb) = return False -- TODO
#endif

copyClipboard (GtkEditorBuffer sb) clipboard = Gtk.textBufferCopyClipboard sb clipboard
#ifdef YI
copyClipboard (YiEditorBuffer fb) _ = return () -- TODO
#endif

createMark (GtkEditorBuffer sb) mbName (GtkEditorIter i) leftGravity =
    fmap GtkEditorMark $ Gtk.textBufferCreateMark sb mbName i leftGravity
#ifdef YI
createMark (YiEditorBuffer fb) mbName (YiEditorIter) leftGravity = return YiEditorMark -- TODO
createMark _ _ _ _ = fail "Mismatching TextEditor types in createMark"
#endif

cutClipboard (GtkEditorBuffer sb) clipboard defaultEditable = Gtk.textBufferCutClipboard sb clipboard defaultEditable
#ifdef YI
cutClipboard (YiEditorBuffer fb) clipboard defaultEditable = return () -- TODO
#endif

delete (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferDelete sb first last
#ifdef YI
delete (YiEditorBuffer fb) (YiEditorIter) (YiEditorIter) = return () -- TODO
delete _ _ _ = fail "Mismatching TextEditor types in delete"
#endif

deleteSelection (GtkEditorBuffer sb) interactive defaultEditable =
    Gtk.textBufferDeleteSelection sb interactive defaultEditable >> return ()
#ifdef YI
deleteSelection (YiEditorBuffer fb) interactive defaultEditable = return () -- TODO
#endif

endNotUndoableAction (GtkEditorBuffer sb) = Gtk.sourceBufferEndNotUndoableAction sb
#ifdef YI
endNotUndoableAction (YiEditorBuffer fb) = return () -- TODO
#endif

endUserAction (GtkEditorBuffer sb) = Gtk.textBufferEndUserAction sb
#ifdef YI
endUserAction (YiEditorBuffer fb) = return () -- TODO
#endif

getEndIter (GtkEditorBuffer sb) = fmap GtkEditorIter $ Gtk.textBufferGetEndIter sb
#ifdef YI
getEndIter (YiEditorBuffer b) = return YiEditorIter -- TODO
#endif

getInsertMark (GtkEditorBuffer sb) = fmap GtkEditorMark $ Gtk.textBufferGetInsert sb
#ifdef YI
getInsertMark (YiEditorBuffer b) = return YiEditorMark -- TODO
#endif

getIterAtLine (GtkEditorBuffer sb) line = fmap GtkEditorIter $ Gtk.textBufferGetIterAtLine sb line
#ifdef YI
getIterAtLine (YiEditorBuffer b) line = return YiEditorIter -- TODO
#endif

getIterAtMark (GtkEditorBuffer sb) (GtkEditorMark m) = fmap GtkEditorIter $ Gtk.textBufferGetIterAtMark sb m
#ifdef YI
getIterAtMark (YiEditorBuffer sb) (YiEditorMark) = return YiEditorIter -- TODO
getIterAtMark _ _ = fail "Mismatching TextEditor types in getIterAtMark" -- TODO
#endif

getIterAtOffset (GtkEditorBuffer sb) offset = fmap GtkEditorIter $ Gtk.textBufferGetIterAtOffset sb offset
#ifdef YI
getIterAtOffset (YiEditorBuffer b) offset = return YiEditorIter -- TODO
#endif

getLineCount (GtkEditorBuffer sb) = Gtk.textBufferGetLineCount sb
#ifdef YI
getLineCount (YiEditorBuffer b) = return 0 -- TODO
#endif

getModified (GtkEditorBuffer sb) = Gtk.textBufferGetModified sb
#ifdef YI
getModified (YiEditorBuffer b) = return False -- TODO
#endif

getSelectionBoundMark (GtkEditorBuffer sb) = fmap GtkEditorMark $ Gtk.textBufferGetSelectionBound sb
#ifdef YI
getSelectionBoundMark (YiEditorBuffer b) = return YiEditorMark -- TODO
#endif

getSelectionBounds (GtkEditorBuffer sb) = do
    (first, last) <- Gtk.textBufferGetSelectionBounds sb
    return (GtkEditorIter first, GtkEditorIter last)
#ifdef YI
getSelectionBounds (YiEditorBuffer b) = return (YiEditorIter, YiEditorIter) -- TODO
#endif

getSlice (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars =
    Gtk.textBufferGetSlice sb first last includeHidenChars
#ifdef YI
getSlice (YiEditorBuffer b) (YiEditorIter) (YiEditorIter) includeHidenChars = return "" -- TODO
getSlice _ _ _ _ = fail "Mismatching TextEditor types in getSlice"
#endif

getStartIter (GtkEditorBuffer sb) = fmap GtkEditorIter $ Gtk.textBufferGetStartIter sb
#ifdef YI
getStartIter (YiEditorBuffer b) = return YiEditorIter -- TODO
#endif

getTagTable (GtkEditorBuffer sb) = fmap GtkEditorTagTable $ Gtk.textBufferGetTagTable sb
#ifdef YI
getTagTable (YiEditorBuffer b) = return YiEditorTagTable -- TODO
#endif

getText (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars =
    Gtk.textBufferGetText sb first last includeHidenChars
#ifdef YI
getText (YiEditorBuffer b) (YiEditorIter) (YiEditorIter) includeHidenChars = return "" -- TODO
getText _ _ _ _ = fail "Mismatching TextEditor types in getText"
#endif

hasSelection (GtkEditorBuffer sb) = Gtk.textBufferHasSelection sb
#ifdef YI
hasSelection (YiEditorBuffer b) = return False -- TODO
#endif

insert (GtkEditorBuffer sb) (GtkEditorIter i) text = Gtk.textBufferInsert sb i text
#ifdef YI
insert (YiEditorBuffer b) (YiEditorIter) text = return () -- TODO
insert _ _ _ = fail "Mismatching TextEditor types in insert"
#endif

moveMark (GtkEditorBuffer sb) (GtkEditorMark m) (GtkEditorIter i) = Gtk.textBufferMoveMark sb m i
#ifdef YI
moveMark (YiEditorBuffer b) (YiEditorMark) (YiEditorIter) = return () -- TODO
moveMark _ _ _ = fail "Mismatching TextEditor types in moveMark"
#endif

newView (GtkEditorBuffer sb) = do
    sv <- Gtk.sourceViewNewWithBuffer sb
    Gtk.sourceViewSetHighlightCurrentLine sv True
    Gtk.sourceViewSetInsertSpacesInsteadOfTabs sv True
    Gtk.sourceViewSetIndentOnTab sv True
    Gtk.sourceViewSetAutoIndent sv True
    Gtk.sourceViewSetSmartHomeEnd sv Gtk.SourceSmartHomeEndBefore
    sw <- Gtk.scrolledWindowNew Nothing Nothing
    Gtk.containerAdd sw sv
    return (GtkEditorView sv)
#ifdef YI
newView (YiEditorBuffer b) = fmap YiEditorView $ Yi.newView b
#endif

pasteClipboard (GtkEditorBuffer sb) clipboard (GtkEditorIter i) defaultEditable =
    Gtk.textBufferPasteClipboard sb clipboard i defaultEditable
#ifdef YI
pasteClipboard (YiEditorBuffer b) clipboard (YiEditorIter) defaultEditable = return () -- TODO
pasteClipboard _ _ _ _ = fail "Mismatching TextEditor types in pasteClipboard"
#endif

placeCursor (GtkEditorBuffer sb) (GtkEditorIter i) = Gtk.textBufferPlaceCursor sb i
#ifdef YI
placeCursor (YiEditorBuffer b) (YiEditorIter) = return () -- TODO
placeCursor _ _ = fail "Mismatching TextEditor types in placeCursor"
#endif

redo (GtkEditorBuffer sb) = Gtk.sourceBufferRedo sb
#ifdef YI
redo (YiEditorBuffer b) = return () -- TODO
#endif

removeTagByName (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferRemoveTagByName sb name first last
#ifdef YI
removeTagByName (YiEditorBuffer b) name (YiEditorIter) (YiEditorIter) = return () -- TODO
removeTagByName _ _ _ _ = fail "Mismatching TextEditor types in removeTagByName"
#endif

selectRange (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferSelectRange sb first last
#ifdef YI
selectRange (YiEditorBuffer b) (YiEditorIter) (YiEditorIter) = return () -- TODO
selectRange _ _ _ = fail "Mismatching TextEditor types in selectRange"
#endif

setModified (GtkEditorBuffer sb) modified = Gtk.textBufferSetModified sb modified >> return ()
#ifdef YI
setModified (YiEditorBuffer b) modified = return () -- TODO
#endif

setStyle (GtkEditorBuffer sb) mbStyle = do
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

setText (GtkEditorBuffer sb) text = Gtk.textBufferSetText sb text
#ifdef YI
setText (YiEditorBuffer b) text =
    writeIORef (Yi.fBufRef b) $ Yi.newB 0 (Left "*leksah*") (Yi.fromString text) -- TODO
#endif

undo (GtkEditorBuffer sb) = Gtk.sourceBufferUndo sb
#ifdef YI
undo (YiEditorBuffer b) = return () -- TODO
#endif

-- View
bufferToWindowCoords (GtkEditorView sv) point = Gtk.textViewBufferToWindowCoords sv Gtk.TextWindowWidget point
#ifdef YI
bufferToWindowCoords (YiEditorView v) point = return point -- TODO
#endif

getBuffer (GtkEditorView sv) = fmap (GtkEditorBuffer . Gtk.castToSourceBuffer) $ sv `Gtk.get` Gtk.textViewBuffer
#ifdef YI
getBuffer (YiEditorView v) = return $ YiEditorBuffer $ Yi.getBuffer v
#endif

getDrawWindow (GtkEditorView sv) = Gtk.widgetGetDrawWindow sv
#ifdef YI
getDrawWindow (YiEditorView v) = Gtk.widgetGetDrawWindow (Yi.drawArea v)
#endif

getIterLocation (GtkEditorView sv) (GtkEditorIter i) = Gtk.textViewGetIterLocation sv i
#ifdef YI
getIterLocation (YiEditorView v) (YiEditorIter) = return $ Gtk.Rectangle 0 0 0 0 -- TODO
getIterLocation _ _ = fail "Mismatching TextEditor types in getIterLocation"
#endif

getOverwrite (GtkEditorView sv) = Gtk.textViewGetOverwrite sv
#ifdef YI
getOverwrite (YiEditorView v) = return False -- TODO
#endif

getScrolledWindow (GtkEditorView sv) = fmap (Gtk.castToScrolledWindow . fromJust) $ Gtk.widgetGetParent sv
#ifdef YI
getScrolledWindow (YiEditorView v) = return $ Yi.scrollWin v
#endif

grabFocus (GtkEditorView sv) = Gtk.widgetGrabFocus sv
#ifdef YI
grabFocus (YiEditorView v) = return () -- TODO
#endif

scrollToMark (GtkEditorView sv) (GtkEditorMark m) withMargin mbAlign = Gtk.textViewScrollToMark sv m withMargin mbAlign
#ifdef YI
scrollToMark (YiEditorView v) (YiEditorMark) withMargin mbAlign = return () -- TODO
scrollToMark _ _ _ _ = fail "Mismatching TextEditor types in scrollToMark"
#endif

scrollToIter (GtkEditorView sv) (GtkEditorIter i) withMargin mbAlign = Gtk.textViewScrollToIter sv i withMargin mbAlign >> return ()
#ifdef YI
scrollToIter (YiEditorView v) (YiEditorIter) withMargin mbAlign = return () -- TODO
scrollToIter _ _ _ _ = fail "Mismatching TextEditor types in scrollToIter"
#endif

setFont (GtkEditorView sv) mbFontString = do
    fd <- case mbFontString of
        Just str -> do
            Gtk.fontDescriptionFromString str
        Nothing -> do
            f <- Gtk.fontDescriptionNew
            Gtk.fontDescriptionSetFamily f "Monospace"
            return f
    Gtk.widgetModifyFont sv (Just fd)
#ifdef YI
setFont (YiEditorView v) mbFontString = do
    fd <- case mbFontString of
        Just str -> do
            Gtk.fontDescriptionFromString str
        Nothing -> do
            f <- Gtk.fontDescriptionNew
            Gtk.fontDescriptionSetFamily f "Monospace"
            return f
    Gtk.layoutSetFontDescription (Yi.layout v) (Just fd)
#endif

setIndentWidth (GtkEditorView sv) width = Gtk.sourceViewSetIndentWidth sv width
#ifdef YI
setIndentWidth (YiEditorView v) width = return () -- TODO
#endif

setRightMargin (GtkEditorView sv) mbRightMargin = do
    case mbRightMargin of
        Just n -> do
            Gtk.sourceViewSetShowRightMargin sv True
            Gtk.sourceViewSetRightMarginPosition sv (fromIntegral n)
        Nothing -> Gtk.sourceViewSetShowRightMargin sv False
#ifdef YI
setRightMargin (YiEditorView v) mbRightMargin = return () -- TODO
#endif

setShowLineNumbers (GtkEditorView sv) show = Gtk.sourceViewSetShowLineNumbers sv show
#ifdef YI
setShowLineNumbers (YiEditorView v) show = return () -- TODO
#endif

setTabWidth (GtkEditorView sv) width = Gtk.sourceViewSetTabWidth sv width
#ifdef YI
setTabWidth (YiEditorView v) width = return () -- TODO
#endif

-- Iterator
backwardChar (GtkEditorIter i) = Gtk.textIterBackwardChar i
#ifdef YI
backwardChar (YiEditorIter) = return False -- TODO
#endif

backwardFindChar (GtkEditorIter i) pred mbLimit = Gtk.textIterBackwardFindChar i pred (
    case mbLimit of
        Just (GtkEditorIter limit) -> Just limit
        Nothing                    -> Nothing
        _                          -> fail "Mismatching TextEditor types in backwardFindChar")
#ifdef YI
backwardFindChar (YiEditorIter) pred mbLimit = return False -- TODO
#endif

backwardWordStart (GtkEditorIter i) = Gtk.textIterBackwardWordStart i
#ifdef YI
backwardWordStart (YiEditorIter) = return False -- TODO
#endif

copyIter (GtkEditorIter i) = fmap GtkEditorIter $ Gtk.textIterCopy i
#ifdef YI
copyIter (YiEditorIter) = return YiEditorIter -- TODO
#endif

endsWord (GtkEditorIter i) = Gtk.textIterEndsWord i
#ifdef YI
endsWord (YiEditorIter) = return False -- TODO
#endif

forwardChar (GtkEditorIter i) = Gtk.textIterForwardChar i
#ifdef YI
forwardChar (YiEditorIter) = return False -- TODO
#endif

forwardChars (GtkEditorIter i) n = Gtk.textIterForwardChars i n
#ifdef YI
forwardChars (YiEditorIter) n = return False -- TODO
#endif

forwardFindChar (GtkEditorIter i) pred mbLimit = Gtk.textIterForwardFindChar i pred (
    case mbLimit of
        Just (GtkEditorIter limit) -> Just limit
        Nothing                    -> Nothing
        _                          -> fail "Mismatching TextEditor types in backwardFindChar")
#ifdef YI
forwardFindChar (YiEditorIter) pred mbLimit = return False -- TODO
#endif

forwardSearch (GtkEditorIter i) str flags mbLimit =
    fmap (fmap (\(start, end) -> (GtkEditorIter start, GtkEditorIter end))) $
        Gtk.textIterForwardSearch i str flags (
            case mbLimit of
                Just (GtkEditorIter limit) -> Just limit
                Nothing                    -> Nothing
                _                          -> fail "Mismatching TextEditor types in backwardFindChar")
#ifdef YI
forwardSearch (YiEditorIter) str pred mbLimit = return Nothing -- TODO
#endif

forwardToLineEnd (GtkEditorIter i) = Gtk.textIterForwardChar i
#ifdef YI
forwardToLineEnd (YiEditorIter) = return False -- TODO
#endif

forwardWordEnd (GtkEditorIter i) = Gtk.textIterForwardWordEnd i
#ifdef YI
forwardWordEnd (YiEditorIter) = return False -- TODO
#endif

getChar (GtkEditorIter i) = Gtk.textIterGetChar i
#ifdef YI
getChar (YiEditorIter) = return Nothing -- TODO
#endif

getCharsInLine (GtkEditorIter i) = Gtk.textIterGetCharsInLine i
#ifdef YI
getCharsInLine (YiEditorIter) = return 0 -- TODO
#endif

getLine (GtkEditorIter i) = Gtk.textIterGetLine i
#ifdef YI
getLine (YiEditorIter) = return 0 -- TODO
#endif

getLineOffset (GtkEditorIter i) = Gtk.textIterGetLineOffset i
#ifdef YI
getLineOffset (YiEditorIter) = return 0 -- TODO
#endif

getOffset (GtkEditorIter i) = Gtk.textIterGetOffset i
#ifdef YI
getOffset (YiEditorIter) = return 0 -- TODO
#endif

isStart (GtkEditorIter i) = Gtk.textIterIsStart i
#ifdef YI
isStart (YiEditorIter) = return False -- TODO
#endif

isEnd (GtkEditorIter i) = Gtk.textIterIsEnd i
#ifdef YI
isEnd (YiEditorIter) = return False -- TODO
#endif

iterEqual (GtkEditorIter i1) (GtkEditorIter i2) = Gtk.textIterEqual i1 i2
#ifdef YI
iterEqual (YiEditorIter) (YiEditorIter) = return False -- TODO
iterEqual _ _ = fail "Mismatching TextEditor types in iterEqual"
#endif

startsLine (GtkEditorIter i) = Gtk.textIterStartsLine i
#ifdef YI
startsLine (YiEditorIter) = return False -- TODO
#endif

setLine (GtkEditorIter i) line = Gtk.textIterSetLine i line
#ifdef YI
setLine (YiEditorIter) line = return () -- TODO
#endif

setLineOffset (GtkEditorIter i) column = Gtk.textIterSetLineOffset i column
#ifdef YI
setLineOffset (YiEditorIter) column = return () -- TODO
#endif

setOffset (GtkEditorIter i) offset = Gtk.textIterSetOffset i offset
#ifdef YI
setOffset (YiEditorIter) offset = return () -- TODO
#endif

-- Tag Table
newTag (GtkEditorTagTable tt) name = do
    t <- Gtk.textTagNew (Just name)
    Gtk.textTagTableAdd tt t
    return $ GtkEditorTag t
#ifdef YI
newTag (YiEditorTagTable) name = return YiEditorTag -- TODO
#endif

lookupTag (GtkEditorTagTable tt) name = fmap (fmap GtkEditorTag) $ Gtk.textTagTableLookup tt name
#ifdef YI
lookupTag (YiEditorTagTable) name = return Nothing -- TODO
#endif

-- Tag
background (GtkEditorTag t) color = Gtk.set t [Gtk.textTagBackground := colorHexString color]
#ifdef YI
background (YiEditorTag) color = return () -- TODO
#endif

underline (GtkEditorTag t) value = Gtk.set t [Gtk.textTagUnderline := value]
#ifdef YI
underline (YiEditorTag) value = return () -- TODO
#endif

-- Events
afterFocusIn (GtkEditorView sv) f = do
    id1 <- sv `Gtk.afterFocusIn` \_ -> f >> return False
    return [ConnectC id1]
#ifdef YI
afterFocusIn (YiEditorView v) f = do
    id1 <- (Yi.drawArea v) `Gtk.afterFocusIn` \_ -> f >> return False
    return [ConnectC id1]
#endif

afterModifiedChanged (GtkEditorBuffer sb) f = do
    id1 <- sb `Gtk.afterModifiedChanged` f
    return [ConnectC id1]
#ifdef YI
afterModifiedChanged (YiEditorBuffer b) f = return [] -- TODO
#endif

afterMoveCursor (GtkEditorView sv) f = do
    GtkEditorBuffer sb <- getBuffer (GtkEditorView sv)
    id1 <- sv `Gtk.afterMoveCursor` \_ _ _ -> f
    sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
    id2 <- sv `Gtk.onButtonRelease` \_ -> f >> return False
    id3 <- sb `Gtk.afterEndUserAction` f
    return [ConnectC id1, ConnectC id2, ConnectC id3]
#ifdef YI
afterMoveCursor (YiEditorView v) f = return [] -- TODO
#endif

afterToggleOverwrite (GtkEditorView sv) f = do
    id1 <- sv `Gtk.afterToggleOverwrite` f
    return [ConnectC id1]
#ifdef YI
afterToggleOverwrite (YiEditorView v) f = return [] -- TODO
#endif

onButtonPress (GtkEditorView sv) f = do
    id1 <- sv `Gtk.onButtonPress` f
    return [ConnectC id1]
#ifdef YI
onButtonPress (YiEditorView v) f = do
    id1 <- (Yi.drawArea v) `Gtk.onButtonPress` f
    return [ConnectC id1]
#endif

onCompletion (GtkEditorView sv) start cancel = do
    (GtkEditorBuffer sb) <- getBuffer (GtkEditorView sv)
    id1 <- sb `Gtk.afterBufferInsertText` \iter text ->
        if (all (\c -> (isAlphaNum c) || (c == '.') || (c == '_')) text)
            then do
                hasSel <- hasSelection (GtkEditorBuffer sb)
                if not hasSel
                    then do
                        (iterC, _) <- getSelectionBounds (GtkEditorBuffer sb)
                        atC <- iterEqual (GtkEditorIter iter) iterC
                        when atC $ start
                    else
                        cancel
            else
                cancel
    id2 <- sv `Gtk.onMoveCursor` \_ _ _ -> cancel
    id3 <- sv `Gtk.onButtonPress` \_ -> cancel >> return False
    return [ConnectC id1]
#ifdef YI
onCompletion (YiEditorView v) start cancel = return [] -- TODO
#endif

onKeyPress (GtkEditorView sv) f = do
    id1 <- sv `Gtk.on` Gtk.keyPressEvent $ do
        name        <- Gtk.eventKeyName
        modifier    <- Gtk.eventModifier
        keyVal      <- Gtk.eventKeyVal
        liftIO $ f name modifier keyVal
    return [ConnectC id1]
#ifdef YI
onKeyPress (YiEditorView v) f = do
    id1 <- (Yi.drawArea v) `Gtk.on` Gtk.keyPressEvent $ do
        name        <- Gtk.eventKeyName
        modifier    <- Gtk.eventModifier
        keyVal      <- Gtk.eventKeyVal
        liftIO $ f name modifier keyVal
    return [ConnectC id1]
#endif

onKeyRelease (GtkEditorView sv) f = do
    id1 <- sv `Gtk.on` Gtk.keyReleaseEvent $ do
        name        <- Gtk.eventKeyName
        modifier    <- Gtk.eventModifier
        keyVal      <- Gtk.eventKeyVal
        liftIO $ f name modifier keyVal
    return [ConnectC id1]
#ifdef YI
onKeyRelease (YiEditorView v) f = do
    id1 <- (Yi.drawArea v) `Gtk.on` Gtk.keyReleaseEvent $ do
        name        <- Gtk.eventKeyName
        modifier    <- Gtk.eventModifier
        keyVal      <- Gtk.eventKeyVal
        liftIO $ f name modifier keyVal
    return [ConnectC id1]
#endif

onLookupInfo (GtkEditorView sv) f = do
    sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
    id1 <- sv `Gtk.onButtonRelease` \e -> when (controlIsPressed e) f >> return False
    return [ConnectC id1]
#ifdef YI
onLookupInfo (YiEditorView v) f = do
    (Yi.drawArea v) `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
    id1 <- (Yi.drawArea v) `Gtk.onButtonRelease` \e -> when (controlIsPressed e) f >> return False
    return [ConnectC id1]
#endif

onPopulatePopup (GtkEditorView sv) f = do
    id1 <- sv `Gtk.onPopulatePopup` f
    return [ConnectC id1]
#ifdef YI
onPopulatePopup (YiEditorView v) f = do
    id1 <- (Yi.drawArea v) `Gtk.onPopupMenu` do
         menu <- Gtk.menuNew
         f menu
         Gtk.menuPopup menu Nothing
    return [ConnectC id1]
#endif



