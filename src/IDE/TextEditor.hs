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
import Control.Monad (when)
import Control.Monad.Reader (liftIO)

import qualified Graphics.UI.Gtk as Gtk hiding(afterToggleOverwrite)
import qualified Graphics.UI.Gtk.SourceView as Gtk
import qualified Graphics.UI.Gtk.Multiline.TextView as Gtk
import qualified Graphics.UI.Gtk.Gdk.EventM as Gtk
import System.Glib.Attributes (AttrOp(..))

import Graphics.UI.Frame.Panes (Connection(..))
import IDE.Core.Types (colorHexString)
import IDE.Core.State (controlIsPressed)

-- Data types
data EditorBuffer = GtkEditorBuffer Gtk.SourceBuffer
data EditorView = GtkEditorView Gtk.SourceView
data EditorMark = GtkEditorMark Gtk.TextMark
data EditorIter = GtkEditorIter Gtk.TextIter
data EditorTagTable = GtkEditorTagTable Gtk.TextTagTable
data EditorTag = GtkEditorTag Gtk.TextTag

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

applyTagByName (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferApplyTagByName sb name first last
beginNotUndoableAction (GtkEditorBuffer sb) = Gtk.sourceBufferBeginNotUndoableAction sb
beginUserAction (GtkEditorBuffer sb) = Gtk.textBufferBeginUserAction sb
canRedo (GtkEditorBuffer sb) = Gtk.sourceBufferGetCanRedo sb
canUndo (GtkEditorBuffer sb) = Gtk.sourceBufferGetCanUndo sb
copyClipboard (GtkEditorBuffer sb) clipboard = Gtk.textBufferCopyClipboard sb clipboard
createMark (GtkEditorBuffer sb) mbName (GtkEditorIter i) leftGravity =
    fmap GtkEditorMark $ Gtk.textBufferCreateMark sb mbName i leftGravity
cutClipboard (GtkEditorBuffer sb) clipboard defaultEditable = Gtk.textBufferCutClipboard sb clipboard defaultEditable
delete (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferDelete sb first last
deleteSelection (GtkEditorBuffer sb) interactive defaultEditable =
    Gtk.textBufferDeleteSelection sb interactive defaultEditable
endNotUndoableAction (GtkEditorBuffer sb) = Gtk.sourceBufferEndNotUndoableAction sb
endUserAction (GtkEditorBuffer sb) = Gtk.textBufferEndUserAction sb
getEndIter (GtkEditorBuffer sb) = fmap GtkEditorIter $ Gtk.textBufferGetEndIter sb
getInsertMark (GtkEditorBuffer sb) = fmap GtkEditorMark $ Gtk.textBufferGetInsert sb
getIterAtLine (GtkEditorBuffer sb) line = fmap GtkEditorIter $ Gtk.textBufferGetIterAtLine sb line
getIterAtMark (GtkEditorBuffer sb) (GtkEditorMark m) = fmap GtkEditorIter $ Gtk.textBufferGetIterAtMark sb m
getIterAtOffset (GtkEditorBuffer sb) offset = fmap GtkEditorIter $ Gtk.textBufferGetIterAtOffset sb offset
getLineCount (GtkEditorBuffer sb) = Gtk.textBufferGetLineCount sb
getModified (GtkEditorBuffer sb) = Gtk.textBufferGetModified sb
getSelectionBoundMark (GtkEditorBuffer sb) = fmap GtkEditorMark $ Gtk.textBufferGetSelectionBound sb
getSelectionBounds (GtkEditorBuffer sb) = do
    (first, last) <- Gtk.textBufferGetSelectionBounds sb
    return (GtkEditorIter first, GtkEditorIter last)
getSlice (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars =
    Gtk.textBufferGetSlice sb first last includeHidenChars
getStartIter (GtkEditorBuffer sb) = fmap GtkEditorIter $ Gtk.textBufferGetStartIter sb
getTagTable (GtkEditorBuffer sb) = fmap GtkEditorTagTable $ Gtk.textBufferGetTagTable sb
getText (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) includeHidenChars =
    Gtk.textBufferGetText sb first last includeHidenChars
hasSelection (GtkEditorBuffer sb) = Gtk.textBufferHasSelection sb
insert (GtkEditorBuffer sb) (GtkEditorIter i) text = Gtk.textBufferInsert sb i text
moveMark (GtkEditorBuffer sb) (GtkEditorMark m) (GtkEditorIter i) = Gtk.textBufferMoveMark sb m i
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
pasteClipboard (GtkEditorBuffer sb) clipboard (GtkEditorIter i) defaultEditable =
    Gtk.textBufferPasteClipboard sb clipboard i defaultEditable
placeCursor (GtkEditorBuffer sb) (GtkEditorIter i) = Gtk.textBufferPlaceCursor sb i
redo (GtkEditorBuffer sb) = Gtk.sourceBufferRedo sb
removeTagByName (GtkEditorBuffer sb) name (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferRemoveTagByName sb name first last
selectRange (GtkEditorBuffer sb) (GtkEditorIter first) (GtkEditorIter last) =
    Gtk.textBufferSelectRange sb first last
setModified (GtkEditorBuffer sb) = Gtk.textBufferSetModified sb
setStyle (GtkEditorBuffer sb) mbStyle = do
    case mbStyle of
        Nothing  -> return ()
        Just str -> do
            styleManager <- Gtk.sourceStyleSchemeManagerNew
            ids <- Gtk.sourceStyleSchemeManagerGetSchemeIds styleManager
            when (elem str ids) $ do
                scheme <- Gtk.sourceStyleSchemeManagerGetScheme styleManager str
                Gtk.sourceBufferSetStyleScheme sb scheme
setText (GtkEditorBuffer sb) text = Gtk.textBufferSetText sb text
undo (GtkEditorBuffer sb) = Gtk.sourceBufferUndo sb

-- View
bufferToWindowCoords (GtkEditorView sv) point = Gtk.textViewBufferToWindowCoords sv Gtk.TextWindowWidget point
getBuffer (GtkEditorView sv) = fmap (GtkEditorBuffer . Gtk.castToSourceBuffer) $ sv `Gtk.get` Gtk.textViewBuffer
getDrawWindow (GtkEditorView sv) = Gtk.widgetGetDrawWindow sv
getIterLocation (GtkEditorView sv) (GtkEditorIter i) = Gtk.textViewGetIterLocation sv i
getOverwrite (GtkEditorView sv) = Gtk.textViewGetOverwrite sv
getScrolledWindow (GtkEditorView sv) = fmap (Gtk.castToScrolledWindow . fromJust) $ Gtk.widgetGetParent sv
grabFocus (GtkEditorView sv) = Gtk.widgetGrabFocus sv
scrollToMark (GtkEditorView sv) (GtkEditorMark m) withMargin mbAlign = Gtk.textViewScrollToMark sv m withMargin mbAlign
scrollToIter (GtkEditorView sv) (GtkEditorIter i) withMargin mbAlign = Gtk.textViewScrollToIter sv i withMargin mbAlign
setFont (GtkEditorView sv) mbFontString = do
    fd <- case mbFontString of
        Just str -> do
            Gtk.fontDescriptionFromString str
        Nothing -> do
            f <- Gtk.fontDescriptionNew
            Gtk.fontDescriptionSetFamily f "Monospace"
            return f
    Gtk.widgetModifyFont sv (Just fd)
setIndentWidth (GtkEditorView sv) width = Gtk.sourceViewSetIndentWidth sv width
setRightMargin (GtkEditorView sv) mbRightMargin = do
    case mbRightMargin of
        Just n -> do
            Gtk.sourceViewSetShowRightMargin sv True
            Gtk.sourceViewSetRightMarginPosition sv (fromIntegral n)
        Nothing -> Gtk.sourceViewSetShowRightMargin sv False
setShowLineNumbers (GtkEditorView sv) show = Gtk.sourceViewSetShowLineNumbers sv show
setTabWidth (GtkEditorView sv) width = Gtk.sourceViewSetTabWidth sv width

-- Iterator
backwardChar (GtkEditorIter i) = Gtk.textIterBackwardChar i
backwardFindChar (GtkEditorIter i) pred mbLimit = Gtk.textIterBackwardFindChar i pred (
    case mbLimit of
        Just (GtkEditorIter limit) -> Just limit
        Nothing                    -> Nothing)
backwardWordStart (GtkEditorIter i) = Gtk.textIterBackwardWordStart i
copyIter (GtkEditorIter i) = fmap GtkEditorIter $ Gtk.textIterCopy i
endsWord (GtkEditorIter i) = Gtk.textIterEndsWord i
forwardChar (GtkEditorIter i) = Gtk.textIterForwardChar i
forwardChars (GtkEditorIter i) n = Gtk.textIterForwardChars i n
forwardFindChar (GtkEditorIter i) pred mbLimit = Gtk.textIterForwardFindChar i pred (
    case mbLimit of
        Just (GtkEditorIter limit) -> Just limit
        Nothing                    -> Nothing)
forwardSearch (GtkEditorIter i) str flags mbLimit =
    fmap (fmap (\(start, end) -> (GtkEditorIter start, GtkEditorIter end))) $
        Gtk.textIterForwardSearch i str flags (
            case mbLimit of
                Just (GtkEditorIter limit) -> Just limit
                Nothing                    -> Nothing)
forwardToLineEnd (GtkEditorIter i) = Gtk.textIterForwardChar i
forwardWordEnd (GtkEditorIter i) = Gtk.textIterForwardWordEnd i
getChar (GtkEditorIter i) = Gtk.textIterGetChar i
getCharsInLine (GtkEditorIter i) = Gtk.textIterGetCharsInLine i
getLine (GtkEditorIter i) = Gtk.textIterGetLine i
getLineOffset (GtkEditorIter i) = Gtk.textIterGetLineOffset i
getOffset (GtkEditorIter i) = Gtk.textIterGetOffset i
isStart (GtkEditorIter i) = Gtk.textIterIsStart i
isEnd (GtkEditorIter i) = Gtk.textIterIsEnd i
iterEqual (GtkEditorIter i1) (GtkEditorIter i2) = Gtk.textIterEqual i1 i2
startsLine (GtkEditorIter i) = Gtk.textIterStartsLine i
setLine (GtkEditorIter i) line = Gtk.textIterSetLine i line
setLineOffset (GtkEditorIter i) column = Gtk.textIterSetLineOffset i column
setOffset (GtkEditorIter i) offset = Gtk.textIterSetOffset i offset

-- Tag Table
newTag (GtkEditorTagTable tt) name = do
    t <- Gtk.textTagNew (Just name)
    Gtk.textTagTableAdd tt t
    return $ GtkEditorTag t
lookupTag (GtkEditorTagTable tt) name = fmap (fmap GtkEditorTag) $ Gtk.textTagTableLookup tt name

-- Tag
background (GtkEditorTag t) color = Gtk.set t [Gtk.textTagBackground := colorHexString color]
underline (GtkEditorTag t) value = Gtk.set t [Gtk.textTagUnderline := value]

-- Events
afterFocusIn (GtkEditorView sv) f = do
    id1 <- sv `Gtk.afterFocusIn` \_ -> f >> return False
    return [ConnectC id1]

afterModifiedChanged (GtkEditorBuffer sb) f = do
    id1 <- sb `Gtk.afterModifiedChanged` f
    return [ConnectC id1]

afterMoveCursor (GtkEditorView sv) f = do
    GtkEditorBuffer sb <- getBuffer (GtkEditorView sv)
    id1 <- sv `Gtk.afterMoveCursor` \_ _ _ -> f
    sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
    id2 <- sv `Gtk.onButtonRelease` \_ -> f >> return False
    id3 <- sb `Gtk.afterEndUserAction` f
    return [ConnectC id1, ConnectC id2, ConnectC id3]

afterToggleOverwrite (GtkEditorView sv) f = do
    id1 <- sv `Gtk.afterToggleOverwrite` f
    return [ConnectC id1]

onButtonPress (GtkEditorView sv) f = do
    id1 <- sv `Gtk.onButtonPress` f
    return [ConnectC id1]

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

onKeyPress (GtkEditorView sv) f = do
    id1 <- sv `Gtk.on` Gtk.keyPressEvent $ do
        name        <- Gtk.eventKeyName
        modifier    <- Gtk.eventModifier
        keyVal      <- Gtk.eventKeyVal
        liftIO $ f name modifier keyVal
    return [ConnectC id1]

onKeyRelease (GtkEditorView sv) f = do
    id1 <- sv `Gtk.on` Gtk.keyReleaseEvent $ do
        name        <- Gtk.eventKeyName
        modifier    <- Gtk.eventModifier
        keyVal      <- Gtk.eventKeyVal
        liftIO $ f name modifier keyVal
    return [ConnectC id1]

onLookupInfo (GtkEditorView sv) f = do
    sv `Gtk.widgetAddEvents` [Gtk.ButtonReleaseMask]
    id1 <- sv `Gtk.onButtonRelease` \e -> when (controlIsPressed e) f >> return False
    return [ConnectC id1]

onPopulatePopup (GtkEditorView sv) f = do
    id1 <- sv `Gtk.onPopulatePopup` f
    return [ConnectC id1]


