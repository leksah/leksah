{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor.Class
-- Copyright   :  2007-2013 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.TextEditor.Class (
    TextEditor(..)
  , EditorStyle(..)
  , updateStyle
  , scrollToCursor
) where

import Prelude ()
import Prelude.Compat
import qualified IDE.Core.Types as Core (IDEM)
import IDE.Core.Types
       (LogRefType, IDE(..), IDERef, IDEEventM, EditorStyle(..),
        Prefs (..), editorStyle)
import Graphics.UI.Editor.Basics (Connection)
import Control.Monad.Reader (ReaderT(..))
import Foreign (Ptr)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import IDE.Core.State (readIDE)
import GI.Gtk.Objects.Clipboard (Clipboard(..))
import qualified GI.Gdk as Gdk (Window)
import GI.Gdk
       (EventCrossing, EventMotion, EventKey, EventButton)
import Graphics.UI.Frame.Rectangle (Rectangle)
import GI.Gtk.Objects.ScrolledWindow (ScrolledWindow(..))
import GI.Gtk.Objects.Widget (Widget(..))
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gtk.Flags (TextSearchFlags)
import Graphics.UI.Editor.Simple (Color(..))
import GI.Pango (Underline)
import GHC.Stack (HasCallStack)
import GI.Gtk (Grid)

type IDEM a = HasCallStack => Core.IDEM a
type IDEAction = IDEM ()

updateStyle :: TextEditor editor => EditorBuffer editor -> IDEAction
updateStyle ebuf = do
    prefs <- readIDE prefs
    setStyle ebuf $ editorStyle (darkUserInterface prefs) prefs

-- | Scrolls the editor to the cursor if necessary
scrollToCursor :: TextEditor editor => EditorView editor -> IDEAction
scrollToCursor view = do
    buf  <- getBuffer view
    iter <- getInsertIter buf
    scrollToIter view iter 0.0 Nothing

class TextEditor editor where
    data EditorBuffer editor
    data EditorView editor
    data EditorMark editor
    data EditorIter editor
    data EditorTagTable editor
    data EditorTag editor

    newBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer editor)
    applyTagByName :: EditorBuffer editor
                      -> Text
                      -> EditorIter editor
                      -> EditorIter editor
                      -> IDEM ()
    beginNotUndoableAction :: EditorBuffer editor -> IDEM ()
    beginUserAction :: EditorBuffer editor -> IDEM ()
    canRedo :: EditorBuffer editor -> IDEM Bool
    canUndo :: EditorBuffer editor -> IDEM Bool
    copyClipboard :: EditorBuffer editor -> Clipboard -> IDEM ()
    createMark :: EditorView editor
                  -> LogRefType
                  -> EditorIter editor
                  -> Text
                  -> IDEM ()
    cutClipboard :: EditorBuffer editor -> Clipboard -> Bool -> IDEM ()
    delete :: EditorBuffer editor -> EditorIter editor -> EditorIter editor -> IDEM ()
    deleteSelection :: EditorBuffer editor -> IDEM ()
    endNotUndoableAction :: EditorBuffer editor -> IDEM ()
    endUserAction :: EditorBuffer editor -> IDEM ()
    getEndIter :: EditorBuffer editor -> IDEM (EditorIter editor)
    getInsertMark :: EditorBuffer editor -> IDEM (EditorMark editor)
    getIterAtLine :: EditorBuffer editor -> Int -> IDEM (EditorIter editor)
    getIterAtMark :: EditorBuffer editor -> EditorMark editor -> IDEM (EditorIter editor)
    getIterAtOffset :: EditorBuffer editor -> Int -> IDEM (EditorIter editor)
    getLineCount :: EditorBuffer editor -> IDEM Int
    getModified :: EditorBuffer editor -> IDEM Bool
    getSelectionBoundMark :: EditorBuffer editor -> IDEM (EditorMark editor)
    getSelectionBounds :: EditorBuffer editor -> IDEM (EditorIter editor, EditorIter editor)
    getInsertIter :: EditorBuffer editor -> IDEM (EditorIter editor)
    getSlice :: EditorBuffer editor
                -> EditorIter editor
                -> EditorIter editor
                -> Bool
                -> IDEM Text
    getStartIter :: EditorBuffer editor -> IDEM (EditorIter editor)
    getTagTable :: EditorBuffer editor -> IDEM (EditorTagTable editor)
    getText :: EditorBuffer editor
               -> EditorIter editor
               -> EditorIter editor
               -> Bool
               -> IDEM Text
    hasSelection :: EditorBuffer editor -> IDEM Bool
    insert :: EditorBuffer editor -> EditorIter editor -> Text -> IDEM ()
    newViewWithMap :: EditorBuffer editor -> Maybe Text -> IDEM (EditorView editor, ScrolledWindow, Grid)
    newView :: EditorBuffer editor -> Maybe Text -> IDEM (EditorView editor, ScrolledWindow)
    newViewNoScroll :: EditorBuffer editor -> Maybe Text -> IDEM (EditorView editor, Widget)
    pasteClipboard :: EditorBuffer editor
                      -> Clipboard
                      -> EditorIter editor
                      -> Bool
                      -> IDEM ()
    placeCursor :: EditorBuffer editor -> EditorIter editor -> IDEM ()
    redo :: EditorBuffer editor -> IDEM ()
    removeTagByName :: EditorBuffer editor
                       -> Text
                       -> IDEM ()
    selectRange :: EditorBuffer editor -> EditorIter editor -> EditorIter editor -> IDEM ()
    setModified :: EditorBuffer editor -> Bool -> IDEM ()
    setStyle :: EditorBuffer editor -> EditorStyle -> IDEM ()
    setText :: EditorBuffer editor -> Text -> IDEM ()
    undo :: EditorBuffer editor -> IDEM ()

    -- Events
    afterChanged :: EditorBuffer editor -> IDEM () -> IDEM [Connection]
    afterModifiedChanged :: EditorBuffer editor -> IDEM () -> IDEM [Connection]

    -- View
    bufferToWindowCoords :: EditorView editor -> (Int, Int) -> IDEM (Int, Int)
    drawTabs :: EditorView editor -> IDEM ()
    getBuffer :: EditorView editor -> IDEM (EditorBuffer editor)
    getWindow :: EditorView editor -> IDEM (Maybe Gdk.Window)
    getIterAtLocation :: EditorView editor -> Int -> Int -> IDEM (EditorIter editor)
    getIterLocation :: EditorView editor -> EditorIter editor -> IDEM Rectangle
    getOverwrite :: EditorView editor -> IDEM Bool
    getScrolledWindow :: EditorView editor -> IDEM ScrolledWindow
    getEditorWidget :: EditorView editor -> IDEM Widget
    grabFocus :: EditorView editor -> IDEM ()
    scrollToMark :: EditorView editor
                    -> EditorMark editor
                    -> Double
                    -> Maybe (Double, Double)
                    -> IDEM ()
    -- | Scrolls the editor to the given `EditorIter`
    scrollToIter :: EditorView editor -- ^ The editor view
                 -> EditorIter editor -- ^ The iter
                 -> Double            -- ^ Margin
                 -> Maybe (Double, Double) -- ^ Alignment of the iter, @Just (0,0) is left-top, Just (1.0, 1.0) is right-bottom
                 -> IDEM ()
    setFont :: EditorView editor -> Maybe Text -> IDEM ()
    setIndentWidth :: EditorView editor -> Int -> IDEM ()
    setWrapMode :: EditorView editor -> Bool -> IDEM ()
    setRightMargin :: EditorView editor -> Maybe Int -> IDEM ()
    setShowLineNumbers :: EditorView editor -> Bool -> IDEM ()
    setShowLineMarks :: EditorView editor -> Bool -> IDEM ()
    setHighlightCurrentLine :: EditorView editor -> Bool -> IDEM ()
    setTabWidth :: EditorView editor -> Int -> IDEM ()
    setEditable :: EditorView editor -> Bool -> IDEM ()

    -- Events
    afterFocusIn :: EditorView editor -> IDEM () -> IDEM [Connection]
    afterMoveCursor :: EditorView editor -> IDEM () -> IDEM [Connection]
    afterToggleOverwrite :: EditorView editor -> IDEM () -> IDEM [Connection]
    onButtonPress :: EditorView editor
                     -> IDEEventM EventButton Bool
                     -> IDEM [Connection]
    onButtonRelease :: EditorView editor
                     -> IDEEventM EventButton Bool
                     -> IDEM [Connection]
    onCompletion :: EditorView editor -> IDEM () -> IDEM () -> IDEM [Connection]
    onKeyPress :: EditorView editor
                  -> IDEEventM EventKey Bool
                  -> IDEM [Connection]
    onMotionNotify :: EditorView editor
                  -> IDEEventM EventMotion Bool
                  -> IDEM [Connection]
    onLeaveNotify :: EditorView editor
                  -> IDEEventM EventCrossing Bool
                  -> IDEM [Connection]
    onKeyRelease :: EditorView editor
                    -> IDEEventM EventKey Bool
                    -> IDEM [Connection]
    onLookupInfo :: EditorView editor -> IDEEventM EventButton Bool -> IDEM [Connection]
    onMotionNotifyEvent :: EditorView editor -> IDEEventM EventMotion Bool -> IDEM [Connection]
    onPopulatePopup :: EditorView editor -> (Menu -> IDEM ()) -> IDEM [Connection]
    onSelectionChanged :: EditorBuffer editor -> IDEM () -> IDEM [Connection]

    -- Iter
    backwardCharC :: EditorIter editor -> IDEM (EditorIter editor)
    backwardFindCharC :: EditorIter editor
                        -> (Char -> Bool)
                        -> Maybe (EditorIter editor)
                        -> IDEM (Maybe (EditorIter editor))
    backwardWordStartC :: EditorIter editor -> IDEM (Maybe (EditorIter editor))
    backwardToLineStartC :: EditorIter editor -> IDEM (EditorIter editor)
    endsWord :: EditorIter editor -> IDEM Bool
    forwardCharC :: EditorIter editor -> IDEM (EditorIter editor)
    forwardCharsC :: EditorIter editor -> Int -> IDEM (EditorIter editor)
    forwardFindCharC :: EditorIter editor
                       -> (Char -> Bool)
                       -> Maybe (EditorIter editor)
                       -> IDEM (Maybe (EditorIter editor))
    forwardSearch :: EditorIter editor
                     -> Text
                     -> [TextSearchFlags]
                     -> Maybe (EditorIter editor)
                     -> IDEM (Maybe (EditorIter editor, EditorIter editor))
    forwardToLineEndC :: EditorIter editor -> IDEM (EditorIter editor)
    forwardWordEndC :: EditorIter editor -> IDEM (Maybe (EditorIter editor))
    getChar :: EditorIter editor -> IDEM (Maybe Char)
    getCharsInLine :: EditorIter editor -> IDEM Int
    getLine :: EditorIter editor -> IDEM Int
    getLineOffset :: EditorIter editor -> IDEM Int
    getOffset :: EditorIter editor -> IDEM Int
    isStart :: EditorIter editor -> IDEM Bool
    isEnd :: EditorIter editor -> IDEM Bool
    iterEqual :: EditorIter editor -> EditorIter editor -> IDEM Bool
    startsLine :: EditorIter editor -> IDEM Bool
    startsWord :: EditorIter editor -> IDEM Bool
    atEnd :: EditorIter editor -> IDEM (EditorIter editor)
    atLine :: EditorIter editor -> Int -> IDEM (EditorIter editor)
    atLineOffset :: EditorIter editor -> Int -> IDEM (EditorIter editor)
    atOffset :: EditorIter editor -> Int -> IDEM (EditorIter editor)
    atStart :: EditorIter editor -> IDEM (EditorIter editor)

    -- Tag Table
    newTag :: EditorTagTable editor -> Text -> IDEM (EditorTag editor)
    lookupTag :: EditorTagTable editor -> Text -> IDEM (Maybe (EditorTag editor))

    -- Tag
    background :: EditorTag editor -> Color -> IDEM ()
    underline :: EditorTag editor -> Underline -> Maybe Color -> IDEM ()

