{-# LANGUAGE TypeFamilies #-}
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
) where

import IDE.Core.Types
       (LogRefType, IDE(..), IDERef, IDEM, IDEEventM, EditorStyle(..),
        IDEAction(..), editorStyle)
import Graphics.UI.Editor.Basics (Connection)
import Control.Monad.Reader (ReaderT(..))
import Graphics.UI.Gtk
       (Widget, ECrossing, ScrolledWindow, DrawWindow, Clipboard,
        TextSearchFlags, Menu, EMotion, EKey, EButton, EventM, Rectangle,
        Color, Underline)
import Foreign (Ptr)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.Glib.Signals (on)
import Data.Text (Text)
import IDE.Core.State (readIDE)
import IDE.Utils.GUIUtils (getDarkState)

updateStyle :: TextEditor editor => EditorBuffer editor -> IDEAction
updateStyle ebuf = do
    prefs <- readIDE prefs
    preferDark <- getDarkState
    setStyle ebuf $ editorStyle preferDark prefs

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
                  -> IDEM (EditorMark editor)
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
    newView :: EditorBuffer editor -> Maybe Text -> IDEM (EditorView editor)
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
    afterModifiedChanged :: EditorBuffer editor -> IDEM () -> IDEM [Connection]

    -- View
    bufferToWindowCoords :: EditorView editor -> (Int, Int) -> IDEM (Int, Int)
    drawTabs :: EditorView editor -> IDEM ()
    getBuffer :: EditorView editor -> IDEM (EditorBuffer editor)
    getWindow :: EditorView editor -> IDEM (Maybe DrawWindow)
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
    scrollToIter :: EditorView editor
                    -> EditorIter editor
                    -> Double
                    -> Maybe (Double, Double)
                    -> IDEM ()
    setFont :: EditorView editor -> Maybe Text -> IDEM ()
    setIndentWidth :: EditorView editor -> Int -> IDEM ()
    setWrapMode :: EditorView editor -> Bool -> IDEM ()
    setRightMargin :: EditorView editor -> Maybe Int -> IDEM ()
    setShowLineNumbers :: EditorView editor -> Bool -> IDEM ()
    setTabWidth :: EditorView editor -> Int -> IDEM ()

    -- Events
    afterFocusIn :: EditorView editor -> IDEM () -> IDEM [Connection]
    afterMoveCursor :: EditorView editor -> IDEM () -> IDEM [Connection]
    afterToggleOverwrite :: EditorView editor -> IDEM () -> IDEM [Connection]
    onButtonPress :: EditorView editor
                     -> IDEEventM EButton Bool
                     -> IDEM [Connection]
    onButtonRelease :: EditorView editor
                     -> IDEEventM EButton Bool
                     -> IDEM [Connection]
    onCompletion :: EditorView editor -> IDEM () -> IDEM () -> IDEM [Connection]
    onKeyPress :: EditorView editor
                  -> IDEEventM EKey Bool
                  -> IDEM [Connection]
    onMotionNotify :: EditorView editor
                  -> IDEEventM EMotion Bool
                  -> IDEM [Connection]
    onLeaveNotify :: EditorView editor
                  -> IDEEventM ECrossing Bool
                  -> IDEM [Connection]
    onKeyRelease :: EditorView editor
                    -> IDEEventM EKey Bool
                    -> IDEM [Connection]
    onLookupInfo :: EditorView editor -> IDEEventM EButton Bool -> IDEM [Connection]
    onMotionNotifyEvent :: EditorView editor -> IDEEventM EMotion Bool -> IDEM [Connection]
    onPopulatePopup :: EditorView editor -> (Menu -> IDEM ()) -> IDEM [Connection]

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
    underline :: EditorTag editor -> Underline -> IDEM ()

