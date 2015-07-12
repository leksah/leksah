{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor.GtkSourceView
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

module IDE.TextEditor.GtkSourceView (
    TextEditor(..)
  , EditorBuffer(..)
  , EditorView(..)
  , EditorIter(..)
  , EditorMark(..)
  , EditorTag(..)
  , EditorTagTable(..)

  , GtkSourceView(..)
  , newGtkBuffer

) where

import IDE.TextEditor.Class (TextEditor(..), EditorStyle(..))
import IDE.Core.Types
       (LogRefType(..), LogRef(..), LogRefType, colorHexString, Prefs(..),
        IDE(..), IDEM)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk
       (castToWidget, textViewGetIterAtLocation, focusOutEvent,
        focusInEvent, modifiedChanged, textTagUnderline, textTagBackground,
        textTagTableLookup, textTagTableAdd, textTagNew, textIterSetOffset,
        textIterSetLineOffset, textIterSetLine, textIterGetBuffer,
        textIterStartsWord, textIterStartsLine, textIterIsEnd,
        textIterIsStart, textIterGetOffset, textIterGetLine,
        textIterGetCharsInLine, textIterGetChar, textIterForwardWordEnd,
        textIterForwardToLineEnd, textIterForwardSearch,
        textIterForwardFindChar, textIterForwardChars, textIterForwardChar,
        textIterEndsWord, textIterBackwardChars, textIterGetLineOffset,
        textIterBackwardWordStart, textIterBackwardFindChar,
        textIterBackwardChar, populatePopup, eventModifier,
        keyReleaseEvent, leaveNotifyEvent, motionNotifyEvent,
        keyPressEvent, focusOutEvent, textIterEqual, idleAdd,
        bufferInsertText, buttonReleaseEvent, buttonPressEvent,
        toggleOverwrite, endUserAction, widgetAddEvents, moveCursor,
        scrolledWindowSetPolicy, textViewScrollToIter,
        textViewScrollToMark, widgetGrabFocus, widgetGetParent,
        castToScrolledWindow, textViewGetOverwrite,
        textViewGetIterLocation, textViewBuffer,
        textViewBufferToWindowCoords, textBufferSetModified,
        textBufferSelectRange, textBufferRemoveTagByName,
        textBufferPlaceCursor, textBufferPasteClipboard, widgetModifyFont,
        containerAdd, scrolledWindowNew, textViewSetWrapMode,
        textBufferMoveMark, textBufferInsert, textBufferHasSelection,
        textBufferGetText, textBufferGetTagTable, textBufferGetStartIter,
        textBufferGetSlice, textBufferGetSelectionBounds,
        textBufferGetSelectionBound, textBufferGetModified,
        textBufferGetLineCount, textBufferGetIterAtOffset,
        textBufferGetIterAtMark, textBufferGetIterAtLine,
        textBufferGetInsert, textBufferGetEndIter, textBufferEndUserAction,
        textBufferDeleteSelection, textBufferDelete,
        textBufferCutClipboard, textBufferCreateMark,
        textBufferCopyClipboard, textBufferBeginUserAction,
        textBufferApplyTagByName, TextTag, TextTagTable, TextMark,
        textBufferSetText, textIterCopy, TextIter, Modifier(..),
        FontDescription, fontDescriptionFromString, fontDescriptionNew,
        fontDescriptionSetFamily, EventMask(..), after, Underline(..),
        Color(..),
#ifdef MIN_VERSION_gtk3
        widgetGetWindow
#else
        widgetGetDrawWindow
#endif
        )
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))
import Graphics.UI.Gtk.SourceView
       (sourceBufferRemoveSourceMarks, sourceViewGetMarkAttributes,
        sourceBufferCreateSourceMark, SourceMark,
        sourceViewSetShowLineMarks,
        sourceStyleSchemeManagerAppendSearchPath, sourceViewSetTabWidth,
        sourceViewSetShowLineNumbers, sourceViewSetRightMarginPosition,
        sourceViewSetShowRightMargin, sourceViewSetIndentWidth,
        castToSourceBuffer, sourceViewSetDrawSpaces, sourceBufferUndo,
        sourceBufferSetStyleScheme, sourceStyleSchemeManagerGetScheme,
        sourceStyleSchemeManagerGetSchemeIds, sourceStyleSchemeManagerNew,
        sourceBufferRedo, sourceViewSetSmartHomeEnd,
        sourceViewSetAutoIndent, sourceViewSetIndentOnTab,
        sourceViewSetInsertSpacesInsteadOfTabs,
        sourceViewSetHighlightCurrentLine, sourceViewNewWithBuffer,
        sourceBufferGetCanUndo, sourceBufferGetCanRedo, SourceView,
        SourceBuffer, sourceBufferEndNotUndoableAction,
        sourceBufferBeginNotUndoableAction, sourceBufferSetMaxUndoLevels,
        sourceBufferNew, sourceBufferNewWithLanguage,
        sourceLanguageManagerGuessLanguage,
        sourceLanguageManagerSetSearchPath,
        sourceLanguageManagerGetSearchPath, sourceLanguageManagerNew)
import System.FilePath ((</>))
import System.GIO (contentTypeGuess)
import IDE.Core.State (onIDE, reflectIDE, readIDE, getDataDir)
import Graphics.UI.Gtk.SourceView.Enums
       (SourceDrawSpacesFlags(..), SourceSmartHomeEndType(..))
import Graphics.UI.Gtk.General.Enums
       (PolicyType(..), TextWindowType(..), WrapMode(..))
import Control.Monad (void, when, forM_)
import Control.Monad.Reader.Class (MonadReader(..))
import Graphics.UI.Editor.Basics (Connection(..))
import Data.Maybe (isNothing, maybeToList, fromJust)
import Data.IORef (writeIORef, readIORef, newIORef)
import System.Glib.MainLoop (priorityDefault, idleRemove)
import Data.Char (isDigit, isSymbol, isAlphaNum)
import System.Glib.Signals (after, on)
import Control.Monad.Trans.Class (MonadTrans(..))
import System.Glib.Attributes (get, AttrOp(..), set)
import qualified Graphics.UI.Gtk as Gtk (endUserAction)
import IDE.Utils.GUIUtils (fontDescription)
import Data.Text (Text)
import qualified Data.Text as T
       (drop, dropWhile, all, length, pack)
import Data.Monoid ((<>))
import Graphics.UI.Gtk.Multiline.TextBuffer
       (markSet, textBufferDeleteMarkByName, textBufferDeleteMark,
        textBufferGetMark)
import Graphics.UI.Gtk.SourceView.SourceMarkAttributes
       (queryTooltipText, sourceMarkAttributesSetIconName,
        sourceMarkAttributesNew)
import Graphics.UI.Gtk.SourceView.SourceView
       (sourceViewSetMarkAttributes)
import Graphics.UI.Gtk.Multiline.TextMark
       (textMarkGetName, toTextMark)
import Control.Arrow (Arrow(..))
import System.Log.Logger (debugM)

transformGtkIter :: EditorIter GtkSourceView -> (TextIter -> IO a) -> IDEM (EditorIter GtkSourceView)
transformGtkIter (GtkIter i) f = do
    new <- liftIO $ textIterCopy i
    liftIO $ f new
    return (GtkIter new)

transformGtkIterMaybe :: EditorIter GtkSourceView -> (TextIter -> IO Bool) -> IDEM (Maybe (EditorIter GtkSourceView))
transformGtkIterMaybe (GtkIter i) f = do
    new <- liftIO $ textIterCopy i
    found <- liftIO $ f new
    return $ if found
        then Just (GtkIter new)
        else Nothing

data GtkSourceView = GtkSourceView deriving( Typeable, Show )

newGtkBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer GtkSourceView)
newGtkBuffer mbFilename contents = liftIO $ do
    lm      <- sourceLanguageManagerNew
    dataDir <- getDataDir
    oldPath <- sourceLanguageManagerGetSearchPath lm
    sourceLanguageManagerSetSearchPath lm (Just $ (dataDir </> "language-specs") : oldPath)
    mbLang  <- case mbFilename of
        Just filename -> do
            guess <- contentTypeGuess filename contents (T.length contents)
            sourceLanguageManagerGuessLanguage lm (Just filename) $
                case guess of
                    (True, _)  -> Just "text/x-haskell"
                    (False, t) -> Just t
        Nothing -> sourceLanguageManagerGuessLanguage lm Nothing (Just ("text/x-haskell" :: Text))
    mbLang2 <- case mbLang of
                    Nothing -> sourceLanguageManagerGuessLanguage lm Nothing (Just ("text/x-haskell" :: Text))
                    _ -> return mbLang
    buffer <- case mbLang2 of
        Just sLang -> sourceBufferNewWithLanguage sLang
        Nothing -> sourceBufferNew Nothing
    sourceBufferSetMaxUndoLevels buffer (-1)
    sourceBufferBeginNotUndoableAction buffer
    textBufferSetText buffer contents
    sourceBufferEndNotUndoableAction buffer
    tagTable <- textBufferGetTagTable buffer
    textTagNew (Just "found") >>= textTagTableAdd tagTable
    textTagNew (Just "match") >>= textTagTableAdd tagTable
    textTagNew (Just "context") >>= textTagTableAdd tagTable
    textTagNew (Just "breakpoint") >>= textTagTableAdd tagTable
    forM_ [minBound .. maxBound :: LogRefType] $ \ refType ->
        textTagNew (Just . T.pack $ show refType) >>= textTagTableAdd tagTable
    return $ GtkBuffer buffer

instance TextEditor GtkSourceView where
    data EditorBuffer GtkSourceView = GtkBuffer SourceBuffer
    data EditorView GtkSourceView = GtkView SourceView
    data EditorMark GtkSourceView = GtkMark TextMark
    data EditorIter GtkSourceView = GtkIter TextIter
    data EditorTagTable GtkSourceView = GtkTagTable TextTagTable
    data EditorTag GtkSourceView = GtkTag TextTag

    newBuffer = newGtkBuffer
    applyTagByName (GtkBuffer sb) name (GtkIter first) (GtkIter last) = liftIO $
        textBufferApplyTagByName sb name first last
    beginNotUndoableAction (GtkBuffer sb) = liftIO $ sourceBufferBeginNotUndoableAction sb
    beginUserAction (GtkBuffer sb) = liftIO $ textBufferBeginUserAction sb
    canRedo (GtkBuffer sb) = liftIO $ sourceBufferGetCanRedo sb
    canUndo (GtkBuffer sb) = liftIO $ sourceBufferGetCanUndo sb
    copyClipboard (GtkBuffer sb) clipboard = liftIO $ textBufferCopyClipboard sb clipboard
    createMark (GtkView sv) refType (GtkIter i) tooltip = liftIO $ do
        let cat = T.pack $ show refType
        sb <- castToSourceBuffer <$> get sv textViewBuffer
        n <- textIterGetLine i
        GtkMark . toTextMark <$> sourceBufferCreateSourceMark sb (Just $ T.pack (show n) <> " " <> tooltip) cat i
    cutClipboard (GtkBuffer sb) clipboard defaultEditable = liftIO $ textBufferCutClipboard sb clipboard defaultEditable
    delete (GtkBuffer sb) (GtkIter first) (GtkIter last) = liftIO $
        textBufferDelete sb first last
    deleteSelection (GtkBuffer sb) = liftIO . void $ textBufferDeleteSelection sb True True
    endNotUndoableAction (GtkBuffer sb) = liftIO $ sourceBufferEndNotUndoableAction sb
    endUserAction (GtkBuffer sb) = liftIO $ textBufferEndUserAction sb
    getEndIter (GtkBuffer sb) = liftIO $ GtkIter <$> textBufferGetEndIter sb
    getInsertMark (GtkBuffer sb) = liftIO $ GtkMark <$> textBufferGetInsert sb
    getIterAtLine (GtkBuffer sb) line = liftIO $ GtkIter <$> textBufferGetIterAtLine sb line
    getIterAtMark (GtkBuffer sb) (GtkMark m) = liftIO $ GtkIter <$> textBufferGetIterAtMark sb m
    getIterAtOffset (GtkBuffer sb) offset = liftIO $ GtkIter <$> textBufferGetIterAtOffset sb offset
    getLineCount (GtkBuffer sb) = liftIO $ textBufferGetLineCount sb
    getModified (GtkBuffer sb) = liftIO $ textBufferGetModified sb
    getSelectionBoundMark (GtkBuffer sb) = liftIO $ GtkMark <$> textBufferGetSelectionBound sb
    getSelectionBounds (GtkBuffer sb) = liftIO $ (GtkIter *** GtkIter) <$>
        textBufferGetSelectionBounds sb
    getInsertIter (GtkBuffer sb) = liftIO $ GtkIter <$> do
        insertMark <- textBufferGetInsert sb
        textBufferGetIterAtMark sb insertMark
    getSlice (GtkBuffer sb) (GtkIter first) (GtkIter last) includeHidenChars = liftIO $
        textBufferGetSlice sb first last includeHidenChars
    getStartIter (GtkBuffer sb) = liftIO $ GtkIter <$> textBufferGetStartIter sb
    getTagTable (GtkBuffer sb) = liftIO $ GtkTagTable <$> textBufferGetTagTable sb
    getText (GtkBuffer sb) (GtkIter first) (GtkIter last) includeHidenChars = liftIO $
        textBufferGetText sb first last includeHidenChars
    hasSelection (GtkBuffer sb) = liftIO $ textBufferHasSelection sb
    insert (GtkBuffer sb) (GtkIter i) text = liftIO $ textBufferInsert sb i text
    newView (GtkBuffer sb) mbFontString = do
        prefs <- readIDE prefs
        fd <- fontDescription mbFontString
        liftIO $ GtkView <$> do
            sv <- sourceViewNewWithBuffer sb
            sourceViewSetHighlightCurrentLine sv True
            sourceViewSetInsertSpacesInsteadOfTabs sv True
            sourceViewSetIndentOnTab sv True
            sourceViewSetAutoIndent sv True
            sourceViewSetSmartHomeEnd sv SourceSmartHomeEndBefore
            sourceViewSetShowLineMarks sv True
            forM_ [minBound..maxBound] $ \ refType -> do
                let cat = T.pack $ show refType
                    icon = case refType of
                            ErrorRef       -> "dialog-error"
                            WarningRef     -> "dialog-warning"
                            TestFailureRef -> "software-update-urgent"
                            LintRef        -> "emblem-generic"
                            BreakpointRef  -> "media-playback-pause"
                            ContextRef     -> "media-playback-start"
                attributes <- sourceMarkAttributesNew
                sourceMarkAttributesSetIconName attributes (Just (icon :: Text))
                on attributes queryTooltipText $ \ mark -> maybe "" (T.drop 1 . T.dropWhile isDigit) <$> textMarkGetName mark
                sourceViewSetMarkAttributes sv cat (Just attributes) (1 + fromEnum(maxBound :: LogRefType) - fromEnum refType)
            if wrapLines prefs
                then textViewSetWrapMode sv WrapWord
                else textViewSetWrapMode sv WrapNone
            sw <- scrolledWindowNew Nothing Nothing
            containerAdd sw sv
            widgetModifyFont sv (Just fd)
            return sv
    pasteClipboard (GtkBuffer sb) clipboard (GtkIter i) defaultEditable = liftIO $
        textBufferPasteClipboard sb clipboard i defaultEditable
    placeCursor (GtkBuffer sb) (GtkIter i) = liftIO $ textBufferPlaceCursor sb i
    redo (GtkBuffer sb) = liftIO $ sourceBufferRedo sb
    removeTagByName (GtkBuffer sb) name = liftIO $ do
        first <- textBufferGetStartIter sb
        last <- textBufferGetEndIter sb
        textBufferRemoveTagByName sb name first last
        sourceBufferRemoveSourceMarks sb first last name
    selectRange (GtkBuffer sb) (GtkIter first) (GtkIter last) = liftIO $
        textBufferSelectRange sb first last
    setModified (GtkBuffer sb) modified = liftIO . void $ textBufferSetModified sb modified
    setStyle (GtkBuffer sb) EditorStyle {..} =
        case styleName of
            Nothing  -> return ()
            Just str -> do
                styleManager <- liftIO sourceStyleSchemeManagerNew
                dataDir <- liftIO getDataDir
                liftIO $ sourceStyleSchemeManagerAppendSearchPath styleManager $ dataDir </> "data/styles"
                ids <- liftIO $ sourceStyleSchemeManagerGetSchemeIds styleManager
                let preferedNames = if preferDark then [str<>"-dark", str] else [str]
                forM_ (take 1 $ filter (`elem` ids) preferedNames) $ \ name -> do
                    liftIO $ do
                        scheme <- sourceStyleSchemeManagerGetScheme styleManager name
                        sourceBufferSetStyleScheme sb (Just scheme)
                    tagTable <- getTagTable (GtkBuffer sb)
                    let isDark = name `elem` ["leksah-dark", "oblivion", "cobalt"]
                        setBG (dark, light) (Just tag) = background tag (if isDark then dark else light)
                        setBG _             Nothing    = return ()
                        setUnderline = maybe (return ()) (`underline` UnderlineError)
                    lookupTag tagTable "match" >>= setBG matchBG
                    lookupTag tagTable "found" >>= setBG foundBG
                    lookupTag tagTable (T.pack $ show ErrorRef      ) >>= setUnderline
                    lookupTag tagTable (T.pack $ show WarningRef    ) >>= setUnderline
                    lookupTag tagTable (T.pack $ show TestFailureRef) >>= setUnderline
                    lookupTag tagTable (T.pack $ show LintRef       ) >>= setBG lintBG
                    lookupTag tagTable (T.pack $ show BreakpointRef ) >>= setBG breakpointBG
                    lookupTag tagTable (T.pack $ show ContextRef    ) >>= setBG contextBG
    setText (GtkBuffer sb) text = liftIO $ textBufferSetText sb text
    undo (GtkBuffer sb) = liftIO $ sourceBufferUndo sb

    afterModifiedChanged (GtkBuffer sb) f = do
        ideR <- ask
        liftIO $ do
            id1 <- sb `after` modifiedChanged $ reflectIDE f ideR
            return [ConnectC id1]

    -- View
    bufferToWindowCoords (GtkView sv) point = liftIO $ textViewBufferToWindowCoords sv TextWindowWidget point
    drawTabs (GtkView sv) = liftIO $ sourceViewSetDrawSpaces sv [SourceDrawSpacesTab, SourceDrawSpacesSpace, SourceDrawSpacesTrailing]
    getBuffer (GtkView sv) = liftIO $ (GtkBuffer . castToSourceBuffer) <$> sv `get` textViewBuffer
#ifdef MIN_VERSION_gtk3
    getWindow (GtkView sv) = liftIO $ widgetGetWindow sv
#else
    getWindow (GtkView sv) = liftIO $ Just <$> widgetGetDrawWindow sv
#endif
    getIterAtLocation (GtkView sv) x y = liftIO $ GtkIter <$> textViewGetIterAtLocation sv x y
    getIterLocation (GtkView sv) (GtkIter i) = liftIO $ textViewGetIterLocation sv i
    getOverwrite (GtkView sv) = liftIO $ textViewGetOverwrite sv
    getScrolledWindow (GtkView sv) = liftIO $ (castToScrolledWindow . fromJust) <$> widgetGetParent sv
    getEditorWidget (GtkView sv) = return $ castToWidget sv
    grabFocus (GtkView sv) = liftIO $ widgetGrabFocus sv
    scrollToMark (GtkView sv) (GtkMark m) withMargin mbAlign = liftIO $ textViewScrollToMark sv m withMargin mbAlign
    scrollToIter (GtkView sv) (GtkIter i) withMargin mbAlign = liftIO . void $ textViewScrollToIter sv i withMargin mbAlign
    setFont (GtkView sv) mbFontString = do
        fd <- fontDescription mbFontString
        liftIO $ widgetModifyFont sv (Just fd)
    setIndentWidth (GtkView sv) width = liftIO $ sourceViewSetIndentWidth sv width
    setWrapMode v@(GtkView sv) wrapLines = do
        sw <- getScrolledWindow v
        if wrapLines
            then liftIO $ do
                textViewSetWrapMode sv WrapWord
                scrolledWindowSetPolicy sw PolicyNever PolicyAutomatic
            else liftIO $ do
                textViewSetWrapMode sv WrapNone
                scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    setRightMargin (GtkView sv) mbRightMargin = liftIO $
        case mbRightMargin of
            Just n -> do
                sourceViewSetShowRightMargin sv True
                sourceViewSetRightMarginPosition sv (fromIntegral n)
            Nothing -> sourceViewSetShowRightMargin sv False
    setShowLineNumbers (GtkView sv) show = liftIO $ sourceViewSetShowLineNumbers sv show
    setTabWidth (GtkView sv) width = liftIO $ sourceViewSetTabWidth sv width

    -- Events
    afterFocusIn (GtkView sv) f = do
        ideR <- ask
        liftIO $ do
            id1 <- sv `after` focusInEvent $ lift $ reflectIDE f ideR >> return False
            return [ConnectC id1]
    afterMoveCursor v@(GtkView sv) f = do
        ideR <- ask
        (GtkBuffer sb) <- getBuffer v
        liftIO $ do
            id1 <- sv `after` moveCursor $ \_ _ _ -> reflectIDE f ideR
            sv `widgetAddEvents` [ButtonReleaseMask]
            id2 <- sv `on` buttonReleaseEvent $ lift $ reflectIDE f ideR >> return False
            id3 <- sb `after` Gtk.endUserAction $ reflectIDE f ideR
            return [ConnectC id1, ConnectC id2, ConnectC id3]
    afterToggleOverwrite (GtkView sv) f = do
        ideR <- ask
        liftIO $ do
            id1 <- sv `after` toggleOverwrite $ reflectIDE f ideR
            return [ConnectC id1]
    onButtonPress (GtkView sv) f = do
        id1 <- sv `onIDE` buttonPressEvent $ f
        return [ConnectC id1]
    onButtonRelease (GtkView sv) f = do
        id1 <- sv `onIDE` buttonReleaseEvent $ f
        return [ConnectC id1]
    onCompletion v@(GtkView sv) start cancel = do
        ideR <- ask
        (GtkBuffer sb) <- getBuffer v
        liftIO $ do
            -- when multiple afterBufferInsertText are called quickly,
            -- we cancel previous idle action which was not processed,
            -- its handler is stored here.
            -- Paste operation is example of such sequential events (each word!).
            lastHandler <- newIORef Nothing
            id1 <- after sb bufferInsertText $ \iter text -> do
                lh <- readIORef lastHandler
                debugM "leksah" $ "Removing " <> show lh
                mapM_ idleRemove $ maybeToList lh
                h <- flip idleAdd priorityDefault $ do
                    writeIORef lastHandler Nothing
                    let isIdent a = isAlphaNum a || a == '\'' || a == '_' || a == '.'
                    let isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                                 || a == '!'  || a == '@' || a == '%' || a == '&' || a == '?'
                    if T.all isIdent text || T.all isOp text
                        then do
                            hasSel <- textBufferHasSelection sb
                            if not hasSel
                                then do
                                    (iterC, _) <- textBufferGetSelectionBounds sb
                                    atC <- textIterEqual iter iterC
                                    when atC $ reflectIDE start ideR
                                    return False
                                else do
                                    reflectIDE cancel ideR
                                    return False
                        else do
                            reflectIDE cancel ideR
                            return False
                writeIORef lastHandler (Just h)
                return ()
            id2 <- sv `on` moveCursor $ \_ _ _ -> reflectIDE cancel ideR
            id3 <- sv `on` buttonPressEvent $ lift $ reflectIDE cancel ideR >> return False
            id4 <- sv `on` focusOutEvent $ lift $ reflectIDE cancel ideR >> return False
            return [ConnectC id1, ConnectC id2, ConnectC id3, ConnectC id4]
    onKeyPress (GtkView sv) f = do
        id1 <- sv `onIDE` keyPressEvent $ f
        return [ConnectC id1]
    onMotionNotify (GtkView sv) f = do
        id1 <- sv `onIDE` motionNotifyEvent $ f
        return [ConnectC id1]
    onLeaveNotify (GtkView sv) f = do
        id1 <- sv `onIDE` leaveNotifyEvent $ f
        return [ConnectC id1]
    onKeyRelease (GtkView sv) f = do
        id1 <- sv `onIDE` keyReleaseEvent $ f
        return [ConnectC id1]
    onLookupInfo (GtkView sv) f = do
        liftIO $ sv `widgetAddEvents` [ButtonReleaseMask]
        id1 <- sv `onIDE` buttonReleaseEvent $ do
            mod <- lift eventModifier
            case mod of
                [Control] -> f >> return True
                _             -> return False
        return [ConnectC id1]
    onMotionNotifyEvent (GtkView sv) handler = do
        liftIO $ widgetAddEvents sv [ButtonMotionMask, Button1MotionMask]  -- TODO: this doesn't work yet event gets fired anyways: restrict event to being fired when left mouse button is pressed down
        id1 <- sv `onIDE` motionNotifyEvent $ handler  --TODO this is potentially slowing leksah, a better event (if there was any) could be more efficient here
        liftIO $ widgetAddEvents sv [ButtonMotionMask, Button1MotionMask]  -- TODO: this doesn't work yet event gets fired anyways: restrict event to being fired when left mouse button is pressed down
        return [ConnectC id1]
    onPopulatePopup (GtkView sv) f = do
        ideR <- ask
        liftIO $ do
            id1 <- sv `on` populatePopup $ \menu -> reflectIDE (f menu) ideR
            return [ConnectC id1]
    onSelectionChanged (GtkBuffer sb) handler = do
        ideR <- ask
        liftIO $ do
            id1 <- sb `on` markSet $ \ _ mark -> do
                name <- textMarkGetName mark
                when (name == Just "insert") $ reflectIDE handler ideR
            return [ConnectC id1]

    -- Iter
    backwardCharC i = transformGtkIter i textIterBackwardChar
    backwardFindCharC i pred mbLimit = transformGtkIterMaybe i $ \x ->
        textIterBackwardFindChar x pred $
            case mbLimit of
                Just (GtkIter limit) -> Just limit
                Nothing              -> Nothing
    backwardWordStartC i = transformGtkIterMaybe i textIterBackwardWordStart
    backwardToLineStartC i = transformGtkIter i $ \new -> do
        n <- textIterGetLineOffset new
        textIterBackwardChars new n
        return ()
    endsWord (GtkIter i) = liftIO $ textIterEndsWord i
    forwardCharC i = transformGtkIter i textIterForwardChar
    forwardCharsC i n = transformGtkIter i $ flip textIterForwardChars n
    forwardFindCharC i pred mbLimit = transformGtkIterMaybe i $ \x ->
        textIterForwardFindChar x pred $
            case mbLimit of
                Just (GtkIter limit) -> Just limit
                Nothing              -> Nothing
    forwardSearch (GtkIter i) str flags mbLimit = liftIO $ fmap (fmap (GtkIter *** GtkIter)) $
        textIterForwardSearch i str flags $
            case mbLimit of
                Just (GtkIter limit) -> Just limit
                Nothing              -> Nothing
    forwardToLineEndC i = transformGtkIter i textIterForwardToLineEnd
    forwardWordEndC i = transformGtkIterMaybe i textIterForwardWordEnd
    getChar (GtkIter i) = liftIO $ textIterGetChar i
    getCharsInLine (GtkIter i) = liftIO $ textIterGetCharsInLine i
    getLine (GtkIter i) = liftIO $ textIterGetLine i
    getLineOffset (GtkIter i) = liftIO $ textIterGetLineOffset i
    getOffset (GtkIter i) = liftIO $ textIterGetOffset i
    isStart (GtkIter i) = liftIO $ textIterIsStart i
    isEnd (GtkIter i) = liftIO $ textIterIsEnd i
    iterEqual (GtkIter i1) (GtkIter i2) = liftIO $ textIterEqual i1 i2
    startsLine (GtkIter i) = liftIO $ textIterStartsLine i
    startsWord (GtkIter i) = liftIO $ textIterStartsWord i
    atEnd (GtkIter i) = liftIO $ GtkIter <$> do
        buffer <- textIterGetBuffer i
        textBufferGetEndIter buffer
    atLine i line = transformGtkIter i $ flip textIterSetLine line
    atLineOffset i column = transformGtkIter i $ flip textIterSetLineOffset column
    atOffset i offset = transformGtkIter i $ flip textIterSetOffset offset
    atStart (GtkIter i) = liftIO $ GtkIter <$> do
        buffer <- textIterGetBuffer i
        textBufferGetEndIter buffer

    -- Tag Table
    newTag (GtkTagTable tt) name = liftIO $ GtkTag <$> do
        t <- textTagNew (Just name)
        textTagTableAdd tt t
        return t
    lookupTag (GtkTagTable tt) name = liftIO $ fmap GtkTag <$> textTagTableLookup tt name

    -- Tag
    background (GtkTag t) color = liftIO $ set t [textTagBackground := T.pack $ colorHexString color]
    underline (GtkTag t) value = liftIO $ set t [textTagUnderline := value]

