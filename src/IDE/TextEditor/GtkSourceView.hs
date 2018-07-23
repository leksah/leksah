{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))
import System.FilePath ((</>))
import IDE.Core.State (onIDE, reflectIDE, readIDE, getDataDir)
import Control.Monad (void, when, forM_)
import Control.Monad.Reader.Class (MonadReader(..))
import Graphics.UI.Editor.Basics (Connection(..))
import Data.Maybe
       (isJust, fromMaybe, isNothing, maybeToList, fromJust)
import Data.IORef (writeIORef, readIORef, newIORef)
import Data.Char (isDigit, isSymbol, isAlphaNum)
import Control.Monad.Trans.Class (MonadTrans(..))
import IDE.Utils.GUIUtils (fontDescription)
import Data.Text (Text)
import qualified Data.Text as T
       (drop, dropWhile, all, length, pack)
import Data.Monoid ((<>))
import Control.Arrow (Arrow(..))
import System.Log.Logger (debugM)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (nullPtr, castPtr)
import qualified GI.GtkSource as Source
import GI.GtkSource
       (mapSetView, mapNew, viewSetDrawSpaces, setViewTabWidth,
        setViewShowLineNumbers, setViewRightMarginPosition,
        setViewShowRightMargin, setViewIndentWidth, setViewDrawSpaces,
        bufferUndo, bufferSetStyleScheme, styleSchemeManagerGetScheme,
        styleSchemeManagerGetSchemeIds, styleSchemeManagerAppendSearchPath,
        styleSchemeManagerNew, bufferRemoveSourceMarks, bufferRedo,
        viewSetMarkAttributes, onMarkAttributesQueryTooltipText,
        markAttributesSetIconName, markAttributesNew, setViewShowLineMarks,
        setViewSmartHomeEnd, setViewAutoIndent, setViewIndentOnTab,
        setViewInsertSpacesInsteadOfTabs, setViewHighlightCurrentLine,
        viewNewWithBuffer, bufferCreateSourceMark, getBufferCanUndo,
        getBufferCanRedo, bufferEndNotUndoableAction,
        bufferBeginNotUndoableAction, bufferSetMaxUndoLevels, bufferNew,
        bufferNewWithLanguage, languageManagerGuessLanguage,
        languageManagerSetSearchPath, languageManagerGetSearchPath,
        languageManagerNew)
import GI.Gio (contentTypeGuess)
import GI.Gtk.Objects.TextBuffer
       (afterTextBufferChanged, setTextBufferText, onTextBufferMarkSet,
        afterTextBufferInsertText, afterTextBufferEndUserAction,
        afterTextBufferModifiedChanged, textBufferSetModified,
        textBufferSelectRange, textBufferRemoveTagByName,
        textBufferPlaceCursor, textBufferPasteClipboard, textBufferInsert,
        textBufferGetText, textBufferGetStartIter, textBufferGetSlice,
        textBufferGetSelectionBounds, textBufferGetSelectionBound,
        textBufferGetModified, textBufferGetLineCount,
        textBufferGetIterAtOffset, textBufferGetIterAtMark,
        textBufferGetIterAtLine, textBufferGetInsert, textBufferGetEndIter,
        textBufferEndUserAction, textBufferDeleteSelection,
        textBufferDelete, textBufferCutClipboard, textBufferGetMark,
        textBufferCopyClipboard, textBufferBeginUserAction,
        textBufferApplyTagByName, textBufferGetTagTable, textBufferSetText)
import GI.Gtk.Objects.TextTag
import GI.Gtk.Objects.TextTagTable
       (noTextTagTable, textTagTableLookup, TextTagTable(..),
        textTagTableAdd)
import GI.Gtk.Objects.TextMark (textMarkGetName, TextMark(..))
import GI.Gtk.Objects.TextView
       (textViewSetEditable, onTextViewPopulatePopup,
        onTextViewMoveCursor, afterTextViewToggleOverwrite,
        afterTextViewMoveCursor, textViewScrollToIter,
        textViewScrollToMark, textViewGetOverwrite,
        textViewGetIterLocation, textViewGetIterAtLocation,
        textViewBufferToWindowCoords, textViewSetWrapMode,
        getTextViewBuffer)
import Data.GI.Base.ManagedPtr
       (castTo, withManagedPtr, unsafeCastTo)
import GI.GObject.Functions
       (signalHandlersBlockMatched, signalLookup)
import Data.GI.Base.BasicTypes (GObject(..))
import GI.GObject.Flags (SignalMatchType(..))
import GI.GtkSource.Enums (SmartHomeEndType(..))
import GI.Gtk.Enums
       (PolicyType(..), TextWindowType(..), WrapMode(..))
import GI.Gtk.Objects.ScrolledWindow
       (ScrolledWindow(..), scrolledWindowSetPolicy, scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.Container (containerAdd)
import GI.Gtk.Objects.Widget
       (onWidgetKeyReleaseEvent, onWidgetLeaveNotifyEvent,
        onWidgetMotionNotifyEvent, onWidgetKeyPressEvent,
        onWidgetFocusOutEvent, onWidgetButtonPressEvent,
        onWidgetButtonReleaseEvent, widgetAddEvents,
        afterWidgetFocusInEvent, widgetGrabFocus, toWidget,
        widgetGetParent, widgetGetWindow, widgetModifyFont)
import GI.Pango.Enums (Underline(..))
import GI.GtkSource.Flags (DrawSpacesFlags(..))
import Data.GI.Base.BasicConversions (gflagsToWord)
import GI.Gdk.Flags (ModifierType(..), EventMask(..))
import GI.GLib (pattern PRIORITY_DEFAULT, idleAdd, sourceRemove)
import GI.Gdk
       (setRGBAAlpha, setRGBABlue, setRGBAGreen, setRGBARed, newZeroRGBA,
        RGBA, getEventButtonState)
import GI.Gtk.Structs.TextIter
       (textIterSetOffset, textIterSetLineOffset, textIterSetLine,
        textIterGetBuffer, textIterStartsWord, textIterStartsLine,
        textIterIsEnd, textIterIsStart, textIterGetOffset,
        textIterGetCharsInLine, textIterGetChar, textIterForwardWordEnd,
        textIterForwardToLineEnd, textIterForwardSearch,
        textIterForwardFindChar, textIterForwardChars, textIterForwardChar,
        textIterEndsWord, textIterBackwardChars, textIterGetLineOffset,
        textIterBackwardWordStart, textIterBackwardFindChar,
        textIterBackwardChar, textIterEqual, textIterGetLine, textIterCopy,
        TextIter(..))
import Data.Text.Encoding (encodeUtf8)
import qualified GI.Gdk as Gdk (Window(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import GI.Gtk.Objects.Menu (Menu(..))
import Data.GI.Base.Constructible (Constructible(..))
import Data.GI.Base.Attributes (AttrOp(..))
import Graphics.UI.Editor.Simple (Color(..))
import GI.Gtk
       (scrollableSetVscrollPolicy, scrollableSetHscrollPolicy,
        widgetSetVexpand, widgetSetHexpand, frameNew, gridNew,
        widgetOverrideFont, setTextTagUnderlineRgba, Widget(..))
import GI.GtkSource.Objects.StyleScheme
       (styleSchemeGetStyle, StyleScheme(..))
import GI.GtkSource.Objects.Style
import GI.Pango.Enums (Weight (..), Style (..))

transformGtkIter :: EditorIter GtkSourceView -> (TextIter -> IO a) -> IDEM (EditorIter GtkSourceView)
transformGtkIter (GtkIter i) f = do
    new <- textIterCopy i
    liftIO $ f new
    return (GtkIter new)

transformGtkIterMaybe :: EditorIter GtkSourceView -> (TextIter -> IO Bool) -> IDEM (Maybe (EditorIter GtkSourceView))
transformGtkIterMaybe (GtkIter i) f = do
    new <- textIterCopy i
    found <- liftIO $ f new
    return $ if found
        then Just (GtkIter new)
        else Nothing

data GtkSourceView = GtkSourceView deriving( Typeable, Show )

newGtkBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer GtkSourceView)
newGtkBuffer mbFilename contents = do
    liftIO $ debugM "lekash" "newGtkBuffer"
    lm      <- languageManagerNew
    dataDir <- getDataDir
    oldPath <- languageManagerGetSearchPath lm
    languageManagerSetSearchPath lm (Just $ T.pack (dataDir </> "language-specs") : oldPath)
    mbLang  <- case mbFilename of
        Just filename -> do
            guess <- contentTypeGuess (Just $ T.pack filename) (Just $ encodeUtf8 contents)
            languageManagerGuessLanguage lm (Just $ T.pack filename) $
                case guess of
                    (_, True)  -> Just "text/x-haskell"
                    (t, False) -> Just t
        Nothing -> languageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
    mbLang2 <- case mbLang of
                    Nothing -> languageManagerGuessLanguage lm Nothing (Just "text/x-haskell")
                    _ -> return mbLang
    buffer <- case mbLang2 of
        Just sLang -> bufferNewWithLanguage sLang
        Nothing -> bufferNew noTextTagTable
    bufferSetMaxUndoLevels buffer (-1)
    bufferBeginNotUndoableAction buffer
    liftIO $ debugM "lekash" "newGtkBuffer setTextBufferText"
    setTextBufferText buffer contents
    bufferEndNotUndoableAction buffer
    liftIO $ debugM "lekash" "newGtkBuffer setup tag table"
    tagTable <- textBufferGetTagTable buffer
    textTagNew (Just "search-match") >>= textTagTableAdd tagTable
    textTagNew (Just "selection-match") >>= textTagTableAdd tagTable
    textTagNew (Just "context") >>= textTagTableAdd tagTable
    textTagNew (Just "breakpoint") >>= textTagTableAdd tagTable
    forM_ [minBound .. maxBound :: LogRefType] $ \ refType ->
        textTagNew (Just . T.pack $ show refType) >>= textTagTableAdd tagTable
    return $ GtkBuffer buffer


-- | Sets the style for a type of tag in the buffer by looking in the stylescheme
-- for a style with the given name, otherwise use the provided action
-- that sets some style on the TextTag (for standard sourceview styleschemes we can't
-- edit the stylescheme files)
setTagStyle :: Source.Buffer -> Text -> StyleScheme -> (Text -> IDEM ()) -> IDEM ()
setTagStyle sb tagName scheme applyDefaultStyle = do
    tagTable <- textBufferGetTagTable sb
    mbTag    <- textTagTableLookup tagTable tagName
    forM_ mbTag $ \tag -> do
        mbStyle <- styleSchemeGetStyle scheme tagName
        case mbStyle of
            Just style -> applyStyle tag style
            Nothing    -> applyDefaultStyle tagName
    where
        applyStyle :: TextTag -> Source.Style -> IDEM ()
        applyStyle tag style = do
            mbBg <- getStyleBackground style
            forM_ mbBg (setTextTagBackground tag)
            mbFg <- getStyleForeground style
            forM_ mbFg (setTextTagForeground tag)
            bold <- getStyleBold style
            when bold (setTextTagWeight tag (fromIntegral . fromEnum $ WeightBold))
            italic <- getStyleItalic style
            when italic (setTextTagStyle tag StyleItalic)

instance TextEditor GtkSourceView where
    data EditorBuffer GtkSourceView = GtkBuffer Source.Buffer
    data EditorView GtkSourceView = GtkView Source.View
    data EditorMark GtkSourceView = GtkMark TextMark
    data EditorIter GtkSourceView = GtkIter TextIter
    data EditorTagTable GtkSourceView = GtkTagTable TextTagTable
    data EditorTag GtkSourceView = GtkTag TextTag

    newBuffer = newGtkBuffer
    applyTagByName (GtkBuffer sb) name (GtkIter first) (GtkIter last) =
        textBufferApplyTagByName sb name first last
    beginNotUndoableAction (GtkBuffer sb) = bufferBeginNotUndoableAction sb
    beginUserAction (GtkBuffer sb) = textBufferBeginUserAction sb
    canRedo (GtkBuffer sb) = getBufferCanRedo sb
    canUndo (GtkBuffer sb) = getBufferCanUndo sb
    copyClipboard (GtkBuffer sb) = textBufferCopyClipboard sb
    createMark (GtkView sv) refType (GtkIter i) tooltip = do
        sb <- getTextViewBuffer sv >>= liftIO . unsafeCastTo Source.Buffer
        n <- textIterGetLine i
        let cat  = T.pack $ show refType
            name = T.pack (show n) <> " " <> tooltip
        mark <- textBufferGetMark sb name
        when (isNothing mark) . void $
            bufferCreateSourceMark sb (Just name) cat i
    cutClipboard (GtkBuffer sb) = textBufferCutClipboard sb
    delete (GtkBuffer sb) (GtkIter first) (GtkIter last) =
        textBufferDelete sb first last
    deleteSelection (GtkBuffer sb) = void $ textBufferDeleteSelection sb True True
    endNotUndoableAction (GtkBuffer sb) = bufferEndNotUndoableAction sb
    endUserAction (GtkBuffer sb) = textBufferEndUserAction sb
    getEndIter (GtkBuffer sb) = GtkIter <$> textBufferGetEndIter sb
    getInsertMark (GtkBuffer sb) = GtkMark <$> textBufferGetInsert sb
    getIterAtLine (GtkBuffer sb) line = GtkIter <$> textBufferGetIterAtLine sb (fromIntegral line)
    getIterAtMark (GtkBuffer sb) (GtkMark m) = GtkIter <$> textBufferGetIterAtMark sb m
    getIterAtOffset (GtkBuffer sb) offset = GtkIter <$> textBufferGetIterAtOffset sb (fromIntegral offset)
    getLineCount (GtkBuffer sb) = fromIntegral <$> textBufferGetLineCount sb
    getModified (GtkBuffer sb) = textBufferGetModified sb
    getSelectionBoundMark (GtkBuffer sb) = GtkMark <$> textBufferGetSelectionBound sb
    getSelectionBounds (GtkBuffer sb) = (\(_, a, b) -> (GtkIter a, GtkIter b)) <$>
        textBufferGetSelectionBounds sb
    getInsertIter (GtkBuffer sb) = GtkIter <$> do
        insertMark <- textBufferGetInsert sb
        textBufferGetIterAtMark sb insertMark
    getSlice (GtkBuffer sb) (GtkIter first) (GtkIter last) includeHidenChars =
        textBufferGetSlice sb first last includeHidenChars
    getStartIter (GtkBuffer sb) = GtkIter <$> textBufferGetStartIter sb
    getTagTable (GtkBuffer sb) = GtkTagTable <$> textBufferGetTagTable sb
    getText (GtkBuffer sb) (GtkIter first) (GtkIter last) includeHidenChars =
        textBufferGetText sb first last includeHidenChars
    hasSelection (GtkBuffer sb) = (\(b, _, _) -> b) <$> textBufferGetSelectionBounds sb
    insert (GtkBuffer sb) (GtkIter i) text = textBufferInsert sb i text (-1)
    newViewWithMap sb mbFontString = do
        (GtkView sv, _) <- newViewNoScroll sb mbFontString
        grid <- gridNew
        sw <- scrolledWindowNew noAdjustment noAdjustment
        containerAdd sw sv
        mapFrame <- frameNew Nothing
        sMap <- mapNew
        mapSetView sMap sv
        containerAdd mapFrame sMap
        widgetSetHexpand sw True
        widgetSetVexpand sw True
        containerAdd grid sw
        containerAdd grid mapFrame
        return (GtkView sv, sw, grid)
    newView sb mbFontString = do
        (GtkView sv, _) <- newViewNoScroll sb mbFontString
        sw <- scrolledWindowNew noAdjustment noAdjustment
        containerAdd sw sv
        return (GtkView sv, sw)
    newViewNoScroll (GtkBuffer sb) mbFontString = do
        liftIO $ debugM "lekash" "newView (GtkSourceView)"
        prefs <- readIDE prefs
        fd <- fontDescription mbFontString
        sv <- viewNewWithBuffer sb

        -- Disable source_mark_updated handler in sv because it schedules a full redraw
        -- that turns out to be unnecessary and very costly in Leksah
        signal <- signalLookup "source_mark_updated" =<< liftIO (gobjectType (undefined :: Source.Buffer))
        liftIO $ withManagedPtr sv $ \svPtr ->
            signalHandlersBlockMatched sb [SignalMatchTypeId, SignalMatchTypeData]
                signal 0 Nothing nullPtr (castPtr svPtr)

        liftIO $ debugM "lekash" "newView set attirbutes"
        setViewHighlightCurrentLine sv True
        setViewInsertSpacesInsteadOfTabs sv True
        setViewIndentOnTab sv True
        setViewAutoIndent sv True
        setViewSmartHomeEnd sv SmartHomeEndTypeBefore
        setViewShowLineMarks sv True

        liftIO $ debugM "lekash" "newView set up mark attributes"
        forM_ [minBound..maxBound] $ \ refType -> do
            let cat = T.pack $ show refType
                icon = case refType of
                        ErrorRef       -> "ide_error"
                        WarningRef     -> "ide_warning"
                        TestFailureRef -> "software-update-urgent"
                        LintRef        -> "ide_suggestion"
                        BreakpointRef  -> "media-playback-pause"
                        ContextRef     -> "media-playback-start"
            attributes <- markAttributesNew
            markAttributesSetIconName attributes icon
            onMarkAttributesQueryTooltipText attributes $ \ mark ->
                maybe "" (T.drop 1 . T.dropWhile isDigit) <$> textMarkGetName mark
            viewSetMarkAttributes sv cat attributes (fromIntegral $ 1 + fromEnum(maxBound :: LogRefType) - fromEnum refType)
        textViewSetWrapMode sv (if wrapLines prefs
                                    then WrapModeWord
                                    else WrapModeNone)
        widgetOverrideFont sv (Just fd)
        (GtkView sv,) <$> toWidget sv

    pasteClipboard (GtkBuffer sb) clipboard (GtkIter i) defaultEditable =
        textBufferPasteClipboard sb clipboard (Just i) defaultEditable
    placeCursor (GtkBuffer sb) (GtkIter i) = textBufferPlaceCursor sb i
    redo (GtkBuffer sb) = bufferRedo sb
    removeTagByName (GtkBuffer sb) name = do
        first <- textBufferGetStartIter sb
        last <- textBufferGetEndIter sb
        textBufferRemoveTagByName sb name first last
        bufferRemoveSourceMarks sb first last (Just name)
    selectRange (GtkBuffer sb) (GtkIter first) (GtkIter last) =
        textBufferSelectRange sb first last
    setModified (GtkBuffer sb) modified = void $ textBufferSetModified sb modified
    setStyle (GtkBuffer sb) EditorStyle {..} = do
        liftIO $ debugM "lekash" "setStyle (GtkSourceView)"
        case styleName of
            Nothing  -> return ()
            Just str -> do
                styleManager <- liftIO styleSchemeManagerNew
                dataDir <- liftIO getDataDir
                styleSchemeManagerAppendSearchPath styleManager . T.pack $ dataDir </> "data/styles"
                ids <- fromMaybe [] <$> styleSchemeManagerGetSchemeIds styleManager
                let preferedNames = if preferDark then [str<>"-dark", str] else [str]
                forM_ (take 1 $ filter (`elem` ids) preferedNames) $ \ name -> do
                    scheme <- styleSchemeManagerGetScheme styleManager name
                    bufferSetStyleScheme sb (Just scheme)
                    tagTable <- getTagTable (GtkBuffer sb)
                    let isDark = name `elem` ["leksah-dark", "oblivion", "cobalt", "industrial"]
                        setBG (dark, light) (Just tag) = background tag (if isDark then dark else light)
                        setBG _             Nothing    = return ()
                        setUnderline mbCol = maybe (return ()) (\tag -> underline tag UnderlineError mbCol)
                    -- This is ugly, we just have to make sure we only provide
                    -- styleschemes that include styles for these tags
                    let applyDefaultStyling tagName = do
                            mbTag <- lookupTag tagTable tagName
                            case tagName of
                                "selection-match" -> setBG matchBG mbTag
                                "search-match" -> setBG foundBG mbTag
                                "BreakpointRef" ->  setBG breakpointBG mbTag
                                "ContextRef" -> setBG contextBG mbTag
                                "ErrorRef" -> setUnderline Nothing mbTag
                                "WarningRef" -> setUnderline (Just $ Color 214 176 4) mbTag
                                "TestFailureRef" -> setUnderline (Just $ Color 207 18 241) mbTag
                                "LintRef" -> setUnderline (Just $ Color 21 110 209) mbTag
                                _ -> return ()
                    let tagNames = ["selection-match", "search-match"]
                                       ++ map (T.pack . show)
                                              [ErrorRef, WarningRef, TestFailureRef, LintRef, BreakpointRef, ContextRef]
                    forM_ tagNames $ \tagName -> do
                        setTagStyle sb tagName scheme applyDefaultStyling

    setText (GtkBuffer sb) text = setTextBufferText sb text
    undo (GtkBuffer sb) = bufferUndo sb

    afterChanged (GtkBuffer sb) f = do
        ideR <- ask
        id1 <- ConnectC sb <$> afterTextBufferChanged sb (reflectIDE f ideR)
        return [id1]

    afterModifiedChanged (GtkBuffer sb) f = do
        ideR <- ask
        id1 <- ConnectC sb <$> afterTextBufferModifiedChanged sb (reflectIDE f ideR)
        return [id1]

    -- View
    bufferToWindowCoords (GtkView sv) (x, y) = (fromIntegral *** fromIntegral) <$>
        textViewBufferToWindowCoords sv TextWindowTypeWidget (fromIntegral x) (fromIntegral y)
    drawTabs (GtkView sv) = viewSetDrawSpaces sv [DrawSpacesFlagsTab, DrawSpacesFlagsSpace, DrawSpacesFlagsTrailing]
    getBuffer (GtkView sv) = GtkBuffer <$> (getTextViewBuffer sv >>= (liftIO . unsafeCastTo Source.Buffer))
    getWindow (GtkView sv) = widgetGetWindow sv
    getIterAtLocation (GtkView sv) x y = GtkIter
#ifdef MIN_VERSION_GTK_3_20
        . snd
#endif
        <$> textViewGetIterAtLocation sv (fromIntegral x) (fromIntegral y)
    getIterLocation (GtkView sv) (GtkIter i) = textViewGetIterLocation sv i
    getOverwrite (GtkView sv) = textViewGetOverwrite sv
    getScrolledWindow (GtkView sv) = widgetGetParent sv >>= \case
        Just v -> liftIO $ unsafeCastTo ScrolledWindow v
        Nothing -> error "getScrolledWindow failed"
    getEditorWidget (GtkView sv) = liftIO $ toWidget sv
    grabFocus (GtkView sv) = widgetGrabFocus sv
    scrollToMark (GtkView sv) (GtkMark m) withMargin mbAlign = uncurry (textViewScrollToMark sv m withMargin (isJust mbAlign)) $ fromMaybe (0,0) mbAlign
    scrollToIter (GtkView sv) (GtkIter i) withMargin mbAlign = void $ uncurry (textViewScrollToIter sv i withMargin (isJust mbAlign)) $ fromMaybe (0,0) mbAlign
    setFont (GtkView sv) mbFontString = do
        fd <- fontDescription mbFontString
        widgetModifyFont sv (Just fd)
    setIndentWidth (GtkView sv) width = setViewIndentWidth sv (fromIntegral width)
    setWrapMode v@(GtkView sv) wrapLines = do
        sw <- getScrolledWindow v
        if wrapLines
            then do
                textViewSetWrapMode sv WrapModeWord
                scrolledWindowSetPolicy sw PolicyTypeNever PolicyTypeAutomatic
            else do
                textViewSetWrapMode sv WrapModeNone
                scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
    setRightMargin (GtkView sv) mbRightMargin =
        case mbRightMargin of
            Just n -> do
                setViewShowRightMargin sv True
                setViewRightMarginPosition sv (fromIntegral n)
            Nothing -> setViewShowRightMargin sv False
    setShowLineNumbers (GtkView sv) = setViewShowLineNumbers sv
    setShowLineMarks (GtkView sv) = setViewShowLineMarks sv
    setHighlightCurrentLine (GtkView sv) = setViewHighlightCurrentLine sv
    setTabWidth (GtkView sv) width = setViewTabWidth sv (fromIntegral width)

    -- Events
    afterFocusIn (GtkView sv) f = do
        ideR <- ask
        id1 <- ConnectC sv <$> afterWidgetFocusInEvent sv (\e -> reflectIDE f ideR >> return False)
        return [id1]
    afterMoveCursor v@(GtkView sv) f = do
        ideR <- ask
        (GtkBuffer sb) <- getBuffer v
        id1 <- ConnectC sv <$> afterTextViewMoveCursor sv (\_ _ _ -> reflectIDE f ideR)
        widgetAddEvents sv [EventMaskButtonReleaseMask]
        id2 <- ConnectC sv <$> onWidgetButtonReleaseEvent sv (\e -> reflectIDE f ideR >> return False)
        id3 <- ConnectC sb <$> afterTextBufferEndUserAction sb (reflectIDE f ideR)
        return [id1, id2, id3]
    afterToggleOverwrite (GtkView sv) f = do
        ideR <- ask
        id1 <- ConnectC sv <$> afterTextViewToggleOverwrite sv (reflectIDE f ideR)
        return [id1]
    onButtonPress (GtkView sv) f = do
        id1 <- onIDE onWidgetButtonPressEvent sv f
        return [id1]
    onButtonRelease (GtkView sv) f = do
        id1 <- onIDE onWidgetButtonReleaseEvent sv f
        return [id1]
    onCompletion v@(GtkView sv) start cancel = do
        ideR <- ask
        (GtkBuffer sb) <- getBuffer v
        -- when multiple afterBufferInsertText are called quickly,
        -- we cancel previous idle action which was not processed,
        -- its handler is stored here.
        -- Paste operation is example of such sequential events (each word!).
        lastHandler <- liftIO $ newIORef Nothing
        id1 <- ConnectC sb <$> afterTextBufferInsertText sb (\iter text _ -> do
            lh <- readIORef lastHandler
            debugM "leksah" $ "Removing " <> show lh
            mapM_ sourceRemove $ maybeToList lh
            h <- idleAdd PRIORITY_DEFAULT $ do
                writeIORef lastHandler Nothing
                let isIdent a = isAlphaNum a || a == '\'' || a == '_' || a == '.'
                let isOp    a = isSymbol   a || a == ':'  || a == '\\' || a == '*' || a == '/' || a == '-'
                                             || a == '!'  || a == '@' || a == '%' || a == '&' || a == '?'
                if T.all isIdent text || T.all isOp text
                    then do
                        (hasSel, _, _) <- textBufferGetSelectionBounds sb
                        if not hasSel
                            then do
                                reflectIDE start ideR
                                return False
                            else do
                                reflectIDE cancel ideR
                                return False
                    else do
                        reflectIDE cancel ideR
                        return False
            writeIORef lastHandler (Just h)
            return ())
        id2 <- ConnectC sv <$> onTextViewMoveCursor sv (\_ _ _ -> do
            readIORef lastHandler >>= mapM_ sourceRemove . maybeToList
            reflectIDE cancel ideR)
        id3 <- ConnectC sv <$> onWidgetButtonPressEvent sv (\e -> reflectIDE cancel ideR >> return False)
        id4 <- ConnectC sv <$> onWidgetFocusOutEvent sv (\e -> reflectIDE cancel ideR >> return False)
        return [id1, id2, id3, id4]
    onKeyPress (GtkView sv) f = do
        id1 <- onIDE onWidgetKeyPressEvent sv f
        return [id1]
    onMotionNotify (GtkView sv) f = do
        id1 <- onIDE onWidgetMotionNotifyEvent sv f
        return [id1]
    onLeaveNotify (GtkView sv) f = do
        id1 <- onIDE onWidgetLeaveNotifyEvent sv f
        return [id1]
    onKeyRelease (GtkView sv) f = do
        id1 <- onIDE onWidgetKeyReleaseEvent sv f
        return [id1]
    onLookupInfo (GtkView sv) f = do
        widgetAddEvents sv [EventMaskButtonReleaseMask]
        id1 <- onIDE onWidgetButtonReleaseEvent sv $ do
            e <- lift ask
            mod <- getEventButtonState e
            case mod of
                [ModifierTypeControlMask] -> f >> return True
                _             -> return False
        return [id1]
    onMotionNotifyEvent (GtkView sv) handler = do
        widgetAddEvents sv [EventMaskButtonMotionMask, EventMaskButton1MotionMask]  -- TODO: this doesn't work yet event gets fired anyways: restrict event to being fired when left mouse button is pressed down
        id1 <- onIDE onWidgetMotionNotifyEvent sv handler  --TODO this is potentially slowing leksah, a better event (if there was any) could be more efficient here
        widgetAddEvents sv [EventMaskButtonMotionMask, EventMaskButton1MotionMask]  -- TODO: this doesn't work yet event gets fired anyways: restrict event to being fired when left mouse button is pressed down
        return [id1]
    onPopulatePopup (GtkView sv) f = do
        ideR <- ask
        id1 <- ConnectC sv <$> onTextViewPopulatePopup sv (\menu -> reflectIDE (f =<< liftIO (unsafeCastTo Menu menu)) ideR)
        return [id1]
    onSelectionChanged (GtkBuffer sb) handler = do
        ideR <- ask
        id1 <- ConnectC sb <$> onTextBufferMarkSet sb (\ _ mark -> do
            name <- textMarkGetName mark
            when (name == Just "insert") $ reflectIDE handler ideR)
        return [id1]

    -- Iter
    backwardCharC i = transformGtkIter i textIterBackwardChar
    backwardFindCharC i pred mbLimit = transformGtkIterMaybe i $ \x ->
        textIterBackwardFindChar x (return . pred) $
            case mbLimit of
                Just (GtkIter limit) -> Just limit
                Nothing              -> Nothing
    backwardWordStartC i = transformGtkIterMaybe i textIterBackwardWordStart
    backwardToLineStartC i = transformGtkIter i $ \new -> do
        n <- textIterGetLineOffset new
        textIterBackwardChars new n
        return ()
    endsWord (GtkIter i) = textIterEndsWord i
    forwardCharC i = transformGtkIter i textIterForwardChar
    forwardCharsC i n = transformGtkIter i $ flip textIterForwardChars (fromIntegral n)
    forwardFindCharC i pred mbLimit = transformGtkIterMaybe i $ \x ->
        textIterForwardFindChar x (return . pred) $
            case mbLimit of
                Just (GtkIter limit) -> Just limit
                Nothing              -> Nothing
    forwardSearch (GtkIter i) str flags mbLimit =
        textIterForwardSearch i str flags (
            case mbLimit of
                Just (GtkIter limit) -> Just limit
                Nothing              -> Nothing) >>= \case
                    (False, _, _) -> return Nothing
                    (True, a, b)  -> return $ Just (GtkIter a, GtkIter b)
    forwardToLineEndC i = transformGtkIter i textIterForwardToLineEnd
    forwardWordEndC i = transformGtkIterMaybe i textIterForwardWordEnd
    getChar (GtkIter i) = textIterGetChar i >>= \case '\0' -> return Nothing
                                                      c    -> return $ Just c
    getCharsInLine (GtkIter i) = fromIntegral <$> textIterGetCharsInLine i
    getLine (GtkIter i) = fromIntegral <$> textIterGetLine i
    getLineOffset (GtkIter i) = fromIntegral <$> textIterGetLineOffset i
    getOffset (GtkIter i) = fromIntegral <$> textIterGetOffset i
    isStart (GtkIter i) = textIterIsStart i
    isEnd (GtkIter i) = textIterIsEnd i
    iterEqual (GtkIter i1) (GtkIter i2) = textIterEqual i1 i2
    startsLine (GtkIter i) = textIterStartsLine i
    startsWord (GtkIter i) = textIterStartsWord i
    atEnd (GtkIter i) = GtkIter <$> do
        buffer <- textIterGetBuffer i
        textBufferGetEndIter buffer
    atLine i line = transformGtkIter i $ flip textIterSetLine (fromIntegral line)
    atLineOffset i column = transformGtkIter i $ flip textIterSetLineOffset (fromIntegral column)
    atOffset i offset = transformGtkIter i $ flip textIterSetOffset (fromIntegral offset)
    atStart (GtkIter i) = GtkIter <$> do
        buffer <- textIterGetBuffer i
        textBufferGetEndIter buffer

    -- Tag Table
    newTag (GtkTagTable tt) name = GtkTag <$> do
        t <- textTagNew (Just name)
        textTagTableAdd tt t
        return t
    lookupTag (GtkTagTable tt) name = fmap GtkTag <$> textTagTableLookup tt name

    -- Tag
    background (GtkTag t) color = setTextTagBackground t . T.pack $ colorHexString color
    underline (GtkTag t) value Nothing = setTextTagUnderline t value
    underline (GtkTag t) value (Just (Color r g b)) = do
        col <- newZeroRGBA
        setRGBARed col (fromIntegral r / 255)
        setRGBAGreen col (fromIntegral g / 255)
        setRGBABlue col (fromIntegral b / 255)
        setRGBAAlpha col 1
        setTextTagUnderline t value
        setTextTagUnderlineRgba t col
    setEditable (GtkView view) b = textViewSetEditable view b

