{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
#ifdef LEKSAH_WITH_CODE_MIRROR
{-# LANGUAGE RecordWildCards #-}
#endif
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor.CodeMirror
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

module IDE.TextEditor.CodeMirror (
    CodeMirror(..)
#ifdef LEKSAH_WITH_CODE_MIRROR
  , TextEditor(..)
  , EditorBuffer(..)
  , EditorView(..)
  , EditorIter(..)
  , EditorMark(..)
  , EditorTag(..)
  , EditorTagTable(..)

  , newCMBuffer
#endif
) where

import Data.Typeable (Typeable)
import Data.Text (Text)
import Text.Show (Show)
import Data.Tuple (snd, fst)
import Data.Function (($), (.))
import Data.Maybe (Maybe, Maybe(..))
import GHC.Base (Functor(..), Monad(..))
import Data.Int (Int)
import System.IO (FilePath)
import Data.List ((++))
import Data.Bool (Bool(..), not)
import GHC.Real (fromIntegral, RealFrac(..))
import GHC.Num (Num(..))
import Data.Eq (Eq(..))
import GHC.Float (Double)
import qualified Data.Text as T (pack)
import Control.Lens.Getter (to)
import Control.Monad (liftM, (=<<))
import GI.WebKit.Objects.WebView
       (onWebViewPopulatePopup, webViewLoadString, webViewGetMainFrame,
        onWebViewLoadFinished, webViewNew, WebView(..))
import GI.Gtk.Functions (mainIteration)
import GI.Gtk.Objects.ScrolledWindow
       (ScrolledWindow(..), scrolledWindowSetShadowType,
        scrolledWindowNew)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Enums (ShadowType(..))
import GI.Gtk.Objects.Container (containerAdd)
import GI.WebKit.Objects.WebFrame (webFrameGetGlobalContext)
import GI.Gtk.Objects.Widget
       (widgetAddEvents, onWidgetKeyReleaseEvent,
        onWidgetLeaveNotifyEvent, onWidgetMotionNotifyEvent,
        onWidgetKeyPressEvent, onWidgetButtonReleaseEvent,
        onWidgetButtonPressEvent, afterWidgetFocusInEvent, widgetGrabFocus,
        toWidget, widgetGetParent, widgetGetWindow)
import Graphics.UI.Frame.Rectangle
       (rectangleHeight, rectangleWidth, rectangleY, rectangleX,
        newRectangle)
import Data.GI.Base.ManagedPtr (withManagedPtr, unsafeCastTo)
import GI.Gdk.Flags (ModifierType(..), EventMask(..))
import GI.Gdk.Structs.EventButton (eventButtonReadState)
import GI.JavaScriptCore.Structs.GlobalContext (GlobalContext(..))
import Foreign.Ptr (castPtr)
import Data.GI.Base.BasicConversions (gflagsToWord)
import Data.GI.Base.Constructible (Constructible(..))
import Data.GI.Base.Attributes (AttrOp(..))
import Data.GI.Base.BasicTypes (nullToNothing)

#ifdef LEKSAH_WITH_CODE_MIRROR
import Control.Monad (unless)
import Data.Text (pack, unpack)
import IDE.TextEditor.Class (TextEditor(..))
import Control.Monad.Reader (ReaderT(..))
import Language.Javascript.JSaddle
       (valToObject, (#), JSContextRef, Object, jsg, jsg2, (<#), obj, js2, jss,
        js, JSM, js1, valToText, valToStr, js3, js0, ToJSVal(..), ToJSString(..),
        JSString, JSVal, valToBool, strToText, valToNumber, MakeObject(..))
import Control.Applicative ((<$>))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Concurrent (putMVar, newEmptyMVar, takeMVar, MVar, tryTakeMVar)
import IDE.Core.Types (IDEM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Lens ((^.), IndexPreservingGetter)
import qualified GHCJS.CodeMirror as CM (getDataDir)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Data.Maybe (fromJust)
import IDE.Core.State (onIDE, reflectIDE, leksahOrPackageDir)
import Graphics.UI.Editor.Basics (Connection(..))
import System.Log.Logger (debugM)
#endif

data CodeMirror = CodeMirror deriving( Typeable, Show )

#ifdef LEKSAH_WITH_CODE_MIRROR

data CodeMirrorState = CodeMirrorState {
    cmContext        :: GlobalContext
  , cmObject         :: Object }
type CM = ReaderT (WebView, CodeMirrorState) JSM
webView :: CM WebView
webView = fst <$> ask
codeMirror :: CM Object
codeMirror = cmObject . snd <$> ask
runCM :: CodeMirrorRef -> CM a -> IDEM a
runCM (v, mvar) f = liftIO $ do
    s <- guiTakeMVar mvar
    withManagedPtr (cmContext s) (runReaderT (runReaderT f (v, s)) . castPtr)
  where
    guiTakeMVar mvar = do
        maybeValue <- tryTakeMVar mvar
        case maybeValue of
            Just value -> do
                putMVar mvar value
                return value
            Nothing    -> do
                debugM "leksah" "looping"
                s <- loop mvar
                debugM "leksah" "done looping"
                return s
    loop mvar = do
        maybeValue <- tryTakeMVar mvar
        case maybeValue of
            Just value -> do
                putMVar mvar value
                return value
            Nothing    -> do
                mainIteration
                loop mvar

type CodeMirrorRef = (WebView, MVar CodeMirrorState)

gCodeMirror x y   = jsg2 "CodeMirror" x y >>= valToObject
body              = js  "body"
setValue v        = jss "value" v
setSize           = js2 "setSize"
setMode m         = jss "mode" m
line              = js  "line" . to valToNumber
setLine l         = jss "line" l
ch                = js  "ch" . to valToNumber
setCh c           = jss "ch" c
setLeft l         = jss "left" l
setTop t          = jss "top" t
left              = js  "left" . to valToNumber
top               = js  "top" . to valToNumber
right             = js  "right" . to valToNumber
bottom            = js  "bottom" . to valToNumber
lastLine          = js0 "lastLine" . to valToNumber
getRange x y      = js2 "getRange" x y . to valToStr
callSetValue      = js1 "setValue"
setBookmark'      = js2 "setBookmark"
find              = js0 "find" . to valToObject
from              = js  "from" . to valToObject
getCursor x       = js1 "getCursor" x . to valToObject
isClean           = js0 "isClean" . to valToBool
markText          = js3 "markText"
className         = "className"
clearHistory      = js0 "clearHistory"
callUndo          = js0 "undo"
undo'             = js  "undo" . to valToBool
callRedo          = js0 "redo"
redo'             = js  "redo" . to valToBool
historySize       = js0 "historySize"
replaceRange      = js3 "replaceRange"
insertAt          = js2 "replaceRange"
replaceSelection  = js1 "replaceSelection"
posFromIndex x    = js1 "posFromIndex" x . to valToObject
lineCount         = js0 "lineCount" . to valToNumber
somethingSelected = js0 "somethingSelected" . to valToBool
setSelection      = js2 "setSelection"
placeCursorAt     = js1 "setSelection"
markClean         = js0 "markClean"
coordsChar pos n  = js2 "coordsChar" pos n . to valToObject
charCoords        = js2 "charCoords"
scrollIntoView    = js2 "scrollIntoView"
getAllMarks       = js0 "getAllMarks"
indexFromPos p    = js1 "indexFromPos" p . to valToNumber
getLineText l     = js1 "getLine" l
jsLength          = js  "length" . to valToNumber

cmIter :: CodeMirrorRef -> Int -> Int -> CM (EditorIter CodeMirror)
cmIter cm l c =
    lift $ do
        i <- obj
        i ^. setLine (fromIntegral l :: Double)
        i ^. setCh   (fromIntegral c :: Double)
        return $ CMIter cm i

newCMBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer CodeMirror)
newCMBuffer mbFilename contents = do
    ideR <- ask
    liftIO $ do
        debugM "leksah" "newCMBuffer"
        scrolledWindow <- scrolledWindowNew noAdjustment noAdjustment
        scrolledWindowSetShadowType scrolledWindow ShadowTypeIn
        cmWebView <- webViewNew
        containerAdd scrolledWindow cmWebView
        dataDir <- liftIO $ leksahOrPackageDir "ghcjs-codemirror" CM.getDataDir
        s <- newEmptyMVar
        onWebViewLoadFinished cmWebView $ \ _ -> do
            debugM "leksah" "newCMBuffer loadFinished"
            cmContext <- webViewGetMainFrame cmWebView >>= webFrameGetGlobalContext
            let runjs f = withManagedPtr cmContext (runReaderT f . castPtr)

            runjs $ do
                document <- jsg "document"
                code <- obj
                code ^. setValue contents
                code ^. setMode "haskell"
                cmObject <- gCodeMirror (document ^. body) code

                cmObject ^. setSize "100%" "100%"
                liftIO $ debugM "leksah" "newCMBuffer loaded"
                liftIO . putMVar s $ CodeMirrorState{..}

        webViewLoadString cmWebView (T.pack $
                   "<html><head>"
                ++ "<script src=\"lib/codemirror.js\">"
                ++ "<link rel=\"stylesheet\" href=\"lib/codemirror.css\">"
                ++ "<script src=\"mode/javascript/javascript.js\">"
                ++ "<script src=\"mode/haskell/haskell.js\">"
                ++ "</head>"
                ++ "<body style=\"margin:0;padding:0 auto;\">"
                ++ "</body></html>"
            ) (T.pack "text/html") (T.pack "UTF-8") (T.pack $ "file://" ++ dataDir ++ "/codemirror.html")
        debugM "leksah" "newCMBuffer loading"
        return $ CMBuffer (cmWebView, s)

instance TextEditor CodeMirror where
    data EditorBuffer CodeMirror = CMBuffer CodeMirrorRef
    data EditorView CodeMirror = CMView CodeMirrorRef
    data EditorMark CodeMirror = CMMark Object | CMCursor JSVal
    data EditorIter CodeMirror = CMIter CodeMirrorRef Object
    data EditorTagTable CodeMirror = CMTagTable CodeMirrorRef
    data EditorTag CodeMirror = CMTag

    newBuffer = newCMBuffer
    applyTagByName (CMBuffer cm) name (CMIter _ first) (CMIter _ last) = runCM cm $ do
        m <- codeMirror
        lift $ do
            o <- obj
            (o <# className) name
            m ^. markText first last o
            return ()
    beginNotUndoableAction (CMBuffer cm) = return () -- TODO
    beginUserAction (CMBuffer cm) = return () -- TODO
    canRedo (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. historySize ^. redo'
    canUndo (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. historySize ^. undo'
    copyClipboard (CMBuffer cm) _ = return () -- TODO
    createMark (CMView cm) _refType (CMIter _ i) _tooltip = runCM cm $ do
        m <- codeMirror
        lift $ do
                o <- obj
                m ^. setBookmark' i o
                return ()
    cutClipboard (CMBuffer cm) _ _ = return () -- TODO
    delete (CMBuffer cm) (CMIter _ first) (CMIter _ last) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. replaceRange "" first last
        return ()
    deleteSelection (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. replaceSelection ""
        return ()
    endNotUndoableAction (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. clearHistory
        return ()
    endUserAction (CMBuffer cm) = return () -- TODO
    getEndIter (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ do
            i <- obj
            l <- m ^. lastLine
            i ^. setLine l
            i ^. setCh (m ^. getLineText l ^. jsLength)
            return $ CMIter cm i
    getInsertMark (CMBuffer cm) = runCM cm . lift $ CMCursor <$> toJSVal "head"
    getIterAtLine (CMBuffer cm) line = runCM cm $ cmIter cm line 0
    getIterAtMark (CMBuffer cm) (CMMark mark) = runCM cm $
        lift $ CMIter cm <$> mark ^. find ^. from
    getIterAtMark (CMBuffer cm) (CMCursor c) = runCM cm $ do
        m <- codeMirror
        lift $ CMIter cm <$> m ^. getCursor c
    getIterAtOffset (CMBuffer cm) offset = runCM cm $ do
        m <- codeMirror
        lift $ CMIter cm <$> m ^. posFromIndex (fromIntegral offset :: Double)
    getLineCount (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ round <$> m ^. lineCount
    getModified (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ not <$> m ^. isClean
    getSelectionBoundMark (CMBuffer cm) = runCM cm . lift $ CMCursor <$> toJSVal "anchor"
    getSelectionBounds (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ do
            start <- m ^. getCursor "start"
            end   <- m ^. getCursor "end"
            return (CMIter cm start, CMIter cm end)
    getInsertIter (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ CMIter cm <$> m ^. getCursor "head"
    getSlice (CMBuffer cm) (CMIter _ first) (CMIter _ last) includeHidenChars = runCM cm $ do
        m <- codeMirror
        lift $ m ^. getRange first last >>= strToText
    getStartIter (CMBuffer cm) = runCM cm $ cmIter cm 0 0
    getTagTable (CMBuffer cm) = return $ CMTagTable cm
    getText (CMBuffer cm) (CMIter _ first) (CMIter _ last) includeHidenChars = runCM cm $ do
        m <- codeMirror
        lift $ m ^. getRange first last >>= strToText
    hasSelection (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. somethingSelected
    insert (CMBuffer cm) (CMIter _ p) text = runCM cm $ do
        m <- codeMirror
        lift $ m ^. insertAt text p >> return ()
    newView (CMBuffer cm) mbFontString = return (CMView cm)
    pasteClipboard (CMBuffer cm) clipboard (CMIter _ p) defaultEditable = return () -- TODO
    placeCursor (CMBuffer cm) (CMIter _ i) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. placeCursorAt i >> return ()
    redo (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. callRedo
        return ()
    removeTagByName (CMBuffer cm) name =  runCM cm $ do
        m <- codeMirror
        lift $ do
            marks <- m ^. getAllMarks
            -- TODO
            return ()
    selectRange (CMBuffer cm) (CMIter _ first) (CMIter _ last) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. setSelection first last >> return ()
    setModified (CMBuffer cm) modified = unless modified . runCM cm $ do
        m <- codeMirror
        lift $ m ^. markClean >> return ()
    setStyle (CMBuffer cm) _style = return () -- TODO
    setText (CMBuffer cm) text = runCM cm $ do
        m <- codeMirror
        lift $ m ^. callSetValue text
        return ()
    undo (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ m ^. callUndo
        return ()
    bufferToWindowCoords (CMView cm) point = return point -- TODO
    drawTabs (CMView _) = return () -- TODO
    getBuffer (CMView cm) = return $ CMBuffer cm
    getWindow (CMView cm) = runCM cm $ do
        v <- webView
        nullToNothing $ widgetGetWindow v
    getIterAtLocation (CMView cm) x y = runCM cm $ do
        m <- codeMirror
        lift $ do
            pos <- obj
            pos ^. setLeft (fromIntegral x :: Double)
            pos ^. setTop (fromIntegral y :: Double)
            CMIter cm <$> (m ^. coordsChar pos "window")
    getIterLocation (CMView cm) (CMIter _ i) = runCM cm $ do
        m <- codeMirror
        lift $ do
            rect <- m ^. charCoords i "window"
            l <- rect ^. left
            r <- rect ^. right
            t <- rect ^. top
            b <- rect ^. bottom
            newRectangle [
                rectangleX := (round l),
                rectangleY := (round t),
                rectangleWidth := (round $ r - l),
                rectangleHeight := (round $ b - t)]
    getOverwrite (CMView cm) = return False -- TODO
    getScrolledWindow (CMView (v,_)) = nullToNothing (widgetGetParent v) >>= (liftIO . unsafeCastTo ScrolledWindow . fromJust)
    getEditorWidget (CMView (v,_)) = liftIO $ toWidget v
    grabFocus (CMView cm) = runCM cm $ do
        v <- webView
        widgetGrabFocus v
    scrollToMark (CMView cm) m withMargin mbAlign = do
        i <- getIterAtMark (CMBuffer cm) m
        scrollToIter (CMView cm) i withMargin mbAlign
    scrollToIter (CMView cm) (CMIter _ i) withMargin mbAlign = runCM cm $ do
        m <- codeMirror
        lift $ m ^. scrollIntoView i withMargin >> return ()
    setFont (CMView cm) mbFontString = return () -- TODO
    setIndentWidth (CMView cm) width = return () -- TODO
    setWrapMode (CMView cm) width = return () -- TODO
    setRightMargin (CMView cm) mbRightMargin = return () -- TODO
    setShowLineNumbers (CMView cm) show = return () -- TODO
    setTabWidth (CMView cm) width = return () -- TODO
    backwardCharC (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ do
            n <- m ^. indexFromPos i
            i2 <- m ^. posFromIndex (n - 1)
            return (CMIter cm i2)
    backwardFindCharC (CMIter cm i) pred mbLimit = return Nothing -- TODO
    backwardWordStartC (CMIter cm i) = return Nothing -- TODO
    backwardToLineStartC (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            l <- i ^. line
            i2 ^. setLine l
            i2 ^. setCh (0 :: Double)
            return $ CMIter cm i2
    endsWord (CMIter cm i) = return False -- TODO
    forwardCharC (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ do
            n <- m ^. indexFromPos i
            i2 <- m ^. posFromIndex (n + 1)
            return (CMIter cm i2)
    forwardCharsC (CMIter cm i) d = runCM cm $ do
        m <- codeMirror
        lift $ do
            n <- m ^. indexFromPos i
            i2 <- m ^. posFromIndex (n + fromIntegral d)
            return (CMIter cm i2)
    forwardFindCharC (CMIter cm i) pred mbLimit = return Nothing -- TODO
    forwardSearch (CMIter cm i) str pred mbLimit = return Nothing -- TODO
    forwardToLineEndC (CMIter cm i) =  runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            l <- i ^. line
            i2 ^. setLine l
            i2 ^. setCh (m ^. getLineText l ^. jsLength)
            return $ CMIter cm i2
    forwardWordEndC (CMIter cm i) = return Nothing -- TODO
    getChar (CMIter cm i) = return Nothing -- TODO
    getCharsInLine (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ round <$> m ^. getLineText (i ^. line) ^. jsLength
    getLine (CMIter cm i) = runCM cm $
        lift $ round <$> i ^. line
    getLineOffset (CMIter cm i) = runCM cm $
        lift $ round <$> i ^. ch
    getOffset (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ round <$> m ^. indexFromPos i
    isStart i@(CMIter cm _) = do
        start <- getStartIter (CMBuffer cm)
        iterEqual start i
    isEnd i@(CMIter cm _) = do
        end <- getEndIter (CMBuffer cm)
        iterEqual end i
    iterEqual (CMIter cm i1) (CMIter _ i2) = runCM cm . lift $ do
        l1 <- i1 ^. line
        l2 <- i2 ^. line
        if l1 /= l2
            then return False
            else do
                c1 <- i1 ^. ch
                c2 <- i2 ^. ch
                return $ c1 == c2
    startsLine i = (== 0) <$> getLineOffset i
    startsWord (CMIter cm i) = return False -- TODO
    atEnd (CMIter cm _) = getEndIter (CMBuffer cm)
    atLine (CMIter cm _) l = runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            i2 ^. setLine (fromIntegral l :: Double)
            i2 ^. setCh (0 :: Double)
            return $ CMIter cm i2
    atLineOffset (CMIter cm i) column = runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            l <- i ^. line
            i2 ^. setLine l
            i2 ^. setCh (fromIntegral column :: Double)
            return $ CMIter cm i2
    atOffset (CMIter cm _ ) offset = getIterAtOffset (CMBuffer cm) offset
    atStart (CMIter cm _) = getStartIter (CMBuffer cm)
    newTag (CMTagTable cm) name = return CMTag -- TODO
    lookupTag (CMTagTable cm) name = return Nothing -- TODO
    background (CMTag) color = return () -- TODO
    underline (CMTag) value = return () -- TODO
    afterFocusIn (CMView (v, _)) f = do
        ideR <- ask
        liftIO $ do
            id1 <- afterWidgetFocusInEvent v $ \e -> reflectIDE f ideR >> return False
            return [ConnectC v id1]
    afterModifiedChanged (CMBuffer cm) f = return [] -- TODO
    afterMoveCursor (CMView cm) f = return [] -- TODO
    afterToggleOverwrite (CMView cm) f = return [] -- TODO
    onButtonPress (CMView (v, _)) f = do
        id1 <- onIDE onWidgetButtonPressEvent v f
        return [id1]
    onButtonRelease (CMView (v, _)) f = do
        id1 <- onIDE onWidgetButtonReleaseEvent v f
        return [id1]
    onCompletion (CMView cm) start cancel = return [] -- TODO
    onKeyPress (CMView (v, _)) f = do
        id1 <- onIDE onWidgetKeyPressEvent v f
        return [id1]
    onMotionNotify (CMView (v, _)) f = do
        id1 <- onIDE onWidgetMotionNotifyEvent v f
        return [id1]
    onLeaveNotify (CMView (v, _)) f = do
        id1 <- onIDE onWidgetLeaveNotifyEvent v f
        return [id1]
    onKeyRelease (CMView (v, _)) f = do
        id1 <- onIDE onWidgetKeyReleaseEvent v f
        return [id1]
    onLookupInfo (CMView (v, _)) f = do
        ideR <- ask
        widgetAddEvents v (gflagsToWord [EventMaskButtonReleaseMask])
        id1 <- onIDE onWidgetButtonReleaseEvent v $ do
            e <- lift ask
            mod <- liftIO $ eventButtonReadState e
            case mod of
                [ModifierTypeControlMask] -> f >> return True
                _                         -> return False
        return [id1]
    onMotionNotifyEvent (CMView cm) f = return [] -- TODO
    onPopulatePopup (CMView (v, _)) f = do
        ideR <- ask
        id1 <- onWebViewPopulatePopup v $ \menu -> do
             reflectIDE (f menu) ideR
        return [ConnectC v id1]
    onSelectionChanged (CMBuffer cm) f = return [] -- TODO
#endif

