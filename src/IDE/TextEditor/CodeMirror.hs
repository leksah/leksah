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
import Graphics.UI.Gtk (scrolledWindowSetShadowType)
import Graphics.UI.Gtk.General.Enums (ShadowType(..))
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

#ifdef LEKSAH_WITH_CODE_MIRROR
import Control.Monad (unless)
import Data.Text (pack, unpack)
import IDE.TextEditor.Class (TextEditor(..))
import Graphics.UI.Gtk.WebKit.Types (WebView(..))
import Control.Monad.Reader (ReaderT(..))
import Language.Javascript.JSaddle
       (valToObject, (#), JSContextRef, JSObjectRef, jsg, (<#), obj, js2,
        js, JSM, js1, valToText, valToStr, js3, js0, MakeValueRef(..), MakeStringRef(..),
        JSStringRef, JSValueRef, valToBool, strToText, valToNumber, MakeObjectRef)
import Control.Applicative ((<$>))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Concurrent (putMVar, newEmptyMVar, takeMVar, MVar, tryTakeMVar)
import IDE.Core.Types (IDEM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Lens ((^.), IndexPreservingGetter)
import Graphics.UI.Gtk.WebKit.WebView
       (webViewLoadUri, webViewLoadString, webViewGetMainFrame,
        loadFinished, webViewNew)
import qualified GHCJS.CodeMirror as CM (getDataDir)
import System.Glib.Signals (after, on)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame
       (webFrameGetGlobalContext)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Graphics.UI.Gtk
       (ScrolledWindow, menuPopup, menuAttachToWidget, menuNew,
        popupMenuSignal, eventModifier, widgetAddEvents, keyReleaseEvent,
        leaveNotifyEvent, motionNotifyEvent, keyPressEvent,
        buttonReleaseEvent, buttonPressEvent, focusInEvent,
        widgetGrabFocus, widgetGetParent, castToScrolledWindow,
        containerAdd, scrolledWindowNew, Rectangle(..),
        EventMask(..), Modifier(..), ContainerClass, mainIteration,
        castToWidget,
#ifdef MIN_VERSION_gtk3
        widgetGetWindow
#else
        widgetGetDrawWindow
#endif
        )
import Data.Maybe (fromJust)
import IDE.Core.State (onIDE, reflectIDE, leksahOrPackageDir)
import Graphics.UI.Editor.Basics (Connection(..))
import System.Log.Logger (debugM)
#endif

data CodeMirror = CodeMirror deriving( Typeable, Show )

#ifdef LEKSAH_WITH_CODE_MIRROR

data CodeMirrorState = CodeMirrorState {
    cmContext        :: JSContextRef
  , cmObject         :: JSObjectRef }
type CM = ReaderT (WebView, CodeMirrorState) JSM
webView :: CM WebView
webView = fst <$> ask
codeMirror :: CM JSObjectRef
codeMirror = cmObject . snd <$> ask
runCM :: CodeMirrorRef -> CM a -> IDEM a
runCM (v, mvar) f = liftIO $ do
    s <- guiTakeMVar mvar
    runReaderT (runReaderT f (v, s)) (cmContext s)
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



body              = js  "body"
value             = js  "value"
setSize           = js2 "setSize"
mode              = js  "mode"
line              = js  "line"
ch                = js  "ch"
left              = js  "left"
top               = js  "top"
right             = js  "right"
bottom            = js  "bottom"
lastLine          = js0 "lastLine"
getRange          = js2 "getRange"
setValue          = js1 "setValue"
setBookmark'      = js2 "setBookmark"
find              = js0 "find"
from              = js  "from"
getCursor :: (MakeValueRef a0, MakeObjectRef o) => a0 -> IndexPreservingGetter o (JSM JSValueRef)
getCursor         = js1 "getCursor"
isClean           = js0 "isClean"
markText          = js3 "markText"
className         = js  "className"
clearHistory      = js0 "clearHistory"
callUndo          = js0 "undo"
undo'             = js  "undo"
callRedo          = js0 "redo"
redo'             = js  "redo"
historySize       = js0 "historySize"
replaceRange      = js3 "replaceRange"
insertAt          = js2 "replaceRange"
replaceSelection  = js1 "replaceSelection"
posFromIndex      = js1 "posFromIndex"
lineCount         = js0 "lineCount"
somethingSelected = js0 "somethingSelected"
setSelection      = js2 "setSelection"
placeCursorAt     = js1 "setSelection"
markClean         = js0 "markClean"
coordsChar        = js2 "coordsChar"
charCoords        = js2 "charCoords"
scrollIntoView    = js2 "scrollIntoView"
getAllMarks       = js0 "getAllMarks"
indexFromPos      = js1 "indexFromPos"
getLineText :: (MakeValueRef a0, MakeObjectRef o) => a0 -> IndexPreservingGetter o (JSM JSValueRef)
getLineText       = js1 "getLine"
jsLength          = js  "length"

cmIter :: CodeMirrorRef -> Int -> Int -> CM (EditorIter CodeMirror)
cmIter cm l c = do
    lift $ do
        i <- obj
        i ^. line <# (fromIntegral l :: Double)
        i ^. ch   <# (fromIntegral c :: Double)
        return $ CMIter cm i

newCMBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer CodeMirror)
newCMBuffer mbFilename contents = do
    ideR <- ask
    liftIO $ do
        debugM "leksah" "newCMBuffer"
        scrolledWindow <- scrolledWindowNew Nothing Nothing
        scrolledWindowSetShadowType scrolledWindow ShadowIn
        cmWebView <- webViewNew
        containerAdd scrolledWindow cmWebView
        dataDir <- liftIO $ leksahOrPackageDir "ghcjs-codemirror" CM.getDataDir
        s <- newEmptyMVar
        cmWebView `on` loadFinished $ \ _ -> do
            debugM "leksah" "newCMBuffer loadFinished"
            cmContext <- webViewGetMainFrame cmWebView >>= webFrameGetGlobalContext
            let runjs f = f `runReaderT` cmContext

            runjs $ do
                document   <- jsg "document"
                codeMirror <- jsg "CodeMirror"
                code <- obj
                code ^. value <# contents
                code ^. mode <# "haskell"
                cmObject <- codeMirror # (document ^. body, code) >>= valToObject

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
            ) Nothing (T.pack $ "file://" ++ dataDir ++ "/codemirror.html")
        debugM "leksah" "newCMBuffer loading"
        return $ CMBuffer (cmWebView, s)

instance TextEditor CodeMirror where
    data EditorBuffer CodeMirror = CMBuffer CodeMirrorRef
    data EditorView CodeMirror = CMView CodeMirrorRef
    data EditorMark CodeMirror = CMMark JSObjectRef | CMCursor JSValueRef
    data EditorIter CodeMirror = CMIter CodeMirrorRef JSObjectRef
    data EditorTagTable CodeMirror = CMTagTable CodeMirrorRef
    data EditorTag CodeMirror = CMTag

    newBuffer = newCMBuffer
    applyTagByName (CMBuffer cm) name (CMIter _ first) (CMIter _ last) = runCM cm $ do
        m <- codeMirror
        lift $ do
            o <- obj
            o ^. className <# name
            m ^. markText first last o
            return ()
    beginNotUndoableAction (CMBuffer cm) = return () -- TODO
    beginUserAction (CMBuffer cm) = return () -- TODO
    canRedo (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ (m ^. historySize ^. redo') >>= valToBool
    canUndo (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ (m ^. historySize ^. undo') >>= valToBool
    copyClipboard (CMBuffer cm) _ = return () -- TODO
    createMark (CMView cm) _refType (CMIter _ i) _tooltip = runCM cm $ do
        m <- codeMirror
        lift $ CMMark <$> do
                o <- obj
                m ^. setBookmark' i o
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
            i ^. line <# l
            i ^. ch   <# m ^. getLineText l ^. jsLength
            return $ CMIter cm i
    getInsertMark (CMBuffer cm) = runCM cm . lift $ CMCursor <$> makeValueRef "head"
    getIterAtLine (CMBuffer cm) line = runCM cm $ cmIter cm line 0
    getIterAtMark (CMBuffer cm) (CMMark mark) = runCM cm $ do
        lift $ CMIter cm <$> (mark ^. find ^. from >>= valToObject)
    getIterAtMark (CMBuffer cm) (CMCursor c) = runCM cm $ do
        m <- codeMirror
        lift $ CMIter cm <$> ((m ^. getCursor c) >>= valToObject)
    getIterAtOffset (CMBuffer cm) offset = runCM cm $ do
        m <- codeMirror
        lift $ CMIter cm <$> ((m ^. posFromIndex (fromIntegral offset :: Double)) >>= valToObject)
    getLineCount (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ round <$> ((m ^. lineCount) >>= valToNumber)
    getModified (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ not <$> ((m ^. isClean) >>= valToBool)
    getSelectionBoundMark (CMBuffer cm) = runCM cm . lift $ CMCursor <$> makeValueRef "anchor"
    getSelectionBounds (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ do
            start <- (m ^. getCursor "start") >>= valToObject
            end   <- (m ^. getCursor "end") >>= valToObject
            return (CMIter cm start, CMIter cm end)
    getInsertIter (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ CMIter cm <$> (m ^. getCursor "head" >>= valToObject)
    getSlice (CMBuffer cm) (CMIter _ first) (CMIter _ last) includeHidenChars = runCM cm $ do
        m <- codeMirror
        lift $ m ^. getRange first last >>= valToText
    getStartIter (CMBuffer cm) = runCM cm $ cmIter cm 0 0
    getTagTable (CMBuffer cm) = return $ CMTagTable cm
    getText (CMBuffer cm) (CMIter _ first) (CMIter _ last) includeHidenChars = runCM cm $ do
        m <- codeMirror
        lift $ m ^. getRange first last >>= valToText
    hasSelection (CMBuffer cm) = runCM cm $ do
        m <- codeMirror
        lift $ (m ^. somethingSelected) >>= valToBool
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
        lift $ m ^. setValue text
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
#ifdef MIN_VERSION_gtk3
        liftIO $ widgetGetWindow v
#else
        liftIO $ Just <$> widgetGetDrawWindow v
#endif
    getIterAtLocation (CMView cm) x y = runCM cm $ do
        m <- codeMirror
        lift $ do
            pos <- obj
            pos ^. left <# (fromIntegral x :: Double)
            pos ^. top  <# (fromIntegral y :: Double)
            CMIter cm <$> (m ^. coordsChar pos "window" >>= valToObject)
    getIterLocation (CMView cm) (CMIter _ i) = runCM cm $ do
        m <- codeMirror
        lift $ do
            rect <- (m ^. charCoords i "window" >>= valToObject)
            l <- rect ^. left   >>= valToNumber
            r <- rect ^. right  >>= valToNumber
            t <- rect ^. top    >>= valToNumber
            b <- rect ^. bottom >>= valToNumber
            return $ Rectangle (round l) (round t) (round $ r - l) (round $ b - t)
    getOverwrite (CMView cm) = return False -- TODO
    getScrolledWindow (CMView (v,_)) = liftIO . fmap (castToScrolledWindow . fromJust) $ widgetGetParent v
    getEditorWidget (CMView (v,_)) = return $ castToWidget v
    grabFocus (CMView cm) = runCM cm $ do
        v <- webView
        liftIO $ widgetGrabFocus v
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
            n <- m ^. indexFromPos i >>= valToNumber
            i2 <- m ^. posFromIndex (n - 1)
            return (CMIter cm i2)
    backwardFindCharC (CMIter cm i) pred mbLimit = return Nothing -- TODO
    backwardWordStartC (CMIter cm i) = return Nothing -- TODO
    backwardToLineStartC (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            i2 ^. line <# i ^. line
            i2 ^. ch <# (0 :: Double)
            return $ CMIter cm i2
    endsWord (CMIter cm i) = return False -- TODO
    forwardCharC (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ do
            n <- m ^. indexFromPos i >>= valToNumber
            i2 <- m ^. posFromIndex (n + 1)
            return (CMIter cm i2)
    forwardCharsC (CMIter cm i) d = runCM cm $ do
        m <- codeMirror
        lift $ do
            n <- m ^. indexFromPos i >>= valToNumber
            i2 <- m ^. posFromIndex (n + fromIntegral d)
            return (CMIter cm i2)
    forwardFindCharC (CMIter cm i) pred mbLimit = return Nothing -- TODO
    forwardSearch (CMIter cm i) str pred mbLimit = return Nothing -- TODO
    forwardToLineEndC (CMIter cm i) =  runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            l <- i ^. line >>= makeValueRef
            i2 ^. line <# l
            i2 ^. ch   <# m ^. getLineText l ^. jsLength
            return $ CMIter cm i2
    forwardWordEndC (CMIter cm i) = return Nothing -- TODO
    getChar (CMIter cm i) = return Nothing -- TODO
    getCharsInLine (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ round <$> (m ^. getLineText (i ^. line) ^. jsLength >>= valToNumber)
    getLine (CMIter cm i) = runCM cm $ do
        lift $ round <$> (i ^. line >>= valToNumber)
    getLineOffset (CMIter cm i) = runCM cm $ do
        lift $ round <$> (i ^. ch >>= valToNumber)
    getOffset (CMIter cm i) = runCM cm $ do
        m <- codeMirror
        lift $ round <$> (m ^. indexFromPos i >>= valToNumber)
    isStart i@(CMIter cm _) = do
        start <- getStartIter (CMBuffer cm)
        iterEqual start i
    isEnd i@(CMIter cm _) = do
        end <- getEndIter (CMBuffer cm)
        iterEqual end i
    iterEqual (CMIter cm i1) (CMIter _ i2) = runCM cm . lift $ do
        l1 <- i1 ^. line >>= valToNumber
        l2 <- i2 ^. line >>= valToNumber
        if l1 /= l2
            then return False
            else do
                c1 <- i1 ^. ch >>= valToNumber
                c2 <- i2 ^. ch >>= valToNumber
                return $ c1 == c2
    startsLine i = (== 0) <$> getLineOffset i
    startsWord (CMIter cm i) = return False -- TODO
    atEnd (CMIter cm _) = getEndIter (CMBuffer cm)
    atLine (CMIter cm _) l = runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            i2 ^. line <# (fromIntegral l :: Double)
            i2 ^. ch <# (0 :: Double)
            return $ CMIter cm i2
    atLineOffset (CMIter cm i) column = runCM cm $ do
        m <- codeMirror
        lift $ do
            i2 <- obj
            i2 ^. line <# i ^. line
            i2 ^. ch <# (fromIntegral column :: Double)
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
            id1 <- v `after` focusInEvent $ lift $ reflectIDE f ideR >> return False
            return [ConnectC id1]
    afterModifiedChanged (CMBuffer cm) f = return [] -- TODO
    afterMoveCursor (CMView cm) f = return [] -- TODO
    afterToggleOverwrite (CMView cm) f = return [] -- TODO
    onButtonPress (CMView (v, _)) f = do
        id1 <- v `onIDE` buttonPressEvent $ f
        return [ConnectC id1]
    onButtonRelease (CMView (v, _)) f = do
        id1 <- v `onIDE` buttonReleaseEvent $ f
        return [ConnectC id1]
    onCompletion (CMView cm) start cancel = return [] -- TODO
    onKeyPress (CMView (v, _)) f = do
        id1 <- v `onIDE` keyPressEvent $ f
        return [ConnectC id1]
    onMotionNotify (CMView (v, _)) f = do
        id1 <- v `onIDE` motionNotifyEvent $ f
        return [ConnectC id1]
    onLeaveNotify (CMView (v, _)) f = do
        id1 <- v `onIDE` leaveNotifyEvent $ f
        return [ConnectC id1]
    onKeyRelease (CMView (v, _)) f = do
        id1 <- v `onIDE` keyReleaseEvent $ f
        return [ConnectC id1]
    onLookupInfo (CMView (v, _)) f = do
        ideR <- ask
        liftIO $ do
            v `widgetAddEvents` [ButtonReleaseMask]
            id1 <- (`reflectIDE` ideR) $ v `onIDE` buttonReleaseEvent $ do
                mod <- lift $ eventModifier
                case mod of
                    [Control] -> f >> return True
                    _             -> return False
            return [ConnectC id1]
    onMotionNotifyEvent (CMView cm) f = return [] -- TODO
    onPopulatePopup (CMView (v, _)) f = do
        ideR <- ask
        liftIO $ do
            id1 <- on v popupMenuSignal $ do
                 menu <- menuNew
                 menuAttachToWidget menu v
                 reflectIDE (f menu) ideR
                 menuPopup menu Nothing
                 return True
            return [ConnectC id1]
#endif

