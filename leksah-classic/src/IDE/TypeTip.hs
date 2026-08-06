{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, LambdaCase, PatternSynonyms #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.TypeTip
-- Copyright   :  2017 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  <maintainer@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.TypeTip (setTypeTip, updateTypeTipStyle) where

import Prelude ()
import Prelude.Compat
import Data.Text (Text)
import qualified Data.Text as T
       (concat, drop, lines, length, splitOn, null)
import IDE.Core.State
       (MonadIDE, Prefs(..), IDEAction, IDEM, ideGtk, prefs,
        modifyIDE_, readIDE)
import IDE.Gtk.State
       (typeTip, TypeTip(..), typeTip, postAsyncIDE')
import Control.Lens ((?~), pre, _Just)
import Graphics.UI.Frame.ViewFrame (getWindows)
import GI.Gtk
       (windowMove, windowResize, widgetHide, containerAdd, widgetShowAll,
        containerSetBorderWidth, setWindowTransientFor,
        setWindowDefaultHeight, setWindowDefaultWidth, setWindowResizable,
        setWindowDecorated, setWindowTypeHint, windowNew, windowGetPosition)
import GI.Gtk.Enums (WindowType(..))
import GI.Gdk.Enums (WindowTypeHint(..))
import IDE.TextEditor (updateStyle, newDefaultBuffer)
import IDE.TextEditor.Class (TextEditor(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int32)
import Data.List (intersperse)
import GI.GLib.Constants (pattern PRIORITY_DEFAULT_IDLE)

withTypeTip :: MonadIDE m => (Maybe (TypeTip IDEM) -> m ()) -> m ()
withTypeTip f = readIDE (pre $ ideGtk . _Just . typeTip) >>= mapM_ f

setTypeTip :: (Int32, Int32) -> Text -> IDEM ()
setTypeTip (x', y) t = do
    let (indent, s) = case T.splitOn " :: " t of
                        (x:_:_) | not (T.null x) -> (T.length (last $ T.lines x) + 1, T.drop (T.length x + 1) t)
                        _ -> (0, t)
        trimmed = case T.lines s of
                    (first:rest) -> T.concat (intersperse "\n" (first : map (T.drop indent) rest))
                    [] -> ""
    withTypeTip $ \case
        Just TypeTip {..} -> ttSetText x' y trimmed
        Nothing           -> initTypeTip x' y trimmed

updateTypeTipStyle :: IDEAction
updateTypeTipStyle =
    withTypeTip $ \case
        Just TypeTip {..} -> ttUpdateStyle
        Nothing           -> return ()

initTypeTip :: Int32 -> Int32 -> Text -> IDEAction
initTypeTip initX initY tip = do
    windows    <- getWindows
    prefs'     <- readIDE prefs
    window     <- windowNew WindowTypePopup
    setWindowTypeHint      window WindowTypeHintUtility
    setWindowDecorated     window False
    setWindowResizable     window False
    setWindowDefaultWidth  window 10
    setWindowDefaultHeight window 10
    setWindowTransientFor  window $ head windows
    containerSetBorderWidth window 1
    windowMove window (initX+2) initY

    buffer <- newDefaultBuffer Nothing tip
    (view, viewWidget) <- newViewNoScroll buffer (textviewFont prefs')
    postAsyncIDE' PRIORITY_DEFAULT_IDLE $ updateStyle buffer
    setEditable view False
    setShowLineMarks view False
    setHighlightCurrentLine view False
    containerAdd window viewWidget

    let updateTypeTip x y t =
            if T.null t
                then liftIO $ widgetHide window
                else do
                    let (x', y') = (x+2, y-1)
                    s <- getStartIter buffer
                    e <- getEndIter buffer
                    curText <- getText buffer s e True
                    curPos <- windowGetPosition window
                    if t /= curText || (x', y') /= curPos
                        then do
                            widgetHide window
                            setText buffer t
                            windowMove window x' y'
                            postAsyncIDE' PRIORITY_DEFAULT_IDLE $ do
                                windowResize window 10 10
                                liftIO $ widgetShowAll window
                        else liftIO $ widgetShowAll window
        updateTypeTipStyle' = updateStyle buffer

    modifyIDE_ $ ideGtk . _Just . typeTip ?~ TypeTip window updateTypeTip updateTypeTipStyle'

    widgetShowAll window
