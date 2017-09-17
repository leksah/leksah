{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, LambdaCase #-}
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

module IDE.TypeTip (setTypeTip) where

import Data.Text (Text)
import qualified Data.Text as T (null)
import IDE.Core.Types
       (Prefs(..), IDE(..), IDEAction, IDEM, IDE(typeTip), TypeTip(..))
import IDE.Core.State (modifyIDE_, readIDE)
import Control.Monad.Trans.Reader (ask)
import Graphics.UI.Frame.ViewFrame (getWindows)
import GI.Gtk
       (windowResize, widgetHide, containerAdd, widgetShowAll,
        containerSetBorderWidth, setWindowTransientFor,
        setWindowDefaultHeight, setWindowDefaultWidth, setWindowResizable,
        setWindowDecorated, setWindowTypeHint, windowNew)
import GI.Gtk.Enums (WindowType(..))
import GI.Gdk.Enums (WindowTypeHint(..))
import IDE.TextEditor (updateStyle, newDefaultBuffer)
import IDE.TextEditor.Class (TextEditor(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef)

setTypeTip :: Text -> IDEM ()
setTypeTip t =
    readIDE typeTip >>= \case
        Just TypeTip {..} -> ttSetText t
        Nothing           -> initTypeTip t

initTypeTip :: Text -> IDEAction
initTypeTip tip = do
    ideR <- ask
    windows    <- getWindows
    prefs      <- readIDE prefs
    window     <- windowNew WindowTypePopup
    setWindowTypeHint      window WindowTypeHintUtility
    setWindowDecorated     window False
    setWindowResizable     window True
    setWindowDefaultWidth  window 10
    setWindowDefaultHeight window 10
    setWindowTransientFor  window $ head windows
    containerSetBorderWidth window 1

    buffer <- newDefaultBuffer Nothing tip
    (view, viewWidget) <- newViewNoScroll buffer (textviewFont prefs)
    updateStyle buffer
    setEditable view False
    setShowLineMarks view False
    setHighlightCurrentLine view False
    containerAdd window viewWidget

    let updateTypeTip t =
            if T.null t
                then liftIO $ widgetHide window
                else do
                    setText buffer t
                    windowResize window 10 10
                    liftIO $ widgetShowAll window

    modifyIDE_ $ \ide -> ide{typeTip = Just (TypeTip window updateTypeTip)}

    widgetShowAll window
