{-# OPTIONS_GHC -XRecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  Yi.UI.Pango.Control
-- Copyright   :  2007-2009 Jean-Philippe Bernardy, Hamish Mackenzie
-- License     :  GPL
--
-- |
--
-----------------------------------------------------------------------------

module Yi.UI.Pango.Control (
    Buffer(..)
,   View(..)
,   newBuffer
,   newView
,   getBuffer
) where

import Prelude (map)

import Data.IORef
import Data.List (drop, zip)
import qualified Data.Rope as Rope
import Yi
import Yi.UI.Utils
import Yi.Style
import Yi.Syntax
import Graphics.UI.Gtk as Gtk
import Control.Monad.Trans (liftIO)

data Buffer = Buffer
    { fBufRef     :: IORef FBuffer
    }

data View = View
    { viewFBufRef :: IORef FBuffer
    , drawArea    :: DrawingArea
    , layout      :: PangoLayout
    , scrollWin   :: ScrolledWindow
    , config      :: Config
    }

newBuffer ref id r = do
    fBufRef <- newIORef $ Yi.newB ref id r
    return Buffer{..}

newView buffer = do
    let viewFBufRef = fBufRef buffer
        config      = defaultVimConfig
    drawArea <- drawingAreaNew
    context  <- widgetCreatePangoContext drawArea
    layout   <- layoutEmpty context
    layoutSetText layout ""

    let winh = 20
        tos  = 0
        bos  = Point winh

    drawArea `Gtk.on` exposeEvent $ liftIO $ do
        fbuf <- readIORef $ fBufRef buffer
        let (tos, point, text, picture) = runBufferDummyWindow fbuf $ do
                    from     <- getMarkPointB =<< fromMark <$> askMarks
                    rope     <- streamB Forward from
                    p        <- pointB
                    let content = fst $ Rope.splitAtLine winh rope
                    -- allow BOS offset to be just after the last line
                    let addNL = if Rope.countNewLines content == winh
                                  then id
                                  else (++"\n")
                        sty = extractValue $ configTheme (configUI config)
                    -- attributesPictureAndSelB sty (currentRegex e) (mkRegion tos bos)
                    return (from, p, addNL $ Rope.toString content, picture)

        -- add color attributes.
        let strokes = [(start',s,end') | ((start', s), end') <- zip picture (drop 1 (map fst picture) ++ [bos]),
                      s /= emptyAttributes]
            rel p = fromIntegral (p - tos)
            allAttrs = concat $ do
                (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
                return $ [ AttrForeground (rel p1) (rel p2) (mkCol True fg)
                         , AttrBackground (rel p1) (rel p2) (mkCol False bg)
                         , AttrStyle      (rel p1) (rel p2) (if itlc then StyleItalic     else StyleNormal)
                         , AttrUnderline  (rel p1) (rel p2) (if udrl then UnderlineSingle else UnderlineNone)
                         , AttrWeight     (rel p1) (rel p2) (if bd   then WeightBold      else WeightNormal)
                         ]
        putStrLn "Setting Layout Attributes"
        -- layoutSetAttributes layout allAttrs -- Currently crashes
        putStrLn "Done Stting Layout Attributes"
        dw      <- widgetGetDrawWindow drawArea
        gc      <- gcNew dw
        oldText <- layoutGetText layout
        when (text /= oldText) $ layoutSetText layout text
        drawLayout dw gc 0 0 layout
        return True

    scrollWin <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport scrollWin drawArea

    return View {..}

getBuffer view = Buffer {fBufRef = viewFBufRef view}

mkCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Gtk.Color
mkCol True  Default = Color 0 0 0
mkCol False Default = Color maxBound maxBound maxBound
mkCol _ (RGB x y z) = Color (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)

