{-# LANGUAGE OverloadedStrings #-}
-- | Parse and print tmux's @#{window_layout}@ strings — the
-- checksum-prefixed cell tree tmux uses to describe a window's pane
-- arrangement, e.g. @b25d,208x59,0,0{104x59,0,0,4,103x59,105,0,7}@.
--
-- Used by the minimal-ancestor-path conversion (see IDE.Web.Main): when a
-- native view must split a specific tmux PANE of a multi-pane window, the
-- panes not on the path from the window root to that pane are re-hosted into
-- fresh windows — multi-pane subtrees by @join-pane@ing their panes together
-- and applying the subtree's geometry with @select-layout@, which accepts
-- exactly this format (checksum included).
module IDE.Web.TmuxLayout
  ( TmuxCell(..)
  , parseWindowLayout
  , printWindowLayout
  , cellPanes
  , cellSize
  , rerootCell
  , fitCell
  , combineCells
  ) where

import Data.Bits (shiftR, shiftL, (.&.))
import Data.Char (isDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

-- | One cell of the layout tree.  @TCRow@ is tmux's @{…}@ (children side by
-- side), @TCCol@ its @[…]@ (children stacked).  Geometry is in character
-- cells; child extents exclude the 1-cell gutters between them.
data TmuxCell
  = TCPane { tcW, tcH, tcX, tcY :: Int, tcId :: Text }  -- ^ pane id @\"%5\"@
  | TCRow  { tcW, tcH, tcX, tcY :: Int, tcKids :: [TmuxCell] }
  | TCCol  { tcW, tcH, tcX, tcY :: Int, tcKids :: [TmuxCell] }
  deriving (Eq, Show)

-- | The cell's panes in traversal order (tmux applies a layout to a
-- window's panes in this order).
cellPanes :: TmuxCell -> [Text]
cellPanes TCPane { tcId = p } = [p]
cellPanes TCRow  { tcKids = ks } = concatMap cellPanes ks
cellPanes TCCol  { tcKids = ks } = concatMap cellPanes ks

-- | (width, height) of a cell.
cellSize :: TmuxCell -> (Int, Int)
cellSize c = (tcW c, tcH c)

-- | Shift a cell (and its children) so its origin is (0,0) — what a subtree
-- needs before it becomes a whole window's layout.
rerootCell :: TmuxCell -> TmuxCell
rerootCell c0 = shift (tcX c0) (tcY c0) c0
  where
    shift dx dy c = case c of
      TCPane {} -> c { tcX = tcX c - dx, tcY = tcY c - dy }
      TCRow  {} -> c { tcX = tcX c - dx, tcY = tcY c - dy
                     , tcKids = map (shift dx dy) (tcKids c) }
      TCCol  {} -> c { tcX = tcX c - dx, tcY = tcY c - dy
                     , tcKids = map (shift dx dy) (tcKids c) }

-- | Parse a full @#{window_layout}@ value (leading @xxxx,@ checksum is
-- accepted and ignored).  'Nothing' on any syntax error.
parseWindowLayout :: Text -> Maybe TmuxCell
parseWindowLayout t0 = do
    -- strip "csum," (4 hex chars + comma) if present
    let t = case T.splitAt 4 t0 of
              (h, rest) | T.isPrefixOf "," rest
                        , T.all isHexDigit h -> T.drop 1 rest
              _ -> t0
    (c, rest) <- cell t
    if T.null rest then Just c else Nothing
  where
    isHexDigit ch = isDigit ch || ch `elem` ("abcdefABCDEF" :: String)
    number s = let (ds, rest) = T.span isDigit s
               in if T.null ds then Nothing
                  else Just (read (T.unpack ds) :: Int, rest)
    expect ch s = if T.isPrefixOf (T.singleton ch) s
                    then Just (T.drop 1 s) else Nothing
    cell s0 = do
      (w, s1) <- number s0
      s2      <- expect 'x' s1
      (h, s3) <- number s2
      s4      <- expect ',' s3
      (x, s5) <- number s4
      s6      <- expect ',' s5
      (y, s7) <- number s6
      case T.uncons s7 of
        Just (',', s8) -> do          -- a pane: ,<id>
          (n, s9) <- number s8
          Just (TCPane w h x y ("%" <> T.pack (show n)), s9)
        Just ('{', s8) -> do
          (ks, s9) <- kids '}' s8
          Just (TCRow w h x y ks, s9)
        Just ('[', s8) -> do
          (ks, s9) <- kids ']' s8
          Just (TCCol w h x y ks, s9)
        _ -> Nothing
    kids close s0 = do
      (k, s1) <- cell s0
      case T.uncons s1 of
        Just (',', s2) -> do
          (ks, s3) <- kids close s2
          Just (k : ks, s3)
        Just (c, s2) | c == close -> Just ([k], s2)
        _ -> Nothing

-- | Re-fit a cell tree to exactly the given rectangle: children resized
-- proportionally to their current extents (each at least 1 cell, the 1-cell
-- gutters between siblings preserved, exact total) and every cell's
-- absolute x/y updated.
fitCell :: Int -> Int -> Int -> Int -> TmuxCell -> TmuxCell
fitCell x y w h c = case c of
    TCPane {} -> c { tcW = w, tcH = h, tcX = x, tcY = y }
    TCRow _ _ _ _ ks ->
        let ws = apportion (max (length ks) (w - (length ks - 1)))
                           (map (fromIntegral . tcW) ks)
            xs = scanl (\a wi -> a + wi + 1) x ws
        in TCRow w h x y [ fitCell xi y wi h k | (k, wi, xi) <- zip3 ks ws xs ]
    TCCol _ _ _ _ ks ->
        let hs = apportion (max (length ks) (h - (length ks - 1)))
                           (map (fromIntegral . tcH) ks)
            ys = scanl (\a hi -> a + hi + 1) y hs
        in TCCol w h x y [ fitCell x yi w hi k | (k, hi, yi) <- zip3 ks hs ys ]

-- | Split @total@ cells into parts proportional to @weights@: each part at
-- least 1 and the parts summing to exactly @total@ (cumulative rounding,
-- reserving a cell for every remaining part so the tail can't be starved).
apportion :: Int -> [Double] -> [Int]
apportion total weights = go 0 0 (length weights) weights
  where
    s = max 1e-9 (sum (map (max 0) weights))
    go _ _ _ [] = []
    go accW accC k (wt : rest) =
        let accW'  = accW + max 0 wt
            target = round (fromIntegral total * accW' / s) :: Int
            c      = max 1 (min (total - accC - (k - 1)) (target - accC))
        in c : go accW' (accC + c) (k - 1) rest

-- | Combine sibling cell trees along one axis (@True@ = left-to-right) into
-- one cell, sized from the children (sum along the axis plus 1-cell
-- gutters, maximum across it), each child re-fitted to its SHARE of the
-- axis.  Same-direction children are flattened into the new cell — their
-- kids keep absolute coordinates, so geometry is unchanged and tmux never
-- sees a nested same-direction cell.
combineCells :: Bool -> [(Double, TmuxCell)] -> TmuxCell
combineCells _ [] = TCRow 1 1 0 0 []
combineCells _ [(_, c)] = fitCell 0 0 (tcW c) (tcH c) c
combineCells row kids
  | row =
      let h  = maximum (map (tcH . snd) kids)
          w  = sum (map (tcW . snd) kids) + (length kids - 1)
          ws = apportion (max (length kids) (w - (length kids - 1)))
                         (map fst kids)
          xs = scanl (\a wi -> a + wi + 1) 0 ws
      in TCRow w h 0 0 (concatMap flat
           [ fitCell xi 0 wi h k | ((_, k), wi, xi) <- zip3 kids ws xs ])
  | otherwise =
      let w  = maximum (map (tcW . snd) kids)
          h  = sum (map (tcH . snd) kids) + (length kids - 1)
          hs = apportion (max (length kids) (h - (length kids - 1)))
                         (map fst kids)
          ys = scanl (\a hi -> a + hi + 1) 0 hs
      in TCCol w h 0 0 (concatMap flat
           [ fitCell 0 yi w hi k | ((_, k), hi, yi) <- zip3 kids hs ys ])
  where
    flat k | row, TCRow { tcKids = ks } <- k = ks
           | not row, TCCol { tcKids = ks } <- k = ks
           | otherwise = [k]

-- | Render a cell as a full layout string, checksum prefix included —
-- directly usable as @select-layout '<result>'@.
printWindowLayout :: TmuxCell -> Text
printWindowLayout c =
    let body = render c
    in checksum body <> "," <> body
  where
    render (TCPane w h x y p) =
        dims w h x y <> "," <> T.drop 1 p
    render (TCRow w h x y ks) =
        dims w h x y <> "{" <> T.intercalate "," (map render ks) <> "}"
    render (TCCol w h x y ks) =
        dims w h x y <> "[" <> T.intercalate "," (map render ks) <> "]"
    dims w h x y = T.pack (show w) <> "x" <> T.pack (show h)
                <> "," <> T.pack (show x) <> "," <> T.pack (show y)
    -- tmux's layout_checksum: csum = (csum >> 1) + ((csum & 1) << 15) + ch
    checksum body =
        let go acc ch = ((acc `shiftR` 1) + ((acc .&. 1) `shiftL` 15) + ord ch)
                          .&. 0xffff
            v = foldl go (0 :: Int) (T.unpack body)
            hx = showHex v ""
        in T.pack (replicate (4 - length hx) '0' <> hx)
