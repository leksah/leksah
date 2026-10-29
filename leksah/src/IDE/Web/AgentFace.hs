{-# LANGUAGE OverloadedStrings #-}
-- | The face that goes with an agent's name ("IDE.Web.AgentNames"): a small,
-- hand-drawn, front-on animal head for every animal of the pool, as inline SVG
-- (a 32×32 grid, a few flat colours, readable at 16–20px).  An agent the user
-- renamed to something that isn't an animal gets a plain disc with its
-- initial.
--
-- Inline text rather than files under @pics/@: it goes straight into
-- @innerHTML@ (as an agent's description does), needs nothing new from the
-- packaging or the ghcjs demo, and stays one module to look at.
module IDE.Web.AgentFace
  ( agentFaceSvg
  , agentFaceDataUri
  ) where

import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | The face for an agent called @name@ (@Wren@, @wren@ and @Wren-2@ all draw
-- the wren).
agentFaceSvg :: Text -> Text
agentFaceSvg name = svg . fromMaybe (initialFace name) $ lookup (animalOf name) faces

-- | The face as a @data:@ URI, for the places that draw an icon with an
-- @\<img src\>@ (tab icons).  Not under @/pics/@, so the light theme's
-- invert-the-mono-icons rule leaves its colours alone.
agentFaceDataUri :: Text -> Text
agentFaceDataUri name = "data:image/svg+xml," <> T.concatMap esc (agentFaceSvg name)
  where
    esc '"' = "'"
    esc '%' = "%25"
    esc '#' = "%23"
    esc '<' = "%3C"
    esc '>' = "%3E"
    esc ' ' = "%20"
    esc c   = T.singleton c

animalOf :: Text -> Text
animalOf = T.toLower . T.takeWhile (/= '-') . T.strip

svg :: [Text] -> Text
svg body = "<svg class=\"agent-face\" xmlns=\"http://www.w3.org/2000/svg\" \
           \viewBox=\"0 0 32 32\" aria-hidden=\"true\">" <> T.concat body <> "</svg>"

--------------------------------------------------------------------------------
-- a very small drawing vocabulary

n :: Double -> Text
n x | r == fromIntegral (round r :: Int) = T.pack (show (round r :: Int))
    | otherwise                          = T.pack (show r)
  where r = fromIntegral (round (x * 100) :: Int) / 100 :: Double

circ :: Double -> Double -> Double -> Text -> Text
circ x y r f = "<circle cx=\"" <> n x <> "\" cy=\"" <> n y <> "\" r=\"" <> n r
               <> "\" fill=\"" <> f <> "\"/>"

ell :: Double -> Double -> Double -> Double -> Text -> Text
ell x y rx ry f = "<ellipse cx=\"" <> n x <> "\" cy=\"" <> n y <> "\" rx=\"" <> n rx
                  <> "\" ry=\"" <> n ry <> "\" fill=\"" <> f <> "\"/>"

poly :: [(Double, Double)] -> Text -> Text
poly ps f = "<polygon points=\"" <> T.unwords [ n x <> "," <> n y | (x, y) <- ps ]
            <> "\" fill=\"" <> f <> "\"/>"

path :: Text -> Text -> Text
path d f = "<path d=\"" <> d <> "\" fill=\"" <> f <> "\"/>"

-- | An unfilled stroke (whiskers, beaks drawn as a line, stripes).
line :: Text -> Double -> Text -> Text
line d w c = "<path d=\"" <> d <> "\" fill=\"none\" stroke=\"" <> c
             <> "\" stroke-width=\"" <> n w <> "\" stroke-linecap=\"round\"/>"

-- | A pair of eyes at height @y@, @dx@ either side of the middle: a dark dot
-- with a highlight, which is what makes a blob read as a face at 16px.
eyes :: Double -> Double -> Double -> [Text]
eyes y dx r = concat [ [ circ x y r ink, circ (x + r * 0.35) (y - r * 0.35) (r * 0.35) "#fff" ]
                     | x <- [16 - dx, 16 + dx] ]

-- | Both sides of a symmetric shape: drawn at @x@, and again mirrored about
-- the vertical centre line.
mirrored :: Double -> (Double -> Text) -> [Text]
mirrored x draw = [ draw x, draw (32 - x) ]

ink :: Text
ink = "#1d1d22"

-- | A head disc with a faint lighter rim — for the dark animals, which would
-- otherwise vanish into a dark theme's background.
rimmed :: Double -> Double -> Double -> Text -> Text
rimmed x y r f = "<circle cx=\"" <> n x <> "\" cy=\"" <> n y <> "\" r=\"" <> n r
                 <> "\" fill=\"" <> f <> "\" stroke=\"#6b6b7a\" stroke-width=\"1\"/>"

--------------------------------------------------------------------------------
-- the faces

faces :: [(Text, [Text])]
faces =
  [ ("alpaca",
      mirrored 9 (\x -> ell x 8 2.2 5 "#e2c9a0")
      <> [ ell 16 18 9 11 "#f4e6cb"
         , circ 11 9 4 "#fffaf0", circ 16 7 4.5 "#fffaf0", circ 21 9 4 "#fffaf0"
         , ell 16 24 5 4 "#e6cda3"
         , line "M14.5 24.5 Q16 26 17.5 24.5" 1 ink ]
      <> eyes 17 4.2 1.5)
  , ("badger",
      mirrored 7 (\x -> circ x 9 3.2 "#4b4f55")
      <> [ ell 16 17 12 11 "#9aa0a6"
         , ell 10 17 3.4 6 ink, ell 22 17 3.4 6 ink
         , path "M13 4 L19 4 L18 28 L14 28 Z" "#f5f5f5"
         , circ 16 25 2.2 ink ]
      <> eyes 16.5 6 1.3)
  , ("crane",
      [ circ 16 15 10 "#f3f3f3"
      , path "M7 18 Q16 30 25 18 Q16 24 7 18 Z" "#2d2d33"
      , circ 16 7.5 3.6 "#d9363e"
      , poly [(14, 18), (18, 18), (16, 31)] "#d8a03a" ]
      <> eyes 14.5 4.2 1.4)
  , ("dingo",
      mirrored 0 (\x -> poly [(x `towards` 6, 3), (x `towards` 13, 10), (x `towards` 7, 15)] "#d4823a")
      <> mirrored 0 (\x -> poly [(x `towards` 7, 6), (x `towards` 11, 10), (x `towards` 8, 12)] "#f2c48e")
      <> [ ell 16 18 10 10 "#e09447"
         , ell 16 23 5.5 4.5 "#f6dfbf"
         , ell 16 21 2 1.4 ink ]
      <> eyes 16 4.5 1.4)
  , ("egret",
      [ circ 16 16 10 "#fbfbfb"
      , line "M16 6.5 Q12 2 6 3" 1.3 "#c9d2d8"
      , line "M16 6.5 Q13 1 9 0.8" 1 "#c9d2d8"
      , "<circle cx=\"16\" cy=\"16\" r=\"10\" fill=\"none\" stroke=\"#cfd8dc\" stroke-width=\"1\"/>"
      , poly [(13.5, 18), (18.5, 18), (16, 29)] "#f2c230" ]
      <> eyes 15 4 1.4)
  , ("fox",
      mirrored 0 (\x -> poly [(x `towards` 4, 2), (x `towards` 13, 9), (x `towards` 4, 13)] "#e8742a")
      <> mirrored 0 (\x -> poly [(x `towards` 5.5, 5), (x `towards` 10.5, 9.5), (x `towards` 5.5, 11)] "#3b2a20")
      <> [ poly [(3, 10), (29, 10), (16, 29)] "#e8742a"
         , poly [(5, 15), (16, 29), (27, 15), (16, 21)] "#fdf6ee"
         , circ 16 27.5 1.8 ink ]
      <> eyes 15 5 1.4)
  , ("gecko",
      [ ell 16 19 12 9 "#6cc04a"
      , circ 8 12 4.2 "#6cc04a", circ 24 12 4.2 "#6cc04a"
      , circ 8 12 3 "#f2e04a", circ 24 12 3 "#f2e04a"
      , ell 8 12 0.8 2.6 ink, ell 24 12 0.8 2.6 ink
      , circ 12 21 1.2 "#4e9a35", circ 21 23 1 "#4e9a35", circ 17 25 0.8 "#4e9a35"
      , line "M9 21 Q16 26 23 21" 1.2 "#2f6b1f" ])
  , ("heron",
      [ circ 16 15 9.5 "#9db4c8"
      , line "M12 8 Q6 6 2 7" 1.6 ink
      , ell 16 17 6 5.5 "#eef3f7"
      , path "M10 10 Q16 7 22 10 L22 12 Q16 9.5 10 12 Z" ink
      , poly [(14, 19), (18, 19), (16, 31)] "#e2b33c" ]
      <> eyes 14.5 4 1.3)
  , ("ibis",
      [ circ 16 13 9.5 "#e8553f"
      , line "M16 19 Q14.5 27 21 31" 2.6 "#a8321f" ]
      <> eyes 12 4 1.4)
  , ("jay",
      [ poly [(11, 9), (17, 0.5), (21, 9)] "#3c7dd9"
      , circ 16 17 10 "#4a8fe7"
      , ell 16 20 6.5 5.5 "#fdfdfd"
      , line "M9 21 Q16 28 23 21" 1.5 ink
      , poly [(14, 17.5), (18, 17.5), (16, 21)] "#333" ]
      <> eyes 15 4.6 1.4)
  , ("koala",
      mirrored 6 (\x -> circ x 10 6 "#9aa3ab")
      <> mirrored 6 (\x -> circ x 10 3.4 "#eadad2")
      <> [ ell 16 18 10.5 10 "#aab3bb"
         , ell 16 20.5 3 4 "#2b2b30" ]
      <> eyes 16 5 1.3)
  , ("lynx",
      mirrored 0 (\x -> poly [(x `towards` 6, 3), (x `towards` 13, 9), (x `towards` 6, 12)] "#b98f55")
      <> mirrored 0 (\x -> line ("M" <> n (x `towards` 6) <> " 3 L" <> n (x `towards` 5) <> " 0") 1.2 ink)
      <> mirrored 0 (\x -> poly [(x `towards` 5, 17), (x `towards` 2, 24), (x `towards` 9, 22)] "#e9dcc4")
      <> [ ell 16 17.5 10.5 10 "#c9a46b"
         , ell 16 22.5 5 4 "#f3ead9"
         , poly [(14.6, 20), (17.4, 20), (16, 21.6)] "#c96d6d" ]
      <> eyes 15.5 4.5 1.4)
  , ("mole",
      [ rimmed 16 17 11 "#4a4a55"
      , ell 10 27 3 2 "#f2a4b4", ell 22 27 3 2 "#f2a4b4" ]
      <> [ ell (16 + 3.2 * cos a) (22 + 3.2 * sin a) 1.3 1.3 "#f08aa0"
         | k <- [0 .. 7 :: Int], let a = fromIntegral k * pi / 4 ]
      <> [ circ 16 22 2.4 "#f7a9b8"
         , circ 12 15 0.9 ink, circ 20 15 0.9 ink ])
  , ("newt",
      [ ell 16 20 12 8 "#f07a2e"
      , circ 9.5 13 3.6 "#f07a2e", circ 22.5 13 3.6 "#f07a2e"
      , circ 9.5 13 1.9 ink, circ 22.5 13 1.9 ink
      , circ 10.1 12.4 0.6 "#fff", circ 23.1 12.4 0.6 "#fff"
      , circ 12 23 1 "#c4561a", circ 20 22 1.2 "#c4561a"
      , line "M10 21 Q16 25 22 21" 1.2 "#8a3a10" ])
  , ("otter",
      mirrored 7 (\x -> circ x 10 3 "#6e4a2e")
      <> [ ell 16 17 11 10 "#8b5e3c"
         , ell 16 22 6 4.5 "#e6cba8"
         , ell 16 20 2.3 1.5 "#2b1d14"
         , line "M10 22 L5 21" 0.7 "#2b1d14", line "M10 23.5 L5 24" 0.7 "#2b1d14"
         , line "M22 22 L27 21" 0.7 "#2b1d14", line "M22 23.5 L27 24" 0.7 "#2b1d14" ]
      <> eyes 15.5 4.5 1.4)
  , ("puffin",
      [ rimmed 16 16 11 "#1f1f24"
      , ell 16 17 8 7 "#fbfbfb"
      , poly [(11, 19), (21, 19), (16, 29)] "#f26b2a"
      , poly [(12.6, 21), (19.4, 21), (18.5, 22.8), (13.5, 22.8)] "#f5c542" ]
      <> eyes 15 4 1.3)
  , ("quail",
      [ line "M16 8 Q15 2 19 1" 1.6 "#3a2a20"
      , circ 19.4 1.4 1.6 "#3a2a20"
      , circ 16 18 10 "#a07850"
      , path "M7 15 Q16 11 25 15 L25 16.5 Q16 13 7 16.5 Z" "#fdf6ee"
      , ell 16 23 5 4 "#3a2a20"
      , poly [(14.8, 19), (17.2, 19), (16, 21.5)] "#5a3a20" ]
      <> eyes 17.6 4.4 1.3)
  , ("raven",
      [ rimmed 16 15 10.5 "#24242c"
      , line "M9 10 Q13 7 18 8" 1.2 "#4b4b5c"
      , poly [(12, 18), (20, 18), (16, 27)] "#44444f" ]
      <> concat [ [ circ x 14.5 1.9 "#d8d8e0", circ x 14.5 1.2 ink ] | x <- [11.5, 20.5] ])
  , ("seal",
      [ circ 16 16 12 "#8d9aa6"
      , circ 13.6 21 3.2 "#b9c4cd", circ 18.4 21 3.2 "#b9c4cd"
      , ell 16 19 2.2 1.5 ink
      , line "M12 21 L6 20" 0.6 ink, line "M12 22.5 L6 23" 0.6 ink
      , line "M20 21 L26 20" 0.6 ink, line "M20 22.5 L26 23" 0.6 ink ]
      <> eyes 14 4.6 2.2)
  , ("tapir",
      mirrored 7 (\x -> circ x 8 3.2 "#f2f2f2")
      <> mirrored 7 (\x -> circ x 8 2.2 "#3b3b45")
      <> [ rimmed 16 15 10 "#3b3b45"
         , ell 16 25 4 5.5 "#565663"
         , ell 14.5 28.5 0.8 0.6 ink, ell 17.5 28.5 0.8 0.6 ink ]
      <> eyes 14 4.6 1.2)
  , ("urial",
      mirrored 7 (\x -> circ x 12 5.2 "#9a7646")
      <> mirrored 7 (\x -> circ x 12 3.2 "#c7a674")
      <> mirrored 7 (\x -> circ x 12 1.4 "#9a7646")
      <> [ ell 16 18 7 10 "#d9b88a"
         , ell 16 25 4.5 3.5 "#efdcbf"
         , ell 16 24 1.6 1 ink ]
      <> eyes 16.5 3.6 1.2)
  , ("vole",
      mirrored 8 (\x -> circ x 9 4 "#9c7350")
      <> mirrored 8 (\x -> circ x 9 2.4 "#f0b9b0")
      <> [ ell 16 18 11 10 "#b08560"
         , ell 16 23 4.5 3.5 "#d9b896"
         , circ 16 21.5 1.6 "#e88a96"
         , line "M12 22 L6 21" 0.6 ink, line "M20 22 L26 21" 0.6 ink ]
      <> eyes 16 4.4 1.5)
  , ("wren",
      [ poly [(22, 9), (29, 1), (30, 4), (25, 11)] "#8a5a34"
      , circ 16 17 11 "#a9744a"
      , path "M8 14 Q12 11.5 15 13.5 L14.5 14.5 Q12 13 8.5 15.5 Z" "#f1dcbc"
      , path "M24 14 Q20 11.5 17 13.5 L17.5 14.5 Q20 13 23.5 15.5 Z" "#f1dcbc"
      , ell 16 23 5 4 "#d9b38a"
      , poly [(14.6, 18.5), (17.4, 18.5), (16, 22)] "#5a3a20" ]
      <> eyes 16 4.3 1.3)
  , ("xerus",
      mirrored 8.5 (\x -> circ x 8 2.6 "#b8874a")
      <> [ ell 16 17.5 10.5 10.5 "#c99a5b"
         , path "M15 7 L17 7 L16.6 18 L15.4 18 Z" "#f3e3c8"
         , circ 11 15 2.8 "#fdf6ea", circ 21 15 2.8 "#fdf6ea"
         , ell 16 23 4.5 3.5 "#ecd6b2"
         , ell 16 21.5 1.5 1 ink ]
      <> eyes 15 5 1.4)
  , ("yak",
      [ line "M9 12 Q3 10 4 3" 2.6 "#ece6da"
      , line "M23 12 Q29 10 28 3" 2.6 "#ece6da"
      , ell 16 18 9 11 "#4a3a30"
      , path "M7 13 Q10 6 16 7 Q22 6 25 13 Q20 10 16 12 Q12 10 7 13 Z" "#2f241d"
      , ell 16 25 5.5 4 "#6a574a"
      , ell 14 25 0.9 0.7 ink, ell 18 25 0.9 0.7 ink ]
      <> eyes 17 4 1.3)
  , ("zebra",
      mirrored 10 (\x -> ell x 5 2 4.5 "#f4f4f4")
      <> [ path "M14 2 L18 2 L18 8 L14 8 Z" ink
         , ell 16 16 9 12 "#f7f7f7"
         , line "M8 11 Q12 12 13 9" 1.6 ink, line "M24 11 Q20 12 19 9" 1.6 ink
         , line "M7.5 16 Q11 17 12 14" 1.6 ink, line "M24.5 16 Q21 17 20 14" 1.6 ink
         , line "M14 6 Q16 8 18 6" 1.4 ink
         , ell 16 24.5 6 4.5 "#333"
         , ell 14 24.5 0.9 0.7 "#888", ell 18 24.5 0.9 0.7 "#888" ]
      <> eyes 14.5 4.6 1.3)
  , ("bear",
      mirrored 8 (\x -> circ x 8.5 4 "#8b5a2b")
      <> mirrored 8 (\x -> circ x 8.5 2.2 "#c08a5a")
      <> [ circ 16 17 11 "#9a6332"
         , ell 16 22 5 4 "#d9b38a"
         , ell 16 20 2.2 1.5 ink ]
      <> eyes 15.5 4.5 1.4)
  , ("beaver",
      mirrored 7 (\x -> circ x 9.5 2.6 "#6e4528")
      <> [ ell 16 17 11 10.5 "#8a5a36"
         , ell 16 21.5 6 4.5 "#c9a07a"
         , ell 16 19 2.4 1.6 ink
         , path "M14.4 23.5 L17.6 23.5 L17.6 27.5 L14.4 27.5 Z" "#fdfdf5"
         , line "M16 23.8 L16 27.2" 0.5 "#c9b9a0" ]
      <> eyes 15 4.6 1.4)
  , ("cat",
      mirrored 0 (\x -> poly [(x `towards` 5, 3), (x `towards` 13, 9), (x `towards` 5, 13)] "#8c8c94")
      <> mirrored 0 (\x -> poly [(x `towards` 6.5, 6), (x `towards` 11, 9.5), (x `towards` 6.5, 11)] "#f2b8c6")
      <> [ ell 16 18 11 10 "#9b9ba3"
         , ell 16 22.5 4.5 3.2 "#f2f2f2"
         , poly [(14.8, 20.2), (17.2, 20.2), (16, 21.6)] "#e88a96"
         , line "M11 22 L5 21" 0.6 ink, line "M11 23.5 L5 24" 0.6 ink
         , line "M21 22 L27 21" 0.6 ink, line "M21 23.5 L27 24" 0.6 ink ]
      <> concat [ [ ell x 16 1.8 2.5 "#9bc53d", ell x 16 0.6 2 ink ] | x <- [11.5, 20.5] ])
  , ("cow",
      [ line "M9 7 Q6 5 6 2" 1.8 "#e8dcc0", line "M23 7 Q26 5 26 2" 1.8 "#e8dcc0" ]
      <> mirrored 5 (\x -> ell x 12 3.5 2 "#e9e1d8")
      <> [ ell 16 15 9 11 "#f5f2ec"
         , ell 11.5 12 4 5 ink
         , ell 16 24 7.5 5 "#f2b8c6"
         , ell 13.5 24 1 1.3 "#8a4a5a", ell 18.5 24 1 1.3 "#8a4a5a" ]
      <> eyes 13.5 4.2 1.4)
  , ("deer",
      [ line "M11 8 Q8 4 9 1" 1.2 "#8a6a40", line "M9.4 4.5 L6 3.5" 1.2 "#8a6a40"
      , line "M21 8 Q24 4 23 1" 1.2 "#8a6a40", line "M22.6 4.5 L26 3.5" 1.2 "#8a6a40" ]
      <> mirrored 6 (\x -> ell x 12 3.5 2 "#b07a45")
      <> [ ell 16 17 8.5 11 "#c08a50"
         , ell 16 24.5 4 4 "#e9d2b0"
         , ell 16 23.5 1.8 1.3 ink ]
      <> eyes 15 4 1.5)
  , ("duck",
      [ circ 16 15 10.5 "#2f8a4a"
      , path "M7 22 Q16 27 25 22 L25 24 Q16 29 7 24 Z" "#f5f5f5"
      , ell 16 20 6.5 3 "#f2c230" ]
      <> eyes 13.5 4.2 1.4)
  , ("elephant",
      mirrored 5.5 (\x -> ell x 15 5.5 8 "#97a2ac")
      <> [ ell 16 14 9 10 "#aab4bd"
         , line "M16 19 Q16 27 20 30" 4.2 "#aab4bd"
         , line "M12.5 20 L11 23" 1.2 "#f5f1e6", line "M19.5 20 L21 23" 1.2 "#f5f1e6" ]
      <> eyes 13 4.2 1.3)
  , ("frog",
      [ ell 16 20 12 8.5 "#5bb04a"
      , circ 9.5 12 4.2 "#5bb04a", circ 22.5 12 4.2 "#5bb04a"
      , circ 9.5 12 2.9 "#fff", circ 22.5 12 2.9 "#fff"
      , circ 9.8 12.3 1.5 ink, circ 22.8 12.3 1.5 ink
      , circ 8 21 1.6 "#f2a0a8", circ 24 21 1.6 "#f2a0a8"
      , line "M10 22 Q16 26.5 22 22" 1.2 "#2f6b1f" ])
  , ("giraffe",
      [ line "M12 8 L11 3" 1.4 "#b8742a", line "M20 8 L21 3" 1.4 "#b8742a"
      , circ 11 3 1.5 "#8a5420", circ 21 3 1.5 "#8a5420" ]
      <> mirrored 6.5 (\x -> ell x 10.5 3 1.6 "#e8b44a")
      <> [ ell 16 16 8 11 "#f2c14e"
         , circ 11.5 11 1.6 "#c8822e", circ 20.5 10 1.3 "#c8822e", circ 19.5 18 1.5 "#c8822e"
         , ell 16 24.5 5.5 4 "#e7b56a"
         , ell 14 24 0.8 0.6 ink, ell 18 24 0.8 0.6 ink ]
      <> eyes 15 4 1.4)
  , ("hedgehog",
      [ path "M3 20 L5 13 L4 9 L8 9 L9 4 L12 7 L16 2 L18 7 L23 4 L23 9 L28 9 L27 14 L29 20 Z" "#6b4a33"
      , ell 16 20 9 8 "#f0d9b5"
      , circ 16 24.5 1.7 ink ]
      <> eyes 18.5 3.6 1.3)
  , ("hippo",
      mirrored 8 (\x -> circ x 7.5 2.3 "#8a7aa0")
      <> [ ell 16 15 10 9 "#9d8cb3"
         , ell 16 23 10 6.5 "#b3a3c7"
         , ell 12.5 21.5 1.2 0.9 ink, ell 19.5 21.5 1.2 0.9 ink ]
      <> eyes 12.5 4.5 1.4)
  , ("kiwi",
      [ circ 16 15 10.5 "#8a6a48"
      , line "M16 19 Q15.5 26 15 31" 1.6 "#e8d6a8" ]
      <> eyes 14 4 1.2)
  , ("lion",
      [ circ 16 16 14.5 "#c8742a"
      , circ 9.5 9.5 2.6 "#f2c25e", circ 22.5 9.5 2.6 "#f2c25e"
      , circ 16 17 9.5 "#f2c25e"
      , ell 16 21.5 4.5 3.5 "#fbe7bf"
      , poly [(14.6, 19.6), (17.4, 19.6), (16, 21.2)] "#6b3a1e" ]
      <> eyes 15.5 3.8 1.4)
  , ("monkey",
      mirrored 4.5 (\x -> circ x 16 3.6 "#8a5a36")
      <> mirrored 4.5 (\x -> circ x 16 2 "#e8c9a0")
      <> [ circ 16 16 11 "#8a5a36"
         , ell 13 15 4 4.5 "#e8c9a0", ell 19 15 4 4.5 "#e8c9a0"
         , ell 16 21 6.5 5 "#e8c9a0"
         , ell 14.8 20.5 0.6 0.8 ink, ell 17.2 20.5 0.6 0.8 ink
         , line "M13.5 23.5 Q16 25 18.5 23.5" 0.9 "#6b4022" ]
      <> eyes 15 3 1.3)
  , ("owl",
      [ poly [(5, 8), (10, 6), (7, 12)] "#6e5238", poly [(27, 8), (22, 6), (25, 12)] "#6e5238"
      , ell 16 17 12 12 "#8a6a48"
      , circ 11.5 15 5 "#e9d9b8", circ 20.5 15 5 "#e9d9b8"
      , circ 11.5 15 3 "#f2c230", circ 20.5 15 3 "#f2c230"
      , circ 11.5 15 1.6 ink, circ 20.5 15 1.6 ink
      , poly [(14.8, 18.5), (17.2, 18.5), (16, 22)] "#e08a2a" ])
  , ("panda",
      mirrored 7 (\x -> circ x 8 3.6 ink)
      <> [ circ 16 17 11 "#fafafa"
         , ell 11.5 16 3.2 4 ink, ell 20.5 16 3.2 4 ink
         , circ 11.8 15.6 1.2 "#fff", circ 20.2 15.6 1.2 "#fff"
         , ell 16 21.5 2 1.4 ink
         , line "M14.5 23.5 Q16 24.8 17.5 23.5" 0.8 ink ])
  , ("penguin",
      [ rimmed 16 16 11 "#23252b"
      , ell 12.5 18 4.5 6.5 "#fbfbfb", ell 19.5 18 4.5 6.5 "#fbfbfb"
      , poly [(13.8, 19), (18.2, 19), (16, 22.5)] "#f2a23c" ]
      <> eyes 15.5 3.6 1.2)
  , ("pig",
      mirrored 0 (\x -> poly [(x `towards` 6, 5), (x `towards` 12, 8), (x `towards` 7, 12)] "#ee8aa3")
      <> [ circ 16 17 11 "#f4a6b8"
         , ell 16 21 5 3.6 "#ee8aa3"
         , ell 14.3 21 1 1.4 "#a8435c", ell 17.7 21 1 1.4 "#a8435c" ]
      <> eyes 14.5 4.2 1.3)
  , ("rabbit",
      mirrored 11.5 (\x -> ell x 7 2.8 7 "#e9e4ec")
      <> mirrored 11.5 (\x -> ell x 7 1.3 5 "#f2b8c6")
      <> [ circ 16 19.5 10 "#ece7ef"
         , poly [(14.8, 21.5), (17.2, 21.5), (16, 22.8)] "#ee8aa3"
         , path "M15 23.6 L17 23.6 L17 25.6 L15 25.6 Z" "#fff"
         , line "M12 23 L6 22" 0.6 "#8a8a96", line "M20 23 L26 22" 0.6 "#8a8a96" ]
      <> eyes 18.5 4 1.4)
  , ("raccoon",
      mirrored 0 (\x -> poly [(x `towards` 5, 4), (x `towards` 12, 8), (x `towards` 6, 12)] "#6f757c")
      <> [ ell 16 17 12 10 "#9aa0a6"
         , path "M5 15 Q16 10 27 15 Q22 20 16 17.5 Q10 20 5 15 Z" "#2a2c31"
         , ell 16 22.5 5 4 "#f0f0f0"
         , circ 16 20.5 1.5 ink ]
      <> eyes 15.5 5 1.4)
  , ("sheep",
      [ circ 9 10 5 "#f4efe6", circ 16 7.5 5.5 "#f4efe6", circ 23 10 5 "#f4efe6"
      , circ 7.5 17 4.5 "#f4efe6", circ 24.5 17 4.5 "#f4efe6"
      , ell 16 19 6.5 8.5 "#3a3236" ]
      <> mirrored 8.5 (\x -> ell x 15 3 1.6 "#3a3236")
      <> concat [ [ circ x 17 1.6 "#f4efe6", circ x 17 0.9 ink ] | x <- [13.5, 18.5] ]
      <> [ ell 16 24 1.6 1 "#8a7a80" ])
  , ("tiger",
      mirrored 7 (\x -> circ x 8.5 3.4 "#e8822e")
      <> mirrored 7 (\x -> circ x 8.5 1.8 "#fbefe0")
      <> [ ell 16 17 12 11 "#e8822e"
         , line "M16 7 L16 11" 1.4 ink, line "M13 7.5 L14 10.5" 1.2 ink, line "M19 7.5 L18 10.5" 1.2 ink
         , line "M4.5 15 L9 16" 1.3 ink, line "M5 19 L9 18.5" 1.3 ink
         , line "M27.5 15 L23 16" 1.3 ink, line "M27 19 L23 18.5" 1.3 ink
         , ell 16 22.5 5.5 4 "#fbefe0"
         , poly [(14.6, 20), (17.4, 20), (16, 21.6)] "#d26a7a" ]
      <> eyes 15.5 4.5 1.4)
  , ("turtle",
      [ ell 16 28 13.5 6 "#4f7f3a"
      , line "M9 26 L12 24 L16 25 L20 24 L23 26" 0.9 "#3a6229"
      , ell 16 14 8 9.5 "#7cb85a"
      , line "M13 18.5 Q16 20.5 19 18.5" 1 "#2f5a1f" ]
      <> eyes 13 3.6 1.5)
  , ("wolf",
      mirrored 0 (\x -> poly [(x `towards` 5, 2), (x `towards` 13, 9), (x `towards` 5, 13)] "#5f6670")
      <> [ poly [(4, 12), (16, 6), (28, 12), (25, 22), (16, 30), (7, 22)] "#7d8590"
         , poly [(10, 19), (16, 17), (22, 19), (16, 29)] "#e8eaec"
         , ell 16 26.5 2 1.4 ink ]
      <> concat [ [ circ x 14.5 1.5 "#f2c94c", circ x 14.5 0.8 ink ] | x <- [11.5, 20.5] ])
  ]
  where
    -- A left-hand x coordinate, on the side @mirrored 0@ is drawing (0 = left,
    -- 32 = right).
    towards side x = if side == 0 then x else 32 - x

-- | A renamed agent: a disc in a colour of its own, with its initial.
initialFace :: Text -> [Text]
initialFace name =
  [ circ 16 16 14 (hashColour name)
  , "<text x=\"16\" y=\"21.5\" text-anchor=\"middle\" font-size=\"15\" \
    \font-family=\"-apple-system, sans-serif\" font-weight=\"600\" fill=\"#fff\">"
    <> T.take 1 (T.toUpper (T.strip name)) <> "</text>" ]

hashColour :: Text -> Text
hashColour t = palette !! (T.foldl' (\h c -> (h * 31 + ord c) `mod` 1000003) 7 t
                           `mod` length palette)
  where palette = ["#e0533d", "#d9a441", "#5aa15a", "#3b9ec7", "#8a6fd1", "#c76ba6"]
