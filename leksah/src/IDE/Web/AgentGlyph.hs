{-# LANGUAGE OverloadedStrings #-}
-- | Which CLI an agent runs in, as a small badge: Claude Code's robot, Codex's
-- cloud, Antigravity's arch.  On agent cards ("IDE.Web.Widget.AgentCard") and
-- beside the menu items that start or resume one ("IDE.Web.Widget.AgentMenu").
-- Self-contained HTML (inline SVG, or an image), sized by its container.
module IDE.Web.AgentGlyph
  ( agentGlyph
  , cliGlyph
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import IDE.Web.AgentCli (AgentCli, cliTag)

-- | 'agentGlyph' of a CLI.
cliGlyph :: AgentCli -> Text
cliGlyph = agentGlyph . cliTag

-- | The badge on the left of the face: which CLI the agent runs in — Codex's
-- logo (its rounded cloud with a @>_@ prompt cut out), Antigravity's arch, or
-- Claude Code's robot.
-- Drawn, not typed: a font glyph sits on its baseline, so it never lines up
-- with the state glyph opposite; these are centred in their box by
-- construction.  The Codex mark is its mid-blue rather than its gradient: the
-- same SVG on every card would repeat the gradient's id, and a gradient
-- referenced from a hidden card need not paint.
agentGlyph :: Text -> Text
agentGlyph "codex" =
  "<svg class=\"agent-glyph-codex\" viewBox=\"0 0 24 24\" fill=\"#7a9dff\" fill-rule=\"evenodd\">\
  \<title>Codex agent</title><path clip-rule=\"evenodd\" d=\"\
  \M8.086.457a6.105 6.105 0 013.046-.415c1.333.153 2.521.72 3.564 1.7\
  \a.117.117 0 00.107.029c1.408-.346 2.762-.224 4.061.366l.063.03.154\
  \.076c1.357.703 2.33 1.77 2.918 3.198.278.679.418 1.388.421 2.126a5\
  \.655 5.655 0 01-.18 1.631.167.167 0 00.04.155 5.982 5.982 0 011.57\
  \8 2.891c.385 1.901-.01 3.615-1.183 5.14l-.182.22a6.063 6.063 0 01-\
  \2.934 1.851.162.162 0 00-.108.102c-.255.736-.511 1.364-.987 1.992-\
  \1.199 1.582-2.962 2.462-4.948 2.451-1.583-.008-2.986-.587-4.21-1.7\
  \36a.145.145 0 00-.14-.032c-.518.167-1.04.191-1.604.185a5.924 5.924\
  \ 0 01-2.595-.622 6.058 6.058 0 01-2.146-1.781c-.203-.269-.404-.522\
  \-.551-.821a7.74 7.74 0 01-.495-1.283 6.11 6.11 0 01-.017-3.064.166\
  \.166 0 00.008-.074.115.115 0 00-.037-.064 5.958 5.958 0 01-1.38-2.\
  \202 5.196 5.196 0 01-.333-1.589 6.915 6.915 0 01.188-2.132c.45-1.4\
  \84 1.309-2.648 2.577-3.493.282-.188.55-.334.802-.438.286-.12.573-.\
  \22.861-.304a.129.129 0 00.087-.087A6.016 6.016 0 015.635 2.31C6.31\
  \5 1.464 7.132.846 8.086.457zm-.804 7.85a.848.848 0 00-1.473.842l1.\
  \694 2.965-1.688 2.848a.849.849 0 001.46.864l1.94-3.272a.849.849 0 \
  \00.007-.854l-1.94-3.393zm5.446 6.24a.849.849 0 000 1.695h4.848a.84\
  \9.849 0 000-1.696h-4.848z\"/></svg>"
-- Antigravity's arch, smooth and in its colours as its website shows it (an
-- arch on a ground of blue, warming to yellow, orange and red at the peak,
-- with green on the left — drawn for leksah, not Google's file).  An @img@
-- of a data-URI SVG rather than inline SVG: its gradients need ids, and an
-- image is a document of its own, so the same ids on every card can't clash
-- (nor go unpainted when the first card holding them is hidden).
agentGlyph "agy" =
  "<img class=\"agent-glyph-agy\" alt=\"\" title=\"Antigravity agent\" src=\"data:image/svg+xml,"
    <> T.replace "#" "%23" (T.replace "<" "%3C" (T.replace ">" "%3E" agyLogoSvg)) <> "\">"
-- Claude Code's: its little orange robot, exactly as its banner draws it in
-- quadrant blocks:
--
-- >  ▐▛███▛█
-- > ▝▜██████▀
-- >  ▝▝   ▝▝
--
-- one unit per quadrant, two tall because a terminal cell is twice as tall as
-- it is wide.  The eyes are the gaps in the second row: the badge shows
-- through them.
agentGlyph _ =
  "<svg class=\"agent-glyph-claude\" viewBox=\"1 0 17 10\" fill=\"#d97757\" \
  \shape-rendering=\"crispEdges\"><title>Claude Code agent</title><path d=\"\
  \M3 0h13v2H3zM3 2h2v2H3zM6 2h7v2H6zM14 2h2v2h-2zM1 4h17v2H1zM3 6h13v2H3z\
  \M3 8h1v2H3zM5 8h1v2H5zM13 8h1v2h-1zM15 8h1v2h-1z\"/></svg>"

-- | Antigravity's badge ('agentGlyph'): its arch, clipped from a blue ground
-- with soft spots of the logo's other colours.  Single quotes only (it goes
-- into a double-quoted attribute).
agyLogoSvg :: Text
agyLogoSvg = T.concat
  [ "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 100 100'><defs>"
  , "<clipPath id='a'><path d='M85 90C90 94 97 91 91 85C71 65 74 8 50 8C26 8 29 65 9 85"
  , "C3 91 10 94 15 90C35 76 33 52 50 52C67 52 65 76 85 90Z'/></clipPath>"
  , spot "g" 20 38 34 "#00B95C" ".85" Nothing
  , spot "r" 68 24 34 "#FC413D" "1" (Just ".7")
  , spot "o" 52 10 16 "#FBBC04" "1" Nothing
  , spot "y" 38 16 22 "#FFE432" "1" Nothing
  , "</defs><g clip-path='url(#a)'><rect width='100' height='100' fill='#3186FF'/>"
  , T.concat [ "<rect width='100' height='100' fill='url(#" <> i' <> ")'/>" | i' <- ["g", "r", "o", "y"] ]
  , "</g></svg>" ]
  where
    spot :: Text -> Int -> Int -> Int -> Text -> Text -> Maybe Text -> Text
    spot i' cx cy r c o mid = T.concat
      [ "<radialGradient id='", i', "' cx='", tshow cx, "' cy='", tshow cy, "' r='", tshow r
      , "' gradientUnits='userSpaceOnUse'><stop offset='0' stop-color='", c
      , "' stop-opacity='", o, "'/>"
      , maybe "" (\m -> "<stop offset='.5' stop-color='" <> c <> "' stop-opacity='" <> m <> "'/>") mid
      , "<stop offset='1' stop-color='", c, "' stop-opacity='0'/></radialGradient>" ]
    tshow = T.pack . show
