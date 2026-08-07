{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
-- | The AI-session picker's presentation: one row per session, and the JS that
-- makes Enter\/Escape\/arrows work while it is up.
--
-- The overlay itself is the FLIPPER ('IDE.Web.Widget.Flipper.flipperWidget') —
-- same list, same highlight, same commit-on-modifier-release — so the two feel
-- identical.  Only the rows differ, which is all that lives here.
module IDE.Web.Widget.AIPicker
  ( aiPickerCss
  , aiChoiceLabel
  , aiPickerKeysJs
  ) where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T

import Clay
       (Css, (?), color, fontSize, fontStyle, italic, opacity, px, marginRight,
        marginLeft, textAlign, width, inlineBlock, center, Color(Other))
import qualified Clay (display)

import Reflex (Dynamic)
import Reflex.Dom.Core (elDynAttr, dynText, (=:))

import System.FilePath (takeFileName)

import IDE.Web.AISession (AIChoice(..), AIRow(..))
import IDE.Web.Theme (dimColor)
import IDE.Web.Frame (MonadWidget)

-- The same three status colours every other Claude surface uses (the in-page
-- traffic light's @statusLightJs@ and the workspace tree's badges) — kept
-- literal here for exactly that reason: they must match, and they are not
-- theme-dependent.
waitingColor, busyColor, idleColor :: Color
waitingColor = Other "#f85149"
busyColor    = Other "#d29922"
idleColor    = Other "#3fb950"

-- | Row chrome for the picker: the state glyph (the same shapes every Claude
-- status surface draws — triangle needs you, diamond working, circle idle,
-- hollow ring gone), the working directory after the title, and the ★ marking
-- the active pane's current default.
aiPickerCss :: Css
aiPickerCss = do
  ".ai-glyph" ? do
    Clay.display inlineBlock
    width (px 14)
    textAlign center
    marginRight (px 6)
  ".ai-glyph.ai-waiting" ? color waitingColor
  ".ai-glyph.ai-busy"    ? color busyColor
  ".ai-glyph.ai-idle"    ? color idleColor
  ".ai-glyph.ai-closed"  ? color dimColor
  ".ai-glyph.ai-new"     ? color dimColor
  -- The directory a session runs in — usually its worktree, which is what tells
  -- two sessions of the same project apart.
  ".ai-dir" ? do
    color dimColor
    marginLeft (px 8)
    fontSize (px 11)
  ".ai-note" ? do
    color dimColor
    marginLeft (px 6)
    fontSize (px 11)
    fontStyle italic
  ".ai-default" ? marginLeft (px 6)
  ".ai-default" ? color busyColor

-- | One picker row: @glyph title · dir ★@ for a session (plus a note when it has
-- exited, since committing it resumes it first), or @+ New Claude session in
-- dir@ for the last row.
aiChoiceLabel :: MonadWidget t m => Dynamic t AIChoice -> m ()
aiChoiceLabel choiceD = do
  elDynAttr "span" (glyphAttr <$> choiceD) $ dynText (glyphFor <$> choiceD)
  elDynAttr "span" (pure ("class" =: "ai-title")) $ dynText (titleFor <$> choiceD)
  elDynAttr "span" (pure ("class" =: "ai-dir")) $ dynText (dirFor <$> choiceD)
  elDynAttr "span" (noteAttr <$> choiceD) $ dynText (noteFor <$> choiceD)
  elDynAttr "span" (starAttr <$> choiceD) $ dynText (starFor <$> choiceD)
  where
    -- Explicit signatures: '=:' is generic over any At/Monoid container, so the
    -- attribute maps have to be pinned down here.
    glyphAttr, noteAttr, starAttr :: AIChoice -> Map Text Text
    glyphAttr (AISessionChoice r) = "class" =: ("ai-glyph ai-" <> arState r)
    glyphAttr (AINewSession _)    = "class" =: "ai-glyph ai-new"
    glyphFor (AINewSession _) = "+"
    glyphFor (AISessionChoice r) = case arState r of
      "waiting" -> "▲"
      "busy"    -> "◆"
      "idle"    -> "●"
      _         -> "○"
    titleFor (AISessionChoice r) = arTitle r
    titleFor (AINewSession _)    = "New Claude session"
    dirFor (AISessionChoice r) = T.pack (takeFileName (arDir r))
    dirFor (AINewSession d)    = T.pack (takeFileName d)
    noteAttr (AISessionChoice r)
      | not (arLive r) = "class" =: "ai-note"
    noteAttr _         = "style" =: "display:none"
    noteFor (AISessionChoice r) | not (arLive r) = "closed — will resume"
    noteFor _                                    = ""
    starAttr (AISessionChoice r)
      | arDefault r = "class" =: "ai-default"
    starAttr _      = "style" =: "display:none"
    starFor (AISessionChoice r) | arDefault r = "★"
    starFor _                                 = ""

-- | Keyboard handling for the picker, in JS.
--
-- It has to be JS and it has to be capture-phase.  The picker is often opened
-- while a tmux pane has DOM focus, so Enter would otherwise ALSO reach xterm.js
-- and submit a newline into the shell; and a Haskell handler can't prevent that,
-- because jsaddle-wkwebview dispatches events asynchronously, so its
-- @preventDefault@ lands too late.
--
-- Installed once; @window.leksahAIPicker(open)@ arms and disarms it, and it
-- reports the user's intent back through @window.leksahAIPickerKey(action)@.
aiPickerKeysJs :: Text
aiPickerKeysJs = T.unlines
  [ "window.__lkAiOpen = false;"
  , "window.leksahAIPicker = function(open){ window.__lkAiOpen = !!open; };"
  , "document.addEventListener('keydown', function(e){"
  , "  if (!window.__lkAiOpen) return;"
  , "  var a = null;"
  , "  switch (e.key) {"
  , "    case 'Enter':      a = 'commit'; break;"
  , "    case 'Escape':     a = 'cancel'; break;"
  , "    case 'ArrowDown':  a = 'down';   break;"
  , "    case 'ArrowUp':    a = 'up';     break;"
  , "    case 'Tab':        a = e.shiftKey ? 'up' : 'down'; break;"
  , "  }"
  , "  if (!a) return;"
  , "  e.preventDefault(); e.stopPropagation();"
  , "  if (window.leksahAIPickerKey) window.leksahAIPickerKey(a);"
  , "}, true);"
  ]
