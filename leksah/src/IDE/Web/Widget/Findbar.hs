{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput' in favour of the lower-level
-- 'inputElement'.  The find/replace fields deliberately use 'textInput' (its
-- value/keypress accessors are exactly what we need); migrating the
-- focus/keypress-sensitive fields carries regression risk for no behavioural
-- gain, so silence the deprecation here rather than churn the widget.
{-# OPTIONS_GHC -Wno-deprecations #-}
module IDE.Web.Widget.Findbar
  ( findbarCss
  , findbarWidget
  , findMatcher
  , findSelection
  ) where

import Control.Lens ((.~), (^.))
import Control.Monad (void)
import Data.Bits (testBit)
import Data.Bool (bool)
import Data.Char (isAlphaNum)
import Data.Default (Default(..))
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
       (toLower, isInfixOf, null, length, drop, breakOnAll, last, head)

import qualified Text.Regex.TDFA as RE
import Text.Regex.TDFA.Text (compile, execute)

import Clay
       (flexGrow, flex, display, middle, fontSize,
        color, borderStyle, textDecoration, vGradient, backgroundImage,
        borderRadius, padding, hover, (#), background, margin, px, width,
        height, (?), Css, Color(..), None(..), VerticalAlign(..))

import Language.Javascript.JSaddle (jsg, js0, js3, liftJSM)

import Reflex
       (constDyn, Dynamic, toggle, ffor, ffilter, tagPromptlyDyn,
        updated, leftmost, gate, current, holdDyn, foldDyn,
        fmapMaybe)
import Reflex.Dom.Core
       (elDynAttr, elDynAttr', textInput, text, (=:),
        Event, attributes, domEvent, EventName(..), blank,
        _textInput_value, _textInput_keypress)

import IDE.Web.Theme
       (selectionColor, fgColor, accentHoverColor,
        barTopColor, barBottomColor, inputTopColor, inputBottomColor)
import IDE.Web.Events (FindbarEvents(..), TabKey(..))
import IDE.Web.Frame (MonadWidget, performEvent_)

findbarCss :: Css
findbarCss = do
    ".findbar" ? do
        display flex
        backgroundImage (vGradient barTopColor barBottomColor)
    ".findbar button" ? do
        verticalAlign middle
        borderRadius (px 3) (px 3) (px 3) (px 3)
        padding (px 2) (px 10) (px 2) (px 10)
        margin (px 0) (px 0) (px 0) (px 0)
        textDecoration none
        borderStyle none
        fontSize (px 13)
        background (Rgba 0 0 0 0.0)
        color fgColor
    ".findbar button" # hover ?
        background accentHoverColor
    ".findbar button.selected" ?
        background selectionColor
    ".findbar input" ? do
        verticalAlign middle
        backgroundImage (vGradient inputTopColor inputBottomColor)
        color fgColor
        margin (px 0) (px 10) (px 0) (px 10)
        padding (px 2) (px 2) (px 2) (px 2)
        borderStyle none
        fontSize (px 13)
    ".findbar .find-text" ?
        flexGrow 100
    -- The replace controls (CM-only) are grouped so they can be hidden together.
    ".findbar .findbar-replace" ?
        display flex
    ".findbar .findbar-replace.hidden" ?
        display none
    ".findbar-button" ? do
        verticalAlign middle
        height (px 16)
        width (px 16)
        borderRadius (px 3) (px 3) (px 3) (px 3)
        padding (px 2) (px 10) (px 2) (px 10)
        margin (px 0) (px 0) (px 0) (px 0)
    ".findbar-button" # hover ?
        background accentHoverColor
    ".findbar-button.selected" ?
        background selectionColor

-- | An image button; returns its click event.
findbarButton
  :: MonadWidget t m
  => Dynamic t Bool
  -> Text
  -> m (Event t ())
findbarButton selected src = do
    (e, _) <- elDynAttr' "img"
        (ffor selected $ \s ->
            "src" =: src <> "class" =: ("findbar-button" <> bool "" " selected" s))
        blank
    return $ domEvent Click e

-- | A text button that toggles a boolean state on click (shown as "selected").
toggleButton :: MonadWidget t m => Text -> m (Dynamic t Bool)
toggleButton lbl = do
    rec stD <- toggle False (domEvent Click e)
        (e, _) <- elDynAttr' "button"
            (ffor stD $ \s -> "class" =: bool "" "selected" s) $ text lbl
    return stD

findbarWidget
  :: MonadWidget t m
  => Dynamic t (Maybe TabKey)   -- ^ the active pane (find/replace targets it)
  -> Dynamic t Bool             -- ^ whether the find bar is shown (Edit ▸ Find toggles it)
  -> m (Event t FindbarEvents)
findbarWidget activePaneD visibleD = do
  (barEl, innerE) <- elDynAttr' "div"
      (ffor visibleD $ \v -> "class" =: ("findbar" <> bool " hidden" "" v)) $ do
    -- Whether the active pane is a CodeMirror editor: Replace is shown only for
    -- editors.  (List/tree-pane find is handled in Haskell elsewhere.)
    let isCMD = (\case Just (EditorKey _) -> True; _ -> False) <$> activePaneD
        -- Whether find should drive the JS layer (LeksahCM): CodeMirror editors
        -- and terminals (xterm's SearchAddon).  Both virtualise/canvas-render
        -- their text, so a DOM search would miss off-screen matches; the JS
        -- dispatch in findSet/findNext/findPrev picks the right one by active
        -- pane.  All other panes (lists/trees) are searched in Haskell.
        isJSD = (\case Just (EditorKey _)    -> True
                       Just (TerminalKey _)  -> True
                       Just (LeksahWinKey _) -> True
                       _                     -> False) <$> activePaneD

    caseD  <- toggleButton "Case"
    wordsD <- toggleButton "Words"
    regexD <- toggleButton "Regex"
    let flagsD = (\c w r -> (if c then 1 else 0) + (if w then 2 else 0)
                          + (if r then 4 else 0 :: Int)) <$> caseD <*> wordsD <*> regexD

    findTi <- textInput $ def & attributes .~ constDyn
        (noCorrect <> "class" =: "find-text" <> "placeholder" =: "Find")
    prevE <- findbarButton (constDyn False) "/pics/tango/actions/go-previous.svg"
    _     <- findbarButton (constDyn True)  "/pics/tango/actions/view-refresh.svg"
    nextE <- findbarButton (constDyn False) "/pics/tango/actions/go-next.svg"

    (replE, replAllE, replTi) <-
        elDynAttr "div"
            (ffor isCMD $ \b -> "class" =: ("findbar-replace" <> bool " hidden" "" b)) $ do
        rti <- textInput $ def & attributes .~ constDyn
            (noCorrect <> "class" =: "replace-text" <> "placeholder" =: "Replace with")
        re  <- findbarButton (constDyn False) "/pics/tango/actions/edit-find-replace.svg"
        (ae, _) <- elDynAttr' "button" (constDyn mempty) $ text "Replace All"
        return (re, domEvent Click ae, rti)

    (grepBtn, _) <- elDynAttr' "button" (constDyn mempty) $ text "Grep"

    let searchD  = _textInput_value findTi
        replaceD = _textInput_value replTi
        curD     = (,,) <$> searchD <*> replaceD <*> flagsD
        enterE   = () <$ ffilter (== 13) (_textInput_keypress findTi)
        runSet (s, rep, fl) =
            liftJSM . void $ jsg ("LeksahCM" :: Text) ^. js3 ("findSet" :: Text) s rep fl
        runOp nm = liftJSM . void $ jsg ("LeksahCM" :: Text) ^. js0 (nm :: Text)
        -- Route to the JS layer (CodeMirror or terminal) when one is active;
        -- otherwise emit find commands for the active list/tree pane (Haskell).
        -- Replace stays editor-only (whenCM).
        whenJS   = gate (current isJSD)
        whenCM   = gate (current isCMD)
        whenList = gate (current (not <$> isJSD))
        stepE    = leftmost [nextE, enterE]
        -- tagPromptlyDyn samples the *new* value in the same frame (plain
        -- `tag . current` would give the previous value — the off-by-one).
        onChange e = performEvent_ $ runSet <$> tagPromptlyDyn curD (whenJS e)
    -- Editor/terminal: typing/toggling (re)sets the query and reveals the first
    -- match; Enter / the prev & next buttons step through matches.
    onChange (() <$ updated searchD)
    onChange (() <$ updated flagsD)
    onChange (() <$ updated replaceD)
    performEvent_ $ ffor (whenJS stepE) $ \_ -> runOp "findNext"
    performEvent_ $ ffor (whenJS prevE) $ \_ -> runOp "findPrev"
    performEvent_ $ ffor (tagPromptlyDyn curD (whenCM replE))    $ \q -> runSet q >> runOp "replaceNext"
    performEvent_ $ ffor (tagPromptlyDyn curD (whenCM replAllE)) $ \q -> runSet q >> runOp "replaceAll"
    -- List/tree panes: emit commands for the active pane to act on (Haskell).
    let queryChangedE = whenList (leftmost [() <$ updated searchD, () <$ updated flagsD])
        findUpdateE   = (\(s, _, fl) -> FindUpdate s fl) <$> tagPromptlyDyn curD queryChangedE
        findStepE     = leftmost [ FindStep True  <$ whenList stepE
                                 , FindStep False <$ whenList prevE ]
        -- Grep is global (not gated on the active pane): it greps the workspace.
        grepE         = (\(s, _, fl) -> FindGrep s fl) <$> tagPromptlyDyn curD (domEvent Click grepBtn)
    return $ leftmost [findUpdateE, findStepE, grepE]
  -- Escape anywhere in the bar (the inputs' keydowns bubble to the container):
  -- close the bar and hand the keyboard back to the active pane (Main routes
  -- 'FindHide' into the visibility fold and a re-select of the active tab).
  let escE = () <$ ffilter (== 27) (domEvent Keydown barEl)
  return $ leftmost [innerE, FindHide <$ escE]
  where
    -- Stop the browser "helpfully" rewriting what's typed into the find boxes.
    noCorrect = "autocorrect" =: "off" <> "autocapitalize" =: "off"
             <> "spellcheck" =: "false" <> "autocomplete" =: "off"

-- | Build a match predicate from the query and the flag bitmask
-- (bit 0 = case sensitive, bit 1 = whole word, bit 2 = regexp).  Built once per
-- query so a regexp is compiled only once.
findMatcher :: Text -> Int -> (Text -> Bool)
findMatcher q flags
  | T.null q        = const False
  | testBit flags 2 = regexMatch cs q
  | testBit flags 1 = \hay -> wordMatch needle (norm hay)
  | otherwise       = \hay -> needle `T.isInfixOf` norm hay
  where
    cs     = testBit flags 0
    needle = if cs then q else T.toLower q
    norm h = if cs then h else T.toLower h

regexMatch :: Bool -> Text -> (Text -> Bool)
regexMatch cs q =
  case compile (RE.defaultCompOpt { RE.caseSensitive = cs }) RE.defaultExecOpt q of
    Left _   -> const False
    Right re -> either (const False) (maybe False (const True)) . execute re

-- | Whole-word substring match (needle already case-normalised); a match counts
-- only when it isn't flanked by word characters.
wordMatch :: Text -> Text -> Bool
wordMatch needle hay = any atWordBoundary (T.breakOnAll needle hay)
  where
    n = T.length needle
    atWordBoundary (before, after) = edge (lastT before) && edge (firstT (T.drop n after))
    edge Nothing  = True
    edge (Just c) = not (isAlphaNum c || c == '_')
    lastT t  = if T.null t then Nothing else Just (T.last t)
    firstT t = if T.null t then Nothing else Just (T.head t)

-- | Drive a select-the-matching-item find over an ordered item list (used by
-- the list/tree panes).  Returns the currently selected key: 'FindUpdate'
-- resets to the first match, 'FindStep' cycles next/previous.
findSelection
  :: (MonadWidget t m, Ord k)
  => Event t FindbarEvents
  -> Dynamic t [(k, Text)]        -- ^ items in display order (key, searchable text)
  -> m (Dynamic t (Maybe k))
findSelection findE itemsD = do
  queryD <- holdDyn ("", 0) $
    fmapMaybe (\case FindUpdate q fl -> Just (q, fl); _ -> Nothing) findE
  let matcherD = (\(q, fl) -> findMatcher q fl) <$> queryD
      matchesD = (\m items -> [ k | (k, t) <- items, m t ]) <$> matcherD <*> itemsD
  idxD <- foldDyn ($) (0 :: Int) $ leftmost
    [ const 0 <$ updated queryD
    , (\d i -> i + d) <$> fmapMaybe (\case FindStep nxt -> Just (if nxt then 1 else -1); _ -> Nothing) findE ]
  return $ (\ms i -> if null ms then Nothing else Just (ms !! (i `mod` length ms)))
             <$> matchesD <*> idxD
