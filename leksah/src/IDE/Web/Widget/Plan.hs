{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; leksah still uses it deliberately.
{-# OPTIONS_GHC -Wno-deprecations #-}
-- | The plan-review pane (plan Stage 4, "spec approval before coding"): when
-- a Claude session finishes plan mode it sits at the ExitPlanMode approval
-- prompt in its terminal — this pane pulls the plan markdown out of the
-- session's transcript ('IDE.Web.Claude.claudeLatestPlan') and renders it
-- full-size, with Approve \/ Approve-and-auto-accept \/ Revise buttons that
-- answer the prompt in the session's pane (tmux send-keys — "1", "2", or
-- "3" + the feedback text).
module IDE.Web.Widget.Plan
  ( planCss
  , planWidget
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try, SomeException)
import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import qualified Data.Text as T

import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Clay ((?), (-:), Css)

import GHCJS.DOM.Element (setAttribute)
import Language.Javascript.JSaddle (liftJSM)

import Reflex
       (Event, holdDyn, getPostBuild, newTriggerEvent,
        ffor, tag, current, never, constDyn, leftmost)
import Reflex.Dom.Core
       (divClass, elClass, elAttr', dynText, blank,
        text, domEvent, EventName(..), (=:), textInput, _textInput_value,
        _element_raw, TextInputConfig, _textInputConfig_attributes)

import Data.Default (def)

import IDE.Web.Claude (claudeLatestPlan, mruClaudePane)
import IDE.Web.ReplTmux (sendKeysTo)
import IDE.Web.Events (PlanEvents)
import IDE.Web.Frame (MonadWidget, performEvent_)

planWidget
  :: forall t m . MonadWidget t m
  => FilePath      -- ^ the session's directory (locates its pane)
  -> FilePath      -- ^ the session's transcript (locates its plan)
  -> Event t ()    -- ^ the tab's select pulse — re-surfacing an already-open
                   --   plan tab (a later plan round of the same session)
                   --   rescans, so the pane never shows a stale plan
  -> m (Event t PlanEvents)
planWidget dir transcript selectE = divClass "plan" $ do
  pb <- getPostBuild
  (planE, firePlan)     <- newTriggerEvent
  (statusE, fireStatus) <- newTriggerEvent
  let rescan = void . forkIO $ claudeLatestPlan transcript >>= firePlan
  performEvent_ $ liftIO rescan <$ leftmost [pb, selectE]
  statusD <- holdDyn "" statusE

  -- Header: verdict buttons + status.
  divClass "plan-header" $ do
    elClass "span" "plan-title" $ text "Plan review"
    elClass "span" "plan-status" $ dynText statusD
    divClass "plan-actions" $ do
      -- Option numbers track Claude Code's current ExitPlanMode prompt:
      -- 1 = yes + auto mode, 2 = yes + manual edit approval, 4 = tell
      -- Claude what to change.
      let answer name keys = do
            (e, _) <- elAttr' "button" ("class" =: "review-btn") $ text name
            performEvent_ $ ffor (domEvent Click e) $ \_ ->
              liftIO . void . forkIO $ sendAnswer keys "" >>= fireStatus
      answer "Approve" "2"
      answer "Approve (auto mode)" "1"
      (refrEl, _) <- elAttr' "button" ("class" =: "review-btn") $ text "Refresh"
      performEvent_ $ ffor (domEvent Click refrEl) $ \_ -> liftIO rescan

  -- Revise: reject the plan and type the feedback into the session.
  divClass "plan-revise" $ do
    ti <- textInput $ (def :: TextInputConfig t)
      { _textInputConfig_attributes = constDyn
          ("placeholder" =: "what to change — sent with “tell Claude what to change”"
           <> "style" =: "flex:1 1 auto;min-width:0") }
    (sendEl, _) <- elAttr' "button" ("class" =: "review-btn") $ text "Revise"
    performEvent_ $ ffor (tag (current (_textInput_value ti))
                              (domEvent Click sendEl)) $ \note ->
      liftIO . void . forkIO $
        if T.null (T.strip note)
          then fireStatus "type the revision first"
          else sendAnswer "4" (T.strip note) >>= fireStatus

  -- The plan itself, rendered as a real HTML DOCUMENT in a fully sandboxed
  -- iframe (no scripts, no same-origin): a markdown plan is converted by
  -- 'mdHtml' and wrapped in a styled shell; a plan that already IS an HTML
  -- document (e.g. an .html plan file) passes straight through.  srcdoc is
  -- only ever set from an event, never in the initial build batch (an iframe
  -- loading mid-batch wedged the wkwebview bridge — the browser-pane lesson).
  (frameEl, _) <- divClass "plan-body" $
    elAttr' "iframe" ("class" =: "plan-frame" <> "sandbox" =: "") blank
  performEvent_ $ ffor planE $ \mp -> liftJSM $
    setAttribute (_element_raw frameEl) ("srcdoc" :: Text) $ case mp of
      Nothing -> planShell
        "<i>No plan found in this session's transcript (yet). \
        \Ask the session to plan (plan mode), then Refresh.</i>"
      Just p
        | looksHtml p -> p
        | otherwise   -> planShell (mdHtml p)
  return (never :: Event t PlanEvents)
 where
  -- Answer the ExitPlanMode prompt in the session's MRU pane: the option
  -- keys act on press; feedback (for "3" = keep planning) follows after the
  -- picker has yielded to the composer, then Enter submits it.
  sendAnswer :: String -> Text -> IO Text
  sendAnswer keys feedback = mruClaudePane dir >>= \case
    Nothing  -> return "no agent session is running here"
    Just pid -> do
      ok <- keysTo pid [keys]
      unless (T.null feedback) $ do
        threadDelay 700000
        _ <- keysTo pid ["-l", T.unpack feedback]
        threadDelay 200000
        void $ keysTo pid ["Enter"]
      return $ if ok then "answered the session ("
                          <> (case keys of
                                "2" -> "approve"
                                "1" -> "approve, auto mode"
                                _   -> "revise + feedback")
                          <> ")"
                     else "send failed (tmux send-keys)"
    where
      keysTo = sendKeysTo

-- | Does the plan text already look like an HTML document?  Then it goes to
-- the (sandboxed) iframe as-is rather than through the markdown converter.
looksHtml :: Text -> Bool
looksHtml t = any (`T.isPrefixOf` T.toLower (T.stripStart t))
                  ["<!doctype", "<html"]

-- | Wrap converted plan HTML in a small self-contained document (the iframe
-- shares no CSS with the app): readable typography, light/dark via the OS
-- scheme.
planShell :: Text -> Text
planShell body = T.concat
  [ "<!doctype html><html><head><meta charset=\"utf-8\"><style>"
  , ":root{color-scheme:light dark}"
  , "body{font:13px/1.5 -apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;"
  , "margin:0;padding:10px 16px;background:#fff;color:#24292f;max-width:52rem}"
  , "@media (prefers-color-scheme:dark){body{background:#14161a;color:#c9d1d9}"
  , "pre,code{background:#1d2127!important;border-color:#333a44!important}"
  , "h2,h3,h4{border-color:#333a44!important}a{color:#b08bf8!important}}"
  , "h2,h3,h4{margin:14px 0 6px;padding-bottom:3px;border-bottom:1px solid #d9dee5}"
  , "pre{background:#f6f7f9;border:1px solid #d9dee5;border-radius:5px;"
  , "padding:7px 10px;overflow-x:auto;font-size:12px;"
  , "font-family:ui-monospace,Menlo,monospace}"
  , "code{background:#f6f7f9;border-radius:3px;padding:0 3px;"
  , "font-family:ui-monospace,Menlo,monospace;font-size:.92em}"
  , "pre code{background:none;border:none;padding:0}"
  , "p{margin:6px 0}ul,ol{margin:4px 0;padding-left:1.6em}li{margin:2px 0}"
  , "a{color:#7c3aed}input[type=checkbox]{margin-right:5px}"
  , "table{border-collapse:collapse;margin:8px 0;font-size:12.5px}"
  , "th,td{border:1px solid #d9dee5;padding:4px 9px;text-align:left}"
  , "th{background:#f6f7f9}"
  , "@media (prefers-color-scheme:dark){th,td{border-color:#333a44!important}"
  , "th{background:#1d2127!important}}"
  , "</style></head><body>", body, "</body></html>" ]

-- | Escaped markdown-lite → HTML: fenced code, #…#### headers, bullet and
-- NUMBERED lists, checkboxes, **bold**, `inline code`, [text](http links),
-- paragraphs.  Everything is HTML-escaped FIRST and only a fixed tag set is
-- emitted (hrefs restricted to http/https), and the result renders inside a
-- fully sandboxed iframe — three layers against transcript-content injection.
mdHtml :: Text -> Text
mdHtml src = go (T.lines src) []
  where
    go [] acc = T.concat (reverse acc)
    go (l : ls) acc
      | "```" `T.isPrefixOf` T.stripStart l =
          let (code, rest) = break (("```" `T.isPrefixOf`) . T.stripStart) ls
          in go (drop 1 rest)
                (("<pre><code>" <> esc (T.unlines code) <> "</code></pre>") : acc)
      | Just h <- T.stripPrefix "#### " l = go ls (("<h4>" <> inline h <> "</h4>") : acc)
      | Just h <- T.stripPrefix "### "  l = go ls (("<h4>" <> inline h <> "</h4>") : acc)
      | Just h <- T.stripPrefix "## "   l = go ls (("<h3>" <> inline h <> "</h3>") : acc)
      | Just h <- T.stripPrefix "# "    l = go ls (("<h2>" <> inline h <> "</h2>") : acc)
      | isBullet l =
          let (items, rest) = span isBullet (l : ls)
          in go rest
                (("<ul>" <> T.concat [ "<li>" <> li (T.drop 2 (T.stripStart i)) <> "</li>"
                                     | i <- items ] <> "</ul>") : acc)
      | isNumbered l =
          let (items, rest) = span isNumbered (l : ls)
          in go rest
                (("<ol>" <> T.concat [ "<li>" <> inline (dropMarker i) <> "</li>"
                                     | i <- items ] <> "</ol>") : acc)
      | isRow l =
          let (rows0, rest) = span isRow (l : ls)
              -- the |---|---| alignment row separates header from body
              (hdr, body) = case rows0 of
                (h : sep : rs) | isSepRow sep -> ([h], rs)
                rs                            -> ([], rs)
              tr tag r = "<tr>" <> T.concat
                [ "<" <> tag <> ">" <> inline (T.strip c) <> "</" <> tag <> ">"
                | c <- cells r ] <> "</tr>"
          in go rest
                (("<table>" <> T.concat (map (tr "th") hdr)
                            <> T.concat (map (tr "td") body) <> "</table>") : acc)
      | T.null (T.strip l) = go ls acc
      | otherwise = go ls (("<p>" <> inline l <> "</p>") : acc)
    isBullet l = let s = T.stripStart l
                 in "- " `T.isPrefixOf` s || "* " `T.isPrefixOf` s
    isNumbered l = let (ds, rest) = T.span (`elem` ("0123456789" :: String))
                                           (T.stripStart l)
                   in not (T.null ds) && (". " `T.isPrefixOf` rest)
    -- Markdown table plumbing: a row is |-delimited; the |---|:---| row is
    -- the header/body separator.
    isRow l = "|" `T.isPrefixOf` T.stripStart l
    isSepRow l = isRow l
              && T.all (`elem` ("|-: " :: String)) (T.strip l)
              && "-" `T.isInfixOf` l
    cells r = let s = T.strip r
                  trimmed = maybe s id (T.stripPrefix "|" s)
                  trimmed' = maybe trimmed id (T.stripSuffix "|" trimmed)
              in T.splitOn "|" trimmed'
    dropMarker l = T.drop 2 (T.dropWhile (`elem` ("0123456789" :: String))
                                         (T.stripStart l))
    -- "- [ ] task" / "- [x] task" bullets become disabled checkboxes.
    li s | Just r <- T.stripPrefix "[ ] " s =
             "<input type=\"checkbox\" disabled>" <> inline r
         | Just r <- T.stripPrefix "[x] " s =
             "<input type=\"checkbox\" checked disabled>" <> inline r
         | otherwise = inline s
    esc = T.replace ">" "&gt;" . T.replace "<" "&lt;" . T.replace "&" "&amp;"
    -- inline: escape, then `code`, **bold**, and [text](http…) links (in that
    -- order, on the escaped text — replacements only introduce our own tags).
    inline t = links (bolds (codes (esc t)))
    codes t = case T.splitOn "`" t of
      xs | odd (length xs) ->
             T.concat (zipWith (\i x -> if even (i :: Int) then x
                                        else "<code>" <> x <> "</code>") [0..] xs)
         | otherwise -> t
    bolds t = case T.splitOn "**" t of
      xs | odd (length xs) ->
             T.concat (zipWith (\i x -> if even (i :: Int) then x
                                        else "<strong>" <> x <> "</strong>") [0..] xs)
         | otherwise -> t
    -- [text](url): url must be http(s) (no javascript: etc.); anything else
    -- is left as literal text.  Runs on escaped text, so url can't carry
    -- quotes (they are &quot; by now) — safe inside the href attribute.
    links t = case T.breakOn "[" t of
      (pre, rest) | T.null rest -> t
                  | otherwise -> case T.breakOn "](" (T.drop 1 rest) of
          (_, rest2) | T.null rest2 -> pre <> "[" <> links (T.drop 1 rest)
          (label, rest2) -> case T.breakOn ")" (T.drop 2 rest2) of
            (_, rest3) | T.null rest3 -> pre <> "[" <> links (T.drop 1 rest)
            (url, rest3)
              | any (`T.isPrefixOf` url) ["http://", "https://"] ->
                  pre <> "<a href=\"" <> url <> "\" target=\"_blank\">"
                      <> label <> "</a>" <> links (T.drop 1 rest3)
              | otherwise -> pre <> "[" <> links (T.drop 1 rest)

planCss :: Css
planCss = do
    ".plan" ? do
        "display"        -: "flex"
        "flex-direction" -: "column"
        "height"         -: "100%"
        "overflow"       -: "hidden"
    ".plan-header" ? do
        "display"     -: "flex"
        "align-items" -: "center"
        "gap"         -: "10px"
        "padding"     -: "4px 8px"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".plan-title" ? ("font-weight" -: "bold")
    ".plan-status" ? do
        "flex"      -: "1 1 auto"
        "color"     -: "var(--leksah-fg-dim)"
        "font-size" -: "12px"
        "overflow"  -: "hidden"
        "white-space" -: "nowrap"
        "text-overflow" -: "ellipsis"
        "min-width" -: "0"
    ".plan-actions" ? do
        "display" -: "flex"
        "gap"     -: "6px"
    ".plan-revise" ? do
        "display"     -: "flex"
        "gap"         -: "6px"
        "padding"     -: "4px 8px"
        "align-items" -: "center"
        "border-bottom" -: "1px solid var(--leksah-border, #333)"
    ".plan-body" ? do
        "flex"     -: "1 1 0"
        "min-height" -: "0"
        "overflow" -: "hidden"
    ".plan-frame" ? do
        "width"  -: "100%"
        "height" -: "100%"
        "border" -: "none"
