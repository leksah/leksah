{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- reflex-dom deprecates 'textInput'; the find bar (and the rest of leksah)
-- still uses it deliberately, so match that here rather than churn.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | A text input with a drop-down list under it: the three fields of the
-- @Add Project…@ modal ("IDE.Web.Widget.AddProject").
--
-- There is ONE popup, not a completion popup plus a separate recents drop-down.
-- What the list contains is a function of state — recents until you type, then
-- completions — so ↑\/↓\/Tab\/Enter\/Esc have a single meaning in all three
-- fields, and nothing has to arbitrate between two overlapping lists:
--
-- > itemsD = if dirty then completions else recents
--
-- 'comboField' is 'pathField' with the completion source switched off, so the
-- Server and Prefix fields behave exactly like the Path field minus the
-- directory listing.
--
-- Deliberately NOT built on "IDE.Web.Widget.Flipper"'s 'flipperWidget': that is
-- a modal overlay driven by ⌘\`-style step events, committing on modifier
-- release, with wrap-around indexing.  This list is inline, must allow "nothing
-- highlighted" (so Enter submits the form instead of picking a row), and must
-- not wrap past its ends.
--
-- Frame-thread rule: a throw inside a reflex 'performEvent' freezes the whole
-- window (see "IDE.Web.Widget.FileTree"), so NOTHING here touches the file
-- system, ssh or the home directory on the frame thread.  Every listing runs on
-- a forked thread, is 'catch'-wrapped to @[]@, and is dropped on arrival if a
-- newer one has been issued.
module IDE.Web.Widget.PathField
  ( PathFieldConfig(..)
  , PathField(..)
  , pathField
  , comboField
  , localServer
  , pathFieldKeysJs
  , focusFieldJs
  -- * Pure helpers (exported for reuse and testing)
  , collapseHome
  , expandHome
  , completionSplit
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Lens ((.~))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Function ((&))
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))

import Language.Javascript.JSaddle (liftJSM, eval)

import Reflex
       (Dynamic, Event, attachWith, attachWithMaybe, constDyn, count, current,
        ffor, ffor2, fmapMaybe, foldDyn, holdDyn, holdUniqDyn, leftmost,
        newTriggerEvent, switchDyn, tag, updated)
import Reflex.Dom.Core
       (attributes, debounce, domEvent, dynText, elAttr, elAttr', elDynAttr,
        elDynAttr', EventName(..), simpleList, text, textInput,
        textInputConfig_initialValue, textInputConfig_setValue, (=:),
        _textInput_input, _textInput_keydown, _textInput_value)

import IDE.Utils.RemoteExec (remoteHomeDir)
import IDE.Utils.RemotePath (renderRemotePath)
import IDE.Web.FS (fsListDirectory)
import IDE.Web.Frame (MonadWidget, performEvent_)

-- | The server name that means "this machine".  Not an ssh host, so it can
-- never collide with one ('rcHosts' entries are host names).
localServer :: Text
localServer = "local"

data PathFieldConfig t = PathFieldConfig
  { pfcId          :: Text
      -- ^ DOM id.  Stable, because the keydown guard in 'pathFieldKeysJs' and
      --   the dialog's autofocus both find the input by it.
  , pfcPlaceholder :: Text
  , pfcInitial     :: Text          -- ^ seed value, already @~@-collapsed
  , pfcServer      :: Dynamic t Text
      -- ^ @"local"@ or an ssh host: routes the directory listing
  , pfcRecents     :: Dynamic t [Text]
  , pfcSetValue    :: Event t Text
      -- ^ set from outside (a browse result, or a server change re-defaulting
      --   the path).  Also closes the popup and clears the "user has typed"
      --   flag, so the list goes back to recents.
  }

data PathField t = PathField
  { pfValue     :: Dynamic t Text
  , pfSubmit    :: Event t ()
      -- ^ Enter with no row highlighted — i.e. "the form is done"
  , pfCancel    :: Event t ()
      -- ^ Escape with the popup already closed — i.e. "close the dialog"
  , pfPopupOpen :: Dynamic t Bool
  }

-- | A text input whose drop-down is its recents list and nothing else: the
-- Server and Prefix fields.
comboField :: MonadWidget t m
           => Text -> Text -> Text -> Dynamic t [Text] -> Event t Text
           -> m (PathField t)
comboField fid placeholder initial recentsD setE =
    popupField fid placeholder initial setE recentsD (\_ -> pure (constDyn []))

-- | A text input that also completes local and remote paths as you type.
pathField :: MonadWidget t m => PathFieldConfig t -> m (PathField t)
pathField cfg =
    popupField (pfcId cfg) (pfcPlaceholder cfg) (pfcInitial cfg)
               (pfcSetValue cfg) (pfcRecents cfg)
               (pathCompletions (pfcServer cfg))

-- | The one implementation.  @mkCompletions@ is given the live value and
-- returns @(label, value-to-set)@ pairs; 'comboField' passes a source that is
-- always empty, which is the only difference between the two fields.
popupField :: MonadWidget t m
           => Text -> Text -> Text
           -> Event t Text
           -> Dynamic t [Text]
           -> (Dynamic t Text -> m (Dynamic t [(Text, Text)]))
           -> m (PathField t)
popupField fid placeholder initial setE recentsD mkCompletions =
  elAttr "div" ("style" =: fieldWrapStyle) $ do
    rec
      ti <- textInput $ def
              & textInputConfig_initialValue .~ initial
              & textInputConfig_setValue .~ leftmost [setE, acceptE]
              & attributes .~ constDyn
                  ("id" =: fid <> "placeholder" =: placeholder
                   <> "class" =: "leksah-popup-field"
                   -- The browser's own autofill dropdown would sit on top of
                   -- ours and eat the arrow keys.
                   <> "autocomplete" =: "off" <> "spellcheck" =: "false"
                   <> "style" =: fieldStyle)
      let valueD = _textInput_value ti
          typedE = _textInput_input ti          -- user keystrokes only, not setValue
          keyE   = _textInput_keydown ti
          key k  = () <$ fmapMaybe (\k' -> if k' == k then Just () else Nothing) keyE

      completionsD <- mkCompletions valueD

      -- Recents until the user types; completions thereafter.  The ▾ button
      -- goes back to recents without clearing what has been typed.
      dirtyD <- holdDyn False $ leftmost
          [ True  <$ typedE
          , False <$ showRecentsE
          , False <$ setE ]
      let itemsD = ffor2 dirtyD ((,) <$> recentsD <*> completionsD) $
              \dirty (recents, comps) ->
                  if dirty then comps else [ (r, r) | r <- recents ]

      -- Open on ↓, on ▾, on Tab, and as you type; closed by Escape, by an
      -- accept, by a set from outside, and whenever the list is empty.
      nonEmptyD <- holdUniqDyn (not . null <$> itemsD)
      openD <- holdUniqDyn =<< holdDyn False (leftmost
          [ True  <$ leftmost [key 40, key 9, showRecentsE, () <$ typedE]
          , False <$ leftmost [escE, () <$ acceptE, () <$ setE] ])
      let visibleD = (&&) <$> openD <*> nonEmptyD

      -- Highlight: 'Nothing' = no row picked, which is what lets Enter submit
      -- the form.  Clamped at both ends rather than wrapping.
      selD <- foldDyn ($) Nothing $ leftmost
          [ (\n -> \case Nothing -> if n > 0 then Just 0 else Nothing
                         Just i  -> Just (min (n - 1) (i + 1)))
              <$> tag (current (length <$> itemsD)) (key 40)
          , (\case Just 0 -> Nothing
                   Just i -> Just (i - 1)
                   Nothing -> Nothing)
              <$ key 38
            -- A changed list makes an old index meaningless.
          , const Nothing <$ leftmost [() <$ typedE, showRecentsE, () <$ setE, escE]
          ]

      -- Accepting a row, three ways in: click, Enter on the highlight, Tab.
      -- Tab with nothing highlighted completes as far as the candidates agree
      -- (the usual shell behaviour) and otherwise just opens the list.
      let highlighted = ffor2 selD itemsD $ \sel items ->
              sel >>= \i -> case drop i items of { ((_, v):_) -> Just v; _ -> Nothing }
          enterAcceptE = fmapMaybe id (tag (current highlighted) (key 13))
          tabAcceptE   = attachWithMaybe
              (\(items, (sel, v)) _ -> case sel of
                   Just _  -> Nothing            -- handled by enterAccept's sibling below
                   Nothing -> tabComplete v items)
              (current ((,) <$> itemsD <*> ((,) <$> selD <*> valueD)))
              (key 9)
          tabHighlightedE = fmapMaybe id (tag (current highlighted) (key 9))
          acceptE = leftmost [clickE, enterAcceptE, tabHighlightedE, tabAcceptE]
          -- Enter submits only when no row is highlighted; Escape closes the
          -- popup first and only reaches the dialog when there is none.
          submitE = fmapMaybe (\case Nothing -> Just (); Just _ -> Nothing)
                              (tag (current highlighted) (key 13))
          escE    = key 27
          cancelE = fmapMaybe (\visible -> if visible then Nothing else Just ())
                              (tag (current visibleD) escE)

      -- The ▾ button force-opens the recents list at any time.  It is a child
      -- of the FIELD wrapper, not of the list's own wrapper: the latter sits
      -- BELOW the input (it is what @top:100%@ is measured from), so an arrow
      -- positioned against it rendered under the field, on top of the list.
      (dropEl, _) <- elAttr' "button"
          ("type" =: "button" <> "class" =: "leksah-popup-arrow"
           <> "title" =: "Recently used" <> "style" =: arrowStyle) $ text "▾"
      clickE <- elAttr "div" ("style" =: "position:relative") $ do
          rowsD <- elDynAttr "div"
              (ffor visibleD $ \v ->
                  "class" =: ("path-popup" <> if v then " open" else "")
                  -- The leading ';' matters: 'popupStyle' does not end in one,
                  -- so without it "display:none" is swallowed into the
                  -- box-shadow value and the list is never hidden.
                  <> "style" =: (popupStyle <> if v then "" else ";display:none")) $
              simpleList (zip [0 :: Int ..] <$> itemsD) $ \iD -> do
                  let selfSelD = ffor2 selD iD $ \sel (i, _) -> sel == Just i
                  (rowEl, _) <- elDynAttr' "div"
                      (ffor selfSelD $ \s ->
                          "class" =: ("path-popup-row" <> if s then " selected" else "")
                          <> "style" =: (rowStyle <> if s then selectedRowStyle else "")) $
                      dynText (fst . snd <$> iD)
                  pure (snd . snd <$> tag (current iD) (domEvent Click rowEl))
          pure (switchDyn (leftmost <$> rowsD))
      let showRecentsE = () <$ domEvent Click dropEl

      -- Anchor the list under the segment it would replace.  Only in
      -- completion mode: a recents row is a whole path, so it belongs at the
      -- field's left edge.
      alignD <- holdUniqDyn $ ffor2 dirtyD valueD $ \dirty v ->
          if dirty then fst (completionSplit v) else ""
      performEvent_ $ ffor (updated alignD) $ \pfx -> liftJSM . void $ eval
          ("var f=window.leksahPathPopupAlign;\
           \if(f)f(" <> jsStr fid <> "," <> jsStr pfx <> ");")

    pure PathField
      { pfValue     = valueD
      , pfSubmit    = submitE
      , pfCancel    = cancelE
      , pfPopupOpen = visibleD
      }

-- | Tab with nothing highlighted: extend the typed leaf by the longest prefix
-- every candidate shares.  'Nothing' when that would not change anything (so
-- Tab falls through to just opening the list).
tabComplete :: Text -> [(Text, Text)] -> Maybe Text
tabComplete v items = case items of
    []      -> Nothing
    [(_,x)] -> Just x
    _ -> let labels = map fst items
             lcp    = foldr1 commonPrefix labels
             (dir, leaf) = completionSplit v
         in if T.length lcp > T.length leaf && leaf `T.isPrefixOf` lcp
              then Just (dir <> lcp)
              else Nothing
  where
    commonPrefix a b = let (p, _, _) = T.commonPrefixes a b `orElse` ("", a, b) in p
    orElse (Just x) _ = x
    orElse Nothing  y = y

-- | Split typed text at the LAST @\/@, keeping the separator on the left:
-- @"~\/hask\/leks"@ → @("~\/hask\/", "leks")@.  No @\/@ at all → @("", t)@,
-- which lists the working directory.
completionSplit :: Text -> (Text, Text)
completionSplit = T.breakOnEnd "/"

-- | @collapseHome home p@ writes @p@ as @~\/…@ when it is under @home@.
-- Boundary-checked, so @\/Users\/hamishmackenzie@ is NOT collapsed against
-- home @\/Users\/hamish@.
collapseHome :: FilePath -> Text -> Text
collapseHome home t
  | T.null homeT             = t
  | t == homeT               = "~/"
  | homeSlash `T.isPrefixOf` t = "~/" <> T.drop (T.length homeSlash) t
  | otherwise                = t
  where
    homeT     = T.dropWhileEnd (== '/') (T.pack home)
    homeSlash = homeT <> "/"

-- | The inverse, for the moment a path leaves the field: @~\/x@ → @\/home\/u\/x@.
-- Only ever applied to LOCAL paths — a remote @~@ is expanded on the far side
-- by 'IDE.Utils.RemoteExec.resolveProjectInput'.
expandHome :: FilePath -> Text -> Text
expandHome home t
  | t == "~"                = T.pack home
  | "~/" `T.isPrefixOf` t   = T.pack (home </> T.unpack (T.drop 2 t))
  | otherwise               = t

-- | The completion source: debounce, fork the listing, drop stale answers.
--
-- The async part only ever supplies "the names in directory D".  Turning that
-- into candidates is pure and reads the CURRENT value, so a listing for a
-- directory the user has since typed past contributes nothing (the @dir /= d@
-- test) instead of showing candidates from the wrong place.
pathCompletions :: MonadWidget t m
                => Dynamic t Text -> Dynamic t Text -> m (Dynamic t [(Text, Text)])
pathCompletions serverD valueD = do
    let requestD = ffor2 serverD valueD $ \s v -> (s, fst (completionSplit v))
    -- Only the DIRECTORY matters, so typing further into one directory does not
    -- re-list it; holdUniqDyn makes that a no-op rather than a debounce race.
    reqE <- debounce 0.15 . updated =<< holdUniqDyn requestD
    seqD <- count reqE
    (resultE, fireResult) <- newTriggerEvent
    performEvent_ $ ffor (attachWith (\n r -> (n + 1 :: Int, r)) (current seqD) reqE) $
        \(n, (server, dir)) -> liftIO . void . forkIO $
            listCompletions server dir >>= \names -> fireResult (n, dir, names)
    -- A result is stale if a newer request has gone out since it was issued.
    let freshE = attachWithMaybe
            (\latest (n, dir, names) ->
                if n < latest then Nothing else Just (dir, names))
            (current seqD) resultE
    listingD <- holdDyn ("", []) freshE
    pure $ ffor2 valueD listingD $ \v (listedDir, names) ->
        let (dir, leaf) = completionSplit v
        in if dir /= listedDir then []
           else [ (n, dir <> n)
                | n <- names
                , T.toLower leaf `T.isPrefixOf` T.toLower n ]

-- | One directory's immediate children, directories marked with a trailing
-- @\/@ so accepting one immediately lists ITS children.
--
-- Total by construction: a path that does not exist, is not a directory, or
-- cannot be read yields @[]@.  Runs on a forked thread only — remotely this is
-- an ssh round trip, and a hung one must not take the UI with it.
listCompletions :: Text -> Text -> IO [Text]
listCompletions server dir = (`catch` \(_ :: SomeException) -> pure []) $ do
    fp      <- resolveDir server dir
    entries <- fsListDirectory fp
    pure . map snd . sortOn fst $
        [ ((not isDir, T.toLower name), name)
        | (n, isDir) <- entries
        , let name = T.pack n <> (if isDir then "/" else "") ]

-- | Where a typed directory prefix actually points.  @~@ expands here and
-- nowhere else on the display side; remote paths become @ssh:\/\/host\/…@ so
-- 'fsListDirectory' routes them over ssh.
resolveDir :: Text -> Text -> IO FilePath
resolveDir server dir
  | server == localServer = case T.unpack dir of
      ""        -> getCurrentDirectory
      "~"       -> getHomeDirectory
      "~/"      -> getHomeDirectory
      '~':'/':r -> (</> r) <$> getHomeDirectory
      d         -> pure d
  | otherwise = do
      abs' <- case T.unpack dir of
        ""        -> remoteHomeDir server
        "~"       -> remoteHomeDir server
        "~/"      -> remoteHomeDir server
        '~':'/':r -> (</> r) <$> remoteHomeDir server
        d         -> pure d
      pure (renderRemotePath server abs')

-- | Suppress the browser's default handling of the popup's navigation keys.
--
-- It has to be JS: jsaddle-wkwebview dispatches events asynchronously, so a
-- 'preventDefault' from a Haskell handler lands too late — Tab would already
-- have moved focus out of the field and ↑\/↓ would have jumped the caret.
-- Evaluated once per window (see "IDE.Web.Main"), capture phase, and scoped to
-- our own inputs by class.
pathFieldKeysJs :: Text
pathFieldKeysJs =
    "document.addEventListener('keydown', function (e) {\
    \  var el = document.activeElement;\
    \  if (!el || !el.classList || !el.classList.contains('leksah-popup-field')) return;\
    \  if (e.key === 'Tab' || e.key === 'ArrowUp' || e.key === 'ArrowDown')\
    \    e.preventDefault();\
    \}, true);"
    <> popupAlignJs

-- | Put the drop-down's left edge under the text it would replace, so a
-- completion sits beneath the segment it completes rather than under the start
-- of the whole path.
--
-- It measures with a canvas rather than the DOM because the answer is wanted
-- for text that is not in the document (the typed directory prefix), and
-- because @measureText@ costs nothing next to a reflow.
--
-- The result is written as a custom property on the wrapper, NOT as the popup's
-- own @left@: reflex rewrites that element's @style@ attribute every time the
-- list opens or closes.  All the quantities involved (computed paddings,
-- @measureText@, @scrollLeft@) are pre-zoom \"local\" CSS px, as is the property
-- it sets, so this needs no zoom conversion (see @leksahLocal@ in
-- "IDE.Web.Main").
popupAlignJs :: Text
popupAlignJs =
    "window.leksahPathPopupAlign = function (id, prefix) {\
    \  var i = document.getElementById(id); if (!i) return;\
    \  var w = i.parentElement; if (!w) return;\
    \  if (!prefix) { w.style.removeProperty('--path-popup-x'); return; }\
    \  var p = w.querySelector('.path-popup'); if (!p) return;\
    \  var cs = getComputedStyle(i);\
    \  var font = cs.font || (cs.fontStyle+' '+cs.fontWeight+' '+cs.fontSize+' '+cs.fontFamily);\
    \  var c = window.__leksahPathCanvas ||\
    \          (window.__leksahPathCanvas = document.createElement('canvas'));\
    \  var g = c.getContext('2d'); if (!g) return;\
    \  g.font = font;\
    \  var row = p.querySelector('.path-popup-row');\
    \  var rowPad = row ? (parseFloat(getComputedStyle(row).paddingLeft) || 0) : 8;\
    \  var popBorder = parseFloat(getComputedStyle(p).borderLeftWidth) || 0;\
    \  var x = (parseFloat(cs.paddingLeft) || 0) + (parseFloat(cs.borderLeftWidth) || 0)\
    \        + g.measureText(prefix).width - (i.scrollLeft || 0)\
    \        - rowPad - popBorder;\
    \  var max = Math.max(0, i.clientWidth - 160);\
    \  w.style.setProperty('--path-popup-x',\
    \                      Math.round(Math.min(Math.max(x, 0), max)) + 'px');\
    \};"

-- | Quote a 'Text' as a JS string literal.  Paths are user data and reach
-- 'eval' verbatim, so this cannot be a bare @\"'\" <> t <> \"'\"@.
jsStr :: Text -> Text
jsStr t = "'" <> T.concatMap esc t <> "'"
  where
    esc '\\' = "\\\\"
    esc '\'' = "\\'"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc c    = T.singleton c

-- | Focus one of these fields by id, from a frame-thread event.  A fresh input
-- is not focused automatically, so without this the dialog opens with the
-- keyboard nowhere.
focusFieldJs :: MonadWidget t m => Text -> Event t a -> m ()
focusFieldJs fid e = performEvent_ $ ffor e $ \_ -> liftJSM . void $ eval
    ("var i=document.getElementById('" <> fid <> "'); if(i){i.focus();i.select();}")

fieldWrapStyle, fieldStyle, popupStyle, rowStyle, selectedRowStyle, arrowStyle :: Text
-- | Wraps the input, the ▾ and the list.  It is the containing block for the
-- arrow, which therefore has to be able to see the FIELD — see 'arrowStyle'.
fieldWrapStyle = "position:relative"
-- The right padding is the arrow's lane: without it a long value runs under
-- the ▾ instead of stopping beside it.
fieldStyle =
    "display:block;width:100%;box-sizing:border-box;margin:4px 0;\
    \padding:5px 30px 5px 8px"
-- @--path-popup-x@ is set by 'popupAlignJs' on the wrapper, so the list starts
-- under the text it would replace.  It has to be a custom property rather than
-- an inline @left@: reflex rewrites this element's whole @style@ attribute
-- whenever the list opens or closes, which would wipe a directly-set left.
popupStyle =
    "position:absolute;left:var(--path-popup-x,0px);right:0;top:100%;\
    \z-index:1001;max-height:220px;\
    \overflow-y:auto;background:var(--leksah-surface);\
    \border:1px solid var(--leksah-border-control);border-radius:4px;\
    \box-shadow:0 4px 16px var(--leksah-shadow-glow)"
rowStyle = "padding:3px 8px;cursor:pointer;white-space:nowrap;overflow:hidden;\
           \text-overflow:ellipsis"
selectedRowStyle = ";background:var(--leksah-accent);color:var(--leksah-on-accent)"
-- Vertically centred on the wrapper, which centres it on the input whether or
-- not the input's 4px margins collapse out of the wrapper (both cases put the
-- input's centre on the wrapper's centre).
arrowStyle =
    "position:absolute;right:2px;top:0;bottom:0;display:flex;align-items:center;\
    \padding:0 7px;border:none;line-height:1;font-size:15px;\
    \background:transparent;color:var(--leksah-fg-dim);cursor:pointer"
