{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The Preferences pane for the web UI: a scrollable, sectioned form over the
-- portable 'Prefs' fields.  Each control reads the current value and emits a
-- @Prefs -> Prefs@ update; 'IDE.Web.Main' applies it with @modifyIDE_ (prefs %~)@,
-- and the existing debounced writer persists it.  Boolean/enum controls are
-- driven by the live IDE 'Prefs' 'Dynamic' (so they stay in sync with the toolbar
-- toggles); text/number/list controls seed from the value at open time.
module IDE.Web.Widget.Preferences
  ( preferencesWidget
  , preferencesCss
  ) where

import Control.Lens ((.~), (&), view)
import Data.Bool (bool)
import Data.Default (def)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, lines, unlines, null, strip)
import Text.Read (readMaybe)


import Clay ((?), (-:), Css)

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Reflex
       (Dynamic, Event, constDyn, current, updated, tag, leftmost, ffor,
        fmapMaybe, sample, newTriggerEvent, performEvent_)
import Reflex.Dom.Core
       (MonadWidget, DomBuilderSpace, EventResult, InputElement,
        TextAreaElement, AttributeName, elClass, elDynAttr', text, (=:), domEvent,
        EventName(Click), blank, inputElement, _inputElement_value,
        inputElementConfig_initialValue, inputElementConfig_elementConfig,
        elementConfig_initialAttributes, textAreaElement,
        _textAreaElement_value, textAreaElementConfig_initialValue,
        textAreaElementConfig_elementConfig, dropdown, _dropdown_value)

import IDE.Core.State (IDE, Prefs(..), prefs, EditorChoice(..))
import IDE.Web.ColorPick (hasColorPickImpl, requestColorPick)
import IDE.Web.Events (PreferencesEvents(..))

preferencesWidget
  :: forall t m. MonadWidget t m
  => Dynamic t IDE
  -> m (Event t PreferencesEvents)
preferencesWidget ide = do
  ide0 <- sample (current ide)
  let p0     = view prefs ide0
      prefsD = view prefs <$> ide
      b w = boolField w prefsD
      i w = intField w p0
      txt w = textField w p0
  fmap (fmap PrefsUpdate . leftmost) . elClass "div" "preferences" . elClass "div" "pref-cols" . fmap concat $ sequence
    [ section "Editor"
        [ b todo "Show line numbers" showLineNumbers (\v p -> p { showLineNumbers = v })
        , i todo "Right margin column (0 = off)" rightMargin (\v p -> p { rightMargin = v })
        , i todo "Tab width" tabWidth (\v p -> p { tabWidth = v })
        , b todo "Wrap lines" wrapLines (\v p -> p { wrapLines = v })
        , b todo "Use standard line ends even on Windows" forceLineEnds (\v p -> p { forceLineEnds = v })
        , b todo "Remove trailing blanks when saving" removeTBlanks (\v p -> p { removeTBlanks = v })
        , b todo "Automatically load files modified outside Leksah" autoLoad (\v p -> p { autoLoad = v })
        , enumField wired p0 "Editor" editorOptions editorChoice (\v p -> p { editorChoice = v })
        ]
    , section "Fonts"
        [ txt wired "Monospace font family (editor, terminals, log)"
            monospaceFont (\v p -> p { monospaceFont = v })
        , i wired "Monospace font size (px)"
            monospaceFontSize (\v p -> p { monospaceFontSize = v })
        ]
    , section "Themes (auto-switch with the OS light/dark setting)"
        [ enumField wired p0 "Monaco editor theme — dark" monacoThemeOpts
            monacoThemeDark (\v p -> p { monacoThemeDark = v })
        , enumField wired p0 "Monaco editor theme — light" monacoThemeOpts
            monacoThemeLight (\v p -> p { monacoThemeLight = v })
        , enumField wired p0 "CodeMirror editor theme — dark" cmThemeOpts
            codeMirrorThemeDark (\v p -> p { codeMirrorThemeDark = v })
        , enumField wired p0 "CodeMirror editor theme — light" cmThemeOpts
            codeMirrorThemeLight (\v p -> p { codeMirrorThemeLight = v })
        , enumField wired p0 "Terminal (xterm.js) theme — dark" xtermThemeOpts
            xtermThemeDark (\v p -> p { xtermThemeDark = v })
        , enumField wired p0 "Terminal (xterm.js) theme — light" xtermThemeOpts
            xtermThemeLight (\v p -> p { xtermThemeLight = v })
        ]
    , section "Language Server (LSP)"
        [ b wired "Enable language server (diagnostics, hover, completion, F12 navigation)"
            lspEnabled (\v p -> p { lspEnabled = v })
        , txt wired "Server command (blank = haskell-language-server --lsp)"
            lspServerCommand (\v p -> p { lspServerCommand = v })
        ]
    , section "User Interface"
        [ b wired "Show hidden files in the workspace" showHiddenFiles (\v p -> p { showHiddenFiles = v })
        , b wired "Show ignored files in the workspace" showIgnoredFiles (\v p -> p { showIgnoredFiles = v })
        , b todo "Show icons in the Workspace pane" showWorkspaceIcons (\v p -> p { showWorkspaceIcons = v })
        , b todo "Collapse errors in the Errors pane by default" collapseErrors (\v p -> p { collapseErrors = v })
        , b todo "Save the session before closing a workspace" saveSessionOnClose (\v p -> p { saveSessionOnClose = v })
        , colorField wired prefsD p0 "Selection highlight colour" uiSelectionColor (\v p -> p { uiSelectionColor = v })
        , colorField wired prefsD p0 "Run-button hover row colour" uiHoverColor (\v p -> p { uiHoverColor = v })
        , b wired "Show navigation shortcut badges while Cmd is held" showShortcutBadges (\v p -> p { showShortcutBadges = v })
        , b wired "Colourful icons" colorfulIcons (\v p -> p { colorfulIcons = v })
        ]
    , section "Terminal"
        [ b wired "Clickable file paths and identifiers in terminal output"
            terminalFileLinks (\v p -> p { terminalFileLinks = v })
        , b wired "Use tmux control mode (-CC): native pane splits (new terminals)"
            terminalControlMode (\v p -> p { terminalControlMode = v })
        , b wired "Intercept the tmux prefix (C-b) in terminals"
            tmuxInterceptPrefix (\v p -> p { tmuxInterceptPrefix = v })
        , linesField wired p0 "Remote hosts in the Terminals tree (ssh, one per line)"
            (map T.unpack . remoteHosts) (\v p -> p { remoteHosts = map T.pack v })
          -- The AI tools ask which session to use (the active pane's default
          -- first); this is only the explicit target for
          -- `leksah-cmd grab-region TARGET`, which names a pane itself.
        , txt wired "Fallback AI target pane (session/window/pane)"
            regionCaptureTarget (\v p -> p { regionCaptureTarget = v })
        ]
    , section "Build"
        [ b wired "Save all files before building" saveAllBeforeBuild (\v p -> p { saveAllBeforeBuild = v })
        , b wired "Run HLint when saving a source file" hlintOnSave (\v p -> p { hlintOnSave = v })
        , b wired "Select first warning if built without errors" jumpToWarnings (\v p -> p { jumpToWarnings = v })
        , b wired "Background build" backgroundBuild (\v p -> p { backgroundBuild = v })
        , b wired "Native" native (\v p -> p { native = v })
        , b wired "JavaScript" javaScript (\v p -> p { javaScript = v })
        , b wired "Interpreted (ghci) mode: build through cached repls" debug (\v p -> p { debug = v })
        , b wired "Make documentation when building" makeDocs (\v p -> p { makeDocs = v })
        , b wired "Run unit tests when building" runUnitTests (\v p -> p { runUnitTests = v })
        , b wired "Run benchmarks when building" runBenchmarks (\v p -> p { runBenchmarks = v })
        , b wired "Make mode" makeMode (\v p -> p { makeMode = v })
        ]
    ]
  where
    section :: Text -> [m (Event t (Prefs -> Prefs))] -> m [Event t (Prefs -> Prefs)]
    section title fields = elClass "div" "pref-group" $ do
      elClass "div" "pref-section-title" $ text title
      elClass "div" "pref-section" $ sequence fields

    -- Applies to tabs opened from now on (like the terminals' control-mode
    -- pref); nano/vim/emacs open files in the file's backing tmux pane.
    editorOptions =
      [ ("Monaco (VS Code, default)", EditorMonaco)
      , ("CodeMirror 6",              EditorCodeMirror)
      , ("nano (in terminal pane)",   EditorNano)
      , ("vim (in terminal pane)",    EditorVim)
      , ("emacs (in terminal pane)",  EditorEmacs) ]
    -- Theme options — the values are the ids the JS bundles / xterm palette
    -- table understand (see IDE.Web.Main's themeSwitchJs).  Each dropdown offers
    -- both light and dark themes; leksah picks the light or dark selection based
    -- on the OS appearance.
    monacoThemeOpts =
      [ ("GitHub Dark",          "leksah-github-dark")
      , ("GitHub Light",         "leksah-github-light")
      , ("VS Dark",              "vs-dark")
      , ("VS Light",             "vs")
      , ("High Contrast Dark",   "hc-black")
      , ("High Contrast Light",  "hc-light") ] :: [(Text, Text)]
    cmThemeOpts =
      [ ("GitHub Dark",  "github-dark")
      , ("GitHub Light", "github-light") ] :: [(Text, Text)]
    xtermThemeOpts =
      [ ("Dark",            "leksah-dark")
      , ("Light",           "leksah-light")
      , ("Solarized Dark",  "solarized-dark")
      , ("Solarized Light", "solarized-light") ] :: [(Text, Text)]

-- | Is a pref actually honoured by the web UI yet?  'todo' rows are stored
-- and written to the settings file but not yet acted on, and get a grey
-- "(TODO)" after the label.
type Wired = Bool

wired, todo :: Wired
wired = True
todo  = False

-- | A labelled row: label on the left, control on the right.
prefRow :: MonadWidget t m => Wired -> Text -> m a -> m a
prefRow w lbl inner = elClass "div" "pref-row" $ do
  elClass "label" "pref-label" $ do
    text lbl
    if w
      then blank
      else elClass "span" "pref-todo" $ text " (TODO)"
  elClass "div" "pref-control" inner

-- | A checkbox driven by the live prefs (stays in sync with toolbar toggles).
boolField
  :: MonadWidget t m
  => Wired -> Dynamic t Prefs -> Text -> (Prefs -> Bool) -> (Bool -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
boolField w prefsD lbl get set = prefRow w lbl $ do
  let stD = get <$> prefsD
  (e, _) <- elDynAttr' "span" (ffor stD $ \v -> "class" =: ("pref-check" <> bool "" " on" v)) blank
  return $ ffor (tag (current stD) (domEvent Click e)) $ \cur -> set (not cur)

-- | An integer field.
intField
  :: MonadWidget t m
  => Wired -> Prefs -> Text -> (Prefs -> Int) -> (Int -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
intField w p0 lbl get set = prefRow w lbl $ do
  inp <- numberInput (show (get p0))
  return $ fmapMaybe (fmap set . readMaybe . T.unpack) (updated (_inputElement_value inp))

-- | A text field.
textField
  :: MonadWidget t m
  => Wired -> Prefs -> Text -> (Prefs -> Text) -> (Text -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
textField w p0 lbl get set = prefRow w lbl $ do
  inp <- textInputAttrs (get p0) ("type" =: "text" <> "class" =: "pref-input")
  return $ set <$> updated (_inputElement_value inp)

-- | A colour picker; the value is a @#rrggbb@ string.  Changes apply live —
-- the colour prefs feed the CSS variables 'IDE.Web.Main' binds in a dynamic
-- style element.  Where a native picker is registered (wkwebview:
-- NSColorPanel) the control is a swatch that opens it — the web colour
-- input's popover mis-anchors in the transparent-titlebar window — and the
-- panel streams changes back while it's open; other front ends fall back to
-- @\<input type="color"\>@.
colorField
  :: MonadWidget t m
  => Wired -> Dynamic t Prefs -> Prefs -> Text -> (Prefs -> Text) -> (Text -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
colorField w prefsD p0 lbl get set = prefRow w lbl $ do
  useNative <- liftIO hasColorPickImpl
  if useNative
    then do
      (pickedE, firePicked) <- newTriggerEvent
      let curD = get <$> prefsD
      (e, _) <- elDynAttr' "button"
          (ffor curD $ \c -> "type" =: "button" <> "class" =: "pref-swatch"
                          <> "style" =: ("background-color: " <> c)
                          <> "title" =: c) blank
      performEvent_ $ ffor (tag (current curD) (domEvent Click e)) $ \c ->
          liftIO . void $ requestColorPick c firePicked
      return $ set <$> pickedE
    else do
      inp <- textInputAttrs (get p0) ("type" =: "color" <> "class" =: "pref-color")
      return $ set <$> updated (_inputElement_value inp)

-- | A multi-line list of paths, one per line.
linesField
  :: MonadWidget t m
  => Wired -> Prefs -> Text -> (Prefs -> [FilePath]) -> ([FilePath] -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
linesField w p0 lbl get set = prefRow w lbl $ do
  ta <- textArea (T.unlines (map T.pack (get p0)))
  return $ ffor (updated (_textAreaElement_value ta)) $ \t ->
    set [ T.unpack l | l <- map T.strip (T.lines t), not (T.null l) ]

-- | A dropdown over a labelled option list, seeded from the current value.
enumField
  :: forall t m a. (MonadWidget t m, Eq a)
  => Wired -> Prefs -> Text -> [(Text, a)] -> (Prefs -> a) -> (a -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
enumField w p0 lbl opts get set = prefRow w lbl $ do
  let vals = map snd opts
      i0   = fromMaybe 0 (elemIndex (get p0) vals)
      om   = M.fromList (zip [0 :: Int ..] (map fst opts)) :: Map Int Text
  dd <- dropdown i0 (constDyn om) def
  return $ fmapMaybe (\i -> set <$> lookup i (zip [0 ..] vals)) (updated (_dropdown_value dd))

numberInput :: MonadWidget t m => String -> m (InputElement EventResult (DomBuilderSpace m) t)
numberInput v = textInputAttrs (T.pack v) ("type" =: "number" <> "class" =: "pref-input")

textInputAttrs
  :: MonadWidget t m => Text -> Map AttributeName Text
  -> m (InputElement EventResult (DomBuilderSpace m) t)
textInputAttrs v attrs = inputElement $ def
  & inputElementConfig_initialValue .~ v
  & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs

textArea :: MonadWidget t m => Text -> m (TextAreaElement EventResult (DomBuilderSpace m) t)
textArea v = textAreaElement $ def
  & textAreaElementConfig_initialValue .~ v
  & textAreaElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: "pref-textarea")

preferencesCss :: Css
preferencesCss = do
  ".preferences" ? do
    "height" -: "100%"
    "box-sizing" -: "border-box"   -- include padding, so the last rows aren't clipped
    "overflow" -: "auto"
    "padding" -: "10px 18px 24px 18px"
    "color" -: "var(--leksah-fg-muted)"
    "background" -: "var(--leksah-bg-sunken)"
    "font-size" -: "13px"
  -- Lay the sections out in as many ~480px columns as the pane is wide enough for
  -- (so a wide window gets two columns), scrolling vertically in .preferences.
  ".preferences .pref-cols" ? do
    "column-width" -: "480px"
    "column-gap" -: "36px"   -- as many ~480px columns as the pane is wide enough for
  ".preferences .pref-group" ? do
    -- Keep a whole section (title + rows) together in one column.
    "break-inside" -: "avoid"
    "-webkit-column-break-inside" -: "avoid"
    "display" -: "inline-block"
    "width" -: "100%"
  ".preferences .pref-section-title" ? do
    "font-size" -: "15px"
    "font-weight" -: "bold"
    "color" -: "var(--leksah-fg)"
    "margin" -: "18px 0 6px 0"
    "border-bottom" -: "1px solid var(--leksah-border-control)"
    "padding-bottom" -: "3px"
  ".preferences .pref-row" ? do
    "display" -: "flex"
    "flex-wrap" -: "wrap"
    "align-items" -: "flex-start"
    "padding" -: "3px 0"
  ".preferences .pref-label" ? do
    "flex" -: "0 1 240px"
    "min-width" -: "120px"
    "padding-top" -: "2px"
  ".preferences .pref-todo" ? do
    "color" -: "var(--leksah-fg-dim)"
  ".preferences .pref-control" ? do
    "flex" -: "1 1 200px"
    "min-width" -: "0"
    "display" -: "flex"
    "align-items" -: "center"
  ".preferences .pref-check" ? do
    "display" -: "inline-block"
    "width" -: "14px"
    "height" -: "14px"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "3px"
    "background" -: "var(--leksah-surface)"
    "cursor" -: "pointer"
  ".preferences .pref-check.on" ? do
    "background" -: "var(--leksah-selection)"
    "border-color" -: "var(--leksah-selection)"
  -- The colour-pref swatch: shows the current value; click opens the native
  -- colour panel.
  ".preferences .pref-swatch" ? do
    "display" -: "inline-block"
    "width" -: "44px"
    "height" -: "18px"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "3px"
    "padding" -: "0"
    "cursor" -: "pointer"
  ".preferences .pref-input" ? do
    "background" -: "var(--leksah-surface)"
    "color" -: "var(--leksah-fg-muted)"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "3px"
    "padding" -: "2px 6px"
    "box-sizing" -: "border-box"
    "min-width" -: "0"
    "width" -: "100%"
    "max-width" -: "260px"
  ".preferences .pref-textarea" ? do
    "background" -: "var(--leksah-surface)"
    "color" -: "var(--leksah-fg-muted)"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "3px"
    "padding" -: "4px 6px"
    "box-sizing" -: "border-box"
    "width" -: "100%"
    "max-width" -: "360px"
    "height" -: "80px"
    "font-family" -: "Hasklig, Menlo, monospace"
  ".preferences select" ? do
    "background" -: "var(--leksah-surface)"
    "color" -: "var(--leksah-fg-muted)"
    "border" -: "1px solid var(--leksah-border-control)"
    "border-radius" -: "3px"
    "padding" -: "2px 6px"
