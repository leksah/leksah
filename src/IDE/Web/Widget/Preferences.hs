{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The Preferences pane for the web UI: a scrollable, sectioned form over the
-- user 'Config'.  The sections mirror 'Config' one-to-one (editor \/ fonts \/
-- themes \/ ui \/ build \/ terminal \/ remote \/ lsp \/ ai).  Each control
-- reads the current value and emits a whole-@Config -> Config@ update; the
-- widget applies it to the config current at edit time and persists it with
-- 'saveConfig' (which also updates the config cell, so every window sees the
-- change).  Boolean\/enum controls are driven by the live 'Config' 'Dynamic'
-- (so they stay in sync with the toolbar toggles); text\/number\/list
-- controls seed from the value at open time.
module IDE.Web.Widget.Preferences
  ( preferencesWidget
  , preferencesCss
  ) where

import Control.Lens ((.~), (&))
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
        fmapMaybe, holdUniqDyn, sample, newTriggerEvent)
import Reflex.Dom.Core
       (DomBuilderSpace, EventResult, InputElement,
        TextAreaElement, AttributeName, elClass, elDynAttr, elDynAttr', text,
        (=:), domEvent, EventName(Click), blank, inputElement,
        _inputElement_value, inputElementConfig_initialValue,
        inputElementConfig_elementConfig, elementConfig_initialAttributes,
        textAreaElement, _textAreaElement_value,
        textAreaElementConfig_initialValue,
        textAreaElementConfig_elementConfig, dropdown, _dropdown_value)

import IDE.App (appConfig)
import IDE.Config
       (Config(..), Editor(..), EditorC(..), FontC(..), ThemeC(..),
        UiC(..), BuildC(..), TerminalC(..), RemoteC(..), LspC(..),
        AiC(..), currentConfig, saveConfig)
import IDE.Web.ColorPick (hasColorPickImpl, requestColorPick)
import IDE.Web.Ctx (Ctx(..))
import IDE.Web.Events (PreferencesEvents(..))
import IDE.Web.Frame (MonadWidget, performEvent_)

preferencesWidget
  :: forall t m. MonadWidget t m
  => Ctx t
  -> m (Event t PreferencesEvents)
preferencesWidget ctx = do
  let cfgD = cCfg ctx
  cfg0 <- sample (current cfgD)
  let b w = boolField w cfgD
      i w = intField w cfg0
      txt w = textField w cfg0
  updE <- fmap leftmost . elClass "div" "preferences" . elClass "div" "pref-cols" . fmap concat $ sequence
    [ section "Editor"
        [ editorField wired cfg0 "Editor"
        , b todo "Show line numbers" (ecLineNumbers . cfgEditor)
            (\v -> onEditor $ \s -> s { ecLineNumbers = v })
        , i todo "Right margin column (0 = off)" (ecRightMargin . cfgEditor)
            (\v -> onEditor $ \s -> s { ecRightMargin = v })
        , i todo "Tab width" (ecTabWidth . cfgEditor)
            (\v -> onEditor $ \s -> s { ecTabWidth = v })
        , b todo "Wrap lines" (ecWrapLines . cfgEditor)
            (\v -> onEditor $ \s -> s { ecWrapLines = v })
        , b todo "Use standard line ends even on Windows" (ecFixLineEnds . cfgEditor)
            (\v -> onEditor $ \s -> s { ecFixLineEnds = v })
        , b todo "Remove trailing blanks when saving" (ecStripBlanks . cfgEditor)
            (\v -> onEditor $ \s -> s { ecStripBlanks = v })
        , b todo "Automatically load files modified outside Leksah" (ecAutoReload . cfgEditor)
            (\v -> onEditor $ \s -> s { ecAutoReload = v })
        ]
    , section "Fonts"
        [ txt wired "Monospace font family (editor, terminals, log)"
            (fcMonoFamily . cfgFont) (\v -> onFont $ \s -> s { fcMonoFamily = v })
        , i wired "Monospace font size (px)"
            (fcMonoSize . cfgFont) (\v -> onFont $ \s -> s { fcMonoSize = v })
        ]
    , section "Themes (auto-switch with the OS light/dark setting)"
        [ enumField wired cfg0 "Monaco editor theme — dark" monacoThemeOpts
            (thMonacoDark . cfgTheme) (\v -> onTheme $ \s -> s { thMonacoDark = v })
        , enumField wired cfg0 "Monaco editor theme — light" monacoThemeOpts
            (thMonacoLight . cfgTheme) (\v -> onTheme $ \s -> s { thMonacoLight = v })
        , enumField wired cfg0 "CodeMirror editor theme — dark" cmThemeOpts
            (thCodeMirrorDark . cfgTheme) (\v -> onTheme $ \s -> s { thCodeMirrorDark = v })
        , enumField wired cfg0 "CodeMirror editor theme — light" cmThemeOpts
            (thCodeMirrorLight . cfgTheme) (\v -> onTheme $ \s -> s { thCodeMirrorLight = v })
        , enumField wired cfg0 "Terminal (xterm.js) theme — dark" xtermThemeOpts
            (thXtermDark . cfgTheme) (\v -> onTheme $ \s -> s { thXtermDark = v })
        , enumField wired cfg0 "Terminal (xterm.js) theme — light" xtermThemeOpts
            (thXtermLight . cfgTheme) (\v -> onTheme $ \s -> s { thXtermLight = v })
        , colorField wired cfgD cfg0 "Selection highlight colour"
            (thSelectionColor . cfgTheme) (\v -> onTheme $ \s -> s { thSelectionColor = v })
        , colorField wired cfgD cfg0 "Run-button hover row colour"
            (thHoverColor . cfgTheme) (\v -> onTheme $ \s -> s { thHoverColor = v })
        ]
    , section "User Interface"
        [ b wired "Show hidden files in the workspace" (uiShowHiddenFiles . cfgUi)
            (\v -> onUi $ \s -> s { uiShowHiddenFiles = v })
        , b wired "Show ignored files in the workspace" (uiShowIgnoredFiles . cfgUi)
            (\v -> onUi $ \s -> s { uiShowIgnoredFiles = v })
        , b todo "Show icons in the Workspace pane" (uiWorkspaceIcons . cfgUi)
            (\v -> onUi $ \s -> s { uiWorkspaceIcons = v })
        , b wired "Colourful icons" (uiColorfulIcons . cfgUi)
            (\v -> onUi $ \s -> s { uiColorfulIcons = v })
        , b todo "Collapse errors in the Errors pane by default" (uiCollapseErrors . cfgUi)
            (\v -> onUi $ \s -> s { uiCollapseErrors = v })
        , b todo "Save the session before closing a workspace" (uiSaveSession . cfgUi)
            (\v -> onUi $ \s -> s { uiSaveSession = v })
        , b wired "Show navigation shortcut badges while Cmd is held" (uiShortcutBadges . cfgUi)
            (\v -> onUi $ \s -> s { uiShortcutBadges = v })
        ]
    , section "Build"
        [ b wired "Save all files before building" (bcSaveAllFirst . cfgBuild)
            (\v -> onBuild $ \s -> s { bcSaveAllFirst = v })
        , b wired "Run linter after save" (bcLintOnSave . cfgBuild)
            (\v -> onBuild $ \s -> s { bcLintOnSave = v })
        , b wired "Select first warning if built without errors" (bcJumpToWarnings . cfgBuild)
            (\v -> onBuild $ \s -> s { bcJumpToWarnings = v })
        , b wired "Background build" (bcBackground . cfgBuild)
            (\v -> onBuild $ \s -> s { bcBackground = v })
        , b wired "Native" (bcNative . cfgBuild)
            (\v -> onBuild $ \s -> s { bcNative = v })
        , b wired "JavaScript" (bcJavaScript . cfgBuild)
            (\v -> onBuild $ \s -> s { bcJavaScript = v })
        , b wired "Interpreted (ghci) mode: build through cached repls" (bcGhci . cfgBuild)
            (\v -> onBuild $ \s -> s { bcGhci = v })
        , b wired "Make documentation when building" (bcDocs . cfgBuild)
            (\v -> onBuild $ \s -> s { bcDocs = v })
        , b wired "Run unit tests when building" (bcTests . cfgBuild)
            (\v -> onBuild $ \s -> s { bcTests = v })
        , b wired "Run benchmarks when building" (bcBenchmarks . cfgBuild)
            (\v -> onBuild $ \s -> s { bcBenchmarks = v })
        , b wired "Make mode" (bcMakeMode . cfgBuild)
            (\v -> onBuild $ \s -> s { bcMakeMode = v })
        ]
    , section "Terminal"
        [ b wired "Clickable file paths and identifiers in terminal output"
            (tcFileLinks . cfgTerminal) (\v -> onTerminal $ \s -> s { tcFileLinks = v })
        , b wired "Use tmux control mode (-CC): native pane splits (new terminals)"
            (tcControlMode . cfgTerminal) (\v -> onTerminal $ \s -> s { tcControlMode = v })
        , b wired "Intercept the tmux prefix (C-b) in terminals"
            (tcTmuxPrefix . cfgTerminal) (\v -> onTerminal $ \s -> s { tcTmuxPrefix = v })
        ]
    , section "Remote"
        [ linesField wired cfg0 "Remote hosts in the Terminals tree (ssh, one per line)"
            (rcHosts . cfgRemote) (\v -> onRemote $ \s -> s { rcHosts = v })
        ]
    , section "Language Server (LSP)"
        [ b wired "Enable language server (diagnostics, hover, completion, F12 navigation)"
            (lcEnabled . cfgLsp) (\v -> onLsp $ \s -> s { lcEnabled = v })
        , txt wired "Server command (blank = per-language default)"
            (lcServerCommand . cfgLsp) (\v -> onLsp $ \s -> s { lcServerCommand = v })
        ]
    , section "AI"
          -- The AI tools ask which session to use (the active pane's default
          -- first); this is only the explicit target for
          -- `leksah-cmd grab-region TARGET`, which names a pane itself.
        [ txt wired "Fallback AI target pane (session/window/pane)"
            (acCaptureTarget . cfgAi) (\v -> onAi $ \s -> s { acCaptureTarget = v })
        ]
    ]
  -- Apply each edit to the config current at edit time and persist it;
  -- 'saveConfig' also writes the config cell, so the change fans out to
  -- every window (including this pane's own live-driven controls).
  performEvent_ $ ffor updE $ \f -> liftIO $ do
    cfg <- currentConfig (appConfig (cApp ctx))
    saveConfig (appConfig (cApp ctx)) (f cfg)
  return (PrefsUpdate <$> updE)
  where
    section :: Text -> [m (Event t (Config -> Config))] -> m [Event t (Config -> Config)]
    section title fields = elClass "div" "pref-group" $ do
      elClass "div" "pref-section-title" $ text title
      elClass "div" "pref-section" $ sequence fields

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

-- Lift a per-section update to the whole 'Config' (the record-update
-- counterpart of a section lens).
onEditor   :: (EditorC -> EditorC)     -> Config -> Config
onEditor   f c = c { cfgEditor   = f (cfgEditor c) }
onFont     :: (FontC -> FontC)         -> Config -> Config
onFont     f c = c { cfgFont     = f (cfgFont c) }
onTheme    :: (ThemeC -> ThemeC)       -> Config -> Config
onTheme    f c = c { cfgTheme    = f (cfgTheme c) }
onUi       :: (UiC -> UiC)             -> Config -> Config
onUi       f c = c { cfgUi       = f (cfgUi c) }
onBuild    :: (BuildC -> BuildC)       -> Config -> Config
onBuild    f c = c { cfgBuild    = f (cfgBuild c) }
onTerminal :: (TerminalC -> TerminalC) -> Config -> Config
onTerminal f c = c { cfgTerminal = f (cfgTerminal c) }
onRemote   :: (RemoteC -> RemoteC)     -> Config -> Config
onRemote   f c = c { cfgRemote   = f (cfgRemote c) }
onLsp      :: (LspC -> LspC)           -> Config -> Config
onLsp      f c = c { cfgLsp      = f (cfgLsp c) }
onAi       :: (AiC -> AiC)             -> Config -> Config
onAi       f c = c { cfgAi       = f (cfgAi c) }

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

-- | A checkbox driven by the live config (stays in sync with toolbar toggles).
boolField
  :: MonadWidget t m
  => Wired -> Dynamic t Config -> Text -> (Config -> Bool) -> (Bool -> Config -> Config)
  -> m (Event t (Config -> Config))
boolField w cfgD lbl get set = prefRow w lbl $ do
  let stD = get <$> cfgD
  (e, _) <- elDynAttr' "span" (ffor stD $ \v -> "class" =: ("pref-check" <> bool "" " on" v)) blank
  return $ ffor (tag (current stD) (domEvent Click e)) $ \cur -> set (not cur)

-- | An integer field.
intField
  :: MonadWidget t m
  => Wired -> Config -> Text -> (Config -> Int) -> (Int -> Config -> Config)
  -> m (Event t (Config -> Config))
intField w cfg0 lbl get set = prefRow w lbl $ do
  inp <- numberInput (show (get cfg0))
  return $ fmapMaybe (fmap set . readMaybe . T.unpack) (updated (_inputElement_value inp))

-- | A text field.
textField
  :: MonadWidget t m
  => Wired -> Config -> Text -> (Config -> Text) -> (Text -> Config -> Config)
  -> m (Event t (Config -> Config))
textField w cfg0 lbl get set = prefRow w lbl $ do
  inp <- textInputAttrs (get cfg0) ("type" =: "text" <> "class" =: "pref-input")
  return $ set <$> updated (_inputElement_value inp)

-- | The 'Editor' choice: one row for the whole ADT — a dropdown picking
-- Monaco \/ CodeMirror \/ external, plus a command text field shown only
-- when \"external\" is selected (the file path is appended to the command).
-- Applies to tabs opened from now on (like the terminals' control-mode pref).
editorField
  :: MonadWidget t m
  => Wired -> Config -> Text
  -> m (Event t (Config -> Config))
editorField w cfg0 lbl = prefRow w lbl $ do
  let ed0  = ecEditor (cfgEditor cfg0)
      i0   = case ed0 of
               EditorMonaco     -> 0 :: Int
               EditorCodeMirror -> 1
               EditorExternal _ -> 2
      cmd0 = case ed0 of
               EditorExternal c -> c
               _                -> ""
      om   = M.fromList
               [ (0 :: Int, "Monaco (VS Code, default)")
               , (1,        "CodeMirror 6")
               , (2,        "External command") ] :: Map Int Text
  dd <- dropdown i0 (constDyn om) def
  inp <- elDynAttr "span"
           (ffor (_dropdown_value dd) $ \sel ->
             if sel == 2 then mempty else "style" =: "display:none") $
    textInputAttrs cmd0
      ("type" =: "text" <> "class" =: "pref-input"
       <> "placeholder" =: "command — the file path is appended")
  edD <- holdUniqDyn $ mk <$> _dropdown_value dd <*> _inputElement_value inp
  return $ ffor (updated edD) $ \v -> onEditor $ \s -> s { ecEditor = v }
  where
    mk :: Int -> Text -> Editor
    mk 1 _ = EditorCodeMirror
    mk 2 c = EditorExternal c
    mk _ _ = EditorMonaco

-- | A colour picker; the value is a @#rrggbb@ string.  Changes apply live —
-- the colour settings feed the CSS variables 'IDE.Web.Main' binds in a dynamic
-- style element.  Where a native picker is registered (wkwebview:
-- NSColorPanel) the control is a swatch that opens it — the web colour
-- input's popover mis-anchors in the transparent-titlebar window — and the
-- panel streams changes back while it's open; other front ends fall back to
-- @\<input type="color"\>@.
colorField
  :: MonadWidget t m
  => Wired -> Dynamic t Config -> Config -> Text -> (Config -> Text) -> (Text -> Config -> Config)
  -> m (Event t (Config -> Config))
colorField w cfgD cfg0 lbl get set = prefRow w lbl $ do
  useNative <- liftIO hasColorPickImpl
  if useNative
    then do
      (pickedE, firePicked) <- newTriggerEvent
      let curD = get <$> cfgD
      (e, _) <- elDynAttr' "button"
          (ffor curD $ \c -> "type" =: "button" <> "class" =: "pref-swatch"
                          <> "style" =: ("background-color: " <> c)
                          <> "title" =: c) blank
      performEvent_ $ ffor (tag (current curD) (domEvent Click e)) $ \c ->
          liftIO . void $ requestColorPick c firePicked
      return $ set <$> pickedE
    else do
      inp <- textInputAttrs (get cfg0) ("type" =: "color" <> "class" =: "pref-color")
      return $ set <$> updated (_inputElement_value inp)

-- | A multi-line list, one entry per line.
linesField
  :: MonadWidget t m
  => Wired -> Config -> Text -> (Config -> [Text]) -> ([Text] -> Config -> Config)
  -> m (Event t (Config -> Config))
linesField w cfg0 lbl get set = prefRow w lbl $ do
  ta <- textArea (T.unlines (get cfg0))
  return $ ffor (updated (_textAreaElement_value ta)) $ \t ->
    set [ l | l <- map T.strip (T.lines t), not (T.null l) ]

-- | A dropdown over a labelled option list, seeded from the current value.
enumField
  :: forall t m a. (MonadWidget t m, Eq a)
  => Wired -> Config -> Text -> [(Text, a)] -> (Config -> a) -> (a -> Config -> Config)
  -> m (Event t (Config -> Config))
enumField w cfg0 lbl opts get set = prefRow w lbl $ do
  let vals = map snd opts
      i0   = fromMaybe 0 (elemIndex (get cfg0) vals)
      om   = M.fromList (zip [0 :: Int ..] (map fst opts)) :: Map Int Text
  dd <- dropdown i0 (constDyn om) def
  return $ fmapMaybe (\sel -> set <$> lookup sel (zip [0 ..] vals)) (updated (_dropdown_value dd))

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
