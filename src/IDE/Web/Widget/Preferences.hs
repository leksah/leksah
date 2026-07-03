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
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M (fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, lines, unlines, null, strip)
import Text.Read (readMaybe)

import Distribution.Pretty (prettyShow)
import Distribution.Parsec (simpleParsec)
import Distribution.Types.Dependency (Dependency)

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

import IDE.Core.State (IDE, Prefs(..), prefs, TallVisibility(..))
import IDE.Core.CTypes (RetrieveStrategy(..))
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
      b   = boolField prefsD
      i   = intField p0
      txt = textField p0
  fmap (fmap PrefsUpdate . leftmost) . elClass "div" "preferences" . elClass "div" "pref-cols" . fmap concat $ sequence
    [ section "Editor"
        [ b "Show line numbers" showLineNumbers (\v p -> p { showLineNumbers = v })
        , boolIntField prefsD p0 "Right margin (show / column)"
            rightMargin (\v p -> p { rightMargin = v })
        , i "Tab width" tabWidth (\v p -> p { tabWidth = v })
        , b "Wrap lines" wrapLines (\v p -> p { wrapLines = v })
        , b "Use standard line ends even on Windows" forceLineEnds (\v p -> p { forceLineEnds = v })
        , b "Remove trailing blanks when saving" removeTBlanks (\v p -> p { removeTBlanks = v })
        , b "Automatically load files modified outside Leksah" autoLoad (\v p -> p { autoLoad = v })
        , txt "External editor command (blank = built-in editor)" externalEditor (\v p -> p { externalEditor = v })
        ]
    , section "User Interface"
        [ b "Show hidden files in the workspace" showHiddenFiles (\v p -> p { showHiddenFiles = v })
        , b "Show ignored files in the workspace" showIgnoredFiles (\v p -> p { showIgnoredFiles = v })
        , b "Show icons in the Workspace pane" showWorkspaceIcons (\v p -> p { showWorkspaceIcons = v })
        , b "Collapse errors in the Errors pane by default" collapseErrors (\v p -> p { collapseErrors = v })
        , b "Use Ctrl-Tab for the flipper" useCtrlTabFlipping (\v p -> p { useCtrlTabFlipping = v })
        , b "Complete only on hotkey" completeRestricted (\v p -> p { completeRestricted = v })
        , b "Save the session before closing a workspace" saveSessionOnClose (\v p -> p { saveSessionOnClose = v })
        , enumField p0 "Side bar visibility" tallVisOptions tallVisibility (\v p -> p { tallVisibility = v })
        , enumField p0 "Bottom bar visibility" tallVisOptions wide1Visibility (\v p -> p { wide1Visibility = v })
        , colorField prefsD p0 "Selection highlight colour" uiSelectionColor (\v p -> p { uiSelectionColor = v })
        , colorField prefsD p0 "Run-button hover row colour" uiHoverColor (\v p -> p { uiHoverColor = v })
        ]
    , section "Terminal"
        [ b "Clickable file paths and identifiers in terminal output"
            terminalFileLinks (\v p -> p { terminalFileLinks = v })
        , b "Use tmux control mode (-CC): native pane splits (new terminals)"
            terminalControlMode (\v p -> p { terminalControlMode = v })
        , linesField p0 "Remote hosts in the Terminals tree (ssh, one per line)"
            (map T.unpack . remoteHosts) (\v p -> p { remoteHosts = map T.pack v })
        ]
    , section "Build"
        [ b "Save all files before building" saveAllBeforeBuild (\v p -> p { saveAllBeforeBuild = v })
        , b "Run HLint when saving a source file" hlintOnSave (\v p -> p { hlintOnSave = v })
        , b "Select first warning if built without errors" jumpToWarnings (\v p -> p { jumpToWarnings = v })
        , b "Background build" backgroundBuild (\v p -> p { backgroundBuild = v })
        , b "Native" native (\v p -> p { native = v })
        , b "JavaScript" javaScript (\v p -> p { javaScript = v })
        , b "Debug" debug (\v p -> p { debug = v })
        , b "Make documentation when building" makeDocs (\v p -> p { makeDocs = v })
        , b "Run unit tests when building" runUnitTests (\v p -> p { runUnitTests = v })
        , b "Run benchmarks when building" runBenchmarks (\v p -> p { runBenchmarks = v })
        , b "Make mode" makeMode (\v p -> p { makeMode = v })
        , b "Single build without linking" singleBuildWithoutLinking (\v p -> p { singleBuildWithoutLinking = v })
        , b "Don't install the last package" dontInstallLast (\v p -> p { dontInstallLast = v })
        ]
    , section "Debug"
        [ b "Enable Show instances in :print" printEvldWithShow (\v p -> p { printEvldWithShow = v })
        , b "Break on any exception thrown" breakOnException (\v p -> p { breakOnException = v })
        , b "Break on uncaught exceptions and errors" breakOnError (\v p -> p { breakOnError = v })
        , b "Print binding results in GHCi" printBindResult (\v p -> p { printBindResult = v })
        ]
    , section "Metadata"
        [ linesField p0 "Source directories (one per line)"
            sourceDirectories (\v p -> p { sourceDirectories = v })
        , maybeTextField p0 "Unpack cabal package source to"
            unpackDirectory (\v p -> p { unpackDirectory = v })
        , txt "URL for prebuilt metadata" retrieveURL (\v p -> p { retrieveURL = v })
        , enumField p0 "Metadata download strategy" retrieveOptions retrieveStrategy (\v p -> p { retrieveStrategy = v })
        , b "Update metadata at startup" collectAtStart (\v p -> p { collectAtStart = v })
        , txt "leksah-server IP address" serverIP (\v p -> p { serverIP = v })
        , i "leksah-server port" serverPort (\v p -> p { serverPort = v })
        , b "Stop leksah-server when leksah disconnects" endWithLastConn (\v p -> p { endWithLastConn = v })
        ]
    , section "Blacklist"
        [ depsField p0 "Packages excluded from the modules pane (one per line, e.g. base or base >=4)"
            packageBlacklist (\v p -> p { packageBlacklist = v })
        ]
    , section "Help"
        [ txt "Browser" browser (\v p -> p { browser = v })
        , txt "URL for searching documentation" docuSearchURL (\v p -> p { docuSearchURL = v })
        ]
    ]
  where
    section :: Text -> [m (Event t (Prefs -> Prefs))] -> m [Event t (Prefs -> Prefs)]
    section title fields = elClass "div" "pref-group" $ do
      elClass "div" "pref-section-title" $ text title
      elClass "div" "pref-section" $ sequence fields

    tallVisOptions = [ ("Show", TallShow), ("Auto-hide", TallAutoHide), ("Hide", TallHide) ]
    retrieveOptions =
      [ ("Download then build", RetrieveThenBuild)
      , ("Build then download", BuildThenRetrieve)
      , ("Never download",      NeverRetrieve) ]

-- | A labelled row: label on the left, control on the right.  Prefs the web UI
-- doesn't act on yet get a grey "(TODO)" after the label.
prefRow :: MonadWidget t m => Text -> m a -> m a
prefRow lbl inner = elClass "div" "pref-row" $ do
  elClass "label" "pref-label" $ do
    text lbl
    if lbl `elem` wiredLabels
      then blank
      else elClass "span" "pref-todo" $ text " (TODO)"
  elClass "div" "pref-control" inner

-- | The prefs the web UI currently honours (file filters, layout visibility, the
-- build flags, metadata/leksah-server settings, the package blacklist).  Anything
-- not listed here is stored but not yet acted on, and is flagged "(TODO)".
wiredLabels :: [Text]
wiredLabels =
  [ "Show hidden files in the workspace", "Show ignored files in the workspace"
  , "Side bar visibility", "Bottom bar visibility"
  , "Save all files before building", "Run HLint when saving a source file"
  , "Select first warning if built without errors", "Background build"
  , "Native", "JavaScript", "Debug", "Make documentation when building"
  , "Run unit tests when building", "Run benchmarks when building", "Make mode"
  , "Single build without linking", "Don't install the last package"
  , "Source directories (one per line)", "Unpack cabal package source to"
  , "URL for prebuilt metadata", "Metadata download strategy"
  , "Update metadata at startup", "leksah-server IP address", "leksah-server port"
  , "Stop leksah-server when leksah disconnects"
  , "Packages excluded from the modules pane (one per line, e.g. base or base >=4)"
  , "Clickable file paths and identifiers in terminal output"
  , "External editor command (blank = built-in editor)"
  , "Use tmux control mode (-CC): native pane splits (new terminals)"
  , "Remote hosts in the Terminals tree (ssh, one per line)"
  , "Selection highlight colour"
  , "Run-button hover row colour"
  ]

-- | A checkbox driven by the live prefs (stays in sync with toolbar toggles).
boolField
  :: MonadWidget t m
  => Dynamic t Prefs -> Text -> (Prefs -> Bool) -> (Bool -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
boolField prefsD lbl get set = prefRow lbl $ do
  let stD = get <$> prefsD
  (e, _) <- elDynAttr' "span" (ffor stD $ \v -> "class" =: ("pref-check" <> bool "" " on" v)) blank
  return $ ffor (tag (current stD) (domEvent Click e)) $ \cur -> set (not cur)

-- | A checkbox + integer, for a @(Bool, Int)@ pref like the right margin.
boolIntField
  :: MonadWidget t m
  => Dynamic t Prefs -> Prefs -> Text -> (Prefs -> (Bool, Int)) -> ((Bool, Int) -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
boolIntField prefsD p0 lbl get set = prefRow lbl $ do
  let stD = (fst . get) <$> prefsD
  (e, _) <- elDynAttr' "span" (ffor stD $ \v -> "class" =: ("pref-check" <> bool "" " on" v)) blank
  let boolE = ffor (tag (current stD) (domEvent Click e)) $ \cur p -> set (not cur, snd (get p)) p
  inp <- numberInput (show (snd (get p0)))
  let intE = fmapMaybe (\t -> (\n p -> set (fst (get p), n) p) <$> readMaybe (T.unpack t))
                       (updated (_inputElement_value inp))
  return $ leftmost [boolE, intE]

-- | An integer field.
intField
  :: MonadWidget t m
  => Prefs -> Text -> (Prefs -> Int) -> (Int -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
intField p0 lbl get set = prefRow lbl $ do
  inp <- numberInput (show (get p0))
  return $ fmapMaybe (fmap set . readMaybe . T.unpack) (updated (_inputElement_value inp))

-- | A text field.
textField
  :: MonadWidget t m
  => Prefs -> Text -> (Prefs -> Text) -> (Text -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
textField p0 lbl get set = prefRow lbl $ do
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
  => Dynamic t Prefs -> Prefs -> Text -> (Prefs -> Text) -> (Text -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
colorField prefsD p0 lbl get set = prefRow lbl $ do
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

-- | An optional path (blank = 'Nothing').
maybeTextField
  :: MonadWidget t m
  => Prefs -> Text -> (Prefs -> Maybe FilePath) -> (Maybe FilePath -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
maybeTextField p0 lbl get set = prefRow lbl $ do
  inp <- textInputAttrs (maybe "" T.pack (get p0)) ("type" =: "text" <> "class" =: "pref-input")
  return $ ffor (updated (_inputElement_value inp)) $ \t ->
    set (if T.null (T.strip t) then Nothing else Just (T.unpack t))

-- | A multi-line list of paths, one per line.
linesField
  :: MonadWidget t m
  => Prefs -> Text -> (Prefs -> [FilePath]) -> ([FilePath] -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
linesField p0 lbl get set = prefRow lbl $ do
  ta <- textArea (T.unlines (map T.pack (get p0)))
  return $ ffor (updated (_textAreaElement_value ta)) $ \t ->
    set [ T.unpack l | l <- map T.strip (T.lines t), not (T.null l) ]

-- | A package blacklist, one 'Dependency' per line (unparseable lines dropped).
depsField
  :: MonadWidget t m
  => Prefs -> Text -> (Prefs -> [Dependency]) -> ([Dependency] -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
depsField p0 lbl get set = prefRow lbl $ do
  ta <- textArea (T.pack (unlines (map prettyShow (get p0))))
  return $ ffor (updated (_textAreaElement_value ta)) $ \t ->
    set (mapMaybe (simpleParsec . T.unpack . T.strip) (filter (not . T.null . T.strip) (T.lines t)))

-- | A dropdown over a labelled option list, seeded from the current value.
enumField
  :: forall t m a. (MonadWidget t m, Eq a)
  => Prefs -> Text -> [(Text, a)] -> (Prefs -> a) -> (a -> Prefs -> Prefs)
  -> m (Event t (Prefs -> Prefs))
enumField p0 lbl opts get set = prefRow lbl $ do
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
    "color" -: "#dcdcdc"
    "background" -: "rgb(24,24,24)"
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
    "color" -: "#fff"
    "margin" -: "18px 0 6px 0"
    "border-bottom" -: "1px solid rgb(60,60,60)"
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
    "color" -: "#888"
  ".preferences .pref-control" ? do
    "flex" -: "1 1 200px"
    "min-width" -: "0"
    "display" -: "flex"
    "align-items" -: "center"
  ".preferences .pref-check" ? do
    "display" -: "inline-block"
    "width" -: "14px"
    "height" -: "14px"
    "border" -: "1px solid rgb(90,90,90)"
    "border-radius" -: "3px"
    "background" -: "rgb(40,40,40)"
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
    "border" -: "1px solid rgb(90,90,90)"
    "border-radius" -: "3px"
    "padding" -: "0"
    "cursor" -: "pointer"
  ".preferences .pref-input" ? do
    "background" -: "rgb(40,40,40)"
    "color" -: "#eee"
    "border" -: "1px solid rgb(70,70,70)"
    "border-radius" -: "3px"
    "padding" -: "2px 6px"
    "box-sizing" -: "border-box"
    "min-width" -: "0"
    "width" -: "100%"
    "max-width" -: "260px"
  ".preferences .pref-textarea" ? do
    "background" -: "rgb(40,40,40)"
    "color" -: "#eee"
    "border" -: "1px solid rgb(70,70,70)"
    "border-radius" -: "3px"
    "padding" -: "4px 6px"
    "box-sizing" -: "border-box"
    "width" -: "100%"
    "max-width" -: "360px"
    "height" -: "80px"
    "font-family" -: "Hasklig, Menlo, monospace"
  ".preferences select" ? do
    "background" -: "rgb(40,40,40)"
    "color" -: "#eee"
    "border" -: "1px solid rgb(70,70,70)"
    "border-radius" -: "3px"
    "padding" -: "2px 6px"
