-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The IDE's user settings: the 'Prefs' record, its defaults, and the
-- sectioned JSON file they live in (@~\/.config\/leksah\/settings.json@).
--
-- The record is flat (call sites read fields like 'tabWidth' directly and the
-- Preferences pane updates them with record syntax); the JSON is sectioned
-- (@editor@ \/ @font@ \/ @theme@ \/ @ui@ \/ @build@ \/ @terminal@ \/ @lsp@ \/
-- @ai@), and — VS Code style — only keys whose value differs from the default
-- are written, so the file stays a readable list of what the user changed.
module IDE.Settings
  ( Prefs(..)
  , defaultPrefs
  , EditorChoice(..)
  , editorChoiceToText
  , editorChoiceFromText
  , externalEditor
  , monacoEditor
  , settingsFilePath
  , readSettings
  , writeSettings
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
       ((.=), (.:?), (.!=), FromJSON(..), ToJSON(..), Value(..), object,
        withObject, eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Encode.Pretty
       (encodePretty', defConfig, Config(..), keyOrder)
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Directory
       (XdgDirectory(..), createDirectoryIfMissing, doesFileExist,
        getXdgDirectory)
import System.FilePath ((</>), takeDirectory)

-- | Which editor opens files: one of the two in-app controls (Monaco /
-- CodeMirror 6), or a terminal editor (nano\/vim\/emacs) run in the file's
-- backing tmux pane.
data EditorChoice
    = EditorMonaco      -- ^ the Monaco (VS Code) editor control (default)
    | EditorCodeMirror  -- ^ the CodeMirror 6 editor control
    | EditorNano
    | EditorVim
    | EditorEmacs
    deriving (Eq, Show, Read, Enum, Bounded, Generic)

-- | Stable names used in the settings file.
editorChoiceToText :: EditorChoice -> Text
editorChoiceToText EditorMonaco     = "monaco"
editorChoiceToText EditorCodeMirror = "codemirror"
editorChoiceToText EditorNano       = "nano"
editorChoiceToText EditorVim        = "vim"
editorChoiceToText EditorEmacs      = "emacs"

editorChoiceFromText :: Text -> Maybe EditorChoice
editorChoiceFromText "monaco"     = Just EditorMonaco
editorChoiceFromText "codemirror" = Just EditorCodeMirror
editorChoiceFromText "nano"       = Just EditorNano
editorChoiceFromText "vim"        = Just EditorVim
editorChoiceFromText "emacs"      = Just EditorEmacs
editorChoiceFromText _            = Nothing

-- | The user's settings.  Field names are the API the rest of the IDE reads;
-- the JSON section/key each is stored under is in the codec below.
data Prefs = Prefs {
    -- editor
        showLineNumbers     :: Bool
    ,   rightMargin         :: Int  -- ^ column, 0 = off
    ,   tabWidth            :: Int
    ,   wrapLines           :: Bool
    ,   editorChoice        :: EditorChoice
                                -- ^ the editor files open in, applied to tabs
                                --   opened from now on (see 'externalEditor')
    ,   forceLineEnds       :: Bool
    ,   removeTBlanks       :: Bool
    ,   autoLoad            :: Bool
    -- font
    ,   monospaceFont       :: Text -- ^ CSS font-family for the monospace
                                    --   surfaces (editor, terminals, log)
    ,   monospaceFontSize   :: Int  -- ^ …and its size in px
    -- theme
    ,   monacoThemeDark     :: Text -- ^ editor/terminal theme ids per OS
    ,   monacoThemeLight    :: Text --   appearance (dark vs light); the values
    ,   codeMirrorThemeDark :: Text --   are the ids the bundles know
    ,   codeMirrorThemeLight:: Text --   (Monaco: leksah-github-dark\/-light,
    ,   xtermThemeDark      :: Text --   vs\/vs-dark\/hc-*; CM: github-dark\/-light;
    ,   xtermThemeLight     :: Text --   xterm: leksah-dark\/-light, solarized-*)
    ,   uiSelectionColor    :: Text -- ^ selection/active highlight colour
                                    --   (#rrggbb; bound to --leksah-selection)
    ,   uiHoverColor        :: Text -- ^ run-button hover row colour
                                    --   (#rrggbb; bound to --leksah-hover)
    -- ui
    ,   showHiddenFiles     :: Bool
    ,   showIgnoredFiles    :: Bool
    ,   showWorkspaceIcons  :: Bool
    ,   collapseErrors      :: Bool
    ,   saveSessionOnClose  :: Bool
    ,   showShortcutBadges  :: Bool -- ^ holding Cmd overlays each pane's
                                    --   navigation shortcut as a badge
    ,   colorfulIcons       :: Bool -- ^ use the coloured icon set (pics/color)
    -- build
    ,   saveAllBeforeBuild  :: Bool
    ,   hlintOnSave         :: Bool
    ,   jumpToWarnings      :: Bool
    ,   backgroundBuild     :: Bool
    ,   native              :: Bool
    ,   javaScript          :: Bool
    ,   debug               :: Bool -- ^ ghci mode: build through cached repls
                                    --   (stored as @build.ghci@)
    ,   makeDocs            :: Bool
    ,   runUnitTests        :: Bool
    ,   runBenchmarks       :: Bool
    ,   makeMode            :: Bool
    -- terminal
    ,   terminalFileLinks   :: Bool -- ^ recognise file paths / identifiers in
                                    --   terminal output (the custom xterm link
                                    --   provider); off lets OSC 8 links through
    ,   terminalControlMode :: Bool -- ^ render terminals via tmux control mode
                                    --   (-CC): one xterm per pane, native splits
    ,   tmuxInterceptPrefix :: Bool -- ^ intercept the tmux @C-b@ prefix in
                                    --   terminals
    ,   remoteHosts         :: [Text] -- ^ ssh hosts shown as top-level nodes in
                                      --   the Terminals tree
    -- lsp
    ,   lspEnabled          :: Bool -- ^ run a Language Server per project
    ,   lspServerCommand    :: Text -- ^ override the LSP server command line
                                    --   (blank = @haskell-language-server --lsp@;
                                    --   a project's @.leksah-lsp@ file overrides
                                    --   even this)
    -- ai
    ,   regionCaptureTarget :: Text -- ^ the terminal `leksah-cmd grab-region`
                                    --   types into when the caller names no
                                    --   TARGET itself (@session\/window\/pane@).
                                    --   A FALLBACK only (see "IDE.Web.AISession")
} deriving (Eq, Show, Generic)

defaultPrefs :: Prefs
defaultPrefs = Prefs
    { showLineNumbers     = True
    , rightMargin         = 100
    , tabWidth            = 4
    , wrapLines           = False
    , editorChoice        = EditorMonaco
    , forceLineEnds       = True
    , removeTBlanks       = True
    , autoLoad            = False
    , monospaceFont       = "Hasklig, Menlo, monospace"
    , monospaceFontSize   = 13
    , monacoThemeDark     = "leksah-github-dark"
    , monacoThemeLight    = "leksah-github-light"
    , codeMirrorThemeDark = "github-dark"
    , codeMirrorThemeLight= "github-light"
    , xtermThemeDark      = "leksah-dark"
    , xtermThemeLight     = "leksah-light"
    , uiSelectionColor    = "#1e58d1"
    , uiHoverColor        = "#0c1e46"
    , showHiddenFiles     = False
    , showIgnoredFiles    = False
    , showWorkspaceIcons  = True
    , collapseErrors      = True
    , saveSessionOnClose  = True
    , showShortcutBadges  = False
    , colorfulIcons       = False
    , saveAllBeforeBuild  = True
    , hlintOnSave         = True
    , jumpToWarnings      = True
    , backgroundBuild     = True
    , native              = True
    , javaScript          = True
    , debug               = True
    , makeDocs            = False
    , runUnitTests        = False
    , runBenchmarks       = False
    , makeMode            = True
    , terminalFileLinks   = True
    , terminalControlMode = True
    , tmuxInterceptPrefix = False
    , remoteHosts         = []
    , lspEnabled          = True
    , lspServerCommand    = ""
    , regionCaptureTarget = "claude/leksah/0"
    }

-- | Legacy view of 'editorChoice': the external-editor command, blank when an
-- in-app editor is selected.
externalEditor :: Prefs -> Text
externalEditor p = case editorChoice p of
    EditorNano  -> "nano"
    EditorVim   -> "vim"
    EditorEmacs -> "emacs"
    _           -> ""

-- | Legacy view of 'editorChoice': whether in-app editors use Monaco.
monacoEditor :: Prefs -> Bool
monacoEditor = (== EditorMonaco) . editorChoice

-- ---------------------------------------------------------------------
-- The sectioned codec.  One table drives both directions: each section
-- lists (key, write-if-changed, parse-with-default) per field.
-- ---------------------------------------------------------------------

instance ToJSON Prefs where
    toJSON p = object $ catMaybes
        [ sectionOut "editor"   (editorFields p)
        , sectionOut "font"     (fontFields p)
        , sectionOut "theme"    (themeFields p)
        , sectionOut "ui"       (uiFields p)
        , sectionOut "build"    (buildFields p)
        , sectionOut "terminal" (terminalFields p)
        , sectionOut "lsp"      (lspFields p)
        , sectionOut "ai"       (aiFields p)
        ]
      where
        sectionOut k fields = case catMaybes fields of
            []    -> Nothing
            pairs -> Just (k .= object pairs)

instance FromJSON Prefs where
    parseJSON = withObject "settings" $ \o -> do
        ed <- sectionIn o "editor"
        fo <- sectionIn o "font"
        th <- sectionIn o "theme"
        ui <- sectionIn o "ui"
        bu <- sectionIn o "build"
        te <- sectionIn o "terminal"
        ls <- sectionIn o "lsp"
        ai <- sectionIn o "ai"
        Prefs
            <$> fld ed "lineNumbers"         showLineNumbers
            <*> fld ed "rightMargin"         rightMargin
            <*> fld ed "tabWidth"            tabWidth
            <*> fld ed "wrapLines"           wrapLines
            <*> (fromMaybe (editorChoice defaultPrefs) . (>>= editorChoiceFromText)
                    <$> ed .:? "choice")
            <*> fld ed "fixLineEndings"      forceLineEnds
            <*> fld ed "stripTrailingBlanks" removeTBlanks
            <*> fld ed "autoReload"          autoLoad
            <*> fld fo "family"              monospaceFont
            <*> fld fo "size"                monospaceFontSize
            <*> fld th "monacoDark"          monacoThemeDark
            <*> fld th "monacoLight"         monacoThemeLight
            <*> fld th "codeMirrorDark"      codeMirrorThemeDark
            <*> fld th "codeMirrorLight"     codeMirrorThemeLight
            <*> fld th "terminalDark"        xtermThemeDark
            <*> fld th "terminalLight"       xtermThemeLight
            <*> fld th "selectionColor"      uiSelectionColor
            <*> fld th "hoverColor"          uiHoverColor
            <*> fld ui "showHiddenFiles"     showHiddenFiles
            <*> fld ui "showIgnoredFiles"    showIgnoredFiles
            <*> fld ui "workspaceIcons"      showWorkspaceIcons
            <*> fld ui "collapseErrors"      collapseErrors
            <*> fld ui "saveSessionOnClose"  saveSessionOnClose
            <*> fld ui "shortcutBadges"      showShortcutBadges
            <*> fld ui "colorfulIcons"       colorfulIcons
            <*> fld bu "saveAllBeforeBuild"  saveAllBeforeBuild
            <*> fld bu "hlintOnSave"         hlintOnSave
            <*> fld bu "jumpToWarnings"      jumpToWarnings
            <*> fld bu "background"          backgroundBuild
            <*> fld bu "native"              native
            <*> fld bu "javaScript"          javaScript
            <*> fld bu "ghci"                debug
            <*> fld bu "docs"                makeDocs
            <*> fld bu "tests"               runUnitTests
            <*> fld bu "benchmarks"          runBenchmarks
            <*> fld bu "makeMode"            makeMode
            <*> fld te "fileLinks"           terminalFileLinks
            <*> fld te "controlMode"         terminalControlMode
            <*> fld te "interceptTmuxPrefix" tmuxInterceptPrefix
            <*> fld te "remoteHosts"         remoteHosts
            <*> fld ls "enabled"             lspEnabled
            <*> fld ls "serverCommand"       lspServerCommand
            <*> fld ai "captureTarget"       regionCaptureTarget
      where
        sectionIn o k = o .:? k .!= KM.empty
        fld o k dflt = o .:? k .!= dflt defaultPrefs

-- | @Just (key .= value)@ when the field differs from the default.
changed :: (Eq a, ToJSON a) => Key.Key -> (Prefs -> a) -> Prefs -> Maybe Pair
changed k get p
    | get p == get defaultPrefs = Nothing
    | otherwise                 = Just (k .= get p)

editorFields, fontFields, themeFields, uiFields, buildFields,
    terminalFields, lspFields, aiFields :: Prefs -> [Maybe Pair]
editorFields p =
    [ changed "lineNumbers"         showLineNumbers p
    , changed "rightMargin"         rightMargin p
    , changed "tabWidth"            tabWidth p
    , changed "wrapLines"           wrapLines p
    , changed "choice"              (editorChoiceToText . editorChoice) p
    , changed "fixLineEndings"      forceLineEnds p
    , changed "stripTrailingBlanks" removeTBlanks p
    , changed "autoReload"          autoLoad p
    ]
fontFields p =
    [ changed "family" monospaceFont p
    , changed "size"   monospaceFontSize p
    ]
themeFields p =
    [ changed "monacoDark"      monacoThemeDark p
    , changed "monacoLight"     monacoThemeLight p
    , changed "codeMirrorDark"  codeMirrorThemeDark p
    , changed "codeMirrorLight" codeMirrorThemeLight p
    , changed "terminalDark"    xtermThemeDark p
    , changed "terminalLight"   xtermThemeLight p
    , changed "selectionColor"  uiSelectionColor p
    , changed "hoverColor"      uiHoverColor p
    ]
uiFields p =
    [ changed "showHiddenFiles"    showHiddenFiles p
    , changed "showIgnoredFiles"   showIgnoredFiles p
    , changed "workspaceIcons"     showWorkspaceIcons p
    , changed "collapseErrors"     collapseErrors p
    , changed "saveSessionOnClose" saveSessionOnClose p
    , changed "shortcutBadges"     showShortcutBadges p
    , changed "colorfulIcons"      colorfulIcons p
    ]
buildFields p =
    [ changed "saveAllBeforeBuild" saveAllBeforeBuild p
    , changed "hlintOnSave"        hlintOnSave p
    , changed "jumpToWarnings"     jumpToWarnings p
    , changed "background"         backgroundBuild p
    , changed "native"             native p
    , changed "javaScript"         javaScript p
    , changed "ghci"               debug p
    , changed "docs"               makeDocs p
    , changed "tests"              runUnitTests p
    , changed "benchmarks"         runBenchmarks p
    , changed "makeMode"           makeMode p
    ]
terminalFields p =
    [ changed "fileLinks"           terminalFileLinks p
    , changed "controlMode"         terminalControlMode p
    , changed "interceptTmuxPrefix" tmuxInterceptPrefix p
    , changed "remoteHosts"         remoteHosts p
    ]
lspFields p =
    [ changed "enabled"       lspEnabled p
    , changed "serverCommand" lspServerCommand p
    ]
aiFields p =
    [ changed "captureTarget" regionCaptureTarget p
    ]

-- ---------------------------------------------------------------------
-- The file
-- ---------------------------------------------------------------------

-- | @~\/.config\/leksah\/settings.json@ (via the XDG config dir).
settingsFilePath :: IO FilePath
settingsFilePath = (</> "settings.json") <$> getXdgDirectory XdgConfig "leksah"

-- | Read the settings.  A missing file is simply the defaults; an unreadable
-- or unparsable one is the defaults plus an error message for the caller to
-- surface (Log pane) — never silent.
readSettings :: IO (Prefs, Maybe Text)
readSettings = do
    fp <- settingsFilePath
    doesFileExist fp >>= \case
        False -> return (defaultPrefs, Nothing)
        True -> try (BS.readFile fp) >>= \case
            Left (e :: SomeException) ->
                return (defaultPrefs, Just (T.pack (show e)))
            Right bytes -> case eitherDecodeStrict' bytes of
                Left err -> return
                    (defaultPrefs, Just (T.pack fp <> ": " <> T.pack err))
                Right p  -> return (p, Nothing)

-- | Write the settings (pretty, sections in a stable order, only keys that
-- differ from the defaults).
writeSettings :: Prefs -> IO ()
writeSettings p = do
    fp <- settingsFilePath
    createDirectoryIfMissing True (takeDirectory fp)
    LBS.writeFile fp $ encodePretty' cfg p <> "\n"
  where
    cfg = defConfig { confCompare = keyOrder
        ["editor", "font", "theme", "ui", "build", "terminal", "lsp", "ai"] }
