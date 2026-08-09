-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User configuration: small per-section records, one file, one cell.
--
-- The file is @~\/.config\/leksah\/settings.json@ — sectioned objects in
-- which only keys that differ from the defaults are ever written, so the
-- file stays a readable record of what the user chose.  A missing or
-- broken file means defaults (plus an error string for the UI to show —
-- never silence).
--
-- Widgets never read this by snapshot: 'ConfigService' holds a
-- 'Cell Config', and each window lifts it (or a section of it) into a
-- 'Dynamic' via @cellDyn@.
module IDE.Config
  ( -- * Sections
    EditorC(..)
  , Editor(..)
  , FontC(..)
  , ThemeC(..)
  , UiC(..)
  , BuildC(..)
  , TerminalC(..)
  , RemoteC(..)
  , LspC(..)
  , AiC(..)
    -- * The lot
  , Config(..)
  , defaultConfig
    -- * File
  , configFilePath
  , readConfigFile
  , writeConfigFile
    -- * Service
  , ConfigService
  , newConfigService
  , configCell
  , currentConfig
  , saveConfig
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Pair, Parser)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
       (XdgDirectory(..), createDirectoryIfMissing, getXdgDirectory)
import System.FilePath ((</>), takeDirectory)

import IDE.Reactive (Cell, newCell, readCell, writeCell)

-- | Which editor opens a file.
data Editor
    = EditorCodeMirror
    | EditorMonaco
    | EditorExternal Text  -- ^ a shell command; the file path is appended
    deriving (Eq, Show)

instance ToJSON Editor where
    toJSON EditorCodeMirror  = String "codemirror"
    toJSON EditorMonaco      = String "monaco"
    toJSON (EditorExternal c) = String c

instance FromJSON Editor where
    parseJSON = withText "Editor" $ \case
        "codemirror" -> pure EditorCodeMirror
        "monaco"     -> pure EditorMonaco
        cmd          -> pure (EditorExternal cmd)

data EditorC = EditorC
    { ecEditor       :: Editor
    , ecLineNumbers  :: Bool
    , ecRightMargin  :: Int   -- ^ column, 0 = off
    , ecTabWidth     :: Int
    , ecWrapLines    :: Bool
    , ecFixLineEnds  :: Bool  -- ^ normalise line endings on save
    , ecStripBlanks  :: Bool  -- ^ strip trailing whitespace on save
    , ecAutoReload   :: Bool  -- ^ reload buffers changed on disk
    } deriving (Eq, Show)

data FontC = FontC
    { fcMonoFamily :: Text  -- ^ CSS font-family for all monospace surfaces
    , fcMonoSize   :: Int   -- ^ px
    } deriving (Eq, Show)

-- | Theme ids per OS appearance; the values are ids the bundles know.
data ThemeC = ThemeC
    { thMonacoDark      :: Text
    , thMonacoLight     :: Text
    , thCodeMirrorDark  :: Text
    , thCodeMirrorLight :: Text
    , thXtermDark       :: Text
    , thXtermLight      :: Text
    , thSelectionColor  :: Text  -- ^ selection\/active highlight
    , thHoverColor      :: Text  -- ^ hover-row highlight
    } deriving (Eq, Show)

data UiC = UiC
    { uiShowHiddenFiles  :: Bool
    , uiShowIgnoredFiles :: Bool
    , uiWorkspaceIcons   :: Bool
    , uiColorfulIcons    :: Bool
    , uiCollapseErrors   :: Bool
    , uiSaveSession      :: Bool  -- ^ save the session on close
    , uiShortcutBadges   :: Bool  -- ^ holding Cmd overlays pane shortcuts
    } deriving (Eq, Show)

data BuildC = BuildC
    { bcSaveAllFirst   :: Bool  -- ^ save all before building
    , bcLintOnSave     :: Bool  -- ^ run the external linter after save
    , bcJumpToWarnings :: Bool
    , bcBackground     :: Bool  -- ^ build on save
    , bcNative         :: Bool
    , bcJavaScript     :: Bool
    , bcGhci           :: Bool  -- ^ build through cached repls (ghci mode)
    , bcDocs           :: Bool
    , bcTests          :: Bool
    , bcBenchmarks     :: Bool
    , bcMakeMode       :: Bool
    } deriving (Eq, Show)

data TerminalC = TerminalC
    { tcFileLinks   :: Bool  -- ^ recognise file paths\/identifiers
    , tcControlMode :: Bool  -- ^ render terminals via tmux control mode
    , tcTmuxPrefix  :: Bool  -- ^ intercept the tmux prefix key in-page
    } deriving (Eq, Show)

newtype RemoteC = RemoteC
    { rcHosts :: [Text]  -- ^ ssh hosts shown as top-level tree nodes
    } deriving (Eq, Show)

data LspC = LspC
    { lcEnabled       :: Bool
    , lcServerCommand :: Text  -- ^ override; blank = per-language default
    } deriving (Eq, Show)

newtype AiC = AiC
    { acCaptureTarget :: Text  -- ^ terminal that receives grab-region paths
    } deriving (Eq, Show)

data Config = Config
    { cfgEditor   :: EditorC
    , cfgFont     :: FontC
    , cfgTheme    :: ThemeC
    , cfgUi       :: UiC
    , cfgBuild    :: BuildC
    , cfgTerminal :: TerminalC
    , cfgRemote   :: RemoteC
    , cfgLsp      :: LspC
    , cfgAi       :: AiC
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { cfgEditor = EditorC
        { ecEditor      = EditorMonaco
        , ecLineNumbers = True
        , ecRightMargin = 100
        , ecTabWidth    = 4
        , ecWrapLines   = False
        , ecFixLineEnds = True
        , ecStripBlanks = True
        , ecAutoReload  = False
        }
    , cfgFont = FontC
        { fcMonoFamily = "Hasklig, Menlo, monospace"
        , fcMonoSize   = 13
        }
    , cfgTheme = ThemeC
        { thMonacoDark      = "leksah-github-dark"
        , thMonacoLight     = "leksah-github-light"
        , thCodeMirrorDark  = "github-dark"
        , thCodeMirrorLight = "github-light"
        , thXtermDark       = "leksah-dark"
        , thXtermLight      = "leksah-light"
        , thSelectionColor  = "#1e58d1"
        , thHoverColor      = "#0c1e46"
        }
    , cfgUi = UiC
        { uiShowHiddenFiles  = False
        , uiShowIgnoredFiles = False
        , uiWorkspaceIcons   = True
        , uiColorfulIcons    = False
        , uiCollapseErrors   = True
        , uiSaveSession      = True
        , uiShortcutBadges   = False
        }
    , cfgBuild = BuildC
        { bcSaveAllFirst   = True
        , bcLintOnSave     = True
        , bcJumpToWarnings = True
        , bcBackground     = True
        , bcNative         = True
        , bcJavaScript     = True
        , bcGhci           = True
        , bcDocs           = False
        , bcTests          = False
        , bcBenchmarks     = False
        , bcMakeMode       = True
        }
    , cfgTerminal = TerminalC
        { tcFileLinks   = True
        , tcControlMode = True
        , tcTmuxPrefix  = False
        }
    , cfgRemote = RemoteC { rcHosts = [] }
    , cfgLsp = LspC
        { lcEnabled       = True
        , lcServerCommand = ""
        }
    , cfgAi = AiC { acCaptureTarget = "claude/leksah/0" }
    }

-- Encoding: a section contributes an object of only its changed keys, and
-- a fully-default section contributes nothing.

chg :: (Eq a, ToJSON a) => Key.Key -> (s -> a) -> s -> s -> Maybe Pair
chg k get dflt s
    | get s == get dflt = Nothing
    | otherwise         = Just (k .= get s)

sect :: Key.Key -> [Maybe Pair] -> Maybe Pair
sect k ps = case catMaybes ps of
    [] -> Nothing
    xs -> Just (k .= object xs)

instance ToJSON Config where
    toJSON c = object $ catMaybes
        [ sect "editor" $ let d = cfgEditor defaultConfig; s = cfgEditor c in
            [ chg "editor"       ecEditor      d s
            , chg "lineNumbers"  ecLineNumbers d s
            , chg "rightMargin"  ecRightMargin d s
            , chg "tabWidth"     ecTabWidth    d s
            , chg "wrapLines"    ecWrapLines   d s
            , chg "fixLineEnds"  ecFixLineEnds d s
            , chg "stripBlanks"  ecStripBlanks d s
            , chg "autoReload"   ecAutoReload  d s
            ]
        , sect "font" $ let d = cfgFont defaultConfig; s = cfgFont c in
            [ chg "monoFamily" fcMonoFamily d s
            , chg "monoSize"   fcMonoSize   d s
            ]
        , sect "theme" $ let d = cfgTheme defaultConfig; s = cfgTheme c in
            [ chg "monacoDark"      thMonacoDark      d s
            , chg "monacoLight"     thMonacoLight     d s
            , chg "codeMirrorDark"  thCodeMirrorDark  d s
            , chg "codeMirrorLight" thCodeMirrorLight d s
            , chg "xtermDark"       thXtermDark       d s
            , chg "xtermLight"      thXtermLight      d s
            , chg "selectionColor"  thSelectionColor  d s
            , chg "hoverColor"      thHoverColor      d s
            ]
        , sect "ui" $ let d = cfgUi defaultConfig; s = cfgUi c in
            [ chg "showHiddenFiles"  uiShowHiddenFiles  d s
            , chg "showIgnoredFiles" uiShowIgnoredFiles d s
            , chg "workspaceIcons"   uiWorkspaceIcons   d s
            , chg "colorfulIcons"    uiColorfulIcons    d s
            , chg "collapseErrors"   uiCollapseErrors   d s
            , chg "saveSession"      uiSaveSession      d s
            , chg "shortcutBadges"   uiShortcutBadges   d s
            ]
        , sect "build" $ let d = cfgBuild defaultConfig; s = cfgBuild c in
            [ chg "saveAllFirst"   bcSaveAllFirst   d s
            , chg "lintOnSave"     bcLintOnSave     d s
            , chg "jumpToWarnings" bcJumpToWarnings d s
            , chg "background"     bcBackground     d s
            , chg "native"         bcNative         d s
            , chg "javaScript"     bcJavaScript     d s
            , chg "ghci"           bcGhci           d s
            , chg "docs"           bcDocs           d s
            , chg "tests"          bcTests          d s
            , chg "benchmarks"     bcBenchmarks     d s
            , chg "makeMode"       bcMakeMode       d s
            ]
        , sect "terminal" $ let d = cfgTerminal defaultConfig; s = cfgTerminal c in
            [ chg "fileLinks"   tcFileLinks   d s
            , chg "controlMode" tcControlMode d s
            , chg "tmuxPrefix"  tcTmuxPrefix  d s
            ]
        , sect "remote" $ let d = cfgRemote defaultConfig; s = cfgRemote c in
            [ chg "hosts" rcHosts d s ]
        , sect "lsp" $ let d = cfgLsp defaultConfig; s = cfgLsp c in
            [ chg "enabled"       lcEnabled       d s
            , chg "serverCommand" lcServerCommand d s
            ]
        , sect "ai" $ let d = cfgAi defaultConfig; s = cfgAi c in
            [ chg "captureTarget" acCaptureTarget d s ]
        ]

-- Decoding: every key optional, defaulting per field, unknown keys
-- ignored — an old or hand-edited file never fails to load outright.

inSect :: Object -> Key.Key -> (Object -> Parser a) -> a -> Parser a
inSect o k p dflt = o .:? k >>= maybe (pure dflt) (withObject "section" p)

fld :: FromJSON a => Object -> Key.Key -> a -> Parser a
fld o k dflt = o .:? k .!= dflt

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o -> do
        let dc = defaultConfig
        cfgEditor' <- inSect o "editor" (\s -> EditorC
            <$> fld s "editor"      (ecEditor      (cfgEditor dc))
            <*> fld s "lineNumbers" (ecLineNumbers (cfgEditor dc))
            <*> fld s "rightMargin" (ecRightMargin (cfgEditor dc))
            <*> fld s "tabWidth"    (ecTabWidth    (cfgEditor dc))
            <*> fld s "wrapLines"   (ecWrapLines   (cfgEditor dc))
            <*> fld s "fixLineEnds" (ecFixLineEnds (cfgEditor dc))
            <*> fld s "stripBlanks" (ecStripBlanks (cfgEditor dc))
            <*> fld s "autoReload"  (ecAutoReload  (cfgEditor dc)))
            (cfgEditor dc)
        cfgFont' <- inSect o "font" (\s -> FontC
            <$> fld s "monoFamily" (fcMonoFamily (cfgFont dc))
            <*> fld s "monoSize"   (fcMonoSize   (cfgFont dc)))
            (cfgFont dc)
        cfgTheme' <- inSect o "theme" (\s -> ThemeC
            <$> fld s "monacoDark"      (thMonacoDark      (cfgTheme dc))
            <*> fld s "monacoLight"     (thMonacoLight     (cfgTheme dc))
            <*> fld s "codeMirrorDark"  (thCodeMirrorDark  (cfgTheme dc))
            <*> fld s "codeMirrorLight" (thCodeMirrorLight (cfgTheme dc))
            <*> fld s "xtermDark"       (thXtermDark       (cfgTheme dc))
            <*> fld s "xtermLight"      (thXtermLight      (cfgTheme dc))
            <*> fld s "selectionColor"  (thSelectionColor  (cfgTheme dc))
            <*> fld s "hoverColor"      (thHoverColor      (cfgTheme dc)))
            (cfgTheme dc)
        cfgUi' <- inSect o "ui" (\s -> UiC
            <$> fld s "showHiddenFiles"  (uiShowHiddenFiles  (cfgUi dc))
            <*> fld s "showIgnoredFiles" (uiShowIgnoredFiles (cfgUi dc))
            <*> fld s "workspaceIcons"   (uiWorkspaceIcons   (cfgUi dc))
            <*> fld s "colorfulIcons"    (uiColorfulIcons    (cfgUi dc))
            <*> fld s "collapseErrors"   (uiCollapseErrors   (cfgUi dc))
            <*> fld s "saveSession"      (uiSaveSession      (cfgUi dc))
            <*> fld s "shortcutBadges"   (uiShortcutBadges   (cfgUi dc)))
            (cfgUi dc)
        cfgBuild' <- inSect o "build" (\s -> BuildC
            <$> fld s "saveAllFirst"   (bcSaveAllFirst   (cfgBuild dc))
            <*> fld s "lintOnSave"     (bcLintOnSave     (cfgBuild dc))
            <*> fld s "jumpToWarnings" (bcJumpToWarnings (cfgBuild dc))
            <*> fld s "background"     (bcBackground     (cfgBuild dc))
            <*> fld s "native"         (bcNative         (cfgBuild dc))
            <*> fld s "javaScript"     (bcJavaScript     (cfgBuild dc))
            <*> fld s "ghci"           (bcGhci           (cfgBuild dc))
            <*> fld s "docs"           (bcDocs           (cfgBuild dc))
            <*> fld s "tests"          (bcTests          (cfgBuild dc))
            <*> fld s "benchmarks"     (bcBenchmarks     (cfgBuild dc))
            <*> fld s "makeMode"       (bcMakeMode       (cfgBuild dc)))
            (cfgBuild dc)
        cfgTerminal' <- inSect o "terminal" (\s -> TerminalC
            <$> fld s "fileLinks"   (tcFileLinks   (cfgTerminal dc))
            <*> fld s "controlMode" (tcControlMode (cfgTerminal dc))
            <*> fld s "tmuxPrefix"  (tcTmuxPrefix  (cfgTerminal dc)))
            (cfgTerminal dc)
        cfgRemote' <- inSect o "remote" (\s -> RemoteC
            <$> fld s "hosts" (rcHosts (cfgRemote dc)))
            (cfgRemote dc)
        cfgLsp' <- inSect o "lsp" (\s -> LspC
            <$> fld s "enabled"       (lcEnabled       (cfgLsp dc))
            <*> fld s "serverCommand" (lcServerCommand (cfgLsp dc)))
            (cfgLsp dc)
        cfgAi' <- inSect o "ai" (\s -> AiC
            <$> fld s "captureTarget" (acCaptureTarget (cfgAi dc)))
            (cfgAi dc)
        return Config
            { cfgEditor   = cfgEditor'
            , cfgFont     = cfgFont'
            , cfgTheme    = cfgTheme'
            , cfgUi       = cfgUi'
            , cfgBuild    = cfgBuild'
            , cfgTerminal = cfgTerminal'
            , cfgRemote   = cfgRemote'
            , cfgLsp      = cfgLsp'
            , cfgAi       = cfgAi'
            }

configFilePath :: IO FilePath
configFilePath = (</> "settings.json") <$> getXdgDirectory XdgConfig "leksah"

-- | Defaults when the file is missing; defaults plus an error string when
-- it is unreadable — the caller shows the string, never swallows it.
readConfigFile :: IO (Config, Maybe Text)
readConfigFile = do
    path <- configFilePath
    tryRead path >>= \case
        Nothing          -> return (defaultConfig, Nothing)
        Just (Left err)  -> return (defaultConfig, Just err)
        Just (Right cfg) -> return (cfg, Nothing)
  where
    tryRead path = do
        r <- try (LBS.readFile path)
        return $ case r of
            Left (_ :: SomeException) -> Nothing  -- treat as absent
            Right bytes -> Just $ case eitherDecode bytes of
                Left err  -> Left ("settings.json: " <> T.pack err)
                Right cfg -> Right cfg

writeConfigFile :: Config -> IO ()
writeConfigFile cfg = do
    path <- configFilePath
    createDirectoryIfMissing True (takeDirectory path)
    LBS.writeFile path $ AP.encodePretty'
        AP.defConfig { AP.confIndent = AP.Spaces 2
                     , AP.confCompare = AP.keyOrder sections <> compare }
        cfg
  where
    sections = [ "editor", "font", "theme", "ui", "build", "terminal"
               , "remote", "lsp", "ai" ]

-- | The live configuration: one cell, loaded once at boot.
data ConfigService = ConfigService
    { csCell :: Cell Config
    }

-- | Load the file and make the service; the 'Maybe Text' is a load error
-- to surface in the UI.
newConfigService :: IO (ConfigService, Maybe Text)
newConfigService = do
    (cfg, err) <- readConfigFile
    cell <- newCell cfg
    return (ConfigService cell, err)

configCell :: ConfigService -> Cell Config
configCell = csCell

currentConfig :: ConfigService -> IO Config
currentConfig = readCell . csCell

-- | Update the live config and persist it.
saveConfig :: ConfigService -> Config -> IO ()
saveConfig cs cfg = do
    writeCell (csCell cs) cfg
    writeConfigFile cfg
