{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Preferences
-- Copyright   :  2007-2014 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :  Definition of the Preferences dialog
--
-- |
--
-----------------------------------------------------------------------------


module IDE.Preferences (
  readPrefs
, writePrefs
, defaultPrefs
) where

import Prelude ()
import Prelude.Compat
import IDE.Core.State
       (Prefs(..), TallVisibility(..), PrefsFile(..), EditorChoice(..),
        editorChoiceToText, editorChoiceFromText, externalEditor, monacoEditor,
        sysMessage, MessageLevel(..))
import IDE.Gtk.State
       (Color(..), PanePathElement(..), PaneDirection(..))
import System.Time (getClockTime)
import Control.Exception (SomeException)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath (takeFileName)
import qualified Data.Text as T (unpack, pack)
import Distribution.Text (display, simpleParse)
import qualified Control.Exception as E (catch)
import qualified Data.ByteString.Lazy as LBS (writeFile, readFile)
import Data.Aeson (eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)


-- | This needs to be incremented when the preferences format changes
prefsVersion :: Int
prefsVersion = 14

-- | The default preferences
defaultPrefs :: Prefs
defaultPrefs = Prefs {
        prefsFormat         =   prefsVersion
    ,   prefsSaveTime       =   ""
    ,   showLineNumbers     =   True
    ,   rightMargin         =   (True,100)
    ,   tabWidth            =   4
    ,   wrapLines           =   False
    ,   darkUserInterface   = True
    ,   saveSessionOnClose  = True
    ,   keymapName          =   "keymap"
    ,   forceLineEnds       =   True
    ,   removeTBlanks       =   True
    ,   textviewFont        =   Just "Monospace 10"
    ,   monospaceFont       =   "Hasklig, Menlo, monospace"
    ,   monospaceFontSize   =   13
    ,   sourceStyle         =   (True,"leksah")
    ,   foundBackgroundLight      = Color 65535 65535 32768
    ,   matchBackgroundLight      = Color 42064 55923 28520
    ,   contextBackgroundLight    = Color 65535 46529 46529
    ,   breakpointBackgroundLight = Color 64879 51921 28114
    ,   lintBackgroundLight       = Color 60000 65535 60000
    ,   foundBackgroundDark       = Color 30364 29149     0
    ,   matchBackgroundDark       = Color 18021 29927  6384
    ,   contextBackgroundDark     = Color 20000 16000 16000
    ,   breakpointBackgroundDark  = Color 15000  5000  5000
    ,   lintBackgroundDark        = Color     0 15000     0
    ,   textEditorType      =   "GtkSourceView"
    ,   autoLoad            =   False
    ,   logviewFont         =   (False, Nothing)
    ,   workspaceFont       =   (False, Nothing)
    ,   defaultSize         =   (1024,800)
    ,   browser             =   "firefox"
    ,   pathForCategory     =   [   ("ExplorerCategory",[SplitP LeftP])
                                ,   ("EditorCategory",[SplitP RightP])
                                ,   ("ToolCategory",[SplitP RightP,SplitP TopP])
                                ,   ("LogCategory",[SplitP RightP,SplitP BottomP])
                                ]
    ,   defaultPath         =   [SplitP RightP]
    ,   categoryForPane     =   [   ("*Breakpoints","LogCategory")
                                ,   ("*Browser","ToolCategory")
                                ,   ("*Debug","ToolCategory")
                                ,   ("*Errors","LogCategory")
                                ,   ("*Files","ToolCategory")
                                ,   ("*Flags","ToolCategory")
                                ,   ("*Grep","LogCategory")
                                ,   ("*HLint","ToolCategory")
                                ,   ("*Doc","ToolCategory")
                                ,   ("*Info","LogCategory")
                                ,   ("*Log","LogCategory")
                                ,   ("*Inspect","LogCategory")
                                ,   ("*Modules","ToolCategory")
                                ,   ("*Out","ToolCategory")
                                ,   ("*Package","EditorCategory")
                                ,   ("*Prefs","EditorCategory")
                                ,   ("*Search","ToolCategory")
                                ,   ("*Trace","LogCategory")
                                ,   ("*Variables","LogCategory")
                                ,   ("*Workspace","ExplorerCategory")]
    ,   useCtrlTabFlipping  =   True
    ,   saveAllBeforeBuild  =   True
    ,   jumpToWarnings      =   True
    ,   useVado             =   False
    ,   backgroundBuild     =   True
    ,   native              =   True
    ,   javaScript          =   True
    ,   debug               =   True
    ,   makeDocs            =   False
    ,   runUnitTests        =   False
    ,   runBenchmarks       =   False
    ,   makeMode            =   True
    ,   singleBuildWithoutLinking  = False
    ,   dontInstallLast     =   False
    ,   showHiddenFiles     =   False
    ,   showIgnoredFiles    =   False
    ,   tallVisibility      =   TallShow
    ,   wide1Visibility     =   TallShow
    ,   showWorkspaceIcons  =   True
    ,   hlintOnSave = True
    ,   collapseErrors = True
    ,   terminalFileLinks = True
    ,   editorChoice = EditorMonaco
    ,   terminalControlMode = True
    ,   tmuxInterceptPrefix = False
    ,   remoteHosts = []
    ,   uiSelectionColor = "#1e58d1"
    ,   uiHoverColor = "#0c1e46"
    ,   monacoThemeDark = "leksah-github-dark"
    ,   monacoThemeLight = "leksah-github-light"
    ,   codeMirrorThemeDark = "github-dark"
    ,   codeMirrorThemeLight = "github-light"
    ,   xtermThemeDark = "leksah-dark"
    ,   xtermThemeLight = "leksah-light"
    ,   showShortcutBadges = False
    ,   colorfulIcons = False
    ,   regionCaptureTarget = "claude/leksah/0"
    ,   lspEnabled          = True
    ,   lspServerCommand    = ""
    }

mergePrefsFile :: Prefs -> PrefsFile -> Prefs
mergePrefsFile Prefs{..} PrefsFile{..} = Prefs
  { prefsFormat = fromMaybe prefsFormat prefsFormat_
  , prefsSaveTime = fromMaybe prefsSaveTime prefsSaveTime_
  , showLineNumbers = fromMaybe showLineNumbers showLineNumbers_
  , rightMargin = fromMaybe rightMargin rightMargin_
  , tabWidth = fromMaybe tabWidth tabWidth_
  , wrapLines = fromMaybe wrapLines wrapLines_
  , darkUserInterface = fromMaybe darkUserInterface darkUserInterface_
  , saveSessionOnClose = fromMaybe saveSessionOnClose saveSessionOnClose_
  , keymapName = fromMaybe keymapName keymapName_
  , forceLineEnds = fromMaybe forceLineEnds forceLineEnds_
  , removeTBlanks = fromMaybe removeTBlanks removeTBlanks_
  , textviewFont = fromMaybe textviewFont textviewFont_
  , monospaceFont = fromMaybe monospaceFont monospaceFont_
  , monospaceFontSize = fromMaybe monospaceFontSize monospaceFontSize_
  , sourceStyle = fromMaybe sourceStyle sourceStyle_
  , foundBackgroundLight = fromMaybe foundBackgroundLight foundBackgroundLight_
  , matchBackgroundLight = fromMaybe matchBackgroundLight matchBackgroundLight_
  , contextBackgroundLight = fromMaybe contextBackgroundLight contextBackgroundLight_
  , breakpointBackgroundLight = fromMaybe breakpointBackgroundLight breakpointBackgroundLight_
  , lintBackgroundLight = fromMaybe lintBackgroundLight lintBackgroundLight_
  , foundBackgroundDark = fromMaybe foundBackgroundDark foundBackgroundDark_
  , matchBackgroundDark = fromMaybe matchBackgroundDark matchBackgroundDark_
  , contextBackgroundDark = fromMaybe contextBackgroundDark contextBackgroundDark_
  , breakpointBackgroundDark = fromMaybe breakpointBackgroundDark breakpointBackgroundDark_
  , lintBackgroundDark = fromMaybe lintBackgroundDark lintBackgroundDark_
  , textEditorType = fromMaybe textEditorType textEditorType_
  , autoLoad = fromMaybe autoLoad autoLoad_
  , logviewFont = fromMaybe logviewFont logviewFont_
  , workspaceFont = fromMaybe workspaceFont workspaceFont_
  , defaultSize = fromMaybe defaultSize defaultSize_
  , browser = fromMaybe browser browser_
  , pathForCategory = fromMaybe pathForCategory pathForCategory_
  , defaultPath = fromMaybe defaultPath defaultPath_
  , categoryForPane = fromMaybe categoryForPane categoryForPane_
  , useCtrlTabFlipping = fromMaybe useCtrlTabFlipping useCtrlTabFlipping_
  , saveAllBeforeBuild = fromMaybe saveAllBeforeBuild saveAllBeforeBuild_
  , jumpToWarnings = fromMaybe jumpToWarnings jumpToWarnings_
  , useVado = fromMaybe useVado useVado_
  , backgroundBuild = fromMaybe backgroundBuild backgroundBuild_
  , native = fromMaybe native native_
  , javaScript = fromMaybe javaScript javaScript_
  , debug = fromMaybe debug debug_
  , makeDocs = fromMaybe makeDocs makeDocs_
  , runUnitTests = fromMaybe runUnitTests runUnitTests_
  , runBenchmarks = fromMaybe runBenchmarks runBenchmarks_
  , makeMode = fromMaybe makeMode makeMode_
  , singleBuildWithoutLinking = fromMaybe singleBuildWithoutLinking singleBuildWithoutLinking_
  , dontInstallLast = fromMaybe dontInstallLast dontInstallLast_
  , showHiddenFiles = fromMaybe showHiddenFiles showHiddenFiles_
  , showIgnoredFiles = fromMaybe showIgnoredFiles showIgnoredFiles_
  , tallVisibility = tallVisibility  -- session-only (not persisted)
  , wide1Visibility = wide1Visibility  -- session-only (not persisted)
  , showWorkspaceIcons = fromMaybe showWorkspaceIcons showWorkspaceIcons_
  , hlintOnSave = fromMaybe hlintOnSave hlintOnSave_
  , collapseErrors = fromMaybe collapseErrors collapseErrors_
  , terminalFileLinks = fromMaybe terminalFileLinks terminalFileLinks_
  , editorChoice = case editorChoice_ >>= editorChoiceFromText of
      Just c  -> c
      -- Migrate pre-13 prefs: a recognised external-editor command wins, else
      -- the old Monaco/CodeMirror boolean.  An unrecognised command (the enum
      -- can't hold arbitrary commands) falls back to the in-app editor.
      Nothing -> case externalEditor_ >>= editorFromCommand of
        Just c  -> c
        Nothing -> case monacoEditor_ of
          Just True  -> EditorMonaco
          Just False -> EditorCodeMirror
          Nothing    -> editorChoice
  , terminalControlMode = fromMaybe terminalControlMode terminalControlMode_
  , tmuxInterceptPrefix = fromMaybe tmuxInterceptPrefix tmuxInterceptPrefix_
  , uiSelectionColor = fromMaybe uiSelectionColor uiSelectionColor_
  , uiHoverColor = fromMaybe uiHoverColor uiHoverColor_
  , monacoThemeDark = fromMaybe monacoThemeDark monacoThemeDark_
  , monacoThemeLight = fromMaybe monacoThemeLight monacoThemeLight_
  , codeMirrorThemeDark = fromMaybe codeMirrorThemeDark codeMirrorThemeDark_
  , codeMirrorThemeLight = fromMaybe codeMirrorThemeLight codeMirrorThemeLight_
  , xtermThemeDark = fromMaybe xtermThemeDark xtermThemeDark_
  , xtermThemeLight = fromMaybe xtermThemeLight xtermThemeLight_
  , showShortcutBadges = fromMaybe showShortcutBadges showShortcutBadges_
  , colorfulIcons = fromMaybe colorfulIcons colorfulIcons_
  , regionCaptureTarget = fromMaybe regionCaptureTarget regionCaptureTarget_
  , lspEnabled = fromMaybe lspEnabled lspEnabled_
  , lspServerCommand = fromMaybe lspServerCommand lspServerCommand_
  , remoteHosts = fromMaybe remoteHosts remoteHosts_
  }
  where
    -- The old free-form command, mapped onto the enum by the basename of its
    -- first word ("/usr/bin/vim -p" -> vim).
    editorFromCommand cmd = case words (T.unpack cmd) of
      (w:_) -> case takeFileName w of
        "nano"  -> Just EditorNano
        "vi"    -> Just EditorVim
        "vim"   -> Just EditorVim
        "nvim"  -> Just EditorVim
        "emacs" -> Just EditorEmacs
        _       -> Nothing
      []    -> Nothing

toPrefsFile :: Prefs -> PrefsFile
toPrefsFile p@Prefs{..} = PrefsFile
  { prefsFormat_ = Just prefsFormat
  , prefsSaveTime_ = Just prefsSaveTime
  , showLineNumbers_ = Just showLineNumbers
  , rightMargin_ = Just rightMargin
  , tabWidth_ = Just tabWidth
  , wrapLines_ = Just wrapLines
  , darkUserInterface_ = Just darkUserInterface
  , saveSessionOnClose_ = Just saveSessionOnClose
  , keymapName_ = Just keymapName
  , forceLineEnds_ = Just forceLineEnds
  , removeTBlanks_ = Just removeTBlanks
  , textviewFont_ = Just textviewFont
  , monospaceFont_ = Just monospaceFont
  , monospaceFontSize_ = Just monospaceFontSize
  , sourceStyle_ = Just sourceStyle
  , foundBackgroundLight_ = Just foundBackgroundLight
  , matchBackgroundLight_ = Just matchBackgroundLight
  , contextBackgroundLight_ = Just contextBackgroundLight
  , breakpointBackgroundLight_ = Just breakpointBackgroundLight
  , lintBackgroundLight_ = Just lintBackgroundLight
  , foundBackgroundDark_ = Just foundBackgroundDark
  , matchBackgroundDark_ = Just matchBackgroundDark
  , contextBackgroundDark_ = Just contextBackgroundDark
  , breakpointBackgroundDark_ = Just breakpointBackgroundDark
  , lintBackgroundDark_ = Just lintBackgroundDark
  , textEditorType_ = Just textEditorType
  , autoLoad_ = Just autoLoad
  , logviewFont_ = Just logviewFont
  , workspaceFont_ = Just workspaceFont
  , defaultSize_ = Just defaultSize
  , browser_ = Just browser
  , pathForCategory_ = Just pathForCategory
  , defaultPath_ = Just defaultPath
  , categoryForPane_ = Just categoryForPane
  , useCtrlTabFlipping_ = Just useCtrlTabFlipping
  , saveAllBeforeBuild_ = Just saveAllBeforeBuild
  , jumpToWarnings_ = Just jumpToWarnings
  , useVado_ = Just useVado
  , backgroundBuild_ = Just backgroundBuild
  , native_ = Just native
  , javaScript_ = Just javaScript
  , debug_ = Just debug
  , makeDocs_ = Just makeDocs
  , runUnitTests_ = Just runUnitTests
  , runBenchmarks_ = Just runBenchmarks
  , makeMode_ = Just makeMode
  , singleBuildWithoutLinking_ = Just singleBuildWithoutLinking
  , dontInstallLast_ = Just dontInstallLast
  , showHiddenFiles_ = Just showHiddenFiles
  , showIgnoredFiles_ = Just showIgnoredFiles
  , showWorkspaceIcons_ = Just showWorkspaceIcons
  , hlintOnSave_ = Just hlintOnSave
  , collapseErrors_ = Just collapseErrors
  , terminalFileLinks_ = Just terminalFileLinks
  -- Legacy mirrors of editorChoice, so an older leksah reading this file
  -- still lands on a sensible editor.
  , externalEditor_ = Just (externalEditor p)
  , monacoEditor_ = Just (monacoEditor p)
  , editorChoice_ = Just (editorChoiceToText editorChoice)
  , terminalControlMode_ = Just terminalControlMode
  , tmuxInterceptPrefix_ = Just tmuxInterceptPrefix
  , uiSelectionColor_ = Just uiSelectionColor
  , uiHoverColor_ = Just uiHoverColor
  , monacoThemeDark_ = Just monacoThemeDark
  , monacoThemeLight_ = Just monacoThemeLight
  , codeMirrorThemeDark_ = Just codeMirrorThemeDark
  , codeMirrorThemeLight_ = Just codeMirrorThemeLight
  , xtermThemeDark_ = Just xtermThemeDark
  , xtermThemeLight_ = Just xtermThemeLight
  , showShortcutBadges_ = Just showShortcutBadges
  , colorfulIcons_ = Just colorfulIcons
  , regionCaptureTarget_ = Just regionCaptureTarget
  , lspEnabled_ = Just lspEnabled
  , lspServerCommand_ = Just lspServerCommand
  , remoteHosts_ = Just remoteHosts
  }

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

-- | Read the preference file
readPrefs :: FilePath -> IO Prefs
readPrefs file = E.catch (
    eitherDecode <$> LBS.readFile file >>= \case
        Left e -> do
            sysMessage Normal . T.pack $  "Error reading file " ++ show file ++ " " ++ show e
            return defaultPrefs
        Right r -> return $ mergePrefsFile defaultPrefs r)
        (\ (e :: SomeException) -> do
            sysMessage Normal . T.pack $ show e
            return defaultPrefs)

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

-- | Write the preference file
writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = do
    timeNow         <- liftIO getClockTime
    let newPrefs    =   prefs {prefsSaveTime = T.pack $ show timeNow, prefsFormat = prefsVersion}
    LBS.writeFile fpath . encodePretty $ toPrefsFile newPrefs


