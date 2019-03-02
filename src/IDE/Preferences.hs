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
       (Prefs(..), PrefsFile(..), sysMessage, MessageLevel(..))
import IDE.Gtk.State
       (Color(..), PanePathElement(..), PaneDirection(..))
import System.Time (getClockTime)
import qualified IDE.StrippedPrefs as SP
import Control.Exception (SomeException)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath ((</>))
import qualified Data.Text as T (unpack, pack)
import Distribution.Text (display, simpleParse)
import qualified Control.Exception as E (catch)
import qualified Data.ByteString.Lazy as LBS (writeFile, readFile)
import Data.Aeson (eitherDecode)
import IDE.Core.CTypes (configDirName)
import Data.Aeson.Encode.Pretty (encodePretty)


-- | This needs to be incremented when the preferences format changes
prefsVersion :: Int
prefsVersion = 12

-- | The default preferences
defaultPrefs :: Prefs
defaultPrefs = Prefs {
        prefsFormat         =   prefsVersion
    ,   prefsSaveTime       =   ""
    ,   showLineNumbers     =   True
    ,   rightMargin         =   (True,100)
    ,   tabWidth            =   4
    ,   wrapLines           =   False
    ,   sourceCandy         =   (False,"candy")
    ,   darkUserInterface   = True
    ,   saveSessionOnClose  = True
    ,   keymapName          =   "keymap"
    ,   forceLineEnds       =   True
    ,   removeTBlanks       =   True
    ,   textviewFont        =   Just "Monospace 10"
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
    ,   sourceDirectories   =   []
    ,   packageBlacklist    =   []
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
    ,   collectAtStart      =   True
    ,   unpackDirectory     =   Just ("~" </> configDirName </> "packageSources")
    ,   retrieveURL         =   "http://leksah.github.io"
    ,   retrieveStrategy    =   SP.RetrieveThenBuild
    ,   useCtrlTabFlipping  =   True
    ,   docuSearchURL       =   "https://www.haskell.org/hoogle/?q="
    ,   completeRestricted  =   False
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
    ,   printEvldWithShow   =   True
    ,   breakOnException    =   True
    ,   breakOnError        =   True
    ,   printBindResult     =   False
    ,   serverPort          =   11111
    ,   serverIP            =   "127.0.0.1"
    ,   endWithLastConn     =   True
    ,   showHiddenFiles     =   False
    ,   showWorkspaceIcons  =   True
    ,   hlintOnSave = True
    ,   collapseErrors = True
    }

mergePrefsFile :: Prefs -> PrefsFile -> Prefs
mergePrefsFile Prefs{..} PrefsFile{..} = Prefs
  { prefsFormat = fromMaybe prefsFormat prefsFormat_
  , prefsSaveTime = fromMaybe prefsSaveTime prefsSaveTime_
  , showLineNumbers = fromMaybe showLineNumbers showLineNumbers_
  , rightMargin = fromMaybe rightMargin rightMargin_
  , tabWidth = fromMaybe tabWidth tabWidth_
  , wrapLines = fromMaybe wrapLines wrapLines_
  , sourceCandy = fromMaybe sourceCandy sourceCandy_
  , darkUserInterface = fromMaybe darkUserInterface darkUserInterface_
  , saveSessionOnClose = fromMaybe saveSessionOnClose saveSessionOnClose_
  , keymapName = fromMaybe keymapName keymapName_
  , forceLineEnds = fromMaybe forceLineEnds forceLineEnds_
  , removeTBlanks = fromMaybe removeTBlanks removeTBlanks_
  , textviewFont = fromMaybe textviewFont textviewFont_
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
  , sourceDirectories = fromMaybe sourceDirectories sourceDirectories_
  , packageBlacklist = fromMaybe packageBlacklist (packageBlacklist_ >>= mapM (simpleParse . T.unpack))
  , pathForCategory = fromMaybe pathForCategory pathForCategory_
  , defaultPath = fromMaybe defaultPath defaultPath_
  , categoryForPane = fromMaybe categoryForPane categoryForPane_
  , collectAtStart = fromMaybe collectAtStart collectAtStart_
  , unpackDirectory = fromMaybe unpackDirectory unpackDirectory_
  , retrieveURL = fromMaybe retrieveURL retrieveURL_
  , retrieveStrategy = fromMaybe retrieveStrategy retrieveStrategy_
  , useCtrlTabFlipping = fromMaybe useCtrlTabFlipping useCtrlTabFlipping_
  , docuSearchURL = fromMaybe docuSearchURL docuSearchURL_
  , completeRestricted = fromMaybe completeRestricted completeRestricted_
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
  , printEvldWithShow = fromMaybe printEvldWithShow printEvldWithShow_
  , breakOnException = fromMaybe breakOnException breakOnException_
  , breakOnError = fromMaybe breakOnError breakOnError_
  , printBindResult = fromMaybe printBindResult printBindResult_
  , serverPort = fromMaybe serverPort serverPort_
  , serverIP = fromMaybe serverIP serverIP_
  , endWithLastConn = fromMaybe endWithLastConn endWithLastConn_
  , showHiddenFiles = fromMaybe showHiddenFiles showHiddenFiles_
  , showWorkspaceIcons = fromMaybe showWorkspaceIcons showWorkspaceIcons_
  , hlintOnSave = fromMaybe hlintOnSave hlintOnSave_
  , collapseErrors = fromMaybe collapseErrors collapseErrors_
  }

toPrefsFile :: Prefs -> PrefsFile
toPrefsFile Prefs{..} = PrefsFile
  { prefsFormat_ = Just prefsFormat
  , prefsSaveTime_ = Just prefsSaveTime
  , showLineNumbers_ = Just showLineNumbers
  , rightMargin_ = Just rightMargin
  , tabWidth_ = Just tabWidth
  , wrapLines_ = Just wrapLines
  , sourceCandy_ = Just sourceCandy
  , darkUserInterface_ = Just darkUserInterface
  , saveSessionOnClose_ = Just saveSessionOnClose
  , keymapName_ = Just keymapName
  , forceLineEnds_ = Just forceLineEnds
  , removeTBlanks_ = Just removeTBlanks
  , textviewFont_ = Just textviewFont
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
  , sourceDirectories_ = Just sourceDirectories
  , packageBlacklist_ = Just (map (T.pack . display) packageBlacklist)
  , pathForCategory_ = Just pathForCategory
  , defaultPath_ = Just defaultPath
  , categoryForPane_ = Just categoryForPane
  , collectAtStart_ = Just collectAtStart
  , unpackDirectory_ = Just unpackDirectory
  , retrieveURL_ = Just retrieveURL
  , retrieveStrategy_ = Just retrieveStrategy
  , useCtrlTabFlipping_ = Just useCtrlTabFlipping
  , docuSearchURL_ = Just docuSearchURL
  , completeRestricted_ = Just completeRestricted
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
  , printEvldWithShow_ = Just printEvldWithShow
  , breakOnException_ = Just breakOnException
  , breakOnError_ = Just breakOnError
  , printBindResult_ = Just printBindResult
  , serverPort_ = Just serverPort
  , serverIP_ = Just serverIP
  , endWithLastConn_ = Just endWithLastConn
  , showHiddenFiles_ = Just showHiddenFiles
  , showWorkspaceIcons_ = Just showWorkspaceIcons
  , hlintOnSave_ = Just hlintOnSave
  , collapseErrors_ = Just collapseErrors
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


