{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  runPreferencesDialog
, applyInterfaceTheme
, readPrefs
, writePrefs
, defaultPrefs
) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Distribution.Package
import Data.IORef
import Data.Typeable

import Control.Event
import Graphics.UI.Editor.Basics
import IDE.Core.State
import Graphics.UI.Editor.Simple
import Graphics.UI.Editor.Composite
import Graphics.UI.Editor.Parameters
import Graphics.UI.Editor.MakeEditor hiding (parameters)
import Graphics.UI.Editor.DescriptionPP
import IDE.TextEditor
import IDE.Pane.SourceBuffer
import IDE.Pane.Log
import IDE.Pane.Info (setInfoStyle)
import IDE.Utils.FileUtils
import IDE.Utils.GUIUtils
import IDE.Pane.Workspace
import IDE.Debug
    (debugSetPrintBindResult,
     debugSetBreakOnError,
     debugSetBreakOnException,
     debugSetPrintEvldWithShow)
import System.Time (getClockTime)
import qualified IDE.StrippedPrefs as SP
import Control.Exception (IOException, SomeException, catch)
import Prelude hiding(catch)
import Data.List (isSuffixOf, sortBy)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, isJust)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (void, when)
import System.FilePath ((</>))
import Data.Text (Text)
import qualified Data.Text as T (isSuffixOf, unpack, pack, null)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Distribution.Text (display, simpleParse)
import Data.Foldable (forM_)
import IDE.Pane.Errors (fillErrorList)
import GHC.IO (evaluate)
import IDE.Metainfo.Provider (getAllPackageIds)
import GI.Gtk.Objects.Dialog
       (Dialog(..), onDialogResponse, dialogResponse, dialogGetHeaderBar,
        dialogGetContentArea, dialogAddActionWidget)
import Data.GI.Base (new', unsafeCastTo, set)
import GI.Gtk.Objects.Window
       (windowSetDefaultSize, setWindowTitle, setWindowTransientFor)
import GI.Gtk.Objects.HButtonBox (hButtonBoxNew)
import GI.Gtk.Objects.Box (Box(..), boxSetSpacing)
import GI.Gtk.Objects.ButtonBox (buttonBoxSetLayout)
import GI.Gtk.Enums
       (Orientation(..), FileChooserAction(..), ShadowType(..),
        ResponseType(..), ButtonBoxStyle(..))
import GI.Gtk.Objects.Button
       (onButtonClicked, buttonNewWithLabel, Button (..))
import GI.Gtk.Objects.Label (labelSetMarkup, labelNew)
import GI.Gtk.Objects.Widget
       (widgetModifyFont, widgetDestroy, widgetShowAll, widgetGrabDefault,
        setWidgetCanDefault, widgetSetSensitive, widgetShow)
import GI.Gtk.Objects.VBox (VBox(..))
import GI.Pango.Structs.FontDescription (fontDescriptionFromString)
import GI.Gtk.Objects.CellRendererText (setCellRendererTextText)
import GI.Gtk.Objects.Settings
       (settingsSetLongProperty, settingsGetForScreen, Settings(..))
import GI.GtkSource
       (styleSchemeManagerGetSchemeIds,
        styleSchemeManagerAppendSearchPath, styleSchemeManagerNew)
import GI.Gtk
       (widgetOverrideFont, constructDialogUseHeaderBar, Container(..),
        containerAdd)
import qualified Control.Exception as E (catch)
import qualified Data.ByteString.Lazy as LBS (writeFile, readFile)
import Data.Aeson (encode, eitherDecode)
import Data.Default (Default(..))


-- | This needs to be incremented when the preferences format changes
prefsVersion :: Int
prefsVersion = 12

runPreferencesDialog :: IDEAction
runPreferencesDialog = do
  packageInfos <- getAllPackageIds
  liftIO $ print packageInfos
  initialPrefs <- readIDE prefs
  parent <- getMainWindow
  reifyIDE $ \ideR -> do
    dialog <-   new' Dialog [constructDialogUseHeaderBar 1]
    setWindowTransientFor dialog parent
    setWindowTitle dialog $ __ "Preferences"
    windowSetDefaultSize dialog 800 500

    configDir    <- getConfigDir
    (widget, inj, ext, notifier) <- buildEditor (extractFieldDescription $ prefsDescription configDir packageInfos) initialPrefs

    actionArea <- dialogGetHeaderBar dialog >>= unsafeCastTo Container
    preview <- buttonNewWithLabel (__ "Preview")
    containerAdd actionArea preview
    apply   <-  dialogAddButton' dialog "gtk-apply" ResponseTypeOk >>= unsafeCastTo Button
    dialogSetDefaultResponse' dialog ResponseTypeOk

    upper <-   dialogGetContentArea dialog >>= unsafeCastTo Box
    boxPackStart' upper widget PackGrow 0
    errorLabel <-  labelNew Nothing
    boxPackStart' upper errorLabel PackNatural 0

    -- Keep an IO ref to the last applied preferences
    -- so it can be restored
    lastAppliedPrefsRef <- liftIO $ newIORef initialPrefs
    let flatPrefsDesc = flattenFieldDescriptionPP (prefsDescription configDir packageInfos)
    let applyPrefs newPrefs = do
            lastAppliedPrefs <- readIORef lastAppliedPrefsRef
            reflectIDE (modifyIDE_ (\ide -> ide{prefs = newPrefs})) ideR
            mapM_ (\f -> reflectIDE (applicator f newPrefs lastAppliedPrefs) ideR) flatPrefsDesc
            writeIORef lastAppliedPrefsRef newPrefs


    onButtonClicked preview $ do
            mbNewPrefs <- extract initialPrefs [ext]
            forM_ mbNewPrefs applyPrefs


    onButtonClicked apply $ do
        mbNewPrefs <- extract initialPrefs [ext]
        forM_ mbNewPrefs $ \newPrefs -> do
            applyPrefs newPrefs

            -- save preferences to disk
            fp   <- getConfigFilePathForSave standardPreferencesFilename
            writePrefs fp newPrefs
            fp2  <-  getConfigFilePathForSave strippedPreferencesFilename
            SP.writeStrippedPrefs fp2
                SP.Prefs {SP.sourceDirectories = sourceDirectories newPrefs,
                           SP.unpackDirectory   = unpackDirectory newPrefs,
                           SP.retrieveURL       = retrieveURL newPrefs,
                           SP.retrieveStrategy  = retrieveStrategy newPrefs,
                           SP.serverPort        = serverPort newPrefs,
                           SP.endWithLastConn   = endWithLastConn newPrefs}

        dialogResponse' dialog ResponseTypeOk

    let onClose = do
            mbP <- extract initialPrefs [ext]
            let hasChanged = case mbP of
                                    Nothing -> False
                                    Just p -> p{prefsFormat = 0, prefsSaveTime = ""} /=
                                              initialPrefs{prefsFormat = 0, prefsSaveTime = ""}
            when hasChanged (applyPrefs initialPrefs)
            dialogResponse' dialog ResponseTypeCancel

--    onButtonClicked cancel onClose
    onDialogResponse dialog $ \resp ->
        case toEnum $ fromIntegral resp of
            ResponseTypeDeleteEvent -> onClose
            _ -> return ()

    registerEvent notifier MayHaveChanged (\ e -> do
        mbP <- extract initialPrefs [ext]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p{prefsFormat = 0, prefsSaveTime = ""} /=
                                          initialPrefs{prefsFormat = 0, prefsSaveTime = ""}
        when (isJust mbP) $ do
            labelSetMarkup errorLabel ""
            widgetSetSensitive apply True
        return (e{gtkReturn=False}))

    registerEvent notifier ValidationError $ \e -> do
        labelSetMarkup errorLabel $ "<span foreground=\"red\">The following fields have invalid values: "
            <> eventText e <> "</span>"
        widgetSetSensitive apply False
        return e


    setWidgetCanDefault apply True
    widgetGrabDefault apply
    widgetShowAll dialog
    resp  <- dialogRun' dialog
    widgetDestroy dialog
    return ()


-- | Represents the Preferences dialog
data PreferencesDialog = PreferencesDialog {
    editorsBox ::   VBox
} deriving Typeable


-- ------------------------------------------------------------
-- * Dialog definition
-- ------------------------------------------------------------

-- | Description of the preference options
prefsDescription :: FilePath -> [PackageIdentifier] -> FieldDescriptionPP Prefs IDEM
prefsDescription configDir packages = NFDPP [
    (__ "Editor", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName (__ "Version number of preferences file format")
                $ paraSynopsis <<<- ParaSynopsis (__ "Integer")
                    $ paraShowLabel <<<- ParaShowLabel False $ emptyParams)
            prefsFormat
            (\ b a -> a{prefsFormat = b})
            (noEditor 0)
            (\b -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Time of last storage")
                $ paraShowLabel <<<- ParaShowLabel False $ emptyParams)
            prefsSaveTime
            (\ b a -> a{prefsSaveTime = b})
            (noEditor "")
            (\b -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Show line numbers")
                $ paraSynopsis <<<- ParaSynopsis (__ "(True/False)")
                    $ emptyParams)
            showLineNumbers
            (\ b a -> a{showLineNumbers = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setShowLineNumbers sv b) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "TextView Font") $ emptyParams)
            textviewFont
            (\ b a -> a{textviewFont = b})
            fontEditor
            (\mbs -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setFont sv mbs) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Right margin")
                $ paraSynopsis <<<- ParaSynopsis (__ "Size or 0 for no right margin")
                    $ paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
            rightMargin
            (\b a -> a{rightMargin = b})
            (disableEditor def (intEditor (1.0, 200.0, 5.0), paraName <<<- ParaName (__ "Position")
                    $ emptyParams)
                    True (__ "Show it ?"))
            (\b -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setRightMargin sv
                                (case b of
                                    (True,v) -> Just v
                                    (False,_) -> Nothing)) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Tab width") $ emptyParams)
            tabWidth
            (\b a -> a{tabWidth = b})
            (intEditor (1.0, 20.0, 1.0))
            (\i -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setIndentWidth sv i) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Wrap lines") $ emptyParams)
            wrapLines
            (\b a -> a{wrapLines = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setWrapMode sv b) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Use standard line ends even on windows") $ emptyParams)
            forceLineEnds
            (\b a -> a{forceLineEnds = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Remove trailing blanks when saving a file") $ emptyParams)
            removeTBlanks
            (\b a -> a{removeTBlanks = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Source candy")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "Empty for do not use or the name of a candy file in a config dir")
                    $ paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
            sourceCandy (\b a -> a{sourceCandy = b})
            (disableEditor "" (textEditor (not . T.null) True,
                paraName <<<- ParaName (__ "Candy specification")
                                    $ emptyParams)
                    True (__ "Use it ?"))
            (\_ -> switchBuffersCandy)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Editor Style") $ emptyParams)
            sourceStyle
            (\b a -> a{sourceStyle = b})
            styleEditor
            (\mbs -> do
                setInfoStyle
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Found Text Background") $ emptyParams)
            foundBackgroundLight
            (\ b a -> a{foundBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Selection Match Text Background") $ emptyParams)
            matchBackgroundLight
            (\ b a -> a{matchBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Execution Context Text Background") $ emptyParams)
            contextBackgroundLight
            (\ b a -> a{contextBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Breakpoint Text Background") $ emptyParams)
            breakpointBackgroundLight
            (\ b a -> a{breakpointBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Lint Text Background") $ emptyParams)
            lintBackgroundLight
            (\ b a -> a{lintBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Found Text Dark Background") $ emptyParams)
            foundBackgroundDark
            (\ b a -> a{foundBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Selection Match Text Dark Background") $ emptyParams)
            matchBackgroundDark
            (\ b a -> a{matchBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Execution Context Text Dark Background") $ emptyParams)
            contextBackgroundDark
            (\ b a -> a{contextBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Breakpoint Text Dark Background") $ emptyParams)
            breakpointBackgroundDark
            (\ b a -> a{breakpointBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Lint Text Dark Background") $ emptyParams)
            lintBackgroundDark
            (\ b a -> a{lintBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Automatically load modified files modified outside of Leksah") $ emptyParams)
            autoLoad
            (\b a -> a{autoLoad = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Text Editor") $ emptyParams)
            textEditorType
            (\b a -> a{textEditorType = b})
            (comboSelectionEditor
                ["GtkSourceView"]
#ifdef LEKSAH_WITH_YI
                ++ ["Yi"]
#endif
#ifdef LEKSAH_WITH_CODE_MIRROR
                ++ ["CodeMirror"]
#endif
                id)
            (\i -> return ())
    ]),
    (__ "User Interface", VFDPP emptyParams [
        mkFieldPP
            -- TODO: be able to load different gtk themes
            (paraName <<<- ParaName (__ "User interface theme") $ emptyParams)
            (\prefs -> if darkUserInterface prefs then "Dark" else "Light")
            (\str a -> a {darkUserInterface = str == "Dark"})
            (comboSelectionEditor ["Dark", "Light"] id)
            (\_ -> applyInterfaceTheme)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "LogView Font") $ emptyParams)
            logviewFont
            (\b a -> a{logviewFont = b})
            (disableEditor def (fontEditor, emptyParams) True "Use custom font")
            (\(b, mbFont) -> do
                log <- getLog
                fdesc <- fontDescriptionFromString (if b then fromMaybe "" mbFont else "Monospace")
                widgetOverrideFont (logLaunchTextView log) (Just fdesc)
            )
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Workspace Font") $ emptyParams)
            workspaceFont
            (\b a -> a{workspaceFont = b})
            (disableEditor def (fontEditor, emptyParams) True "Use custom font")
            (\(b, mbFont) -> do
                wp <- getWorkspacePane
                case (b, mbFont) of
                    (True, Just font) -> do
                        fdesc <- fontDescriptionFromString (if b then fromMaybe "" mbFont else "")
                        widgetOverrideFont (treeView wp) (Just fdesc)
                    _ -> do
                        widgetOverrideFont (treeView wp) Nothing
            )
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Window default size")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "Default size of the main ide window specified as pair")
                $ paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
            defaultSize (\(c,d) a -> a{defaultSize = (c,d)})
            (pairEditor (intEditor (0.0, 3000.0, 25.0),
                            paraName <<<- ParaName "X" $ emptyParams)
                        (intEditor (0.0, 3000.0, 25.0),
                            paraName <<<- ParaName "Y" $ emptyParams))
            (\a -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Show hidden files in workspace") $ emptyParams)
            showHiddenFiles
            (\b a -> a {showHiddenFiles = b})
            boolEditor
            (\_ -> refreshWorkspacePane)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Show icons in the Workspace pane") $ emptyParams)
            showWorkspaceIcons
            (\b a -> a {showWorkspaceIcons = b})
            boolEditor
            (\_ -> rebuildWorkspacePane)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Collapse errors in Errors pane by default") $ emptyParams)
            collapseErrors
            (\b a -> a {collapseErrors = b})
            boolEditor
            (\_ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Use ctrl Tab for Notebook flipper") $ emptyParams)
            useCtrlTabFlipping
            (\b a -> a{useCtrlTabFlipping = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Complete only on Hotkey") $ emptyParams)
            completeRestricted
            (\b a -> a{completeRestricted = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Name of the keymap")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "The name of a keymap file in a config dir")
                    $ paraOrientation <<<- ParaOrientation OrientationHorizontal $ emptyParams)
            keymapName
            (\b a -> a{keymapName = b})
            (textEditor (not . T.null) True)
            (\ a -> return ())
    ]),
    (__ "Sessions", VFDPP emptyParams [
         mkFieldPP
            (paraName <<<- ParaName (__ "Save the session (open files, pane positioning and sizing, etc) before closing a workspace") $
                paraSynopsis <<<- ParaSynopsis
                    (__ "Save the session (open files, pane positioning and sizing, etc) before closing a workspace")
                    $ paraShadow <<<- ParaShadow ShadowTypeIn  $ emptyParams)
            saveSessionOnClose
            (\b a -> a{saveSessionOnClose = b})
            boolEditor
            (\_ -> return ())
    ]),
    (__ "Initial Pane positions", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName
                (__ "Categories for panes")
                $ paraShadow <<<- ParaShadow ShadowTypeIn
                     $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,130)
                            $ emptyParams)
            categoryForPane
            (\b a -> a{categoryForPane = b})
            (multisetEditor ("", "")
                (ColumnDescr True [(__ "Pane Id", \cell (n, _) -> setCellRendererTextText cell n)
                                   ,(__ "Pane Category", \cell (_, v) -> setCellRendererTextText cell v)])
                (pairEditor
                    (textEditor (not . T.null) True, emptyParams)
                    (textEditor (not . T.null) True, emptyParams), emptyParams)
                (Just (sortBy (\(a,_) (a2,_) -> compare a a2)))
                (Just (\(a,_) (a2,_) -> a == a2)))
            (\i -> return ())
       ,mkFieldPP
            (paraName <<<- ParaName
                (__ "Pane path for category")
                $ paraShadow <<<- ParaShadow ShadowTypeIn
                     $ paraOrientation <<<- ParaOrientation OrientationVertical
                        $ paraMinSize <<<- ParaMinSize (-1,130)
                            $ emptyParams)
            pathForCategory
            (\b a -> a{pathForCategory = b})
            (multisetEditor ("", def)
                (ColumnDescr True [(__ "Pane category", \cell (n, _) -> setCellRendererTextText cell n)
                                   ,(__ "Pane path", \cell (_, v) -> setCellRendererTextText cell . T.pack $ show v)])
                (pairEditor (textEditor (not . T.null) True, emptyParams)
                    (genericEditor, emptyParams),
                  emptyParams)
                (Just (sortBy (\(a,_) (a2,_) -> compare a a2)))
                (Just (\(a,_) (a2,_) -> a == a2)))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Default pane path") $ emptyParams)
            defaultPath
            (\b a -> a{defaultPath = b})
            genericEditor
            (\i -> return ())
    ]),
    (__ "Metadata", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName
                (__ "Paths under which haskell sources for packages may be found")
                        $ paraMinSize <<<- ParaMinSize (-1,100)
                            $ emptyParams)
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder (__ "Select folder"))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Unpack source for cabal packages to") $ emptyParams)
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
            (maybeEditor "" (stringEditor (const True) True,emptyParams) True "")
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "URL from which to download prebuilt metadata") $ emptyParams)
            retrieveURL
            (\b a -> a{retrieveURL = b})
            (textEditor (const True) True)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Strategy for downloading prebuilt metadata") $ emptyParams)
            retrieveStrategy
            (\b a -> a{retrieveStrategy = b})
            (enumEditor [__ "Try to download and then build locally if that fails",
                         __ "Try to build locally and then download if that fails",
                         __ "Never download (just try to build locally)"])
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Update metadata at startup") $ emptyParams)
            collectAtStart
            (\b a -> a{collectAtStart = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Port number for leksah to comunicate with leksah-server") $ emptyParams)
            serverPort
            (\b a -> a{serverPort = b})
            (intEditor (1.0, 65535.0, 1.0))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "IP address for leksah to comunicate with leksah-server") $ emptyParams)
            serverIP
            (\b a -> a{serverIP = b})
            (textEditor (not . T.null) True)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Stop the leksah-server process when leksah disconnects") $ emptyParams)
            endWithLastConn
            (\b a -> a{endWithLastConn = b})
            boolEditor
            (\i -> return ())
    ]),
    (__ "Blacklist", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName
                (__ "Packages which are excluded from the modules pane")
                        $ paraMinSize <<<- ParaMinSize (-1,200)
                            $ emptyParams)
            packageBlacklist
            (\b a -> a{packageBlacklist = b})
            (dependenciesEditor packages)
            (\i -> return ())
    ]),
    (__ "Build", VFDPP emptyParams [
         mkFieldPP
            (paraName <<<- ParaName (__ "Automatically save all files before building") $ emptyParams)
            saveAllBeforeBuild
            (\b a -> a{saveAllBeforeBuild = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
               (paraName <<<- ParaName (__ "Run HLint when saving a source file") $ emptyParams)
               hlintOnSave
               (\b a -> a{hlintOnSave = b})
               boolEditor
               (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Select first warning if built without errors") $ emptyParams)
            jumpToWarnings
            (\b a -> a{jumpToWarnings = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Background build") $ emptyParams)
            backgroundBuild
            (\b a -> a{backgroundBuild = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Native") $ emptyParams)
            native
            (\b a -> a{native = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "JavaScript") $ emptyParams)
            javaScript
            (\b a -> a{javaScript = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Debug") $ emptyParams)
            debug
            (\b a -> a{debug = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Make documentation when building") $ emptyParams)
            makeDocs
            (\b a -> a{makeDocs = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Run unit tests when building") $ emptyParams)
            runUnitTests
            (\b a -> a{runUnitTests = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Run benchmarks when building") $ emptyParams)
            runBenchmarks
            (\b a -> a{runBenchmarks = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Make mode") $ emptyParams)
            makeMode
            (\b a -> a{makeMode = b})
            (boolEditor2 (__ "Single mode"))
            (\i -> return ())

         , mkFieldPP
            (paraName <<<- ParaName (__ "Single build without linking") $ emptyParams)
            singleBuildWithoutLinking
            (\b a -> a{singleBuildWithoutLinking = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Don't install last package") $ emptyParams)
            dontInstallLast
            (\b a -> a{dontInstallLast = b})
            boolEditor
            (\i -> return ())

        , mkFieldPP
            (paraName <<<- ParaName (__ "Use vado to run commands on the remote machine") $ emptyParams)
            useVado
            (\b a -> a{useVado = b})
            boolEditor
            (\i -> return ())
    ]),
    (__ "Debug", VFDPP emptyParams [
           mkFieldPP
            (paraName <<<- ParaName (__ "Enable usage of Show instances in :print") $ emptyParams)
            printEvldWithShow
            (\b a -> a{printEvldWithShow = b})
            boolEditor
            debugSetPrintEvldWithShow
         , mkFieldPP
            (paraName <<<- ParaName (__ "Break on any exception thrown") $ emptyParams)
            breakOnException
            (\b a -> a{breakOnException = b})
            boolEditor
            debugSetBreakOnException
         , mkFieldPP
            (paraName <<<- ParaName (__ "Break on uncaught exceptions and errors") $ emptyParams)
            breakOnError
            (\b a -> a{breakOnError = b})
            boolEditor
            debugSetBreakOnError
         , mkFieldPP
            (paraName <<<- ParaName (__ "Turn on printing of binding results in GHCi") $ emptyParams)
            printBindResult
            (\b a -> a{printBindResult = b})
            boolEditor
            debugSetPrintBindResult
    ]),
    (__ "Help", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName (__ "Browser") $ emptyParams)
            browser
            (\b a -> a{browser = b})
            (textEditor (not . T.null) True)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "URL for searching documentation") $
                paraSynopsis <<<- ParaSynopsis
                    ("e.g https://www.haskell.org/hoogle/?q= or " <>
                        "Hayoo: http://hayoo.fh-wedel.de/?query=")
                        $ emptyParams)
            docuSearchURL
            (\b a -> a{docuSearchURL = b})
            (textEditor (not . T.null) True)
            (\i -> return ())
    ])]

getActiveSettings :: PaneMonad alpha => alpha (Maybe Settings)
getActiveSettings = do
    mbScreen <- getActiveScreen
    case mbScreen of
        Nothing -> return Nothing
        Just screen -> Just <$> settingsGetForScreen screen

applyInterfaceTheme :: IDEAction
applyInterfaceTheme = do
    setInfoStyle
    fillErrorList False
    prefs <- readIDE prefs
    buffers <- allBuffers
    mapM_ updateStyle' buffers
    mbSettings <- getActiveSettings
    case mbSettings of
        Just settings -> settingsSetLongProperty
                            settings
                            "gtk-application-prefer-dark-theme"
                            (if darkUserInterface prefs then 1 else 0)
                            "Leksah"
        Nothing -> return ()
    menuDarkState <- getDarkState
    when (menuDarkState /= darkUserInterface prefs) $
        setDarkState (darkUserInterface prefs)

-- | Editor for enabling a different syntax stylesheet
styleEditor :: Editor (Bool, Text)
styleEditor p n = do
    styleManager <- styleSchemeManagerNew
    dataDir <- getDataDir
    styleSchemeManagerAppendSearchPath styleManager . T.pack $ dataDir </> "data/styles"
    ids <- fromMaybe [] <$> styleSchemeManagerGetSchemeIds styleManager
    let notDarkIds = filter (not . T.isSuffixOf "-dark") ids
    disableEditor "" (comboSelectionEditor notDarkIds id, p) True (__ "Select a special style?") p n


-- | The default preferences
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
    configDir <- getConfigDir
    let newPrefs    =   prefs {prefsSaveTime = T.pack $ show timeNow, prefsFormat = prefsVersion}
    LBS.writeFile fpath . encode $ toPrefsFile prefs


