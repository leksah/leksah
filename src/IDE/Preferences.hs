{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
import Text.PrinterParser hiding (fieldParser,parameters)
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
import Control.Exception(SomeException,catch)
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
       (onDialogResponse, dialogResponse, dialogGetActionArea,
        dialogGetContentArea, dialogNew)
import Data.GI.Base (unsafeCastTo, set, nullToNothing)
import GI.Gtk.Objects.Window
       (windowSetDefaultSize, setWindowTitle, setWindowTransientFor)
import GI.Gtk.Objects.HButtonBox (hButtonBoxNew)
import GI.Gtk.Objects.Box (Box(..), boxSetSpacing)
import GI.Gtk.Objects.ButtonBox (buttonBoxSetLayout)
import GI.Gtk.Enums
       (FileChooserAction(..), ShadowType(..), ResponseType(..),
        ButtonBoxStyle(..))
import GI.Gtk.Objects.Button
       (onButtonClicked, buttonNewFromStock, buttonNewWithLabel)
import GI.Gtk.Objects.Label (labelSetMarkup, labelNew)
import GI.Gtk.Objects.Widget
       (widgetModifyFont, widgetDestroy, widgetShowAll, widgetGrabDefault,
        setWidgetCanDefault, widgetSetSensitive)
import GI.Gtk.Objects.VBox (VBox(..))
import GI.Pango.Structs.FontDescription (fontDescriptionFromString)
import GI.Gtk.Objects.CellRendererText (setCellRendererTextText)
import GI.Gtk.Objects.Settings
       (settingsSetLongProperty, settingsGetForScreen, Settings(..))
import GI.GtkSource
       (styleSchemeManagerGetSchemeIds,
        styleSchemeManagerAppendSearchPath, styleSchemeManagerNew)


-- | This needs to be incremented when the preferences format changes
prefsVersion :: Int
prefsVersion = 7

runPreferencesDialog :: IDEAction
runPreferencesDialog = do
  packageInfos <- getAllPackageIds
  initialPrefs <- readIDE prefs
  parent <- getMainWindow
  reifyIDE $ \ideR -> do
    dialog <-   dialogNew
    setWindowTransientFor dialog parent
    setWindowTitle dialog $ __ "Preferences"
    windowSetDefaultSize dialog 800 500

    configDir    <- getConfigDir
    (widget, inj, ext, notifier) <- buildEditor (extractFieldDescription $ prefsDescription configDir packageInfos) initialPrefs

    bb      <-  hButtonBoxNew
    boxSetSpacing bb 6
    buttonBoxSetLayout bb ButtonBoxStyleSpread
    load    <-  buttonNewWithLabel (__ "Load")
    save    <-  buttonNewWithLabel (__ "Save")
    preview <-  buttonNewWithLabel (__ "Preview")
    cancel  <-  buttonNewFromStock "gtk-cancel"
    apply   <-  buttonNewFromStock "gtk-apply"
    forM_ [preview, cancel, apply] $ \but ->
        boxPackEnd' bb but PackNatural 0



    upper <-   dialogGetContentArea dialog >>= unsafeCastTo Box
    boxPackStart' upper widget PackGrow 0
    errorLabel <-  labelNew Nothing
    boxPackStart' upper errorLabel PackNatural 0
    lower <-   dialogGetActionArea dialog >>= unsafeCastTo Box
    boxPackEnd' lower bb PackNatural 5

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

    onButtonClicked cancel onClose
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
        when (isJust mbP) $ labelSetMarkup errorLabel ""
        return (e{gtkReturn=False}))

    registerEvent notifier ValidationError $ \e -> do
        labelSetMarkup errorLabel $ "<span foreground=\"red\" size=\"x-large\">The following fields have invalid values: "
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
            (PP.text . show)
            intParser
            prefsFormat
            (\ b a -> a{prefsFormat = b})
            (noEditor 0)
            (\b -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Time of last storage")
                $ paraShowLabel <<<- ParaShowLabel False $ emptyParams)
            (PP.text . show)
            stringParser
            prefsSaveTime
            (\ b a -> a{prefsSaveTime = b})
            (noEditor "")
            (\b -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Show line numbers")
                $ paraSynopsis <<<- ParaSynopsis (__ "(True/False)")
                    $ emptyParams)
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> a{showLineNumbers = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setShowLineNumbers sv b) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "TextView Font") $ emptyParams)
            (\a -> PP.text (case a of Nothing -> show ""; Just s -> show s))
            (do str <- stringParser
                return (if T.null str then Nothing else Just str))
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
            (PP.text . show)
            readParser
            rightMargin
            (\b a -> a{rightMargin = b})
            (disableEditor (intEditor (1.0, 200.0, 5.0), paraName <<<- ParaName (__ "Position")
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
            (PP.text . show)
            intParser
            tabWidth
            (\b a -> a{tabWidth = b})
            (intEditor (1.0, 20.0, 1.0))
            (\i -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setIndentWidth sv i) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Wrap lines") $ emptyParams)
            (PP.text . show)
            boolParser
            wrapLines
            (\b a -> a{wrapLines = b})
            boolEditor
            (\b -> do
                buffers <- allBuffers
                mapM_ (\(IDEBuffer {sourceView = sv}) -> setWrapMode sv b) buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Use standard line ends even on windows") $ emptyParams)
            (PP.text . show)
            boolParser
            forceLineEnds
            (\b a -> a{forceLineEnds = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Remove trailing blanks when saving a file") $ emptyParams)
            (PP.text . show)
            boolParser
            removeTBlanks
            (\b a -> a{removeTBlanks = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Source candy")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "Empty for do not use or the name of a candy file in a config dir")
                    $ paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
            (PP.text . show)
            readParser
            sourceCandy (\b a -> a{sourceCandy = b})
            (disableEditor (textEditor (not . T.null) True,
                paraName <<<- ParaName (__ "Candy specification")
                                    $ emptyParams)
                    True (__ "Use it ?"))
            (\_ -> switchBuffersCandy)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Editor Style") $ emptyParams)
            (\a -> PP.text (case a of (False,_) -> show ""; (True, s) -> show s))
            (do str <- stringParser
                return (if T.null str then (False, __ "classic") else (True,str)))
            sourceStyle
            (\b a -> a{sourceStyle = b})
            styleEditor
            (\mbs -> do
                setInfoStyle
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Found Text Background") $ emptyParams)
            (PP.text . show)
            colorParser
            foundBackgroundLight
            (\ b a -> a{foundBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Selection Match Text Background") $ emptyParams)
            (PP.text . show)
            colorParser
            matchBackgroundLight
            (\ b a -> a{matchBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Execution Context Text Background") $ emptyParams)
            (PP.text . show)
            colorParser
            contextBackgroundLight
            (\ b a -> a{contextBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Breakpoint Text Background") $ emptyParams)
            (PP.text . show)
            colorParser
            breakpointBackgroundLight
            (\ b a -> a{breakpointBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Lint Text Background") $ emptyParams)
            (PP.text . show)
            colorParser
            lintBackgroundLight
            (\ b a -> a{lintBackgroundLight = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Found Text Dark Background") $ emptyParams)
            (PP.text . show)
            colorParser
            foundBackgroundDark
            (\ b a -> a{foundBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Selection Match Text Dark Background") $ emptyParams)
            (PP.text . show)
            colorParser
            matchBackgroundDark
            (\ b a -> a{matchBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Execution Context Text Dark Background") $ emptyParams)
            (PP.text . show)
            colorParser
            contextBackgroundDark
            (\ b a -> a{contextBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Breakpoint Text Dark Background") $ emptyParams)
            (PP.text . show)
            colorParser
            breakpointBackgroundDark
            (\ b a -> a{breakpointBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Lint Text Dark Background") $ emptyParams)
            (PP.text . show)
            colorParser
            lintBackgroundDark
            (\ b a -> a{lintBackgroundDark = b})
            colorEditor
            (\c -> do
                buffers <- allBuffers
                mapM_ updateStyle' buffers)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Automatically load modified files modified outside of Leksah") $ emptyParams)
            (PP.text . show)
            boolParser
            autoLoad
            (\b a -> a{autoLoad = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Text Editor") $ emptyParams)
            (PP.text . show)
            stringParser
            textEditorType
            (\b a -> a{textEditorType = b})
            (comboSelectionEditor ["GtkSourceView", "Yi", "CodeMirror"] id)
            (\i -> return ())
    ]),
    (__ "User Interface", VFDPP emptyParams [
        mkFieldPP
            -- TODO: be able to load different gtk themes
            (paraName <<<- ParaName (__ "User interface theme") $ emptyParams)
            (PP.text . show)
            stringParser
            (\prefs -> if darkUserInterface prefs then "Dark" else "Light")
            (\str a -> a {darkUserInterface = str == "Dark"})
            (comboSelectionEditor ["Dark", "Light"] id)
            (\_ -> applyInterfaceTheme)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "LogView Font") $ emptyParams)
            (\a -> PP.text (case a of Nothing -> show ""; Just s -> show s))
            (do str <- stringParser
                return (if T.null str then Nothing else Just str))
            logviewFont
            (\ b a -> a{logviewFont = b})
            fontEditor
            (\mbs -> do
                log <- getLog
                fdesc <- fontDescriptionFromString (fromMaybe "" mbs)
                widgetModifyFont (logLaunchTextView log) (Just fdesc))
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Window default size")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "Default size of the main ide window specified as pair")
                $ paraShadow <<<- ParaShadow ShadowTypeIn $ emptyParams)
            (PP.text.show)
            (pairParser intParser)
            defaultSize (\(c,d) a -> a{defaultSize = (c,d)})
            (pairEditor (intEditor (0.0, 3000.0, 25.0),
                            paraName <<<- ParaName "X" $ emptyParams)
                        (intEditor (0.0, 3000.0, 25.0),
                            paraName <<<- ParaName "Y" $ emptyParams))
            (\a -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Show hidden files in workspace") $ emptyParams)
            (PP.text . show)
            boolParser
            showHiddenFiles
            (\b a -> a {showHiddenFiles = b})
            boolEditor
            (\_ -> refreshWorkspacePane)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Show icons in the Workspace pane") $ emptyParams)
            (PP.text . show)
            boolParser
            showWorkspaceIcons
            (\b a -> a {showWorkspaceIcons = b})
            boolEditor
            (\_ -> rebuildWorkspacePane)
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Use ctrl Tab for Notebook flipper") $ emptyParams)
            (PP.text . show)
            boolParser
            useCtrlTabFlipping
            (\b a -> a{useCtrlTabFlipping = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Complete only on Hotkey") $ emptyParams)
            (PP.text . show)
            boolParser
            completeRestricted
            (\b a -> a{completeRestricted = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Name of the keymap")
                $ paraSynopsis <<<- ParaSynopsis
                    (__ "The name of a keymap file in a config dir")
                    $ paraDirection <<<- ParaDirection Horizontal $ emptyParams)
            (PP.text . T.unpack)
            identifier
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
            (PP.text . show)
            boolParser
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
                     $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,130)
                            $ emptyParams)
            (PP.text . show)
            readParser
            categoryForPane
            (\b a -> a{categoryForPane = b})
            (multisetEditor
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
                     $ paraDirection <<<- ParaDirection Vertical
                        $ paraMinSize <<<- ParaMinSize (-1,130)
                            $ emptyParams)
            (PP.text . show)
            readParser
            pathForCategory
            (\b a -> a{pathForCategory = b})
            (multisetEditor
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
            (PP.text . show)
            readParser
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
            (PP.text . show)
            readParser
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
            (filesEditor Nothing FileChooserActionSelectFolder (__ "Select folder"))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Unpack source for cabal packages to") $ emptyParams)
            (PP.text . show)
            readParser
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
            (maybeEditor (stringEditor (const True) True,emptyParams) True "")
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "URL from which to download prebuilt metadata") $ emptyParams)
            (PP.text . show)
            stringParser
            retrieveURL
            (\b a -> a{retrieveURL = b})
            (textEditor (const True) True)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Strategy for downloading prebuilt metadata") $ emptyParams)
            (PP.text . show)
            readParser
            retrieveStrategy
            (\b a -> a{retrieveStrategy = b})
            (enumEditor [__ "Try to download and then build locally if that fails",
                         __ "Try to build locally and then download if that fails",
                         __ "Never download (just try to build locally)"])
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Update metadata at startup") $ emptyParams)
            (PP.text . show)
            boolParser
            collectAtStart
            (\b a -> a{collectAtStart = b})
            boolEditor
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Port number for leksah to comunicate with leksah-server") $ emptyParams)
            (PP.text . show)
            intParser
            serverPort
            (\b a -> a{serverPort = b})
            (intEditor (1.0, 65535.0, 1.0))
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "IP address for leksah to comunicate with leksah-server") $ emptyParams)
            (PP.text . show)
            stringParser
            serverIP
            (\b a -> a{serverIP = b})
            (textEditor (not . T.null) True)
            (\i -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Stop the leksah-server process when leksah disconnects") $ emptyParams)
            (PP.text . show)
            boolParser
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
            (PP.text . show . map display)
            (fmap (mapMaybe simpleParse) readParser)
            packageBlacklist
            (\b a -> a{packageBlacklist = b})
            (dependenciesEditor packages)
            (\i -> return ())
    ]),
    (__ "Build", VFDPP emptyParams [
         mkFieldPP
            (paraName <<<- ParaName (__ "Automatically save all files before building") $ emptyParams)
            (PP.text . show)
            boolParser
            saveAllBeforeBuild
            (\b a -> a{saveAllBeforeBuild = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
               (paraName <<<- ParaName (__ "Run HLint when saving a source file") $ emptyParams)
               (PP.text . show)
               boolParser
               hlintOnSave
               (\b a -> a{hlintOnSave = b})
               boolEditor
               (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Select first warning if built without errors") $ emptyParams)
            (PP.text . show)
            boolParser
            jumpToWarnings
            (\b a -> a{jumpToWarnings = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Background build") $ emptyParams)
            (PP.text . show)
            boolParser
            backgroundBuild
            (\b a -> a{backgroundBuild = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Run unit tests when building") $ emptyParams)
            (PP.text . show)
            boolParser
            runUnitTests
            (\b a -> a{runUnitTests = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Run benchmarks when building") $ emptyParams)
            (PP.text . show)
            boolParser
            runBenchmarks
            (\b a -> a{runBenchmarks = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Make mode") $ emptyParams)
            (PP.text . show)
            boolParser
            makeMode
            (\b a -> a{makeMode = b})
            (boolEditor2 (__ "Single mode"))
            (\i -> return ())

         , mkFieldPP
            (paraName <<<- ParaName (__ "Single build without linking") $ emptyParams)
            (PP.text . show)
            boolParser
            singleBuildWithoutLinking
            (\b a -> a{singleBuildWithoutLinking = b})
            boolEditor
            (\i -> return ())
         , mkFieldPP
            (paraName <<<- ParaName (__ "Don't install last package") $ emptyParams)
            (PP.text . show)
            boolParser
            dontInstallLast
            (\b a -> a{dontInstallLast = b})
            boolEditor
            (\i -> return ())

        , mkFieldPP
            (paraName <<<- ParaName (__ "Use vado to run commands on the remote machine") $ emptyParams)
            (PP.text . show)
            boolParser
            useVado
            (\b a -> a{useVado = b})
            boolEditor
            (\i -> return ())

        -- cabal-dev is noot supported any more leaving this here for now
        -- so the we don't have to udate the prefs files.
        , mkFieldPP
            (paraName <<<- ParaName (__ "Use cabal-dev") $ emptyParams)
            (PP.text . show)
            boolParser
            (const False)
            (\b a -> a)
            boolEditor
            (\i -> return ())
    ]),
    (__ "Debug", VFDPP emptyParams [
           mkFieldPP
            (paraName <<<- ParaName (__ "Enable usage of Show instances in :print") $ emptyParams)
            (PP.text . show)
            boolParser
            printEvldWithShow
            (\b a -> a{printEvldWithShow = b})
            boolEditor
            debugSetPrintEvldWithShow
         , mkFieldPP
            (paraName <<<- ParaName (__ "Break on any exception thrown") $ emptyParams)
            (PP.text . show)
            boolParser
            breakOnException
            (\b a -> a{breakOnException = b})
            boolEditor
            debugSetBreakOnException
         , mkFieldPP
            (paraName <<<- ParaName (__ "Break on uncaught exceptions and errors") $ emptyParams)
            (PP.text . show)
            boolParser
            breakOnError
            (\b a -> a{breakOnError = b})
            boolEditor
            debugSetBreakOnError
         , mkFieldPP
            (paraName <<<- ParaName (__ "Turn on printing of binding results in GHCi") $ emptyParams)
            (PP.text . show)
            boolParser
            printBindResult
            (\b a -> a{printBindResult = b})
            boolEditor
            debugSetPrintBindResult
    ]),
    (__ "Help", VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName (__ "Browser") $ emptyParams)
            (PP.text . show)
            stringParser
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
            (PP.text . show)
            stringParser
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
    ids <- fromMaybe [] <$> nullToNothing (styleSchemeManagerGetSchemeIds styleManager)
    let notDarkIds = filter (not . T.isSuffixOf "-dark") ids
    disableEditor (comboSelectionEditor notDarkIds id, p) True (__ "Select a special style?") p n


-- | The default preferences
defaultPrefs = Prefs {
        prefsFormat         =   prefsVersion
    ,   prefsSaveTime       =   ""
    ,   showLineNumbers     =   True
    ,   rightMargin         =   (True,100)
    ,   tabWidth            =   4
    ,   wrapLines           =   False
    ,   sourceCandy         =   (False,"candy")
    ,   darkUserInterface   = False
    ,   saveSessionOnClose  = True
    ,   keymapName          =   "keymap"
    ,   forceLineEnds       =   True
    ,   removeTBlanks       =   True
    ,   textviewFont        =   Nothing
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
    ,   logviewFont         =   Nothing
    ,   defaultSize         =   (1024,800)
    ,   browser             =   "firefox"
    ,   sourceDirectories   =   []
    ,   packageBlacklist    =   []
    ,   pathForCategory     =   [   ("EditorCategory",[SplitP LeftP])
                                ,   ("LogCategory",[SplitP RightP, SplitP BottomP])
                                ,   ("ToolCategory",[SplitP RightP, SplitP TopP])
                                ]
    ,   defaultPath         =   [SplitP LeftP]
    ,   categoryForPane     =   [   ("*ClassHierarchy","ToolCategory")
                                ,   ("*Breakpoints","LogCategory")
                                ,   ("*Browser","ToolCategory")
                                ,   ("*Debug","ToolCategory")
                                ,   ("*Errors","ToolCategory")
                                ,   ("*Files","ToolCategory")
                                ,   ("*Flags","ToolCategory")
                                ,   ("*Grep","ToolCategory")
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
                                ,   ("*Workspace","LogCategory")]
    ,   collectAtStart      =   True
    ,   unpackDirectory     =   Just ("~" </> configDirName </> "packageSources")
    ,   retrieveURL         =   "http://www.leksah.org"
    ,   retrieveStrategy    =   SP.RetrieveThenBuild
    ,   useCtrlTabFlipping  =   True
    ,   docuSearchURL       =   "https://www.haskell.org/hoogle/?q="
    ,   completeRestricted  =   False
    ,   saveAllBeforeBuild  =   True
    ,   jumpToWarnings      =   True
    ,   useVado             =   False
    ,   backgroundBuild     =   True
    ,   runUnitTests        =   False
    ,   runBenchmarks        =   False
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
    ,   showWorkspaceIcons       =   True
    ,   hlintOnSave = True
    }

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

-- | Read the preference file
readPrefs :: FilePath -> IO Prefs
readPrefs fn = catch (do
    configDir <- getConfigDir
    readFields fn (flattenFieldDescriptionPPToS (prefsDescription configDir [])) defaultPrefs)
        (\ (e :: SomeException) -> do
            sysMessage Normal (T.pack $ show e)
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
    writeFields fpath newPrefs (flattenFieldDescriptionPPToS (prefsDescription configDir []))


