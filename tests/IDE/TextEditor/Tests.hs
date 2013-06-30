{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor.Tests
-- Copyright   :  2007-2013 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.TextEditor.Tests (
    testEditors
) where

import Graphics.UI.Gtk
       (mainQuit, postGUIAsync, windowSetPosition, windowSetDefaultSize,
        mainGUI, widgetShowAll, containerAdd, uiManagerNew, widgetSetName,
        windowNew)
import IDE.TextEditor
       (CodeMirror(..), Yi(..), GtkSourceView(..), TextEditor(..))
import IDE.Core.Types
       (IDEM, KeymapI, Prefs(..), IDE(..), IDEState(..))
import qualified Data.Map as Map (empty)
import Graphics.UI.Frame.Panes
       (PaneLayout(..), FrameState(..))
import IDE.SourceCandy (parseCandy)
import IDE.Utils.FileUtils (getConfigFilePathForLoad)
import IDE.Utils.Utils
       (leksahKeymapFileExtension, leksahCandyFileExtension)
import IDE.Pane.Preferences (defaultPrefs)
import IDE.Keymap (Keymap(..))
import IDE.Command (mkActions)
import qualified IDE.YiConfig as Yi (start)
import IDE.YiConfig (defaultYiConfig)
import Data.IORef (newIORef)
import IDE.Core.State (reflectIDE)
import Control.Monad.IO.Class (MonadIO(..))
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
import System.Log.Logger (debugM)
import Control.Concurrent (takeMVar, putMVar, newEmptyMVar)
import Test.QuickCheck.Monadic (assert, run, monadicIO)
import Test.QuickCheck.All (quickCheckAll)
import Graphics.UI.Frame.ViewFrame (getWindows)
import Test.QuickCheck (Property)
import Control.Monad.Loops (allM)
import System.IO (stderr, stdout, hFlush)

testEditors :: IO Bool
testEditors = do
    result <- newEmptyMVar
    postGUIAsync $ do
        $quickCheckAll >>= putMVar result
        mainQuit
    mainGUI
    takeMVar result

testIDE :: IDEM Bool -> IO Bool
testIDE f = do
  result <- newEmptyMVar
  Yi.start defaultYiConfig $ \yiControl -> do
    uiManager   <-  uiManagerNew
    dataDir       <- getDataDir
    candyPath   <-  getConfigFilePathForLoad
                        (case sourceCandy defaultPrefs of
                            (_,name)   ->   name ++ leksahCandyFileExtension) Nothing dataDir
    candySt     <-  parseCandy candyPath
    -- keystrokes
    keysPath    <-  getConfigFilePathForLoad (keymapName defaultPrefs ++ leksahKeymapFileExtension) Nothing dataDir
    keyMap      <-  parseKeymap keysPath
    let accelActions = setKeymap (keyMap :: KeymapI) mkActions
    specialKeys <-  buildSpecialKeys keyMap accelActions

    win         <-  windowNew
    windowSetDefaultSize win 900 600
    windowSetPosition win WinPosCenter
    widgetSetName win "Leksah Main Window"
    let fs = FrameState
            {   windows       =   [win]
            ,   uiManager     =   uiManager
            ,   panes         =   Map.empty
            ,   activePane    =   Nothing
            ,   paneMap       =   Map.empty
            ,   layout        =   (TerminalP Map.empty Nothing (-1) Nothing Nothing)
            ,   panePathFromNB =  Map.empty
            }
        ide = IDE
              {   frameState        =   fs
              ,   recentPanes       =   []
              ,   specialKeys       =   specialKeys
              ,   specialKey        =   Nothing
              ,   candy             =   candySt
              ,   prefs             =   defaultPrefs
              ,   workspace         =   Nothing
              ,   activePack        =   Nothing
              ,   activeExe         =   Nothing
              ,   bufferProjCache   =   Map.empty
              ,   allLogRefs        =   []
              ,   currentHist       =   0
              ,   currentEBC        =   (Nothing, Nothing, Nothing)
              ,   systemInfo        =   Nothing
              ,   packageInfo       =   Nothing
              ,   workspaceInfo     =   Nothing
              ,   workspInfoCache   =   Map.empty
              ,   handlers          =   Map.empty
              ,   currentState      =   IsStartingUp
              ,   guiHistory        =   (False,[],-1)
              ,   findbar           =   (False,Nothing)
              ,   toolbar           =   (True,Nothing)
              ,   recentFiles       =   []
              ,   recentWorkspaces  =   []
              ,   runningTool       =   Nothing
              ,   debugState        =   Nothing
              ,   completion        =   ((750,400),Nothing)
              ,   yiControl         =   yiControl
              ,   server            =   Nothing
              ,   vcsData           =   (Map.empty, Nothing)
              ,   logLaunches       =   Map.empty
              ,   autoCommand       =   return ()
              }
    ideR <- newIORef ide
    (`reflectIDE` ideR) f >>= putMVar result
  takeMVar result

allEditors :: (forall editor. TextEditor editor
           => (   Maybe FilePath
                    -> String
                    -> IDEM (EditorBuffer editor))
               -> IDEM Bool)
           -> IO Bool
allEditors test = allM id
    [ doTest GtkSourceView
    , doTest Yi
    , doTest CodeMirror]
  where
    doTest :: forall editor. (TextEditor editor, Show editor) => editor -> IO Bool
    doTest editor = do
        hFlush stdout
        hFlush stderr
        debugM "leksah" $ show editor
        testIDE $ test (newBuffer :: Maybe FilePath -> String -> IDEM (EditorBuffer editor))

prop_test :: String -> Property
prop_test s = monadicIO $ do
    let input = filter (not . flip elem ['\NUL', '\r']) s
    result <- run $ allEditors (\buf -> do
        (win:_) <- getWindows
        buffer <- buf Nothing ""
        view <- newView buffer (Just "monospace")
        sw <- getScrolledWindow view
        liftIO $ containerAdd win sw
        setText buffer input
        first <- getStartIter buffer
        last <- getEndIter buffer
        out <- getText buffer first last True
        return $ input == out)
    assert result

