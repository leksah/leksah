{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module IDE.Gtk.Types (
    IDEState(..)
  , IDEGtk(..)
  , IDEGtkEvent(..)
  , getGtkEventSelector
  , LogLaunch(..)
  , LogLaunchData(..)
  , SpecialKeyTable
  , SpecialKeyCons
  , CompletionWindow(..)
  , TypeTip(..)
  , GUIHistory
  , GUIHistory'(..)
  , Color(..)
  , toGdkColor
  , fromGdkColor
  , KeyVal
  , PanePath
  , MergeTool
  , ActionString
  , KeyString
  , ActionDescr(..)

-- IDEGtk
  , application
  , frameState
  , flipper
  , typeTip
  , guiHistory
  , findbar
  , toolbar
  , specialKeys
  , specialKey
  , completion
  , vcsData
) where

import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT(..))

import GI.Gtk.Objects.Toolbar (Toolbar(..))
import Data.GI.Gtk.ModelView.SeqStore (SeqStore(..))
import GI.Gtk.Objects.MenuItem (MenuItem(..))
import GI.Gtk.Objects.TreeView (TreeView(..))
import GI.Gtk.Objects.Menu (Menu(..))
import GI.Gdk.Flags (ModifierType)
import GI.Gtk.Objects.TextBuffer (TextBuffer(..))
import GI.Gtk.Objects.Window (Window(..))
import GI.Gtk.Objects.Application (Application(..))
import GI.Gtk (CssProvider)
import Graphics.UI.Frame.Panes
import Graphics.UI.Editor.Simple (Color(..), toGdkColor, fromGdkColor)
import Data.Map (Map)
import System.Process (ProcessHandle)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Distribution.ModuleName (ModuleName(..))
import IDE.Core.CTypes (Descr, Scope)
import Data.Int (Int32)
import Control.Lens (makeLenses)
import VCSGui.Common (MergeTool)

data IDEState =
        -- | Leksah is in startup mode
        IsStartingUp
        -- | Leksah is about to go down
    |   IsShuttingDown
        -- | Leksah is running
    |   IsRunning
        -- | The completion feature is used
    |   IsCompleting Connections


data IDEGtk idem ideref = IDEGtk {
    _application         :: Application
,   _frameState          :: FrameState idem         -- ^ state of the windows framework
,   _flipper             :: Maybe TreeView             -- ^ used to select the active pane
,   _typeTip             :: Maybe (TypeTip idem)
,   _guiHistory          :: (Bool,[GUIHistory],Int)
,   _findbar             :: (Bool,Maybe (Toolbar,SeqStore Text,CssProvider))
,   _toolbar             :: (Bool,Maybe Toolbar)
,   _vcsData             :: (Map FilePath MenuItem, Maybe (Maybe Text)) -- menus for packages, password
,   _specialKeys         :: SpecialKeyTable ideref  -- ^ a structure for emacs like keystrokes
,   _specialKey          :: SpecialKeyCons ideref   -- ^ the first of a double keystroke
,   _completion          :: ((Int, Int), Maybe CompletionWindow)
}

data IDEGtkEvent ideref =
        RecordHistory GUIHistory
    |   GetTextPopup (Maybe (ideref -> Menu -> IO ()))

getGtkEventSelector :: IDEGtkEvent ideref -> Text
getGtkEventSelector (RecordHistory _)       =   "RecordHistory"
getGtkEventSelector (GetTextPopup _)        =   "GetTextPopup"

-- ---------------------------------------------------------------------
-- Other data structures which are used in the state
--

--
-- | ActionDescr is a data structure from which GtkActions are build, which are used for
--   menus, toolbars, and accelerator keystrokes
--
data ActionDescr alpha = AD {
    name        ::   ActionString
,   label       ::   Text
,   tooltip     ::   Maybe Text
,   stockID     ::   Maybe Text
,   action      ::   ReaderT alpha IO ()
,   accelerator ::   [KeyString]
,   isToggle    ::   Bool
}

type ActionString = Text
type KeyString = Text

data LogLaunchData = LogLaunchData {
    logLaunch :: LogLaunch
,   mbPid :: Maybe ProcessHandle
}

newtype LogLaunch = LogLaunch {
    logBuffer   :: TextBuffer
} deriving Typeable

type KeyVal = Word32

type SpecialKeyTable alpha  =   Map (KeyVal,[ModifierType]) (Map (KeyVal,[ModifierType]) (ActionDescr alpha))

type SpecialKeyCons  alpha  =   Maybe (Map (KeyVal, [ModifierType]) (ActionDescr alpha), Text)

-- | the first one is the new and the second the old state
type GUIHistory = (GUIHistory', GUIHistory')

data GUIHistory' =
        ModuleSelected  {
            moduleS :: Maybe ModuleName
        ,   facetS  :: Maybe Text}
    |   ScopeSelected {
            scope   :: Scope
        ,   blacklist :: Bool}
    |   InfoElementSelected {
            mbInfo  :: Maybe Descr}
    |   PaneSelected {
            paneN   :: Maybe Text}
   deriving (Eq, Ord, Show)

data CompletionWindow = CompletionWindow {
    cwWindow :: Window,
    cwTreeView :: TreeView,
    cwSeqStore :: SeqStore Text}

data TypeTip idem = TypeTip {
    ttWindow :: Window,
    ttSetText :: Int32 -> Int32 -> Text -> idem (),
    ttUpdateStyle :: idem ()}

makeLenses ''IDEGtk
