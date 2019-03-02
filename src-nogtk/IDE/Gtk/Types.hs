{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
module IDE.Gtk.Types (
    IDEState(..)
  , IDEGtk
  , IDEGtkEvent
  , getGtkEventSelector
  , Color(..)
  , PanePath
  , PanePathElement(..)
  , PaneDirection(..)
  , MergeTool(..)
  , ActionString
  , KeyString
  , ActionDescr(..)
  , LogLaunchData(..)
  , LogLaunch(..)
) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Data.Word (Word16)
import GHC.Generics (Generic)
import System.Process (ProcessHandle)
import Control.Monad.Trans.Reader (ReaderT(..))

data IDEState =
        -- | Leksah is in startup mode
        IsStartingUp
        -- | Leksah is about to go down
    |   IsShuttingDown
        -- | Leksah is running
    |   IsRunning

type IDEGtk (idem :: * -> *) ideref = ()

type IDEGtkEvent ideref = Void

getGtkEventSelector :: IDEGtkEvent ideref -> Text
getGtkEventSelector _ =   error "Should not be possible"

data Color = Color Word16 Word16 Word16 deriving(Eq, Show, Generic)

instance ToJSON Color
instance FromJSON Color

type PanePath       =   [PanePathElement]
data PanePathElement = SplitP PaneDirection | GroupP Text
    deriving (Eq, Show, Read, Generic)

instance ToJSON PanePathElement
instance FromJSON PanePathElement

data PaneDirection  =   TopP | BottomP | LeftP | RightP
    deriving (Eq, Show, Read, Generic)

instance ToJSON PaneDirection
instance FromJSON PaneDirection

newtype MergeTool = MergeTool {
    fullPath :: FilePath
    } deriving (Show, Read, Generic)

instance ToJSON MergeTool
instance FromJSON MergeTool

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

data LogLaunch = LogLaunch
