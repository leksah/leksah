module IDE.Pane.Log where

import Data.Text (Text)
import System.Process (ProcessHandle)
import IDE.Core.Types (MonadIDE, IDEAction, LogTag, IDEM)
import IDE.Gtk.Types (LogLaunch(..))
import IDE.Core.State (readIDE, logLineMap, modifyIDE_)
import Control.Lens ((%~))
import qualified Data.Map as M (size, insert)

data IDELog = IDELog

getLog :: MonadIDE m => m IDELog
getLog = return IDELog

getDefaultLogLaunch :: MonadIDE m => m LogLaunch
getDefaultLogLaunch = return LogLaunch

markErrorInLog :: IDELog -> (Int, Int) -> IDEAction
markErrorInLog _ _ = return ()

appendLog :: IDELog
          -> LogLaunch
          -> Text
          -> LogTag
          -> IDEM Int
appendLog _ _ t tag = do
  modifyIDE_ (logLineMap %~ (\l -> M.insert (M.size l) (t, tag) l))
  pred . M.size <$> readIDE logLineMap

showDefaultLogLaunch' :: MonadIDE m => m ()
showDefaultLogLaunch' = return ()

addLogLaunchData :: Text -> LogLaunch -> ProcessHandle -> IDEM ()
addLogLaunchData _ _ _ = return ()

showLog :: IDEAction
showLog = return ()

buildLogLaunchByName :: Text -> IDEM (LogLaunch, Text)
buildLogLaunchByName logName = return (LogLaunch, logName)
