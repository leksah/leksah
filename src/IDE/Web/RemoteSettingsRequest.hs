-- | A process-global queue of "open the Remote Settings… dialog for this
-- project" requests.
--
-- Editing a project's remote settings is a reflex-level modal in
-- "IDE.Web.Main".  The project-tree context-menu item only runs an 'IDEAction',
-- so it cannot touch the reflex network directly; instead it drops the
-- 'ProjectKey' here and "IDE.Web.Main" drains the queue from a background
-- thread, turning it into a reflex 'Event' that shows the dialog for that
-- project.  Mirrors "IDE.Web.AddRemoteRequest" (which carries no payload).
module IDE.Web.RemoteSettingsRequest
  ( requestRemoteSettings
  , nextRemoteSettings
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

import IDE.Ws.Types (ProjectKey)

{-# NOINLINE remoteSettingsChan #-}
remoteSettingsChan :: Chan ProjectKey
remoteSettingsChan = unsafePerformIO newChan

-- | Ask for the Remote Settings dialog for a project (from the context menu).
requestRemoteSettings :: ProjectKey -> IO ()
requestRemoteSettings = writeChan remoteSettingsChan

-- | Block until the next request (drained by the reflex bridge in "IDE.Web.Main").
nextRemoteSettings :: IO ProjectKey
nextRemoteSettings = readChan remoteSettingsChan
