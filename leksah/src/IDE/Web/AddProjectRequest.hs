-- | A process-global queue of "open the Add Project… dialog" requests.
--
-- Adding a project — a local project file, a plain folder, or a directory on
-- another machine over ssh — is one reflex-level modal in 'IDE.Web.Main'.  The
-- native menus only run a command's 'commandAction' ('IDEAction'), so they
-- cannot touch the reflex network directly; instead the @workspace.addProject@
-- command drops a token here and 'IDE.Web.Main' drains the queue from a
-- background thread, turning it into a reflex 'Event' that shows the dialog (the
-- in-page web menubar route to the same dialog without the round-trip).
-- Mirrors 'IDE.Web.FindRequest' / 'IDE.Web.SaveRequest'.
module IDE.Web.AddProjectRequest
  ( requestAddProject
  , nextAddProjectRequest
  ) where

import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE addProjectChan #-}
addProjectChan :: Chan ()
addProjectChan = unsafePerformIO newChan

-- | Ask for the Add Project dialog (called from the menu command action).
requestAddProject :: IO ()
requestAddProject = writeChan addProjectChan ()

-- | Block until the next request (drained by the reflex bridge in "IDE.Web.Main").
nextAddProjectRequest :: IO ()
nextAddProjectRequest = readChan addProjectChan
