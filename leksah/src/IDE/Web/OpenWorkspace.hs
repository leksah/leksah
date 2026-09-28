{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Workspace ▸ Open Workspace…: pick a @.leksah-workspace@ in the native
-- file panel and make it the open workspace.
--
-- The menu command can show the panel itself ('requestOpenWorkspace'), but the
-- chosen path comes back asynchronously on the shared pick queue
-- ("IDE.Web.PickPathRequest"), routed to one window, whose network hands it to
-- 'openPickedWorkspace'.  Results are told apart by token: this command owns
-- one process-wide token, so a path picked for the Add Project… dialog is never
-- taken for a workspace, nor the reverse.
module IDE.Web.OpenWorkspace
  ( openWorkspaceToken
  , requestOpenWorkspace
  , openPickedWorkspace
  ) where

import Control.Exception (SomeException, catch)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

import IDE.App (App(..), appNote)
import IDE.Workspace (wsOpenFile)
import IDE.Ws.File (isWorkspaceFile)
import IDE.Web.OpenPanel (PickMode(..), runPickPathPanel)
import IDE.Web.PickPathRequest (newPickToken)

-- | The token every Open Workspace… panel is opened with.
{-# NOINLINE openWorkspaceToken #-}
openWorkspaceToken :: Int
openWorkspaceToken = unsafePerformIO newPickToken

-- | Show the native file panel (a no-op on front ends without one).
requestOpenWorkspace :: IO ()
requestOpenWorkspace = runPickPathPanel PickFiles openWorkspaceToken

-- | Open a picked path as the workspace.  Anything but a workspace file
-- ('isWorkspaceFile': @.leksah-workspace@, or the older @.leksah.json@) is
-- refused with a note rather than handed to the workspace parser.  The
-- session saver then remembers it, so it is also what reopens next time
-- ('IDE.Web.Session.wsWorkspace').
openPickedWorkspace :: App -> FilePath -> IO ()
openPickedWorkspace app p
  | isWorkspaceFile p =
      wsOpenFile (appWorkspace app) p
        `catch` \(e :: SomeException) ->
          appNote app ("Can't open workspace " <> T.pack p <> ": " <> T.pack (show e))
  | otherwise =
      appNote app (T.pack p <> " is not a .leksah-workspace file")
