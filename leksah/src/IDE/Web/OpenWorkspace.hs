{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
-- | Workspace ▸ Open Workspace… (pick a @.leksah-workspace@ in the native file
-- panel and make it the open workspace) and Workspace ▸ New Workspace… (pick
-- a folder and create one there).
--
-- The menu command can show the panel itself ('requestOpenWorkspace'), but the
-- chosen path comes back asynchronously on the shared pick queue
-- ("IDE.Web.PickPathRequest"), routed to one window, whose network hands it to
-- 'openPickedWorkspace' / 'createPickedWorkspace'.  Results are told apart by
-- token: each command owns one process-wide token, so a path picked for the
-- Add Project… dialog is never taken for a workspace, nor the reverse.
module IDE.Web.OpenWorkspace
  ( openWorkspaceToken
  , requestOpenWorkspace
  , openPickedWorkspace
  , newWorkspaceToken
  , requestNewWorkspace
  , createPickedWorkspace
  ) where

import Control.Exception (SomeException, catch)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>), dropTrailingPathSeparator, takeFileName)
import System.IO.Unsafe (unsafePerformIO)

import IDE.App (App(..), appNote)
import IDE.Workspace (projectOpenPath, wsOpenFile)
import IDE.Ws.File
       (Workspace(..), isWorkspaceFile, workspaceExtension, writeWorkspaceFile)
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

-- | The token every New Workspace… panel is opened with.
{-# NOINLINE newWorkspaceToken #-}
newWorkspaceToken :: Int
newWorkspaceToken = unsafePerformIO newPickToken

-- | Show the native folder panel (a no-op on front ends without one).
requestNewWorkspace :: IO ()
requestNewWorkspace = runPickPathPanel PickDirs newWorkspaceToken

-- | Create a workspace in a picked folder: @\<folder\>/\<folder-name\>.leksah-workspace@,
-- opened at once with the folder itself as its first project (detected the
-- same way Add Project… detects it), since a workspace inside a folder is
-- nearly always for that folder.  An existing file of that name is opened,
-- never overwritten.
createPickedWorkspace :: App -> FilePath -> IO ()
createPickedWorkspace app dir0 = go `catch` \(e :: SomeException) ->
    appNote app ("Can't create a workspace in " <> T.pack dir0 <> ": " <> T.pack (show e))
  where
    dir  = dropTrailingPathSeparator dir0
    name = takeFileName dir
    file = dir </> (name <> workspaceExtension)
    svc  = appWorkspace app
    go = doesFileExist file >>= \case
      True -> do
        appNote app (T.pack file <> " already exists — opened it instead")
        wsOpenFile svc file
      False -> do
        writeWorkspaceFile file (Workspace (T.pack name) [] Nothing)
        wsOpenFile svc file
        projectOpenPath svc dir
