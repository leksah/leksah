{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The workspace / project / package action guards: resolve the ambient
-- context (open workspace, active project, active package) and report when
-- there is none.
module IDE.Gtk.Workspaces
  ( workspaceTry
  , workspaceTryQuiet
  , projectTry
  , projectTryQuiet
  , packageTry
  , packageTryQuiet
  , makePackage
  ) where

import Control.Monad.Reader (ask, lift)

import IDE.Core.State
       (IDEAction, MessageLevel(..), PackageAction, ProjectAction,
        WorkspaceAction, __, activePack, activeProject, ideMessage,
        liftIDE, readIDE, runPackage, runProject, runWorkspace, workspace)
import IDE.Project.Build (buildTarget)

workspaceTry :: WorkspaceAction -> IDEAction
workspaceTry = workspaceTryQuiet

workspaceTryQuiet :: WorkspaceAction -> IDEAction
workspaceTryQuiet f =
    readIDE workspace >>= \case
        Just ws -> runWorkspace f ws
        Nothing -> ideMessage Normal (__ "No workspace open")

projectTry :: ProjectAction -> IDEAction
projectTry = projectTryQuiet

projectTryQuiet :: ProjectAction -> IDEAction
projectTryQuiet f = workspaceTryQuiet $
    readIDE activeProject >>= \case
        Just project -> runProject f project
        Nothing      -> ideMessage Normal (__ "No project active")

packageTry :: PackageAction -> IDEAction
packageTry = packageTryQuiet

packageTryQuiet :: PackageAction -> IDEAction
packageTryQuiet f =
    readIDE activePack >>= \case
        Just p  -> projectTryQuiet (runPackage f p)
        Nothing -> ideMessage Normal (__ "No active package")

-- | Build the package the command table points at.
makePackage :: PackageAction
makePackage = do
    package <- ask
    project <- lift ask
    liftIDE (buildTarget project package)
