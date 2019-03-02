module IDE.Gtk.Workspaces where

import IDE.Core.Types
       (WorkspaceAction, ProjectAction, PackageAction, IDEAction)
import IDE.Workspaces
       (workspaceTryQuiet, projectTryQuiet, packageTryQuiet, makePackage')

workspaceTry :: WorkspaceAction -> IDEAction
workspaceTry = workspaceTryQuiet

projectTry :: ProjectAction -> IDEAction
projectTry = projectTryQuiet

packageTry :: PackageAction -> IDEAction
packageTry = packageTryQuiet

makePackage :: PackageAction
makePackage = makePackage'
