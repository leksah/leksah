-----------------------------------------------------------------------------
--
-- Module      :  IDE.Command.VCS.Common.GUI
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Command.VCS.Common.GUI (
--    addMenuForPackage
) where

--import Prelude ()
--import Prelude.Compat
--import IDE.Core.Types
--import IDE.Core.State
--
--import Graphics.UI.Gtk (
--    menuNew, menuItemNewWithLabel, onActivateLeaf, menuShellAppend, menuItemSetSubmenu
--        ,widgetShowAll, menuItemNewWithMnemonic, menuItemGetSubmenu, widgetHideAll, widgetDestroy)
--import Control.Monad.Reader(liftIO,ask,when)

--vcsMenu from
--        vcsItem <- GUIUtils.getVCS
--        vcsMenu <- liftIO $ menuNew
-- ideR from ask
-- setupRepoAction actionContext packageMenuOperations from common
--addMenuForPackage cabalFp = do
--                    packageItem <- liftIO $ menuItemNewWithLabel cabalFp
--                    packageMenu <- liftIO $ menuNew
--
--                    -- set-up repo action
--                    actionItem <- liftIO $ menuItemNewWithMnemonic "_Setup Repo"
--                    liftIO $ actionItem `onActivateLeaf` (
--                            reflectIDE (
--                                    setupRepoAction cabalFp
--                                ) ideR)
--                    liftIO $ menuShellAppend packageMenu actionItem
--
--                    -- other actions if repo set
--                    liftIO $ addActions cabalFp packageMenu ideR packageMenuOperations actionContext
--
--                    liftIO $ menuItemSetSubmenu packageItem packageMenu
--                    liftIO $ menuShellAppend vcsMenu packageItem
--                    where
--                    addActions cabalFp packageMenu ideR actions runner =  mapM_ (\(name,action) -> do
--                        -- for each operation add it to menu and connect action
--                        actionItem <- menuItemNewWithMnemonic name
--                        actionItem `onActivateLeaf` (reflectIDE (runner action cabalFp) ideR)
--                        menuShellAppend packageMenu actionItem
--                        ) actions




