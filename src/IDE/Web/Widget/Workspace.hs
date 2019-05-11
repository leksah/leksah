{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module IDE.Web.Widget.Workspace (
    workspaceCss
  , workspaceWidget
) where

import Control.Lens
       (to, view, preview, _Just)

import Data.Bool (bool)
import Data.List (stripPrefix)
import qualified Data.Map as M (elems, fromList)
import Data.Maybe (listToMaybe, maybeToList, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S (fromList, member)
import Data.Text (Text)
import qualified Data.Text as T (pack)

import System.FilePath ((<.>), (</>), dropFileName)

import Clay
       (pct, hover, width, bold, fontWeight, black, paddingBottom,
        borderRadius, paddingRight, marginBottom, marginTop, checked,
        userSelect, (|+), absolute, position, left, nil, paddingLeft, px,
        marginLeft, listStyleType, listStyleImage, middle, grey, color,
        nowrap, whiteSpace, inlineBlock, scroll, overflow, height, (?),
        Css, background, none, white, Color(..), VerticalAlign(..),
        cursorDefault, Cursor(..))
import qualified Clay (display, (#))
import Clay.Stylesheet (key)

import Reflex
       (leftmost, listViewWithKey, switchHold, constDyn,
        holdUniqDyn, Dynamic, Event, never, fmapMaybe)
import Reflex.Dom.Core
       (elDynClass, MonadWidget, elAttr, dyn, button, (=:), elDynAttr,
        divClass, text, el, elClass, dynText)

import IDE.Core.CTypes (packageIdentifierToString)
import IDE.Core.State
       (DebugState(..), nixEnv, activeComponent, ipdPackageDir,
        ipdLib, pjDir, IDEPackage(..), runPackage, runProject,
        pjPackages, Project(..), wsFile, workspace, wsProjects, IDE,
        activeProject, activePack, debugState, pjFile, pjFileOrDir,
        ProjectKey(..), pjCabalFile)
import IDE.Gtk.Package (packageRun)
import IDE.Gtk.Workspaces (makePackage)
import IDE.Package
       (packageClean, packageBench, packageTest, projectRefreshNix)
import IDE.Web.Command (Command(..))
import IDE.Web.Events (PackageEvent(..), ProjectEvent(..), ProjectEvents, FileEvent(..))
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Widget.FileTree (fileTree)
import IDE.Web.Widget.Tree (treeItemDynAttr, treeSelect, treeItem)
import IDE.Workspaces
       (workspaceRemoveProject, workspaceActivatePackage)

workspaceCss :: Css
workspaceCss = do
    ".workspace" ? do
        height (pct 100)
        key "fill" grey
        overflow scroll
    ".workspace li.active > .tree-expand" ?
        key "fill" white
    ".workspace li > .tree-expand" Clay.# hover ?
        key "fill" (Rgba 30 88 209 1.0)
    ".workspace li.active > label" ?
        fontWeight bold
    ".tree-item" ? do
        Clay.display inlineBlock
        whiteSpace nowrap
        cursor cursorDefault
        paddingLeft (px 2)
        paddingRight (px 2)
        paddingBottom (px 1)
        borderRadius (px 2) (px 2) (px 2) (px 2)
    ".tree-item > img" ? do
        width (px 16)
        height (px 16)
    ".tree-expand" ? do
        Clay.display inlineBlock
        whiteSpace nowrap
        cursor cursorDefault
    "div.package-id" ? do
        Clay.display inlineBlock
        whiteSpace nowrap
    "div.package-file" ? do
        Clay.display inlineBlock
        whiteSpace nowrap
        color grey
    "img" ? do
        Clay.display inlineBlock
        verticalAlign middle
    "ul" ? do
        listStyleImage none
        listStyleType none
        marginTop nil
        marginBottom nil
        marginLeft (px 20)
        paddingLeft nil
    "li.project" ?
        whiteSpace nowrap
    "li.package" ?
        whiteSpace nowrap
    "li.component" ? do
        whiteSpace nowrap
        marginLeft (px 12)
    "li.branch" ? do
        whiteSpace nowrap
        marginLeft (px 12)
    "li.file" ? do
        whiteSpace nowrap
        marginLeft (px 12)
    "ul.projects" ? do
        marginLeft nil
        paddingLeft nil
    "svg" ? do
        Clay.display inlineBlock
        verticalAlign middle
    "input.tree-select" ? do
        left (px (-1000))
        position absolute
    "input" |+ "div" ?
        userSelect none
    ("input" Clay.# checked) |+ "div" ?
        background (Rgba 30 88 209 1.0)
    ("input" Clay.# checked) |+ "div div.package-file" ?
        color black

components :: IDEPackage -> [Text]
components package =
     map ("lib:"<>) (maybeToList (ipdLib package))
  ++ map ("lib:"<>) (ipdSubLibraries package)
  ++ map ("exe:"<>) (ipdExes package)
  ++ map ("test:"<>) (ipdTests package)
  ++ map ("bench:"<>) (ipdBenchmarks package)

absolutSourceDirs :: IDEPackage -> Set FilePath
absolutSourceDirs p =
  S.fromList ((ipdPackageDir p </>) <$> ipdSrcDirs p)

workspaceWidget
  :: MonadWidget t m
  => Dynamic t IDE
  -> m (Event t ProjectEvents)
workspaceWidget ide = do
  debugPackagesD <- holdUniqDyn $ S.fromList .
      (>>= (\DebugState{..} -> map ((dsProjectKey,) . ipdCabalFile) dsPackages)) . view debugState <$> ide
  divClass "workspace" $
    divClass "workspace-body" $ do
      workspaceIsOpenD <- holdUniqDyn $ view (workspace . to isJust) <$> ide
      _ <- elDynAttr "div" (bool mempty ("style" =: "display: none") <$> workspaceIsOpenD) $
        button "Open Workspace (TODO)"
      wsFileD <- holdUniqDyn $ view (workspace . _Just . wsFile) <$> ide
      let wsDirD = dropFileName <$> wsFileD
      elClass "ul" "projects" $ do
        let addFileToKey = map (\(n, p) -> ((n, pjKey p), p))
            projectsD = M.fromList . addFileToKey . zip [0..] . fromMaybe mempty . preview (workspace . _Just . wsProjects) <$> ide
        activeProjectKeyD <- holdUniqDyn $ fmap pjKey . view activeProject <$> ide
        activePackageFileD <- holdUniqDyn $ fmap ipdCabalFile . view activePack <$> ide
        activeComponentD <- holdUniqDyn $ view activeComponent <$> ide
        listViewWithKey projectsD $ \(_, pKey) projectD -> do
          let isActiveProjectD = (== Just pKey) <$> activeProjectKeyD
          treeItemDynAttr (("class" =:) . ("project" <>) <$> (bool "" " active" <$> isActiveProjectD)) True
            (treeSelect "workspace" (menu $
                [ ("Activate",) . ProjectCommand . CommandWorkspaceAction "Set as Active Project" "" <$>
                    (workspaceActivatePackage <$> projectD <*> pure Nothing <*> pure Nothing)
                ] <> case pjFile pKey of
                        Just file -> [ constDyn ("Open Project File", ProjectFileEvents . ("" =:) $ OpenFile False file) ]
                        _ -> []
                  <> case pKey of
                        CabalTool p ->
                          [ constDyn ("Open Project Configuration File", ProjectFileEvents . ("" =:) . OpenFile True $ pjCabalFile p <.> "local") ]
                        _ -> []
                <> [ ("Refresh Nix Environment Varialbes",) . ProjectCommand . CommandWorkspaceAction "" "" <$>
                    (runProject projectRefreshNix <$> projectD)
                , constDyn ("Remove From Workspace", ProjectCommand (CommandWorkspaceAction "" "" (workspaceRemoveProject pKey)))
                ]) $ do
              isNixD <- holdUniqDyn $ isJust . nixEnv pKey "ghc" <$> ide
              elDynAttr "img" (("src" =:) . (\f -> "/pics/ide_" <> f <> ".png") . bool "source_dependency" "nix" <$> isNixD) $ return ()
              dynText $ do
                wsDir <- wsDirD
                let fileOrDir = pjFileOrDir pKey
                return . T.pack $ " " <> fromMaybe fileOrDir (stripPrefix wsDir fileOrDir)
              return never) $
            el "ul" $ do
              let packagesD = M.fromList . map (\p -> (ipdPackageId p, p)) . pjPackages <$> projectD
              packagesE <- listViewWithKey packagesD $ \packageId packageD -> do
                cabalFileD <- holdUniqDyn $ ipdCabalFile <$> packageD
                mbLibD <- holdUniqDyn $ ipdLib <$> packageD
                let isActivePackageD = (&&) <$> isActiveProjectD <*> ((==) <$> activePackageFileD <*> (Just <$> cabalFileD))
                    pkgCmd t f = (t,) . PackageCommand . CommandWorkspaceAction "" "" <$> (runProject . runPackage f <$> packageD <*> projectD)
                treeItemDynAttr (("class" =:) . ("package" <>) <$> (bool "" " active" <$> isActivePackageD)) False
                  (treeSelect "workspace" (menu
                      [ ("Activate",) . PackageCommand . CommandWorkspaceAction "Set as Active Package" "" <$>
                          (workspaceActivatePackage <$> projectD <*> (Just <$> packageD) <*> pure Nothing)
                      , pkgCmd "Build" makePackage
                      , pkgCmd "Run" packageRun
                      , pkgCmd "Test" packageTest
                      , pkgCmd "Benchmark" packageBench
                      , pkgCmd "Clean" packageClean
                      , ("Open Package File",) . PackageFileEvents . (("" =:) . OpenFile False . ipdCabalFile) <$> packageD
                      ]) $ do
                    let isDebugD = S.member . (pKey,) <$> cabalFileD <*> debugPackagesD
                    elDynAttr "img" (("src" =:) . (\f -> "/pics/ide_" <> f <> ".png") . bool "package" "debug" <$> isDebugD) $ return ()
                    text " "
                    divClass "package-id" $ do
                      text $ packageIdentifierToString packageId
                      dynText $ do
                        isActive <- isActivePackageD
                        activeComp <- activeComponentD
                        mbLib <- mbLibD
                        return $ if isActive
                          then maybe (if isJust mbLib then " (library)" else "")
                                   (\comp -> " (" <> comp <> ")") activeComp
                          else ""
                    text " "
                    divClass "package-file" $ dynText =<< holdUniqDyn (do
                        cabalFile <- cabalFileD
                        return . T.pack $ fromMaybe cabalFile $ stripPrefix (pjDir pKey) cabalFile)
                    return never) $
                  el "ul" $ do
                    componentsE <- treeItem "components" False
                      (treeSelect "workspace" (return never) $ do
                          elAttr "img" ("src" =: "/pics/ide_component.png") $ return ()
                          text " Components"
                          return never) $
                      el "ul" $
                        fmap (fmapMaybe (listToMaybe . M.elems)) . listViewWithKey (M.fromList . zip [0::Int ..] . components <$> packageD) $ \_ componentD -> do
                          let isActiveComponentD = (&&) <$> isActivePackageD <*> ((==) <$> activeComponentD <*> (Just <$> componentD))
                          elDynClass "li" (("component" <>) <$> (bool "" " active" <$> isActiveComponentD)) $
                            treeSelect "workspace" (menu
                              [ ("Activate",) . PackageCommand . CommandWorkspaceAction "Set as Active Component" "" <$>
                                  (workspaceActivatePackage <$> projectD <*> (Just <$> packageD) <*> (Just <$> componentD))
                              ]) $ do
                              elAttr "img" ("src" =: "/pics/ide_component.png") $ return ()
                              dynText $ (" " <>) <$> componentD
                              return never
                    _ <- elClass "li" "branch" $
                      treeSelect "workspace" (return never) $ do
                        elAttr "img" ("src" =: "/pics/ide_git.png") $ return ()
                        text " master"
                        return never
                    filesE <- treeItem "package-files" False (treeSelect "workspace" (return never) $ do
                      elAttr "img" ("src" =: "/pics/ide_folder.png") $ return ()
                      text " Files"
                      return never) $
                        el "ul" $ do
                          sourceDirsD <- holdUniqDyn $ absolutSourceDirs <$> packageD
                          dirD <- holdUniqDyn $ dropFileName . ipdCabalFile <$> packageD
                          (switchHold never =<<) . dyn $ fileTree "workspace" <$> sourceDirsD <*> dirD
                    return $ leftmost [componentsE, PackageFileEvents <$> filesE]
              pjSourceDirsD <- holdUniqDyn $ mconcat . (fmap absolutSourceDirs . pjPackages) <$> projectD
              projectFilesE <- treeItem "project-files" False (treeSelect "workspace" (return never) $ do
                  elAttr "img" ("src" =: "/pics/ide_folder.png") $ return ()
                  text " Files"
                  return never) $
                    el "ul" $
                      (switchHold never =<<) . dyn $ (\d -> fileTree "workspace" d (pjDir pKey)) <$> pjSourceDirsD
              return $ leftmost [ProjectPackageEvents <$> packagesE, ProjectFileEvents <$> projectFilesE]
