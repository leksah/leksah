{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Widget.Workspace (
    workspaceCss
  , workspaceWidget
) where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Lens
       (to, view, preview, _Just)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)

import Data.Bool (bool)
import Data.List (stripPrefix, isPrefixOf, dropWhileEnd)
import qualified Data.Map as M (elems, fromList)
import Data.Maybe (listToMaybe, maybeToList, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S (fromList, member)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Data.Time.Clock (UTCTime)

import System.Directory (getModificationTime, doesFileExist)
import System.FilePath ((<.>), (</>), dropFileName, dropTrailingPathSeparator)

import Clay
       (pct, hover, width, bold, fontWeight, paddingBottom,
        borderRadius, borderStyle, backgroundImage, vGradient,
        paddingRight, marginBottom, marginTop, checked,
        userSelect, (|+), absolute, position, left, nil, paddingLeft, px,
        marginLeft, listStyleType, listStyleImage, middle, grey, color, rgb,
        nowrap, whiteSpace, inlineBlock, scroll, overflow, height, (?),
        Css, background, none, white, Color(..), VerticalAlign(..),
        cursorDefault, Cursor(..))
import qualified Clay (display, (#))
import Clay.Stylesheet (key)

import Reflex
       (leftmost, listViewWithKey, switchHold, constDyn, ffilter, ffor, updated,
        tag, current, getPostBuild, holdUniqDyn, holdDyn, performEvent,
        performEvent_, newTriggerEvent, tickLossyFromPostBuildTime, Dynamic,
        Event, never, fmapMaybe, tagPromptlyDyn)
import Reflex.Dom.Core
       (elDynClass, MonadWidget, elAttr, dyn, button, (=:), elDynAttr,
        divClass, text, el, elClass, dynText)

import IDE.Core.CTypes (packageIdentifierToString)
import IDE.Core.State
       (DebugState(..), activeComponent, ipdPackageDir,
        ipdLib, pjDir, IDEPackage(..), runPackage, runProject,
        pjPackages, Project(..), wsFile, workspace, wsProjects, IDE,
        activeProject, activePack, debugState, pjFile, pjFileOrDir,
        ProjectKey(..), pjCabalFile, prefs, showHiddenFiles, showIgnoredFiles)
import IDE.Gtk.Package (packageRun)
import IDE.Gtk.Workspaces (makePackage)
import IDE.Package
       (packageClean, packageBench, packageTest, projectRefreshNix,
        packageOpenRepl)
import IDE.Web.Command (Command(..))
import IDE.Web.Events (PackageEvent(..), ProjectEvent(..), ProjectEvents, FileEvent(..))
import IDE.Web.Widget.Flake
       (FlakeResult, flakeOutputs, flakeSystemCategories, flakeSystemNames,
        flakeTreeWidget, runButton, openNixWindow, developAttr)
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Widget.FileTree (fileTree)
import IDE.Web.Widget.Tree (treeItemDynAttr, treeItemDynAttr', treeSelect, treeItem, treeItem')
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
    -- The run (>) buttons at the right of component / flake / shell rows:
    -- the same dark look as the Terminals pane's buttons, kept subtle until
    -- hovered.
    ".workspace .ws-run" ? do
        color white
        borderStyle none
        borderRadius (px 3) (px 3) (px 3) (px 3)
        backgroundImage (vGradient (Rgba 64 64 64 1.0) (Rgba 40 40 40 1.0))
        key "font-size" ("11px" :: Text)
        key "font-weight" ("bold" :: Text)
        key "padding" ("0 4px" :: Text)
        key "margin-left" ("8px" :: Text)
        key "opacity" ("0.55" :: Text)
        key "vertical-align" ("middle" :: Text)
        cursor cursorDefault
    (".workspace .ws-run" Clay.# hover) ? do
        key "opacity" ("1" :: Text)
        backgroundImage (vGradient (Rgba 84 84 84 1.0) (Rgba 60 60 60 1.0))
    -- Git status decorations on the file tree (VS Code-style colours).
    ".git-modified"  ? color (rgb 0xe2 0xc0 0x8d)
    ".git-added"     ? color (rgb 0x73 0xc9 0x91)
    ".git-untracked" ? color (rgb 0x73 0xc9 0x91)
    ".git-renamed"   ? color (rgb 0x73 0xc9 0x91)
    ".git-deleted"   ? color (rgb 0xc7 0x4e 0x39)
    ".git-conflict"  ? color (rgb 0xe4 0x67 0x6b)
    ".git-badge" ? do
        paddingLeft (px 6)
        fontWeight bold
    -- The focused source file (the editor tab being shown) is highlighted; a
    -- find match can also be a directory.
    ".workspace li.file.active" ?
        background (Rgba 30 88 209 1.0)
    ".workspace li.dir.active > label" ?
        background (Rgba 30 88 209 1.0)

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

-- | Is the focused file somewhere under @dir@?
fileUnder :: FilePath -> Maybe FilePath -> Bool
fileUnder dir = maybe False ((dropWhileEnd (== '/') dir <> "/") `isPrefixOf`)

-- | An event that fires @True@ when the focused file is under @dir@ — used to
-- auto-expand a tree node so the file is revealed.  Checked on focus change and
-- when the node is (re)built (its parent just opened).
revealUnder
  :: MonadWidget t m
  => Dynamic t FilePath -> Dynamic t (Maybe FilePath) -> m (Dynamic t Bool)
revealUnder dirD activeFileD = revealUnderExcept dirD (constDyn mempty) activeFileD

-- | Whether the active file lives under the given directory but not under one of
-- the excluded directories (e.g. a nested package's directory) — so a node isn't
-- revealed towards a file that is actually shown under a more-specific node.
-- Returned as a 'Dynamic' so the (lazily-rendered) tree node can check it at its
-- own postBuild and open even when rendered after its parent expands.
revealUnderExcept
  :: MonadWidget t m
  => Dynamic t FilePath -> Dynamic t (Set FilePath) -> Dynamic t (Maybe FilePath)
  -> m (Dynamic t Bool)
revealUnderExcept dirD exceptD activeFileD =
  pure $ (\dir except mf -> fileUnder dir mf && not (any (\d -> fileUnder d mf) except))
           <$> dirD <*> exceptD <*> activeFileD

-- | A file's modification time, or 'Nothing' if it doesn't exist / can't be
-- read.  Used to cheaply detect when a flake's @flake.nix@/@flake.lock@ changed
-- so its outputs can be re-evaluated.
safeMtime :: FilePath -> IO (Maybe UTCTime)
safeMtime f =
  either (const Nothing) Just <$> (try (getModificationTime f) :: IO (Either SomeException UTCTime))

-- | The collapsed \"Flake\" tree node for the project directory @dir@, shown
-- only when @dir/flake.nix@ exists.  Its children — built, and so evaluated,
-- only when it is expanded — are one node per top-level output category that
-- has a @${builtins.currentSystem}@ attribute under it (devShells, packages,
-- …), each listing that category's current-system names, and finally an
-- \"All Outputs\" node with the full lazily-drillable output tree (see
-- "IDE.Web.Widget.Flake").  The row's run (>) button opens @nix repl .#@.
flakeNode :: MonadWidget t m => FilePath -> m ()
flakeNode dir = do
  pb <- getPostBuild
  hasFlakeE <- performEvent $ ffor pb $ \_ -> liftIO (doesFileExist (dir </> "flake.nix"))
  hasFlakeD <- holdUniqDyn =<< holdDyn False hasFlakeE
  void . dyn $ ffor hasFlakeD $ \hasFlake -> when hasFlake . void $
    treeItem "flake" False
      (treeSelect "workspace" (return never) $ do
          elAttr "img" ("src" =: "/pics/ide_nix.png") (return ())
          text " Flake"
          runE <- runButton "nix repl .#"
          performEvent_ $ ffor runE $ \_ -> liftIO $
              openNixWindow dir "nix repl" "nix repl .# --show-trace"
          return never)
      (do
          -- Which categories are per-system: found when expanded (re-found
          -- on each re-expand — that's the refresh).
          (catsE, fireCats) <- newTriggerEvent
          cpb <- getPostBuild
          performEvent_ $ ffor cpb $ \_ ->
              liftIO . void . forkIO $ flakeSystemCategories dir >>= fireCats
          catsD <- holdDyn Nothing (Just <$> catsE)
          el "ul" $ do
              _ <- dyn $ ffor catsD $ \case
                  Nothing         -> divClass "flake-hint"  $ text "evaluating…"
                  Just (Left err) -> divClass "flake-error" $ text err
                  Just (Right (_, cats)) -> mapM_ (systemCatNode dir) cats
              allOutputsNode dir
          return never)

-- | One per-system output category (devShells, packages, …): its children
-- are the names under @<category>.${currentSystem}@, each with a run (>)
-- button opening @nix develop@ for it; when a @default@ name exists the
-- collapsed row itself gets the button.
systemCatNode :: MonadWidget t m => FilePath -> Text -> m ()
systemCatNode dir cat = do
  (namesE, fireNames) <- newTriggerEvent
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ ->
      liftIO . void . forkIO $ flakeSystemNames dir cat >>= fireNames
  resultD <- holdDyn (Right ("", [])) namesE
  hasDefaultD <- holdUniqDyn $ either (const False) (elem "default" . snd) <$> resultD
  void $ treeItem "flake-node" False
    (do elClass "span" "flake-label" (text (" " <> cat))
        -- Open the default entry without having to expand the node.
        void . dyn $ ffor hasDefaultD $ \hasDef -> when hasDef $ do
            runE <- runButton ("nix develop .#" <> cat <> ".default")
            performEvent_ $ ffor (tagPromptlyDyn resultD runE) $ \case
                Right (sys, names) | "default" `elem` names ->
                    liftIO $ developAttr dir (cat <> "." <> sys <> ".default")
                _ -> return ()
        return never)
    (do
        _ <- el "ul" . dyn $ ffor resultD $ \case
            Left err      -> divClass "flake-error" $ text err
            Right (_, []) -> divClass "flake-hint"  $ text "(none)"
            Right (sys, names) -> mapM_ (nameRow sys) names
        return never)
  where
    nameRow sys nm = elClass "li" "flake-leaf" $ do
        text (" " <> nm)
        runE <- runButton ("nix develop .#" <> cat <> "." <> sys <> "." <> nm)
        performEvent_ $ ffor runE $ \_ -> liftIO $
            developAttr dir (cat <> "." <> sys <> "." <> nm)

-- | \"All Outputs\": the full flake-outputs tree.  Evaluated only while
-- expanded (re-evaluating when flake.nix/flake.lock change); deeper levels
-- drill lazily (see 'flakeTreeWidget').
allOutputsNode :: MonadWidget t m => FilePath -> m ()
allOutputsNode dir = void $ treeItem "flake-node" False
    (do elClass "span" "flake-label" (text " All Outputs"); return never)
    (el "ul" $ do
        ipb <- getPostBuild
        ptick <- tickLossyFromPostBuildTime 2
        mtimeE <- performEvent $ ffor (leftmost [ipb, () <$ ptick]) $ \_ -> liftIO $
            (,) <$> safeMtime (dir </> "flake.nix") <*> safeMtime (dir </> "flake.lock")
        mtimeD <- holdUniqDyn =<< holdDyn (Nothing, Nothing) mtimeE
        (resultE, fireResult) <- newTriggerEvent
        -- The first mtime read (≈ on expand) and any later change re-evaluate.
        performEvent_ $ ffor (() <$ updated mtimeD) $ \_ ->
            liftIO . void . forkIO $ flakeOutputs dir >>= fireResult
        resultD <- holdDyn (Right [] :: FlakeResult) resultE
        flakeTreeWidget dir resultD
        return never)

workspaceWidget
  :: MonadWidget t m
  => Dynamic t IDE
  -> Dynamic t (Maybe FilePath)   -- ^ the focused file (highlighted in the tree)
  -> Dynamic t (Maybe FilePath)   -- ^ the file to reveal (expand/scroll to), or
                                  --   Nothing when an occurrence is already shown
  -> m (Event t ProjectEvents)
workspaceWidget ide activeFileD revealFileD = do
  debugPackagesD <- holdUniqDyn $ S.fromList .
      (>>= (\DebugState{..} -> map ((dsProjectKey,) . ipdCabalFile) dsPackages)) . view debugState <$> ide
  showHiddenD  <- holdUniqDyn $ view (prefs . to showHiddenFiles)  <$> ide
  showIgnoredD <- holdUniqDyn $ view (prefs . to showIgnoredFiles) <$> ide
  divClass "workspace leksah-nav" $
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
          -- Reveal (expand) the project when the active file is anywhere under it.
          projNodeRevealD <- revealUnder (constDyn (pjDir pKey)) revealFileD
          treeItemDynAttr' projNodeRevealD (("class" =:) . ("project" <>) <$> (bool "" " active" <$> isActiveProjectD)) True
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
              elAttr "img" ("src" =: "/pics/ide_source_dependency.png") $ return ()
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
                pkgDirD <- holdUniqDyn $ dropFileName . ipdCabalFile <$> packageD
                -- Directories of other packages *nested inside* this one (a file
                -- there belongs to the nested package, not this one): used to hide
                -- them from this package's tree and to not reveal towards them.
                nestedPkgDirsD <- holdUniqDyn $ (\proj p ->
                      let myDir = dropTrailingPathSeparator (ipdPackageDir p)
                      in S.fromList
                         [ d | p' <- pjPackages proj, ipdCabalFile p' /= ipdCabalFile p
                             , let d = dropTrailingPathSeparator (ipdPackageDir p')
                             , (myDir <> "/") `isPrefixOf` d ])
                    <$> projectD <*> packageD
                pkgRevealE <- revealUnderExcept pkgDirD nestedPkgDirsD revealFileD
                let isActivePackageD = (&&) <$> isActiveProjectD <*> ((==) <$> activePackageFileD <*> (Just <$> cabalFileD))
                    pkgCmd t f = (t,) . PackageCommand . CommandWorkspaceAction "" "" <$> (runProject . runPackage f <$> packageD <*> projectD)
                treeItemDynAttr' pkgRevealE (("class" =:) . ("package" <>) <$> (bool "" " active" <$> isActivePackageD)) False
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
                    -- The package's cabal-file path (relative to the project)
                    -- is a tooltip, not an inline label — it was crowding the
                    -- row.
                    relPathD <- holdUniqDyn $
                        (\cf -> T.pack $ fromMaybe cf $ stripPrefix (pjDir pKey) cf) <$> cabalFileD
                    elDynAttr "div" ((\p -> "class" =: "package-id" <> "title" =: p) <$> relPathD) $ do
                      text $ packageIdentifierToString packageId
                      dynText $ do
                        isActive <- isActivePackageD
                        activeComp <- activeComponentD
                        mbLib <- mbLibD
                        return $ if isActive
                          then maybe (if isJust mbLib then " (library)" else "")
                                   (\comp -> " (" <> comp <> ")") activeComp
                          else ""
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
                              -- The repl (>) button: bring up the component's
                              -- ffcabal repl window as a terminal tab.
                              case pKey of
                                CabalTool {} -> do
                                  runE <- runButton "Open component repl (ffcabal)"
                                  let runActD = (\proj pkg comp ->
                                          PackageCommand . CommandWorkspaceAction "" "" $
                                            runProject (runPackage (packageOpenRepl comp) pkg) proj)
                                        <$> projectD <*> packageD <*> componentD
                                  return $ tagPromptlyDyn runActD runE
                                _ -> return never
                    _ <- elClass "li" "branch" $
                      treeSelect "workspace" (return never) $ do
                        elAttr "img" ("src" =: "/pics/ide_git.png") $ return ()
                        text " master"
                        return never
                    filesE <- treeItem' pkgRevealE "package-files" False (treeSelect "workspace" (return never) $ do
                      elAttr "img" ("src" =: "/pics/ide_folder.png") $ return ()
                      text " Files"
                      return never) $
                        el "ul" $ do
                          sourceDirsD <- holdUniqDyn $ absolutSourceDirs <$> packageD
                          dirD <- holdUniqDyn $ dropFileName . ipdCabalFile <$> packageD
                          (switchHold never =<<) . dyn $
                            (\sd ig d -> fileTree "workspace" sd ig showHiddenD showIgnoredD activeFileD revealFileD d)
                              <$> sourceDirsD <*> nestedPkgDirsD <*> dirD
                    return $ leftmost [componentsE, PackageFileEvents <$> filesE]
              pjSourceDirsD <- holdUniqDyn $ mconcat . (fmap absolutSourceDirs . pjPackages) <$> projectD
              -- The project's own ("Other Files") tree excludes the directory of
              -- every package under the project root; and it's left out entirely
              -- when a package sits at the project root (its files would just
              -- duplicate that package's tree).
              let projDir = dropTrailingPathSeparator (pjDir pKey)
              pkgDirsD <- holdUniqDyn $ (\proj -> S.fromList
                  [ d | p <- pjPackages proj
                      , let d = dropTrailingPathSeparator (ipdPackageDir p)
                      , (projDir <> "/") `isPrefixOf` d ])
                <$> projectD
              hasRootPackageD <- holdUniqDyn $
                any ((== projDir) . dropTrailingPathSeparator . ipdPackageDir) . pjPackages <$> projectD
              projectFilesE <- switchHold never =<< dyn (ffor hasRootPackageD $ \hasRoot ->
                if hasRoot then return never else do
                  projRevealE <- revealUnderExcept (constDyn (pjDir pKey)) pkgDirsD revealFileD
                  treeItem' projRevealE "project-files" False (treeSelect "workspace" (return never) $ do
                      elAttr "img" ("src" =: "/pics/ide_folder.png") $ return ()
                      -- A nix project's tree has no package "Files" nodes to
                      -- distinguish from, so plain "Files" reads better.
                      text $ case pKey of
                        NixTool {} -> " Files"
                        _          -> " Other Files"
                      return never) $
                        el "ul" $
                          (switchHold never =<<) . dyn $
                            (\sd ig -> fileTree "workspace" sd ig showHiddenD showIgnoredD activeFileD revealFileD (pjDir pKey))
                              <$> pjSourceDirsD <*> pkgDirsD)
              -- Any project with a flake.nix gets a (collapsed) Flake node;
              -- it self-hides when there's no flake and only evaluates once
              -- expanded, so there's no overhead otherwise.
              flakeNode (pjDir pKey)
              return $ leftmost [ProjectPackageEvents <$> packagesE, ProjectFileEvents <$> projectFilesE]
