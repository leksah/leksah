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
import Data.List (stripPrefix, isPrefixOf, dropWhileEnd, find)
import qualified Data.Map as M (elems, fromList, keys)
import Data.Maybe (listToMaybe, maybeToList, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S (fromList, member)
import Data.Text (Text)
import qualified Data.Text as T (pack, strip, null, takeWhile)

import System.Exit (ExitCode(..))
import System.FilePath
       ((<.>), (</>), dropFileName, dropTrailingPathSeparator, takeFileName,
        splitDirectories, joinPath)

import IDE.Git (runGit)
import IDE.Utils.RemotePath (isRemotePath, parseRemotePath)
import IDE.Web.RemoteSettingsRequest (requestRemoteSettings)
import IDE.Web.ReplTmux (openTerminalInDir)
import IDE.Web.FS (fsDoesFileExist)
import IDE.Web.RemoteRefresh (registerRemoteRefresh)
import IDE.Web.LocalRefresh (registerLocalRefresh)

import Clay
       (pct, hover, width, bold, fontWeight, paddingBottom,
        borderRadius, borderStyle, backgroundImage, vGradient,
        paddingRight, marginBottom, marginTop, marginRight, checked,
        userSelect, (|+), absolute, position, left, nil, paddingLeft, px,
        marginLeft, listStyleType, listStyleImage, middle, grey, color, rgb,
        opacity, nowrap, whiteSpace, inlineBlock, scroll, overflow, height, (?),
        Css, background, none, white, Color(..), VerticalAlign(..),
        cursorDefault, Cursor(..))
import qualified Clay (display, (#))
import Clay.Stylesheet (key)

import Reflex
       (leftmost, listViewWithKey, switchHold, constDyn, ffor, updated,
        current, getPostBuild, holdUniqDyn, holdDyn, performEvent,
        performEvent_, newTriggerEvent, Dynamic,
        Event, never, fmapMaybe, tagPromptlyDyn, sample)
import Reflex.Dom.Core
       (elDynClass, MonadWidget, elAttr, dyn, button, (=:), elDynAttr,
        divClass, text, el, elClass, dynText, domEvent, EventName(..))

import IDE.Web.Theme (selectionColor, hoverColor, dimColor, dimOpacity)
import IDE.Core.CTypes (packageIdentifierToString)
import IDE.Core.State
       (DebugState(..), activeComponent, ipdPackageDir,
        ipdLib, pjDir, IDEPackage(..), runPackage, runProject,
        pjPackages, Project(..), workspace, wsProjects, IDE,
        activeProject, activePack, debugState, pjFile, pjFileOrDir,
        ProjectKey(..), pjCabalFile, prefs, showHiddenFiles, showIgnoredFiles)
import IDE.Gtk.Package (packageRun)
import IDE.Gtk.Workspaces (makePackage)
import IDE.Package
       (packageClean, packageBench, packageTest, projectRefreshNix,
        packageOpenRepl, packageRunComponentTerm, projectOpenTerminal)
import IDE.Web.Command (Command(..))
import IDE.Web.Events (PackageEvent(..), ProjectEvent(..), ProjectEvents, FileEvent(..))
import IDE.Web.Widget.Flake
       (FlakeResult, flakeOutputs, flakeSystemCategories, flakeSystemNames,
        flakeTreeWidget, execButton, openNixWindow, developAttr)
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Widget.FileTree (fileTree)
import IDE.Web.Widget.Tree
       (treeItemDynAttr', treeSelect, treeSelect', treeItem,
        treeItem')
import IDE.Workspaces
       (workspaceRemoveProject, workspaceActivatePackage)

workspaceCss :: Css
workspaceCss = do
    ".workspace" ? do
        height (pct 100)
        key "fill" grey
        overflow scroll
        -- A uniform right inset so the run buttons line up clear of the
        -- scrollbar.  On the PANE, not the rows — row padding would compound
        -- per nesting level and step deeper buttons leftward.
        key "padding-right" ("8px" :: Text)
        key "box-sizing" ("border-box" :: Text)
    ".workspace li.active > .tree-expand" ?
        key "fill" white
    ".workspace li > .tree-expand" Clay.# hover ?
        key "fill" selectionColor
    -- De-emphasis instead of emphasis: rather than bolding the active row, dim
    -- every *other* row's label + icon to light grey (git-status colours on a
    -- name still win, staying legible).  The active row keeps full-brightness
    -- white text and a full-opacity icon.
    ".workspace label" ?
        color dimColor
    ".workspace img.tree-icon" ?
        opacity dimOpacity
    ".workspace li.active > label" ?
        color white
    ".workspace li.active > label img.tree-icon" ?
        opacity 1
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
    -- B&W tree-item icons (Workspace/Terminals/Metadata nodes): 16px, centred
    -- with the label, with a little gap before the text.
    "img.tree-icon" ? do
        width (px 16)
        height (px 16)
        verticalAlign middle
        marginRight (px 4)
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
        background selectionColor
    -- The run (>) buttons at the right of component / flake / shell rows:
    -- the same dark look as the Terminals pane's buttons, kept subtle until
    -- hovered.  margin-left auto pushes them to the row's right edge (the
    -- rows are flex, below); the extra bottom padding rides the glyph 2px
    -- high, which optically centres it.
    ".workspace .ws-run" ? do
        color white
        borderStyle none
        borderRadius (px 3) (px 3) (px 3) (px 3)
        backgroundImage (vGradient (Rgba 64 64 64 1.0) (Rgba 40 40 40 1.0))
        key "font-size" ("11px" :: Text)
        key "font-weight" ("bold" :: Text)
        key "padding" ("0 4px 2px 4px" :: Text)
        -- gap between adjacent buttons matches the Terminals tree's action
        -- glyphs (2px); the row's FIRST button is pushed right (below)
        key "margin-left" ("2px" :: Text)
        key "opacity" ("0.55" :: Text)
        cursor cursorDefault
    -- the FIRST button of a row is pushed to the right edge; any later
    -- buttons (exe/test/bench rows have repl + run) trail it
    ".workspace .ws-run:first-of-type" ?
        key "margin-left" ("auto" :: Text)
    (".workspace .ws-run" Clay.# hover) ? do
        key "opacity" ("1" :: Text)
        backgroundImage (vGradient (Rgba 84 84 84 1.0) (Rgba 60 60 60 1.0))
    -- Rows that carry a run button lay out like the Terminals pane's rows:
    -- flexbox, label taking the free space, the button at the right edge,
    -- expanded children wrapping onto their own full-width line.
    ".workspace li.component, .workspace li.flake, .workspace li.flake-node, .workspace li.flake-leaf" ? do
        key "display" ("flex" :: Text)
        key "flex-wrap" ("wrap" :: Text)
        key "align-items" ("center" :: Text)
    ".workspace li.component > label, .workspace li.flake > label, .workspace li.flake-node > label, .workspace li.flake-leaf > label" ? do
        key "flex" ("1" :: Text)
        key "min-width" ("0" :: Text)
    ".workspace li.component > label > .tree-item, .workspace li.flake > label > .tree-item, .workspace li.flake-node > label > .tree-item, .workspace li.flake-leaf > label > .tree-item" ? do
        key "display" ("flex" :: Text)
        key "align-items" ("center" :: Text)
        width (pct 100)
        key "box-sizing" ("border-box" :: Text)
        -- the tree-item's own 2px right padding would put these buttons 2px
        -- left of the ones that sit directly in their li
        paddingRight nil
    ".workspace .tree-children" ? key "flex-basis" ("100%" :: Text)
    -- Hovering a run button highlights its whole row line — label through
    -- the area behind the button — with the (configurable) hover colour; the
    -- button itself keeps its normal look.  The background is clipped to the
    -- row's first line so it doesn't bleed over expanded children.
    ".workspace li.component:has(> label .ws-run:hover), .workspace li.flake:has(> label .ws-run:hover), .workspace li.flake-node:has(> label .ws-run:hover), .workspace li.flake-leaf:has(> label .ws-run:hover)" ? do
        backgroundImage (vGradient hoverColor hoverColor)
        key "background-size" ("100% 20px" :: Text)
        key "background-repeat" ("no-repeat" :: Text)
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
        background selectionColor
    ".workspace li.dir.active > label" ?
        background selectionColor

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

-- | Tooltip for a runnable component's ▶ button — the cabal subcommand it
-- runs (@run@ for exes, @test@\/@bench@ for the others).
runComponentTip :: Text -> Text
runComponentTip comp = "cabal " <> sub <> " " <> comp
  where sub = case T.takeWhile (/= ':') comp of
                "test"  -> "test"
                "bench" -> "bench"
                _       -> "run"

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

-- | The shortest right-anchored suffix (by path segment) of @dir@ that is
-- unique among @allDirs@ on the same server, so the project label shows just
-- enough of the path to tell projects apart (the full path is the tooltip).
-- Uniqueness is per-server: the host pill already distinguishes hosts, so only
-- projects sharing this project's host (or all-local, for a local project) can
-- force the suffix longer.  @dir@/@allDirs@ are project directories, possibly
-- @ssh:\/\/HOST\/…@; the host is stripped before comparing path segments.
shortProjectSuffix :: FilePath -> [FilePath] -> Text
shortProjectSuffix dir allDirs =
    T.pack (joinPath (takeEnd k segs))
  where
    split q = case parseRemotePath q of
                Just (h, l) -> (Just h, dropTrailingPathSeparator l)
                Nothing     -> (Nothing, dropTrailingPathSeparator q)
    (mbHost, local) = split dir
    segs = splitDirectories local
    n = length segs
    takeEnd i xs = drop (length xs - i) xs
    others = [ l | q <- allDirs, let (h, l) = split q, (h, l) /= (mbHost, local), h == mbHost ]
    isUniq i = let s = takeEnd i segs
               in all ((/= s) . takeEnd i . splitDirectories) others
    k = fromMaybe n (find isUniq [1 .. n])

-- | A git branch node: the checkout's current branch, read in the
-- background; hidden entirely when @dir@ isn't inside a git checkout.
gitBranchNode :: forall t m . MonadWidget t m => FilePath -> m ()
gitBranchNode dir = do
  pb <- getPostBuild
  (brE, fireBr) <- newTriggerEvent
  let scan = void . forkIO $ do
        r <- try (runGit dir ["rev-parse", "--abbrev-ref", "HEAD"])
        fireBr $ case r :: Either SomeException (ExitCode, Text, Text) of
            Right (ExitSuccess, out, _)
              | b <- T.strip out, not (T.null b) -> Just b
            _ -> Nothing
  performEvent_ $ liftIO scan <$ pb
  -- Remote projects: re-read the branch on refresh events (no polling).
  when (isRemotePath dir) . void . liftIO $
      registerRemoteRefresh (\_ -> scan)
  brD <- holdDyn Nothing brE
  void . dyn $ ffor brD $ \case
      Nothing -> return ()
      Just b  -> void . elClass "li" "branch" $
          treeSelect "workspace" (return never) $ do
              elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-git.svg") $ return ()
              text b
              return (never :: Event t ())

-- | The collapsed \"Flake\" tree node for the project directory @dir@, shown
-- only when @dir/flake.nix@ exists.  Its children — built, and so evaluated,
-- only when it is expanded — are one node per top-level output category that
-- has a @${builtins.currentSystem}@ attribute under it (devShells, packages,
-- …), each listing that category's current-system names, and finally an
-- \"All Outputs\" node with the full lazily-drillable output tree (see
-- "IDE.Web.Widget.Flake").  Double-clicking the row opens @nix repl .#@.
flakeNode :: MonadWidget t m => FilePath -> m ()
flakeNode dir = do
  pb <- getPostBuild
  hasFlakeE <- performEvent $ ffor pb $ \_ -> liftIO (fsDoesFileExist (dir </> "flake.nix"))
  hasFlakeD <- holdUniqDyn =<< holdDyn False hasFlakeE
  void . dyn $ ffor hasFlakeD $ \hasFlake -> when hasFlake . void $
    treeItem "flake" False
      (do (rowEl, _) <- treeSelect' "workspace" (return never) $ do
              elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/nix.svg") (return ())
              text "Flake"
              return never
          -- double-click opens `nix repl .#` (no inline button)
          performEvent_ $ ffor (domEvent Dblclick rowEl) $ \_ -> liftIO $
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
-- are the names under @<category>.${currentSystem}@, each double-clickable to
-- open @nix develop@ for it; when a @default@ name exists the collapsed row
-- itself opens it on double-click.
systemCatNode :: forall t m . MonadWidget t m => FilePath -> Text -> m ()
systemCatNode dir cat = do
  (namesE, fireNames) <- newTriggerEvent
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ ->
      liftIO . void . forkIO $ flakeSystemNames dir cat >>= fireNames
  resultD <- holdDyn (Right ("", [])) namesE
  void $ treeItem "flake-node" False
    (do let runDefault = \case
                Right (sys, names) | "default" `elem` names ->
                    liftIO $ developAttr dir (cat <> "." <> sys <> ".default")
                _ -> return ()
        (rowEl, _) <- treeSelect' "workspace" (return never) $ do
            elClass "span" "flake-label" (text (" " <> cat))
            return never
        -- double-click opens the category's `default` entry (no inline button)
        performEvent_ $ ffor (tagPromptlyDyn resultD (domEvent Dblclick rowEl)) runDefault
        return never)
    (do
        _ <- el "ul" . dyn $ ffor resultD $ \case
            Left err      -> divClass "flake-error" $ text err
            Right (_, []) -> divClass "flake-hint"  $ text "(none)"
            Right (sys, names) -> mapM_ (nameRow sys) names
        return never)
  where
    nameRow sys nm = elClass "li" "flake-leaf" $ do
        (rowEl, _) <- treeSelect' "workspace" (return never) $ do
            text (" " <> nm)
            return (never :: Event t ())
        -- double-click opens `nix develop .#…` (no inline button)
        performEvent_ $ ffor (domEvent Dblclick rowEl) $ \_ -> liftIO $
            developAttr dir (cat <> "." <> sys <> "." <> nm)

-- | \"All Outputs\": the full flake-outputs tree.  Evaluated only while
-- expanded (re-evaluating when flake.nix/flake.lock change); deeper levels
-- drill lazily (see 'flakeTreeWidget').
allOutputsNode :: MonadWidget t m => FilePath -> m ()
allOutputsNode dir = void $ treeItem "flake-node" False
    (treeSelect "workspace" (return never) $ do
        elClass "span" "flake-label" (text " All Outputs")
        return never)
    (el "ul" $ do
        ipb <- getPostBuild
        (resultE, fireResult) <- newTriggerEvent
        if isRemotePath dir
          then do
            -- Remote: no mtime polling — evaluate on expand and on
            -- RemoteRefresh events (the eval itself runs on the host).
            performEvent_ $ ffor ipb $ \_ ->
                liftIO . void . forkIO $ flakeOutputs dir >>= fireResult
            void . liftIO $ registerRemoteRefresh
                (\_ -> void . forkIO $ flakeOutputs dir >>= fireResult)
          else do
            -- Local: no mtime polling — evaluate on expand and when an
            -- fsnotify LocalRefresh reports flake.nix / flake.lock changed
            -- (both live directly in the project dir, so they're covered by
            -- the project-dir watcher in IDE.Workspaces.Writer).
            performEvent_ $ ffor ipb $ \_ ->
                liftIO . void . forkIO $ flakeOutputs dir >>= fireResult
            let base = dropTrailingPathSeparator dir
            void . liftIO $ registerLocalRefresh $ \p ->
                when (takeFileName p `elem` ["flake.nix", "flake.lock"]
                      && (base <> "/") `isPrefixOf` p) $
                    void . forkIO $ flakeOutputs dir >>= fireResult
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
      elClass "ul" "projects" $ do
        let addFileToKey = map (\(n, p) -> ((n, pjKey p), p))
            projectsD = M.fromList . addFileToKey . zip [0..] . fromMaybe mempty . preview (workspace . _Just . wsProjects) <$> ide
        -- Every project's directory, for computing the shortest label suffix
        -- that uniquely identifies each project (see 'shortProjectSuffix').
        allProjectDirsD <- holdUniqDyn $ map (pjDir . snd) . M.keys <$> projectsD
        activeProjectKeyD <- holdUniqDyn $ fmap pjKey . view activeProject <$> ide
        activePackageFileD <- holdUniqDyn $ fmap ipdCabalFile . view activePack <$> ide
        activeComponentD <- holdUniqDyn $ view activeComponent <$> ide
        listViewWithKey projectsD $ \(_, pKey) projectD -> do
          let isActiveProjectD = (== Just pKey) <$> activeProjectKeyD
          -- Reveal (expand) the project when the active file is anywhere under it.
          projNodeRevealD <- revealUnder (constDyn (pjDir pKey)) revealFileD
          treeItemDynAttr' projNodeRevealD (("class" =:) . ("project" <>) <$> (bool "" " active" <$> isActiveProjectD)) True
            (do
              (projRowEl, rowE) <- treeSelect' "workspace" (menu $
                [ ("Activate",) . ProjectCommand . CommandWorkspaceAction "Set as Active Project" "" <$>
                    (workspaceActivatePackage <$> projectD <*> pure Nothing <*> pure Nothing)
                ] <> case pjFile pKey of
                        Just file -> [ constDyn ("Open Project File", ProjectFileEvents . ("" =:) $ OpenFile False file) ]
                        _ -> []
                  <> case pKey of
                        CabalTool p ->
                          [ constDyn ("Open Project Configuration File", ProjectFileEvents . ("" =:) . OpenFile True $ pjCabalFile p <.> "local") ]
                        _ -> []
                <> [ ("Open Terminal Here",) . ProjectCommand . CommandWorkspaceAction "" "" <$>
                    (runProject projectOpenTerminal <$> projectD)
                , ("Refresh Nix Environment Varialbes",) . ProjectCommand . CommandWorkspaceAction "" "" <$>
                    (runProject projectRefreshNix <$> projectD)
                , constDyn ("Remove From Workspace", ProjectCommand (CommandWorkspaceAction "" "" (workspaceRemoveProject pKey)))
                , constDyn ("Project Settings…", ProjectCommand (CommandWorkspaceAction "" "" (liftIO (requestRemoteSettings pKey))))
                ]) $ do
                elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-project.svg") $ return ()
                -- Label = (for a remote project) the server name, then the
                -- shortest right-anchored path suffix that uniquely identifies
                -- this project among the workspace; the full path is the
                -- tooltip.  The server name is plain text, not a highlighted
                -- pill.
                let (mbHost, fullLocal) = case parseRemotePath (pjFileOrDir pKey) of
                        Just (host, l) -> (Just host, l)
                        Nothing        -> (Nothing, pjFileOrDir pKey)
                    fullTitle = maybe id (\h t -> h <> ":" <> t) mbHost (T.pack fullLocal)
                suffixD <- holdUniqDyn $ shortProjectSuffix (pjDir pKey) <$> allProjectDirsD
                let labelD = maybe id (\h s -> h <> ":" <> s) mbHost <$> suffixD
                elAttr "span" ("title" =: fullTitle) $ dynText labelD
                return never
              -- Double-click a project row → open a terminal at its directory.
              performEvent_ $ openTerminalInDir (pjDir pKey) <$ domEvent Dblclick projRowEl
              return rowE) $
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
                    elDynAttr "img" (("class" =: "tree-icon" <>) . ("src" =:) . (\f -> "/pics/tree-" <> f <> ".svg") . bool "package" "debug" <$> isDebugD) $ return ()
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
                          elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-component.svg") $ return ()
                          text "Components"
                          return never) $
                      el "ul" $
                        fmap (fmapMaybe (listToMaybe . M.elems)) . listViewWithKey (M.fromList . zip [0::Int ..] . components <$> packageD) $ \_ componentD -> do
                          let isActiveComponentD = (&&) <$> isActivePackageD <*> ((==) <$> activeComponentD <*> (Just <$> componentD))
                          elDynClass "li" (("component" <>) <$> (bool "" " active" <$> isActiveComponentD)) $ do
                            let mkActD f = (\proj pkg comp ->
                                    PackageCommand . CommandWorkspaceAction "" "" $
                                      runProject (runPackage (f comp) pkg) proj)
                                  <$> projectD <*> packageD <*> componentD
                            (rowEl, rowE) <- treeSelect' "workspace" (menu
                              [ ("Activate",) . PackageCommand . CommandWorkspaceAction "Set as Active Component" "" <$>
                                  (workspaceActivatePackage <$> projectD <*> (Just <$> packageD) <*> (Just <$> componentD))
                              ]) $ do
                              elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-component.svg") $ return ()
                              dynText componentD
                              -- A ▶ button on runnable components (exe/test/bench,
                              -- of a cabal project) runs the component in a
                              -- terminal (cabal run/test/bench); libraries have
                              -- none.  Opening the repl is the row's double-click.
                              case pKey of
                                CabalTool {} -> switchHold never =<< dyn (ffor componentD $ \comp ->
                                  if T.takeWhile (/= ':') comp `elem` ["exe", "test", "bench"]
                                    then do
                                      runE <- execButton (runComponentTip comp)
                                      return $ tagPromptlyDyn (mkActD packageRunComponentTerm) runE
                                    else return never)
                                _ -> return never
                            -- Double-click a component opens its ffcabal repl as
                            -- a terminal tab (replacing the old inline repl button
                            -- that used to clutter every row).
                            let dblE = case pKey of
                                  CabalTool {} -> tagPromptlyDyn (mkActD packageOpenRepl) (domEvent Dblclick rowEl)
                                  _ -> never
                            return $ leftmost [rowE, dblE]
                    pkgDir <- sample (current pkgDirD)
                    gitBranchNode pkgDir
                    filesE <- treeItem' pkgRevealE "package-files" False (treeSelect "workspace" (return never) $ do
                      elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-folder.svg") $ return ()
                      text "Files"
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
                      elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-folder.svg") $ return ()
                      -- A nix project's tree has no package "Files" nodes to
                      -- distinguish from, so plain "Files" reads better.
                      text $ case pKey of
                        NixTool {}  -> "Files"
                        MakeTool {} -> "Files"
                        _           -> "Other Files"
                      return never) $
                        el "ul" $
                          (switchHold never =<<) . dyn $
                            (\sd ig -> fileTree "workspace" sd ig showHiddenD showIgnoredD activeFileD revealFileD (pjDir pKey))
                              <$> pjSourceDirsD <*> pkgDirsD)
              -- Nix/Makefile projects have no package rows to carry a git
              -- branch node, so it lives at the project level.
              case pKey of
                NixTool {}  -> gitBranchNode (pjDir pKey)
                MakeTool {} -> gitBranchNode (pjDir pKey)
                _           -> return ()
              -- Any project with a flake.nix gets a (collapsed) Flake node;
              -- it self-hides when there's no flake and only evaluates once
              -- expanded, so there's no overhead otherwise.
              flakeNode (pjDir pKey)
              return $ leftmost [ProjectPackageEvents <$> packagesE, ProjectFileEvents <$> projectFilesE]
