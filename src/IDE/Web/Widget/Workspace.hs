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
import Control.Monad (void, when, forM_)
import Control.Monad.IO.Class (liftIO)

import Data.Bool (bool)
import Data.List (stripPrefix, isPrefixOf, dropWhileEnd, find)
import qualified Data.Map as M (elems, fromList, keys)
import Data.Maybe (listToMaybe, maybeToList, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S (fromList, member)
import Data.Aeson (FromJSON(..), withObject, (.:), eitherDecodeStrict)
import Data.Text (Text)
import qualified Data.Text as T
       (pack, unpack, strip, null, takeWhile, lines, words, isPrefixOf, drop,
        length, breakOn, splitOn, stripSuffix, dropWhile)
import Data.Text.Encoding (encodeUtf8)

import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath
       ((<.>), (</>), dropFileName, dropTrailingPathSeparator, takeFileName,
        splitDirectories, joinPath)
import System.Info (os)
import System.Process (proc, createProcess, readProcessWithExitCode)

import IDE.Git (runGit)
import IDE.Utils.RemotePath (isRemotePath, parseRemotePath, renderRemotePath)
import IDE.Web.RemoteSettingsRequest (requestRemoteSettings)
import IDE.Web.GitLogRequest (requestGitLog)
import IDE.Web.ReplTmux (openTerminalInDir)
import IDE.Web.FS (fsDoesFileExist, fsDoesDirectoryExist)
import IDE.Web.RemoteRefresh (registerRemoteRefresh)
import IDE.Web.LocalRefresh (registerLocalRefresh)

import Clay
       (pct, hover, width, bold, fontWeight, paddingBottom,
        borderRadius, borderStyle, backgroundImage, vGradient,
        paddingRight, marginBottom, marginTop, marginRight, checked,
        userSelect, (|+), (-:), absolute, position, left, nil, paddingLeft, px,
        marginLeft, listStyleType, listStyleImage, middle, grey, color, rgb,
        opacity, nowrap, whiteSpace, inlineBlock, scroll, overflow, height, (?),
        Css, background, none, white, Color(..), VerticalAlign(..),
        cursorDefault, Cursor(..))
import qualified Clay (display, (#))
import Clay.Stylesheet (key)

import Reflex
       (leftmost, listViewWithKey, switchHold, constDyn, ffor,
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
    -- Keyboard-nav cursor: a blue OUTLINE, matching the Terminals tree (see
    -- 'IDE.Web.Widget.Terminals') — so it doesn't paint a SECOND blue fill on top
    -- of the active-file highlight above (both are the same blue).  The default
    -- '.leksah-nav-current' is a fill (see 'IDE.Web.Layout'); this overrides it
    -- for the workspace pane only.
    ".workspace .leksah-nav-item.leksah-nav-current" ? do
        "background" -: "transparent"
        "box-shadow" -: "inset 0 0 0 1px var(--leksah-selection)"
    -- The checkout's current branch stands out (bold) among the Branches list.
    ".workspace .git-branch-current" ? fontWeight bold
    -- The open PR whose head is the current branch's upstream, bolded too.
    ".workspace .git-pr-current" ? fontWeight bold

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

-- | The git subtree for the checkout rooted at @dir@ — shown as the top node
-- under a project (or a non-root package) whose directory is a git checkout,
-- hidden entirely otherwise.  Collapsed by default; its children only load when
-- expanded.  The row shows the current branch.
--
-- Children: \"Branches\" (click one to check it out), \"Submodules\" (only when
-- the checkout has any), the GitHub issue/PR nodes (only for a github.com
-- origin — see 'gitHubNodes'), and \"Worktrees\" (the repo's OTHER worktrees;
-- click to open one in a terminal).  All git reads go through 'runGit', so a
-- remote (@ssh://@) checkout works too; refreshes ride the
-- LocalRefresh/RemoteRefresh buses.
gitTreeNode :: forall t m . MonadWidget t m => FilePath -> m ()
gitTreeNode dir = do
  pb <- getPostBuild
  isGitE <- performEvent $ ffor pb $ \_ -> liftIO (isGitCheckout dir)
  isGitD <- holdUniqDyn =<< holdDyn False isGitE
  void . dyn $ ffor isGitD $ \isGit -> when isGit $ do
      (brE, fireBr) <- newTriggerEvent
      let scanBr = void . forkIO $ gitCurrentBranch dir >>= fireBr
      bpb <- getPostBuild
      performEvent_ $ liftIO scanBr <$ bpb
      liftIO $ registerGitRefresh dir scanBr
      brD <- holdDyn Nothing brE
      void $ treeItem "git" False
        (treeSelect "workspace" (return never) $ do
            gitIcon
            dynText $ ffor brD $ maybe " git" (" " <>)
            return (never :: Event t ()))
        (el "ul" $ do
            gitBranchesNode dir brD
            gitSubmodulesNode dir
            gitHubNodes dir
            gitWorktreesNode dir
            return (never :: Event t ()))

-- | A leading git tree-row icon.
gitIcon :: MonadWidget t m => m ()
gitIcon = elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-git.svg") (return ())

-- | \"Branches\": every local branch, the current one bolded; clicking a branch
-- opens a git log viewer for it (a center tab) rather than checking it out.
gitBranchesNode :: forall t m . MonadWidget t m => FilePath -> Dynamic t (Maybe Text) -> m ()
gitBranchesNode dir curD = void $ treeItem "git-branches" False
    (treeSelect "workspace" (return never) $ gitIcon >> text "Branches" >> return (never :: Event t ()))
    (el "ul" $ do
        (bsE, fireBs) <- newTriggerEvent
        cpb <- getPostBuild
        let scan = void . forkIO $ gitBranches dir >>= fireBs
        performEvent_ $ liftIO scan <$ cpb
        liftIO $ registerGitRefresh dir scan
        bsD <- holdDyn [] bsE
        void . dyn $ ffor ((,) <$> bsD <*> curD) $ \(bs, cur) ->
            forM_ bs $ \b -> el "li" $ do
                (rowEl, _) <- treeSelect' "workspace" (return never) $ do
                    gitIcon
                    elClass "span" (if Just b == cur then "git-branch-current" else "git-branch")
                        (text b)
                    return (never :: Event t ())
                performEvent_ $ ffor (domEvent Click rowEl) $ \_ ->
                    liftIO (requestGitLog dir b)
                return ()
        return (never :: Event t ()))

-- | \"Submodules\": the checkout's submodule paths.  The whole node self-hides
-- when there are none.
gitSubmodulesNode :: forall t m . MonadWidget t m => FilePath -> m ()
gitSubmodulesNode dir = do
    cpb <- getPostBuild
    subsE <- performEvent $ ffor cpb $ \_ -> liftIO (gitSubmodulePaths dir)
    subsD <- holdDyn [] subsE
    void . dyn $ ffor subsD $ \subs -> when (not (null subs)) . void $
        treeItem "git-submodules" False
          (treeSelect "workspace" (return never) $ gitIcon >> text "Submodules" >> return (never :: Event t ()))
          (el "ul" $ do
              forM_ subs $ \s -> el "li" . void . treeSelect "workspace" (return never) $
                  gitIcon >> text s >> return (never :: Event t ())
              return (never :: Event t ()))

-- | \"Worktrees\": the repo's OTHER worktrees (the current checkout is dropped);
-- clicking one opens a terminal at its directory.
gitWorktreesNode :: forall t m . MonadWidget t m => FilePath -> m ()
gitWorktreesNode dir = void $ treeItem "git-worktrees" False
    (treeSelect "workspace" (return never) $ gitIcon >> text "Worktrees" >> return (never :: Event t ()))
    (el "ul" $ do
        (wtE, fireWt) <- newTriggerEvent
        cpb <- getPostBuild
        let scan = void . forkIO $ gitWorktrees dir >>= fireWt
        performEvent_ $ liftIO scan <$ cpb
        liftIO $ registerGitRefresh dir scan
        wtD <- holdDyn [] wtE
        void . dyn $ ffor wtD $ \wts ->
            forM_ (filter (not . isCurrentWorktree dir) wts) $ \wt -> el "li" $ do
                (rowEl, _) <- treeSelect' "workspace" (return never) $ do
                    gitIcon
                    text (worktreeLabel wt)
                    return (never :: Event t ())
                performEvent_ $ ffor (domEvent Click rowEl) $ \_ ->
                    openTerminalInDir (fullWorktreePath dir wt)
                return ()
        return (never :: Event t ()))

-- | True when @dir@ is the root of a git checkout — it has its own @.git@ dir
-- (a normal checkout) or @.git@ file (a linked worktree).  A subdirectory of a
-- repo has no @.git@ and returns False, which is exactly the \"the dir has a
-- .git\" rule for whether to show a git tree.
isGitCheckout :: FilePath -> IO Bool
isGitCheckout dir = do
    d <- fsDoesDirectoryExist (dir </> ".git")
    if d then return True else fsDoesFileExist (dir </> ".git")

gitCurrentBranch :: FilePath -> IO (Maybe Text)
gitCurrentBranch dir = do
    r <- try (runGit dir ["rev-parse", "--abbrev-ref", "HEAD"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) | b <- T.strip out, not (T.null b) -> Just b
        _ -> Nothing

gitBranches :: FilePath -> IO [Text]
gitBranches dir = do
    r <- try (runGit dir ["for-each-ref", "--format=%(refname:short)", "refs/heads"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) -> filter (not . T.null) (map T.strip (T.lines out))
        _ -> []

-- @git config -f .gitmodules --get-regexp path@ prints @submodule.NAME.path PATH@
-- per submodule (and fails when there's no .gitmodules — hence []).
gitSubmodulePaths :: FilePath -> IO [Text]
gitSubmodulePaths dir = do
    r <- try (runGit dir ["config", "-f", ".gitmodules", "--get-regexp", "path"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) ->
            [ p | l <- T.lines out, Just p <- [listToMaybe (reverse (T.words l))] ]
        _ -> []

data GitWorktree = GitWorktree { gwPath :: Text, gwBranch :: Text }

gitWorktrees :: FilePath -> IO [GitWorktree]
gitWorktrees dir = do
    r <- try (runGit dir ["worktree", "list", "--porcelain"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) -> parseWorktrees out
        _ -> []

-- @--porcelain@ is blank-line-separated blocks; each has a @worktree <path>@
-- line and either @branch refs/heads/<b>@ or @detached@.
parseWorktrees :: Text -> [GitWorktree]
parseWorktrees out =
    [ GitWorktree path br
    | block <- splitBlocks (T.lines out)
    , Just path <- [field "worktree " block]
    , let br = fromMaybe (if "detached" `elem` block then "detached" else "")
                         (field "branch refs/heads/" block) ]
  where
    splitBlocks ls = case break T.null ls of
        (blk, [])     -> filter (not . null) [blk]
        (blk, _:rest) -> filter (not . null) [blk] ++ splitBlocks rest
    field pre ls = listToMaybe [ T.drop (T.length pre) l | l <- ls, pre `T.isPrefixOf` l ]

-- | The current checkout's own worktree entry (dropped from the list, since
-- \"Worktrees\" shows the OTHER worktrees).
isCurrentWorktree :: FilePath -> GitWorktree -> Bool
isCurrentWorktree dir wt =
    dropTrailingPathSeparator (T.unpack (gwPath wt)) == dropTrailingPathSeparator localDir
  where localDir = maybe dir snd (parseRemotePath dir)

-- | A worktree's full path in the caller's namespace — re-qualified with the
-- @ssh://host@ prefix when the checkout is remote (git prints host-local paths).
fullWorktreePath :: FilePath -> GitWorktree -> FilePath
fullWorktreePath dir wt = case parseRemotePath dir of
    Just (host, _) -> renderRemotePath host (T.unpack (gwPath wt))
    Nothing        -> T.unpack (gwPath wt)

worktreeLabel :: GitWorktree -> Text
worktreeLabel wt =
    T.pack (takeFileName (dropTrailingPathSeparator (T.unpack (gwPath wt))))
      <> (if T.null (gwBranch wt) then "" else "  [" <> gwBranch wt <> "]")

-- | Re-scan @act@ whenever this checkout changes: for a local checkout, on any
-- LocalRefresh under @dir@ (the fsnotify @.git@/@.git\/refs@ watcher fires those
-- on branch/HEAD/worktree changes — see "IDE.Workspaces.Writer"); for a remote
-- one, on any RemoteRefresh.  (Registration leaks if the node is rebuilt, as in
-- the sibling flake/allOutputs nodes — the accepted pattern here.)
registerGitRefresh :: FilePath -> IO () -> IO ()
registerGitRefresh dir act
    | isRemotePath dir = void $ registerRemoteRefresh (const act)
    | otherwise        = void $ registerLocalRefresh $ \p ->
          when ((dropTrailingPathSeparator dir <> "/") `isPrefixOf` p) act

-- | The GitHub issue/PR section of the git tree, shown only when the checkout's
-- @origin@ is a github.com URL: \"Open Issues (N)\", \"Open PRs (N)\", and
-- \"Closed\" (which expands to \"Closed Issues (N)\" and \"Closed PRs (N)\").
-- Each count node expands to the items (\"#number title\"), and clicking an item
-- opens it in the browser.  Counts + items come from one GitHub search per
-- category (see 'ghSearch'); the closed searches only run when \"Closed\" is
-- expanded.
gitHubNodes :: forall t m . MonadWidget t m => FilePath -> m ()
gitHubNodes dir = do
    pb <- getPostBuild
    ghE <- performEvent $ ffor pb $ \_ -> liftIO ((>>= parseGitHub) <$> gitOriginUrl dir)
    ghD <- holdDyn Nothing ghE
    -- The open PR whose head branch is the current branch's upstream — bolded
    -- in the Open PRs list.  Resolved once (upstream lookup + one pulls query).
    (prE, firePr) <- newTriggerEvent
    matchPrD <- holdDyn Nothing prE
    void . dyn $ ffor ghD $ \case
        Nothing   -> return ()
        Just repo -> do
            liftIO . void . forkIO $ do
                mhead <- gitUpstreamBranch dir
                mpr   <- maybe (return Nothing) (ghPullForHead repo) mhead
                firePr mpr
            ghSearchNode repo "Open Issues" "issue" "open" (constDyn Nothing)
            ghSearchNode repo "Open PRs"    "pr"    "open" matchPrD
            void $ treeItem "git-closed" False
                (treeSelect "workspace" (return never) $
                    gitIcon >> text "Closed" >> return (never :: Event t ()))
                (el "ul" $ do
                    ghSearchNode repo "Closed Issues" "issue" "closed" (constDyn Nothing)
                    ghSearchNode repo "Closed PRs"    "pr"    "closed" (constDyn Nothing)
                    return (never :: Event t ()))

-- | One GitHub search node: its label carries the total count, and it expands
-- to the (up to 30) matching items; clicking an item opens it in the browser.
ghSearchNode
    :: forall t m . MonadWidget t m
    => (Text, Text) -> Text -> Text -> Text -> Dynamic t (Maybe Int) -> m ()
ghSearchNode repo label typ state hlD = do
    (resE, fireRes) <- newTriggerEvent
    pb <- getPostBuild
    performEvent_ $ ffor pb $ \_ ->
        liftIO . void . forkIO $ ghSearch repo typ state >>= fireRes
    resD <- holdDyn Nothing resE
    void $ treeItem "git-gh" False
        (treeSelect "workspace" (return never) $ do
            gitIcon
            dynText $ ffor resD $ \r ->
                " " <> label <> maybe "" (\s -> " (" <> T.pack (show (ghTotal s)) <> ")") r
            return (never :: Event t ()))
        (el "ul" $ do
            void . dyn $ ffor resD $ \case
                Nothing -> return ()
                Just s  -> forM_ (ghItems s) $ \it -> el "li" $ do
                    (rowEl, _) <- treeSelect' "workspace" (return never) $ do
                        gitIcon
                        elDynClass "span"
                            (ffor hlD $ \mh -> if mh == Just (ghNumber it) then "git-pr-current" else "")
                            (text ("#" <> T.pack (show (ghNumber it)) <> " " <> ghTitle it))
                        return (never :: Event t ())
                    performEvent_ $ ffor (domEvent Click rowEl) $ \_ ->
                        liftIO (openUrl (ghUrl it))
                    return ()
            return (never :: Event t ()))

-- | The checkout's @remote.origin.url@ (if any).
gitOriginUrl :: FilePath -> IO (Maybe Text)
gitOriginUrl dir = do
    r <- try (runGit dir ["config", "--get", "remote.origin.url"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) | u <- T.strip out, not (T.null u) -> Just u
        _ -> Nothing

-- | Parse a github.com remote URL into @(owner, repo)@ — handling the
-- @git\@github.com:owner\/repo(.git)@, @https:\/\/github.com\/owner\/repo(.git)@
-- and @ssh:\/\/git\@github.com\/owner\/repo(.git)@ forms.
parseGitHub :: Text -> Maybe (Text, Text)
parseGitHub raw =
    case T.breakOn "github.com" (fromMaybe u (T.stripSuffix ".git" u)) of
        (_, rest)
          | not (T.null rest)
          , path <- T.dropWhile (`elem` (":/" :: String)) (T.drop (T.length "github.com") rest)
          , (owner : repo : _) <- T.splitOn "/" path
          , not (T.null owner), not (T.null repo) -> Just (owner, repo)
        _ -> Nothing
  where u = T.strip raw

data GhItem = GhItem { ghNumber :: Int, ghTitle :: Text, ghUrl :: Text }
instance FromJSON GhItem where
    parseJSON = withObject "GhItem" $ \o ->
        GhItem <$> o .: "number" <*> o .: "title" <*> o .: "html_url"

data GhSearch = GhSearch { ghTotal :: Int, ghItems :: [GhItem] }
instance FromJSON GhSearch where
    parseJSON = withObject "GhSearch" $ \o ->
        GhSearch <$> o .: "total_count" <*> o .: "items"

-- | Query the GitHub search API for @type:<typ> state:<state>@ in @owner/repo@;
-- returns the total count and up to 30 items.  Uses @$GITHUB_TOKEN@ when set
-- (higher rate limit / private repos), else the unauthenticated API (public
-- repos only).  Any failure (no curl, offline, rate-limited, private) → Nothing,
-- which just hides the count/list — matching the tree's "empty on failure"
-- convention.
ghSearch :: (Text, Text) -> Text -> Text -> IO (Maybe GhSearch)
ghSearch (owner, repo) typ state = do
    tok <- lookupEnv "GITHUB_TOKEN"
    let q   = "repo:" <> owner <> "/" <> repo <> "+type:" <> typ <> "+state:" <> state
        url = "https://api.github.com/search/issues?per_page=30&q=" <> q
        auth = maybe [] (\t -> ["-H", "Authorization: Bearer " <> t]) tok
        args = [ "-s", "-H", "Accept: application/vnd.github+json"
               , "-H", "User-Agent: leksah" ] <> auth <> [T.unpack url]
    r <- try (readProcessWithExitCode "curl" args "")
    return $ case r :: Either SomeException (ExitCode, String, String) of
        Right (ExitSuccess, out, _) ->
            either (const Nothing) Just (eitherDecodeStrict (encodeUtf8 (T.pack out)))
        _ -> Nothing

-- | The upstream (tracking) branch's head ref for the current branch — e.g.
-- @origin/foo@ → @foo@ (the remote name is stripped, keeping any @/@ in the
-- branch).  'Nothing' when there is no upstream configured.
gitUpstreamBranch :: FilePath -> IO (Maybe Text)
gitUpstreamBranch dir = do
    r <- try (runGit dir ["rev-parse", "--abbrev-ref", "--symbolic-full-name", "@{upstream}"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _)
          | u <- T.strip out, not (T.null u)
          , let (_remote, rest) = T.breakOn "/" u
          , h <- T.drop 1 rest, not (T.null h) -> Just h
        _ -> Nothing

-- | The number of the open PR whose head branch is @headRef@ (in the repo's own
-- @owner@ namespace — same-repo PRs; a fork's PR won't match).  The
-- @search/issues@ API 'ghSearch' uses doesn't return a PR's head ref, so this
-- hits the @pulls@ endpoint filtered by @head=owner:branch@.
ghPullForHead :: (Text, Text) -> Text -> IO (Maybe Int)
ghPullForHead (owner, repo) headRef = do
    tok <- lookupEnv "GITHUB_TOKEN"
    let url  = "https://api.github.com/repos/" <> owner <> "/" <> repo
                 <> "/pulls?state=open&head=" <> owner <> ":" <> headRef
        auth = maybe [] (\t -> ["-H", "Authorization: Bearer " <> t]) tok
        args = [ "-s", "-H", "Accept: application/vnd.github+json"
               , "-H", "User-Agent: leksah" ] <> auth <> [T.unpack url]
    r <- try (readProcessWithExitCode "curl" args "")
    return $ case r :: Either SomeException (ExitCode, String, String) of
        Right (ExitSuccess, out, _) ->
            case eitherDecodeStrict (encodeUtf8 (T.pack out)) of
                Right (p : _) -> Just (ghpNumber p)
                _             -> Nothing
        _ -> Nothing

newtype GhPull = GhPull { ghpNumber :: Int }
instance FromJSON GhPull where
    parseJSON = withObject "GhPull" $ \o -> GhPull <$> o .: "number"

-- | Open a URL in the system browser (macOS @open@ / else @xdg-open@).
openUrl :: Text -> IO ()
openUrl url = do
    _ <- (try (void $ createProcess (proc opener [T.unpack url]))
            :: IO (Either SomeException ()))
    return ()
  where opener = if os == "darwin" then "open" else "xdg-open"

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
              -- Top item: the project's git tree (self-hides unless the project
              -- dir is itself a git checkout).
              gitTreeNode (pjDir pKey)
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
                    -- Top item: the package's git tree — only when the package
                    -- is NOT the project root and is itself a git checkout.
                    pkgDir <- sample (current pkgDirD)
                    when (dropTrailingPathSeparator pkgDir /= dropTrailingPathSeparator (pjDir pKey)) $
                        gitTreeNode pkgDir
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
              -- Any project with a flake.nix gets a (collapsed) Flake node;
              -- it self-hides when there's no flake and only evaluates once
              -- expanded, so there's no overhead otherwise.
              flakeNode (pjDir pKey)
              return $ leftmost [ProjectPackageEvents <$> packagesE, ProjectFileEvents <$> projectFilesE]
