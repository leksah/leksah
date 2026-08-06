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
import Control.Monad (void, when, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle (liftJSM, eval, valToNumber)
import qualified System.IO as IO (hPutStrLn, stderr)

import Data.Bool (bool)
import Data.List (stripPrefix, isPrefixOf, dropWhileEnd, find, nub, sortBy, sortOn)
import Data.Ord (comparing)
import qualified Data.Map as M
       (elems, fromList, fromListWith, keys, toList, null)
import Data.Maybe (listToMaybe, maybeToList, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S (fromList, member)
import Data.Aeson (FromJSON(..), withObject, (.:), eitherDecodeStrict)
import Data.Text (Text)
import qualified Data.Text as T
       (pack, unpack, strip, null, takeWhile, lines, words, isPrefixOf, drop,
        length, breakOn, splitOn, stripSuffix, stripPrefix, dropWhile,
        intercalate, replace, isSuffixOf, dropEnd, dropAround)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Text.Read (readMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath
       ((<.>), (</>), dropFileName, dropTrailingPathSeparator, takeFileName,
        splitDirectories, joinPath, makeRelative)
import System.Info (os)
import System.Process (proc, createProcess, readProcessWithExitCode)

import IDE.Git (runGit)
import IDE.Utils.RemotePath (isRemotePath, parseRemotePath, renderRemotePath)
import IDE.Web.RemoteSettingsRequest (requestRemoteSettings)
import IDE.Web.GitLogRequest (requestGitLog)
import IDE.Web.Worktree (requestReview)
import IDE.Web.ReplTmux (openTerminalInDir, runInTerminal)
import IDE.Web.FS (fsReadFile, fsDoesFileExist, fsDoesDirectoryExist)
import IDE.Web.RemoteRefresh (registerRemoteRefresh, requestRemoteRefresh, RefreshReason(..))
import IDE.Web.LocalRefresh (registerLocalRefresh, requestLocalRefresh)

import Clay
       (pct, hover, width, bold, fontWeight, paddingBottom,
        borderRadius, borderStyle, backgroundImage, vGradient,
        paddingRight, marginBottom, marginTop, marginRight, checked,
        userSelect, (|+), (-:), absolute, position, left, nil, paddingLeft, px,
        marginLeft, listStyleType, listStyleImage, middle, color, rgb,
        opacity, nowrap, whiteSpace, inlineBlock, scroll, overflow, height, (?),
        Css, background, none, VerticalAlign(..),
        cursorDefault, Cursor(..))
import qualified Clay (display, (#))
import Clay.Stylesheet (key)

import Reflex
       (leftmost, listViewWithKey, switchHold, constDyn, ffor,
        current, getPostBuild, holdUniqDyn, holdDyn, performEvent,
        performEvent_, newTriggerEvent, Dynamic, updated, delay, tag,
        Event, never, fmapMaybe, tagPromptlyDyn, sample)
import Reflex.Dom.Core
       (elDynClass, MonadWidget, elAttr, dyn, button, (=:), elDynAttr,
        divClass, text, el, elClass, dynText, domEvent, EventName(..))

import IDE.Web.Theme
       (selectionColor, hoverColor, dimColor, dimOpacity, fgColor,
        btnTopColor, btnBottomColor, btnHoverTopColor, btnHoverBottomColor)
import IDE.Core.Location (packageIdentifierToString)
import IDE.Core.State
       (activeComponent, ipdPackageDir,
        ipdLib, pjDir, IDEPackage(..), runPackage, runProject,
        pjPackages, Project(..), workspace, wsProjects, IDE,
        activeProject, activePack, pjFile, pjFileOrDir,
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
import IDE.Web.Widget.Menu (menu, menuSplit, menuSplitWith)
import IDE.Web.Widget.FileTree
       (fileTree, claudeNode, gitClass, gitBadge)
import IDE.Web.Widget.Changes (gitChanges, FileChange(..))
import IDE.Web.OpenFileRequest (deliverOpenedFile)
import IDE.Web.Claude (claudeAvailable, runClaudeCmd, ClaudeCmd(..))
import IDE.Web.Widget.Tree
       (treeItemDynAttr', treeItemDynAttrSet', treeSelect, treeSelect', treeItem,
        treeItem', clickMods, dblclickMods)
import IDE.Web.GitInfo (prForBranch)
import IDE.Web.SplitOpenRequest (SplitTarget(..), requestSplitOpen)
import IDE.Workspaces
       (workspaceRemoveProject, workspaceActivatePackage)

workspaceCss :: Css
workspaceCss = do
    ".workspace" ? do
        height (pct 100)
        key "fill" dimColor
        overflow scroll
        -- A uniform right inset so the run buttons line up clear of the
        -- scrollbar.  On the PANE, not the rows — row padding would compound
        -- per nesting level and step deeper buttons leftward.
        key "padding-right" ("8px" :: Text)
        key "box-sizing" ("border-box" :: Text)
    ".workspace li.active > .tree-expand" ?
        key "fill" fgColor
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
        color fgColor
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
        color fgColor
        borderStyle none
        borderRadius (px 3) (px 3) (px 3) (px 3)
        backgroundImage (vGradient btnTopColor btnBottomColor)
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
        backgroundImage (vGradient btnHoverTopColor btnHoverBottomColor)
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
    -- Branch + PR summary on a project row: hidden while expanded (the git
    -- subtree shows it), inline while the project is collapsed.
    ".workspace .proj-git-summary" ? do
        "display" -: "none"
        color dimColor
        "font-size" -: "11px"
    ".workspace .project.tree-collapsed .proj-git-summary" ? ("display" -: "inline")
    ".workspace .proj-git-summary img" ? do
        "height" -: "12px"
        "width" -: "12px"
        "vertical-align" -: "middle"
        "margin" -: "0 3px 0 6px"
        "opacity" -: "0.7"

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
gitTreeNode :: forall t m . MonadWidget t m
            => FilePath
            -> Dynamic t (Maybe Text)         -- ^ current branch (scanned by the project node)
            -> Dynamic t (Maybe (Int, Text))  -- ^ open PR (number, url)
            -> m ()
gitTreeNode dir brD prD = do
  pb <- getPostBuild
  isGitE <- performEvent $ ffor pb $ \_ -> liftIO (isGitCheckout dir)
  isGitD <- holdUniqDyn =<< holdDyn False isGitE
  void . dyn $ ffor isGitD $ \isGit -> when isGit $ do
      (abE, fireAb) <- newTriggerEvent
      let scanAb = void . forkIO $ gitAheadBehind dir >>= fireAb
      bpb <- getPostBuild
      performEvent_ $ liftIO scanAb <$ bpb
      liftIO $ registerGitRefresh dir scanAb
      abD <- holdDyn Nothing abE
      void $ treeItem "git" False
        (do (_, mE) <- treeSelect' "workspace" (gitRootMenu dir brD prD) $ do
                gitIcon
                -- Branch name, then the open PR number (e.g. "main #1234").
                dynText $ (\mb mpr -> maybe "git" id mb <> prSuffix mpr) <$> brD <*> prD
                -- Ahead/behind of the current branch vs its upstream.
                void . dyn $ ffor abD $ \case
                    Just (a, b) -> abSpan a b
                    Nothing     -> return ()
                return (never :: Event t (IO ()))
            performEvent_ $ liftIO <$> mE
            return (never :: Event t ()))
        (el "ul" $ do
            gitChangesNode dir
            gitBranchesNode dir brD
            gitSubmodulesNode dir
            gitHubNodes dir
            gitWorktreesNode dir
            return (never :: Event t ()))

-- | Suffix showing the open PR number next to a branch, e.g. @" #1234"@.
prSuffix :: Maybe (Int, Text) -> Text
prSuffix = maybe "" (\(n, _) -> " #" <> T.pack (show n))

-- | Scan a checkout's current branch and open PR, refreshing on git changes.
-- Computed once at the project node so the git subtree, the collapsed-project
-- summary and the "Open PR" menu items all share a single scan (PR lookups are
-- additionally cached in 'IDE.Web.GitInfo').  A non-git dir just yields
-- @(Nothing, Nothing)@.
gitBranchPr :: forall t m. MonadWidget t m
            => FilePath -> m (Dynamic t (Maybe Text), Dynamic t (Maybe (Int, Text)))
gitBranchPr dir = do
  (brE, fireBr) <- newTriggerEvent
  (prE, firePr) <- newTriggerEvent
  let scan = void . forkIO $ do
        gitCurrentBranch dir >>= fireBr
        prForBranch dir >>= firePr
  pb <- getPostBuild
  performEvent_ $ liftIO scan <$ pb
  liftIO $ registerGitRefresh dir scan
  (,) <$> holdDyn Nothing brE <*> holdDyn Nothing prE

-- | A leading git tree-row icon.
gitIcon :: MonadWidget t m => m ()
gitIcon = elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-git.svg") (return ())

-- | A muted @↑ahead ↓behind@ indicator vs the upstream; renders nothing when the
-- branch is level with (or has no) upstream.
abSpan :: MonadWidget t m => Int -> Int -> m ()
abSpan ahead behind = when (ahead > 0 || behind > 0) $
    elAttr "span"
        (  "class" =: "git-ab"
        <> "title" =: "commits ahead / behind upstream"
        <> "style" =: "color:var(--leksah-fg-dim);margin-left:6px;font-size:11px" )
        (text label)
  where
    label = T.intercalate " " $
        [ "\x2191" <> tshow ahead  | ahead  > 0 ] ++
        [ "\x2193" <> tshow behind | behind > 0 ]
    tshow = T.pack . show

-- | Run @git \<args\>@ in a reusable "git" terminal for this checkout, so network
-- auth, progress and any errors/conflicts are visible.  The window closes itself
-- on success (exit 0) and only lingers as a shell when git fails — so a clean run
-- tidies up while an error stays on screen.  The working tree change (e.g. a
-- checkout) is picked up by the @.git@ watcher, which refreshes the tree.
gitAction :: FilePath -> [Text] -> IO ()
gitAction dir args =
    runInTerminal False dir "git" "git" (gitCmdLine args)

-- | The shell line 'gitAction' runs (each argument single-quoted).
gitCmdLine :: [Text] -> Text
gitCmdLine args = T.intercalate " " ("git" : map shq args)
  where shq a = "'" <> T.replace "'" "'\\''" a <> "'"

-- | The ⌥-split target matching @'gitAction' dir args@ — the same command run
-- in a split of the active pane instead of the reusable git window.  'Nothing'
-- for remote checkouts (a split runs on the local tmux server).
gitTarget :: FilePath -> [Text] -> Maybe SplitTarget
gitTarget dir args
  | isRemotePath dir = Nothing
  | otherwise        = Just (STRunCmd False dir "git" "git" (gitCmdLine args))

-- | Ask the git subtree to rescan now (branches, ahead/behind, worktrees, …).
refreshGit :: FilePath -> IO ()
refreshGit dir
  | isRemotePath dir = requestRemoteRefresh RefreshManual
  | otherwise        = requestLocalRefresh (dir </> ".git" </> "HEAD")

-- | Right-click menu for the root git row: whole-repo network actions, the
-- current branch's log, and a manual refresh.
gitRootMenu :: forall t m. MonadWidget t m
            => FilePath -> Dynamic t (Maybe Text) -> Dynamic t (Maybe (Int, Text))
            -> m (Event t (IO ()))
gitRootMenu dir curD prD = menuSplit $
  [ constDyn ("Fetch",   (gitTarget dir ["fetch", "--all", "--prune"], gitAction dir ["fetch", "--all", "--prune"]))
  , constDyn ("Pull",    (gitTarget dir ["pull"], gitAction dir ["pull"]))
  , constDyn ("Push",    (gitTarget dir ["push"], gitAction dir ["push"]))
  , ffor curD $ \mb ->
      ("Open Log", ( if isRemotePath dir then Nothing else STGitLog dir <$> mb
                   , maybe (return ()) (requestGitLog dir) mb ))
  , ffor prD $ \mpr ->
      ("Open PR" <> prSuffix mpr, (Nothing, maybe (return ()) (openUrl . snd) mpr))
  ] <>
  -- The Review pane diffs against the recorded/derived base branch and reads
  -- working files directly — local checkouts only.
  [ constDyn ("Review Changes…", (Nothing, requestReview dir)) | not (isRemotePath dir) ] <>
  [ constDyn ("Refresh", (Nothing, refreshGit dir))
  ]

-- | Right-click menu for a branch row: its log always; pull/push for the current
-- branch, or checkout/delete for any other.
gitBranchMenu :: forall t m. MonadWidget t m => FilePath -> Bool -> Text -> m (Event t (IO ()))
gitBranchMenu dir isCurrent name = menuSplit $
  constDyn ("Open Log", ( if isRemotePath dir then Nothing else Just (STGitLog dir name)
                        , requestGitLog dir name ))
  : if isCurrent
      then [ constDyn ("Pull", (gitTarget dir ["pull"], gitAction dir ["pull"]))
           , constDyn ("Push", (gitTarget dir ["push"], gitAction dir ["push"])) ]
      else [ constDyn ("Checkout",      (gitTarget dir ["checkout", name], gitAction dir ["checkout", name]))
           , constDyn ("Delete Branch", (gitTarget dir ["branch", "-d", name], gitAction dir ["branch", "-d", name])) ]

-- | Right-click menu for a submodule row.
gitSubmoduleMenu :: forall t m. MonadWidget t m => FilePath -> Text -> m (Event t (IO ()))
gitSubmoduleMenu dir path = menuSplit
  [ let p = dir </> T.unpack path
    in constDyn ("Open Terminal Here",
                 ( if isRemotePath p then Nothing else Just (STTermDir p)
                 , openTerminalInDir p ))
  , constDyn ("Update Submodule",   (gitTarget dir ["submodule", "update", "--init", "--", path], gitAction dir ["submodule", "update", "--init", "--", path]))
  ]

-- | Right-click menu for a worktree row.
gitWorktreeMenu :: forall t m. MonadWidget t m => FilePath -> GitWorktree -> m (Event t (IO ()))
gitWorktreeMenu dir wt = menuSplit $
  [ let p = fullWorktreePath dir wt
    in constDyn ("Open Terminal Here",
                 ( if isRemotePath p then Nothing else Just (STTermDir p)
                 , openTerminalInDir p ))
  ] <>
  [ constDyn ("Review Worktree…", (Nothing, requestReview (fullWorktreePath dir wt)))
  | not (isRemotePath (fullWorktreePath dir wt)) ] <>
  [ constDyn ("Remove Worktree",    (gitTarget dir ["worktree", "remove", gwPath wt], gitAction dir ["worktree", "remove", gwPath wt]))
  ]

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
            -- Branches are grouped into a tree by their "/" segments (all
            -- `hkm/…` under an `hkm/` node); at each level the group (container)
            -- nodes are listed before the leaf branches, as in the file tree.
            let renderForest pfx = mapM_ (renderNode pfx)
                -- A group starts expanded only when it contains the current
                -- branch (its full "…/" path is a prefix of the current branch
                -- name); otherwise it starts collapsed, like a directory.
                renderNode pfx (BranchGroup seg kids) =
                    let full = pfx <> seg <> "/"
                    in void $ treeItem "git-branch-group"
                        (maybe False (full `T.isPrefixOf`) cur)
                        (treeSelect "workspace" (return never) $
                            gitIcon >> text (seg <> "/") >> return (never :: Event t ()))
                        (el "ul" $ renderForest full kids >> return (never :: Event t ()))
                renderNode _ (BranchLeaf b label) = el "li" $ do
                    let name  = gbName b
                        isCur = Just name == cur
                    -- Show only the last "/" segment (the prefix is on the group
                    -- node), but click/menu still act on the full branch name.
                    (rowEl, mE) <- treeSelect' "workspace" (gitBranchMenu dir isCur name) $ do
                        gitIcon
                        elClass "span" (if isCur then "git-branch-current" else "git-branch")
                            (text label)
                        abSpan (gbAhead b) (gbBehind b)
                        return (never :: Event t (IO ()))
                    -- ⌥-click (or ⌥-Enter) opens the git log into a split of the
                    -- active pane instead of a new tab (⌥⇧ = the other direction).
                    cmE <- clickMods rowEl
                    performEvent_ $ ffor cmE $ \(alt, sh) -> liftIO $
                        if alt then requestSplitOpen (STGitLog dir name, sh)
                               else requestGitLog dir name
                    performEvent_ $ liftIO <$> mE
                    return ()
            in do
                -- Recent branches form the main tree; branches whose last commit
                -- is over a year old are tucked into an "Old" node at the bottom
                -- (collapsed unless it holds the current branch).
                renderForest "" (buildBranchForest (filter (not . gbOld) bs))
                let old = filter gbOld bs
                when (not (null old)) . void $
                    treeItem "git-branch-group"
                        (any (\b -> Just (gbName b) == cur) old)
                        (treeSelect "workspace" (return never) $
                            gitIcon >> text ("Old (" <> T.pack (show (length old)) <> ")")
                                    >> return (never :: Event t ()))
                        (el "ul" $ renderForest "" (buildBranchForest old)
                                   >> return (never :: Event t ()))
        return (never :: Event t ()))

-- | A branch tree node: a @\/@-delimited prefix group (its segment + children),
-- or a leaf branch (the full 'GitBranch' plus its last-segment display label).
data BranchNode = BranchGroup Text [BranchNode] | BranchLeaf GitBranch Text

-- | Group a flat branch list into a tree by splitting names on @\/@, so all
-- @hkm\/…@ branches nest under an @hkm\/@ group.  Each level lists group
-- (container) nodes before leaf branches, each alphabetically — the same
-- \"directories first\" ordering the file tree uses.
buildBranchForest :: [GitBranch] -> [BranchNode]
buildBranchForest bs = groupByPath [ (T.splitOn "/" (gbName b), b) | b <- bs ]

groupByPath :: [([Text], GitBranch)] -> [BranchNode]
groupByPath items =
    sortBy (comparing nodeKey) (concatMap toNodes (M.toList byFirst))
  where
    byFirst = M.fromListWith (flip (++))
        [ (seg, [(rest, gb)]) | (seg : rest, gb) <- items ]
    toNodes (seg, subs) =
        [ BranchGroup seg (groupByPath deeper) | not (null deeper) ]
        ++ [ BranchLeaf gb seg | ([], gb) <- subs ]
      where deeper = [ (r, gb) | (r@(_:_), gb) <- subs ]
    nodeKey (BranchGroup s _) = (0 :: Int, s)   -- container nodes first…
    nodeKey (BranchLeaf _ s)  = (1 :: Int, s)   -- …then leaf branches

-- | \"Changes\": the staged + unstaged changes for this checkout (reusing the
-- Changes pane's 'gitChanges' scan).  Each row shows a git status badge and the
-- path relative to the checkout; clicking opens the file in the editor (⌥-click
-- opens it into a split).  Refreshes on the git/local-refresh bus, so the count
-- tracks edits and git operations.
gitChangesNode :: forall t m . MonadWidget t m => FilePath -> m ()
gitChangesNode dir = do
    (chE, fireCh) <- newTriggerEvent
    cpb <- getPostBuild
    let scan = void . forkIO $ (M.elems <$> gitChanges dir) >>= fireCh
    performEvent_ $ liftIO scan <$ cpb
    liftIO $ registerGitRefresh dir scan
    chD <- holdDyn [] chE
    void $ treeItem "git-changes" False
        (treeSelect "workspace" (return never) $ do
            gitIcon
            dynText $ ffor chD $ \cs ->
                "Changes" <> if null cs then "" else " (" <> T.pack (show (length cs)) <> ")"
            return (never :: Event t ()))
        (el "ul" $ do
            void . dyn $ ffor chD $ \changes ->
                forM_ (sortOn changePath changes) $ \c -> el "li" $ do
                    let path = changePath c
                        st   = changeStatus c
                    (rowEl, _) <- treeSelect' "workspace" (return never) $ do
                        elClass "span" ("git-change-badge " <> gitClass st) (text (gitBadge st))
                        text " "
                        elClass "span" (gitClass st) (text (T.pack (makeRelative dir path)))
                        -- +added / -deleted line counts (as in the Changes pane);
                        -- a zero count is omitted (no -0 on a new file, +0 on a
                        -- deleted one).
                        let fmt sign = maybe "" (\n -> if n == 0 then "" else sign <> T.pack (show n))
                        elClass "span" "git-added"   (text (fmt " +" (changeAdded c)))
                        elClass "span" "git-deleted" (text (fmt " \x2212" (changeDeleted c)))
                        return (never :: Event t (IO ()))
                    cmE <- clickMods rowEl
                    performEvent_ $ ffor cmE $ \(alt, sh) -> liftIO $
                        if alt then requestSplitOpen (STFile path, sh)
                               else deliverOpenedFile path
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
              forM_ subs $ \s -> el "li" $ do
                  (_, mE) <- treeSelect' "workspace" (gitSubmoduleMenu dir s) $
                      gitIcon >> text s >> return (never :: Event t (IO ()))
                  performEvent_ $ liftIO <$> mE
                  return ()
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
                (rowEl, mE) <- treeSelect' "workspace" (gitWorktreeMenu dir wt) $ do
                    gitIcon
                    text (worktreeLabel wt)
                    return (never :: Event t (IO ()))
                -- ⌥-click opens the worktree terminal into a split of the
                -- active pane instead (local only).
                wmE <- clickMods rowEl
                performEvent_ $ ffor wmE $ \(alt, sh) ->
                    if alt && not (isRemotePath (fullWorktreePath dir wt))
                      then liftIO (requestSplitOpen (STTermDir (fullWorktreePath dir wt), sh))
                      else openTerminalInDir (fullWorktreePath dir wt)
                performEvent_ $ liftIO <$> mE
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

-- | A local branch and how far it is ahead/behind its upstream (0/0 when level,
-- or when it has no upstream — the indicator then just hides).
data GitBranch = GitBranch
  { gbName   :: Text
  , gbAhead  :: Int
  , gbBehind :: Int
  , gbOld    :: Bool   -- ^ last commit more than a year ago
  } deriving (Eq, Show)

-- @%(upstream:track,nobracket)@ prints e.g. @ahead 1, behind 2@ / @gone@ / empty
-- (no upstream or level), so one @for-each-ref@ gives every branch's divergence.
gitBranches :: FilePath -> IO [GitBranch]
gitBranches dir = do
    now <- getPOSIXTime
    let yearAgo   = now - 365 * 24 * 60 * 60   -- ~1 year, in POSIX seconds
        isOld ts  = ts < yearAgo
        atOr d i xs = if length xs > i then xs !! i else d
        parseUnix t = fromInteger <$> (readMaybe (T.unpack (T.strip t)) :: Maybe Integer) :: Maybe POSIXTime
    r <- try (runGit dir
        [ "for-each-ref"
        , "--format=%(refname:short)%09%(upstream:track,nobracket)%09%(committerdate:unix)"
        , "refs/heads" ])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _) ->
            [ GitBranch name a b old
            | l <- T.lines out, not (T.null (T.strip l))
            , let fs      = T.splitOn "\t" l
                  name    = T.strip (atOr "" 0 fs)
                  (a, b)  = parseTrack (atOr "" 1 fs)
                  old     = maybe False isOld (parseUnix (atOr "" 2 fs))
            , not (T.null name) ]
        _ -> []

-- | Parse a @%(upstream:track,nobracket)@ value (@"ahead 1, behind 2"@, @"gone"@,
-- @""@) into @(ahead, behind)@.
parseTrack :: Text -> (Int, Int)
parseTrack track = foldr acc (0, 0) (T.splitOn ", " track)
  where
    acc piece (a, b)
      | Just n <- num "ahead "  piece = (n, b)
      | Just n <- num "behind " piece = (a, n)
      | otherwise                     = (a, b)
    num kw p = T.stripPrefix kw (T.strip p) >>= (readMaybe . T.unpack)

-- | The current branch's @(ahead, behind)@ vs its upstream, for the root git
-- row.  @rev-list --left-right --count \@{u}...HEAD@ prints @behind<TAB>ahead@
-- (left = upstream-only, right = HEAD-only); 'Nothing' with no upstream.
gitAheadBehind :: FilePath -> IO (Maybe (Int, Int))
gitAheadBehind dir = do
    r <- try (runGit dir ["rev-list", "--left-right", "--count", "@{upstream}...HEAD"])
    return $ case r :: Either SomeException (ExitCode, Text, Text) of
        Right (ExitSuccess, out, _)
          | [bh, ah] <- T.words (T.strip out)
          , Just behind <- readMaybe (T.unpack bh)
          , Just ahead  <- readMaybe (T.unpack ah) -> Just (ahead, behind)
        _ -> Nothing

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
                label <> maybe "" (\s -> " (" <> T.pack (show (ghTotal s)) <> ")") r
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

-- | Introspection node for a Rust or Python project directory: collapsed and
-- self-hiding unless a @Cargo.toml@ / @pyproject.toml@ / @setup.py@ exists.  On
-- expand it parses the manifest (on a background thread) and lists the package
-- name, its dependencies, and — for Cargo — its binary targets, as child rows.
-- The header row double-clicks to open the manifest in the editor.
manifestNode :: forall t m . MonadWidget t m => FilePath -> m ()
manifestNode dir = do
  pb <- getPostBuild
  kindE <- performEvent $ ffor pb $ \_ -> liftIO $ do
    cargo <- fsDoesFileExist (dir </> "Cargo.toml")
    py    <- fsDoesFileExist (dir </> "pyproject.toml")
    setup <- fsDoesFileExist (dir </> "setup.py")
    return $ if cargo then Just ("Cargo.toml" :: FilePath, "Crate" :: Text)
             else if py then Just ("pyproject.toml", "Python package")
             else if setup then Just ("setup.py", "Python package")
             else Nothing
  kindD <- holdUniqDyn =<< holdDyn Nothing kindE
  void . dyn $ ffor kindD $ \case
    Nothing -> pure ()
    Just (file, label) -> void $ treeItem "manifest" False
      (do (rowEl, _) <- treeSelect' "workspace" (return never) $ do
              elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-package.svg") (return ())
              text label
              return (never :: Event t ())
          performEvent_ $ ffor (domEvent Dblclick rowEl) $ \_ -> liftIO $
              deliverOpenedFile (dir </> file)
          return (never :: Event t ()))
      (do (infoE, fireInfo) <- newTriggerEvent
          cpb <- getPostBuild
          performEvent_ $ ffor cpb $ \_ ->
              liftIO . void . forkIO $ parseManifest (dir </> file) >>= fireInfo
          infoD <- holdDyn Nothing (Just <$> infoE)
          _ <- el "ul" . dyn $ ffor infoD $ \case
              Nothing -> divClass "flake-hint" (text "reading…")
              Just (mName, secs)
                | mName == Nothing && null secs -> divClass "flake-hint" (text "(no metadata)")
                | otherwise -> do
                    forM_ mName $ \nm -> elClass "li" "flake-leaf" . void $
                        treeSelect' "workspace" (return never) $ do
                            elClass "span" "flake-label" (text (" name: " <> nm))
                            return (never :: Event t ())
                    mapM_ manifestSection secs
          return (never :: Event t ()))
  where
    manifestSection :: (Text, [Text]) -> m ()
    manifestSection (title, items) = void $ treeItem "flake-node" False
      (do _ <- treeSelect' "workspace" (return never) $ do
              elClass "span" "flake-label"
                  (text (" " <> title <> " (" <> T.pack (show (length items)) <> ")"))
              return (never :: Event t ())
          return (never :: Event t ()))
      (do _ <- el "ul" $ forM_ items $ \it -> elClass "li" "flake-leaf" . void $
              treeSelect' "workspace" (return never) $ do
                  text (" " <> it)
                  return (never :: Event t ())
          return (never :: Event t ()))

-- | Parse a project manifest into (package name, [(section title, entries)]).
-- Best-effort: an unreadable file yields @(Nothing, [])@.
parseManifest :: FilePath -> IO (Maybe Text, [(Text, [Text])])
parseManifest path = do
  r <- try (decodeUtf8With lenientDecode <$> fsReadFile path)
  return $ case r :: Either SomeException Text of
    Left _    -> (Nothing, [])
    Right src
      | takeFileName path == "Cargo.toml" -> parseCargo src
      | otherwise                         -> parsePyproject src

-- | Cargo.toml: @[package].name@, @[[bin]].name@ targets, @[…dependencies]@ keys.
parseCargo :: Text -> (Maybe Text, [(Text, [Text])])
parseCargo src =
  let secs = tomlSections src
      name = listToMaybe [ v | ("package", body) <- secs, Just v <- map (kvString "name") body ]
      bins = nub [ v | ("bin", body) <- secs, Just v <- map (kvString "name") body ]
      deps = nub $ concat [ tableKeys body | (h, body) <- secs, "dependencies" `T.isSuffixOf` h ]
  in (name, [ ("Binaries", bins) | not (null bins) ]
         ++ [ ("Dependencies", deps) | not (null deps) ])

-- | pyproject.toml: name from @[project]@ or @[tool.poetry]@; deps from a
-- @[…dependencies]@ table (poetry) or a @dependencies = [ … ]@ array (PEP 621).
parsePyproject :: Text -> (Maybe Text, [(Text, [Text])])
parsePyproject src =
  let secs = tomlSections src
      name = listToMaybe $
                 [ v | ("project", body)     <- secs, Just v <- map (kvString "name") body ]
              ++ [ v | ("tool.poetry", body) <- secs, Just v <- map (kvString "name") body ]
      tableDeps = concat [ tableKeys body | (h, body) <- secs, "dependencies" `T.isSuffixOf` h ]
      arrDeps   = concat [ arrayItems body | ("project", body) <- secs ]
      deps = nub (tableDeps ++ arrDeps)
  in (name, [ ("Dependencies", deps) | not (null deps) ])

-- | A crude TOML reader: groups lines into (header, body) sections, header being
-- the bracketed name with brackets stripped (@[[bin]]@ and @[bin]@ both → @bin@;
-- @[tool.poetry.dependencies]@ → @tool.poetry.dependencies@).  Not full TOML —
-- enough to pull a package name and a few name/dependency lists.
tomlSections :: Text -> [(Text, [Text])]
tomlSections = go "" [] . filter (not . T.null) . map (T.strip . fst . T.breakOn "#") . T.lines
  where
    go h acc [] = [(h, reverse acc)]
    go h acc (l:ls) = case header l of
        Just h' -> (h, reverse acc) : go h' [] ls
        Nothing -> go h (l:acc) ls
    header s
      | "[[" `T.isPrefixOf` s && "]]" `T.isSuffixOf` s = Just (T.strip (T.dropEnd 2 (T.drop 2 s)))
      | "["  `T.isPrefixOf` s && "]"  `T.isSuffixOf` s = Just (T.strip (T.dropEnd 1 (T.drop 1 s)))
      | otherwise                                      = Nothing

-- | Split a @key = value@ line (value un-trimmed of quotes).
kv :: Text -> Maybe (Text, Text)
kv l = case T.breakOn "=" l of
    (k, v) | not (T.null v) -> Just (T.strip k, T.strip (T.drop 1 v))
    _                       -> Nothing

kvString :: Text -> Text -> Maybe Text
kvString key l = case kv l of { Just (k, v) | k == key -> Just (unquote v); _ -> Nothing }

unquote :: Text -> Text
unquote = T.dropAround (`elem` ("\"'" :: String))

-- | Keys of @key = …@ lines in a table body (skips nested @[...]@ headers).
tableKeys :: [Text] -> [Text]
tableKeys body =
    [ k | l <- body, Just (k, _) <- [kv l], not (T.null k), not ("[" `T.isPrefixOf` k) ]

-- | Package names from a single-line @dependencies = [ "a>=1", "b" ]@ array.
arrayItems :: [Text] -> [Text]
arrayItems body =
    [ dep
    | l <- body, Just (k, v) <- [kv l], k == "dependencies", "[" `T.isPrefixOf` v
    , raw <- T.splitOn "," (T.dropAround (`elem` ("[]" :: String)) v)
    , let dep = T.strip (T.takeWhile (`notElem` (" ><=!~;[" :: String)) (unquote (T.strip raw)))
    , not (T.null dep) ]

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
        -- Self-heal (rare, pathological cold ghci boots): the project rows can
        -- build exactly once yet never be ATTACHED to the live DOM — frames all
        -- settle, model and statusbar are current, but `.projects` stays
        -- childless until some unrelated UI event nudges reflex-dom's deferred
        -- commit (observed live; any editor-open made the rows appear with NO
        -- rebuild).  Until that commit stall is root-caused, verify the DOM a
        -- while after this widget builds, and if the model has projects but
        -- the DOM has none, log loudly and rebuild the whole row list — the
        -- rebuild runs in a small, healthy frame and attaches normally.
        (rebuildE, fireRebuild) <- newTriggerEvent
        rebuildD <- holdDyn (0 :: Int) rebuildE
        wsPb <- getPostBuild
        checkE <- delay 30 wsPb
        performEvent_ $ ffor (tag (current projectsD) checkE) $ \projs ->
          unless (M.null projs) $ do
            n <- liftJSM $ valToNumber =<<
                   eval ("document.querySelectorAll('.projects > li').length" :: Text)
            when (n < 1) . liftIO $ do
              IO.hPutStrLn IO.stderr $
                "LEKSAH: workspace tree missing from DOM after settle ("
                  <> show (length (M.keys projs))
                  <> " projects in the model) — self-heal rebuild"
              fireRebuild 1
        (switchHold never =<<) . dyn $ ffor rebuildD $ \_ -> listViewWithKey projectsD $ \(_, pKey) projectD -> do
          let isActiveProjectD = (== Just pKey) <$> activeProjectKeyD
          -- Reveal (expand) the project when the active file is anywhere under it.
          projNodeRevealD <- revealUnder (constDyn (pjDir pKey)) revealFileD
          -- Current branch + open PR for this project's checkout, scanned once
          -- here and shared by the git subtree, the collapsed summary and the
          -- "Open PR" menu item.
          (brD, prD) <- gitBranchPr (pjDir pKey)
          -- Only the active project starts expanded; activating another project
          -- collapses this one (and expands that one).
          initActive <- sample (current isActiveProjectD)
          treeItemDynAttrSet' (updated isActiveProjectD) projNodeRevealD
            (("class" =:) . ("project" <>) <$> (bool "" " active" <$> isActiveProjectD)) initActive
            (do
              claudeAvail <- liftIO claudeAvailable
              -- ⌥-clicking the terminal/Claude items opens them into a split of
              -- the active pane (⌥⇧ = the other direction), like the ⌥-open
              -- tree gestures; wrapSplit embeds the request in the menu's
              -- command type.  Remote projects can't split (a split runs on the
              -- local tmux server), so their items carry no target.
              let wrapSplit t sh = ProjectCommand (CommandWorkspaceAction "" ""
                                     (liftIO (requestSplitOpen (t, sh))))
                  localTgt t = if isRemotePath (pjDir pKey) then Nothing else Just t
                  plain = fmap (fmap (Nothing,))
              (projRowEl, rowE) <- treeSelect' "workspace" (menuSplitWith wrapSplit $
                [ plain $ ("Activate",) . ProjectCommand . CommandWorkspaceAction "Set as Active Project" "" <$>
                    (workspaceActivatePackage <$> projectD <*> pure Nothing <*> pure Nothing)
                ] <> case pjFile pKey of
                        Just file -> [ constDyn ("Open Project File", (Just (STFile file), ProjectFileEvents . ("" =:) $ OpenFile False file)) ]
                        _ -> []
                  <> case pKey of
                        CabalTool p ->
                          [ let f = pjCabalFile p <.> "local"
                            in constDyn ("Open Project Configuration File", (Just (STFile f), ProjectFileEvents . ("" =:) $ OpenFile True f)) ]
                        _ -> []
                <> [ ("Open Terminal Here",) . (localTgt (STTermDir (pjDir pKey)),) . ProjectCommand . CommandWorkspaceAction "" "" <$>
                    (runProject projectOpenTerminal <$> projectD)
                , plain $ ("Refresh Nix Environment Varialbes",) . ProjectCommand . CommandWorkspaceAction "" "" <$>
                    (runProject projectRefreshNix <$> projectD)
                , plain $ constDyn ("Remove From Workspace", ProjectCommand (CommandWorkspaceAction "" "" (workspaceRemoveProject pKey)))
                , plain $ constDyn ("Project Settings…", ProjectCommand (CommandWorkspaceAction "" "" (liftIO (requestRemoteSettings pKey))))
                , plain $ ffor prD $ \mpr ->
                    ( "Open PR" <> prSuffix mpr
                    , ProjectCommand (CommandWorkspaceAction "" "" (liftIO (maybe (return ()) (openUrl . snd) mpr))) )
                ]
                -- Claude Code (only when the CLI is on PATH): start a fresh
                -- session or continue the most recent one in the project dir.
                <> [ constDyn ("New Claude Session", (localTgt (STClaudeNew (pjDir pKey)), ProjectCommand (CommandWorkspaceAction "" "" (liftIO (runClaudeCmd (ClaudeNew (pjDir pKey))))))) | claudeAvail ]
                <> [ constDyn ("Continue Last Claude Session", (localTgt (STClaudeContinue (pjDir pKey)), ProjectCommand (CommandWorkspaceAction "" "" (liftIO (runClaudeCmd (ClaudeContinue (pjDir pKey))))))) | claudeAvail ]) $ do
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
                -- Git icon + branch + PR number, shown inline only while the
                -- project is collapsed (CSS keys off the li's tree-collapsed
                -- class); rendered only when the checkout has a branch.
                elClass "span" "proj-git-summary" $
                    void . dyn $ ffor ((,) <$> brD <*> prD) $ \case
                        (Nothing, _)   -> return ()
                        (Just b, mpr)  -> gitIcon >> text (b <> prSuffix mpr)
                return never
              -- Double-click a project row → open a terminal at its directory.
              -- ⌥ opens it into a split of the active pane instead (local only).
              pmE <- dblclickMods projRowEl
              performEvent_ $ ffor pmE $ \(alt, sh) ->
                if alt && not (isRemotePath (pjDir pKey))
                  then liftIO (requestSplitOpen (STTermDir (pjDir pKey), sh))
                  else openTerminalInDir (pjDir pKey)
              return rowE) $
            el "ul" $ do
              -- Top item: the project's git tree (self-hides unless the project
              -- dir is itself a git checkout).
              gitTreeNode (pjDir pKey) brD prD
              -- The project's Claude Code sessions, surfaced right under the
              -- project row (self-hides unless the project dir has sessions) so
              -- it's reachable without drilling into the Files node.
              claudeAvail <- liftIO claudeAvailable
              when claudeAvail $ claudeNode "workspace" (pjDir pKey)
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
                    pkgCmd t f = plainPkg $ (t,) . PackageCommand . CommandWorkspaceAction "" "" <$> (runProject . runPackage f <$> packageD <*> projectD)
                    -- A Claude launch item for the package's directory (Dynamic
                    -- because the package dir is); @st@ is its ⌥-split target
                    -- (local dirs only — a split runs on the local tmux server).
                    pkgClaude t c st = (\d -> (t, ( if isRemotePath d then Nothing else Just (st d)
                                                  , PackageCommand (CommandWorkspaceAction "" "" (liftIO (runClaudeCmd (c d))))))) <$> pkgDirD
                    -- ⌥-split plumbing for the package menu (see the project
                    -- menu's wrapSplit/plain above).
                    wrapSplitPkg t sh = PackageCommand (CommandWorkspaceAction "" ""
                                          (liftIO (requestSplitOpen (t, sh))))
                    plainPkg = fmap (fmap ((,) Nothing))
                -- claudeAvail is the project-level binding above (in scope here).
                treeItemDynAttr' pkgRevealE (("class" =:) . ("package" <>) <$> (bool "" " active" <$> isActivePackageD)) False
                  (treeSelect "workspace" (menuSplitWith wrapSplitPkg $
                      [ plainPkg $ ("Activate",) . PackageCommand . CommandWorkspaceAction "Set as Active Package" "" <$>
                          (workspaceActivatePackage <$> projectD <*> (Just <$> packageD) <*> pure Nothing)
                      , pkgCmd "Build" makePackage
                      , pkgCmd "Run" packageRun
                      , pkgCmd "Test" packageTest
                      , pkgCmd "Benchmark" packageBench
                      , pkgCmd "Clean" packageClean
                      , (\cf -> ("Open Package File", (Just (STFile cf), PackageFileEvents (("" =:) (OpenFile False cf)))))
                          . ipdCabalFile <$> packageD
                      ]
                      <> [ pkgClaude "New Claude Session" ClaudeNew STClaudeNew | claudeAvail ]
                      <> [ pkgClaude "Continue Last Claude Session" ClaudeContinue STClaudeContinue | claudeAvail ]) $ do
                    elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-package.svg") $ return ()
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
                    when (dropTrailingPathSeparator pkgDir /= dropTrailingPathSeparator (pjDir pKey)) $ do
                        (pkgBrD, pkgPrD) <- gitBranchPr pkgDir
                        gitTreeNode pkgDir pkgBrD pkgPrD
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
                            -- Show the package's own Claude node unless the
                            -- package sits at the project root (the project row
                            -- already surfaces that dir's Claude node).
                            (\sd ig d -> fileTree "workspace" sd ig showHiddenD showIgnoredD activeFileD revealFileD
                                (dropTrailingPathSeparator d /= dropTrailingPathSeparator (pjDir pKey)) d)
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
                      -- A directory project (nix / make / plain "Open Folder"
                      -- CustomTool) has no package "Files" nodes to distinguish
                      -- from, so plain "Files" reads better than "Other Files".
                      text $ case pKey of
                        NixTool {}    -> "Files"
                        MakeTool {}   -> "Files"
                        CustomTool {} -> "Files"
                        _             -> "Other Files"
                      return never) $
                        el "ul" $
                          (switchHold never =<<) . dyn $
                            -- The project row already surfaces the Claude node
                            -- for pjDir, so suppress it on this "Other Files" tree.
                            (\sd ig -> fileTree "workspace" sd ig showHiddenD showIgnoredD activeFileD revealFileD False (pjDir pKey))
                              <$> pjSourceDirsD <*> pkgDirsD)
              -- Any project with a flake.nix gets a (collapsed) Flake node;
              -- it self-hides when there's no flake and only evaluates once
              -- expanded, so there's no overhead otherwise.
              flakeNode (pjDir pKey)
              -- Rust (Cargo.toml) / Python (pyproject.toml / setup.py) manifest
              -- introspection, self-hiding like the flake node.
              manifestNode (pjDir pKey)
              return $ leftmost [ProjectPackageEvents <$> packagesE, ProjectFileEvents <$> projectFilesE]
