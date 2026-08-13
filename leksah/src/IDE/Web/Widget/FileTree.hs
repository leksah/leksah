{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module IDE.Web.Widget.FileTree
  ( filesAndDirs
  , joinPaths
  , fileTree
  , claudeNode
  , statusBadge
  , GitStatus(..)
  , gitClass
  , gitBadge
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (catch, SomeException)
import Control.Monad (void, when, unless)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails, dropWhileEnd, partition)
import Data.Map (Map, mapKeys)
import qualified Data.Map as M (toList, fromList, lookup, empty, elems)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S (member, fromList)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)

-- File access goes through the IDE.Web.FS seam (real FS natively; the
-- in-memory demo tree in the browser build; ssh for remote projects).
import IDE.Git (qualifyPath, runGitBatch)
import IDE.Utils.RemotePath (isRemotePath)
import IDE.Web.FS (fsListDirectory)
import IDE.Web.RemoteRefresh (registerRemoteRefresh)
import Data.IORef (newIORef, readIORef, writeIORef)
import IDE.Web.Coalesce (newCoalescer)
import IDE.Web.LocalRefresh (registerLocalRefresh)
import IDE.Web.ReplTmux (openTerminalInDir)
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension, (</>), dropTrailingPathSeparator)

import Reflex
       (Dynamic, listViewWithKey, Event, never, ffilter, updated, leftmost,
        tag, current, getPostBuild, holdDyn,
        newTriggerEvent, holdUniqDyn, ffor, constDyn, zipDynWith,
        tickLossyFromPostBuildTime, sample)
import Reflex.Dom.Core
       (elAttr, elDynAttr, (=:), text, el, elClass, elDynClass,
        dynText, dyn, domEvent, EventName(..))

import IDE.Web.Events (FileEvents, FileEvent(..))
import IDE.Web.Widget.Tree
       (treeItem, treeItemDynAttr', treeSelect', scrollIntoViewNearest,
        dblclickMods)
import IDE.Web.SplitOpenRequest (SplitTarget(..), requestSplitOpen)
import IDE.Web.Widget.Menu (menuSplit)
import IDE.Web.Claude
       (claudeAvailable, claudeSessionsFor, claudeLiveBySession,
        ClaudeSession(..), csTitle, ClaudeLive(..),
        ClaudeCmd(..), runClaudeCmd, claudeRunning, activateMruClaude,
        copySessionId, revealSession, deleteSession)
import IDE.Web.Worktree (requestNewWorktree)
import IDE.Web.ClaudeQueue (requestTaskQueue, requestPlanReview)
import IDE.Web.Frame (MonadWidget, performEvent, performEvent_)

-- | Sub-directories and files of a directory, names only.
--
-- Total: an unreadable path (gone, no permission, or — the way this used to
-- take the whole UI down — not a directory at all, when a project ended up
-- rooted at a file) yields an EMPTY tree, never an exception.  This runs in
-- a 'performEvent', i.e. on the reflex frame thread, where anything thrown
-- is fatal to the entire window rather than to this one node.
filesAndDirs :: MonadIO m => FilePath -> m ([FilePath], [FilePath])
filesAndDirs dir = liftIO . (`catch` \(_ :: SomeException) -> return ([], [])) $ do
  -- One fsListDirectory call: names + is-directory flags together, so a
  -- remote directory costs one round trip instead of one per child.
  entries <- filter ((`notElem` [".", ".."]) . fst) <$> fsListDirectory dir
  let (dirs, files) = partition snd entries
  return (map fst dirs, map fst files)

joinPaths :: Map FilePath (Map FilePath a) -> Map FilePath a
joinPaths m = mconcat [mapKeys (dir </>) m' | (dir, m') <- M.toList m]

-- | Git status of a file, ordered by ascending severity so that `maximum`
-- gives the most significant change in a directory (used to colour folders).
data GitStatus = Untracked | Added | Renamed | Modified | Deleted | Conflict
  deriving (Eq, Ord, Show)

-- | CSS class for a status, mirroring VS Code's git decoration colours
-- (modified gold, added/untracked green, deleted/conflict red).
gitClass :: GitStatus -> Text
gitClass Untracked = "git-untracked"
gitClass Added     = "git-added"
gitClass Renamed   = "git-renamed"
gitClass Modified  = "git-modified"
gitClass Deleted   = "git-deleted"
gitClass Conflict  = "git-conflict"

-- | The single-letter badge VS Code shows to the right of a changed file.
gitBadge :: GitStatus -> Text
gitBadge Untracked = "U"
gitBadge Added     = "A"
gitBadge Renamed   = "R"
gitBadge Modified  = "M"
gitBadge Deleted   = "D"
gitBadge Conflict  = "C"

-- | Class for a (file or folder) name span: the base `git-name` plus the
-- status colour when there is one.
gitNameClass :: Maybe GitStatus -> Text
gitNameClass = ("git-name" <>) . maybe "" ((" " <>) . gitClass)

-- | Classify a porcelain XY status code.
classifyStatus :: String -> Maybe GitStatus
classifyStatus = \case
  ('?':_)   -> Just Untracked
  code@(x:y:_)
    | x == 'U' || y == 'U' || code == "AA" || code == "DD" -> Just Conflict
    | x == 'R' || y == 'R'                                 -> Just Renamed
    | x == 'A'                                             -> Just Added
    | x == 'D' || y == 'D'                                 -> Just Deleted
    | x == 'M' || x == 'T' || y == 'M' || y == 'T'         -> Just Modified
  _ -> Nothing

-- | Run git for the repo containing `dir` and return (status map, ignored set),
-- both keyed/holding absolute paths.  Empty if `dir` is not in a git repo (or
-- git is unavailable) — decoration/filtering is then simply absent.
--
-- @wantIgnored@ says whether the ignored set is needed at all.  It is used
-- ONLY to hide entries, so when the "show ignored files" preference is on
-- there is nothing to hide and the second git pass — a full working-tree walk
-- that takes tens of seconds over a populated @dist-newstyle@, plus parsing
-- its output — is skipped entirely.
gitInfo :: MonadIO m => Bool -> FilePath -> m (Map FilePath GitStatus, Set FilePath)
gitInfo wantIgnored dir = liftIO $ (`catch` \(_ :: SomeException) -> return (mempty, mempty)) $ do
  -- One batch = one ssh round trip for a remote dir (IDE.Git routes).
  results <- runGitBatch dir
    ([ ["rev-parse", "--show-toplevel"]
      -- `-uall` lists untracked files individually (so each is decorated).
     , ["-c", "core.quotePath=false", "status", "--porcelain", "-uall"]
     ] <>
      -- The ignored pass (default `-u`, which collapses fully ignored
      -- directories like dist-newstyle into one entry rather than listing
      -- every file) — only when something will be hidden with it.
     [ ["-c", "core.quotePath=false", "status", "--porcelain", "--ignored"]
     | wantIgnored ])
  case results of
    ((ExitSuccess, root, _) : (rc, out, _) : rest) -> do
      let root' = qualifyPath dir (dropWhileEnd isSpace (T.unpack root))
      return ( if rc == ExitSuccess then parseStatus root' (T.unpack out) else mempty
             , case rest of
                 ((_, iout, _) : _) -> parseIgnored root' (T.unpack iout)
                 []                 -> mempty )
    _ -> return (mempty, mempty)
  where
    parseStatus root out = M.fromList
      [ (root </> afterArrow path, st)
      | line <- lines out
      , length line >= 4
      , let (code, rest) = splitAt 2 line
      , Just st <- [classifyStatus code]
      , let path = drop 1 rest  -- the separating space after the XY code
      ]
    parseIgnored root out = S.fromList
      [ root </> dropWhileEnd (== '/') (drop 3 line)
      | line <- lines out
      , length line >= 4
      , take 2 line == "!!"
      ]
    -- "old -> new" (renames/copies): decorate the new path.
    afterArrow p = case [ s | s <- tails p, " -> " `isPrefixOf` s ] of
      (s:_) -> drop 4 s
      []    -> p

-- | The most significant status among files under a directory (for colouring
-- the folder itself, as VS Code does).
dirStatus :: FilePath -> Map FilePath GitStatus -> Maybe GitStatus
dirStatus d m =
  case [ s | (p, s) <- M.toList m, (d <> "/") `isPrefixOf` p ] of
    [] -> Nothing
    ss -> Just (maximum ss)

fileTree
  :: MonadWidget t m
  => Text
  -> Set FilePath
  -> Set FilePath     -- ^ directories to omit (e.g. other packages' dirs)
  -> Dynamic t Bool   -- ^ show hidden (dot-) files
  -> Dynamic t Bool   -- ^ show git-ignored files
  -> Dynamic t (Maybe FilePath)  -- ^ the focused file (highlighted)
  -> Dynamic t (Maybe FilePath)  -- ^ the file to reveal (expand/scroll to)
  -> Bool             -- ^ show a Claude node for the ROOT dir (subdirs always
                      --   do); pass False when a project/package row already
                      --   surfaces the Claude node for this dir, to avoid a dup
  -> FilePath
  -> m (Event t FileEvents)
fileTree treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD claudeAtRoot dir = do
  -- Compute git status/ignored once for this (top-level) directory; thread
  -- down.  The scan runs off the frame thread (remote dirs = an ssh round
  -- trip), and remote dirs rescan on RemoteRefresh events (save/build/⟳)
  -- since they have no watchers or polling.
  postBuild <- getPostBuild
  (infoE, fireInfo) <- newTriggerEvent
  -- COALESCED: a watcher fires per changed file, and `git status --ignored`
  -- over a big ignored tree (dist-newstyle during a build) takes tens of
  -- seconds — so requests while a scan runs set a flag instead of spawning
  -- another scan, and one more runs when it finishes.
  -- The ignored set is only needed while ignored files are hidden; keep the
  -- current answer in an IORef the coalesced scan reads, and rescan when the
  -- preference changes.
  wantIgnoredRef <- liftIO . newIORef . not =<< sample (current showIgnoredD)
  rescan <- liftIO . newCoalescer $
      readIORef wantIgnoredRef >>= \want -> gitInfo want dir >>= fireInfo
  performEvent_ $ liftIO rescan <$ postBuild
  performEvent_ $ ffor (updated showIgnoredD) $ \showI -> liftIO $ do
      writeIORef wantIgnoredRef (not showI)
      rescan
  when (isRemotePath dir) . void . liftIO $
      registerRemoteRefresh (const rescan)
  -- Local dirs: no polling — rescan when an fsnotify watcher fires a
  -- LocalRefresh for a path inside this tree.
  unless (isRemotePath dir) $ do
      let base = dropTrailingPathSeparator dir
      void . liftIO $
          registerLocalRefresh $ \p ->
              when (p == base || (base <> "/") `isPrefixOf` p) rescan
  infoD <- holdDyn (mempty, mempty) infoE
  fileTree' treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD infoD claudeAtRoot dir

fileTree'
  :: MonadWidget t m
  => Text
  -> Set FilePath
  -> Set FilePath
  -> Dynamic t Bool
  -> Dynamic t Bool
  -> Dynamic t (Maybe FilePath)
  -> Dynamic t (Maybe FilePath)
  -> Dynamic t (Map FilePath GitStatus, Set FilePath)
  -> Bool             -- ^ show the Claude node for THIS dir (subdirs always do)
  -> FilePath
  -> m (Event t FileEvents)
fileTree' treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD infoD claudeHere dir = do
  -- A synthetic "Claude" node at the top of every directory that has saved
  -- Claude Code sessions (self-hiding otherwise; only when the CLI is on PATH).
  -- Suppressed at a dir already covered by a project/package row's Claude node.
  avail <- liftIO claudeAvailable
  when (claudeHere && avail) $ claudeNode treeName dir
  postBuild <- getPostBuild
  newListE <- performEvent $ filesAndDirs dir <$ postBuild
  allD <- holdDyn ([], []) newListE
  let statusD  = fst <$> infoD
      ignoredD = snd <$> infoD
      -- A reveal target is only reachable in this tree if it isn't inside (or
      -- equal to) one of the omitted directories — otherwise expanding towards
      -- it would just open a dead-end path that never shows the file.
      reachable f = not (any (\d -> d == f || (d <> "/") `isPrefixOf` f) ignoreDirs)
      -- A name is shown unless it is a dot-file (and hidden are off) or
      -- git-ignored (and ignored are off).
      visible showH showI ign name =
        (showH || not ("." `isPrefixOf` name)) &&
        (showI || not ((dir </> name) `S.member` ign))
      keepD = visible <$> showHiddenD <*> showIgnoredD <*> ignoredD
      -- Hide directories that are themselves another package's directory (passed
      -- in via ignoreDirs) so a package/project tree doesn't nest its siblings.
      subdirsD = (\keep (dirs, _) -> M.fromList
                    (map (,()) (filter (\s -> keep s && not ((dir </> s) `S.member` ignoreDirs)) dirs)))
                   <$> keepD <*> allD
      filesD   = (\keep (_, fs)  -> M.fromList (map (,()) (filter keep fs)))   <$> keepD <*> allD
  subdirE <- listViewWithKey subdirsD $ \subdir _ -> do
    let subPath = dir </> subdir
        isSrcDir = subPath `S.member` srcDirs
        imgSrc = "/pics/tree-folder" <> (if isSrcDir then "-src" else "") <> ".svg"
        aggD = dirStatus subPath <$> statusD
        -- Auto-expand this directory when the reveal target is somewhere inside
        -- it *and* actually reachable here (not behind an omitted dir).  Highlight
        -- + scroll the directory itself when it *is* the highlight/reveal target
        -- (a find match can be a directory).
        underD = maybe False (\f -> reachable f && (subPath <> "/") `isPrefixOf` f) <$> revealD
        isRevealD = (== Just subPath) <$> revealD
        dirClassD = ("class" =:) . ("dir" <>) . bool "" " active" . (== Just subPath) <$> highlightD
    treeItemDynAttr' underD dirClassD False (do
      (dirEl, dmenuE) <- treeSelect' treeName (dirClaudeMenu avail subPath) $ do
        elAttr "img" ("class" =: "tree-icon" <> "src" =: imgSrc) $ return ()
        elDynClass "span" (gitNameClass <$> aggD) . text $ T.pack subdir
        return never
      pbD <- getPostBuild
      scrollIntoViewNearest (ffilter id $ leftmost [updated isRevealD, tag (current isRevealD) pbD]) dirEl
      -- Double-click a directory row → open a terminal there (local or ssh://).
      -- ⌥ opens the terminal into a split of the active pane instead (local
      -- dirs only — a split runs on the local tmux server).
      dmE <- dblclickMods dirEl
      performEvent_ $ ffor dmE $ \(alt, sh) ->
        if alt && not (isRemotePath subPath)
          then liftIO (requestSplitOpen (STTermDir subPath, sh))
          else openTerminalInDir subPath
      -- Right-click → "New Claude Session" / "Continue Last …" (when claude is on PATH).
      performEvent_ $ liftIO <$> dmenuE
      return never
      ) $ el "ul" $ fileTree' treeName srcDirs ignoreDirs showHiddenD showIgnoredD highlightD revealD infoD True subPath
  fileE <- listViewWithKey filesD $ \file _ -> do
    let imgSrc = "/pics/" <> case takeExtension file of
                    ".cabal" -> "tree-file-cabal.svg"
                    ".hs" -> "tree-file-hs.svg"
                    ".lhs" -> "tree-file-hs.svg"
                    _ -> "tree-file.svg"
        absPath = dir </> file
        fileStatusD = M.lookup absPath <$> statusD
        -- Highlight this file when it is the focused (active) editor tab.  The
        -- data-reveal-key lets the "already visible?" check find occurrences.
        liAttrsD = (\mf -> "class" =: ("file" <> bool "" " active" (mf == Just absPath))
                        <> "data-reveal-key" =: T.pack absPath) <$> highlightD
    elDynAttr "li" liAttrsD $ do
      (elFile, fmenuE) <- treeSelect' treeName (fileClaudeMenu avail absPath) $ do
        elAttr "img" ("class" =: "tree-icon" <> "src" =: imgSrc) $ return ()
        elDynClass "span" (gitNameClass <$> fileStatusD) . text $ T.pack file
        -- VS Code-style status letter to the right of the name.
        elDynClass "span" (("git-badge" <>) . maybe "" ((" " <>) . gitClass) <$> fileStatusD) $
          dynText $ maybe "" gitBadge <$> fileStatusD
        return never
      -- Right-click → "Ask Claude about this file" (when claude is on PATH).
      performEvent_ $ liftIO <$> fmenuE
      -- Scroll this file into view when it is the one to reveal.
      pbF <- getPostBuild
      let revealMeD = (== Just absPath) <$> revealD
      scrollIntoViewNearest (ffilter id $ leftmost [updated revealMeD, tag (current revealMeD) pbF]) elFile
      -- ⌥-double-click (or ⌥-Enter) opens the file into a split of the active
      -- pane instead of a new editor tab (⌥⇧ = split the other way); a plain
      -- double-click opens it normally.
      mE <- dblclickMods elFile
      performEvent_ $ ffor (ffilter fst mE) $ \(_, sh) ->
        liftIO (requestSplitOpen (STFile absPath, sh))
      return $ OpenFile False absPath <$ ffilter (not . fst) mE
  return $ (joinPaths <$> subdirE) <> fileE

-- | The "New Claude Session" context menu for a directory row — empty when the
-- @claude@ CLI isn't on PATH.  Each item's value is the 'IO' action to run.
dirClaudeMenu :: forall t m. MonadWidget t m => Bool -> FilePath -> m (Event t (IO ()))
dirClaudeMenu avail dir
  | not avail = return never
  | otherwise = menuSplit $
      [ constDyn ("New Claude Session",           (Just (STClaudeNew dir),      runClaudeCmd (ClaudeNew dir)))
      , constDyn ("Continue Last Claude Session", (Just (STClaudeContinue dir), runClaudeCmd (ClaudeContinue dir)))
      ] <>
      -- Worktrees are created on the local git CLI; remote dirs don't offer it.
      [ constDyn ("New Claude Session in Worktree…", (Nothing, requestNewWorktree dir))
      | not (isRemotePath dir) ]

-- | The right-click menu for a file row — currently just "Ask Claude about this
-- file", which starts a @claude@ session seeded to explain it.  Empty (so the
-- browser's own menu shows) when the @claude@ CLI isn't on PATH.
fileClaudeMenu :: forall t m. MonadWidget t m => Bool -> FilePath -> m (Event t (IO ()))
fileClaudeMenu avail f
  | not avail = return never
  | otherwise = menuSplit
      [ constDyn ("Ask Claude about this file", ( if isRemotePath f then Nothing else Just (STClaudeAsk f)
                                                , runClaudeCmd (ClaudeAsk f) )) ]

-- | A leading robot icon for Claude tree rows.
claudeIcon :: MonadWidget t m => m ()
claudeIcon = elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-claude.svg") (return ())

-- | A little status glyph driven by @(glyph, colour, tooltip)@; hidden when
-- 'Nothing'.  Shape AND colour differ per state (red ▲ = blocked on approval,
-- orange ● = working, green ● = running/idle) so the states stay tellable
-- apart without colour vision.
statusBadge
  :: MonadWidget t m => Dynamic t (Maybe (Text, Text, Text)) -> m ()
statusBadge badgeD = elDynAttr "span"
    (ffor badgeD $ \case
       Just (_, col, tit) -> "title" =: tit
                          <> "style" =: ("color:" <> col <> ";margin-left:5px")
       Nothing            -> "style" =: "display:none")
    (dynText $ ffor badgeD $ maybe "" (\(g, _, _) -> g))

-- | The synthetic "Claude" node for @dir@, shown only while @dir@ has saved
-- Claude Code sessions.  Double-click/Enter on the row (robot icon + count)
-- activates the most-recently-used OPEN claude terminal here, falling back to
-- the resume picker when none is open; right-click offers New/Continue/Resume.
-- Each child is a
-- session (MRU order) that resumes on double-click/Enter, with fork/copy/reveal/
-- delete on right-click.  The list rescans on a slow tick, so new sessions —
-- and freshening "3h ago" ages — appear on their own; @/rename@ names ride a
-- separate fast tick (see 'namesD') so a rename shows up promptly.
claudeNode :: forall t m. MonadWidget t m => Text -> FilePath -> m ()
claudeNode treeName dir = do
  pb <- getPostBuild
  (sessE, fireSess) <- newTriggerEvent
  (runE,  fireRun)  <- newTriggerEvent
  (nameE, fireName) <- newTriggerEvent
  -- Head-reads only ('claudeSessionsFor'), never whole transcripts.  This used
  -- to call a claudeSessionsWithUsage that summed every assistant message's
  -- token counts to fill a tooltip — which meant reading the entire folder end
  -- to end (739MB here, one transcript of 450MB), once per OS WINDOW, and again
  -- on every 30s tick that fired while the previous scan was still running.
  -- Cold, that pegged eight cores for minutes after every ghci reload (the
  -- byte-offset cache was a CAF, so :reload wiped it).  Don't reintroduce it.
  let doScan = void . forkIO $ do
        ok <- claudeAvailable
        (if ok then claudeSessionsFor dir else return []) >>= fireSess
        (if ok then claudeRunning dir     else return False) >>= fireRun
  performEvent_ $ liftIO doScan <$ pb
  tick <- tickLossyFromPostBuildTime 30
  performEvent_ $ liftIO doScan <$ tick
  -- Renames and status changes are only knowable from the live-session files,
  -- and produce no other observable change — so poll them on their own, much
  -- faster than the full rescan.  Cheap: one directory listing plus a few
  -- hundred bytes per live session (and one @ps@ to drop dead leftovers),
  -- versus the rescan's head-read of every transcript in the folder.
  let doNames = void . forkIO $ claudeLiveBySession >>= fireName
  nameTick <- tickLossyFromPostBuildTime 3
  performEvent_ $ liftIO doNames <$ leftmost [() <$ pb, () <$ nameTick]
  liveD <- holdUniqDyn =<< holdDyn M.empty nameE
  sessD <- holdDyn [] sessE
  runD  <- holdUniqDyn =<< holdDyn False runE
  hasD  <- holdUniqDyn (not . null <$> sessD)
  -- The node's status badge: the "worst" state among the live sessions running
  -- in THIS directory (waiting > working > idle), falling back to the plain
  -- green dot while a claude terminal is open here but no live state is
  -- readable (older CLI).
  let badgeD = zipDynWith nodeBadge runD liveD
  void . dyn $ ffor hasD $ \has -> when has . void $
    treeItem "claude" False
      (do (rowEl, dmenuE) <- treeSelect' treeName rootMenu $ do
             claudeIcon
             dynText $ ffor sessD $ \ss -> "Claude (" <> T.pack (show (length ss)) <> ")"
             statusBadge badgeD
             return (never :: Event t (IO ()))
          -- Double-click / Enter on the Claude node → the most-recently-used
          -- OPEN claude terminal for this directory, if any (activated in
          -- place, wherever its pane lives now); otherwise the interactive
          -- picker.
          performEvent_ $
            (liftIO . void . forkIO $
               activateMruClaude dir >>= \hit ->
                 unless hit $ runClaudeCmd (ClaudeResumePicker dir))
            <$ domEvent Dblclick rowEl
          performEvent_ $ liftIO <$> dmenuE
          return (never :: Event t ()))
      (el "ul" $ do
          void . dyn $ ffor sessD $ mapM_ (sessionRow treeName dir doScan liveD)
          return (never :: Event t ()))
  where
    nodeBadge run m
      | any ((== Just "waiting") . clStatus) here =
          Just ("▲", "#f85149", "Claude is waiting for approval here")
      | any ((`elem` [Just "busy", Just "shell"]) . clStatus) here =
          Just ("●", "#d29922", "Claude is working here")
      | not (null here) || run =
          Just ("●", "#3fb950", "A Claude session is running here")
      | otherwise = Nothing
      where here = [ l | l <- M.elems m, clDir l == dropTrailingPathSeparator dir ]
    rootMenu = menuSplit $
      [ constDyn ("New Claude Session",           (Just (STClaudeNew dir),      runClaudeCmd (ClaudeNew dir)))
      , constDyn ("Continue Last Claude Session", (Just (STClaudeContinue dir), runClaudeCmd (ClaudeContinue dir)))
      , constDyn ("Resume Session…",              (Nothing,                     runClaudeCmd (ClaudeResumePicker dir)))
      ] <>
      [ constDyn ("New Claude Session in Worktree…", (Nothing, requestNewWorktree dir))
      | not (isRemotePath dir) ] <>
      [ constDyn ("Claude Task Queue…", (Nothing, requestTaskQueue dir))
      | not (isRemotePath dir) ]

-- | One session row under a Claude node (@rescan@ refreshes the list, e.g. after
-- a delete; @liveD@ is the live-session map, polled by 'claudeNode', so renaming
-- a running session relabels its row — and its status badge tracks the CLI's
-- semantic state — without a full rescan).  The row's tooltip is the session id.
sessionRow
  :: forall t m. MonadWidget t m
  => Text -> FilePath -> IO () -> Dynamic t (Map Text ClaudeLive) -> ClaudeSession -> m ()
sessionRow treeName dir rescan liveD s = el "li" $ do
  (sEl, actE) <- treeSelect' treeName sessMenu $ do
      claudeIcon
      elAttr "span" ("class" =: "claude-session-label" <> "title" =: csId s)
        . dynText $ ffor liveD $ \m ->
          csAge s <> " · " <> fromMaybe (csTitle s) (clName =<< M.lookup (csId s) m)
      statusBadge $ ffor liveD (rowBadge . M.lookup (csId s))
      return (never :: Event t (IO ()))
  -- Double-click / Enter → resume this session.  ⌥ resumes it into a split of
  -- the active pane instead (local dirs only).
  smE <- dblclickMods sEl
  performEvent_ $ ffor smE $ \(alt, sh) -> liftIO $
    if alt && not (isRemotePath dir)
      then requestSplitOpen (STClaudeResume dir (csId s), sh)
      else runClaudeCmd (ClaudeResume dir (csId s))
  performEvent_ $ liftIO <$> actE
  where
    rowBadge Nothing  = Nothing
    rowBadge (Just l) = case clStatus l of
      Just "waiting" -> Just ("▲", "#f85149",
                              "Waiting for approval"
                                <> maybe "" (": " <>) (clWaitingFor l))
      Just "busy"    -> Just ("●", "#d29922", "Working")
      Just "shell"   -> Just ("●", "#d29922", "Running a shell command")
      Just "idle"    -> Just ("●", "#3fb950", "Idle — ready for input")
      _              -> Just ("●", "#3fb950", "Running")
    sessMenu = menuSplit
      [ constDyn ("Resume in New Session (fork)", ( if isRemotePath dir then Nothing else Just (STClaudeFork dir (csId s))
                                                  , runClaudeCmd (ClaudeResumeFork dir (csId s)) ))
      , constDyn ("Review Plan…",                 (Nothing, requestPlanReview (dir, T.pack (csPath s))))
      , constDyn ("Copy Session Id",              (Nothing, copySessionId (csId s)))
      , constDyn ("Reveal Transcript",            (Nothing, revealSession (csPath s)))
      , constDyn ("Delete Session",               (Nothing, deleteSession (csPath s) >> rescan))
      ]
