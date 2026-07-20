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
import qualified Data.Map as M (toList, fromList, lookup)
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
import IDE.Web.LocalRefresh (registerLocalRefresh)
import IDE.Web.ReplTmux (openTerminalInDir)
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension, (</>), dropTrailingPathSeparator)

import Reflex
       (Dynamic, listViewWithKey, Event, never, ffilter, updated, leftmost,
        tag, current, getPostBuild, performEvent, performEvent_, holdDyn,
        newTriggerEvent, holdUniqDyn, ffor, constDyn,
        tickLossyFromPostBuildTime)
import Reflex.Dom.Core
       (MonadWidget, elAttr, elDynAttr, (=:), text, el, elClass, elDynClass,
        dynText, dyn, domEvent, EventName(..))

import IDE.Web.Events (FileEvents, FileEvent(..))
import IDE.Web.Widget.Tree
       (treeItem, treeItemDynAttr', treeSelect', scrollIntoViewNearest)
import IDE.Web.Widget.Menu (menu)
import IDE.Web.Claude
       (claudeAvailable, claudeSessionsFor, ClaudeSession(..),
        ClaudeCmd(..), runClaudeCmd, claudeRunning, activateMruClaude,
        copySessionId, revealSession, deleteSession)

filesAndDirs :: MonadIO m => FilePath -> m ([FilePath], [FilePath])
filesAndDirs dir = liftIO $ do
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
gitInfo :: MonadIO m => FilePath -> m (Map FilePath GitStatus, Set FilePath)
gitInfo dir = liftIO $ (`catch` \(_ :: SomeException) -> return (mempty, mempty)) $ do
  -- One batch = one ssh round trip for a remote dir (IDE.Git routes).
  results <- runGitBatch dir
    [ ["rev-parse", "--show-toplevel"]
      -- `-uall` lists untracked files individually (so each is decorated).
    , ["-c", "core.quotePath=false", "status", "--porcelain", "-uall"]
      -- A second pass with `--ignored` (default `-u`, which collapses fully
      -- ignored directories like dist-newstyle into one entry rather than
      -- listing every file).
    , ["-c", "core.quotePath=false", "status", "--porcelain", "--ignored"]
    ]
  case results of
    [(ExitSuccess, root, _), (rc, out, _), (_, iout, _)] -> do
      let root' = qualifyPath dir (dropWhileEnd isSpace (T.unpack root))
      return ( if rc == ExitSuccess then parseStatus root' (T.unpack out) else mempty
             , parseIgnored root' (T.unpack iout) )
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
  let scan = liftIO . void . forkIO $ gitInfo dir >>= fireInfo
  performEvent_ $ scan <$ postBuild
  when (isRemotePath dir) . void . liftIO $
      registerRemoteRefresh (\_ -> void . forkIO $ gitInfo dir >>= fireInfo)
  -- Local dirs: no polling — rescan when an fsnotify watcher fires a
  -- LocalRefresh for a path inside this tree.
  unless (isRemotePath dir) $ do
      let base = dropTrailingPathSeparator dir
      void . liftIO $
          registerLocalRefresh $ \p ->
              when (p == base || (base <> "/") `isPrefixOf` p) $
                  void . forkIO $ gitInfo dir >>= fireInfo
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
      performEvent_ $ openTerminalInDir subPath <$ domEvent Dblclick dirEl
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
      return $ OpenFile False absPath <$ domEvent Dblclick elFile
  return $ (joinPaths <$> subdirE) <> fileE

-- | The "New Claude Session" context menu for a directory row — empty when the
-- @claude@ CLI isn't on PATH.  Each item's value is the 'IO' action to run.
dirClaudeMenu :: forall t m. MonadWidget t m => Bool -> FilePath -> m (Event t (IO ()))
dirClaudeMenu avail dir
  | not avail = return never
  | otherwise = menu
      [ constDyn ("New Claude Session",           runClaudeCmd (ClaudeNew dir))
      , constDyn ("Continue Last Claude Session", runClaudeCmd (ClaudeContinue dir))
      ]

-- | The right-click menu for a file row — currently just "Ask Claude about this
-- file", which starts a @claude@ session seeded to explain it.  Empty (so the
-- browser's own menu shows) when the @claude@ CLI isn't on PATH.
fileClaudeMenu :: forall t m. MonadWidget t m => Bool -> FilePath -> m (Event t (IO ()))
fileClaudeMenu avail f
  | not avail = return never
  | otherwise = menu
      [ constDyn ("Ask Claude about this file", runClaudeCmd (ClaudeAsk f)) ]

-- | A leading robot icon for Claude tree rows.
claudeIcon :: MonadWidget t m => m ()
claudeIcon = elAttr "img" ("class" =: "tree-icon" <> "src" =: "/pics/tree-claude.svg") (return ())

-- | The synthetic "Claude" node for @dir@, shown only while @dir@ has saved
-- Claude Code sessions.  Double-click/Enter on the row (robot icon + count)
-- activates the most-recently-used OPEN claude terminal here, falling back to
-- the resume picker when none is open; right-click offers New/Continue/Resume.
-- Each child is a
-- session (MRU order) that resumes on double-click/Enter, with fork/copy/reveal/
-- delete on right-click.  The list rescans on a slow tick, so new sessions —
-- and freshening "3h ago" ages — appear on their own.
claudeNode :: forall t m. MonadWidget t m => Text -> FilePath -> m ()
claudeNode treeName dir = do
  pb <- getPostBuild
  (sessE, fireSess) <- newTriggerEvent
  (runE,  fireRun)  <- newTriggerEvent
  let doScan = void . forkIO $ do
        ok <- claudeAvailable
        (if ok then claudeSessionsFor dir else return []) >>= fireSess
        (if ok then claudeRunning dir     else return False) >>= fireRun
  performEvent_ $ liftIO doScan <$ pb
  tick <- tickLossyFromPostBuildTime 30
  performEvent_ $ liftIO doScan <$ tick
  sessD <- holdDyn [] sessE
  runD  <- holdUniqDyn =<< holdDyn False runE
  hasD  <- holdUniqDyn (not . null <$> sessD)
  void . dyn $ ffor hasD $ \has -> when has . void $
    treeItem "claude" False
      (do (rowEl, dmenuE) <- treeSelect' treeName rootMenu $ do
             claudeIcon
             dynText $ ffor sessD $ \ss -> "Claude (" <> T.pack (show (length ss)) <> ")"
             -- Green "running" dot while a live claude terminal exists here.
             elDynAttr "span"
               (ffor runD $ \r -> "title" =: "A Claude session is running here"
                  <> "style" =: (if r then "color:#3fb950;margin-left:5px"
                                      else "display:none"))
               (text "●")
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
          void . dyn $ ffor sessD $ mapM_ (sessionRow treeName dir doScan)
          return (never :: Event t ()))
  where
    rootMenu = menu
      [ constDyn ("New Claude Session",           runClaudeCmd (ClaudeNew dir))
      , constDyn ("Continue Last Claude Session", runClaudeCmd (ClaudeContinue dir))
      , constDyn ("Resume Session…",              runClaudeCmd (ClaudeResumePicker dir))
      ]

-- | One session row under a Claude node (@rescan@ refreshes the list, e.g. after
-- a delete).
sessionRow
  :: forall t m. MonadWidget t m
  => Text -> FilePath -> IO () -> ClaudeSession -> m ()
sessionRow treeName dir rescan s = el "li" $ do
  (sEl, actE) <- treeSelect' treeName sessMenu $ do
      claudeIcon
      elClass "span" "claude-session-label" $ text (csAge s <> " · " <> csLabel s)
      return (never :: Event t (IO ()))
  -- Double-click / Enter → resume this session.
  performEvent_ $ liftIO (runClaudeCmd (ClaudeResume dir (csId s))) <$ domEvent Dblclick sEl
  performEvent_ $ liftIO <$> actE
  where
    sessMenu = menu
      [ constDyn ("Resume in New Session (fork)", runClaudeCmd (ClaudeResumeFork dir (csId s)))
      , constDyn ("Copy Session Id",              copySessionId (csId s))
      , constDyn ("Reveal Transcript",            revealSession (csPath s))
      , constDyn ("Delete Session",               deleteSession (csPath s) >> rescan)
      ]
