{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Which AI session an AI tool should aim at.
--
-- Every pane has a default AI session (a Claude Code session).  Only the
-- EXPLICIT choices are stored — in '_paneAISession', keyed by 'AIPaneRef' and
-- persisted in the web session file; everything else is derived here, on demand,
-- from the live sessions.  That keeps the pane-creation paths untouched (a
-- 'SplitTarget' carries primitives only, by design) and self-heals as sessions
-- come and go.
--
-- The derivation, first hit wins ('paneDefaultSession'):
--
--   1. an explicit binding for this pane;
--   2. the pane IS a live Claude session's pane — it targets itself;
--   3. the pane's project (its file, or a shell pane's cwd) → that project's
--      most recent session: live ones in flipper order, else the most recent
--      saved transcript (which the caller resumes);
--   4. the frontmost live session in flipper order;
--   5. nothing at all is running.
--
-- Ordering everywhere is leksah's own flip MRU, so the picker agrees with ⌘\`:
-- a session's position is the position of the tmux pane it runs in.
module IDE.Web.AISession
  ( AIRow(..)
  , AIChoice(..)
  , choiceKey
  , activeAIPaneRef
  , paneDefaultSession
  , paneProjectDir
  , aiPickerChoices
  , sessionInPane
  ) where

import Control.Lens ((^.))

import Data.List (elemIndex, sortOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T

import IDE.Core.Types
       (AIPaneRef(..), FlipItem(..), IDE, LeafId(..), LeksahWindow(..),
        PaneContent(..), PaneKind(..), TabKey(..), WebWindow(..),
        activeWindow, flipMru, leksahWindows, paneAISession, pjDir, pjKey,
        webWindows, workspace, wsProjects)
import IDE.Utils.Files (isSubPath)
import IDE.Web.Claude
       (ClaudeLive(..), ClaudeSession(..), claudeLiveBySession,
        claudeLiveOwners, claudeSessionLabel, claudeSessionsFor,
        paneCurrentPath)
import IDE.Web.ClaudeStatus
       (ClaudeStatus(..), ClaudeStatusRow(..), claudeStatusNow)
import IDE.Web.Widget.Terminal (TmuxPane(..), TmuxWindow(..))

-- | One row of the AI-session picker.
data AIRow = AIRow
  { arSession :: Text      -- ^ the Claude session id — the durable handle
  , arTitle   :: Text      -- ^ its @\/rename@ name, else its first prompt
  , arDir     :: FilePath  -- ^ its working directory
  , arState   :: Text      -- ^ @waiting@ \/ @busy@ \/ @idle@, or @closed@
  , arLive    :: Bool      -- ^ 'False' = exited; committing it resumes it first
  , arDefault :: Bool      -- ^ this is the active pane's current default
  } deriving (Eq, Ord, Show)

-- | What the picker offers: one of the sessions, or starting a fresh one.  The
-- last row exists so the AI tools can't dead-end when nothing is running — which
-- is what they used to do (silently send into a pane named by a preference that
-- may not exist).
data AIChoice
  = AISessionChoice AIRow
  | AINewSession FilePath  -- ^ start a new session in this directory
  deriving (Eq, Ord, Show)

-- | A stable key per row, for the overlay's keyed list.
choiceKey :: AIChoice -> Text
choiceKey (AISessionChoice r) = arSession r
choiceKey (AINewSession d)    = "new:" <> T.pack d

--
-- Which pane are we talking about
--

-- | The pane an AI tool should aim from: the frontmost OS window's visible wide0
-- tab, and — when that tab is a leksah window — its focused leaf.  Focus in the
-- side/bottom areas is deliberately ignored: those are not AI-relevant panes, so
-- the wide0 pane stays the subject while you click about in the trees.
activeAIPaneRef :: IDE -> Map Text (Text, [TmuxWindow]) -> Maybe AIPaneRef
activeAIPaneRef ide tree = do
  wid <- ide ^. activeWindow
  ww  <- M.lookup wid (ide ^. webWindows)
  k   <- _wwActive ww
  case k of
    LeksahWinKey n -> do
      lw           <- M.lookup n (ide ^. leksahWindows)
      lid@(LeafId l) <- lwFocused lw
      pc           <- M.lookup lid (lwPanes lw)
      case pcKind pc of
        PaneView _ -> Just (PRLeaf n l)
        -- A leksah pane holds a whole tmux WINDOW; the pane that actually has
        -- the keyboard is tmux's active pane within it.
        PaneTmux w -> PRTmux <$> activeTmuxPaneIn w tree
    _ -> Just (PRTab k)

-- | The active (or first) tmux pane of tmux window @wid@ (@\@7@).
activeTmuxPaneIn :: Text -> Map Text (Text, [TmuxWindow]) -> Maybe Text
activeTmuxPaneIn wid tree = listToMaybe
  [ tpId p
  | (_, (_, wins)) <- M.toList tree, w <- wins, twId w == wid
  , p <- filter tpActive (twPanes w) ++ twPanes w ]

-- | The directory a pane is \"in\", for finding its project: an editor's file, a
-- git-log\/review checkout, or a shell pane's current working directory (one
-- tmux round trip, only on the AI path).
paneDir :: Map Text (Text, [TmuxWindow]) -> IDE -> AIPaneRef -> IO (Maybe FilePath)
paneDir tree ide = \case
  PRTab k    -> return (tabDir k)
  PRLeaf n l -> return $ do
    lw <- M.lookup n (ide ^. leksahWindows)
    pc <- M.lookup (LeafId l) (lwPanes lw)
    case pcKind pc of
      PaneView k -> tabDir k
      PaneTmux _ -> Nothing
  PRTmux pane -> paneCurrentPath pane
  where
    tabDir = \case
      EditorKey f   -> Just f
      GitLogKey d _ -> Just d
      ReviewKey d   -> Just d
      PlanKey d _   -> Just d
      CompareKey d _ -> Just d
      _             -> Nothing

-- | The workspace project a path belongs to, most specific first (so a worktree
-- added as its own project wins over the checkout that contains it).
projectDirFor :: IDE -> FilePath -> Maybe FilePath
projectDirFor ide fp = listToMaybe
  [ d | ws <- maybe [] pure (ide ^. workspace)
      , d <- sortOn (Down . length) [ pjDir (pjKey p) | p <- ws ^. wsProjects ]
      , d `isSubPath` fp ]

-- | The live session running in tmux pane @pane@, from a tree we already have
-- plus one 'claudeLiveOwners' read — no extra process spawns per pane.
sessionInPane :: Map Int ClaudeLive -> Map Text (Text, [TmuxWindow]) -> Text
              -> Maybe Text
sessionInPane owners tree pane = listToMaybe
  [ clSession l
  | (_, (_, wins)) <- M.toList tree, w <- wins, p <- twPanes w, tpId p == pane
  , Just l <- [M.lookup (tpPid p) owners] ]

--
-- Ordering
--

-- | Where a session sits in the flip MRU: the position of the tmux pane it runs
-- in.  'Nothing' (→ sorts last) when it isn't in a pane leksah knows, or has
-- never been flipped to.
flipPosOf :: Map Int ClaudeLive -> Map Text (Text, [TmuxWindow]) -> [FlipItem]
          -> Text -> Maybe Int
flipPosOf owners tree mru sid = do
  item <- listToMaybe
    [ FlipPane s (twIndex w) (tpIndex p)
    | (s, (_, wins)) <- M.toList tree, w <- wins, p <- twPanes w
    , Just l <- [M.lookup (tpPid p) owners], clSession l == sid ]
  elemIndex item mru

-- | Live sessions in flipper order: the ones leksah has flipped to, in that
-- order, then the rest worst-state-first (a session waiting for approval is more
-- interesting than an idle one) and by title.
orderSessions :: Map Int ClaudeLive -> Map Text (Text, [TmuxWindow]) -> [FlipItem]
              -> Map Text ClaudeLive -> [Text]
orderSessions owners tree mru live =
  map fst . sortOn key $ M.toList live
  where
    -- Never-flipped sessions all share the maxBound slot, so 'rank' and the
    -- title only ever break ties among THOSE — flipped ones have distinct
    -- positions and keep strict MRU order.
    key (sid, l) =
      ( fromMaybe (maxBound :: Int) (flipPosOf owners tree mru sid)
      , rank l
      , T.toLower (fromMaybe "" (clName l)) )
    rank l = case clStatus l of
      Just "waiting" -> 0 :: Int
      Just "busy"    -> 1
      Just "shell"   -> 1
      _              -> 2

--
-- The derivation
--

-- | The pane's default AI session — the chain documented at the top of this
-- module.  One 'claudeLiveOwners' \/ 'claudeLiveBySession' read per call (plus a
-- tmux round trip for a shell pane's cwd, and a transcript scan only when a
-- project has no LIVE session).
paneDefaultSession :: IDE -> Map Text (Text, [TmuxWindow]) -> AIPaneRef
                   -> IO (Maybe Text)
paneDefaultSession ide tree ref = do
  owners <- claudeLiveOwners
  live   <- claudeLiveBySession
  paneDefaultSessionWith owners live ide tree ref

-- | 'paneDefaultSession' with the two live-session reads already done — they cost
-- a @ps@ each, and the picker needs them anyway.
paneDefaultSessionWith
  :: Map Int ClaudeLive -> Map Text ClaudeLive -> IDE
  -> Map Text (Text, [TmuxWindow]) -> AIPaneRef -> IO (Maybe Text)
paneDefaultSessionWith owners live ide tree ref =
  case M.lookup ref (ide ^. paneAISession) of
  Just sid -> return (Just sid)          -- 1. explicitly chosen
  Nothing  -> do
    let ordered = orderSessions owners tree (ide ^. flipMru) live
        self = case ref of               -- 2. the pane is a session's own pane
          PRTmux pane -> sessionInPane owners tree pane
          _           -> Nothing
    case self of
      Just sid -> return (Just sid)
      Nothing -> do
        mdir <- paneDir tree ide ref     -- 3. the pane's project
        let mproj = projectDirFor ide =<< mdir
        inProject <- case mproj of
          Nothing   -> return Nothing
          Just pdir -> case [ s | s <- ordered
                                , Just l <- [M.lookup s live]
                                , pdir `isSubPath` clDir l ] of
            (s : _) -> return (Just s)
            -- No live session here: the project's most recent transcript, which
            -- the caller resumes on commit.
            []      -> fmap (fmap csId . listToMaybe) (claudeSessionsFor pdir)
        return $ case inProject of
          Just s  -> Just s
          Nothing -> listToMaybe ordered -- 4./5. frontmost live session, or none

--
-- The picker's rows
--

-- | The project directory a pane belongs to (its file's project, else the
-- directory itself) — where a \"new session\" started from this pane should run.
paneProjectDir :: IDE -> Map Text (Text, [TmuxWindow]) -> AIPaneRef
               -> IO (Maybe FilePath)
paneProjectDir ide tree ref = do
  mdir <- paneDir tree ide ref
  return $ case mdir of
    Nothing -> Nothing
    Just d  -> Just (fromMaybe d (projectDirFor ide d))

-- | The picker's list for pane @ref@: its default first (even if that session
-- has exited — it is shown @closed@ and resumed on commit), then every live
-- session in flipper order, then \"start a new one here\".  Also returns the
-- default's session id, for the caller's own bookkeeping.
-- NOT for the reflex frame thread: this spawns @ps@ (twice) and may make a tmux
-- round trip and read a transcript head.  Call it from a forked thread and fire
-- the result in as an event — see the picker's wiring in "IDE.Web.Main".
aiPickerChoices :: IDE -> Map Text (Text, [TmuxWindow]) -> Maybe AIPaneRef
                -> IO ([AIChoice], Maybe Text)
aiPickerChoices ide tree mref = do
  owners  <- claudeLiveOwners
  live    <- claudeLiveBySession
  -- Titles come from the status poll's cache (an 'IORef' read): it already holds
  -- a name-or-first-prompt for every live session, so the picker never re-reads a
  -- transcript for one.
  cached  <- claudeStatusNow
  mdflt   <- maybe (return Nothing) (paneDefaultSessionWith owners live ide tree) mref
  let titles  = M.fromList [ (csrSession r, csrTitle r) | r <- csRows cached ]
      ordered = orderSessions owners tree (ide ^. flipMru) live
      -- The default leads, and is not repeated further down.
      sids = maybe ordered (\d -> d : filter (/= d) ordered) mdflt
  rows  <- mapM (row titles mdflt live) sids
  mnew  <- maybe (return Nothing) (paneProjectDir ide tree) mref
  let newRow = case mnew of
        Just d  -> [AINewSession d]
        -- No directory to anchor a new session to (an untitled pane, no
        -- workspace): offering it would be a lie, so don't.
        Nothing -> []
  return (map AISessionChoice rows ++ newRow, mdflt)
  where
    row titles mdflt live sid = do
      let mlive = M.lookup sid live
          dir   = maybe "" clDir mlive
      title <- case clName =<< mlive of
        Just n  -> return n
        Nothing -> case M.lookup sid titles of
          Just t  -> return t
          -- Only a session the poll hasn't seen (i.e. a CLOSED default) costs a
          -- transcript read, and only if we know where it ran.
          Nothing -> do
            lbl <- if null dir then return Nothing
                               else claudeSessionLabel dir sid
            return (fromMaybe (T.take 8 sid) lbl)
      return AIRow
        { arSession = sid
        , arTitle   = title
        , arDir     = dir
        , arState   = case mlive of
            Nothing -> "closed"
            Just l  -> case clStatus l of
              Just "waiting" -> "waiting"
              Just "busy"    -> "busy"
              Just "shell"   -> "busy"
              _              -> "idle"
        , arLive    = isJust mlive
        , arDefault = mdflt == Just sid
        }
