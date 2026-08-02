{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The pure algebra and persistence of LEKSAH WINDOWS ('LeksahWindow' in
-- @IDE.Core.Types@): each wide0 tab holds a native split tree whose panes are
-- whole tmux windows (tmux keeps laying out its own panes inside them) or
-- native leksah views (editors, git logs), each with its own font size.
--
-- Persistence is split by backing.  Session-backed windows live ON their
-- tmux session, as the @\@leksah_layout@ session option (the session-level
-- sibling of the @\@leksah_run@ pane option): a base64-encoded, versioned
-- JSON blob holding every leksah window backed by that session.  Base64
-- sidesteps tmux quoting and any @#{}@ format expansion when the option is
-- read back through a format string.  Storing it on the session means those
-- layouts survive leksah restarts for as long as the tmux session lives, and
-- die with it — exactly the lifetime of the tmux windows they describe.
-- Sessionless windows (pure native views) are persisted in the web session
-- file instead (see 'wsLeksahWindows' in "IDE.Web.Session").
--
-- 'reconcileWindows' is the single validation choke point: everything read
-- from the options (or mutated at runtime) is reconciled against the LIVE
-- local sessions — dead tmux windows drop out (collapsing their splits),
-- emptied leksah windows disappear, and every live tmux window nobody owns
-- (a "stray") is auto-wrapped in a new single-pane leksah window.  With no
-- stored layouts at all the result is one leksah window per tmux window:
-- exactly the classic one-tab-per-window rendering, so old sessions are
-- compatible by construction.
module IDE.Web.SplitLayout
  ( -- * Ids
    lwIdText
  , lwIdNum
    -- * Codec
  , layoutVersion
  , encodeLayoutOption
  , decodeLayoutOption
    -- * Validation / defaulting
  , singlePaneWindow
  , reconcileWindows
  , viewLeafAllowed
    -- * Tree queries
  , treeLeafIds
  , successorLeaf
  , lwWindowIds
  , paneForWindow
  , windowOwner
  , LeafRect(..)
  , leafRects
  , NativeDivider(..)
  , treeDividers
    -- * Edits
  , splitLeaf
  , closeLeaf
  , setPaneFont
  , resizeNode
  , ConvertedPath(..)
  , spliceConverted
    -- * tmux persistence
  , saveSessionLayouts
  , readLeksahWindows
  ) where

import Control.Applicative ((<|>))
import Control.Exception (catch, SomeException)
import Control.Monad (void, unless)
import Data.Aeson
       (FromJSON(..), ToJSON(..), Value, object, withObject, (.=), (.:),
        (.:?), decodeStrict)
import qualified Data.Aeson as Aeson (encode)
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
       (adjust, delete, empty, fromList, insert, keysSet, lookup, member,
        singleton, toAscList)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
       (difference, fromList, insert, member, union, unions)
import Data.Text (Text)
import qualified Data.Text as T
       (isPrefixOf, length, null, pack, splitOn, stripPrefix, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import System.Directory (findExecutable)
import System.Log.Logger (debugM)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)

import IDE.Core.Types
       (LeafId(..), LeksahWindow(..), PaneContent(..), PaneKind(..),
        SplitOrientation(..), SplitTree(..), TabKey(..))
import IDE.Web.ReplTmux (tmuxSocket)
import IDE.Web.Session (viewLeafAllowed)   -- + orphan JSON instances

-- | Version of the @\@leksah_layout@ JSON.  v1 (the retired per-session
-- stack layout) is still READ and migrated; any other mismatch (or decode
-- failure) means the stored option is ignored and the windows are rebuilt as
-- strays — never a partial read.
layoutVersion :: Int
layoutVersion = 2

unLeafId :: LeafId -> Int
unLeafId (LeafId n) = n

-- | Leksah window ids as stored/minted: @lw-N@.
lwIdText :: Int -> Text
lwIdText n = "lw-" <> T.pack (show n)

-- | Recover the number from an @lw-N@ id (used to seed the minter past every
-- persisted id).
lwIdNum :: Text -> Maybe Int
lwIdNum t = T.stripPrefix "lw-" t >>= readMaybe . T.unpack

--
-- Codec.  The JSON instances for 'SplitTree' / 'PaneContent' /
-- 'LeksahWindow' live in "IDE.Web.Session" (shared with the web session
-- file's 'wsLeksahWindows').
--

-- | One session's option payload: every leksah window backed by it, as
-- @(id, window)@ pairs, base64 encoded.
encodeLayoutOption :: [(Text, LeksahWindow)] -> Text
encodeLayoutOption lws =
  case decodeUtf8' (B64.encode (LBS.toStrict (Aeson.encode payload))) of
    Right t -> t
    Left _  -> ""   -- base64 output is always ASCII; unreachable
  where
    payload = object [ "v" .= layoutVersion, "windows" .= lws ]

-- | Decode a stored option value for session @sid@.  Yields the windows with
-- 'lwSession' forced to @sid@ (context wins over whatever was stored) and
-- their stored id — 'Nothing' for windows migrated from a v1 payload, which
-- had none (the caller mints those).  'Nothing' overall on any failure — the
-- caller rebuilds the session's windows as strays.
decodeLayoutOption :: Text -> Text -> Maybe [(Maybe Text, LeksahWindow)]
decodeLayoutOption sid t = case B64.decode (encodeUtf8 t) of
    Left _   -> Nothing
    Right bs -> do
      v <- decodeStrict bs
      parseMaybe (versioned v) v
  where
    versioned v = withObject "leksah_layout" $ \o -> o .: "v" >>= \case
      (2 :: Int) -> parseV2 v
      1          -> parseV1 v
      n          -> fail ("layout version " <> show n)
    parseV2 = withObject "leksah_layout v2" $ \o -> do
      lws <- o .: "windows"
      return [ (Just i, lw { lwSession = Just sid }) | (i, lw) <- lws ]
    parseV1 v = migrateV1 sid <$> parseJSON v

--
-- v1 migration: the retired per-session layout whose leaves were tabbed
-- STACKS of tmux windows.  The stored tree survives as ONE leksah window
-- (each stack leaf becomes its active member window); the other stack
-- members become additional single-pane leksah windows.  Delete once v1
-- options have cycled out (they are rewritten as v2 on the first save).
--

data V1Leaf
  = V1Stack [Text] (Maybe Text) (Maybe Int)
  | V1View TabKey (Maybe Int)

data V1Layout = V1Layout
  { v1Tree    :: SplitTree
  , v1Leaves  :: Map LeafId V1Leaf
  , v1Focused :: Maybe LeafId
  , v1Zoomed  :: Maybe LeafId
  , v1Next    :: Int
  }

instance FromJSON V1Leaf where
  parseJSON = withObject "v1 leaf" $ \o -> do
    kind <- o .: "kind"
    font <- o .:? "font"
    case kind :: Text of
      "stack" -> V1Stack <$> o .: "windows" <*> o .:? "active" <*> pure font
      "view"  -> do
        tab <- o .: "tab"
        unless (viewLeafAllowed tab) $ fail "bad v1 view leaf"
        return (V1View tab font)
      _ -> fail "unknown v1 leaf kind"

instance FromJSON V1Layout where
  parseJSON = withObject "v1 layout" $ \o -> do
    tree    <- o .: "tree"
    leaves  <- o .: "leaves"
    focused <- o .:? "focused"
    zoomed  <- o .:? "zoomed"
    next    <- o .: "next"
    return V1Layout
      { v1Tree    = tree
      , v1Leaves  = M.fromList [ (LeafId n, c) | (n, c) <- leaves ]
      , v1Focused = LeafId <$> focused
      , v1Zoomed  = LeafId <$> zoomed
      , v1Next    = next
      }

migrateV1 :: Text -> V1Layout -> [(Maybe Text, LeksahWindow)]
migrateV1 sid v1 = mainWindow <> extraWindows
  where
    converted =
      [ (l, pc, extras)
      | l <- treeLeafIds (v1Tree v1)
      , Just leaf <- [M.lookup l (v1Leaves v1)]
      , Just (pc, extras) <- [convert leaf]
      ]
    convert (V1View t f)     = Just (PaneContent (PaneView t) f, [])
    convert (V1Stack ws a f) = case a of
      Just w | w `elem` ws -> Just (PaneContent (PaneTmux w) f, wrap f (filter (/= w) ws))
      _ -> case ws of
        (w : rest) -> Just (PaneContent (PaneTmux w) f, wrap f rest)
        []         -> Nothing
    wrap f ws = [ singlePaneWindow (Just sid) (PaneContent (PaneTmux w) f) | w <- ws ]
    keptIds = S.fromList [ l | (l, _, _) <- converted ]
    mainWindow = case removeLeaves
        (S.fromList (treeLeafIds (v1Tree v1)) `S.difference` keptIds)
        (v1Tree v1) of
      Nothing   -> []
      Just tree ->
        [ ( Nothing
          , LeksahWindow
              { lwSession = Just sid
              , lwTree    = tree
              , lwPanes   = M.fromList [ (l, pc) | (l, pc, _) <- converted ]
              , lwFocused = keep (v1Focused v1)
              , lwZoomed  = keep (v1Zoomed v1)
              , lwNext    = v1Next v1
              } ) ]
      where keep mb = mb >>= \l -> if l `S.member` keptIds then Just l else Nothing
    extraWindows = [ (Nothing, lw) | (_, _, ex) <- converted, lw <- ex ]

--
-- Tree queries
--

-- | Leaf ids in tree order (left-to-right, depth-first) — the canonical
-- reading order used everywhere a deterministic pane order matters.
treeLeafIds :: SplitTree -> [LeafId]
treeLeafIds (SplitLeaf l)     = [l]
treeLeafIds (SplitNode _ ks)  = concatMap (treeLeafIds . snd) ks

-- | The focus successor of a closing leaf: the pane that visually absorbs
-- its space — the nearest @alive@ sibling in the closed leaf's own split
-- (the one before it if any, else the one after; a sibling subtree
-- contributes its leaf on the edge facing the closed one), walking up a
-- level when the whole parent died.  NEVER an MRU pick: closing a pane must
-- keep the keyboard in the same leksah window, beside where it was.
successorLeaf :: (LeafId -> Bool)   -- ^ still alive?
              -> LeafId -> SplitTree -> Maybe LeafId
successorLeaf alive target = go
  where
    go (SplitLeaf _) = Nothing
    go (SplitNode _ ks) =
      case break (contains . snd) ks of
        (before, (_, inside) : after) ->
          let prevs = [ l | (_, t) <- reverse before
                          , l <- reverse (treeLeafIds t), alive l ]
              nexts = [ l | (_, t) <- after
                          , l <- treeLeafIds t, alive l ]
          in go inside <|> listToMaybe prevs <|> listToMaybe nexts
        _ -> Nothing
    contains t = target `elem` treeLeafIds t

-- | Every tmux window id the leksah window's panes hold, in tree order.
lwWindowIds :: LeksahWindow -> [Text]
lwWindowIds lw =
  [ w
  | l <- treeLeafIds (lwTree lw)
  , Just (PaneContent (PaneTmux w) _) <- [M.lookup l (lwPanes lw)]
  ]

-- | The pane holding the given tmux window, if any.
paneForWindow :: Text -> LeksahWindow -> Maybe LeafId
paneForWindow w lw = listToMaybe
  [ l
  | l <- treeLeafIds (lwTree lw)
  , Just (PaneContent (PaneTmux w') _) <- [M.lookup l (lwPanes lw)]
  , w' == w ]

-- | Which leksah window (and pane) owns the given tmux window, if any.
windowOwner :: Text -> Map Text LeksahWindow -> Maybe (Text, LeafId)
windowOwner w lws = listToMaybe
  [ (i, l) | (i, lw) <- M.toAscList lws, Just l <- [paneForWindow w lw] ]

-- | One pane's box, as FRACTIONS of the container (0..1), plus whether the
-- pane is visible.  With a zoomed pane, that pane gets the full container and
-- every other pane keeps its rect but goes invisible — invisible, not absent,
-- so its keyed widget (and the xterms inside) is never torn down by a zoom.
data LeafRect = LeafRect
  { lrX, lrY, lrW, lrH :: Double
  , lrVisible :: Bool
  } deriving (Eq, Show)

-- | Every pane's rect, from the ratio tree (see 'LeafRect' for zoom).
leafRects :: Maybe LeafId -> SplitTree -> Map LeafId LeafRect
leafRects zoomed tree = case zoomed of
    Just z | z `elem` treeLeafIds tree ->
      M.fromList [ (l, if l == z then LeafRect 0 0 1 1 True
                                 else r { lrVisible = False })
                 | (l, r) <- M.toAscList (go 0 0 1 1 tree) ]
    _ -> go 0 0 1 1 tree
  where
    go x y w h (SplitLeaf l) = M.singleton l (LeafRect x y w h True)
    go x y w h (SplitNode o ks) =
      let total = max 1e-9 (sum (map fst ks))
          place off (r, t) =
            let f = r / total
            in ( off + f
               , case o of
                   SplitH -> go (x + off * w) y (f * w) h t
                   SplitV -> go x (y + off * h) w (f * h) t )
      in mconcat (snd (foldl' (\(off, ms) k -> let (off', m) = place off k
                                               in (off', m : ms))
                              (0, []) ks))

-- | A draggable gutter between two adjacent children of a split node: the
-- CENTRE line of the gap, in container fractions, plus how to address the
-- resize ('resizeNode': path of child indexes from the root to the node, and
-- the index of the child BEFORE the gutter).
data NativeDivider = NativeDivider
  { ndVertical :: Bool    -- ^ True = a vertical line (children side by side)
  , ndX, ndY   :: Double  -- ^ the line's top-left corner (fractions 0..1)
  , ndLen      :: Double  -- ^ its length along the perpendicular axis
  , ndAxis     :: Double  -- ^ the node's extent along the DRAG axis (fraction
                          --   of the container) — converts a px drag delta
                          --   into 'resizeNode's node-relative fraction
  , ndPath     :: [Int]   -- ^ child indexes from the root to the 'SplitNode'
  , ndIndex    :: Int     -- ^ the gutter sits after this child
  } deriving (Eq, Show)

-- | Every internal gutter of the tree (nothing for a lone leaf).
treeDividers :: SplitTree -> [NativeDivider]
treeDividers = go [] 0 0 1 1
  where
    go _ _ _ _ _ SplitLeaf{} = []
    go path x y w h (SplitNode o ks) =
      let total = max 1e-9 (sum (map fst ks))
          offs  = scanl (+) 0 [ r / total | (r, _) <- ks ]
          kids  = concat
            [ go (path <> [i]) cx cy cw ch t
            | (i, (off, (_, t))) <- zip [0 ..] (zip offs ks)
            , let (cx, cy, cw, ch) = case o of
                    SplitH -> (x + off * w, y, (fst (ks !! i) / total) * w, h)
                    SplitV -> (x, y + off * h, w, (fst (ks !! i) / total) * h) ]
          guts = [ case o of
                     SplitH -> NativeDivider True  (x + off * w) y h w path i
                     SplitV -> NativeDivider False x (y + off * h) w h path i
                 | (i, off) <- zip [0 ..] (drop 1 offs)
                 , i < length ks - 1 ]
      in guts <> kids

-- | Drag one gutter: transfer @delta@ (a fraction of the NODE's extent along
-- its axis) from the child after the gutter to the one before it, clamping
-- both to a minimum share so neither collapses.
resizeNode :: [Int] -> Int -> Double -> SplitTree -> SplitTree
resizeNode path0 i delta = go path0
  where
    minShare = 0.05
    go [] (SplitNode o ks)
      | i >= 0, i + 1 < length ks =
          let total = max 1e-9 (sum (map fst ks))
              d     = delta * total
              (ra, ta) = ks !! i
              (rb, tb) = ks !! (i + 1)
              d'    = max (minShare * total - ra) (min (rb - minShare * total) d)
              ks'   = [ if j == i then (ra + d', ta)
                        else if j == i + 1 then (rb - d', tb)
                        else k
                      | (j, k) <- zip [0 ..] ks ]
          in SplitNode o ks'
    go (p : ps) (SplitNode o ks)
      | p >= 0, p < length ks =
          SplitNode o [ if j == p then (r, go ps t) else (r, t)
                      | (j, (r, t)) <- zip [0 ..] ks ]
    go _ t = t

--
-- Edits (pure; callers 'modifyIDE_' the result into '_leksahWindows')
--

-- | Mint a fresh pane beside @target@.  When the target's parent node already
-- has the wanted orientation the new pane becomes one more sibling (halving
-- the target's share); otherwise the target is replaced by a two-child node.
-- Returns the new pane's id (also made the focused pane).
splitLeaf :: LeafId -> SplitOrientation -> Bool -- ^ new pane AFTER the target?
          -> PaneContent -> LeksahWindow -> (LeafId, LeksahWindow)
splitLeaf target o after content lw =
  ( newId
  , lw { lwTree    = insertNode (lwTree lw)
       , lwPanes   = M.insert newId content (lwPanes lw)
       , lwFocused = Just newId
       , lwNext    = lwNext lw + 1
       } )
  where
    newId = LeafId (lwNext lw)
    pair r = if after then [ (r, SplitLeaf target), (r, SplitLeaf newId) ]
                      else [ (r, SplitLeaf newId), (r, SplitLeaf target) ]
    insertNode t@(SplitLeaf l)
      | l == target = SplitNode o (pair 0.5)
      | otherwise   = t
    insertNode (SplitNode o' ks)
      | o' == o
      , Just _ <- lookupIdx =
          SplitNode o' (concat
            [ if j == idx then pair (r / 2) else [(r, t)]
            | (j, (r, t)) <- zip [0 ..] ks ])
      | otherwise = SplitNode o' [ (r, insertNode t) | (r, t) <- ks ]
      where
        lookupIdx = listToMaybe [ j | (j, (_, SplitLeaf l)) <- zip [0 ..] ks
                                    , l == target ]
        idx = maybe (-1) id lookupIdx

-- | Remove a pane (content and all) and collapse the tree.  A closed
-- 'PaneTmux' pane's window becomes unplaced — the next reconcile auto-wraps
-- it in a new leksah window — so callers dropping a pane whose tmux window
-- is still alive should kill (or hide) the window too.  Closing the last
-- pane leaves an empty window that the reconcile deletes.
closeLeaf :: LeafId -> LeksahWindow -> LeksahWindow
closeLeaf l lw = lw
  { lwTree    = tree'
  , lwPanes   = M.delete l (lwPanes lw)
    -- Focus falls to the closed leaf's NEIGHBOUR — the sibling that absorbs
    -- its space ('successorLeaf') — never a dangling ref (that breaks
    -- everything keyed on lwFocused: tab-click promotion, ⌘W/⌘D/fonts, …).
  , lwFocused = if lwFocused lw == Just l
                  then successorLeaf (/= l) l (lwTree lw)
                         <|> listToMaybe (filter (/= l) (treeLeafIds tree'))
                  else lwFocused lw
  , lwZoomed  = if lwZoomed lw == Just l then Nothing else lwZoomed lw
  }
  where tree' = fromMaybe (SplitLeaf l) (removeLeaves (S.fromList [l]) (lwTree lw))

-- | The minimal-ancestor-path structure of a converted multi-pane tmux
-- window: the nodes from the window root down to the isolated pane, each
-- sibling subtree already re-hosted into its own fresh tmux window (the
-- @Left@ ids).  Built by the conversion driver in IDE.Web.Main.
data ConvertedPath
  = CPLeaf
  | CPNode SplitOrientation [(Double, Either Text ConvertedPath)]
  deriving (Eq, Show)

-- | Replace converted pane @target@ (whose tmux window now holds ONLY the
-- isolated pane) with the mirrored path structure: the target leaf keeps its
-- id and content (so its keyed widget — and xterm — survives), each
-- re-hosted sibling window becomes a fresh pane inheriting the original's
-- font, and the isolated leaf takes the focus.
spliceConverted :: LeafId -> ConvertedPath -> LeksahWindow -> LeksahWindow
spliceConverted target cp lw =
    lw { lwTree    = replace (lwTree lw)
       , lwPanes   = foldr (uncurry M.insert) (lwPanes lw) newPanes
       , lwNext    = next'
       , lwFocused = Just target
       }
  where
    font = pcFontSize =<< M.lookup target (lwPanes lw)
    (next', tree', newPanes) = go (lwNext lw) cp
    go n CPLeaf = (n, SplitLeaf target, [])
    go n (CPNode o ks) =
        let step (n0, entries, ps) (share, ek) = case ek of
              Left w ->
                ( n0 + 1
                , entries <> [(share, SplitLeaf (LeafId n0))]
                , ps <> [(LeafId n0, PaneContent (PaneTmux w) font)] )
              Right cp' ->
                let (n1, t, ps') = go n0 cp'
                in (n1, entries <> [(share, t)], ps <> ps')
            (nEnd, entries', ps'') = foldl' step (n, [], []) ks
        in (nEnd, SplitNode o entries', ps'')
    replace t@(SplitLeaf l) | l == target = tree'
                            | otherwise   = t
    replace (SplitNode o ks) = SplitNode o [ (r, replace t) | (r, t) <- ks ]

-- | Set (or clear, back to the global pref) a pane's font size.
setPaneFont :: LeafId -> Maybe Int -> LeksahWindow -> LeksahWindow
setPaneFont l mf lw = lw
  { lwPanes = M.adjust (\pc -> pc { pcFontSize = fmap (max 6 . min 72) mf })
                       l (lwPanes lw) }

--
-- Validation / defaulting
--

-- | A fresh one-pane leksah window (what a plain open, a stray tmux window,
-- or a materialized editor tab starts as).
singlePaneWindow :: Maybe Text -> PaneContent -> LeksahWindow
singlePaneWindow session content = LeksahWindow
  { lwSession = session
  , lwTree    = SplitLeaf (LeafId 0)
  , lwPanes   = M.singleton (LeafId 0) content
  , lwFocused = Just (LeafId 0)
  , lwZoomed  = Nothing
  , lwNext    = 1
  }

-- | THE validation choke point (see the module header).  Pure; callers feed
-- it the LIVE local sessions (window ids in tmux window-index order), the
-- hidden set already resolved to window IDS, and the sessions currently
-- mid-conversion (their strays are left alone — a conversion's break-pane
-- children must not be adopted before the converting window claims them).
-- Returns the reconciled windows plus the advanced id minter.
reconcileWindows
  :: Map Text [Text]        -- ^ live LOCAL sessions -> window ids, index order
  -> Set Text               -- ^ hidden window ids
  -> Set Text               -- ^ sessions with a conversion in flight
  -> Int                    -- ^ next leksah window id ('nextLeksahWin')
  -> Map Text LeksahWindow
  -> (Map Text LeksahWindow, Int)
reconcileWindows liveBySession hidden converting next0 lws0 =
    (M.fromList (kept <> strayWindows), next1)
  where
    liveSet = S.unions [ S.fromList ws | (_, ws) <- M.toAscList liveBySession ]

    -- Pass 1 over windows in id order: prune dead/foreign/duplicate tmux
    -- panes and disallowed views, collapse, drop emptied windows, unbind a
    -- dead session, clamp fonts, fix dangling refs.
    (_, keptRev) = foldl' step (mempty, []) (M.toAscList lws0)
    kept = reverse keptRev
    step (seen, out) (i, lw) =
      let sessionAlive = maybe False (`M.member` liveBySession) (lwSession lw)
          keepPane l = case M.lookup l (lwPanes lw) of
            Just (PaneContent (PaneTmux w) f)
              | sessionAlive
              , w `S.member` liveSet, not (w `S.member` seen)
              , not (w `S.member` hidden)   -- "Hide Window" drops the pane;
                                            -- un-hiding readopts as a stray
              , maybe False (w `elem`)
                  (lwSession lw >>= (`M.lookup` liveBySession)) ->
                  Just (PaneContent (PaneTmux w) (clamp f))
            Just (PaneContent (PaneView t) f)
              | viewLeafAllowed t -> Just (PaneContent (PaneView t) (clamp f))
            _ -> Nothing
          panes1 = M.fromList
            [ (l, pc) | l <- treeLeafIds (lwTree lw), Just pc <- [keepPane l] ]
          seen' = seen `S.union` S.fromList
            [ w | PaneContent (PaneTmux w) _ <- [ pc | (_, pc) <- M.toAscList panes1 ] ]
      in case removeLeaves
            (S.fromList (treeLeafIds (lwTree lw)) `S.difference` M.keysSet panes1)
            (lwTree lw) of
           Nothing -> (seen, out)   -- every pane died: the window goes
           Just tree1 ->
             let hasTmux = not (null [ () | (_, PaneContent PaneTmux{} _)
                                              <- M.toAscList panes1 ])
                 lw1 = lw
                   { lwTree    = tree1
                   , lwPanes   = panes1
                   , lwSession = if hasTmux then lwSession lw
                                 else if sessionAlive then lwSession lw
                                 else Nothing   -- views only + dead session
                     -- Focus falls to the pruned leaf's NEIGHBOUR (the pane
                     -- absorbing its space, computed in the OLD tree among
                     -- survivors) when the focused one died — a stale Nothing
                     -- breaks tab-click promotion and ⌘W/⌘D/fonts.
                   , lwFocused = case keepRef panes1 (lwFocused lw) of
                       Nothing   -> (lwFocused lw >>= \old ->
                                       successorLeaf (`M.member` panes1) old
                                                     (lwTree lw))
                                    <|> listToMaybe (treeLeafIds tree1)
                       kept      -> kept
                   , lwZoomed  = keepRef panes1 (lwZoomed lw)
                   }
             in (seen', (i, lw1) : out)
    keepRef panes mb = mb >>= \l -> if l `M.member` panes then Just l else Nothing
    clamp = fmap (max 6 . min 72)

    owned = S.unions [ S.fromList (lwWindowIds lw) | (_, lw) <- kept ]

    -- Pass 2: every live window nobody owns becomes its own leksah window
    -- (sessions mid-conversion and hidden windows excepted).
    (next1, strayWindowsRev) = foldl' adopt (next0, [])
      [ (s, w)
      | (s, ws) <- M.toAscList liveBySession
      , not (s `S.member` converting)
      , w <- ws
      , not (w `S.member` owned), not (w `S.member` hidden) ]
    strayWindows = reverse strayWindowsRev
    adopt (n, out) (s, w) =
      ( n + 1
      , (lwIdText n, singlePaneWindow (Just s) (PaneContent (PaneTmux w) Nothing))
          : out )

-- | Remove a set of leaves from the tree, dissolving single-child nodes and
-- renormalising sibling ratios.  'Nothing' when nothing is left.
removeLeaves :: Set LeafId -> SplitTree -> Maybe SplitTree
removeLeaves dead = go
  where
    go t@(SplitLeaf l)
      | l `S.member` dead = Nothing
      | otherwise         = Just t
    go (SplitNode o ks) =
      case mapMaybe (\(r, t) -> (,) r <$> go t) ks of
        []       -> Nothing
        [(_, t)] -> Just t
        ks'      -> Just (SplitNode o (renorm ks'))
    renorm ks =
      let total = sum (map fst ks)
      in if total <= 0
           then [ (1 / fromIntegral (length ks), t) | (_, t) <- ks ]
           else [ (r / total, t) | (r, t) <- ks ]

--
-- tmux persistence
--

-- | Persist one session's leksah windows onto it.  Fire-and-forget (failures
-- are logged and swallowed — the in-memory windows stay the runtime truth).
-- Remote (@ssh://@) session tabs keep today's single-window view, so their
-- layouts are not persisted.
saveSessionLayouts :: Text -> [(Text, LeksahWindow)] -> IO ()
saveSessionLayouts sid lws
  | "ssh://" `T.isPrefixOf` sid = return ()
  | otherwise = (`catch` \(_ :: SomeException) -> return ()) $ do
      let b64 = encodeLayoutOption lws
      if T.length b64 > 6144
        then debugM "leksah" $ "SplitLayout: @leksah_layout for " <> T.unpack sid
               <> " too large to persist (" <> show (T.length b64) <> " chars)"
        else findExecutable "tmux" >>= mapM_ (\tmux ->
               void $ readProcessWithExitCode tmux
                 [ "-L", tmuxSocket, "set-option", "-t", T.unpack sid
                 , "@leksah_layout", T.unpack b64 ] "")

-- | Every local session's stored leksah windows, in one @list-sessions@
-- call.  Sessions with no (or an undecodable) option contribute nothing —
-- their windows are rebuilt as strays by the first reconcile.  Windows
-- without a stored id (v1 migration) are minted one here; returns the map
-- plus the next unused id number (seed 'nextLeksahWin' with at least this).
readLeksahWindows :: IO (Map Text LeksahWindow, Int)
readLeksahWindows = (`catch` \(_ :: SomeException) -> return (mempty, 0)) $
  findExecutable "tmux" >>= \case
    Nothing -> return (mempty, 0)
    Just tmux -> do
      (_, out, _) <- readProcessWithExitCode tmux
        [ "-L", tmuxSocket, "list-sessions", "-F"
        , "#{session_id}\t#{@leksah_layout}" ] ""
      let parsed =
            [ p
            | line <- lines out
            , (sid : b64 : _) <- [T.splitOn "\t" (T.pack line)]
            , not (T.null b64)
            , Just ps <- [decodeLayoutOption sid b64]
            , p <- ps ]
          start = 1 + maximum
            (0 : [ n | (Just i, _) <- parsed, Just n <- [lwIdNum i] ])
          go (n, m) (Just i, lw)  = (n, M.insert i lw m)
          go (n, m) (Nothing, lw) = (n + 1, M.insert (lwIdText n) lw m)
          (next, lws) = foldl' go (start, M.empty) parsed
      return (lws, next)
