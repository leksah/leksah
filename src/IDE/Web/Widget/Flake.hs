{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
-- | The flake-outputs subtree for Nix-flake projects (see
-- 'IDE.Web.Widget.Workspace').  A flake project's tree looks a bit like
-- @nix flake show@ — output categories, then (for the per-system categories)
-- systems, then names — but the data is gathered by evaluating the flake with
-- @builtins.getFlake@ over a @git+file@ reference (so it follows the git tree)
-- via @nix eval --json@, rather than by running @nix flake show@.
--
-- The top level enumerates only category names and their immediate attribute
-- names (each force guarded by @builtins.tryEval@), so it stays cheap.  Deeper
-- levels load lazily, one @nix eval@ per expanded node ('flakeChildren'),
-- stopping at derivations (forcing into a derivation's attributes would
-- trigger builds / import-from-derivation).  Double-clicking a node opens
-- @nix develop .#<attribute path>@ in a repl-session terminal window;
-- double-clicking the tree root runs @nix repl .#@.
module IDE.Web.Widget.Flake
  ( FlakeNode(..)
  , FlakeKind(..)
  , FlakeChild(..)
  , FlakeResult
  , flakeOutputs
  , flakeChildren
  , flakeSystemCategories
  , flakeSystemNames
  , flakeCss
  , flakeTreeWidget
  , execButton
  , openNixWindow
  , developAttr
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (FromJSON(..), withObject, eitherDecode, (.:))
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
       (pack, unpack, strip, replace, concatMap, singleton, intercalate,
        unwords)
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)

import Clay
       (color, grey, red, padding, px, whiteSpace, nowrap, cursor,
        cursorDefault, (?), Css)

import Reflex
       (ffor, never, holdDyn, newTriggerEvent, performEvent_, getPostBuild,
        Dynamic, Event)
import Reflex.Dom.Core
       (MonadWidget, divClass, el, elClass, elAttr', dyn, dynText, text,
        domEvent, EventName(..), (=:))

import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import IDE.Utils.RemoteExec (runSsh, shellQuote)
import IDE.Utils.RemotePath (parseRemotePath)
import IDE.Web.RemoteTermRequest (requestLocalTerm, requestRemoteTerm)
import IDE.Web.ReplTmux (ensureCommandWindow, ensureRemoteWindow)
import IDE.Web.Widget.Tree (treeItem, treeSelect, treeSelect')

-- | One node of the top-level flake-outputs listing: a category and its
-- immediate attribute names.  The shape parses straight from the JSON.
data FlakeNode = FlakeNode
  { fnName     :: Text
  , fnChildren :: [FlakeNode]
  } deriving (Eq, Show)

instance FromJSON FlakeNode where
  parseJSON = withObject "FlakeNode" $ \o ->
      FlakeNode <$> o .: "name" <*> o .: "children"

-- | What a child attribute is: an attrset to drill into, a derivation (a leaf
-- — forcing deeper would build / import-from-derivation), or a plain value.
data FlakeKind = KindAttrs | KindDerivation | KindValue
  deriving (Eq, Show)

data FlakeChild = FlakeChild
  { fcName :: Text
  , fcKind :: FlakeKind
  } deriving (Eq, Show)

instance FromJSON FlakeChild where
  parseJSON = withObject "FlakeChild" $ \o -> do
      n <- o .: "name"
      k <- o .: "kind"
      return . FlakeChild n $ case k :: Text of
        "attrs" -> KindAttrs
        "drv"   -> KindDerivation
        _       -> KindValue

-- | Either an error message (nix missing, eval/parse failure) or the outputs.
type FlakeResult = Either Text [FlakeNode]

-- | Run @nix eval --impure --json@ on an expression (on the project's host
-- when @dir@ is remote) and parse the JSON.
nixEvalJson :: forall a . FromJSON a => FilePath -> [String] -> Text -> IO (Either Text a)
nixEvalJson dir extraOpts expr = either (Left . T.pack . show) id <$>
    (try go :: IO (Either SomeException (Either Text a)))
  where
    go = case parseRemotePath dir of
      Just (host, _) -> do
        (rc, out, err) <- runSsh host
            ("exec nix " <> T.unwords (map (shellQuote . T.pack)
                ([ "--extra-experimental-features", "nix-command flakes" ]
                 <> extraOpts
                 <> [ "eval", "--impure", "--json", "--expr" ]))
             <> " \"$0\"")
            [expr] mempty
        return $ parseResult rc (decodeUtf8Lenient out) (decodeUtf8Lenient err)
      Nothing -> findExecutable "nix" >>= \case
        Nothing  -> return (Left "nix was not found on $PATH")
        Just nix -> do
          (rc, out, err) <- readProcessWithExitCode nix
              ([ "--extra-experimental-features", "nix-command flakes" ]
               <> extraOpts
               <> [ "eval", "--impure", "--json", "--expr", T.unpack expr ]) ""
          return $ parseResult rc (T.pack out) (T.pack err)
    parseResult rc out err = case rc of
      ExitSuccess ->
        case eitherDecode (LBS.fromStrict (encodeUtf8 out)) of
          Right v -> Right v
          Left e  -> Left (T.pack ("could not parse nix output: " <> e))
      _ -> Left (T.strip err)

-- | The directory as the flake's own host sees it (the local part of an
-- ssh:// dir) — what goes inside the nix expression.
nixDirOf :: FilePath -> FilePath
nixDirOf dir = maybe dir snd (parseRemotePath dir)

-- | Evaluate the flake in @dir@ and return its top-level output tree.
flakeOutputs :: FilePath -> IO FlakeResult
flakeOutputs dir = nixEvalJson dir [] (flakeExpr (nixDirOf dir))

-- | A nix string literal (escaping @"@, @\\@ and @${@ interpolation).
nixString :: Text -> Text
nixString t = "\"" <> T.concatMap esc t <> "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '$'  = "\\$"
    esc c    = T.singleton c

-- | The flake attributes 'builtins.getFlake' mixes in that aren't outputs,
-- as a nix list literal.
flakeMetaNix :: String
flakeMetaNix = intercalate "\n"
  [ "[ \"_type\" \"outPath\" \"outputs\" \"sourceInfo\" \"inputs\" \"narHash\""
  , "  \"lastModified\" \"lastModifiedDate\" \"rev\" \"revCount\" \"shortRev\""
  , "  \"dirtyRev\" \"dirtyShortRev\" \"submodules\" \"original\" \"originalUrl\""
  , "  \"resolved\" \"resolvedUrl\" \"description\" ]"
  ]

-- | The nix expression evaluated for @dir@.  The @git+file@ reference makes
-- nix follow the git tree (including the dirty working tree).
--
-- It enumerates each output category and that category's immediate attribute
-- names (the systems for per-system categories like @packages@, or the names
-- for the rest) — one level, like @nix flake show@'s outline.  It deliberately
-- does NOT descend into per-system contents: forcing e.g. @packages.<system>@
-- triggers import-from-derivation on flakes like haskell.nix, which aborts the
-- whole evaluation (and @builtins.tryEval@ does not catch IFD/coercion errors).
-- @isAttrs@ guards keep function-valued outputs (e.g. @overlays@) from throwing;
-- @legacyPackages@ is skipped (it mirrors all of nixpkgs).
flakeExpr :: FilePath -> Text
flakeExpr dir = T.replace "__DIR__" (T.pack dir) . T.pack $ intercalate "\n"
  [ "let"
  , "  flake = builtins.getFlake \"git+file://__DIR__\";"
  , "  try = f: let r = builtins.tryEval f; in if r.success then r.value else [];"
  , "  names = s: try (if builtins.isAttrs s then builtins.attrNames s else []);"
  , "  meta = " <> flakeMetaNix <> ";"
  , "  outputs = builtins.removeAttrs flake meta;"
  , "  mk = n: c: { name = n; children = c; };"
  , "  leaf = n: mk n [];"
  , "  catNode = c: v: mk c (map leaf (names v));"
  , "in map (c: catNode c (try (outputs.${c})))"
  , "     (builtins.filter (c: c != \"legacyPackages\") (builtins.attrNames outputs))"
  ]

-- | Enumerate the children of @flake.<path>@ with their kinds.  One level;
-- run when the node is expanded.
--
-- Every eval here REFUSES import-from-derivation: determining a child's kind
-- forces its attrset head, which on flakes like haskell.nix means IFD — an
-- abort @builtins.tryEval@ cannot catch, and with IFD allowed a build nobody
-- asked for.  With IFD off it fails fast instead; we then retry enumerating
-- names only (children of unknown kind, still expandable).  Where even the
-- names need IFD the error lands on this one node, not the whole tree.
flakeChildren :: FilePath -> [Text] -> IO (Either Text [FlakeChild])
flakeChildren dir path =
    childrenEval True >>= \case
      Right cs -> return (Right cs)
      Left err -> either (const (Left err)) Right <$> childrenEval False
  where
    childrenEval withKinds = nixEvalJson dir noIFD
        . T.replace "__DIR__" (T.pack (nixDirOf dir)) . T.pack $ intercalate "\n"
      [ "let"
      , "  flake = builtins.getFlake \"git+file://__DIR__\";"
      , "  v = builtins.foldl' (a: n: builtins.getAttr n a) flake [ "
          <> T.unpack (T.intercalate " " (map nixString path)) <> " ];"
      , "  kind = x:"
      , "    let r = builtins.tryEval ("
      , "          if builtins.isAttrs x"
      , "          then (if (x.type or null) == \"derivation\" then \"drv\" else \"attrs\")"
      , "          else \"value\");"
      , "    in if r.success then r.value else \"value\";"
      , if withKinds
          then "in map (n: { name = n; kind = kind (builtins.getAttr n v); })"
          else "in map (n: { name = n; kind = \"attrs\"; })"
      , "     (builtins.attrNames v)"
      ]

-- | The top-level output categories that have a
-- @${builtins.currentSystem}@ attribute under them (devShells, packages, …),
-- plus the system itself.  Each category's force is @tryEval@-guarded;
-- import-from-derivation is refused (this runs automatically when the Flake
-- node expands, where nobody asked for a build).  @legacyPackages@ is
-- skipped (it mirrors all of nixpkgs).
flakeSystemCategories :: FilePath -> IO (Either Text (Text, [Text]))
flakeSystemCategories dir = fmap (fmap unSysNames) . nixEvalJson dir noIFD
    . T.replace "__DIR__" (T.pack (nixDirOf dir)) . T.pack $ intercalate "\n"
  [ "let"
  , "  flake = builtins.getFlake \"git+file://__DIR__\";"
  , "  sys = builtins.currentSystem;"
  , "  meta = " <> flakeMetaNix <> ";"
  , "  outputs = builtins.removeAttrs flake meta;"
  , "  hasSys = c:"
  , "    let r = builtins.tryEval (builtins.isAttrs outputs.${c} && outputs.${c} ? ${sys});"
  , "    in c != \"legacyPackages\" && r.success && r.value;"
  , "in { system = sys;"
  , "     names = builtins.filter hasSys (builtins.attrNames outputs); }"
  ]

-- | The names under @<category>.${builtins.currentSystem}@ (and the system
-- itself).  Import-from-derivation is refused (fails fast with an error) —
-- listing e.g. haskell.nix packages would otherwise kick off builds nobody
-- asked for.
flakeSystemNames :: FilePath -> Text -> IO (Either Text (Text, [Text]))
flakeSystemNames dir cat = fmap (fmap unSysNames) . nixEvalJson dir noIFD
    . T.replace "__DIR__" (T.pack (nixDirOf dir)) . T.pack $ intercalate "\n"
  [ "let"
  , "  flake = builtins.getFlake \"git+file://__DIR__\";"
  , "  sys = builtins.currentSystem;"
  , "  cat = " <> T.unpack (nixString cat) <> ";"
  , "in { system = sys;"
  , "     names = if flake ? ${cat} && builtins.isAttrs flake.${cat} && flake.${cat} ? ${sys}"
  , "             then builtins.attrNames flake.${cat}.${sys} else []; }"
  ]

noIFD :: [String]
noIFD = ["--option", "allow-import-from-derivation", "false"]

newtype SysNames = SysNames { unSysNames :: (Text, [Text]) }

instance FromJSON SysNames where
  parseJSON = withObject "SysNames" $ \o ->
      fmap SysNames $ (,) <$> o .: "system" <*> o .: "names"

flakeCss :: Css
flakeCss = do
    ".flake-label" ? do
        whiteSpace nowrap
        cursor cursorDefault
    ".flake-leaf" ? whiteSpace nowrap
    ".flake-hint" ? do
        color grey
        padding (px 2) (px 8) (px 2) (px 8)
    ".flake-error" ? do
        color red
        whiteSpace nowrap
        padding (px 2) (px 8) (px 2) (px 8)

-- | A small execute (▶) glyph button for a tree row — runs a component.
execButton :: MonadWidget t m => Text -> m (Event t ())
execButton tip = do
  (e, _) <- elAttr' "button" ("class" =: "ws-run" <> "title" =: tip) $ text "▶"
  return (domEvent Click e)

-- | Open (or bring back up) a repl-session terminal window running @cmd@ in
-- @dir@: ensure the tmux window exists (keyed so a second click reuses it)
-- and ask 'IDE.Web.Main' for its terminal tab.
openNixWindow :: FilePath -> Text -> Text -> IO ()
openNixWindow dir name cmd = void . forkIO $
    case parseRemotePath dir of
        -- Remote project: the window lives in the host's default tmux
        -- `leksah` session, surfaced as the ssh://HOST tab.
        Just (host, rdir) -> do
            _ <- ensureRemoteWindow host rdir name (Just cmd)
            requestRemoteTerm (host <> "#leksah")
        Nothing ->
            ensureCommandWindow False (T.pack dir <> "#" <> name) dir name cmd
                >>= mapM_ requestLocalTerm

-- | Open @nix develop .#\<attr path\>@ in a repl-session terminal window.
developAttr :: FilePath -> Text -> IO ()
developAttr dir attr =
    openNixWindow dir attr ("nix develop '.#" <> attr <> "' --show-trace")

-- | Render the flake-outputs tree.  The top level (categories and their
-- immediate names) comes from the initial eval; deeper levels load lazily per
-- node.  Rebuilt whenever the result changes (a refresh / re-evaluation).
flakeTreeWidget :: forall t m . MonadWidget t m => FilePath -> Dynamic t FlakeResult -> m ()
flakeTreeWidget dir resultD = void . dyn $ ffor resultD $ \case
    Left err -> divClass "flake-error" $ dynText (pure err)
    Right [] -> divClass "flake-hint"  $ text "No flake outputs."
    Right ns -> el "ul" $ mapM_ categoryNode ns
  where
    -- A category (packages, devShells, overlays, …): expanded, its children
    -- (the systems / names) already known from the top-level eval but of
    -- unknown kind — they drill lazily.  The row is a tree selection like
    -- every other workspace row (click/keyboard).
    categoryNode (FlakeNode name children) = void $ treeItem "flake-node" True
      (treeSelect "workspace" (return never) $ do
          elClass "span" "flake-label" (text (" " <> name))
          return (never :: Event t ()))
      (el "ul" $ do
          mapM_ (\(FlakeNode n _) -> flakeChildNode dir [name, n] Nothing) children
          return never)

-- | A node at @path@ under the flake root.  @kind@ 'Nothing' = unknown (not
-- yet evaluated — offer an expander and find out on demand); derivations and
-- plain values are leaves.  Double-clicking a row opens
-- @nix develop .#<path>@; rows are tree selections (click/keyboard) like the
-- rest of the workspace tree.
flakeChildNode :: forall t m . MonadWidget t m => FilePath -> [Text] -> Maybe FlakeKind -> m ()
flakeChildNode dir path kind = case kind of
    Just KindDerivation -> leafRow
    Just KindValue      -> leafRow
    _                   -> void $ treeItem "flake-node" False
      (developRow (elClass "span" "flake-label" (text (" " <> name))))
      -- Children are (re-)evaluated on each expansion, off the reflex thread.
      (do pb <- getPostBuild
          (resE, fireRes) <- newTriggerEvent
          performEvent_ $ ffor pb $ \_ -> liftIO . void . forkIO $
              flakeChildren dir path >>= fireRes
          resD <- holdDyn Nothing (Just <$> resE)
          _ <- el "ul" . dyn $ ffor resD $ \case
              Nothing         -> divClass "flake-hint"  $ text "evaluating…"
              Just (Left err) -> divClass "flake-error" $ text err
              Just (Right []) -> divClass "flake-hint"  $ text "(empty)"
              Just (Right cs) -> mapM_ (\c ->
                  flakeChildNode dir (path ++ [fcName c]) (Just (fcKind c))) cs
          return never)
  where
    name = last path
    attr = T.intercalate "." path
    leafRow = void . elClass "li" "flake-leaf" $
        developRow (text (" " <> name))
    -- A selectable row whose develop button is also its double-click action.
    developRow :: m () -> m (Event t ())
    developRow label = do
        (rowEl, _) <- treeSelect' "workspace" (return never) $ do
            label
            return (never :: Event t ())
        -- double-click opens `nix develop .#…` (no inline button)
        performEvent_ $ ffor (domEvent Dblclick rowEl) $ \_ ->
            liftIO $ developAttr dir attr
        return never
