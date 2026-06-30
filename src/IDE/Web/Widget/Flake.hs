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
-- Only attribute *names* are enumerated (each force guarded by
-- @builtins.tryEval@), so evaluation stays cheap and won't build derivations or
-- force heavy outputs (e.g. nixosConfigurations).
module IDE.Web.Widget.Flake
  ( FlakeNode(..)
  , FlakeResult
  , flakeOutputs
  , flakeCss
  , flakeTreeWidget
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (void)

import Data.Aeson (FromJSON(..), withObject, eitherDecode, (.:))
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack, strip, replace)
import Data.Text.Encoding (encodeUtf8)

import Clay
       (color, grey, red, padding, px, whiteSpace, nowrap, cursor,
        cursorDefault, (?), Css)

import Reflex (ffor, never, Dynamic)
import Reflex.Dom.Core
       (MonadWidget, divClass, el, elClass, dyn, dynText, text)

import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import IDE.Web.Widget.Tree (treeItem)

-- | One node of the flake-outputs tree: a name plus its children (empty for a
-- leaf).  The shape is uniform so it parses straight from the @nix eval@ JSON.
data FlakeNode = FlakeNode
  { fnName     :: Text
  , fnChildren :: [FlakeNode]
  } deriving (Eq, Show)

instance FromJSON FlakeNode where
  parseJSON = withObject "FlakeNode" $ \o ->
      FlakeNode <$> o .: "name" <*> o .: "children"

-- | Either an error message (nix missing, eval/parse failure) or the outputs.
type FlakeResult = Either Text [FlakeNode]

-- | Evaluate the flake in @dir@ and return its output tree.  Runs
-- @nix eval --impure --json@ on an expression that uses @builtins.getFlake@ over
-- a @git+file@ reference and walks the outputs with @attrNames@/@tryEval@.
flakeOutputs :: FilePath -> IO FlakeResult
flakeOutputs dir = either (Left . T.pack . show) id <$>
    (try go :: IO (Either SomeException FlakeResult))
  where
    go = findExecutable "nix" >>= \case
      Nothing  -> return (Left "nix was not found on $PATH")
      Just nix -> do
        (rc, out, err) <- readProcessWithExitCode nix
            [ "--extra-experimental-features", "nix-command flakes"
            , "eval", "--impure", "--json", "--expr", T.unpack (flakeExpr dir) ] ""
        return $ case rc of
          ExitSuccess ->
            case eitherDecode (LBS.fromStrict (encodeUtf8 (T.pack out))) of
              Right ns -> Right ns
              Left e   -> Left (T.pack ("could not parse nix output: " <> e))
          _ -> Left (T.strip (T.pack err))

-- | The nix expression evaluated for @dir@.  @__DIR__@ is replaced with the
-- flake directory; the @git+file@ reference makes nix follow the git tree
-- (including the dirty working tree).
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
  , "  meta = [ \"_type\" \"outPath\" \"outputs\" \"sourceInfo\" \"inputs\" \"narHash\""
  , "           \"lastModified\" \"lastModifiedDate\" \"rev\" \"revCount\" \"shortRev\""
  , "           \"dirtyRev\" \"dirtyShortRev\" \"submodules\" \"original\" \"originalUrl\""
  , "           \"resolved\" \"resolvedUrl\" \"description\" ];"
  , "  outputs = builtins.removeAttrs flake meta;"
  , "  mk = n: c: { name = n; children = c; };"
  , "  leaf = n: mk n [];"
  , "  catNode = c: v: mk c (map leaf (names v));"
  , "in map (c: catNode c (try (outputs.${c})))"
  , "     (builtins.filter (c: c != \"legacyPackages\") (builtins.attrNames outputs))"
  ]

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

-- | Render the flake-outputs tree (display only).  Rebuilt whenever the result
-- changes (i.e. on a refresh / re-evaluation).
flakeTreeWidget :: MonadWidget t m => Dynamic t FlakeResult -> m ()
flakeTreeWidget resultD = void . dyn $ ffor resultD $ \case
    Left err -> divClass "flake-error" $ dynText (pure err)
    Right [] -> divClass "flake-hint"  $ text "No flake outputs."
    Right ns -> el "ul" $ mapM_ (flakeNode 0) ns

-- | One tree node; the top two levels (categories and systems/names) start
-- expanded so the structure is visible at a glance, like @nix flake show@.
flakeNode :: MonadWidget t m => Int -> FlakeNode -> m ()
flakeNode depth (FlakeNode name children)
  | null children = void . elClass "li" "flake-leaf" $ text (" " <> name)
  | otherwise = void $ treeItem "flake-node" (depth < 2)
      (do elClass "span" "flake-label" (text (" " <> name)); return never)
      (el "ul" $ mapM_ (flakeNode (depth + 1)) children >> return never)
