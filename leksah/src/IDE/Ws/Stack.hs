{-# LANGUAGE OverloadedStrings #-}
-- SPDX-License-Identifier: Apache-2.0

-- | The stack project type.
--
-- Root marker: @stack.yaml@ ('pkFile').  Packages come from its
-- @packages:@ list (defaulting to @[\".\"]@ when the key is absent); each
-- entry is a directory whose @.cabal@ file is parsed by
-- 'IDE.Ws.Cabal.readCabalPackage'.  Only simple path entries are
-- recognised — structured entries (@location:@ forms, extra-dep style) are
-- skipped, as is anything stack itself would fetch rather than find on
-- disk.
module IDE.Ws.Stack
  ( stackProjectType
  , stackYamlPackages
  ) where

import Data.Char (isSpace)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import System.FilePath ((</>), normalise)

import IDE.Ws.Cabal (componentTarget, findCabalFileIn, readCabalPackage)
import IDE.Ws.Types

-- | The @stack@ project type.
stackProjectType :: ProjectType
stackProjectType = ProjectType
  { ptId = "stack"
  , ptDetect = detect
  , ptEnumerate = enumerate
  , ptCommand = command
  }

detect :: Effects -> FilePath -> IO (Maybe ProjectKey)
detect eff dir = do
  let yaml = dir </> "stack.yaml"
  has <- eDoesExist eff yaml
  pure $ if has then Just (ProjectKey "stack" dir (Just yaml)) else Nothing

enumerate :: Effects -> ProjectKey -> IO (Either Text Project)
enumerate eff key = do
  let yaml = maybe (pkRoot key </> "stack.yaml") id (pkFile key)
  mbs <- eReadFile eff yaml
  case mbs of
    Nothing -> pure (Left ("cannot read " <> T.pack yaml))
    Just bs -> do
      let entries = stackYamlPackages
            (TE.decodeUtf8With TEE.lenientDecode bs)
          dirs = nub [normalise (pkRoot key </> e) | e <- entries]
      cabalFiles <- catMaybes <$> mapM (findCabalFileIn eff) dirs
      results <- mapM (readCabalPackage eff) cabalFiles
      pure $ case sequence results of
        Left err   -> Left err
        Right pkgs -> Right (Project key pkgs)

-- | Extract the simple path entries of the top-level @packages:@ key from
-- @stack.yaml@ text (block-style @- path@ list items, or a single inline
-- flow list @[a, b]@).  Absent key defaults to @[\".\"]@.
stackYamlPackages :: Text -> [String]
stackYamlPackages txt = go (T.lines txt)
 where
  go [] = ["."]
  go (l:ls)
    | key == "packages:" = case inline of
        "" -> blockItems ls
        v | "[" `T.isPrefixOf` v -> flowItems v
        v -> [T.unpack (unquote v)]
    | otherwise = go ls
   where
    stripped = stripComment (T.stripEnd l)
    (key, rest) = T.break isSpace stripped
    inline = T.strip rest
  blockItems ls = case items of
    [] -> ["."]
    xs -> xs
   where
    items =
      [ T.unpack (unquote (T.strip (T.drop 1 body)))
      | l <- takeWhile continuation ls
      , let body = T.strip (stripComment l)
      , "-" `T.isPrefixOf` body
      , simpleScalar (T.strip (T.drop 1 body))
      ]
    -- block items may sit at column 0 ("- path") or indented
    continuation l =
      T.null (T.strip l) || isSpace (T.head l)
        || "-" `T.isPrefixOf` l || commentLine l
    commentLine l = "#" `T.isPrefixOf` T.stripStart l
  flowItems v =
    [ T.unpack e'
    | e <- T.split (== ',') (T.dropAround (`elem` ("[]" :: String)) v)
    , let e' = unquote (T.strip e)
    , not (T.null e')
    ]
  -- a plain path, not the start of a nested mapping ("location: ...")
  simpleScalar v = not (T.null v) && not (":" `T.isSuffixOf` v)
                 && not (": " `T.isInfixOf` v)
  unquote v
    | T.length v >= 2, T.head v `elem` juncts, T.last v == T.head v
    = T.init (T.tail v)
    | otherwise = v
   where juncts = "\"'" :: String
  stripComment l = case T.breakOn " #" l of
    (before, rest) | T.null rest -> l
                   | otherwise   -> T.stripEnd before

command :: Verb -> Scope -> Maybe ToolCmd
command verb scope = case verb of
  VClean -> Just (stack ["clean"])
  VBuild -> Just (stack ("build" : targets))
  VRepl  -> Just (stack ("repl" : targets))
  VRun -> case scope of
    ScopeComponent _ c
      | cKind c == KExe -> Just (stack ["run", cName c])
    _ -> Nothing
  VTest -> case scope of
    ScopeComponent _ c
      | cKind c /= KTest -> Nothing
    _ -> Just (stack ("test" : targets))
  VBench -> case scope of
    ScopeComponent _ c
      | cKind c /= KBench -> Nothing
    _ -> Just (stack ("bench" : targets))
 where
  stack args = ToolCmd "stack" args "."
  targets = case scope of
    ScopeProject -> []                        -- stack default: all packages
    ScopePackage p -> [pkgName p]
    ScopeComponent p c -> [componentTarget p c]
