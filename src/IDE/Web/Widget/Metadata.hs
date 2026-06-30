{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A metadata browser, like the GTK \"Modules\" pane: a lazily-expanded tree
-- of packages -> modules -> declarations, built from the collected metadata
-- held in the IDE's workspace/system scope.  Populate it with the toolbar's
-- \"Update Workspace Info\" command.  Clicking a declaration (or a module)
-- navigates to its source location (emitted as a 'MetadataGoto', opened by the
-- editor).
module IDE.Web.Widget.Metadata
  ( metadataCss
  , metadataWidget
  , lookupIdentLocations
  ) where

import Control.Lens ((^.))

import Data.List (sortOn, nub)
import qualified Data.Map as M (fromList, elems, lookup)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set (fromList, notMember)
import Data.Text (Text)
import qualified Data.Text as T (pack, intercalate, takeWhileEnd)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Clay
       (overflow, auto, height, pct, whiteSpace, nowrap, grey, color,
        background, padding, px, (?), Css, Color(..), Cursor(..), cursorDefault)
import Clay.Stylesheet (key)

import Distribution.Text (display)

import Reflex
       (holdUniqDyn, holdDyn, listViewWithKey, never, ffor, switchHold,
        fmapMaybe, ffilter, updated, leftmost, getPostBuild, tag, current,
        Dynamic, Event)
import Reflex.Dom.Core
       (MonadWidget, divClass, el, elDynAttr', dynText, text,
        dyn, domEvent, EventName(..), (=:))

import IDE.Core.CTypes
       (PackageDescr, pdPackage, pdModules, ModuleDescr, mdModuleId,
        mdMbSourcePath, mdIdDescriptions, Descr(..), RealDescr(..),
        TypeDescr(..), dscName, dscMbTypeStr, dscMbModu,
        dscMbLocation, modu, Location(..), SrcSpan(..),
        GenScope(..), PackScope(..), packageIdentifierToString, symLookup)
import IDE.Core.State (IDE, systemInfo, workspaceInfo)
import IDE.Web.Events (MetadataEvents(..), FindbarEvents)
import IDE.Web.Widget.Findbar (findSelection)
import IDE.Web.Widget.Tree (treeItem, treeItem', scrollIntoViewNearest)

metadataCss :: Css
metadataCss = do
    ".metadata" ? do
        height (pct 100)
        overflow auto
        -- The expand/collapse triangles are SVG; give them a visible fill
        -- (only `.workspace` sets it otherwise, so here they'd be black-on-dark).
        key "fill" grey
    ".metadata li" ? do
        whiteSpace nowrap
        cursor cursorDefault
    ".metadata .metadata-descr" ?
        cursor cursorDefault
    -- The module whose source file is the focused editor tab is highlighted.
    ".metadata .metadata-active" ?
        background (Rgba 30 88 209 1.0)
    ".metadata .metadata-hint" ? do
        color grey
        padding (px 8) (px 8) (px 8) (px 8)

-- | The packages to browse, tagged with a sort group so they stay ordered
-- workspace-first: the workspace-scope packages (group 0) on top, then the
-- system-scope packages not already in the workspace (group 1).  Both scopes
-- are populated by metadata collection / the \"Update Workspace Info\" command;
-- showing both at once means local packages no longer hide the system ones (or
-- vice-versa) depending on what has been collected.
metadataPackages :: IDE -> [(Int, PackageDescr)]
metadataPackages ide = workspacePkgs <> systemPkgs
  where
    pkgId = packageIdentifierToString . pdPackage
    workspacePkgs = case ide ^. workspaceInfo of
      Just (GenScopeC (PackScope m _), _) -> map (0,) (M.elems m)
      Nothing                             -> []
    wsIds = Set.fromList (map (pkgId . snd) workspacePkgs)
    systemPkgs = case ide ^. systemInfo of
      Just (GenScopeC (PackScope m _)) ->
        map (1,) . filter ((`Set.notMember` wsIds) . pkgId) $ M.elems m
      Nothing                          -> []

moduleLabel :: ModuleDescr -> Text
moduleLabel = T.pack . display . modu . mdModuleId

-- | How a declaration is labelled in the tree.  Matches the GTK Modules pane's
-- @descrTreeText@: an instance is shown as the class name followed by the types
-- it binds (e.g. @CharParsing WriterT@), so the many @instance CharParsing X@
-- rows don't all collapse to a bare @CharParsing@.  Everything else is its name.
descrLabel :: Descr -> Text
descrLabel (Real (RealDescr id' _ _ _ _ (InstanceDescr binds) _)) =
    case binds of
      [] -> id'
      bs -> id' <> " " <> T.intercalate " " bs
descrLabel d = dscName d

locationToSrcSpan :: Location -> SrcSpan
locationToSrcSpan l =
  SrcSpan (locationFile l) (locationSLine l) (locationSCol l) (locationELine l) (locationECol l)

-- | Look an identifier up in the workspace + system metadata, returning its
-- navigable definitions as (module label, source span).  Used by the terminal's
-- Ctrl/Cmd-click identifier lookup.  Only descriptors that carry a real source
-- location are kept (installed-package descriptors without sources are skipped),
-- and any module qualifier is stripped from the name first (e.g. @Data.Map.lookup@
-- looks up @lookup@).
lookupIdentLocations :: Text -> IDE -> [(Text, SrcSpan)]
lookupIdentLocations name ide = nub
  [ (label d, locationToSrcSpan loc)
  | d <- descrs, Just loc <- [dscMbLocation d] ]
  where
    name'  = T.takeWhileEnd (/= '.') name
    descrs = wsDescrs (ide ^. workspaceInfo) ++ sysDescrs (ide ^. systemInfo)
    wsDescrs (Just (GenScopeC (PackScope _ st1), GenScopeC (PackScope _ st2))) =
      symLookup name' st1 ++ symLookup name' st2
    wsDescrs _ = []
    sysDescrs (Just (GenScopeC (PackScope _ st))) = symLookup name' st
    sysDescrs _ = []
    label d = maybe (dscName d) (T.pack . display . modu) (dscMbModu d)

metadataWidget
  :: forall t m . MonadWidget t m
  => Dynamic t IDE
  -> Dynamic t (Maybe FilePath)   -- ^ the focused file (its module is highlighted)
  -> Dynamic t (Maybe FilePath)   -- ^ the file to reveal (expand/scroll to), or
                                  --   Nothing when its module is already shown
  -> Event t FindbarEvents
  -> m (Event t MetadataEvents)
metadataWidget ide activeFileD revealMetaD findE = divClass "metadata" $ do
  -- Each module's unique key is (group, package id, module name); find selects
  -- by that, so it highlights exactly one module even when several share a
  -- source file.  Reveal still uses the source path (to expand/scroll), and is
  -- event-driven so focusing the pane doesn't re-scroll to an old match.
  -- Modules are listed sorted by label, matching the on-screen order.
  let pkgIdStr = packageIdentifierToString . pdPackage
      allModsD = (\pkgs -> [ ((grp, pkgIdStr pd, moduleLabel md), md)
                           | (grp, pd) <- pkgs, md <- sortOn moduleLabel (pdModules pd) ])
                   . metadataPackages <$> ide
      metaItemsD = map (\(k, md) -> (k, moduleLabel md)) <$> allModsD
      -- Only modules with a real (non-empty) source path can be revealed; the
      -- collector records "" for modules whose source location it couldn't find.
      keyToPathD = (\mods -> M.fromList [ (k, sp) | (k, md) <- mods
                                                  , Just sp <- [mdMbSourcePath md], not (null sp) ])
                     <$> allModsD
  findSelKeyD <- findSelection findE metaItemsD
  let findSelPathD = (\mk m -> mk >>= (`M.lookup` m)) <$> findSelKeyD <*> keyToPathD
  revealMetaD' <- holdDyn Nothing $
    leftmost [updated revealMetaD, Just <$> fmapMaybe id (updated findSelPathD)]
  packagesD <- holdUniqDyn $
    M.fromList . map (\(grp, pd) -> ((grp, pkgIdStr pd), pd)) . metadataPackages <$> ide
  emptyD <- holdUniqDyn $ null <$> packagesD
  switchHold never =<< dyn (ffor emptyD $ \isEmpty ->
    if isEmpty
      then do
        divClass "metadata-hint" $
          text "No metadata loaded. Use the toolbar's \"Update Workspace Info\" to populate."
        return never
      else el "ul" $
        fmapMaybe (listToMaybe . M.elems) <$>
          listViewWithKey packagesD (\pkgKey -> packageNode pkgKey findSelKeyD revealMetaD'))
  where
    packageNode :: (Int, Text) -> Dynamic t (Maybe (Int, Text, Text))
                -> Dynamic t (Maybe FilePath) -> Dynamic t PackageDescr -> m (Event t MetadataEvents)
    packageNode pkgKey findSelKeyD revealMetaD' pkgD = do
      -- Auto-expand the package when the file to reveal is one of its modules.
      let underD = (\pd mf -> maybe False (\f -> any ((== Just f) . mdMbSourcePath) (pdModules pd)) mf)
                     <$> pkgD <*> revealMetaD'
      treeItem' underD "metadata-package" False
        (do dynText (packageIdentifierToString . pdPackage <$> pkgD); return never) $
        el "ul" $ do
          modulesD <- holdUniqDyn $ M.fromList . zip [0 :: Int ..] . sortOn moduleLabel . pdModules <$> pkgD
          fmapMaybe (listToMaybe . M.elems) <$>
            listViewWithKey modulesD (const (moduleNode pkgKey findSelKeyD revealMetaD'))

    moduleNode :: (Int, Text) -> Dynamic t (Maybe (Int, Text, Text))
               -> Dynamic t (Maybe FilePath) -> Dynamic t ModuleDescr -> m (Event t MetadataEvents)
    moduleNode (grp, pid) findSelKeyD revealMetaD' modD =
      treeItem "metadata-module" False
        (do -- Highlight when this module is the focused file's module (by path)
            -- or the unique find selection.  data-reveal-key lets the "already
            -- visible?" check find this module by its source path.
            let attrsD = (\md mf fk ->
                           "class" =: (if maybe False ((== mf) . Just) (mdMbSourcePath md)
                                          || Just (grp, pid, moduleLabel md) == fk
                                         then "metadata-active" else "")
                           <> maybe mempty (("data-reveal-key" =:) . T.pack) (mdMbSourcePath md))
                         <$> modD <*> activeFileD <*> findSelKeyD
                revealMeD = (\md mf -> maybe False ((== mf) . Just) (mdMbSourcePath md))
                              <$> modD <*> revealMetaD'
            (e, _) <- elDynAttr' "span" attrsD $ dynText (moduleLabel <$> modD)
            -- Scroll this module into view when it is the one to reveal.
            pbM <- getPostBuild
            scrollIntoViewNearest (ffilter id $ leftmost [updated revealMeD, tag (current revealMeD) pbM]) e
            return $ fmapMaybe id $ tag (current (moduleGoto <$> modD)) (domEvent Click e)) $
        el "ul" $ do
          descrsD <- holdUniqDyn $ M.fromList . zip [0 :: Int ..] . sortOn dscName . mdIdDescriptions <$> modD
          fmapMaybe (listToMaybe . M.elems) <$> listViewWithKey descrsD (const descrNode)

    descrNode :: Dynamic t Descr -> m (Event t MetadataEvents)
    descrNode dD = do
      let attrsD = ffor dD $ \d -> "class" =: "metadata-descr"
            <> maybe mempty (("title" =:) . decodeUtf8With lenientDecode) (dscMbTypeStr d)
      (e, _) <- elDynAttr' "li" attrsD $ dynText (descrLabel <$> dD)
      return $ fmapMaybe id $ tag (current (descrGoto <$> dD)) (domEvent Click e)

    descrGoto :: Descr -> Maybe MetadataEvents
    descrGoto d = MetadataGoto . locationToSrcSpan <$> dscMbLocation d

    moduleGoto :: ModuleDescr -> Maybe MetadataEvents
    moduleGoto md = (\p -> MetadataGoto (SrcSpan p 1 0 1 0)) <$> mdMbSourcePath md
