-- | Browser stub of leksah-server's IDE.Utils.GHCUtils (GHC JavaScript
-- backend only; picked over @vendor/leksah-server/src@ by hs-source-dirs
-- order).  The real module wraps the GHC API (unavailable on the JS target);
-- the only pieces the web UI's modules use are these pure Cabal-type
-- helpers, copied verbatim from the real module.
module IDE.Utils.GHCUtils (
    mkDependency
,   viewDependency
,   LibraryName(..)
,   libraryNameToMaybe
,   maybeOrLibraryName
) where

import Distribution.Simple (Dependency(..), PackageName, VersionRange)
import Distribution.Types.LibraryName (LibraryName(..))
import Distribution.Types.UnqualComponentName (UnqualComponentName)
import Distribution.Compat.NonEmptySet (NonEmptySet)

mkDependency :: PackageName -> VersionRange -> NonEmptySet LibraryName -> Dependency
mkDependency = Dependency

viewDependency :: Dependency -> (PackageName, VersionRange, NonEmptySet LibraryName)
viewDependency (Dependency a b c) = (a, b, c)

maybeOrLibraryName :: Maybe UnqualComponentName -> LibraryName
maybeOrLibraryName Nothing = LMainLibName
maybeOrLibraryName (Just n) = LSubLibName n

libraryNameToMaybe :: LibraryName -> Maybe UnqualComponentName
libraryNameToMaybe LMainLibName = Nothing
libraryNameToMaybe (LSubLibName n) = Just n
