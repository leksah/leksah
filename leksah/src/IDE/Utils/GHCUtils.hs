-- | Pure Cabal-type helpers (Dependency and LibraryName views).  This began
-- as the browser stub of leksah-server's GHC-API-wrapping module of the same
-- name; the GHC-API half is gone, so the stub IS the module now.
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
