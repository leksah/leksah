-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DeriveGeneric #-}

-- | Source locations and package/module identities — the small vocabulary the
-- IDE uses to point at code (error refs, grep hits, editor jumps, LSP
-- positions).  Fresh definitions replacing the parts of leksah-server's
-- @IDE.Core.CTypes@ the web UI still used; the field names are the API the
-- rest of the IDE is written against.  The whole module is superseded by
-- @IDE.Diagnostics@ when the diagnostics model is rewritten.
module IDE.Core.Location
  ( SrcSpan(..)
  , Location(..)
  , PackModule(..)
  , packageIdentifierToString
  , packageIdentifierFromString
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Distribution.ModuleName (ModuleName)
import Distribution.Package (PackageIdentifier)
import Distribution.Pretty (prettyShow)
import Distribution.Text (simpleParse)

-- | A span of source: file plus 1-based start/end line and column.
data SrcSpan = SrcSpan
    { srcSpanFilename    :: FilePath
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
  deriving (Eq, Ord, Show, Generic)

-- | Like 'SrcSpan' but relative to a module rather than naming the file
-- (an editor jump target).
data Location = Location
    { locationFile  :: FilePath
    , locationSLine :: Int
    , locationSCol  :: Int
    , locationELine :: Int
    , locationECol  :: Int
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | A module within a specific package version.
data PackModule = PM
    { pack :: PackageIdentifier
    , modu :: ModuleName
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Render a package identifier the way cabal prints it (@name-1.2.3@).
packageIdentifierToString :: PackageIdentifier -> Text
packageIdentifierToString = T.pack . prettyShow

-- | Parse what 'packageIdentifierToString' renders.
packageIdentifierFromString :: Text -> Maybe PackageIdentifier
packageIdentifierFromString = simpleParse . T.unpack
