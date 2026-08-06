-- SPDX-License-Identifier: Apache-2.0

-- | What the IDE knows about one problem in the user's code — a compiler
-- error, a warning, a lint suggestion — wherever it came from (a build
-- tool's output, a language server's push).  The shape follows LSP, since
-- that is one producer already and every consumer (problems pane, editor
-- marks, status counts) wants the same fields.
--
-- Positions are ZERO-based, like LSP and unlike what compilers print;
-- parsers convert at the edge, renderers convert back.
module IDE.Problems.Types
  ( Pos(..)
  , Range(..)
  , pointRange
  , Loc(..)
  , Severity(..)
  , Problem(..)
  , problemLoc
  , problemsByPath
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)

-- | A cursor position: 0-based line and column.
data Pos = Pos
    { posLine :: !Int
    , posCol  :: !Int
    } deriving (Eq, Ord, Show)

-- | Half-open span from 'rFrom' (inclusive) to 'rTo' (exclusive).
data Range = Range
    { rFrom :: !Pos
    , rTo   :: !Pos
    } deriving (Eq, Ord, Show)

-- | The empty range at a position — for tools that report only a point.
pointRange :: Pos -> Range
pointRange p = Range p p

-- | A place in a file — what a jump-to-source event carries (a grep hit,
-- a terminal file link, an error's target).
data Loc = Loc
    { locPath  :: !FilePath
    , locRange :: !Range
    } deriving (Eq, Ord, Show)

-- | How bad it is.  Ordered worst-first so sorting a list of problems
-- surfaces errors.
data Severity = SevError | SevWarning | SevHint | SevInfo
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | One problem, as reported.  'pPath' is the path exactly as the tool
-- printed it (possibly relative to the tool's working directory); the
-- consumer that launched the tool knows the root to resolve it against,
-- and only it can — so resolution does not happen here.
data Problem = Problem
    { pPath     :: !FilePath
    , pRange    :: !Range
    , pSeverity :: !Severity
    , pCode     :: !(Maybe Text)  -- ^ e.g. @GHC-83865@, @E0308@, a lint name
    , pMessage  :: !Text          -- ^ full message body, newlines preserved
    , pTool     :: !Text          -- ^ producer tag: @ghc@, @cargo@, @lsp@, …
    } deriving (Eq, Show)

-- | Where a problem points (its path is as-printed; resolve before use).
problemLoc :: Problem -> Loc
problemLoc p = Loc (pPath p) (pRange p)

-- | Group problems for per-file consumers (editor gutters).
problemsByPath :: [Problem] -> Map FilePath [Problem]
problemsByPath = M.fromListWith (flip (++)) . map (\p -> (pPath p, [p]))
