-- SPDX-License-Identifier: Apache-2.0

-- | The problems service: one cell holding every known 'Problem', keyed
-- by who reported it.  Producers (a finished build's parse fold, a
-- language server's publish) each own a source key and replace their own
-- slice atomically; consumers (problems pane, editor gutters, status
-- counts) watch the cell — usually through @cellDyn@.
--
-- Source keys are @tool:root@-shaped by convention (@build:\/path@,
-- @lsp:\/path@) so one project's rebuild replaces its old diagnostics
-- without touching another producer's.
module IDE.Problems
  ( Problems
  , newProblems
  , problemsCell
  , setProblems
  , clearSource
  , allProblems
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import IDE.Problems.Types (Problem)
import IDE.Reactive (Cell, modifyCell, newCell, readCell)

newtype Problems = Problems (Cell (Map Text [Problem]))

newProblems :: IO Problems
newProblems = Problems <$> newCell M.empty

-- | The cell itself, for widgets to lift.
problemsCell :: Problems -> Cell (Map Text [Problem])
problemsCell (Problems c) = c

-- | Replace one source's slice (an empty list removes the key, so idle
-- sources cost nothing to enumerate).
setProblems :: Problems -> Text -> [Problem] -> IO ()
setProblems (Problems c) src [] = modifyCell c (M.delete src)
setProblems (Problems c) src ps = modifyCell c (M.insert src ps)

clearSource :: Problems -> Text -> IO ()
clearSource p src = setProblems p src []

allProblems :: Problems -> IO [Problem]
allProblems (Problems c) = concat . M.elems <$> readCell c
