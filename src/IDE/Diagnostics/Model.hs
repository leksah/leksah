-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the IDE knows about one compiler\/linter message, and where it
-- lives on disk.  A leaf module: the IDE state stores these, and
-- 'IDE.Diagnostics' produces them, so neither can own the types.
--
-- A 'LogRef' pairs a source 'SrcSpan' with the message text, the log lines
-- it occupies, its severity, and the build root it was reported against —
-- from which the file path resolves three ways: relative to the root (for
-- display), absolute (to open), and remote-aware (an @ssh:\/\/@ root's
-- compiler prints host-local paths).
module IDE.Diagnostics.Model
  ( -- * Severity
    LogRefType(..)
  , isError
  , isBreakpoint
    -- * Where a build ran
  , Log(..)
  , logRootPath
    -- * A message
  , LogRef(..)
  , logRefRootPath
  , logRefFilePath
  , logRefFullFilePath
  , displaySrcSpan
    -- * Log-pane tagging
  , LogTag(..)
#if defined(ghcjs_HOST_OS) || defined(LEKSAH_NO_HLINT)
  , Idea(..)
#endif
  ) where

import Data.Text (Text)
import qualified Data.Text as T (unpack)
import System.FilePath
       ((</>), dropFileName, isAbsolute, makeRelative)

import IDE.Core.Location
       (SrcSpan(..), srcSpanEndColumn, srcSpanEndLine, srcSpanFilename,
        srcSpanStartColumn, srcSpanStartLine)
import IDE.Utils.RemotePath
       (isRemotePath, parseRemotePath, remoteMakeRelative, renderRemotePath)
#if !defined(ghcjs_HOST_OS) && !defined(LEKSAH_NO_HLINT)
import Language.Haskell.HLint (Idea(..))
#endif

-- | How severe a message is.  The order is the gutter's icon priority: a
-- context marker loses to a breakpoint, which loses to an error, and so on.
data LogRefType
    = ContextRef
    | BreakpointRef
    | ErrorRef
    | TestFailureRef
    | WarningRef
    | LintRef
    deriving (Eq, Ord, Show, Enum, Bounded)

isError :: LogRef -> Bool
isError = (== ErrorRef) . logRefType

isBreakpoint :: LogRef -> Bool
isBreakpoint = (== BreakpointRef) . logRefType

-- | What a build was run against — the thing whose directory the compiler's
-- relative paths resolve from.
data Log
    = LogProject { logBasePath     :: FilePath }
    | LogCabal   { logCabalFile    :: FilePath }
    | LogNix     { logNixFile      :: FilePath
                 , logNixAttribute :: Text }
    deriving (Eq, Show)

-- | The directory paths in a build's output are relative to.
logRootPath :: Log -> FilePath
logRootPath = \case
    LogProject { logBasePath = p }  -> p
    LogCabal   { logCabalFile = f } -> dropFileName f
    LogNix     { logNixFile = f }   -> dropFileName f

#if defined(ghcjs_HOST_OS) || defined(LEKSAH_NO_HLINT)
-- | Stand-in for hlint's 'Language.Haskell.HLint.Idea': hlint (via
-- ghc-lib-parser, whose RTS-internals hsc doesn't compile) is unavailable on
-- the JS backend, and is dropped by the no-hlint flag (leksah.sh --ghci,
-- where the RTS linker can't load ghc-lib-parser's static archive).  A
-- 'LogRef' stores one and the hint-resolution check reads 'ideaHint' /
-- 'ideaTo'; nothing more of the real record is used here.
data Idea = Idea { ideaHint :: String, ideaTo :: Maybe String }
    deriving (Eq, Show)
#endif

-- | One message about a piece of source code: where it is, what it says,
-- which build reported it, and (for a lint hint) the replacement it offers.
data LogRef = LogRef
    { logRefSrcSpan  :: SrcSpan
    , logRefLog      :: Log
    , refDescription :: Text
    , logRefIdea     :: Maybe (Text, Idea)
    , logLines       :: Maybe (Int, Int)  -- ^ the lines it occupies in the log
    , logRefType     :: LogRefType
    } deriving (Eq)

instance Show LogRef where
    show lr = T.unpack (refDescription lr) <> displaySrcSpan (logRefSrcSpan lr)

-- | A span the compact way compilers print it: @file:line:col@, widening to
-- @col-col@ or @line:col-line:col@ only when the span really covers more.
displaySrcSpan :: SrcSpan -> String
displaySrcSpan s
    | srcSpanStartLine s /= srcSpanEndLine s =
        prefix <> show (srcSpanStartLine s) <> ":"
               <> show (srcSpanStartColumn s) <> "-" <> show (srcSpanEndColumn s)
    | srcSpanStartColumn s /= srcSpanEndColumn s =
        prefix <> show (srcSpanStartLine s) <> ":"
               <> show (srcSpanStartColumn s) <> "-" <> show (srcSpanEndColumn s)
    | otherwise =
        prefix <> show (srcSpanStartLine s) <> ":" <> show (srcSpanStartColumn s)
  where prefix = srcSpanFilename s <> ":"

-- | The root folder of the package the message references.
logRefRootPath :: LogRef -> FilePath
logRefRootPath = logRootPath . logRefLog

-- | The file the message references, relative to the root path (what the
-- Errors pane shows).  Compilers sometimes print absolute paths — notably
-- stack for a file shared by several components — so relativize those.
logRefFilePath :: LogRef -> FilePath
logRefFilePath lr
    -- Stored ssh:// span (an out-of-root remote file): show it relative to
    -- the (remote) root when possible.
    | isRemotePath f = remoteMakeRelative (logRefRootPath lr) f
    | isAbsolute f   = makeRelative (logRefRootPath lr) f
    | otherwise      = f
  where f = srcSpanFilename (logRefSrcSpan lr)

-- | The file the message references, as a path that can be opened.
logRefFullFilePath :: LogRef -> FilePath
logRefFullFilePath lr
    | isRemotePath f = f
    -- An absolute (host-local) filename under a remote root came from the
    -- remote compiler — re-attach the host.
    | isAbsolute f   = maybe f (\(host, _) -> renderRemotePath host f)
                             (parseRemotePath root)
    | otherwise      = root </> f
  where
    f    = srcSpanFilename (logRefSrcSpan lr)
    root = logRefRootPath lr

-- | How a line is tagged in the Log pane (drives its colour).
data LogTag = LogTag | ErrorTag | FrameTag | InputTag | InfoTag
    deriving (Eq, Ord, Show)
