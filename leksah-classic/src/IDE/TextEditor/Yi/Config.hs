{-# LANGUAGE CPP #-}
#ifdef LEKSAH_WITH_YI
{-# LANGUAGE OverloadedStrings #-}
#endif
{- Based on example config yi example-configs/yi-vim.hs -}

module IDE.TextEditor.Yi.Config (
    defaultYiConfig
,   Config
,   Control
,   ControlM
,   YiM
,   start
,   runControl
,   liftYi
) where

#ifdef LEKSAH_WITH_YI

import Prelude ()
import Prelude.Compat
import Data.List (reverse, isPrefixOf)

import Yi
import qualified Yi.Keymap.Vim as V2
import qualified Yi.Keymap.Vim.Common as V2
import qualified Yi.Keymap.Vim.Utils as V2

import qualified Yi.Mode.Haskell as Haskell

import qualified Yi.UI.Pango
import Yi.UI.Pango.Control

import Control.Monad (replicateM_)
import Control.Applicative (Alternative(..))
import qualified Data.Text as T (singleton)
import qualified Yi.Rope as R (toText)

start yiConfig f =
    startControl yiConfig $ do
        yiControl <- getControl
        controlIO (f yiControl)

-- Set soft tabs of 4 spaces in width.
prefIndent :: Mode s -> Mode s
prefIndent m = m {
        modeIndentSettings = IndentSettings
            {
                expandTabs = True,
                shiftWidth = 4,
                tabSize = 4
            }}

defaultYiConfig = defaultVimConfig {
    modeTable = myModes ++ modeTable defaultVimConfig,
    defaultKm = myKeymapSet,
    configCheckExternalChangesObsessively = False
}

defaultSearchKeymap :: Keymap
defaultSearchKeymap = do
    Event (KASCII c) [] <- anyEvent
    write (isearchAddE $ T.singleton c)

myKeymapSet :: KeymapSet
myKeymapSet = V2.mkKeymapSet $ V2.defVimConfig `override` \super this ->
    let eval = V2.pureEval this
    in super {
          -- Here we can add custom bindings.
          -- See Yi.Keymap.Vim.Common for datatypes and
          -- Yi.Keymap.Vim.Utils for useful functions like mkStringBindingE

          -- In case of conflict, that is if there exist multiple bindings
          -- whose prereq function returns WholeMatch,
          -- the first such binding is used.
          -- So it's important to have custom bindings first.
          V2.vimBindings = myBindings eval <> V2.vimBindings super
        }

myBindings :: (V2.EventString -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
    let nmap x y = V2.mkStringBindingE V2.Normal V2.Drop (x, y, id)
        imap x y = V2.VimBindingE (\evs state -> case V2.vsMode state of
                                    V2.Insert _ ->
                                        fmap (const (y >> return V2.Continue))
                                             (evs `V2.matchesString` x)
                                    _ -> V2.NoMatch)
    in [ nmap "<C-h>" previousTabE
       , nmap "<C-l>" nextTabE
       , nmap "<C-l>" nextTabE

         -- Press space to clear incremental search highlight
       , nmap " " (eval ":nohlsearch<CR>")

         -- for times when you don't press shift hard enough
       , nmap ";" (eval ":")

       , nmap "<F3>" (withCurrentBuffer deleteTrailingSpaceB)
       , nmap "<F4>" (withCurrentBuffer moveToSol)
       , nmap "<F1>" (withCurrentBuffer readCurrentWordB >>= printMsg . R.toText)
       , imap "<Home>" (withCurrentBuffer moveToSol)
       , imap "<End>" (withCurrentBuffer moveToEol)
       ]

myModes :: [AnyMode]
myModes = [
         AnyMode Haskell.fastMode {
             -- Disable beautification
             modePrettify = const $ return ()
         }
    ]

#else

data Config = Config
data Control = Control
data ControlM a
data YiM a

defaultYiConfig :: Config
defaultYiConfig = Config
start :: Config -> (Control -> IO a) -> IO a
start _yiConfig f = f Control
runControl :: ControlM a -> Control -> IO a
runControl = undefined
liftYi :: YiM a -> ControlM a
liftYi = undefined

#endif

