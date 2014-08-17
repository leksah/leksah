{-# LANGUAGE CPP #-}
{- Based on example config Yi/Users/Corey.hs that uses the Vim keymap with these additions:
    - Always uses the VTY UI by default.
    - The color style is darkBlueTheme
    - The insert mode of the Vim keymap has been extended with a few additions
      I find useful.
 -}

module IDE.YiConfig (
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

noHaskellAnnots m
    | modeName m == "haskell" = m { modeGetAnnotations = modeGetAnnotations emptyMode }
    | otherwise = m

defaultYiConfig = defaultVimConfig {
    modeTable = myModes ++ modeTable defaultVimConfig,
    defaultKm = myKeymapSet,
    configCheckExternalChangesObsessively = False
}

defaultSearchKeymap :: Keymap
defaultSearchKeymap = do
    Event (KASCII c) [] <- anyEvent
    write (isearchAddE [c])

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
          V2.vimBindings = myBindings eval ++ V2.vimBindings super
        }

myBindings :: (String -> EditorM ()) -> [V2.VimBinding]
myBindings eval =
    let nmap x y = V2.mkStringBindingE V2.Normal V2.Drop (x, y, id)
        imap x y = V2.VimBindingE (\evs state -> case V2.vsMode state of
                                    V2.Insert _ ->
                                        fmap (const (y >> return V2.Continue))
                                             (evs `V2.matchesString` x)
                                    _ -> V2.NoMatch)
    in [
         -- Tab traversal
         nmap "<C-h>" previousTabE
       , nmap "<C-l>" nextTabE
       , nmap "<C-l>" nextTabE

         -- Press space to clear incremental search highlight
       , nmap " " (eval ":nohlsearch<CR>")

         -- for times when you don't press shift hard enough
       , nmap ";" (eval ":")

       , nmap "<F3>" (withBuffer0 deleteTrailingSpaceB)
       , nmap "<F4>" (withBuffer0 moveToSol)
       , nmap "<F1>" (withBuffer0 readCurrentWordB >>= printMsg)

       , imap "<Home>" (withBuffer0 moveToSol)
       , imap "<End>" (withBuffer0 moveToEol)
       ]

myModes = [
         AnyMode Haskell.fastMode {
             -- Disable beautification
             modePrettify = const $ return ()
           , modeGetAnnotations = (const . const) []
         }
    ]

#else

data Config = Config
data Control = Control
data ControlM a = ControlM
data YiM a = YiM

defaultYiConfig :: Config
defaultYiConfig = Config
start :: Config -> (Control -> IO a) -> IO a
start yiConfig f = f Control
runControl :: ControlM a -> Control -> IO a
runControl = undefined
liftYi :: YiM a -> ControlM a
liftYi = undefined

#endif

