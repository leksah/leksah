-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-rebindable keyboard shortcuts, VS Code style.
--
-- Every bindable action has a stable command id in the
-- 'IDE.Web.Commands.allCommands' registry ('CommandSpec' here, so the two
-- modules don't cycle).  'defaultKeybindings' is the shipped table;
-- @~\/.config\/leksah\/keybindings.json@ holds an array of
-- @{\"key\", \"command\", \"args\"?}@ rules that APPEND to it — a later rule
-- for the same chord wins, and a rule whose command starts with @-@ removes
-- the matching earlier binding:
--
-- > [ { "key": "cmd+shift+m", "command": "package.build" }
-- > , { "key": "ctrl+shift+b", "command": "-package.build" }
-- > , { "key": "cmd+8", "command": "nav.split", "args": 8 } ]
--
-- The resolved 'Keymap' is the single source every surface renders from: the
-- DOM keydown handler, the native menu key equivalents, the web menubar
-- hints, and the Shortcuts cheat sheet.  'loadKeybindings' re-reads the file
-- and notifies the registered listeners (the reflex network and the native
-- menu rebuild), so @edit.reloadKeybindings@ — or the fsnotify watch on the
-- file — applies changes live.
module IDE.Web.Keybindings
  ( When(..)
  , CommandSpec(..)
  , nullarySpec
  , Binding(..)
  , Keymap
  , KeyRule(..)
  , defaultKeybindings
  , keybindingsFilePath
  , readKeyRules
  , resolveKeymap
  , loadKeybindings
  , currentKeymap
  , registerKeymapListener
  , bindingFor
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
       ((.=), (.:), (.:?), FromJSON(..), ToJSON(..), eitherDecodeStrict',
        object, withObject)
import qualified Data.ByteString as BS
import Data.IORef
       (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (XdgDirectory(..), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

import IDE.Web.Chord (Chord, parseChord)
import IDE.Web.Command (Command)

-- | When a binding is live.  'WhenTerminal' and 'WhenTerminalOrConvertible'
-- chords exist only as (gated) native menu key equivalents — the DOM handler
-- binds only 'WhenAlways' chords, exactly as before.
data When
  = WhenAlways
  | WhenTerminal              -- ^ a terminal tab is active
  | WhenTerminalOrConvertible -- ^ … or a tab convertible to a tmux pane
  deriving (Eq, Show)

-- | One bindable command: its stable id (@\"package.build\"@), the label the
-- menus (and the Shortcuts pane, as a fallback) show, when its chord is
-- live, and how to make the runnable 'Command' from the binding's optional
-- integer argument.
data CommandSpec = CommandSpec
  { csId    :: Text
  , csTitle :: Text
  , csWhen  :: When
  , csMake  :: Maybe Int -> Maybe Command
  }

-- | A spec for the common case: no argument.
nullarySpec :: Text -> Text -> When -> Command -> CommandSpec
nullarySpec i title w cmd = CommandSpec i title w $ \case
    Nothing -> Just cmd
    Just _  -> Nothing

-- | A resolved binding: the chord, the argument, and the command spec it
-- dispatches (embedding the spec keeps consumers free of registry lookups).
data Binding = Binding
  { bChord :: Chord
  , bArgs  :: Maybe Int
  , bSpec  :: CommandSpec
  }

-- | Resolution order matters: later bindings win a chord, and the LAST
-- binding of a command is what the menus display (so user rules override).
type Keymap = [Binding]

-- | One line of @keybindings.json@ (and of 'defaultKeybindings').  @when@ is
-- accepted for forward compatibility but ignored — gating comes from the
-- command itself.
data KeyRule = KeyRule
  { krKey     :: Text
  , krCommand :: Text
  , krArgs    :: Maybe Int
  } deriving (Eq, Show)

instance FromJSON KeyRule where
    parseJSON = withObject "keybinding" $ \o -> KeyRule
        <$> o .: "key"
        <*> o .: "command"
        <*> o .:? "args"

instance ToJSON KeyRule where
    toJSON r = object $
        [ "key" .= krKey r, "command" .= krCommand r ]
        <> maybe [] (\a -> [ "args" .= a ]) (krArgs r)

-- | The shipped bindings (the successor of the old @globalBindings@ table,
-- the menu key-equivalent specs and the generated navigation chords).
defaultKeybindings :: [KeyRule]
defaultKeybindings =
    [ r "mod+n"            "workspace.newWindow"
    , r "mod+w"            "workspace.closeFile"
    , r "mod+f"            "edit.find"
    , r "mod+/"            "edit.showShortcuts"
    , r "mod+,"            "edit.showPreferences"
    -- Build: mod+shift+B matching VS Code (plain Ctrl+B is the tmux prefix);
    -- Ctrl+Shift+B additionally works on macOS, as before.  The primary
    -- chord comes LAST — the menus display a command's last binding.
    , r "ctrl+shift+b"     "package.build"
    , r "mod+shift+b"      "package.build"
    , r "ctrl+j"           "errors.next"
    , r "ctrl+shift+j"     "errors.previous"
    -- The tab flipper: step with mod+`, commit on the modifier's release.
    , r "mod+`"            "view.nextTab"
    , r "mod+shift+`"      "view.previousTab"
    , r "cmd+="            "view.fontBigger"
    , r "cmd+-"            "view.fontSmaller"
    , r "cmd+0"            "view.fontReset"
    , r "cmd+ctrl+b"       "view.newBrowserPane"
    -- Terminal (native-menu key equivalents, gated by the command's when).
    , r "cmd+shift+t"      "terminal.newWindow"
    , r "cmd+shift+["      "terminal.previousWindow"
    , r "cmd+shift+]"      "terminal.nextWindow"
    , r "cmd+d"            "terminal.splitRight"
    , r "cmd+shift+d"      "terminal.splitDown"
    , r "cmd+alt+Up"       "terminal.selectSplitAbove"
    , r "cmd+alt+Down"     "terminal.selectSplitBelow"
    , r "cmd+alt+Left"     "terminal.selectSplitLeft"
    , r "cmd+alt+Right"    "terminal.selectSplitRight"
    , r "cmd+["            "terminal.selectPreviousSplit"
    , r "cmd+]"            "terminal.selectNextSplit"
    , r "cmd+ctrl+="       "terminal.equalizeSplits"
    , r "cmd+ctrl+Up"      "terminal.moveDividerUp"
    , r "cmd+ctrl+Down"    "terminal.moveDividerDown"
    , r "cmd+ctrl+Left"    "terminal.moveDividerLeft"
    , r "cmd+ctrl+Right"   "terminal.moveDividerRight"
    , r "cmd+shift+Enter"  "terminal.zoomSplit"
    , r "cmd+alt+y"        "terminal.togglePaneTransparency"
    , r "cmd+alt+u"        "terminal.snapWindowToPane"
    , r "ctrl+alt+a"       "terminal.focusAlerting"
    -- AI
    , r "cmd+ctrl+s"       "ai.sendSelection"
    , r "cmd+ctrl+r"       "ai.sendFileRef"
    , r "cmd+ctrl+e"       "ai.sendError"
    , r "cmd+ctrl+j"       "ai.focusTerminal"
    , r "cmd+ctrl+g"       "ai.grabRegion"
    , r "cmd+ctrl+c"       "ai.newClaudeSession"
    , r "cmd+ctrl+k"       "ai.continueClaudeSession"
    ]
    -- Numbered navigation (terminal-app style): ⌘1…9 the active terminal's
    -- Nth split, ⌥⌘1…9 the Nth side-bar pane, ⌃⌘1…9 the Nth bottom-bar pane.
    <> concat
    [ [ KeyRule ("cmd+" <> d)      "nav.split"      (Just n)
      , KeyRule ("cmd+alt+" <> d)  "nav.sidePane"   (Just n)
      , KeyRule ("cmd+ctrl+" <> d) "nav.bottomPane" (Just n) ]
    | (n, d) <- zip [1 :: Int ..] (map (T.pack . show) [1 :: Int .. 9]) ]
  where
    r k c = KeyRule k c Nothing

-- | @~\/.config\/leksah\/keybindings.json@ (beside @settings.json@).
keybindingsFilePath :: IO FilePath
keybindingsFilePath = (</> "keybindings.json") <$> getXdgDirectory XdgConfig "leksah"

-- | The user's rules.  A missing file is no rules; an unreadable or
-- unparsable one is no rules plus an error message to surface.
readKeyRules :: IO ([KeyRule], Maybe Text)
readKeyRules = do
    fp <- keybindingsFilePath
    doesFileExist fp >>= \case
        False -> return ([], Nothing)
        True -> try (BS.readFile fp) >>= \case
            Left (e :: SomeException) -> return ([], Just (T.pack (show e)))
            Right bytes -> case eitherDecodeStrict' bytes of
                Left err -> return ([], Just (T.pack fp <> ": " <> T.pack err))
                Right rs -> return (rs, Nothing)

-- | Resolve defaults + user rules against the command registry.  Returns the
-- keymap and human-readable warnings (unknown command, unparsable chord).
resolveKeymap :: [CommandSpec] -> [KeyRule] -> (Keymap, [Text])
resolveKeymap registry rules = foldl' step ([], []) rules
  where
    step (km, warns) rule = case T.stripPrefix "-" (krCommand rule) of
        -- A removal: drop earlier bindings of that command on that chord.
        Just cid -> case parseChord (krKey rule) of
            Nothing -> (km, warns <> [badKey rule])
            Just ch -> ( filter (\b -> not (csId (bSpec b) == cid && bChord b == ch)) km
                       , warns )
        Nothing -> case parseChord (krKey rule) of
            Nothing -> (km, warns <> [badKey rule])
            Just ch -> case find ((== krCommand rule) . csId) registry of
                Nothing -> (km, warns <> [ "keybindings: unknown command \""
                                           <> krCommand rule <> "\"" ])
                Just spec -> case csMake spec (krArgs rule) of
                    Nothing -> (km, warns <> [ "keybindings: bad args for \""
                                               <> krCommand rule <> "\"" ])
                    Just _  -> (km <> [Binding ch (krArgs rule) spec], warns)
    badKey rule = "keybindings: cannot parse key \"" <> krKey rule <> "\""

-- | The binding the menus display for a command: the LAST one in the keymap
-- (user rules come after the defaults, so a rebind wins).
bindingFor :: Keymap -> Text -> Maybe Binding
bindingFor km cid =
    case [ b | b <- km, csId (bSpec b) == cid ] of
        [] -> Nothing
        bs -> Just (last bs)

-- ---------------------------------------------------------------------
-- The live table + its listeners (native menu rebuild, reflex networks)
-- ---------------------------------------------------------------------

{-# NOINLINE keymapRef #-}
keymapRef :: IORef Keymap
keymapRef = unsafePerformIO (newIORef [])

{-# NOINLINE listenersRef #-}
listenersRef :: IORef [Keymap -> IO ()]
listenersRef = unsafePerformIO (newIORef [])

-- | Read the file, resolve against @registry@, store the result and notify
-- every listener.  Returns the warnings (file error first, if any) for the
-- caller to log.
loadKeybindings :: [CommandSpec] -> IO (Keymap, [Text])
loadKeybindings registry = do
    (rules, mbErr) <- readKeyRules
    let (km, warns) = resolveKeymap registry (defaultKeybindings <> rules)
        allWarns = maybe [] (\e -> ["Error reading keybindings: " <> e]) mbErr
                   <> warns
    writeIORef keymapRef km
    listeners <- readIORef listenersRef
    forM_ listeners ($ km)
    return (km, allWarns)

currentKeymap :: IO Keymap
currentKeymap = readIORef keymapRef

-- | Subscribe to keymap changes; fires immediately with the current table so
-- late registrants start in sync.
registerKeymapListener :: (Keymap -> IO ()) -> IO ()
registerKeymapListener f = do
    atomicModifyIORef' listenersRef (\ls -> (ls <> [f], ()))
    readIORef keymapRef >>= f
