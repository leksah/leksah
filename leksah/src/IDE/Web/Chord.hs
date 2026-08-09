-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | One keyboard-chord vocabulary for the whole IDE.  A chord is written the
-- VS Code way (@\"cmd+shift+b\"@, @\"ctrl+j\"@, @\"mod+`\"@ — @mod@ is the
-- platform primary modifier: ⌘ on the native front ends, Ctrl when
-- browser-hosted) and parsed ONCE by 'parseChord'; the three consumers each
-- render the same 'Chord' their own way:
--
--   * 'toReflexKey' — the @(modifier set, trigger key)@ the DOM keydown
--     handler matches ('IDE.Web.Widget.Keymap');
--   * 'toNativeSpec' — the @\"cmd+shift+b\"@ spec string the native menu glue
--     parses into an @NSMenuItem@ key equivalent (and its Gtk/Win32 peers);
--   * 'toGlyphs' — the display form (@\"⇧⌘B\"@) for the web menubar and the
--     Shortcuts cheat sheet.
module IDE.Web.Chord
  ( Chord(..)
  , parseChord
  , renderChord
  , toReflexKey
  , toReflexKeys
  , toNativeSpec
  , toGlyphs
  ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.List (nub)
import qualified Data.Set as S (Set, fromList)
import Data.Text (Text)
import qualified Data.Text as T

import Reflex.Dom.Core (Key(..))

-- | A parsed chord: which modifiers, plus the trigger key in canonical
-- spelling (single letters lowercase, digits, punctuation verbatim, named
-- keys capitalised: @Up Down Left Right Enter Space Tab Escape@).
data Chord = Chord
  { chCtrl  :: Bool
  , chAlt   :: Bool
  , chShift :: Bool
  , chCmd   :: Bool
  , chMod   :: Bool  -- ^ the platform-primary alias (@\"mod\"@), kept
                     --   abstract so each front end can resolve it
  , chKey   :: Text
  } deriving (Eq, Ord, Show)

-- | Parse a spec like @\"cmd+shift+b\"@.  A trailing empty part (a spec
-- ending in @+@) means the key itself is @+@.  Returns 'Nothing' on an
-- empty key or an unknown modifier arrangement (anything before the last
-- part that isn't a modifier name).
parseChord :: Text -> Maybe Chord
parseChord spec =
    let parts = T.splitOn "+" spec
        (modParts, keyParts) = splitAt (length parts - 1) parts
        key0 = case keyParts of
            [""] | not (null modParts) -> "+"  -- "cmd++" or trailing +
            [k]  -> k
            _    -> ""
        key = canonicalKey key0
    in if T.null key || not (all isMod modParts)
        then Nothing
        else Just Chord
            { chCtrl  = "ctrl"  `elem` modParts
            , chAlt   = "alt"   `elem` modParts || "opt" `elem` modParts
            , chShift = "shift" `elem` modParts
            , chCmd   = "cmd"   `elem` modParts || "super" `elem` modParts
            , chMod   = "mod"   `elem` modParts
            , chKey   = key
            }
  where
    isMod m = m `elem` (["cmd", "super", "ctrl", "alt", "opt", "shift", "mod"] :: [Text])

canonicalKey :: Text -> Text
canonicalKey k = case T.unpack k of
    [c] | isAsciiUpper c -> T.singleton (toLower c)
    _ -> case T.toLower k of
        "up"     -> "Up"
        "down"   -> "Down"
        "left"   -> "Left"
        "right"  -> "Right"
        "enter"  -> "Enter"
        "return" -> "Enter"
        "space"  -> "Space"
        "tab"    -> "Tab"
        "escape" -> "Escape"
        "esc"    -> "Escape"
        _        -> k

-- | Render back to spec form (the parse's inverse, mod first then
-- cmd/ctrl/alt/shift — the order the existing specs use).
renderChord :: Chord -> Text
renderChord c = T.intercalate "+" $
    [ "mod"   | chMod c ] ++ [ "cmd" | chCmd c ] ++ [ "ctrl" | chCtrl c ] ++
    [ "alt"   | chAlt c ] ++ [ "shift" | chShift c ] ++ [ chKey c ]

-- | The DOM keymap's shape: the exact modifier set the keydown must carry and
-- the trigger 'Key'.  @primaryIsCtrl@ resolves the @mod@ alias (True when
-- browser-hosted).  'Nothing' when the trigger key has no key-code mapping —
-- such a chord can only live as a native menu equivalent.
toReflexKey :: Bool -> Chord -> Maybe (S.Set Key, Key)
toReflexKey primaryIsCtrl c = do
    key <- reflexKey (chKey c)
    let primary = if primaryIsCtrl then Control else Command
    return ( S.fromList $
               [ Control | chCtrl c ] ++ [ Alt | chAlt c ] ++
               [ Shift | chShift c ] ++ [ Command | chCmd c ] ++
               [ primary | chMod c ]
           , key )

-- | Every @(modifier set, trigger key)@ a chord should match — what the DOM
-- keymap actually binds.  One pair normally; TWO for a @mod@ chord on a
-- browser-hosted front end, where ⌘ is accepted alongside the resolved Ctrl.
--
-- Browser-hosted resolves @mod@ to Ctrl because ⌘-chords belong to the
-- browser and the OS.  But leksah looks like a Mac app in the page, and a Mac
-- user reaches for ⌘\` \/ ⌘F \/ ⌘\/ first — so accept both.
--
-- Except where the browser has already spoken for the ⌘ chord and will act on
-- it whatever the page does ('browserReserved'): binding those would run the
-- leksah command AND close the tab \/ open a window.  Ctrl keeps them.
toReflexKeys :: Bool -> Chord -> [(S.Set Key, Key)]
toReflexKeys primaryIsCtrl c
    | primaryIsCtrl, chMod c, not (browserReserved c) = nub (both True <> both False)
    | otherwise                                       = both primaryIsCtrl
  where both p = maybe [] (:[]) (toReflexKey p c)

-- | ⌘-chords a browser handles itself and does not let a page cancel (macOS
-- tab\/window management).  Adding another modifier takes the chord out of
-- that set — ⇧ does not (⇧⌘T reopens a closed tab).
browserReserved :: Chord -> Bool
browserReserved c =
    not (chCtrl c || chAlt c) && chKey c `elem` ["n", "t", "w", "q"]

reflexKey :: Text -> Maybe Key
reflexKey t = case T.unpack t of
    [c] | isAsciiLower c -> lookup c (zip ['a'..'z'] [KeyA ..])
        | isDigit c      -> lookup c (zip ['0'..'9'] [Digit0 ..])
    _ -> case t of
        "`"      -> Just Backquote
        "="      -> Just Equals
        "-"      -> Just Subtract
        "/"      -> Just ForwardSlash
        ","      -> Just Comma
        "."      -> Just Period
        ";"      -> Just Semicolon
        "["      -> Just BracketLeft
        "]"      -> Just BracketRight
        "\\"     -> Just Backslash
        "'"      -> Just Apostrophe
        "Up"     -> Just ArrowUp
        "Down"   -> Just ArrowDown
        "Left"   -> Just ArrowLeft
        "Right"  -> Just ArrowRight
        "Enter"  -> Just Enter
        "Space"  -> Just Space
        "Tab"    -> Just Tab
        "Escape" -> Just Escape
        _        -> Nothing

-- | The spec string the native menu glue parses (@mod@ resolved to @cmd@ —
-- the native front ends are exactly where ⌘ is the primary).
toNativeSpec :: Chord -> Text
toNativeSpec c = T.intercalate "+" $
    [ "cmd"   | chCmd c || chMod c ] ++ [ "ctrl" | chCtrl c ] ++
    [ "alt"   | chAlt c ] ++ [ "shift" | chShift c ] ++ [ chKey c ]

-- | Display glyphs, macOS style and modifier order (⌃⌥⇧⌘).
toGlyphs :: Chord -> Text
toGlyphs c = T.concat $
    [ "⌃" | chCtrl c ] ++ [ "⌥" | chAlt c ] ++ [ "⇧" | chShift c ] ++
    [ "⌘" | chCmd c || chMod c ] ++ [ keyGlyph (chKey c) ]

keyGlyph :: Text -> Text
keyGlyph = \case
    "Up"     -> "↑"
    "Down"   -> "↓"
    "Left"   -> "←"
    "Right"  -> "→"
    "Enter"  -> "⏎"
    "Space"  -> "Space"
    "Tab"    -> "⇥"
    "Escape" -> "⎋"
    k        -> T.toUpper k
