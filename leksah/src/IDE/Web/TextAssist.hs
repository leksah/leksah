{-# LANGUAGE OverloadedStrings #-}
-- | The browser's typing "help" — spell checking (red squiggles), macOS
-- autocorrect (which silently rewrites a host name, a branch, a command
-- prefix or an agent's name into an English word), auto-capitalisation and
-- the autofill dropdown — is wrong for every text field leksah has: they take
-- host names, paths, commands, identifiers and short notes to agents.
--
-- 'noTextAssist' turns all of it off; every text field adds it to its
-- attributes.  The page root also defaults @spellcheck@ to off (it is
-- inherited), so a field that forgets is still spared the squiggles — but
-- only the full set stops autocorrect, autocapitalize and autofill everywhere
-- WebKit honours them.
module IDE.Web.TextAssist
  ( noTextAssist
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.String (IsString)
import Data.Text (Text)

-- | For @textInput@'s attribute map ('Text' keys) and @inputElement@'s initial
-- attributes ('Reflex.Dom.AttributeName' keys) alike.
noTextAssist :: (Ord k, IsString k) => Map k Text
noTextAssist = M.fromList
  [ ("spellcheck", "false")
  , ("autocorrect", "off")
  , ("autocapitalize", "off")
  , ("autocomplete", "off") ]
