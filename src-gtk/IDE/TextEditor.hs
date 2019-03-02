{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.TextEditor
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.TextEditor (
    module Exported

  , TextEditor(..)
  , EditorBuffer(..)
  , EditorView(..)
  , EditorIter(..)
  , EditorMark(..)
  , EditorTag(..)
  , EditorTagTable(..)

  , newDefaultBuffer
#ifndef LEKSAH_WITH_YI
  , newYiBuffer
#endif
#ifndef LEKSAH_WITH_CODE_MIRROR
  , newCMBuffer
#endif
) where

import Prelude ()
import Prelude.Compat
import Data.Text (Text)
import IDE.TextEditor.Class as Exported
import IDE.TextEditor.GtkSourceView as Exported
import IDE.TextEditor.Yi as Exported
import IDE.TextEditor.CodeMirror as Exported
import IDE.Core.Types (IDEM)

newDefaultBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer GtkSourceView)
newDefaultBuffer = newGtkBuffer

#ifndef LEKSAH_WITH_YI
newYiBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer GtkSourceView)
newYiBuffer = newDefaultBuffer
#endif

#ifndef LEKSAH_WITH_CODE_MIRROR
newCMBuffer :: Maybe FilePath -> Text -> IDEM (EditorBuffer GtkSourceView)
newCMBuffer = newDefaultBuffer
#endif
