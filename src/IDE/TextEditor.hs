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
import IDE.TextEditor.Class as Exported
import IDE.TextEditor.GtkSourceView as Exported
import IDE.TextEditor.Yi as Exported
import IDE.TextEditor.CodeMirror as Exported

newDefaultBuffer = newGtkBuffer

#ifndef LEKSAH_WITH_YI
newYiBuffer = newDefaultBuffer
#endif

#ifndef LEKSAH_WITH_CODE_MIRROR
newCMBuffer = newDefaultBuffer
#endif
