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
    module IDE.TextEditor.Class
  , module IDE.TextEditor.GtkSourceView
  , module IDE.TextEditor.Yi
  , module IDE.TextEditor.CodeMirror

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

import IDE.TextEditor.Class
import IDE.TextEditor.GtkSourceView
import IDE.TextEditor.Yi
import IDE.TextEditor.CodeMirror

newDefaultBuffer = newGtkBuffer

#ifndef LEKSAH_WITH_YI
newYiBuffer = newDefaultBuffer
#endif

#ifndef LEKSAH_WITH_CODE_MIRROR
newCMBuffer = newDefaultBuffer
#endif
