{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.Utils
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

module IDE.Utils.Utils where

leksahSessionFileExtension           = ".lkshs"
leksahWorkspaceFileExtension         = ".lkshw"
leksahPreferencesFileExtension       = ".lkshp"
leksahCandyFileExtension             = ".lkshc"
leksahKeymapFileExtension            = ".lkshk"
leksahSourcesFileExtension           = ".lksho"
leksahMetadataSystemFileExtension    = ".lkshm"
leksahMetadataPathFileExtension      = ".lkshp"
leksahMetadataWorkspaceFileExtension = ".lkshe"
leksahMetadataDebugExtension         = ".lkshd"
leksahTemplateFileExtension          = ".lksht"
leksahFlagFileExtension              = ".lkshf"

standardSessionFilename              =   "current" ++ leksahSessionFileExtension
emptySessionFilename                 =   "empty" ++ leksahSessionFileExtension
packageSessionFilename               =   "leksah" ++ leksahSessionFileExtension
standardKeymapFilename               =   "keymap" ++ leksahKeymapFileExtension
standardCandyFilename                =   "candy" ++ leksahCandyFileExtension
standardPreferencesFilename          =   "prefs" ++ leksahPreferencesFileExtension
strippedPreferencesFilename          =   "prefscoll" ++ leksahPreferencesFileExtension
standardSourcesFilename              =   "sources" ++ leksahSourcesFileExtension
standardModuleTemplateFilename       =   "module" ++ leksahTemplateFileExtension




