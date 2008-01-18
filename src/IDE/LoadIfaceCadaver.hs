
module IDE.LoadIfaceCadaver (
	findAndReadIface2,
   ) where

import qualified FastString as FS
#define SLIT(x)	 (FS.mkLitString# (x#))


import TcIface( tcIfaceDecl, tcIfaceRules, tcIfaceInst,
				 tcIfaceFamInst, tcIfaceVectInfo )

import DynFlags
import IfaceSyn
import IfaceEnv
import HscTypes

import BasicTypes hiding (SuccessFlag(..))
import TcRnMonad
import Type

import PrelNames
import PrelInfo
import PrelRules
import Rules
import InstEnv
--import FamInstEnv
import Name
import NameEnv
import MkId
import Module
import OccName
import SrcLoc
import Maybes
import ErrUtils
import Finder
import UniqFM
import StaticFlags
import Outputable
import BinIface
import Panic

import Data.List
import Data.Maybe
import Data.IORef

findAndReadIface2 :: SDoc -> Module
		 -> IsBootInterface	-- True  <=> Look for a .hi-boot file
					-- False <=> Look for .hi file
		 -> TcRnIf gbl lcl (MaybeErr Message (ModIface, FilePath))
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed

	-- It *doesn't* add an throwIDE to the monad, because
	-- sometimes it's ok to fail... see notes with loadInterface

findAndReadIface2 doc_str mod hi_boot_file
  = do	{ traceIf (sep [hsep [ptext SLIT("Reading"),
			      if hi_boot_file
				then ptext SLIT("[boot]")
				else empty,
			      ptext SLIT("interface for"),
			      ppr mod <> semi],
		        nest 4 (ptext SLIT("reason:") <+> doc_str)])

	-- Check for GHC.Prim, and return its static interface
	; dflags <- getDOpts
	; if mod == gHC_PRIM
	  then returnM (Succeeded (ghcPrimIface,
				   "<built in interface for GHC.Prim>"))
	  else do

	-- Look for the file
	; hsc_env <- getTopEnv
	; mb_found <- ioToIOEnv (findExactModule hsc_env mod)
	; case mb_found of {

	      err | notFound err -> do
		{ traceIf (ptext SLIT("...not found"))
		; dflags <- getDOpts
		; returnM (Failed (cannotFindInterface dflags
					(moduleName mod) err)) } ;
	      Found loc mod -> do

	-- Found file, so read it
	{ let { file_path = addBootSuffix_maybe hi_boot_file (ml_hi_file loc) }

        ; if thisPackage dflags == modulePackageId mod
                && not (isOneShot (ghcMode dflags))
            then returnM (Failed (homeModError mod loc))
            else do {

        ; traceIf (ptext SLIT("readIFace") <+> text file_path)
	; read_result <- readIface mod file_path hi_boot_file
	; case read_result of
	    Failed err -> returnM (Failed (badIfaceFile file_path err))
	    Succeeded iface
		| mi_module iface /= mod ->
		  return (Failed (wrongIfaceModErr iface mod file_path))
		| otherwise ->
		  returnM (Succeeded (iface, file_path))
			-- Don't forget to fill in the package name...
	}}}}

notFound (Found _ _) = False
notFound _ = True

badIfaceFile file err
  = vcat [ptext SLIT("Bad interface file:") <+> text file,
	  nest 4 err]

hiModuleNameMismatchWarn :: Module -> Module -> Message
hiModuleNameMismatchWarn requested_mod read_mod =
  withPprStyle defaultUserStyle $
    -- we want the Modules below to be qualified with package names,
    -- so reset the PrintUnqualified setting.
    hsep [ ptext SLIT("Something is amiss; requested module ")
	 , ppr requested_mod
	 , ptext SLIT("differs from name found in the interface file")
   	 , ppr read_mod
  	 ]

wrongIfaceModErr iface mod_name file_path
  = sep [ptext SLIT("Interface file") <+> iface_file,
         ptext SLIT("contains module") <+> quotes (ppr (mi_module iface)) <> comma,
         ptext SLIT("but we were expecting module") <+> quotes (ppr mod_name),
	 sep [ptext SLIT("Probable cause: the source code which generated"),
	     nest 2 iface_file,
	     ptext SLIT("has an incompatible module name")
	    ]
	]
  where iface_file = doubleQuotes (text file_path)

homeModError mod location
  = ptext SLIT("attempting to use module ") <> quotes (ppr mod)
    <> (case ml_hs_file location of
           Just file -> space <> parens (text file)
           Nothing   -> empty)
    <+> ptext SLIT("which is not loaded")

