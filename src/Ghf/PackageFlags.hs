-----------------------------------------------------------------------------
--
-- Module      :  Ghf.PackageFlags
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
--
-- | Module for saving, restoring and editing projectFlags
--
---------------------------------------------------------------------------------


module Ghf.PackageFlags (
    readFlags
,   writeFlags
,   editFlags
) where

import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk
import Control.Monad.Reader
import qualified Text.ParserCombinators.Parsec as P
import Data.IORef
import Data.List
import System.IO
import qualified Text.PrettyPrint.HughesPJ as PP

import Ghf.Core.State
import GUI.Ghf.EditorBasics
import GUI.Ghf.MakeEditor hiding (fieldEditor, parameters)
import GUI.Ghf.SimpleEditors
import GUI.Ghf.CompositeEditors
import GUI.Ghf.Parameters

import Ghf.SourceEditor
import Ghf.PrinterParser hiding (fieldParser, parameters)
import Ghf.File
import Ghf.ViewFrame
import Ghf.DescriptionPP

concatString :: [String] -> String
concatString l = foldl (\r s -> if null r then s else r ++ " " ++ s) "" l

flagsDescription :: [(String,[FieldDescriptionPP GhfPackage])]
flagsDescription = [
    ("Flags", [
        mkFieldPP
            (paraName <<<- ParaName "Config flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (configFlags p))
            (\ b a -> a{configFlags = if null b then [] else [b]})
            stringEditor
            (\ _ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Build flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (buildFlags p))
            (\ b a -> a{buildFlags = if null b then [] else [b]})
            stringEditor
            (\ _ ->  return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Haddock flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (haddockFlags p))
            (\ b a -> a{haddockFlags = if null b then [] else [b]})
            stringEditor
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Executable flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (exeFlags p))
            (\ b a -> a{exeFlags = if null b then [] else [b]})
            stringEditor
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Install flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (installFlags p))
            (\ b a -> a{installFlags = if null b then [] else [b]})
            stringEditor
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Register flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (registerFlags p))
            (\ b a -> a{registerFlags = if null b then [] else [b]})
            stringEditor
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Unregister flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (unregisterFlags p))
            (\ b a -> a{unregisterFlags = if null b then [] else [b]})
            stringEditor
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Source Distribution flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (sdistFlags p))
            (\ b a -> a{sdistFlags = if null b then [] else [b]})
            stringEditor
            (\ _ -> return ())
    ])]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readFlags :: FilePath -> GhfPackage -> IO GhfPackage
readFlags fn pack = do
    res <- P.parseFromFile (flagsParser pack (concatMap snd flagsDescription)) fn
    case res of
        Left pe -> error $"Error reading flags file " ++ show fn ++ " " ++ show pe
        Right r -> return r

flagsParser ::  a ->  [FieldDescriptionPP a] ->  P.CharParser () a
flagsParser def descriptions =
    let parsersF = map fieldParser descriptions in do
        whiteSpace
        res <-  applyFieldParsers def parsersF
        return res
        P.<?> "flags parser"

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writeFlags :: FilePath -> GhfPackage -> IO ()
writeFlags fpath flags =
    writeFile fpath (showFlags flags (concatMap snd flagsDescription))

showFlags ::  a ->  [FieldDescriptionPP a] ->  String
showFlags flags flagsDesc = PP.render $
    foldl (\ doc (FDPP _ printer _ _ _ ) ->  doc PP.$+$ printer flags) PP.empty flagsDesc

-- ------------------------------------------------------------
-- * Editing
-- ------------------------------------------------------------

editFlags :: GhfAction
editFlags = do
    ghfR <- ask
    mbP <- readGhf activePack
    case mbP of
        Nothing -> return ()
        Just p -> lift $editFlags' p flagsDescription ghfR


editFlags' :: GhfPackage -> [(String,[FieldDescriptionPP GhfPackage])] -> GhfRef -> IO ()
editFlags' flags flagsDesc ghfR  = do
    let flatflagsDesc = concatMap snd flagsDesc
    dialog  <- windowNew
    vb      <- vBoxNew False 0
    bb      <- hButtonBoxNew
    ok      <- buttonNewFromStock "gtk-ok"
    cancel  <- buttonNewFromStock "gtk-cancel"
    boxPackStart bb ok PackNatural 0
    boxPackStart bb cancel PackNatural 0
    nb <- newNotebook
    notebookSetTabPos nb PosTop
    res <- mapM
        (\ (tabLabel, partflagsDesc) -> do
            resList <- mapM (\ fd -> (fieldEditor fd) flags) partflagsDesc
            let (widgetsP, setInjsP, getExtsP,notifiersP) = unzip4 resList
            nbbox <- vBoxNew False 0
            mapM_ (\ w -> boxPackStart nbbox w PackNatural 0) widgetsP
            sw <- scrolledWindowNew Nothing Nothing
            scrolledWindowAddWithViewport sw nbbox
            scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
            notebookAppendPage nb sw tabLabel
            return (widgetsP, setInjsP, getExtsP, notifiersP))
                flagsDesc
    let (widgets, setInjs, getExts, notifiers) =
            foldl (\ (w,i,e,n) (w2,i2,e2,n2) -> (w ++ w2, i ++ i2, e ++ e2, n ++ n2)) ([],[],[],[]) res
    let fieldNames = map (\fd -> case getParameterPrim paraName (parameters fd) of
                                        Just s -> s
                                        Nothing -> "Unnamed") flatflagsDesc
    ok `onClicked` (do
        mbNewflags <- extractAndValidate flags getExts fieldNames
        case mbNewflags of
            Nothing -> return ()
            Just newflags -> do
                runReaderT (modifyGhf_ (\ghf -> return (ghf{activePack = Just newflags}))) ghfR
                widgetDestroy dialog
                mainQuit)
    cancel `onClicked` (do
        widgetDestroy dialog)
    boxPackStart vb nb PackGrow 7
    boxPackEnd vb bb PackNatural 7
    containerAdd dialog vb
    widgetSetSizeRequest dialog 500 700
    widgetShowAll dialog
    return ()







