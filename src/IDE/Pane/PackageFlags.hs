{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Pane.PackageFlags
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Module for saving, restoring and editing projectFlags
--
---------------------------------------------------------------------------------


module IDE.Pane.PackageFlags (
    readFlags
,   writeFlags
,   getFlags
,   IDEFlags(..)
,   FlagsState
) where

import Graphics.UI.Gtk
import Control.Monad.Reader
import qualified Text.ParserCombinators.Parsec as P
import Data.List
import System.IO
import qualified Text.PrettyPrint.HughesPJ as PP
import Data.Typeable
import System.FilePath.Posix

import IDE.Core.State
import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Simple
import Graphics.UI.Editor.Parameters

import IDE.PrinterParser hiding (fieldParser,parameters)

import Control.Event (registerEvent)
import IDE.DescriptionPP (extractFieldDescription,FieldDescriptionPP(..),
    mkFieldPP,flattenFieldDescriptionPP)

data IDEFlags               =   IDEFlags {
    flagsBox                ::   VBox
} deriving Typeable

data FlagsState             =   FlagsState
    deriving(Eq,Ord,Read,Show,Typeable)

instance IDEObject IDEFlags

instance Pane IDEFlags IDEM
    where
    primPaneName _  =   "Package Flags"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . flagsBox
    paneId b        =   "*Flags"
    makeActive prefs =  activatePane prefs []
    close           =   closePane

instance RecoverablePane IDEFlags FlagsState IDEM where
    saveState p     =   do
        mbFlags :: Maybe IDEFlags <- getPane
        case mbFlags of
            Nothing ->  return Nothing
            Just p  ->  return (Just FlagsState)
    recoverState pp st  =  do
            mbPack  <- readIDE activePack
            case mbPack of
                Just pack -> do
                    nb          <-  getNotebook pp
                    initFlags pack pp nb
                Nothing -> return ()


concatString :: [String] -> String
concatString l = foldl' (\r s -> if null r then s else r ++ " " ++ s) "" l

flatFlagsDescription :: [FieldDescriptionPP IDEPackage]
flatFlagsDescription = flattenFieldDescriptionPP flagsDescription

flagsDescription :: FieldDescriptionPP IDEPackage
flagsDescription = VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName "Config flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (configFlags p))
            (\ b a -> a{configFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Build flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (buildFlags p))
            (\ b a -> a{buildFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ ->  return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Haddock flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (haddockFlags p))
            (\ b a -> a{haddockFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Executable flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (exeFlags p))
            (\ b a -> a{exeFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Install flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (installFlags p))
            (\ b a -> a{installFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Register flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (registerFlags p))
            (\ b a -> a{registerFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Unregister flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (unregisterFlags p))
            (\ b a -> a{unregisterFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Source Distribution flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> concatString (sdistFlags p))
            (\ b a -> a{sdistFlags = if null b then [] else [b]})
            (stringEditor (const True))
            (\ _ -> return ())]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readFlags :: FilePath -> IDEPackage -> IO IDEPackage
readFlags fn pack = do
    res <- P.parseFromFile (flagsParser pack flatFlagsDescription) fn
    case res of
        Left pe -> throwIDE $"Error reading flags file " ++ show fn ++ " " ++ show pe
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

writeFlags :: FilePath -> IDEPackage -> IO ()
writeFlags fpath flags =
    writeFile fpath (showFlags flags flatFlagsDescription)

showFlags ::  a ->  [FieldDescriptionPP a] ->  String
showFlags flags flagsDesc = PP.render $
    foldl' (\ doc (FDPP _ printer _ _ _ ) ->  doc PP.$+$ printer flags) PP.empty flagsDesc

-- ------------------------------------------------------------
-- * Editing
-- ------------------------------------------------------------

getFlags :: IDEM (Maybe IDEFlags)
getFlags = do
    mbFlags <- getPane
    case mbFlags of
        Nothing -> do
            mbPack      <-  readIDE activePack
            case mbPack of
                Nothing -> do
                    ideMessage Normal "can't edit flags without active package"
                    return Nothing
                Just pack -> do
                    prefs       <-  readIDE prefs
                    layout      <-  readIDE layout
                    let pp      =   getStandardPanePath (sourcePanePath prefs) layout
                    nb          <-  getNotebook pp
                    initFlags pack pp nb
                    mbFlags <- getPane
                    case mbFlags of
                        Nothing ->  throwIDE "Can't init flags pane"
                        Just m  ->  do
                                liftIO $bringPaneToFront m
                                return (Just m)
        Just m ->   do
            liftIO $bringPaneToFront m
            return (Just m)

initFlags :: IDEPackage -> PanePath -> Notebook -> IDEAction
initFlags idePackage panePath nb = do
    let flagsDesc       =   extractFieldDescription flagsDescription
    let flatflagsDesc   =   flattenFieldDescription flagsDesc
    (buf,cids)  <-  reifyIDE $ \ ideR ->  do
        vb                  <-  vBoxNew False 0
        let flagsPane = IDEFlags vb
        bb                  <-  hButtonBoxNew
        ok                  <-  buttonNewFromStock "gtk-ok"
        closeB              <-  buttonNewFromStock "gtk-close"
        boxPackStart bb ok PackNatural 0
        boxPackStart bb closeB PackNatural 0
        (widget,injb,ext,notifier)
                            <-  buildEditor flagsDesc idePackage
        sw <- scrolledWindowNew Nothing Nothing
        scrolledWindowAddWithViewport sw widget
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        ok `onClicked` (do
            mbPackWithNewFlags <- extract idePackage [ext]
            case mbPackWithNewFlags of
                Nothing -> return ()
                Just packWithNewFlags -> do
                    reflectIDE (do
                        modifyIDE_ (\ide -> return (ide{activePack = Just packWithNewFlags}))
                        close flagsPane) ideR -- we don't trigger the activePack event here
                    writeFlags ((dropFileName (cabalFile packWithNewFlags)) </> "IDE.flags")
                        packWithNewFlags)
        closeB `onClicked` (reflectIDE (close flagsPane) ideR)
        registerEvent notifier FocusIn (Left (\e -> do
            reflectIDE (makeActive flagsPane) ideR
            return (e{gtkReturn=False})))
        boxPackStart vb sw PackGrow 7
        boxPackEnd vb bb PackNatural 7
        notebookInsertOrdered nb vb (paneName flagsPane) Nothing
        widgetShowAll vb
        return (flagsPane,[])
    addPaneAdmin buf [] panePath
    liftIO $ widgetGrabFocus (flagsBox buf)



