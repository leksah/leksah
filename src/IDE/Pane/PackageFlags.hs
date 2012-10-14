{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, DeriveDataTypeable,
             MultiParamTypeClasses, TypeSynonymInstances, Rank2Types #-}
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
,   IDEFlags(..)
,   FlagsState
,   getFlags
) where

import Graphics.UI.Gtk
import qualified Text.PrettyPrint.HughesPJ as PP
import Data.Typeable
import System.FilePath.Posix
import IDE.Core.State
import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Simple
import Graphics.UI.Editor.Parameters
import Text.PrinterParser hiding (fieldParser,parameters)
import Control.Event (registerEvent)
import Graphics.UI.Editor.DescriptionPP
    (flattenFieldDescriptionPPToS,
     extractFieldDescription,
     FieldDescriptionPP(..),
     mkFieldPP)
import Text.ParserCombinators.Parsec hiding(Parser)
import Debug.Trace (trace)

data IDEFlags               =   IDEFlags {
    flagsBox                ::   VBox
} deriving Typeable

data FlagsState             =   FlagsState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEFlags IDEM
    where
    primPaneName _  =   "Package Flags"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . flagsBox
    paneId b        =   "*Flags"

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
                    pp  <- getBestPathForId "*Flags"
                    nb  <- getNotebook pp
                    case mbPack of
                        Nothing -> return Nothing
                        Just pack -> buildThisPane pp nb builder
                Nothing -> return Nothing
    builder pp nb w =
        let flagsDesc       =   extractFieldDescription flagsDescription
            flatflagsDesc   =   flattenFieldDescription flagsDesc
        in  do
                mbPack      <-  readIDE activePack
                case mbPack of
                    Nothing -> return (Nothing,[])
                    Just p  -> reifyIDE $ \ideR -> builder' p flagsDesc flatflagsDesc  pp nb window ideR

builder' idePackage flagsDesc flatflagsDesc pp nb window ideR = do
    vb                  <-  vBoxNew False 0
    let flagsPane = IDEFlags vb
    bb                  <-  hButtonBoxNew
    saveB               <-  buttonNewFromStock "gtk-save"
    widgetSetSensitive saveB False
    cancelB             <-  buttonNewFromStock "gtk-cancel"
    boxPackStart bb cancelB PackGrow 0
    boxPackStart bb saveB PackGrow 0
    (widget,injb,ext,notifier)
                        <-  buildEditor flagsDesc idePackage
    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport sw widget
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    saveB `onClicked` (do
        mbPackWithNewFlags <- extract idePackage [ext]
        case mbPackWithNewFlags of
            Nothing -> return ()
            Just packWithNewFlags -> do
                reflectIDE (do
                	changePackage packWithNewFlags
                	closePane flagsPane) ideR
                writeFields ((dropExtension (ipdCabalFile packWithNewFlags)) ++ leksahFlagFileExtension)
                    packWithNewFlags flatFlagsDescription)
    cancelB `onClicked` (reflectIDE (closePane flagsPane >> return ()) ideR)
    registerEvent notifier FocusIn (\e -> do
        reflectIDE (makeActive flagsPane) ideR
        return (e{gtkReturn=False}))
    registerEvent notifier MayHaveChanged (\e -> do
        mbP <- extract idePackage [ext]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= idePackage
        markLabel nb (getTopWidget flagsPane) hasChanged
        widgetSetSensitive saveB hasChanged
        return (e{gtkReturn=False}))

    boxPackStart vb sw PackGrow 7
    boxPackEnd vb bb PackNatural 7
    return (Just flagsPane,[])

getFlags :: Maybe PanePath -> IDEM IDEFlags
getFlags Nothing    = forceGetPane (Right "*Flags")
getFlags (Just pp)  = forceGetPane (Left pp)

quoteArg :: String -> String
quoteArg s | ' ' `elem` s = "\"" ++ (escapeQuotes s) ++ "\""
quoteArg s = s

escapeQuotes = foldr (\c s -> if c == '"' then '\\':c:s else c:s) ""

quotedArgCharParser :: CharParser () Char
quotedArgCharParser = try (do
        char '\\'
        anyChar)
    <|> try (do
        noneOf "\"")
    <?> "argsParser"

argParser :: CharParser () String
argParser = try (do
        char '"'
        s <- many quotedArgCharParser
        char '"'
        return s)
    <|> try (do
        many1 (noneOf " "))
    <?> "argParser"

argsParser :: CharParser () [String]
argsParser = try (do
        many (do
            many (char ' ')
            argParser))
    <?> "argsParser"

unargs :: [String] -> String
unargs = unwords . (map quoteArg)

args :: String -> [String]
args s = case parse argsParser "" s of
            Right result -> result
            _ -> [s]

flatFlagsDescription :: [FieldDescriptionS IDEPackage]
flatFlagsDescription = flattenFieldDescriptionPPToS flagsDescription

flagsDescription :: FieldDescriptionPP IDEPackage IDEM
flagsDescription = VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName "Config flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdConfigFlags p))
            (\ b a -> a{ipdConfigFlags = args b})
            (stringEditor (const True) True)
            (\ _ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Build flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdBuildFlags p))
            (\ b a -> a{ipdBuildFlags = args b})
            (stringEditor (const True) True)
            (\ _ ->  return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Test flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdTestFlags p))
            (\ b a -> a{ipdTestFlags = args b})
            (stringEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Haddock flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdHaddockFlags p))
            (\ b a -> a{ipdHaddockFlags = args b})
            (stringEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Executable flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdExeFlags p))
            (\ b a -> a{ipdExeFlags = args b})
            (stringEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Install flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdInstallFlags p))
            (\ b a -> a{ipdInstallFlags = args b})
            (stringEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Register flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdRegisterFlags p))
            (\ b a -> a{ipdRegisterFlags = args b})
            (stringEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Unregister flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdUnregisterFlags p))
            (\ b a -> a{ipdUnregisterFlags = args b})
            (stringEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Source Distribution flags" $ emptyParams)
            (PP.text . show)
            readParser
            (\p -> unargs (ipdSdistFlags p))
            (\ b a -> a{ipdSdistFlags = args b})
            (stringEditor (const True) True)
            (\ _ -> return ())]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readFlags :: FilePath -> IDEPackage -> IO IDEPackage
readFlags fn pack = readFields fn flatFlagsDescription pack

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writeFlags :: FilePath -> IDEPackage -> IO ()
writeFlags fpath flags = writeFields fpath flags flatFlagsDescription



