{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances -XRank2Types #-}
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
import Text.PrinterParser hiding (fieldParser,parameters)
import Control.Event (registerEvent)
import Graphics.UI.Editor.DescriptionPP
    (flattenFieldDescriptionPPToS,
     extractFieldDescription,
     FieldDescriptionPP(..),
     mkFieldPP)
import Text.ParserCombinators.Parsec hiding(Parser)

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
                    modifyIDE_ (\ide -> ide{activePack = Just packWithNewFlags})
                    closePane flagsPane) ideR -- we don't trigger the activePack event here
                writeFields ((dropExtension (ipdCabalFile packWithNewFlags)) ++ leksahFlagFileExtension)
                    packWithNewFlags flatFlagsDescription)
    closeB `onClicked` (reflectIDE (closePane flagsPane >> return ()) ideR)
    registerEvent notifier FocusIn (Left (\e -> do
        reflectIDE (makeActive flagsPane) ideR
        return (e{gtkReturn=False})))
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
            stringParser
            (\p -> unargs (ipdConfigFlags p))
            (\ b a -> a{ipdConfigFlags = args b})
            (stringEditor (const True))
            (\ _ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Build flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (ipdBuildFlags p))
            (\ b a -> a{ipdBuildFlags = args b})
            (stringEditor (const True))
            (\ _ ->  return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Haddock flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (ipdHaddockFlags p))
            (\ b a -> a{ipdHaddockFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Executable flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (ipdExeFlags p))
            (\ b a -> a{ipdExeFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Install flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (ipdInstallFlags p))
            (\ b a -> a{ipdInstallFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Register flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (ipdRegisterFlags p))
            (\ b a -> a{ipdRegisterFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Unregister flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (ipdUnregisterFlags p))
            (\ b a -> a{ipdUnregisterFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Source Distribution flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (ipdSdistFlags p))
            (\ b a -> a{ipdSdistFlags = args b})
            (stringEditor (const True))
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



