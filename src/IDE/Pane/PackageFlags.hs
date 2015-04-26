{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
import IDE.Utils.GUIUtils (__)
import Control.Monad (void)
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T (unwords, unpack, pack)
import Control.Applicative ((<$>))

data IDEFlags               =   IDEFlags {
    flagsBox                ::   VBox
} deriving Typeable

data FlagsState             =   FlagsState
    deriving(Eq,Ord,Read,Show,Typeable)

instance Pane IDEFlags IDEM
    where
    primPaneName _  =   __ "Package Flags"
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
    boxSetSpacing bb 6
    buttonBoxSetLayout bb ButtonboxSpread
    saveB               <-  buttonNewFromStock "gtk-save"
    widgetSetSensitive saveB False
    cancelB             <-  buttonNewFromStock "gtk-cancel"
    boxPackStart bb cancelB PackNatural 0
    boxPackStart bb saveB PackNatural 0
    (widget,injb,ext,notifier)
                        <-  buildEditor flagsDesc idePackage
    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetShadowType sw ShadowIn
    scrolledWindowAddWithViewport sw widget
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    on saveB buttonActivated (do
        mbPackWithNewFlags <- extract idePackage [ext]
        case mbPackWithNewFlags of
            Nothing -> return ()
            Just packWithNewFlags -> do
                reflectIDE (do
                        changePackage packWithNewFlags
                        closePane flagsPane) ideR
                writeFields (dropExtension (ipdCabalFile packWithNewFlags) ++
                                leksahFlagFileExtension)
                    packWithNewFlags flatFlagsDescription)
    on cancelB buttonActivated (reflectIDE (void (closePane flagsPane)) ideR)
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

    boxPackStart vb sw PackGrow 0
    boxPackEnd vb bb PackNatural 6
    return (Just flagsPane,[])

getFlags :: Maybe PanePath -> IDEM IDEFlags
getFlags Nothing    = forceGetPane (Right "*Flags")
getFlags (Just pp)  = forceGetPane (Left pp)

quoteArg :: String -> String
quoteArg s | ' ' `elem` s = "\"" <> escapeQuotes s <> "\""
quoteArg s = s

escapeQuotes = foldr (\c s -> if c == '"' then '\\':c:s else c:s) ""

quotedArgCharParser :: CharParser () Char
quotedArgCharParser = try (do
        char '\\'
        anyChar)
    <|> try (
        noneOf "\"")
    <?> "argsParser"

argParser :: CharParser () Text
argParser = try (do
        char '"'
        s <- many quotedArgCharParser
        char '"'
        return $ T.pack s)
    <|> try (
        T.pack <$> many1 (noneOf " "))
    <?> "argParser"

argsParser :: CharParser () [Text]
argsParser = try (
        many (do
            many (char ' ')
            argParser))
    <?> "argsParser"

unargs :: [Text] -> Text
unargs = T.unwords . map (T.pack . quoteArg . T.unpack)

args :: Text -> [Text]
args s = case parse argsParser "" $ T.unpack s of
            Right result -> result
            _ -> [s]

flatFlagsDescription :: [FieldDescriptionS IDEPackage]
flatFlagsDescription = flattenFieldDescriptionPPToS flagsDescription

flagsDescription :: FieldDescriptionPP IDEPackage IDEM
flagsDescription = VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName (__ "Config flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdConfigFlags)
            (\ b a -> a{ipdConfigFlags = args b})
            (textEditor (const True) True)
            (\ _ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Build flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdBuildFlags)
            (\ b a -> a{ipdBuildFlags = args b})
            (textEditor (const True) True)
            (\ _ ->  return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Test flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdTestFlags)
            (\ b a -> a{ipdTestFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Haddock flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdHaddockFlags)
            (\ b a -> a{ipdHaddockFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Executable flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdExeFlags)
            (\ b a -> a{ipdExeFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Install flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdInstallFlags)
            (\ b a -> a{ipdInstallFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Register flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdRegisterFlags)
            (\ b a -> a{ipdRegisterFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Unregister flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdUnregisterFlags)
            (\ b a -> a{ipdUnregisterFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Source Distribution flags") $ emptyParams)
            (PP.text . show)
            readParser
            (unargs . ipdSdistFlags)
            (\ b a -> a{ipdSdistFlags = args b})
            (textEditor (const True) True)
            (\ _ -> return ())]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readFlags :: FilePath -> IDEPackage -> IO IDEPackage
readFlags fn = readFields fn flatFlagsDescription

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writeFlags :: FilePath -> IDEPackage -> IO ()
writeFlags fpath flags = writeFields fpath flags flatFlagsDescription



