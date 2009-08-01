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

import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec hiding(Parser)
import qualified Text.ParserCombinators.Parsec.Token as P

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
                    pp  <- getBestPathForId "*Flags"
                    nb  <- getNotebook pp
                    initFlags pack pp nb
                Nothing -> return ()

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

flatFlagsDescription :: [FieldDescriptionPP IDEPackage]
flatFlagsDescription = flattenFieldDescriptionPP flagsDescription

flagsDescription :: FieldDescriptionPP IDEPackage
flagsDescription = VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName "Config flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (configFlags p))
            (\ b a -> a{configFlags = args b})
            (stringEditor (const True))
            (\ _ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Build flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (buildFlags p))
            (\ b a -> a{buildFlags = args b})
            (stringEditor (const True))
            (\ _ ->  return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Haddock flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (haddockFlags p))
            (\ b a -> a{haddockFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Executable flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (exeFlags p))
            (\ b a -> a{exeFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Install flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (installFlags p))
            (\ b a -> a{installFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Register flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (registerFlags p))
            (\ b a -> a{registerFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Unregister flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (unregisterFlags p))
            (\ b a -> a{unregisterFlags = args b})
            (stringEditor (const True))
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName "Source Distribution flags" $ emptyParams)
            (PP.text . show)
            stringParser
            (\p -> unargs (sdistFlags p))
            (\ b a -> a{sdistFlags = args b})
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
                    pp          <-  getBestPathForId "*Flags"
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
    newPane panePath nb (builder idePackage flagsDesc flatflagsDesc)
    return ()

builder :: IDEPackage ->
    FieldDescription IDEPackage ->
    [FieldDescription IDEPackage] ->
    PanePath ->
    Notebook ->
    Window ->
    IDERef ->
    IO (IDEFlags,Connections)
builder idePackage flagsDesc flatflagsDesc pp nb window ideR = do
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
                    close flagsPane) ideR -- we don't trigger the activePack event here
                writeFlags ((dropFileName (cabalFile packWithNewFlags)) </> "IDE.flags")
                    packWithNewFlags)
    closeB `onClicked` (reflectIDE (close flagsPane >> return ()) ideR)
    registerEvent notifier FocusIn (Left (\e -> do
        reflectIDE (makeActive flagsPane) ideR
        return (e{gtkReturn=False})))
    boxPackStart vb sw PackGrow 7
    boxPackEnd vb bb PackNatural 7
    return (flagsPane,[])
