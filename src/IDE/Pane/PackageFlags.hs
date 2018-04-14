{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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
-- | Pane for saving, restoring and editing flags for specific cabal
-- commands.
--
---------------------------------------------------------------------------------


module IDE.Pane.PackageFlags (
    readFlags
,   writeFlags
,   IDEFlags(..)
,   FlagsState
,   getFlags
) where

import qualified Text.PrettyPrint.HughesPJ as PP
import Data.Typeable
import System.FilePath.Posix
import IDE.Core.State
import Graphics.UI.Editor.Basics
import Graphics.UI.Editor.MakeEditor
import Graphics.UI.Editor.Simple
import Graphics.UI.Editor.Parameters
import Control.Event (registerEvent)
import Graphics.UI.Editor.DescriptionPP
    (extractFieldDescription,
     FieldDescriptionPP(..),
     mkFieldPP)
import Text.ParserCombinators.Parsec hiding(Parser)
import IDE.Utils.GUIUtils (__)
import Control.Monad (void)
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Aeson (eitherDecode, encode, FromJSON, ToJSON)
import qualified Data.Text as T (unwords, unpack, pack)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import GI.Gtk.Objects.Widget (widgetSetSensitive, Widget(..))
import GI.Gtk.Objects.Box (boxSetSpacing, Box(..), boxNew)
import GI.Gtk.Objects.ButtonBox (buttonBoxSetLayout, buttonBoxNew)
import GI.Gtk.Enums
       (PolicyType(..), ShadowType(..), ButtonBoxStyle(..),
        Orientation(..))
import GI.Gtk.Objects.Button (onButtonClicked, buttonNewWithLabel)
import GI.Gtk.Objects.Adjustment (noAdjustment)
import GI.Gtk.Objects.ScrolledWindow
       (scrolledWindowSetPolicy,
        scrolledWindowSetShadowType, scrolledWindowNew)
import GI.Gtk (containerAdd)
import GHC.Generics (Generic)
import qualified Control.Exception as E (catch)
import qualified Data.ByteString.Lazy as LBS (writeFile, readFile)
import Control.Exception (IOException)
import Data.Maybe (fromMaybe)
import System.Log.Logger (errorM)

data IDEFlags               =   IDEFlags {
    flagsBox                ::   Box
} deriving Typeable

data FlagsState             =   FlagsState
    deriving(Eq,Ord,Read,Show,Typeable,Generic)

instance ToJSON FlagsState
instance FromJSON FlagsState

instance Pane IDEFlags IDEM
    where
    primPaneName _  =   __ "Package Flags"
    getAddedIndex _ =   0
    getTopWidget    =   liftIO . unsafeCastTo Widget . flagsBox
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
                    buildThisPane pp nb builder
                Nothing -> return Nothing
    builder pp nb w =
        let flagsDesc       =   extractFieldDescription flagsDescription
            flatflagsDesc   =   flattenFieldDescription flagsDesc
        in  do
                mbPack      <-  readIDE activePack
                case mbPack of
                    Nothing -> return (Nothing,[])
                    Just p  -> reifyIDE $ \ideR -> builder' p flagsDesc flatflagsDesc  pp nb window ideR


-- | Builds the Flags pane
builder' idePackage flagsDesc flatflagsDesc pp nb window ideR = do
    vb                  <-  boxNew OrientationVertical 0
    let flagsPane = IDEFlags vb
    bb                  <-  buttonBoxNew OrientationHorizontal
    boxSetSpacing bb 6
    buttonBoxSetLayout bb ButtonBoxStyleSpread
    saveB               <-  buttonNewWithLabel "Save"
    widgetSetSensitive saveB False
    cancelB             <-  buttonNewWithLabel "Cancel"
    boxPackStart' bb cancelB PackNatural 0
    boxPackStart' bb saveB PackNatural 0
    (widget,injb,ext,notifier)
                        <-  buildEditor flagsDesc idePackage
    sw <- scrolledWindowNew noAdjustment noAdjustment
    scrolledWindowSetShadowType sw ShadowTypeIn
    containerAdd sw widget
    scrolledWindowSetPolicy sw PolicyTypeAutomatic PolicyTypeAutomatic
    onButtonClicked saveB (do
        mbPackWithNewFlags <- extract idePackage [ext]
        case mbPackWithNewFlags of
            Nothing -> return ()
            Just packWithNewFlags -> do
                reflectIDE (do
                        changePackage packWithNewFlags
                        closePane flagsPane) ideR
                writeFlags (dropExtension (ipdCabalFile packWithNewFlags) ++
                                leksahFlagFileExtension)
                    packWithNewFlags)
    onButtonClicked cancelB (reflectIDE (void (closePane flagsPane)) ideR)
    registerEvent notifier FocusIn (\e -> do
        reflectIDE (makeActive flagsPane) ideR
        return (e{gtkReturn=False}))
    registerEvent notifier MayHaveChanged (\e -> (`reflectIDE` ideR) $ do
        mbP <- liftIO $ extract idePackage [ext]
        let hasChanged = case mbP of
                                Nothing -> False
                                Just p -> p /= idePackage
        topWidget <- getTopWidget flagsPane
        markLabel nb topWidget hasChanged
        widgetSetSensitive saveB hasChanged
        return (e{gtkReturn=False}))

    boxPackStart' vb sw PackGrow 0
    boxPackEnd' vb bb PackNatural 6
    return (Just flagsPane,[])


-- | Gets the Flags pane
getFlags :: Maybe PanePath -> IDEM IDEFlags
getFlags Nothing    = forceGetPane (Right "*Flags")
getFlags (Just pp)  = forceGetPane (Left pp)


-- | Quote the string if it contains spaces and escape
-- any other quotes.
quoteArg :: String -> String
quoteArg s | ' ' `elem` s = "\"" <> escapeQuotes s <> "\""
quoteArg s = s

escapeQuotes = foldr (\c s -> if c == '"' then '\\':c:s else c:s) ""


-- | Parse any (escaped) character (ignoring a prefixed @\@)
quotedArgCharParser :: CharParser () Char
quotedArgCharParser = try (do
        char '\\'
        anyChar)
    <|> try (
        noneOf "\"")
    <?> "argsParser"


-- | Parse an argument that is either quoted or does not
-- contain spaces
argParser :: CharParser () Text
argParser = try (do
        char '"'
        s <- many quotedArgCharParser
        char '"'
        return $ T.pack s)
    <|> try (
        T.pack <$> many1 (noneOf " "))
    <?> "argParser"


-- | Parse many arguments, possibly seperated by spaces
argsParser :: CharParser () [Text]
argsParser = try (
        many (do
            many (char ' ')
            argParser))
    <?> "argsParser"


-- | Quote all arguments and concatenate them
unargs :: [Text] -> Text
unargs = T.unwords . map (T.pack . quoteArg . T.unpack)


-- | Parse a list of arguments from a given string
args :: Text -> [Text]
args s = case parse argsParser "" $ T.unpack s of
            Right result -> result
            _ -> [s]


-- | The description of the fields in the pane
flagsDescription :: FieldDescriptionPP IDEPackage IDEM
flagsDescription = VFDPP emptyParams [
        mkFieldPP
            (paraName <<<- ParaName (__ "Config flags") $ emptyParams)
            (unargs . ipdConfigFlags)
            (\ b a -> a{ipdConfigFlags = args b})
            (textEditor (const True) True)
            (\ _ -> return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Build flags") $ emptyParams)
            (unargs . ipdBuildFlags)
            (\ b a -> a{ipdBuildFlags = args b})
            (textEditor (const True) True)
            (\ _ ->  return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Test flags") $ emptyParams)
            (unargs . ipdTestFlags)
            (\ b a -> a{ipdTestFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Benchmark flags") $ emptyParams)
            (unargs . ipdBenchmarkFlags)
            (\ b a -> a{ipdBenchmarkFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Haddock flags") $ emptyParams)
            (unargs . ipdHaddockFlags)
            (\ b a -> a{ipdHaddockFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Executable flags") $ emptyParams)
            (unargs . ipdExeFlags)
            (\ b a -> a{ipdExeFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Install flags") $ emptyParams)
            (unargs . ipdInstallFlags)
            (\ b a -> a{ipdInstallFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Register flags") $ emptyParams)
            (unargs . ipdRegisterFlags)
            (\ b a -> a{ipdRegisterFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Unregister flags") $ emptyParams)
            (unargs . ipdUnregisterFlags)
            (\ b a -> a{ipdUnregisterFlags = args b})
            (textEditor (const True) True)
            (\ _ ->   return ())
    ,   mkFieldPP
            (paraName <<<- ParaName (__ "Source Distribution flags") $ emptyParams)
            (unargs . ipdSdistFlags)
            (\ b a -> a{ipdSdistFlags = args b})
            (textEditor (const True) True)
            (\ _ -> return ())]

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

data FlagsFile = FlagsFile
  { configFlags     :: Maybe [Text]
  , buildFlags      :: Maybe [Text]
  , testFlags       :: Maybe [Text]
  , benchmarkFlags  :: Maybe [Text]
  , haddockFlags    :: Maybe [Text]
  , exeFlags        :: Maybe [Text]
  , installFlags    :: Maybe [Text]
  , registerFlags   :: Maybe [Text]
  , unregisterFlags :: Maybe [Text]
  , sdistFlags      :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON FlagsFile
instance FromJSON FlagsFile

setFlags :: IDEPackage -> FlagsFile -> IDEPackage
setFlags p@IDEPackage{..} FlagsFile{..} = p
  { ipdConfigFlags     = fromMaybe [] configFlags
  , ipdBuildFlags      = fromMaybe [] buildFlags
  , ipdTestFlags       = fromMaybe [] testFlags
  , ipdBenchmarkFlags  = fromMaybe [] benchmarkFlags
  , ipdHaddockFlags    = fromMaybe [] haddockFlags
  , ipdExeFlags        = fromMaybe [] exeFlags
  , ipdInstallFlags    = fromMaybe [] installFlags
  , ipdRegisterFlags   = fromMaybe [] registerFlags
  , ipdUnregisterFlags = fromMaybe [] unregisterFlags
  , ipdSdistFlags      = fromMaybe [] sdistFlags
  }

getFlagsFile :: IDEPackage -> FlagsFile
getFlagsFile IDEPackage{..} = FlagsFile
  { configFlags     = Just ipdConfigFlags
  , buildFlags      = Just ipdBuildFlags
  , testFlags       = Just ipdTestFlags
  , benchmarkFlags  = Just ipdBenchmarkFlags
  , haddockFlags    = Just ipdHaddockFlags
  , exeFlags        = Just ipdExeFlags
  , installFlags    = Just ipdInstallFlags
  , registerFlags   = Just ipdRegisterFlags
  , unregisterFlags = Just ipdUnregisterFlags
  , sdistFlags      = Just ipdSdistFlags
  }

-- | Read all the field values from the given 'FilePath'
readFlags :: FilePath -> IDEPackage -> IO IDEPackage
readFlags file pkg = E.catch (
    eitherDecode <$> LBS.readFile file >>= \case
        Left e -> do
            liftIO . errorM "leksah" $ "Error reading file " ++ show file ++ " " ++ show e
            return pkg
        Right f -> return $ setFlags pkg f)
    (\ (e::IOException) -> do
        liftIO . errorM "leksah" $ "Error reading file " ++ show file ++ " " ++ show e
        return pkg)

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

-- | Write all field values to the given 'FilePath'
writeFlags :: FilePath -> IDEPackage -> IO ()
writeFlags file = LBS.writeFile file . encode . getFlagsFile

