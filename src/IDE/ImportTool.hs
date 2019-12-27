{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.ImportTool
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  Jutaro <jutaro@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- | Help for constructing import statements
--
-----------------------------------------------------------------------------

module IDE.ImportTool (
    resolveErrors
,   addOneImport
,   addResolveMenuItems
,   resolveMenuItems
,   parseHiddenModule
,   HiddenModuleResult(..)
) where

import Prelude ()
import Prelude.Compat

import Control.Applicative ((<$>))
import Control.Exception (SomeException)
import Control.Lens ((%~))
import Control.Monad
       (forM_, void, MonadPlus(..), unless, when)
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Foldable as F (toList, foldr)
import Data.Functor.Identity (Identity(..))
import Data.List (intercalate, sort, nub, nubBy)
import Data.Maybe
       (mapMaybe, fromMaybe, catMaybes, fromJust, isNothing, isJust)
import qualified Data.Set as S (singleton)
import Data.Text (Text)
import qualified Data.Text as T
       (takeWhile, stripPrefix, lines, dropWhile, empty, pack, unpack)

import qualified Distribution.ModuleName as D (ModuleName, components, fromString)
import Distribution.Package
import Distribution.PackageDescription
       (GenericPackageDescription(..), Benchmark(..), TestSuite(..),
        Executable(..), BuildInfo(..), Library(..), CondTree(..),
        condExecutables, condLibrary)
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
       (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse
       (readGenericPackageDescription)
#endif
import Distribution.Pretty (prettyShow)
import Distribution.Text (simpleParse, display)
import Distribution.Verbosity (normal)
import Distribution.Version
       (anyVersion, orLaterVersion, intersectVersionRanges,
        earlierVersion, versionNumbers, mkVersion)

import Language.Haskell.Exts (KnownExtension(..))

import System.Log.Logger (debugM)

import Text.Parsec (ParsecT)
import Text.Parsec.Token (GenTokenParser)
import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import qualified Text.ParserCombinators.Parsec.Token as P
       (operator, dot, identifier, symbol, lexeme, whiteSpace,
        makeTokenParser)
import Text.PrettyPrint (render)

import Data.GI.Base (unsafeCastTo)
import Data.GI.Base.GObject (new')
import GI.Gtk (constructDialogUseHeaderBar)
import GI.Gtk.Enums (ResponseType(..))
import GI.Gtk.Objects.Box (Box(..))
import GI.Gtk.Objects.Dialog (Dialog(..), dialogGetContentArea)
import GI.Gtk.Objects.MenuItem
       (onMenuItemActivate, menuItemNewWithLabel)
import GI.Gtk.Objects.MenuShell (IsMenuShell, menuShellAppend)
import GI.Gtk.Objects.Widget
       (widgetDestroy, widgetHide, widgetGrabDefault, setWidgetCanDefault,
        widgetShowAll)
import GI.Gtk.Objects.Window (setWindowTransientFor, Window(..))

import Graphics.UI.Editor.MakeEditor
       (FieldDescription(..), buildEditor, mkField)
import Graphics.UI.Editor.Parameters
       (dialogRun', dialogSetDefaultResponse', Packing(..), boxPackStart',
        dialogAddButton', (<<<-), paraMinSize, emptyParams, Parameter(..),
        paraMultiSel, paraName)
import Graphics.UI.Editor.Simple (staticListEditor)

import IDE.Core.State
       (IDEAction, backgroundBuild, LogRef, Descr(..), IDEM, LogRef,
        dscMbModu', dsrMbModu, PackModule(..), modu, GenScope(..),
        PackScope(..), LogRef(..), Log(..), ServerAnswer(..),
        ImportDecl, ImportSpecList(..), ImportDecl(..), ImportSpec(..),
        TypeDescr(..), IDERef, readIDE, prefs, modifyIDE_, errorRefs,
        ideMessage, MessageLevel(..), currentError,
        logRefFullFilePath, catchIDE, dsMbModu, dscName, ServerCommand(..),
        pjKey, ipdCabalFile, prettyPrint, locationELine, locationSLine,
        throwIDE, Location(..), dsrDescr, dscTypeHint', __, reflectIDE)
import IDE.BufferMode (editInsertCode)
import IDE.Gtk.State (getMainWindow)
import IDE.HLint (resolveActiveHLint)
import IDE.Metainfo.Provider
       (getWorkspaceInfo, getPackageImportInfo, getIdentifierDescr)
import IDE.Pane.SourceBuffer
import IDE.TextEditor (delete, setModified, getIterAtLine)
import IDE.Utils.CabalUtils (writeGenericPackageDescription')
import IDE.Utils.GHCUtils (mkDependency, LibraryName(..))
import IDE.Utils.ServerConnection

readMaybe :: Read a => Text -> Maybe a
readMaybe s = case reads $ T.unpack s of
                [(x, "")] -> Just x
                _         -> Nothing

-- | Add all imports which gave error messages ...
resolveErrors :: IDEAction
resolveErrors = do
    liftIO $ debugM "leksah" "resolveErrors"
    prefs' <- readIDE prefs
    let buildInBackground = backgroundBuild prefs'
    when buildInBackground $
        modifyIDE_ $ prefs %~ (\p -> p{backgroundBuild = False})
    errors <- F.toList <$> readIDE errorRefs
    addPackageResult <- addPackages errors
    let notInScopes = [ y | (x,y) <-
            nubBy (\ (p1,_) (p2,_) -> p1 == p2)
                  [(x,y) |  (x,y) <- [((parseNotInScope . refDescription) e, e) | e <- errors]],
                                isJust x]
    let extensions = [ y | (x,y) <-
            nubBy (\ (p1,_) (p2,_) -> p1 == p2)
                  [(x,y) |  (x,y) <- [((parsePerhapsYouIntendedToUse . refDescription) e, e) | e <- errors]],
                                length x == 1]
    addAll addPackageResult buildInBackground notInScopes extensions
  where
    addAll addPackageResult buildInBackground notInScopes extensions = addAllImports notInScopes (True,[])
      where
        addAllImports :: [LogRef] -> (Bool,[Descr]) -> IDEM ()
        addAllImports (errorSpec:rest) (True,descrList)  =  addImport errorSpec descrList (addAllImports rest)
        addAllImports _ (cont, _)                        =  addAllExtensions extensions cont

        addAllExtensions :: [LogRef] -> Bool -> IDEM ()
        addAllExtensions (errorSpec:rest) True =  addExtension errorSpec (addAllExtensions rest)
        addAllExtensions _ _                   =  finally

        finally = do
            when buildInBackground $
                modifyIDE_ $ prefs %~ (\p -> p{backgroundBuild = True})
            when (not addPackageResult && null notInScopes && null extensions) $ do
                hlintResolved <- resolveActiveHLint
                unless hlintResolved $ ideMessage Normal "No errors, warnings or selected hlints that can be auto resolved"

-- | Add import for current error ...
addOneImport :: IDEAction
addOneImport = do
    currentErr' <- readIDE currentError
    case currentErr' of
        Nothing -> do
            ideMessage Normal "No error selected"
            return ()
        Just ref -> addImport ref [] (\ _ -> return ())

mapDescr :: Descr -> Descr
mapDescr (Real d) = Real d { dscMbModu' = mapPackModule <$> dscMbModu' d }
mapDescr (Reexported d) = Reexported d { dsrMbModu = mapPackModule <$> dsrMbModu d }

mapPackModule :: PackModule -> PackModule
mapPackModule m@PM{ modu = n } = m { modu = D.fromString . intercalate "." . mapModuleName $ D.components n }

mapModuleName :: [String] -> [String]
mapModuleName ("JSDOM":"Generated":rest) = "GHCJS":"DOM":rest
mapModuleName ("JSDOM":"Custom":rest) = "GHCJS":"DOM":rest
mapModuleName ("JSDOM":rest) = "GHCJS":"DOM":rest
mapModuleName n = n

-- | Add one missing import
-- Returns a boolean, if the process should be stopped in case of multiple addition
-- Returns a list of already added descrs, so that it will not be added two times and can
-- be used for default selection
addImport :: LogRef -> [Descr] -> ((Bool,[Descr]) -> IDEAction) -> IDEAction
addImport err descrList continuation =
    case parseNotInScope $ refDescription err of
        Nothing -> continuation (True,descrList)
        Just nis -> do
            currentInfo' <- getScopeForRef err
            wsInfo' <- getWorkspaceInfo
            case (currentInfo', wsInfo') of
                (Nothing, _) -> continuation (True,descrList)
                (_, Nothing) -> continuation (True,descrList)
                (Just (GenScopeC(PackScope _ symbolTable1),GenScopeC(PackScope _ symbolTable2)),
                    Just (GenScopeC(PackScope _ symbolTable3),GenScopeC(PackScope _ symbolTable4))) ->
                    let list = map mapDescr $ getIdentifierDescr (id' nis) symbolTable1 symbolTable2
                        wslist = map mapDescr $ getIdentifierDescr (id' nis) symbolTable3 symbolTable4
                    in case (list, wslist) of
                        ([], []) ->  do
                            ideMessage Normal $ "Identifier " <> id' nis <> " not found"
                            continuation (True, descrList)
                        ([], l) -> do
                            window' <- getMainWindow
                            mbDescr <-  liftIO $ selectModuleDialog window' l (id' nis) (mbQual' nis)
                                            (if null descrList
                                                then Nothing
                                                else Just (head descrList))
                            case mbDescr of
                                Nothing     ->  continuation (False, [])
                                Just descr  ->  if descr `elem` descrList
                                                    then continuation (True,descrList)
                                                    else addImport' nis (logRefFullFilePath err)
                                                            descr descrList continuation
                        ([descr], _)  ->  addImport' nis (logRefFullFilePath err) descr descrList continuation
                        _ ->  do
                            let fullList = list ++ wslist
                            window' <- getMainWindow
                            mbDescr <-  liftIO $ selectModuleDialog window' fullList (id' nis) (mbQual' nis)
                                            (if null descrList
                                                then Nothing
                                                else Just (head descrList))
                            case mbDescr of
                                Nothing     ->  continuation (False, [])
                                Just descr  ->  if descr `elem` descrList
                                                    then continuation (True,descrList)
                                                    else addImport' nis (logRefFullFilePath err)
                                                            descr descrList continuation

addPackages :: [LogRef] -> IDEM Bool
addPackages errors = do
    let packs = nub $ mapMaybe (\case
                    err@LogRef{logRefLog = LogCabal cabalFile} ->
                        case parseHiddenModule $ refDescription err of
                            Nothing -> Nothing
                            Just (HiddenModuleResult _ pack') -> Just (cabalFile, dep pack')
                    _ -> Nothing) errors

    forM_ packs $ \(cabalFile, d) -> do
        gpd <- liftIO $ readGenericPackageDescription normal cabalFile
        ideMessage Normal $ "Adding build-depends " <> T.pack (display d <> " to " <> cabalFile)
        liftIO $ writeGenericPackageDescription' cabalFile
            gpd { condLibrary     = addDepToLib d (condLibrary gpd),
                  condExecutables = map (addDepToExe d)
                                        (condExecutables gpd),
                  condTestSuites  = map (addDepToTest d)
                                        (condTestSuites gpd),
                  condBenchmarks  = map (addDepToBenchmark d)
                                        (condBenchmarks gpd)}
      `catchIDE`
        (\(e :: SomeException) -> ideMessage High . T.pack $ show e)

    return . not $ null packs
  where
    addDepToLib _ Nothing = Nothing
    addDepToLib d (Just cn@CondNode{
        condTreeConstraints = deps,
        condTreeData        = lib@Library{libBuildInfo = bi}}) = Just (cn{
            condTreeConstraints = deps <> [d],
            condTreeData        = lib {libBuildInfo = bi {targetBuildDepends = targetBuildDepends bi <> [d]}}})
    addDepToExe d (str,cn@CondNode{
        condTreeConstraints = deps,
        condTreeData        = exe@Executable{buildInfo = bi}}) = (str,cn{
                condTreeConstraints = deps <> [d],
                condTreeData        = exe { buildInfo = bi {targetBuildDepends = targetBuildDepends bi <> [d]}}})
    addDepToTest d (str,cn@CondNode{
        condTreeConstraints = deps,
        condTreeData        = test@TestSuite{testBuildInfo = bi}}) = (str,cn{
                condTreeConstraints = deps <> [d],
                condTreeData        = test { testBuildInfo = bi {targetBuildDepends = targetBuildDepends bi <> [d]}}})
    addDepToBenchmark d (str,cn@CondNode{
        condTreeConstraints = deps,
        condTreeData        = bm@Benchmark{benchmarkBuildInfo = bi}}) = (str,cn{
                condTreeConstraints = deps <> [d],
                condTreeData        = bm { benchmarkBuildInfo = bi {targetBuildDepends = targetBuildDepends bi <> [d]}}})
    -- Empty version is probably only going to happen for ghc-prim
    dep p | null . versionNumbers $ packageVersion p = mkDependency (packageName p) anyVersion (S.singleton LMainLibName)
    dep p = mkDependency (packageName p) (
        intersectVersionRanges (orLaterVersion (packageVersion p))
                               (earlierVersion (majorAndMinor (packageVersion p)))) (S.singleton LMainLibName)

    majorAndMinor v = mkVersion . nextMinor $ versionNumbers v
    nextMinor = nextMinor' . (++[0,0])
    nextMinor' (major:minor:_) = [major, minor+1]
    nextMinor' _ = undefined

getScopeForRef :: LogRef -> IDEM (Maybe (GenScope, GenScope))
getScopeForRef ref =
    selectSourceBuf (logRefFullFilePath ref) >>= \case
        Nothing -> return Nothing
        Just buf ->
            belongsToPackages' buf >>= \case
                [] -> return Nothing
                (_,pack'):_ -> getPackageImportInfo pack'

addImport' :: NotInScopeParseResult -> FilePath -> Descr -> [Descr] -> ((Bool,[Descr]) -> IDEAction) -> IDEAction
addImport' nis filePath descr descrList continuation =  do
    mbBuf  <- selectSourceBuf filePath
    let mbMod  = case dsMbModu descr of
                    Nothing -> Nothing
                    Just pm -> Just (modu pm)
    case (mbBuf,mbMod) of
        (Just _buf,Just mod') ->
            inActiveBufContext () $ \_ gtkbuf _ -> do
                ideMessage Normal $ "addImport " <> T.pack (show $ dscName descr) <> " from "
                    <> T.pack (prettyShow mod')
                belongsToPackages filePath >>= \case
                    [] -> return ()
                    (project, package):_ ->
                        doServerCommand (ParseHeaderCommand (pjKey project) (ipdCabalFile package) filePath) $ \case
                             ServerHeader (Left imports) ->
                                case filter (qualifyAsImportStatement mod') imports of
                                    []     ->   let newLine  =  prettyPrint (newImpDecl mod') <> "\n"
                                                    lastLine = F.foldr (max . locationELine . importLoc) 0 imports
                                                in do
                                                    i1 <- getIterAtLine gtkbuf lastLine
                                                    editInsertCode gtkbuf i1 newLine
                                                    _ <- fileSave False
                                                    setModified gtkbuf True
                                                    continuation (True, descr : descrList)
                                    (impDecl:_) ->
                                                    let newDecl     =  addToDecl impDecl
                                                        newLine     =  prettyPrint newDecl <> "\n"
                                                        myLoc       =  importLoc impDecl
                                                        lineStart   =  locationSLine myLoc
                                                        lineEnd     =  locationELine myLoc
                                                    in do
                                                        i1 <- getIterAtLine gtkbuf (lineStart - 1)
                                                        i2 <- getIterAtLine gtkbuf lineEnd
                                                        delete gtkbuf i1 i2
                                                        editInsertCode gtkbuf i1 newLine
                                                        _ <- fileSave False
                                                        setModified gtkbuf True
                                                        continuation (True, descr : descrList)
                             ServerHeader (Right lastLine) ->
                                                let newLine  =  prettyPrint (newImpDecl mod') <> "\n"
                                                in do
                                                    i1 <- getIterAtLine gtkbuf lastLine
                                                    editInsertCode gtkbuf i1 newLine
                                                    _ <- fileSave False
                                                    setModified gtkbuf True
                                                    continuation (True, descr : descrList)
                             ServerFailed msg -> do
                                ideMessage Normal ("Can't parse module header " <> T.pack filePath <>
                                        " failed with: " <> msg)
                                continuation (False,[])
                             _ ->    do
                                ideMessage Normal "ImportTool>>addImport: Impossible server answer"
                                continuation (False,[])
        _  -> return ()
    where
        qualifyAsImportStatement :: D.ModuleName -> ImportDecl -> Bool
        qualifyAsImportStatement moduleName impDecl =
            let importName = importModule impDecl
                getHiding (ImportSpecList isHiding _) = isHiding
            in importName == T.pack (display moduleName)
                && ((isNothing (mbQual' nis) &&  not (importQualified impDecl)) ||
                    (isJust (mbQual' nis) && importQualified impDecl
                        && fromJust (mbQual' nis) == qualString impDecl))
                && (isNothing (importSpecs impDecl) || not (getHiding (fromJust (importSpecs impDecl))))
        newImpDecl :: D.ModuleName -> ImportDecl
        newImpDecl mod' = ImportDecl {
                        importLoc       = noLocation,
                        importModule    = T.pack $ display mod',
                        importQualified = isJust (mbQual' nis),
                        importSrc       = False,
                        importPkg       = Nothing,
                        importAs        = mplus (mbQual' nis) Nothing,
                        importSpecs = Just (ImportSpecList False [newImportSpec])}
        newImportSpec :: ImportSpec
        newImportSpec =  getRealId descr (id' nis)
        addToDecl :: ImportDecl -> ImportDecl
        addToDecl impDecl = case importSpecs impDecl of
                                Just (ImportSpecList True _) -> throwIDE "ImportTool>>addToDecl: ImpList is hiding"
                                Just (ImportSpecList False listIE) ->
                                    impDecl{importSpecs = Just (ImportSpecList False (nub (newImportSpec : listIE)))}
                                Nothing             ->
                                    impDecl{importSpecs = Just (ImportSpecList False [newImportSpec])}
        noLocation  = Location "" 0 0 0 0

getRealId :: Descr -> Text -> ImportSpec
getRealId descr i = case descr of
    Reexported rdescr -> getRealId (dsrDescr rdescr) i
    Real edescr -> getReal (dscTypeHint' edescr)
    where
        getReal (FieldDescr d) = IThingAll (dscName d)
        getReal (ConstructorDescr d) = IThingAll (dscName d)
        getReal (MethodDescr d) = IThingAll (dscName d)
        getReal PatternSynonymDescr = IVar ("pattern " <> i)
        getReal _ = IVar i

qualString ::  ImportDecl -> Text
qualString impDecl = fromMaybe "" (importAs impDecl)

-- | The import data

data NotInScopeParseResult = NotInScopeParseResult {
        mbQual' ::   Maybe Text
    ,   id'     ::   Text
    ,   _isSub'  ::   Bool
    ,   _isOp'   ::   Bool}
    deriving Eq

-- |* The error line parser
lexer :: GenTokenParser String u Identity
lexer      = P.makeTokenParser haskellStyle
whiteSpace :: ParsecT String u Identity ()
whiteSpace = P.whiteSpace lexer
lexeme :: ParsecT String u Identity String -> ParsecT String u Identity Text
lexeme     = (T.pack <$>) . P.lexeme lexer
symbol :: String -> ParsecT String u Identity String
symbol     = P.symbol lexer
identifier :: ParsecT String u Identity Text
identifier = T.pack <$> P.identifier lexer
dot :: ParsecT String u Identity String
dot        = P.dot lexer
operator :: ParsecT String u Identity Text
operator   = T.pack <$> P.operator lexer

parseNotInScope :: Text -> Maybe NotInScopeParseResult
parseNotInScope str =
    case Parsec.parse scopeParser "" $ T.unpack str of
        Left _   -> Nothing
        Right r  -> Just r

scopeParser :: CharParser () NotInScopeParseResult
scopeParser = do whiteSpace
                 isSub <- (do
                    _ <- symbol "Not in scope:"
                    isJust <$> optionMaybe
                            (try
                               (choice
                                  [symbol "type constructor or class", symbol "data constructor"])))
                    <|>
                    (do void (optionMaybe $ symbol "•" >> whiteSpace)
                        (symbol "Variable not in scope:" >> return False)
                            <|> (symbol "Data constructor not in scope:" >> return True))
                 (do void (choice [char '\8219', char '\8216'])
                     mbQual <- optionMaybe
                                   (try
                                      (lexeme conid))
                     result <- optionMaybe (try identifier) >>= \case
                         Just id'' -> return
                                      (NotInScopeParseResult mbQual id'' isSub False)
                         Nothing -> do op <- operator
                                       return (NotInScopeParseResult mbQual op isSub True)
                     _ <- char '\8217'
                     return result)
                    <|>
                    (do whiteSpace
                        (do mbQual <- optionMaybe
                                   (try
                                      (lexeme conid))
                            result <- optionMaybe (try identifier) >>= \case
                                         Just id'' -> return
                                                      (NotInScopeParseResult mbQual id'' isSub False)
                                         Nothing -> do op <- operator
                                                       return (NotInScopeParseResult mbQual op isSub True)
                            _ <- many anyChar
                            return result)
                            <|> (do
                                _ <- char '('
                                op <- operator
                                _ <- char ')'
                                _ <- many anyChar
                                return (NotInScopeParseResult Nothing op isSub True)))
    <?> "scopeParser"

conid :: ParsecT String u Identity String
conid  = do
    c <-  upper
    cs <- many (alphaNum <|> oneOf "_'")
    _ <- dot
    return (c:cs)
        <?> "conid"


-- |* The little dialog to choose between possible modules

moduleFields :: [Text] -> Text -> FieldDescription Text
moduleFields list ident =
        mkField
            (paraName <<<- ParaName ("From which module is " <> ident)
                $ paraMultiSel <<<- ParaMultiSel False
                    $ paraMinSize <<<- ParaMinSize (300,400)
                        $ emptyParams)
            id
            const
            (staticListEditor list id)

selectModuleDialog :: Window -> [Descr] -> Text -> Maybe Text -> Maybe Descr -> IO (Maybe Descr)
selectModuleDialog parentWindow list id'' mbQual mbDescr =
    let selectionList       =  (nub . sort) $ map (T.pack . prettyShow . modu . fromJust . dsMbModu) list
    in if length selectionList == 1
        then return (Just (head list))
        else do
            let mbSelectedString    =  case mbDescr of
                                            Nothing -> Nothing
                                            Just descr -> case dsMbModu descr of
                                                            Nothing -> Nothing
                                                            Just pm -> Just ((T.pack . prettyShow . modu) pm)
            let realSelectionString =  case mbSelectedString of
                                            Nothing -> head selectionList
                                            Just str -> if str `elem` selectionList
                                                            then str
                                                            else head selectionList
            let qualId             =  case mbQual of
                                            Nothing -> id''
                                            Just str -> str <> "." <> id''
            dia               <- new' Dialog [constructDialogUseHeaderBar 1]
            setWindowTransientFor dia parentWindow
            upper'            <- dialogGetContentArea dia >>= unsafeCastTo Box
            okButton <- dialogAddButton' dia (__"Add Import") ResponseTypeOk
            _ <- dialogAddButton' dia (__"Cancel") ResponseTypeCancel
            (widget,_inj,ext,_) <- buildEditor (moduleFields selectionList qualId) realSelectionString
            boxPackStart' upper' widget PackGrow 7
            dialogSetDefaultResponse' dia ResponseTypeOk --does not work for the tree view
            widgetShowAll dia
            setWidgetCanDefault okButton True
            widgetGrabDefault okButton
            resp <- dialogRun' dia
            value                      <- ext T.empty
            widgetHide dia
            widgetDestroy dia
            --find
            case (resp,value) of
                (ResponseTypeOk,Just v)    -> return (Just (head
                                            (filter (\e -> case dsMbModu e of
                                                Nothing -> False
                                                Just pm -> (T.pack . prettyShow . modu) pm == v) list)))
                _                      -> return Nothing

data HiddenModuleResult = HiddenModuleResult {
        hiddenModule      :: Text
    ,   missingPackage    :: PackageId}
    deriving (Eq, Show)

parseHiddenModule :: Text -> Maybe HiddenModuleResult
parseHiddenModule str =
    case Parsec.parse hiddenModuleParser "" $ T.unpack str of
        Left _             -> Nothing
        Right (mod', pack')  ->
            case simpleParse $ T.unpack pack' of
                Just p  -> Just $ HiddenModuleResult mod' p
                Nothing -> Nothing

hiddenModuleParser :: CharParser () (Text, Text)
hiddenModuleParser = do
    whiteSpace
    _ <- symbol "Could not find module " <|> symbol "Failed to load interface for "
    _ <- char '`' <|> char '‛' <|> char '‘'
    mod'    <- T.pack <$> many (noneOf "'’")
    _ <- many (noneOf "\n")
    _ <- symbol "\n"
    whiteSpace
    _ <- symbol "It is a member of the hidden package "
    _ <- char '`' <|> char '‛' <|> char '‘'
    pack'   <- T.pack <$> many (noneOf "'’@")
    _ <- many (noneOf "'’")
    _ <- char '\'' <|> char '’'
    _ <- symbol ".\n"
    _ <- many anyChar
    return (mod', pack')
    <?> "hiddenModuleParser"

-- | Given an error message this returns the list of extensions that were
-- suggested in the message.
--
-- > parsePerhapsExt "Error blah blah\n   Perhaps you intended to use -XNoBugsExt\n\n"
--
-- > parsePerhapsExt "Error blah blah\n   Perhaps you intended to use ScopedTypeVariables\n\n"
--
-- > parsePerhapsExt "Error blah blah\n   Use -XNoBugsExt\n\n"
parsePerhapsYouIntendedToUse :: Text -> [KnownExtension]
parsePerhapsYouIntendedToUse =
    concatMap (parseLine . T.dropWhile (==' ')) . T.lines
  where
    parseLine :: Text -> [KnownExtension]
    parseLine "parse error on input ‘case’" = [LambdaCase]
    parseLine line = take 1 . mapMaybe readMaybe $ catMaybes [
        T.stripPrefix "Perhaps you intended to use -X" line
      , T.stripPrefix "Perhaps you intended to use " line
      , T.stripPrefix "Illegal tuple section: use " line
      , T.stripPrefix "Package-qualified imports are not enabled; use " line
      , T.stripPrefix "Type signatures are only allowed in patterns with " line
      , T.takeWhile (/=' ') <$> T.stripPrefix "Use -X" line
      , T.takeWhile (/=' ') <$> T.stripPrefix "Use " line
      , T.takeWhile (/=' ') <$> T.stripPrefix "(Use -X" line
      , T.takeWhile (/=' ') <$> T.stripPrefix "(Use " line
      , T.takeWhile (/=' ') <$> T.stripPrefix "You need -X" line
      , T.takeWhile (/=' ') <$> T.stripPrefix "You need " line]

addExtension :: LogRef -> (Bool -> IDEAction) -> IDEAction
addExtension err continuation =
    case parsePerhapsYouIntendedToUse $ refDescription err of
        []    -> continuation True
        [ext] -> addExtension' (T.pack $ show ext) (logRefFullFilePath err) continuation
        _     -> continuation True

addExtension' :: Text -> FilePath -> (Bool -> IDEAction) -> IDEAction
addExtension' ext filePath continuation =  do
    mbBuf  <- selectSourceBuf filePath
    case mbBuf of
        Just _buf ->
            inActiveBufContext () $ \_ gtkbuf _idebuf -> do
                ideMessage Normal $ "addExtension " <> ext
                i1 <- getIterAtLine gtkbuf 0
                editInsertCode gtkbuf i1 $ "{-# LANGUAGE " <> ext <> " #-}\n"
                _ <- fileSave False
                setModified gtkbuf True
                continuation True
        _  -> return ()

resolveMenuItems :: LogRef -> [(Text, IDEAction)]
resolveMenuItems logRef
    | isJust (parseNotInScope msg)   = [("Add Import", addImport logRef [] (\ _ -> return ()))]
    | isJust (parseHiddenModule msg) = [("Add Package", void $ addPackages [logRef])]
    | length (parsePerhapsYouIntendedToUse msg) == 1 = [("Add Extension", addExtension logRef (\ _ -> return ()))]
    | otherwise = []
    where msg = refDescription logRef

addResolveMenuItems
  :: (MonadIO m, IsMenuShell a)
  => IDERef
  -> a
  -> LogRef
  -> m ()
addResolveMenuItems ideR theMenu logRef = do
    let msg = refDescription logRef
    when (isJust $ parseNotInScope msg) $
        addFixMenuItem (__"Add Import")  $ addImport logRef [] (\ _ -> return ())
    when (isJust $ parseHiddenModule msg) $
        addFixMenuItem (__"Add Package") $ addPackages [logRef]
    when (length (parsePerhapsYouIntendedToUse msg) == 1) $
        addFixMenuItem (__"Add Extension") $ addExtension logRef (\ _ -> return ())
  where
    addFixMenuItem name fix = do
        item <- menuItemNewWithLabel name
        _ <- onMenuItemActivate item $ reflectIDE (void fix) ideR
        menuShellAppend theMenu item

