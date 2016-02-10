{-# LANGUAGE OverloadedStrings #-}
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

import IDE.Core.State
import Data.Maybe
       (mapMaybe, fromMaybe, catMaybes, fromJust, isNothing, isJust)
import IDE.Metainfo.Provider
       (getWorkspaceInfo, getPackageImportInfo, getIdentifierDescr)
import Text.PrettyPrint (render)
import Distribution.Text (simpleParse, display, disp)
import IDE.Pane.SourceBuffer
import IDE.HLint (resolveActiveHLint)
import Graphics.UI.Gtk
import IDE.Utils.GUIUtils
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Graphics.UI.Editor.MakeEditor
       (getRealWidget, FieldDescription(..), buildEditor, mkField)
import Graphics.UI.Editor.Parameters
       ((<<<-), paraMinSize, emptyParams, Parameter(..), paraMultiSel,
        paraName)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Graphics.UI.Editor.Simple (staticListEditor)
import Control.Monad
       (forM_, void, MonadPlus(..), unless, forM, when)
import Control.Applicative ((<$>))
import Data.List (stripPrefix, sort, nub, nubBy)
import IDE.Utils.ServerConnection
import Text.PrinterParser (prettyPrint)
import IDE.TextEditor (delete, setModified, insert, getIterAtLine)
import qualified Distribution.ModuleName as D (ModuleName(..))
import qualified Text.ParserCombinators.Parsec.Token as P
       (operator, dot, identifier, symbol, lexeme, whiteSpace,
        makeTokenParser)
import Distribution.PackageDescription.Parse
       (readPackageDescription)
import Distribution.Verbosity (normal)
import IDE.Pane.PackageEditor (hasConfigs)
import Distribution.Package
import Distribution.Version
       (anyVersion, orLaterVersion, intersectVersionRanges,
        earlierVersion, Version(..))
import Distribution.PackageDescription
       (GenericPackageDescription(..), Benchmark(..), TestSuite(..),
        Executable(..), BuildInfo(..), Library(..), CondTree(..),
        condExecutables, condLibrary, packageDescription, buildDepends)
import Distribution.PackageDescription.Configuration
       (flattenPackageDescription)
import IDE.BufferMode (editInsertCode)
import Control.Monad.IO.Class (MonadIO(..))
import Distribution.PackageDescription.PrettyPrint
       (writeGenericPackageDescription)
import qualified Data.Text as T
       (takeWhile, stripPrefix, lines, dropWhile, empty, length, take,
        pack, unpack)
import Language.Haskell.Exts (KnownExtension)
import Data.Text (Text)
import Data.Monoid ((<>))
import System.Log.Logger (debugM)
import qualified Data.Traversable as Tr (forM)
import qualified Data.Foldable as F (toList, foldr, or)

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
        modifyIDE_ (\ide -> ide{prefs = prefs'{backgroundBuild = False}})
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
            when buildInBackground $ do
                prefs' <- readIDE prefs
                modifyIDE_ (\ide -> ide{prefs = prefs'{backgroundBuild = True}})
            when (not addPackageResult && null notInScopes && null extensions) $ do
                hlintResolved <- resolveActiveHLint
                unless hlintResolved $ ideMessage Normal "No errors, warnings or selected hlints that can be auto resolved"

-- | Add import for current error ...
addOneImport :: IDEAction
addOneImport = do
    errors'     <- readIDE errorRefs
    currentErr' <- readIDE currentError
    case currentErr' of
        Nothing -> do
            ideMessage Normal "No error selected"
            return ()
        Just ref -> addImport ref [] (\ _ -> return ())

-- | Add one missing import
-- Returns a boolean, if the process should be stopped in case of multiple addition
-- Returns a list of already added descrs, so that it will not be added two times and can
-- be used for default selection
addImport :: LogRef -> [Descr] -> ((Bool,[Descr]) -> IDEAction) -> IDEAction
addImport error descrList continuation =
    case parseNotInScope $ refDescription error of
        Nothing -> continuation (True,descrList)
        Just nis -> do
            currentInfo' <- getScopeForActiveBuffer
            wsInfo' <- getWorkspaceInfo
            case (currentInfo', wsInfo') of
                (Nothing, _) -> continuation (True,descrList)
                (_, Nothing) -> continuation (True,descrList)
                (Just (GenScopeC(PackScope _ symbolTable1),GenScopeC(PackScope _ symbolTable2)),
                    Just (GenScopeC(PackScope _ symbolTable3),GenScopeC(PackScope _ symbolTable4))) ->
                    let list = getIdentifierDescr (id' nis) symbolTable1 symbolTable2
                        wslist = getIdentifierDescr (id' nis) symbolTable3 symbolTable4
                    in case (list, wslist) of
                        ([], []) ->  do
                            ideMessage Normal $ "Identifier " <> id' nis <> " not found"
                            continuation (True, descrList)
                        ([], list) -> do
                            window' <- getMainWindow
                            mbDescr <-  liftIO $ selectModuleDialog window' list (id' nis) (mbQual' nis)
                                            (if null descrList
                                                then Nothing
                                                else Just (head descrList))
                            case mbDescr of
                                Nothing     ->  continuation (False, [])
                                Just descr  ->  if descr `elem` descrList
                                                    then continuation (True,descrList)
                                                    else addImport' nis (logRefFullFilePath error)
                                                            descr descrList continuation
                        ([descr], _)  ->  addImport' nis (logRefFullFilePath error) descr descrList continuation
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
                                                    else addImport' nis (logRefFullFilePath error)
                                                            descr descrList continuation

addPackages :: [LogRef] -> IDEM Bool
addPackages errors = do
    let packs = nub $ mapMaybe (\error ->
                    case parseHiddenModule $ refDescription error of
                        Nothing -> Nothing
                        Just (HiddenModuleResult _ pack) -> Just (ipdCabalFile (logRefPackage error), dep pack)) errors

    forM_ packs $ \(cabalFile, d) -> do
        gpd <- liftIO $ readPackageDescription normal cabalFile
        ideMessage Normal $ "Adding build-depends " <> T.pack (display d <> " to " <> cabalFile)
        liftIO $ writeGenericPackageDescription cabalFile
            gpd { condLibrary     = addDepToLib d (condLibrary gpd),
                  condExecutables = map (addDepToExe d)
                                        (condExecutables gpd),
                  condTestSuites  = map (addDepToTest d)
                                        (condTestSuites gpd),
                  condBenchmarks  = map (addDepToBenchmark d)
                                        (condBenchmarks gpd)}
        return True

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
    dep p | null . versionBranch $ packageVersion p = Dependency (packageName p) anyVersion
    dep p = Dependency (packageName p) (
        intersectVersionRanges (orLaterVersion (packageVersion p))
                               (earlierVersion (majorAndMinor (packageVersion p))))

    majorAndMinor v@Version{versionBranch = b} = v{versionBranch = nextMinor b}
    nextMinor = nextMinor' . (++[0,0])
    nextMinor' (major:minor:_) = [major, minor+1]
    nextMinor' _ = undefined

getScopeForActiveBuffer :: IDEM (Maybe (GenScope, GenScope))
getScopeForActiveBuffer = do
    mbActiveBuf <- maybeActiveBuf
    case mbActiveBuf of
        Nothing -> return Nothing
        Just buf -> do
            packages <- belongsToPackages' buf
            case packages of
                [] -> return Nothing
                pack:_ -> getPackageImportInfo pack

addImport' :: NotInScopeParseResult -> FilePath -> Descr -> [Descr] -> ((Bool,[Descr]) -> IDEAction) -> IDEAction
addImport' nis filePath descr descrList continuation =  do
    mbBuf  <- selectSourceBuf filePath
    let mbMod  = case dsMbModu descr of
                    Nothing -> Nothing
                    Just pm -> Just (modu pm)
    case (mbBuf,mbMod) of
        (Just buf,Just mod) ->
            inActiveBufContext () $ \ nb _ gtkbuf idebuf n -> do
                ideMessage Normal $ "addImport " <> T.pack (show $ dscName descr) <> " from "
                    <> T.pack (render $ disp mod)
                doServerCommand (ParseHeaderCommand filePath)  $ \ res ->
                    case res of
                         ServerHeader (Left imports) ->
                            case filter (qualifyAsImportStatement mod) imports of
                                []     ->   let newLine  =  prettyPrint (newImpDecl mod) <> "\n"
                                                lastLine = F.foldr (max . locationELine . importLoc) 0 imports
                                            in do
                                                i1 <- getIterAtLine gtkbuf lastLine
                                                editInsertCode gtkbuf i1 newLine
                                                fileSave False
                                                setModified gtkbuf True
                                                continuation (True, descr : descrList)
                                l@(impDecl:_) ->
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
                                                    fileSave False
                                                    setModified gtkbuf True
                                                    continuation (True, descr : descrList)
                         ServerHeader (Right lastLine) ->
                                            let newLine  =  prettyPrint (newImpDecl mod) <> "\n"
                                            in do
                                                i1 <- getIterAtLine gtkbuf lastLine
                                                editInsertCode gtkbuf i1 newLine
                                                fileSave False
                                                setModified gtkbuf True
                                                continuation (True, descr : descrList)
                         ServerFailed string -> do
                            ideMessage Normal ("Can't parse module header " <> T.pack filePath <>
                                    " failed with: " <> string)
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
        newImpDecl mod = ImportDecl {
                        importLoc       = noLocation,
                        importModule    = T.pack $ display mod,
                        importQualified = isJust (mbQual' nis),
                        importSrc       = False,
                        importPkg       = Nothing,
                        importAs        = mplus (mbQual' nis) Nothing,
                        importSpecs = Just (ImportSpecList False [newImportSpec])}
        newImportSpec :: ImportSpec
        newImportSpec =  getRealId descr (id' nis)
        addToDecl :: ImportDecl -> ImportDecl
        addToDecl impDecl = case importSpecs impDecl of
                                Just (ImportSpecList True listIE)  -> throwIDE "ImportTool>>addToDecl: ImpList is hiding"
                                Just (ImportSpecList False listIE) ->
                                    impDecl{importSpecs = Just (ImportSpecList False (nub (newImportSpec : listIE)))}
                                Nothing             ->
                                    impDecl{importSpecs = Just (ImportSpecList False [newImportSpec])}
        noLocation  = Location "" 0 0 0 0

getRealId descr id = case descr of
    Reexported rdescr -> getRealId (dsrDescr rdescr) id
    Real edescr -> getReal (dscTypeHint' edescr)
    where
        getReal (FieldDescr d) = IThingAll (dscName d)
        getReal (ConstructorDescr d) = IThingAll (dscName d)
        getReal (MethodDescr d) = IThingAll (dscName d)
        getReal _ = IVar id

qualString ::  ImportDecl -> Text
qualString impDecl = fromMaybe "" (importAs impDecl)

-- | The import data

data NotInScopeParseResult = NotInScopeParseResult {
        mbQual' ::   Maybe Text
    ,   id'     ::   Text
    ,   isSub'  ::   Bool
    ,   isOp'   ::   Bool}
    deriving Eq

-- |* The error line parser

lexer      = P.makeTokenParser haskellStyle
whiteSpace = P.whiteSpace lexer
lexeme     = (T.pack <$>) . P.lexeme lexer
symbol     = P.symbol lexer
identifier = T.pack <$> P.identifier lexer
dot        = P.dot lexer
operator   = T.pack <$> P.operator lexer

parseNotInScope :: Text -> Maybe NotInScopeParseResult
parseNotInScope str =
    case Parsec.parse scopeParser "" $ T.unpack str of
        Left e   -> Nothing
        Right r  -> Just r

scopeParser :: CharParser () NotInScopeParseResult
scopeParser = do whiteSpace
                 symbol "Not in scope:"
                 isSub <- optionMaybe
                            (try
                               (choice
                                  [symbol "type constructor or class", symbol "data constructor"]))
                 (do char '`'
                     mbQual <- optionMaybe
                                 (try
                                    (do q <- lexeme conid
                                        dot
                                        return q))
                     id <- optionMaybe (try identifier)
                     case id of
                         Just id -> return
                                      (NotInScopeParseResult mbQual (T.take (T.length id - 1) id)
                                         (isJust isSub)
                                         False)
                         Nothing -> do op <- operator
                                       char '\''
                                       return (NotInScopeParseResult mbQual op (isJust isSub) True))
                   <|>
                   (do choice [char '\8219', char '\8216']
                       mbQual <- optionMaybe
                                   (try
                                      (do q <- lexeme conid
                                          dot
                                          return q))
                       id <- optionMaybe (try identifier)
                       result <- case id of
                                     Just id -> return
                                                  (NotInScopeParseResult mbQual id (isJust isSub) False)
                                     Nothing -> do op <- operator
                                                   return (NotInScopeParseResult mbQual op (isJust isSub) True)
                       char '\8217'
                       return result)
    <?> "scopeParser"

conid  = do
    c <-  upper
    cs <- many (alphaNum <|> oneOf "_'")
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
selectModuleDialog parentWindow list id mbQual mbDescr =
    let selectionList       =  (nub . sort) $ map (T.pack . render . disp . modu . fromJust . dsMbModu) list
    in if length selectionList == 1
        then return (Just (head list))
        else do
            let mbSelectedString    =  case mbDescr of
                                            Nothing -> Nothing
                                            Just descr -> case dsMbModu descr of
                                                            Nothing -> Nothing
                                                            Just pm -> Just ((T.pack . render . disp . modu) pm)
            let realSelectionString =  case mbSelectedString of
                                            Nothing -> head selectionList
                                            Just str -> if str `elem` selectionList
                                                            then str
                                                            else head selectionList
            let qualId             =  case mbQual of
                                            Nothing -> id
                                            Just str -> str <> "." <> id
            dia               <- dialogNew
            set dia [ windowTransientFor := parentWindow ]
            upper             <- dialogGetContentArea dia
            okButton <- dialogAddButton dia (__"Add Import") ResponseOk
            dialogAddButton dia (__"Cancel") ResponseCancel
            (widget,inj,ext,_) <- buildEditor (moduleFields selectionList qualId) realSelectionString
            boxPackStart (castToBox upper) widget PackGrow 7
            dialogSetDefaultResponse dia ResponseOk --does not work for the tree view
            widgetShowAll dia
            rw <- getRealWidget widget
            set okButton [widgetCanDefault := True]
            widgetGrabDefault okButton
            resp <- dialogRun dia
            value                      <- ext T.empty
            widgetHide dia
            widgetDestroy dia
            --find
            case (resp,value) of
                (ResponseOk,Just v)    -> return (Just (head
                                            (filter (\e -> case dsMbModu e of
                                                Nothing -> False
                                                Just pm -> (T.pack . render . disp . modu) pm == v) list)))
                _                      -> return Nothing

data HiddenModuleResult = HiddenModuleResult {
        hiddenModule      :: Text
    ,   missingPackage    :: PackageId}
    deriving (Eq, Show)

parseHiddenModule :: Text -> Maybe HiddenModuleResult
parseHiddenModule str =
    case Parsec.parse hiddenModuleParser "" $ T.unpack str of
        Left e             -> Nothing
        Right (mod, pack)  ->
            case simpleParse $ T.unpack pack of
                Just p  -> Just $ HiddenModuleResult mod p
                Nothing -> Nothing

hiddenModuleParser :: CharParser () (Text, Text)
hiddenModuleParser = do
    whiteSpace
    symbol "Could not find module "
    char '`' <|> char '‛' <|> char '‘'
    mod    <- T.pack <$> many (noneOf "'’")
    many (noneOf "\n")
    symbol "\n"
    whiteSpace
    symbol "It is a member of the hidden package "
    char '`' <|> char '‛' <|> char '‘'
    pack   <- T.pack <$> many (noneOf "'’@")
    many (noneOf "'’")
    char '\'' <|> char '’'
    symbol ".\n"
    many anyChar
    return (mod, pack)
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
    parseLine line = take 1 . mapMaybe readMaybe $ catMaybes [
        T.stripPrefix "Perhaps you intended to use -X" line
      , T.stripPrefix "Perhaps you intended to use " line
      , T.takeWhile (/=' ') <$> T.stripPrefix "Use -X" line
      , T.takeWhile (/=' ') <$> T.stripPrefix "Use " line
      , T.takeWhile (/=' ') <$> T.stripPrefix "(Use -X" line
      , T.takeWhile (/=' ') <$> T.stripPrefix "(Use " line
      , T.takeWhile (/=' ') <$> T.stripPrefix "You need -X" line
      , T.takeWhile (/=' ') <$> T.stripPrefix "You need " line]

addExtension :: LogRef -> (Bool -> IDEAction) -> IDEAction
addExtension error continuation =
    case parsePerhapsYouIntendedToUse $ refDescription error of
        []    -> continuation True
        [ext] -> addExtension' (T.pack $ show ext) (logRefFullFilePath error) continuation
        list  -> continuation True

addExtension' :: Text -> FilePath -> (Bool -> IDEAction) -> IDEAction
addExtension' ext filePath continuation =  do
    mbBuf  <- selectSourceBuf filePath
    case mbBuf of
        Just buf ->
            inActiveBufContext () $ \ nb _ gtkbuf idebuf n -> do
                ideMessage Normal $ "addExtension " <> ext
                i1 <- getIterAtLine gtkbuf 0
                editInsertCode gtkbuf i1 $ "{-# LANGUAGE " <> ext <> " #-}\n"
                fileSave False
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
        item `on` menuItemActivate $ reflectIDE (void fix) ideR
        menuShellAppend theMenu item

