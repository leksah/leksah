{-# LANGUAGE CPP #-}
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
,   parseHiddenModule
,   HiddenModuleResult(..)
) where

import IDE.Core.State
import Data.Maybe (isNothing,isJust)
import IDE.Metainfo.Provider
       (getWorkspaceInfo, getPackageImportInfo, getIdentifierDescr)
import Text.PrettyPrint (render)
import Distribution.Text (simpleParse, display, disp)
import IDE.Pane.SourceBuffer
import Graphics.UI.Gtk
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Graphics.UI.Editor.MakeEditor
       (getRealWidget, FieldDescription(..), buildEditor, mkField)
import Graphics.UI.Editor.Parameters
       ((<<<-), paraMinSize, emptyParams, Parameter(..), paraMultiSel,
        paraName)
import Data.Maybe (catMaybes, fromJust)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Graphics.UI.Editor.Simple (staticListEditor)
import Control.Monad (forM, when)
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
       (CondTree(..), condExecutables, condLibrary, packageDescription,
        buildDepends)
import Distribution.PackageDescription.Configuration
       (flattenPackageDescription)
import IDE.BufferMode (editInsertCode)
import Control.Monad.IO.Class (MonadIO(..))
import Distribution.PackageDescription.PrettyPrint
       (writeGenericPackageDescription)
import qualified Data.Text as T (unpack)
import Language.Haskell.Exts (KnownExtension)
import Text.Read (readMaybe)

-- | Add all imports which gave error messages ...
resolveErrors :: IDEAction
resolveErrors = do
    prefs' <- readIDE prefs
    let buildInBackground = backgroundBuild prefs'
    when buildInBackground $
        modifyIDE_ (\ide -> ide{prefs = prefs'{backgroundBuild = False}})
    errors <- readIDE errorRefs
    addPackageResults <- forM errors addPackage
    let notInScopes = [ y | (x,y) <-
            nubBy (\ (p1,_) (p2,_) -> p1 == p2)
                $ [(x,y) |  (x,y) <- [((parseNotInScope . T.unpack . refDescription) e, e) | e <- errors]],
                                isJust x]
    let extensions = [ y | (x,y) <-
            nubBy (\ (p1,_) (p2,_) -> p1 == p2)
                $ [(x,y) |  (x,y) <- [((parsePerhapsYouIntendedToUse . T.unpack . refDescription) e, e) | e <- errors]],
                                length x == 1]
    when (not (or addPackageResults) && null notInScopes && null extensions) $ ideMessage Normal $ "No errors that can be auto resolved"
    addAll buildInBackground notInScopes extensions
  where
    addAll buildInBackground notInScopes extensions = addAllImports notInScopes (True,[])
      where
        addAllImports :: [LogRef] -> (Bool,[Descr]) -> IDEM ()
        addAllImports (errorSpec:rest) (True,descrList)  =  addImport errorSpec descrList (addAllImports rest)
        addAllImports _ (cont, _)                        =  addAllExtensions extensions cont

        addAllExtensions :: [LogRef] -> Bool -> IDEM ()
        addAllExtensions (errorSpec:rest) True =  addExtension errorSpec (addAllExtensions rest)
        addAllExtensions _ _                   =  finally

        finally = when buildInBackground $ do
            prefs' <- readIDE prefs
            modifyIDE_ (\ide -> ide{prefs = prefs'{backgroundBuild = True}})

-- | Add import for current error ...
addOneImport :: IDEAction
addOneImport = do
    errors'     <- readIDE errorRefs
    currentErr' <- readIDE currentError
    case currentErr' of
        Nothing -> do
            ideMessage Normal $ "No error selected"
            return ()
        Just ref -> addImport ref [] (\ _ -> return ())

-- | Add one missing import
-- Returns a boolean, if the process should be stopped in case of multiple addition
-- Returns a list of already added descrs, so that it will not be added two times and can
-- be used for default selection
addImport :: LogRef -> [Descr] -> ((Bool,[Descr]) -> IDEAction) -> IDEAction
addImport error descrList continuation =
    case parseNotInScope . T.unpack $ refDescription error of
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
                            ideMessage Normal $ "Identifier " ++ (id' nis) ++ " not found"
                            continuation (True, descrList)
                        ([], list) -> do
                            window' <- getMainWindow
                            mbDescr <-  liftIO $ selectModuleDialog window' list (id' nis) (mbQual' nis)
                                            (if null descrList
                                                then Nothing
                                                else Just (head descrList))
                            case mbDescr of
                                Nothing     ->  continuation (False, [])
                                Just descr  ->  if elem descr descrList
                                                    then continuation (True,descrList)
                                                    else addImport' nis (logRefFullFilePath error)
                                                            descr descrList continuation
                        (descr : [], _)  ->  addImport' nis (logRefFullFilePath error) descr descrList continuation
                        _ ->  do
                            let fullList = list ++ wslist
                            window' <- getMainWindow
                            mbDescr <-  liftIO $ selectModuleDialog window' fullList (id' nis) (mbQual' nis)
                                            (if null descrList
                                                then Nothing
                                                else Just (head descrList))
                            case mbDescr of
                                Nothing     ->  continuation (False, [])
                                Just descr  ->  if elem descr descrList
                                                    then continuation (True,descrList)
                                                    else addImport' nis (logRefFullFilePath error)
                                                            descr descrList continuation

addPackage :: LogRef -> IDEM Bool
addPackage error = do
    case parseHiddenModule . T.unpack $ refDescription error of
        Nothing -> return False
        Just (HiddenModuleResult mod pack) -> do
            let idePackage = logRefPackage error
            gpd <- liftIO $ readPackageDescription normal (ipdCabalFile $ idePackage)
            ideMessage Normal $ "addPackage " ++ (display $ pkgName pack)
            liftIO $ writeGenericPackageDescription (ipdCabalFile $ idePackage)
                gpd { condLibrary     = addDepToLib pack (condLibrary gpd),
                      condExecutables = map (addDepToExe pack)
                                            (condExecutables gpd)}
            return True
  where
    addDepToLib _ Nothing = Nothing
    addDepToLib p (Just cn@CondNode{condTreeConstraints = deps}) =
        Just (cn{condTreeConstraints = dep p : deps})
    addDepToExe p (str,cn@CondNode{condTreeConstraints = deps}) =
        (str,cn{condTreeConstraints = dep p : deps})
    -- Empty version is probably only going to happen for ghc-prim
    dep p | null . versionBranch $ packageVersion p = Dependency (packageName p) (anyVersion)
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
            packages <- belongsToPackages buf
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
        (Just buf,Just mod) -> do
            inActiveBufContext () $ \ nb _ gtkbuf idebuf n -> do
                ideMessage Normal $ "addImport " ++ show (dscName descr) ++ " from "
                    ++ (render $ disp $ mod)
                doServerCommand (ParseHeaderCommand filePath)  $ \ res ->
                    case res of
                         ServerHeader (Left imports) ->
                            case filter (qualifyAsImportStatement mod) imports of
                                []     ->   let newLine  =  prettyPrint (newImpDecl mod) ++ "\n"
                                                lastLine = foldr max 0 (map (locationELine . importLoc) imports)
                                            in do
                                                i1 <- getIterAtLine gtkbuf lastLine
                                                editInsertCode gtkbuf i1 newLine
                                                fileSave False
                                                setModified gtkbuf True
                                                continuation (True,(descr : descrList))
                                l@(impDecl:_) ->
                                                let newDecl     =  addToDecl impDecl
                                                    newLine     =  prettyPrint newDecl ++ "\n"
                                                    myLoc       =  importLoc impDecl
                                                    lineStart   =  locationSLine myLoc
                                                    lineEnd     =  locationELine myLoc
                                                in do
                                                    i1 <- getIterAtLine gtkbuf (lineStart - 1)
                                                    i2 <- getIterAtLine gtkbuf (lineEnd)
                                                    delete gtkbuf i1 i2
                                                    editInsertCode gtkbuf i1 newLine
                                                    fileSave False
                                                    setModified gtkbuf True
                                                    continuation (True,(descr : descrList))
                         ServerHeader (Right lastLine) ->
                                            let newLine  =  prettyPrint (newImpDecl mod) ++ "\n"
                                            in do
                                                i1 <- getIterAtLine gtkbuf lastLine
                                                editInsertCode gtkbuf i1 newLine
                                                fileSave False
                                                setModified gtkbuf True
                                                continuation (True,(descr : descrList))
                         ServerFailed string -> do
                            ideMessage Normal ("Can't parse module header " ++ filePath ++
                                    " failed with: " ++ string)
                            continuation (False,[])
                         _ ->    do
                            ideMessage Normal ("ImportTool>>addImport: Impossible server answer")
                            continuation (False,[])
        _  -> return ()
    where
        qualifyAsImportStatement :: D.ModuleName -> ImportDecl -> Bool
        qualifyAsImportStatement moduleName impDecl =
            let importName = importModule impDecl
                getHiding (ImportSpecList isHiding _) = isHiding
            in importName == display moduleName
                && ((isNothing (mbQual' nis) &&  not (importQualified impDecl)) ||
                    (isJust (mbQual' nis) && importQualified impDecl
                        && fromJust (mbQual' nis) == qualString impDecl))
                && (isNothing (importSpecs impDecl) || not (getHiding (fromJust (importSpecs impDecl))))
        newImpDecl :: D.ModuleName -> ImportDecl
        newImpDecl mod = ImportDecl {
                        importLoc       = noLocation,
                        importModule    = display mod,
                        importQualified = isJust (mbQual' nis),
                        importSrc       = False,
                        importPkg       = Nothing,
                        importAs        = if isJust (mbQual' nis)
                                            then Just (fromJust (mbQual' nis))
                                            else Nothing,
                        importSpecs = (Just (ImportSpecList False [newImportSpec]))}
        newImportSpec :: ImportSpec
        newImportSpec =  getRealId descr (id' nis)
        addToDecl :: ImportDecl -> ImportDecl
        addToDecl impDecl = case importSpecs impDecl of
                                Just (ImportSpecList True listIE)  -> throwIDE "ImportTool>>addToDecl: ImpList is hiding"
                                Just (ImportSpecList False listIE) ->
                                    impDecl{importSpecs = Just (ImportSpecList False (nub (newImportSpec : listIE)))}
                                Nothing             ->
                                    impDecl{importSpecs = Just (ImportSpecList False [newImportSpec])}
        noLocation  = Location 0 0 0 0

getRealId descr id = case descr of
    Reexported rdescr -> getRealId (dsrDescr rdescr) id
    Real edescr -> getReal (dscTypeHint' edescr)
    where
        getReal (FieldDescr d) = IThingAll (dscName d)
        getReal (ConstructorDescr d) = IThingAll (dscName d)
        getReal (MethodDescr d) = IThingAll (dscName d)
        getReal _ = IVar id

qualString ::  ImportDecl -> String
qualString impDecl = case importAs impDecl of
                        Nothing -> ""
                        Just modName -> modName

-- | The import data

data NotInScopeParseResult = NotInScopeParseResult {
        mbQual' ::   Maybe String
    ,   id'     ::   String
    ,   isSub'  ::   Bool
    ,   isOp'   ::   Bool}
    deriving Eq

-- |* The error line parser

lexer      = P.makeTokenParser haskellStyle
whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
identifier = P.identifier lexer
dot        = P.dot lexer
operator   = P.operator lexer

parseNotInScope :: String -> (Maybe NotInScopeParseResult)
parseNotInScope str =
    case Parsec.parse scopeParser "" str of
        Left e   -> Nothing
        Right r  -> Just r

scopeParser :: CharParser () NotInScopeParseResult
scopeParser = do
    whiteSpace
    symbol "Not in scope:"
    isSub   <- optionMaybe (try (choice [symbol "type constructor or class"
                    , symbol "data constructor"]))
    (   (do
            char '`'
            mbQual <- optionMaybe (try (do
                q  <- lexeme conid
                dot
                return q))
            id     <- optionMaybe (try identifier)
            case id of
                Just id -> return (NotInScopeParseResult mbQual
                                (take (length id - 1) id)  (isJust isSub) False)
                Nothing -> do
                    op <-   operator
                    char '\''
                    return (NotInScopeParseResult mbQual op (isJust isSub) True))
       <|> (do
            choice [char '‛', char '‘']
            mbQual <- optionMaybe (try (do
                q  <- lexeme conid
                dot
                return q))
            id     <- optionMaybe (try identifier)
            result <- case id of
                Just id -> return (NotInScopeParseResult mbQual
                                id  (isJust isSub) False)
                Nothing -> do
                    op <-   operator
                    return (NotInScopeParseResult mbQual op (isJust isSub) True)
            char '’'
            return result))
    <?> "scopeParser"

conid  = do
    c <-  upper
    cs <- many (alphaNum <|> oneOf "_'")
    return (c:cs)
        <?> "conid"


-- |* The little dialog to choose between possible modules

moduleFields :: [String] -> String -> FieldDescription String
moduleFields list ident =
        mkField
            (paraName <<<- ParaName ("From which module is " ++ ident)
                $ paraMultiSel <<<- ParaMultiSel False
                    $ paraMinSize <<<- ParaMinSize (300,400)
                        $ emptyParams)
            (\ a -> a)
            (\ a b -> a)
            (staticListEditor ( list) id)

selectModuleDialog :: Window -> [Descr] -> String -> Maybe String -> Maybe Descr -> IO (Maybe Descr)
selectModuleDialog parentWindow list id mbQual mbDescr =
    let selectionList       =  (nub . sort) $ map (render . disp . modu . fromJust . dsMbModu) list
    in if length selectionList == 1
        then return (Just (head list))
        else do
            let mbSelectedString    =  case mbDescr of
                                            Nothing -> Nothing
                                            Just descr -> case dsMbModu descr of
                                                            Nothing -> Nothing
                                                            Just pm -> Just ((render . disp . modu) pm)
            let realSelectionString =  case mbSelectedString of
                                            Nothing -> head selectionList
                                            Just str -> if elem str selectionList
                                                            then str
                                                            else head selectionList
            let qualId             =  case mbQual of
                                            Nothing -> id
                                            Just str -> str ++ "." ++ id
            dia               <- dialogNew
            set dia [ windowTransientFor := parentWindow ]
#ifdef MIN_VERSION_gtk3
            upper             <- dialogGetContentArea dia
#else
            upper             <- dialogGetUpper dia
#endif
            okButton <- dialogAddButton dia "Ok" ResponseOk
            dialogAddButton dia "Cancel" ResponseCancel
            (widget,inj,ext,_) <- buildEditor (moduleFields selectionList qualId) realSelectionString
            boxPackStart (castToBox upper) widget PackGrow 7
            dialogSetDefaultResponse dia ResponseOk --does not work for the tree view
            widgetShowAll dia
            rw <- getRealWidget widget
            set okButton [widgetCanDefault := True]
            widgetGrabDefault okButton
            resp <- dialogRun dia
            value                      <- ext ([])
            widgetHide dia
            widgetDestroy dia
            --find
            case (resp,value) of
                (ResponseOk,Just v)    -> return (Just (head
                                            (filter (\e -> case dsMbModu e of
                                                Nothing -> False
                                                Just pm -> (render . disp . modu) pm == v) list)))
                _                      -> return Nothing

data HiddenModuleResult = HiddenModuleResult {
        hiddenModule      :: String
    ,   missingPackage    :: PackageId}
    deriving (Eq, Show)

parseHiddenModule :: String -> (Maybe HiddenModuleResult)
parseHiddenModule str =
    case Parsec.parse hiddenModuleParser "" str of
        Left e             -> Nothing
        Right (mod, pack)  ->
            case simpleParse pack of
                Just p  -> Just $ HiddenModuleResult mod p
                Nothing -> Nothing

hiddenModuleParser :: CharParser () (String, String)
hiddenModuleParser = do
    whiteSpace
    symbol "Could not find module "
    (char '`' <|> char '‛' <|> char '‘')
    mod    <- many (noneOf "'’")
    many (noneOf "\n")
    symbol "\n"
    whiteSpace
    symbol "It is a member of the hidden package "
    (char '`' <|> char '‛' <|> char '‘')
    pack   <- many (noneOf "'’")
    (char '\'' <|> char '’')
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
parsePerhapsYouIntendedToUse :: String -> [KnownExtension]
parsePerhapsYouIntendedToUse =
    concatMap (parseLine . dropWhile (==' ')) . lines
  where
    parseLine :: String -> [KnownExtension]
    parseLine line = take 1 . catMaybes . map readMaybe $ catMaybes [
        stripPrefix "Perhaps you intended to use -X" line
      , stripPrefix "Perhaps you intended to use " line
      , takeWhile (/=' ') <$> stripPrefix "Use -X" line
      , takeWhile (/=' ') <$> stripPrefix "Use " line
      , takeWhile (/=' ') <$> stripPrefix "(Use -X" line
      , takeWhile (/=' ') <$> stripPrefix "(Use " line
      , takeWhile (/=' ') <$> stripPrefix "You need -X" line
      , takeWhile (/=' ') <$> stripPrefix "You need " line]

addExtension :: LogRef -> (Bool -> IDEAction) -> IDEAction
addExtension error continuation =
    case parsePerhapsYouIntendedToUse . T.unpack $ refDescription error of
        []    -> continuation True
        [ext] -> addExtension' (show ext) (logRefFullFilePath error) continuation
        list  -> continuation True

addExtension' :: String -> FilePath -> (Bool -> IDEAction) -> IDEAction
addExtension' ext filePath continuation =  do
    mbBuf  <- selectSourceBuf filePath
    case mbBuf of
        Just buf ->
            inActiveBufContext () $ \ nb _ gtkbuf idebuf n -> do
                ideMessage Normal $ "addExtension " ++ ext
                i1 <- getIterAtLine gtkbuf 0
                editInsertCode gtkbuf i1 $ "{-# LANGUAGE " ++ ext ++ " #-}\n"
                fileSave False
                setModified gtkbuf True
                continuation True
        _  -> return ()

addResolveMenuItems ideR theMenu logRef = do
    let msg = T.unpack $ refDescription logRef
    when (isJust $ parseNotInScope msg) $
        addFixMenuItem "Add Import"  $ addImport logRef [] (\ _ -> return ())
    when (isJust $ parseHiddenModule msg) $
        addFixMenuItem "Add Package" $ addPackage logRef
    when ((length $ parsePerhapsYouIntendedToUse msg) == 1) $
        addFixMenuItem "Add Extension" $ addExtension logRef (\ _ -> return ())
  where
    addFixMenuItem name fix = do
        item <- menuItemNewWithLabel name
        item `on` menuItemActivate $ do
            reflectIDE (fix >> return ()) ideR
        menuShellAppend theMenu item

