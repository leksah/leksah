-----------------------------------------------------------------------------
--
-- Module      :  IDE.ImportTool
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
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
    addAllPackagesAndImports
,   addOneImport
,   addImport
,   addPackage
,   parseNotInScope
,   parseHiddenModule
) where

import IDE.Core.State
import Data.Maybe (isNothing,isJust)
import IDE.Metainfo.Provider
       (getPackageImportInfo, getIdentifierDescr)
import Text.PrettyPrint (render)
import Distribution.Text (disp)
import IDE.Pane.SourceBuffer
       (fileOpenThis, belongsToPackage, maybeActiveBuf, fileSave,
        inActiveBufContext, selectSourceBuf)
import Graphics.UI.Gtk
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Graphics.UI.Editor.MakeEditor
       (getRealWidget, FieldDescription(..), buildEditor, mkField)
import Graphics.UI.Editor.Parameters
       ((<<<-), paraMinSize, emptyParams, Parameter(..), paraMultiSel,
        paraName)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as Parsec (parse)
import Graphics.UI.Editor.Simple (staticListEditor)
import Control.Monad (forM_, when)
import Distribution.Text (simpleParse, Text(..), display)
import Data.List (sort, nub, nubBy)
import IDE.Utils.ServerConnection
import Text.PrinterParser (prettyPrint)
import IDE.TextEditor (delete, setModified, insert, getIterAtLine)
import qualified Distribution.ModuleName as D (ModuleName(..))
import qualified Text.ParserCombinators.Parsec.Token as P
       (operator, dot, identifier, symbol, lexeme, whiteSpace,
        makeTokenParser)
import Graphics.UI.Gtk.Gdk.Events (Event(..))
import Control.Monad.Trans (liftIO)
import Distribution.PackageDescription.Parse
       (writePackageDescription, readPackageDescription)
import Distribution.Verbosity (normal)
import IDE.Pane.PackageEditor (hasConfigs)
import Distribution.Package
       (PackageIdentifier(..), pkgName, PackageId, PackageName(..),
        Dependency(..))
import Distribution.Version (VersionRange(..))
import Distribution.PackageDescription (buildDepends)
import Distribution.PackageDescription.Configuration
       (flattenPackageDescription)
import Data.Version (Version(..))




-- | Add all imports which gave error messages ...
addAllPackagesAndImports :: IDEAction
addAllPackagesAndImports = do
    prefs' <- readIDE prefs
    let buildInBackground = backgroundBuild prefs'
    when buildInBackground $
        modifyIDE_ (\ide -> ide{prefs = prefs'{backgroundBuild = False}})
    errors <- readIDE errorRefs
    forM_ errors addPackage
    addAll buildInBackground
        [ y | (x,y) <-
            nubBy (\ (p1,_) (p2,_) -> p1 == p2)
                $ [(x,y) |  (x,y) <- [((parseNotInScope . refDescription) e, e) | e <- errors]],
                                isJust x] (True,[])
    where
        addAll :: Bool -> [LogRef] -> (Bool,[Descr]) -> IDEM ()
        addAll bib (errorSpec:rest) (True,descrList)  =  addImport errorSpec descrList (addAll bib rest)
        addAll bib _ _                                =  finally bib

        finally buildInBackground = when buildInBackground $ do
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
    case parseNotInScope (refDescription error) of
        Nothing -> continuation (True,descrList)
        Just nis -> do
            currentInfo' <- getScopeForActiveBuffer
            case currentInfo' of
                Nothing -> continuation (True,descrList)
                Just (GenScopeC(PackScope _ symbolTable1),GenScopeC(PackScope _ symbolTable2)) ->
                    let list = getIdentifierDescr (id' nis) symbolTable1 symbolTable2
                    in case list of
                        []          ->  do
                                            ideMessage Normal $ "Identifier " ++ (id' nis) ++
                                                " not found in imported packages"
                                            continuation (True, descrList)
                        descr : []  ->  addImport' nis (logRefFullFilePath error) descr descrList continuation
                        list        ->  do
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

addPackage :: LogRef -> IDEAction
addPackage error = do
    case parseHiddenModule (refDescription error) of
        Nothing -> return ()
        Just (HiddenModuleResult mod pack) -> do
            let idePackage = logRefPackage error
            package <- liftIO $ readPackageDescription normal (ipdCabalFile $ idePackage)
            if hasConfigs package
                then return ()
                else do
                    let flat = flattenPackageDescription package
                    ideMessage Normal $ "addPackage " ++ (display $ pkgName pack)
                    liftIO $ writePackageDescription (ipdCabalFile $ idePackage)
                        flat { buildDepends =
                            Dependency (pkgName pack) AnyVersion : buildDepends flat}

getScopeForActiveBuffer :: IDEM (Maybe (GenScope, GenScope))
getScopeForActiveBuffer = do
    mbActiveBuf <- maybeActiveBuf
    case mbActiveBuf of
        Nothing -> return Nothing
        Just buf -> do
            mbPackage <- belongsToPackage buf
            case mbPackage of
                Nothing -> return Nothing
                Just pack -> getPackageImportInfo pack

addImport' :: NotInScopeParseResult -> FilePath -> Descr -> [Descr] -> ((Bool,[Descr]) -> IDEAction) -> IDEAction
addImport' nis filePath descr descrList continuation =  do
    mbBuf  <- selectSourceBuf filePath
    let mbMod  = case dsMbModu descr of
                    Nothing -> Nothing
                    Just pm -> Just (modu pm)
    case (mbBuf,mbMod) of
        (Just buf,Just mod) -> do
            inActiveBufContext () $ \ nb gtkbuf idebuf n -> do
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
                                                insert gtkbuf i1 newLine
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
                                                    insert gtkbuf i1 newLine
                                                    fileSave False
                                                    setModified gtkbuf True
                                                    continuation (True,(descr : descrList))
                         ServerHeader (Right lastLine) ->
                                            let newLine  =  prettyPrint (newImpDecl mod) ++ "\n"
                                            in do
                                                i1 <- getIterAtLine gtkbuf lastLine
                                                insert gtkbuf i1 newLine
                                                fileSave False
                                                setModified gtkbuf True
                                                continuation (True,(descr : descrList))
                         ServerFailed string	-> do
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
    symbol "`"
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
            symbol "'"
            return (NotInScopeParseResult mbQual op (isJust isSub) True)
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
            let qualId              =  case mbQual of
                                            Nothing -> id
                                            Just str -> str ++ "." ++ id
            dia               <- dialogNew
            windowSetTransientFor dia parentWindow
            upper             <- dialogGetUpper dia
            dialogAddButton dia "Ok" ResponseOk
            dialogAddButton dia "Cancel" ResponseCancel
            (widget,inj,ext,_) <- buildEditor (moduleFields selectionList qualId) realSelectionString
            boxPackStart upper widget PackGrow 7
            dialogSetDefaultResponse dia ResponseOk --does not work for the tree view
            widgetShowAll dia
            rw <- getRealWidget widget
--            case rw of
--                Nothing -> return ()
--                Just r -> do
--                    r `onKeyPress` \ event -> do
--                        case event of
--                            Key { eventKeyName = name} ->
--                                case name of
--                                    "Return" -> do
--                                        dialogResponse dia ResponseOk
--                                        return True
--                                    _        -> return False
--                            _ -> return False
--                    return ()
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

--testString =    "    Could not find module `Graphics.UI.Gtk':\n"
--             ++ "      It is a member of the hidden package `gtk-0.11.0'.\n"
--             ++ "      Perhaps you need to add `gtk' to the build-depends in your .cabal file.\n"
--             ++ "      Use -v to see a list of the files searched for."
--
--test = parseHiddenModule testString == Just (HiddenModuleResult {hiddenModule = "Graphics.UI.Gtk", missingPackage = PackageIdentifier {pkgName = PackageName "gtk", pkgVersion = Version {versionBranch = [0,11,0], versionTags = []}}})

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
    symbol "Could not find module `"
    mod    <- many (noneOf "'")
    symbol "':\n"
    whiteSpace
    symbol "It is a member of the hidden package `"
    pack   <- many (noneOf "'")
    symbol "'.\n"
    many anyChar
    return (mod, pack)
    <?> "hiddenModuleParser"
