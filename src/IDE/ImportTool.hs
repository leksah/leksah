-----------------------------------------------------------------------------
--
-- Module      :  IDE.ImportTool
-- Copyright   :  2007-2009 Jürgen Nicklisch-Franken
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
    addAllImports
,   addOneImport
,   addImport
,   parseNotInScope
) where


import IDE.Core.State
import Data.List (sort,nub,nubBy)
import Data.Maybe (isNothing,isJust)
import IDE.Metainfo.Provider (getIdentifierDescr)
import Text.PrettyPrint (render)
import Distribution.Text (disp)
import IDE.TextEditor
import IDE.Pane.SourceBuffer
    (fileSave, inActiveBufContext, selectSourceBuf)
import Graphics.UI.Gtk
import IDE.SourceCandy (getCandylessText)
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Graphics.UI.Editor.MakeEditor (buildEditor,mkField,FieldDescription(..))
import Graphics.UI.Editor.Parameters
    (paraMinSize,
     emptyParams,
     Parameter(..),
     paraMultiSel,
     (<<<-),
     paraName)
import Graphics.UI.Editor.Basics (eventPaneName,GUIEventSelector(..))
import IDE.Metainfo.GHCUtils(parseHeader)
import Data.Maybe (fromJust)
import RdrName (mkRdrUnqual)
import OccName (mkDataOcc,mkVarOcc)
import Module (pprModuleName)
import Distribution.Text (display)
import GHC hiding (ModuleName)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as  P
    (whiteSpace,
     operator,
     dot,
     identifier,
     symbol,
     lexeme,
     makeTokenParser)
import Graphics.UI.Editor.Simple (okCancelFields, staticListEditor)
import Control.Event (registerEvent)
import Outputable (showSDoc, ppr)
import Control.Monad.Trans (liftIO)
import Control.Monad (foldM_, when)
import Distribution.ModuleName (ModuleName)



-- | Add all imports which gave error messages ...
addAllImports :: IDEAction
addAllImports = do
    prefs' <- readIDE prefs
    let buildInBackground = backgroundBuild prefs'
    when buildInBackground (
        modifyIDE_ (\ide -> ide{prefs = prefs'{backgroundBuild = False}}))
    errors <- readIDE errorRefs
    foldM_ addThis (True,[])
        [ y | (x,y) <-
            nubBy (\ (p1,_) (p2,_) -> p1 == p2)
                $ [(x,y) |  (x,y) <- [((parseNotInScope . refDescription) e, e) | e <- errors]],
                                isJust x]
    when buildInBackground $
        modifyIDE_ (\ide -> ide{prefs = prefs'{backgroundBuild = True}})

    where
        addThis :: (Bool,[Descr]) -> LogRef -> IDEM (Bool,[Descr])
        addThis c@(False,_) _                =  return c
        addThis c@(True,descrList) errorSpec =  addImport errorSpec descrList

-- | Add import for current error ...
addOneImport :: IDEAction
addOneImport = do
    errors'     <- readIDE errorRefs
    currentErr' <- readIDE currentError
    case currentErr' of
        Nothing -> do
            ideMessage Normal $ "No error selected"
            return ()
        Just ref -> do
            addImport ref [] >> return ()

-- | Add one missing import
-- Returns a boolean, if the process should be stopped in case of multiple addition
-- Returns a list of already added descrs, so that it will not be added two times and can
-- be used for default selection
addImport :: LogRef -> [Descr] -> IDEM (Bool, [Descr])
addImport error descrList =
    case parseNotInScope (refDescription error) of
        Nothing -> return (True,descrList)
        Just nis -> do
            currentInfo' <- readIDE packageInfo
            case currentInfo' of
                Nothing -> return (False,descrList)
                Just ((_,symbolTable1),(_,symbolTable2)) ->
                    case (getIdentifierDescr (id' nis) symbolTable1 symbolTable2) of
                        []          ->  do
                                            ideMessage Normal $ "Identifier " ++ (id' nis) ++
                                                " not found in imported packages"
                                            return (True,descrList)
                        descr : []  ->  addImport' nis (logRefFullFilePath error) descr descrList
                        list        ->  do
                            window' <- getMainWindow
                            mbDescr <-  liftIO $ selectModuleDialog window' list (id' nis)
                                            (if null descrList
                                                then Nothing
                                                else Just (head descrList))
                            case mbDescr of
                                Nothing     ->  return (False,descrList)
                                Just descr  ->  if elem descr descrList
                                                    then return (True, descrList)
                                                    else addImport' nis (logRefFullFilePath error) descr descrList

addImport' :: NotInScopeParseResult -> FilePath -> Descr -> [Descr] -> IDEM (Bool,[Descr])
addImport' nis filePath descr descrList =  do
    candy' <- readIDE candy
    mbBuf  <- selectSourceBuf filePath
    let mbMod  = case descrModu' descr of
                    Nothing -> Nothing
                    Just pm -> Just (modu pm)
    case (mbBuf,mbMod) of
        (Just buf,Just mod) -> do
            inActiveBufContext (False,descrList) $ \ nb gtkbuf idebuf n -> do
                ideMessage Normal $ "addImport " ++ show (descrName descr) ++ " from "
                    ++ (render $ disp $ mod)
                i1          <- getStartIter gtkbuf
                i2          <- getEndIter gtkbuf
                text        <- getCandylessText candy' gtkbuf
                parseResult <- parseHeader filePath text
                case parseResult of
                     Nothing            -> do
                        ideMessage Normal ("Can't parse module header " ++ filePath)
                        return (False, descrList)
                     Just pr@HsModule{ hsmodImports = imports } ->
                        case filter (qualifyAsImportStatement mod) imports of
                            []     ->   let newLine  =  showSDoc (ppr (newImpDecl mod)) ++ "\n"
                                            lastLoc = foldr max noSrcSpan (map getLoc imports)
                                            lineSel = if isGoodSrcSpan lastLoc
                                                            then srcSpanEndLine lastLoc
                                                            else figureOutImportLine text pr
                                            in do
                                                i1 <- getIterAtLine gtkbuf lineSel
                                                insert gtkbuf i1 newLine
                                                fileSave False
                                                setModified gtkbuf True
                                                return (True,descr : descrList)
                            l@(impDecl:_) ->
                                            let newImpDecl  =  addToDecl (unLoc impDecl)
                                                newLine     =  showSDoc (ppr newImpDecl) ++ "\n"
                                                myLoc       =   getLoc impDecl
                                                mbLineSel    =   if isGoodSrcSpan myLoc
                                                                    then Just (srcSpanStartLine myLoc,
                                                                                srcSpanEndLine myLoc)
                                                                    else Nothing

                                            in  case mbLineSel of
                                                    Nothing -> do
                                                        ideMessage Normal "No source location"
                                                        return  (False,descrList)
                                                    Just (lineStart, lineEnd) -> do
                                                        i1 <- getIterAtLine gtkbuf (lineStart - 1)
                                                        i2 <- getIterAtLine gtkbuf (lineEnd)
                                                        delete gtkbuf i1 i2
                                                        insert gtkbuf i1 newLine
                                                        fileSave False
                                                        setModified gtkbuf True
                                                        return (True, descr : descrList)
        _  -> return (False, descrList)
    where
        qualifyAsImportStatement :: ModuleName -> LImportDecl alpha -> Bool
        qualifyAsImportStatement moduleName limpDecl =
            let impDecl = unLoc limpDecl in
                showSDoc (ppr (ideclName impDecl)) == display moduleName
                && ((isNothing (mbQual' nis) &&  not (ideclQualified impDecl)) ||
                    (isJust (mbQual' nis) && ideclQualified impDecl
                        && fromJust (mbQual' nis) == qualString impDecl))
                && (isNothing (ideclHiding impDecl) || not (fst (fromJust (ideclHiding impDecl))))
        newImpDecl :: ModuleName -> LImportDecl RdrName
        newImpDecl moduleName = noLoc (ImportDecl
                        (noLoc (mkModuleName (display moduleName)))
                        Nothing
                        False
                        (isJust (mbQual' nis))
                        (if isJust (mbQual' nis)
                            then Just (mkModuleName (fromJust (mbQual' nis)))
                            else Nothing)
                        (Just (False, [noLoc (newIE)])))
        addToDecl :: ImportDecl RdrName -> ImportDecl RdrName
        addToDecl impDecl = case ideclHiding impDecl of
                                Just (True,listIE)  -> throwIDE "ImportTool>>addToDecl: ImpList is hiding"
                                Just (False,listIE) ->
                                    impDecl{ideclHiding = Just (False, (noLoc newIE : listIE))}
                                Nothing             ->
                                    impDecl{ideclHiding = Just (False, [noLoc (newIE)])}

        newIE :: IE RdrName
        newIE =  if isSub' nis
                then IEThingAll (mkRdrUnqual (mkDataOcc (getRealId descr (id' nis))))
                else if isOp' nis
                        then IEVar (mkRdrUnqual (mkVarOcc (id' nis)))
                        else IEVar (mkRdrUnqual (mkVarOcc (id' nis)))


getRealId descr id = if isReexported descr
                    then getRealId (impDescr descr) id
                    else getReal (details' descr)
    where
        getReal (FieldDescr d) = descrName' d
        getReal (ConstructorDescr d) = descrName' d
        getReal (MethodDescr d) = descrName' d
        getReal _ = id

qualString ::  ImportDecl alpha -> String
qualString impDecl = case ideclAs impDecl of
                        Nothing -> ""
                        Just modName -> showSDoc (pprModuleName modName)

-- | A single Haskell @import@ declaration.
{--
type LImportDecl name = Located (ImportDecl name)
data ImportDecl name
  = ImportDecl {
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe FastString,   -- ^ Package qualifier.
      ideclSource    :: Bool,               -- ^ True <=> {-# SOURCE #-} import
      ideclQualified :: Bool,               -- ^ True => qualified
      ideclAs        :: Maybe ModuleName,   -- ^ as Module
      ideclHiding    :: Maybe (Bool, [LIE name]) -- ^ (True => hiding, names)
    }
type LIE name = Located (IE name)

-- | Imported or exported entity.
data IE name
  = IEVar               name
  | IEThingAbs          name		 -- ^ Class/Type (can't tell)
  | IEThingAll          name		 -- ^ Class/Type plus all methods/constructors
  | IEThingWith         name [name]	 -- ^ Class/Type plus some methods/constructors
  | IEModuleContents    ModuleName	 -- ^ (Export Only)
  | IEGroup             Int (HsDoc name) -- ^ Doc section heading
  | IEDoc               (HsDoc name)     -- ^ Some documentation
  | IEDocNamed          String           -- ^ Reference to named doc
--}


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
    case parse scopeParser "" str of
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


-- |* Where to insert the first import statement?

figureOutImportLine :: String -> HsModule alpha -> Int
figureOutImportLine modSource hsMod =
    case hsmodExports hsMod of
        Nothing ->
            case hsmodName hsMod of
                Nothing -> 0
                Just locModName -> srcSpanStartLine (getLoc locModName)
        Just hsmodExports -> srcSpanStartLine (foldr max noSrcSpan (map getLoc hsmodExports)) + 1

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
            (staticListEditor ((nub . sort) list) id)

selectModuleDialog :: Window -> [Descr] -> String -> Maybe Descr -> IO (Maybe Descr)
selectModuleDialog parentWindow list id mbDescr = do
    let listWithMods        =  filter (isJust . descrModu') list
    let selectionList       =  map (render . disp . modu . fromJust . descrModu') listWithMods
    let mbSelectedString    =  case mbDescr of
                                    Nothing -> Nothing
                                    Just descr -> case descrModu' descr of
                                                    Nothing -> Nothing
                                                    Just pm -> Just ((render . disp . modu) pm)
    let realSelectionString =  case mbSelectedString of
                                    Nothing -> head selectionList
                                    Just str -> if elem str selectionList
                                                    then str
                                                    else head selectionList
    dia               <- dialogNew
    windowSetTransientFor dia parentWindow
    upper             <- dialogGetUpper dia
    lower             <- dialogGetActionArea dia
    (widget,inj,ext,_) <- buildEditor (moduleFields selectionList id) realSelectionString
    (widget2,_,_,notifier)     <-   buildEditor okCancelFields ()
    registerEvent notifier Clicked (Left (\e -> do
            case eventPaneName e of
                "Ok"    ->  dialogResponse dia ResponseOk
                _       ->  dialogResponse dia ResponseCancel
            return e))
    boxPackStart upper widget PackGrow 7
    boxPackStart lower widget2 PackNatural 7
    widgetShowAll dia
    resp <- dialogRun dia
    value                      <- ext ([])
    widgetDestroy dia
    --find
    case (resp,value) of
        (ResponseOk,Just v)    -> return (Just (head
                                    (filter (\e -> case descrModu' e of
                                        Nothing -> False
                                        Just pm -> (render . disp . modu) pm == v) list)))
        _                      -> return Nothing

