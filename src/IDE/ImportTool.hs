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
-- |
--
-----------------------------------------------------------------------------

module IDE.ImportTool (
    addAllImports
,   addOneImport
,   addImport
,   parseNotInScope

) where


----------------------------------------------------------------------------
-- |* Import Wizard

import IDE.Core.State (isReexported,candy,Descr(..),currentInfo,IDEM(..),ErrorSpec(..),MessageLevel(..),ideMessage,currentErr,errors,readIDE,IDEAction(..))
import Data.List (sort,nub,nubBy)
import IDE.Core.Types (SpDescr(..),Descr(..),modu,filePath,errDescription)
import Data.Maybe (isNothing,isJust)
import Distribution.ModuleName (ModuleName(..))
import Debug.Trace (trace)
import IDE.Metainfo.Provider (getIdentifierDescr)
import Control.Monad.Trans (liftIO)
import Text.PrettyPrint (render)
import Distribution.Text (disp)
import IDE.Pane.SourceBuffer (inActiveBufContext',selectSourceBuf)
import Graphics.UI.Gtk (widgetDestroy,dialogRun,widgetShowAll,boxPackStart,dialogResponse,dialogGetActionArea,dialogGetUpper,dialogNew,textBufferDelete,textBufferInsert,textIterSetLine,textBufferGetEndIter,textBufferGetStartIter)
import IDE.SourceCandy (getCandylessText)
import Data.Foldable (foldl')
import Text.Parsec.Language (haskellDef,haskellStyle)
import qualified Text.Parsec.Token as P  (integer,makeTokenParser,identStart,operator,parens,dot,comma,identifier,symbol,whiteSpace,lexeme)
import Text.Parsec (upper,oneOf,alphaNum,(<|>),manyTill,sourceLine,eof,anyChar,skipMany,getPosition,newline,many,sepBy,(<?>),choice,optionMaybe,parse)
import Text.ParserCombinators.Parsec (try,CharParser(..))
import Data.Char (isUpper)
import Graphics.UI.Editor.MakeEditor (buildEditor,mkField,FieldDescription(..))
import Graphics.UI.Editor.Parameters (emptyParams,Parameter(..),paraMultiSel,(<<<-),paraName)
import Graphics.UI.Editor.Simple (okCancelFields,staticListEditor)
import Control.Event (registerEvent)
import Graphics.UI.Editor.Basics (eventPaneName,GUIEventSelector(..))
import Graphics.UI.Gtk.General.Structs (ResponseId(..))
import Graphics.UI.Gtk.General.Enums (Packing(..))


-- | Add all imports which gave error messages ...
addAllImports :: IDEAction
addAllImports = do
    errors <- readIDE errors
    mapM_ addImport [ y | (x,y) <-
        nubBy (\ (p1,_) (p2,_) -> p1 == p2)
            $ [(x,y) |  (x,y) <- [((parseNotInScope . errDescription) e, e) | e <- errors]],
                        isJust x]

-- | Add import for current error ...
addOneImport :: IDEAction
addOneImport = do
    errors'     <- readIDE errors
    currentErr' <- readIDE currentErr
    case currentErr' of
        Nothing -> do
            ideMessage Normal $ "No error selected"
            return ()
        Just i -> do
            if  0 <= i && i < length errors'
                then let error = errors' !! i
                     in addImport error >> return ()
                else error "Log>>addOneImport: Error out of range"

-- | Add one missing import
addImport :: ErrorSpec -> IDEM (Bool,Maybe ModuleName)
addImport error =
    case parseNotInScope (errDescription error) of
        Nothing -> trace "Dont parse as not in scope error" return (True,Nothing)
        Just nis -> do
            currentInfo' <- readIDE currentInfo
            case currentInfo' of
                Nothing -> trace "No current info" return (False,Nothing)
                Just ((_,symbolTable1),(_,symbolTable2)) ->
                    case (getIdentifierDescr (id' nis) symbolTable1 symbolTable2) of
                        []          ->  do
                                            ideMessage Normal $ "Identifier " ++ (id' nis) ++
                                                " not found in imported packages"
                                            return (True,Nothing)
                        descr : []  ->  addImport' nis (filePath error) descr
                        list        ->  do
                            mbDescr <-  liftIO $ selectModuleDialog list (id' nis)
                            case mbDescr of
                                Nothing ->  return (False,Nothing)
                                Just descr  ->  addImport' nis (filePath error) descr

addImport' :: NotInScopeParseResult -> FilePath -> Descr -> IDEM (Bool,Maybe ModuleName)
addImport' nis filePath descr = return (False,Nothing)
{--
let
    mbQ = mbQual' nis
    id = id' nis
    mod =  case descr of
                Descr _ _ m _ _ _ -> modu m
                Reexported m _    -> modu m
    importId = if isSub' nis
                then ImportCoC (getRealId descr id) Nothing
                else if isOp' nis
                        then ImportOp (id' nis)
                        else ImportId (id' nis)
    importSpec = ImportSpec (render $disp mod) mbQ (Just ([importId],True)) 0
    in do
        candy' <- readIDE candy
        mbBuf  <- selectSourceBuf filePath
        case mbBuf of
            Nothing  -> return (True,Nothing)
            Just buf -> do
                inActiveBufContext' () $ \ _ gtkbuf _ _ -> do
                    ideMessage Normal $ "addImport " ++ show id ++ " from " ++ (render $ disp $ mod)
                    liftIO $ do
                        i1          <-  textBufferGetStartIter gtkbuf
                        i2          <-  textBufferGetEndIter gtkbuf
                        text        <-  getCandylessText candy' gtkbuf
                        let (importSpecs,lastImport) =  getImportSpecs text
                            linesToModify            =  filter (\ is -> modid is == (render $ disp $ mod)
                                                                && mbQual is == mbQ
                                                                && not(isHiding (mbImpspec is))) importSpecs
                        case linesToModify of
                            []     ->   let newSpec  =  importSpec
                                            newLine  =  show newSpec ++ "\n"
                                            lineSel  =  let bv = foldl' max 0 (map lineNr importSpecs) in
                                                            if bv == 0
                                                                then (lastImport `max` 0)
                                                                else bv
                                        in  do
                                            textIterSetLine i1 lineSel
                                            textBufferInsert gtkbuf i1 newLine
                            (spec:t) -> let newSpec  =  spec{mbImpspec =
                                                            case mbImpspec spec of
                                                                Nothing       ->  Just ([importId],True)
                                                                Just (ids,b)  ->  Just (nub(importId:ids),b)}
                                            newLine  =  show newSpec ++ "\n"
                                        in  do
                                            textIterSetLine i1 (lineNr spec - 1)
                                            textIterSetLine i2 (lineNr spec)
                                            textBufferDelete gtkbuf i1 i2
                                            textBufferInsert gtkbuf i1 newLine
                return (True,Just mod)
        where
            isHiding (Just (_,False)) =  True
            isHiding _                =  False
--}


getRealId descr id = if isReexported descr
                    then getRealId (impDescr descr) id
                    else getReal (details' descr)
    where
        getReal (FieldDescr d) = descrName' d
        getReal (ConstructorDescr d) = descrName' d
        getReal (MethodDescr d) = descrName' d
        getReal _ = id

-- |* The import data

data NotInScopeParseResult = NotInScopeParseResult {
        mbQual' ::   Maybe String
    ,   id'     ::   String
    ,   isSub'  ::   Bool
    ,   isOp'   ::   Bool}
    deriving Eq


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


{--
data ImportSpec = ImportSpec
    {   modid :: String
    ,   mbQual :: Maybe String
    ,   mbImpspec :: Maybe ([ImportItem],Bool)
    ,   lineNr  :: Int}

data ImportItem =
        ImportId String
    |   ImportOp String
    |   ImportCoC String (Maybe [ImportSubItem])
    deriving Eq

instance Show ImportItem where
    show (ImportId str) = str
    show (ImportOp str) = "(" ++ str ++ ")"
    show (ImportCoC str Nothing)   = str ++ "(..)"
    show (ImportCoC str (Just [])) = str
    show (ImportCoC str (Just l))  = str ++ "(" ++ showCoC l
        where
        showCoC [a]              = show a ++ ")"
        showCoC (a:b)            = show a ++ "," ++ showCoC b
        showCoC _                = error "Log>>instance ImportItem Show: Empty list"


data ImportSubItem =
        ImportSubId String
    |   ImportSubOp String
    deriving Eq

instance Show ImportSubItem where
    show (ImportSubId str) = str
    show (ImportSubOp str) = "(" ++ str ++ ")"

instance Show ImportSpec where
    show (ImportSpec modid mbQual mbImpspec _) =   "import "
                        ++   case mbQual of
                                Nothing         ->  ""
                                Just _          ->  "qualified "
                        ++   modid
                        ++   case mbQual of
                                Nothing         ->  ""
                                Just str        ->  " as " ++ str ++ " "
                        ++   case mbImpspec of
                                Nothing         ->  ""
                                Just ([],True)  ->  ""
                                Just (l,True)   ->  " (" ++ showImportSpecs l
                                Just (l, False) ->  " hiding (" ++ showImportSpecs l
        where
        showImportSpecs [a]   = show a ++ ")"
        showImportSpecs (a:b) = show a ++ "," ++ showImportSpecs b
        showImportSpecs _     = error "Log>>showImportSpecs: Empty list"
--}

-- |* The error line parser

lexer      = P.makeTokenParser haskellStyle
lexeme     = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
symbol     = P.symbol lexer
identifier = P.identifier lexer
comma      = P.comma lexer
dot        = P.dot lexer
integer    = P.integer lexer
parens     = P.parens lexer
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


-- |* The import parser

{--
getImportSpecs :: String -> ([ImportSpec],Int)
getImportSpecs modSource =
    case parse importSpecParser "" modSource of
        Left e      -> trace ("parse failed with " ++ show e) ([],0)
        Right specs -> trace (show specs) specs

importSpecParser :: CharParser () ([ImportSpec],Int)
importSpecParser = do
    whiteSpace
    skipHeader
    lines <- sepBy (try parseImportLine) (many newline)
    pos   <- getPosition
    skipMany anyChar
    eof
    return (lines, sourceLine pos - 2)
    <?> "importSpecParser"

skipHeader :: CharParser () ()
skipHeader =do
    try (do
        symbol "module"
        manyTill anyChar (try (symbol "where"))
        trace "with module header " (return ()))
    <|> trace "no module header " (return ())
    <?> "skipHeader"

parseImportLine :: CharParser () ImportSpec
parseImportLine = do
    symbol "import"
    pos              <- getPosition
    isQualified      <- optionMaybe (try (symbol "qualified"))
    modid            <- lexeme mident
    whiteSpace
    mbQName          <- if isJust isQualified
                            then do
        symbol "as"
        qid          <- identifier
        return (Just qid)
                            else return Nothing
    mbImpSpec        <- optionMaybe
        (try (do
            mbHiding <- optionMaybe (try (symbol "hiding"))
            idList   <- parens (sepBy parseImportItem comma)
            return (idList, isNothing mbHiding)))
    whiteSpace
    return (ImportSpec modid mbQName mbImpSpec (sourceLine pos))
    <?> "parseImportLine"

parseImportItem :: CharParser () ImportItem
parseImportItem = do
    id                  <- optionMaybe (try identifier)
    case id of
        Just id         -> if isUpper (head id)
            then do
                ccList  <- optionMaybe (try parseSubList)
                return (ImportCoC id Nothing)
            else return (ImportId id)
        Nothing         -> do
            op          <- optionMaybe (try (parens operator))
            case op of
                Just op -> return (ImportOp op)
                Nothing -> do
                    pos <- getPosition
                    error $ "Can't parse import item " ++ show pos
    <?> "parseImportItem"

parseSubList :: CharParser () (Maybe [ImportSubItem])
parseSubList = do
    all          <- optionMaybe (try (symbol "(..)"))
    case all of
        Just _          -> return Nothing
        Nothing         -> do
            list <- optionMaybe (try (parens (sepBy parseSubItem comma)))
            case list of
                Nothing -> return (Just [])
                Just l  -> return list
    <?> "parseSubList"

parseSubItem :: CharParser () ImportSubItem
parseSubItem = do
    id <- optionMaybe (try identifier)
    case id of
        Just id -> return (ImportSubId id)
        Nothing -> do
            op <- optionMaybe (try operator)
            case op of
                Just op -> return (ImportSubOp op)
                Nothing -> do
                    pos <- getPosition
                    error $ "Can't parse sub import item " ++ show pos
    <?> "parseSubItem"

mident  = do
    c <- P.identStart haskellDef
    cs <- many (alphaNum <|> oneOf "_'.")
    return (c:cs)
        <?> "midentifier"
--}

-- |* The little dialog to choose between possible modules

moduleFields :: [String] -> String -> FieldDescription String
moduleFields list id =
        mkField
            (paraName <<<- ParaName ("From which module is " ++ id)
                $ paraMultiSel <<<- ParaMultiSel False
                    $ emptyParams)
            (\ a -> [a])
            (\ [a] b -> a)
            (staticListEditor ((nub . sort) list))

selectModuleDialog :: [Descr] -> String -> IO (Maybe Descr)
selectModuleDialog list id = do
    dia                        <-   dialogNew
    upper                      <-   dialogGetUpper dia
    lower                      <-   dialogGetActionArea dia
    (widget,_,ext,_)           <-   buildEditor (moduleFields
                                        (map (render . disp . modu . descrModu')  list) id)
                                            ((render . disp . modu . descrModu') (head list))
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
        (ResponseOk,Just v)    -> return (Just (head (filter (\e -> (render . disp . modu . descrModu') e == v)
                                            list)))
        _                      -> trace ("cancel") return Nothing

