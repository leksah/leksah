{-# OPTIONS_GHC -XScopedTypeVariables -XDeriveDataTypeable -XMultiParamTypeClasses
    -XTypeSynonymInstances -XParallelListComp #-}
--
-- Module      :  IDE.Pane.Log
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <info@leksah.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Log pane
--
-------------------------------------------------------------------------------


module IDE.Pane.Log (
    IDELog(..)
,   LogView(..)
,   LogAction(..)
,   LogState
,   LogTag(..)

,   readOut
,   readErr
,   runExternal
) where

import Data.Typeable (Typeable(..))
import Text.ParserCombinators.Parsec
import IDE.Core.State
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Editor.MakeEditor (buildEditor)
import Control.Monad.Trans (liftIO)
import Graphics.UI.Editor.Basics (eventPaneName)
import Graphics.UI.Editor.Basics (GUIEventSelector(..))
import Control.Event (registerEvent)
import Graphics.UI.Editor.Simple (okCancelFields)
import Graphics.UI.Editor.Parameters
import Text.ParserCombinators.Parsec.Language (haskellDef)
import Data.Char (isUpper)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (haskellStyle)
import Data.List (foldl',sort,nub)
import IDE.SourceCandy (getCandylessText)
import IDE.Pane.SourceBuffer (inBufContext')
import IDE.Pane.Info (getIdentifierDescr)
import Debug.Trace (trace)
import Data.List (nubBy)
import IDE.Pane.SourceBuffer (markErrorInSourceBuf)
import IDE.Pane.SourceBuffer (selectSourceBuf)
import Graphics.UI.Editor.MakeEditor (FieldDescription(..),mkField)
import Graphics.UI.Editor.Simple (staticListEditor)
import Data.Maybe (isNothing,isJust)
import Distribution.ModuleName(ModuleName)
import Distribution.Text(disp)
import Text.PrettyPrint (render)
import System.Process
import System.IO
import Prelude hiding (catch)
import Control.Exception hiding (try)
-------------------------------------------------------------------------------
--
-- * Interface
--

--
-- | The Log pane
--


data IDELog         =   IDELog {
    textView        ::   TextView
,   scrolledWindowL ::   ScrolledWindow
} deriving Typeable

data LogState               =   LogState
    deriving(Eq,Ord,Read,Show,Typeable)


class LogAction alpha where
    clearLog        ::   alpha
    addAllImports   ::   alpha
    addOneImport    ::   alpha

instance LogAction IDEAction where
    clearLog        =   clearLog'
    addAllImports   =   addAllImports'
    addOneImport    =   addOneImport'

class Pane alpha beta => LogView alpha beta where
    getLog          ::   beta alpha
    appendLog       ::   alpha  -> String -> LogTag -> IO Int
    markErrorInLog  ::   alpha  -> (Int, Int) -> IO ()

instance IDEObject IDELog

instance LogView IDELog IDEM
    where
    getLog          =   getLog'
    appendLog       =   appendLog'
    markErrorInLog  =   markErrorInLog'

instance Pane IDELog IDEM
    where
    primPaneName _  =   "Log"
    getAddedIndex _ =   0
    getTopWidget    =   castToWidget . scrolledWindowL
    paneId b        =   "*Log"
    makeActive log  =   activatePane log []
    close pane     =   do
        (panePath,_)    <-  guiPropertiesFromName (paneName pane)
        nb              <-  getNotebook panePath
        mbI             <-  liftIO $notebookPageNum nb (getTopWidget pane)
        case mbI of
            Nothing ->  liftIO $ do
                sysMessage Normal "notebook page not found: unexpected"
                return ()
            Just i  ->  do
                deactivatePaneIfActive pane
                liftIO $ do
                    notebookRemovePage nb i
                    widgetDestroy (getTopWidget pane)
                removePaneAdmin pane

instance RecoverablePane IDELog LogState IDEM where
    saveState p     =   return (Just LogState)
    recoverState pp LogState = do
        nb <- getNotebook pp
        initLog pp nb


-------------------------------------------------------------------------------
--
-- * Implementation
--


initLog :: PanePath -> Notebook -> IDEAction
initLog panePath nb = do
    panes      <- readIDE panes
    paneMap    <- readIDE paneMap
    prefs      <- readIDE prefs
    (buf,cids) <- reifyIDE $ \ideR session ->  do
        tv           <- textViewNew
        buf          <- textViewGetBuffer tv
        iter         <- textBufferGetEndIter buf
        textBufferCreateMark buf (Just "end") iter True

        tags         <- textBufferGetTagTable buf
        errtag       <- textTagNew (Just "err")
        set errtag[textTagForeground := "red"]
        textTagTableAdd tags errtag
        frametag     <- textTagNew (Just "frame")
        set frametag[textTagForeground := "green"]
        textTagTableAdd tags frametag
        activeErrtag <- textTagNew (Just "activeErr")
        set activeErrtag[textTagBackground := "yellow"]
        textTagTableAdd tags activeErrtag

        textViewSetEditable tv True
        fd           <- case logviewFont prefs of
            Just str -> do
                fontDescriptionFromString str
            Nothing  -> do
                f    <- fontDescriptionNew
                fontDescriptionSetFamily f "Sans"
                return f
        widgetModifyFont tv (Just fd)
        sw           <- scrolledWindowNew Nothing Nothing
        containerAdd sw tv
        scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
        scrolledWindowSetShadowType sw ShadowIn

        let buf = IDELog tv sw
        notebookInsertOrdered nb sw (paneName buf) Nothing
        widgetShowAll (scrolledWindowL buf)
        cid1         <- tv `afterFocusIn`
            (\_      -> do reflectIDE (makeActive buf) ideR session; return True)
        cid2         <- tv `onButtonPress`
            (\ b     -> do reflectIDE (clicked b buf) ideR session; return True)
        return (buf,[ConnectC cid1, ConnectC cid2])
    addPaneAdmin buf cids panePath
    liftIO $widgetGrabFocus (textView buf)

clicked :: Event -> IDELog -> IDEAction
clicked (Button _ SingleClick _ _ _ _ LeftButton x y) ideLog = do
    errors'     <-  readIDE errors
    line' <- liftIO $ do
        (x,y)       <-  widgetGetPointer (textView ideLog)
        (_,y')      <-  textViewWindowToBufferCoords (textView ideLog) TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY (textView ideLog) y'
        textIterGetLine iter
    case filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
            (zip errors' [0..(length errors')]) of
        [(thisErr,n)] -> do
            succ <- selectSourceBuf (filePath thisErr)
            if isJust succ
                then markErrorInSourceBuf (line thisErr) (column thisErr)
                        (errDescription thisErr)
                else return ()
            log :: IDELog <- getLog
            liftIO $ markErrorInLog log (logLines thisErr)
            modifyIDE_ (\ide -> return (ide{currentErr = Just n}))
        otherwise   -> return ()
clicked (Button _ SingleClick _ _ _ _ RightButton x y) ideLog = do
    errors'     <-  readIDE errors
    line'       <-  reifyIDE $ \ideR session ->  do
        (x,y)       <-  widgetGetPointer (textView ideLog)
        (_,y')      <-  textViewWindowToBufferCoords (textView ideLog) TextWindowWidget (x,y)
        (iter,_)    <-  textViewGetLineAtY (textView ideLog) y'
        textIterGetLine iter
    case filter (\(es,_) -> fst (logLines es) <= (line'+1) && snd (logLines es) >= (line'+1))
            (zip errors' [0..(length errors')]) of
        [(thisErr,n)] -> reifyIDE $ \ideR session ->  do
            theMenu         <-  menuNew
            item0           <-  menuItemNewWithLabel "Add all imports"
            item0 `onActivateLeaf` do
                reflectIDE addAllImports' ideR session
            menuShellAppend theMenu item0
            case parseNotInScope (errDescription thisErr) of
                Nothing   -> do
                    return ()
                Just _  -> do
                    item1   <-  menuItemNewWithLabel "Add import"
                    item1 `onActivateLeaf` do
                        reflectIDE (addImport thisErr >> return()) ideR session
                    menuShellAppend theMenu item1
            menuPopup theMenu Nothing
            widgetShowAll theMenu
            return ()
        otherwise   -> return ()
clicked _ _ = return ()

getLog' :: IDEM IDELog
getLog' = do
    mbPane <- getPane
    case mbPane of
        Nothing -> do
            prefs   <- readIDE prefs
            layout  <- readIDE layout
            let pp  =  getStandardPanePath (logPanePath prefs) layout
            nb      <- getNotebook pp
            initLog pp nb
            mbPane <- getPane
            case mbPane of
                Nothing ->  throwIDE "Can't init log"
                Just l  ->  return l
        Just p -> return p

simpleLog :: String -> IDEAction
simpleLog str = do
    log :: IDELog <- getLog
    liftIO $ appendLog log str LogTag
    return ()

appendLog' :: IDELog -> String -> LogTag -> IO Int
appendLog' l@(IDELog tv _) string tag = do
    buf   <- textViewGetBuffer tv
    iter  <- textBufferGetEndIter buf
    textBufferSelectRange buf iter iter
    textBufferInsert buf iter string
    iter2 <- textBufferGetEndIter buf
    case tag of
        LogTag   -> return ()
        ErrorTag -> do
            len   <- textBufferGetCharCount buf
            strti <- textBufferGetIterAtOffset buf (len - length string)
            textBufferApplyTagByName buf "err" iter2 strti
        FrameTag -> do
            len   <- textBufferGetCharCount buf
            strti <- textBufferGetIterAtOffset buf (len - length string)
            textBufferApplyTagByName buf "frame" iter2 strti
    textBufferMoveMarkByName buf "end" iter2
    mbMark <- textBufferGetMark buf "end"
    line   <- textIterGetLine iter2
    case mbMark of
        Nothing   -> return ()
        Just mark -> textViewScrollMarkOnscreen tv mark
    bringPaneToFront l
    return line

markErrorInLog' :: IDELog -> (Int,Int) -> IO ()
markErrorInLog' (IDELog tv _) (l1,l2) = do
    idleAdd  (do
        buf    <- textViewGetBuffer tv
        iter   <- textBufferGetIterAtLineOffset buf (l1-1) 0
        iter2  <- textBufferGetIterAtLineOffset buf l2 0
        textBufferSelectRange buf iter iter2
        textBufferMoveMarkByName buf "end" iter
        mbMark <- textBufferGetMark buf "end"
        case mbMark of
            Nothing   -> return ()
            Just mark ->  do
                    textViewScrollToMark tv  mark 0.0 (Just (0.3,0.3))
                    return ()
        return False) priorityDefaultIdle
    return ()


clearLog' :: IDEAction
clearLog' = do
    log <- getLog
    buf <- liftIO$ textViewGetBuffer $textView log
    liftIO $textBufferSetText buf ""
    modifyIDE_ (\ide -> return (ide{errors = [], currentErr = Nothing}))

----------------------------------------------------------------------------
-- |* Import Wizard

-- | Add all imports which gave error messages ...
addAllImports' :: IDEAction
addAllImports' = do
    errors <- readIDE errors
    mapM_ addImport [ y | (x,y) <-
        nubBy (\ (p1,_) (p2,_) -> p1 == p2)
            $ [(x,y) |  (x,y) <- [((parseNotInScope . errDescription) e, e) | e <- errors]],
                        isJust x]

-- | Add import for current error ...
addOneImport' :: IDEAction
addOneImport' = do
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
addImport' nis filePath descr = let
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
                inBufContext' () $ \ _ gtkbuf _ _ -> do
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

-- |* The import parser

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

conid  = do
    c <-  upper
    cs <- many (alphaNum <|> oneOf "_'")
    return (c:cs)
        <?> "conid"

-- |* The stupid little dialog

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

-- ---------------------------------------------------------------------
-- ** Spawning external processes
--

readOut :: IDELog -> Handle -> IO ()
readOut log hndl =
     catch (readAndShow)
       (\(e :: SomeException) -> do
        --appendLog log ("----------------------------------------\n") FrameTag
        hClose hndl
        return ())
    where
    readAndShow = do
        line <- hGetLine hndl
        appendLog log (line ++ "\n") LogTag
        readAndShow

readErr :: IDELog -> Handle -> IO ()
readErr log hndl =
     catch (readAndShow)
       (\(e :: SomeException) -> do
        hClose hndl
        return ())
    where
    readAndShow = do
        line <- hGetLine hndl
        appendLog log (line ++ "\n") ErrorTag
        readAndShow

runExternal :: FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)
runExternal path args = do
    putStrLn $ "Run external called with args " ++ show args
    hndls@(inp, out, err, _) <- runInteractiveProcess path args Nothing Nothing
    sysMessage Normal $ "Starting external tool: " ++ path ++ " with args " ++ (show args)
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    hSetBuffering inp NoBuffering
    hSetBinaryMode out True
    hSetBinaryMode err True
    return hndls

