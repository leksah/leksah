--
-- | Module for saving, restoring and editing preferences
-- 

module Ghf.Editor.PropertyEditor (
    field
,   Comment
,   Name
,   FieldDescription (..)

,   Getter
,   Setter
,   Printer
,   Parser
,   Injector
,   Extractor
,   Notifier
,   Editor
,   Applicator

,   prefsParser
,   boolParser
,   intParser
,   pairParser
,   identifier
,   emptyParser

,   showPrefs
,   emptyPrinter

,   boolEditor
,   stringEditor
,   multilineStringEditor
,   intEditor
,   maybeEditor
,   pairEditor
,   eitherOrEditor
,   genericEditor
,   selectionEditor
,   fileEditor
,   versionEditor
,   multisetEditor

,   Default(..)
) where

import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Text.PrettyPrint.HughesPJ as PP
import Control.Monad(foldM)
import Graphics.UI.Gtk hiding (afterToggleOverwrite,Focus,onChanged)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.ModelView as New
import Control.Monad.Reader
import qualified Data.Map as Map
import Data.Map(Map,(!))
import Data.Maybe(isNothing,fromJust)
import Data.Version
import Data.IORef
import Data.Maybe(isJust)
import System.Directory
import Data.List(unzip4,elemIndex)
import Text.ParserCombinators.ReadP(readP_to_S)

import Debug.Trace



import Ghf.Core

type Comment            =   String
type Name               =   String

data FieldDescription alpha =  FD {
        name                ::  Name
    ,   comment             ::  Comment
    ,   fieldPrinter        ::  alpha -> PP.Doc
    ,   fieldParser         ::  alpha -> CharParser () alpha
    ,   fieldEditor         ::  alpha -> IO (Widget, alpha -> IO(), alpha -> IO(alpha), IORef (Map String Notifier))
    ,   fieldApplicator     ::  alpha -> alpha -> GhfAction
}

class Default a 
    where getDefault :: a 

type Getter alpha beta  =   alpha -> beta
type Setter alpha beta  =   beta -> alpha -> alpha
type Printer beta       =   beta -> PP.Doc
type Parser beta        =   CharParser () beta

type Injector beta      =   beta -> IO()
type Extractor beta     =   IO(Maybe (beta))
type Notifier        =   IO ()  
type Editor beta        =   Name -> IO(Widget, Injector beta, Extractor beta, IORef (Map String Notifier))
type Applicator beta    =   beta -> GhfAction

{--
class EditorC where
    get :: IO(Widget, Injector beta, Extractor beta, IORef (Map String Notifier))
    
instance (Editor a) EditorC where
    get = 
--}

type MkFieldDescription alpha beta =
              String ->                         --name
              String ->                         --comment
              (Printer beta) ->                
              (Parser beta) ->
              (Getter alpha beta) ->            
              (Setter alpha beta) ->            
              (Editor beta) ->
              (Applicator beta) ->
              FieldDescription alpha

field :: Eq beta => MkFieldDescription alpha beta
field name comment printer parser getter setter editor applicator =
    FD  name 
        comment
        (\ dat -> (PP.text name PP.<> PP.colon)
                PP.$$ (PP.nest 15 (printer (getter dat)))
                PP.$$ (PP.nest 5 (if null comment 
                                        then PP.empty 
                                        else PP.text $"--" ++ comment)))
        (\ dat -> try (do
            symbol name
            colon
            val <- parser
            return (setter val dat)))
        (\ dat -> do
            (widget, inj,ext,notiRef) <- editor name
            inj (getter dat)
            let handler = (do 
                v <- ext
                case v of
                    Just _ -> return ()
                    Nothing -> do
                        md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                                $"Field " ++ name ++ " has invalid value. Please correct"
                        dialogRun md
                        widgetDestroy md
                        return ())
            registerHandler notiRef handler "onFocusOut"
            return (widget,
                    (\a -> inj (getter a)), 
                    (\a -> do 
                        b <- ext
                        case b of
                            Just b -> return (setter b a)
                            Nothing -> return a),
                    notiRef))
        (\ newDat oldDat -> do
            let newField = getter newDat
            let oldField = getter oldDat
            if newField == oldField
                then return ()
                else applicator newField)

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

prefsStyle  :: P.LanguageDef st
prefsStyle  = emptyDef                      
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                }      

lexer = P.makeTokenParser prefsStyle
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
hexadecimal = P.hexadecimal lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
colon = P.colon lexer
integer = P.integer lexer

prefsParser :: a -> [FieldDescription a] -> CharParser () a
prefsParser def descriptions = 
    let parsersF = map fieldParser descriptions in do     
        whiteSpace
        res <- applyFieldParsers def parsersF
        return res
        <?> "prefs parser" 

applyFieldParsers :: a -> [a -> CharParser () a] -> CharParser () a
applyFieldParsers prefs parseF = do
    let parsers = map (\a -> a prefs) parseF
    newprefs <- choice parsers
    whiteSpace
    applyFieldParsers newprefs parseF
    <|> do
    eof
    return (prefs)
    <?> "field parser"

boolParser :: CharParser () Bool
boolParser = do
    (symbol "True" <|> symbol "true")
    return True
    <|> do
    (symbol "False"<|> symbol "false")
    return False
    <?> "bool parser"

pairParser :: CharParser () alpha -> CharParser () (alpha,alpha) 
pairParser p2 = do
    char '('    
    v1 <- p2
    char ','
    v2 <- p2
    char ')'
    return (v1,v2)
    <?> "pair parser"

intParser :: CharParser () Int
intParser = do
    i <- integer
    return (fromIntegral i)

emptyParser :: CharParser () a
emptyParser = pzero

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

showPrefs :: a -> [FieldDescription a] -> String
showPrefs prefs prefsDesc = PP.render $
    foldl (\ doc (FD _ _ printer _ _ _) -> doc PP.$+$ printer prefs) PP.empty prefsDesc 

emptyPrinter :: alpha -> PP.Doc
emptyPrinter _ = PP.empty

-- ------------------------------------------------------------
-- * Editing
-- ------------------------------------------------------------
xalign = 0.5
yalign = 0.5
xscale = 0.95
yscale = 0.95
paddingTop = 2
paddingBottom = 5
paddingLeft = 3
paddingRight = 3

boolEditor :: Editor Bool
boolEditor label = do
--    frame   <-  frameNew
--    frameSetShadowType frame ShadowNone
    alignment <- alignmentNew xalign yalign xscale yscale
--    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    button   <-  checkButtonNewWithLabel label
--    buttonSetRelief button ReliefNone
--    toggleButtonSetMode button False
    containerAdd alignment button
    let injector = toggleButtonSetActive button
    let extractor = do
        r <- toggleButtonGetActive button
        return (Just r)
    let clickedNotifier f = do button `onClicked` f; return ()
    notiRef <- standardNotifiers button extractor label
    registerEvent button notiRef onClicked "onClicked"
    return ((castToWidget) alignment, injector, extractor, notiRef)

stringEditor :: Editor String
stringEditor label = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    entry   <-  entryNew
    containerAdd frame alignment
    containerAdd alignment entry
    let injector = entrySetText entry
    let extractor = do
        r <- entryGetText entry
        return (Just r)
    notiRef <- standardNotifiers entry extractor label
    return ((castToWidget) frame, injector, extractor, notiRef)

multilineStringEditor :: Editor String
multilineStringEditor label = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    textView   <-  textViewNew
    vBox <- vBoxNew False 0
    boxPackStart vBox textView PackGrow 0
    containerAdd frame alignment
    containerAdd alignment vBox
    let injector = (\s -> do
        buffer <- textViewGetBuffer textView
        textBufferSetText buffer s)
    let extractor = do
        buffer <- textViewGetBuffer textView
        start <- textBufferGetStartIter buffer
        end <- textBufferGetEndIter buffer
        r <- textBufferGetText buffer start end False
        return (Just r)
    notiRef <- standardNotifiers textView extractor label
    return ((castToWidget) frame, injector, extractor, notiRef)

intEditor :: Double -> Double -> Double -> Editor Int
intEditor min max step label = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    spin <- spinButtonNewWithRange min max step
    containerAdd frame alignment
    containerAdd alignment spin
    let injector = (\v -> spinButtonSetValue spin (fromIntegral v))
    let extractor = (do
        newNum <- spinButtonGetValue spin
        return (Just (truncate newNum)))
    notiRef <- standardNotifiers spin extractor label
    return ((castToWidget) frame, injector, extractor, notiRef)

genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor label = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    entry   <-  entryNew
    containerAdd frame alignment
    containerAdd alignment entry
    let injector = (\t -> entrySetText entry (show t))
    let extractor = do r <- entryGetText entry; return (Just (read r))
    notiRef <- standardNotifiers entry extractor label
    return ((castToWidget) frame, injector, extractor, notiRef)

selectionEditor :: (Show beta, Eq beta) => [beta] -> Editor beta
selectionEditor list label = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    combo   <-  New.comboBoxNewText
    vBox <- vBoxNew False 0
    boxPackStart vBox combo PackNatural 0
    New.comboBoxSetActive combo 1
    mapM_ (\v -> New.comboBoxAppendText combo (show v)) list
    containerAdd frame alignment 
    containerAdd alignment vBox
    let injector = (\t -> let mbInd = elemIndex t list in
                            case mbInd of
                                Just ind -> New.comboBoxSetActive combo ind
                                Nothing -> return ())
    let extractor = do  mbInd <- New.comboBoxGetActive combo; 
                        case mbInd of 
                            Just ind -> return (Just (list !! ind))
                            Nothing -> return Nothing
    let changedNotifier f = do combo `New.onChanged` f; return ()
    notiRef <- standardNotifiers combo extractor label
    registerEvent combo notiRef onChanged "onChanged"
    return ((castToWidget) frame, injector, extractor, notiRef)

{--
multiselectionEditor :: (Show beta, Eq beta) => [beta] -> Editor [beta]
multiselectionEditor list label = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame label
    combo   <-  New.comboBoxNewText
    mapM_ (\v -> New.comboBoxAppendText combo (show v)) list
    containerAdd frame combo
    let injector = (\t -> let mbInd = elemIndex t list in
                            case mbInd of
                                Just ind -> New.comboBoxSetActive combo ind
                                Nothing -> return ())
    let extractor = do  mbInd <- New.comboBoxGetActive combo; 
                        case mbInd of 
                            Just ind -> return (Just (list !! ind))
                            Nothing -> return Nothing
    let changedNotifier f = do combo `New.onChanged` f; return ()
    let notifiers = Map.insert "onChanged" changedNotifier (standardNotifiers (castToWidget combo))
    New.comboBoxSetActive combo 1
    return ((castToWidget) frame, injector, extractor, notifiers)
--}

fileEditor :: Editor FilePath
fileEditor label = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    button <- buttonNewWithLabel "Select file"    
    entry   <-  entryNew
    hBox <- hBoxNew False 1
    boxPackStart hBox entry PackGrow 0
    boxPackStart hBox button PackGrow 0
    containerAdd frame alignment
    containerAdd alignment hBox
    let injector = entrySetText entry
    let extractor = do
        str <- entryGetText entry
        dfe <- doesFileExist str
        if dfe 
            then return (Just str) 
            else return Nothing 
    button `onClicked` do
        mbFileName <- do     
            dialog <- fileChooserDialogNew
                            (Just $ "Select File")             
                            Nothing                   
                        FileChooserActionOpen              
                        [("gtk-cancel"                       
                        ,ResponseCancel)
                        ,("gtk-open"                                  
                        ,ResponseAccept)]
            widgetShow dialog
            response <- dialogRun dialog
            case response of
                ResponseAccept -> do
                    f <- fileChooserGetFilename dialog
                    widgetDestroy dialog               
                    return f
                ResponseCancel -> do
                    widgetDestroy dialog       
                    return Nothing
                ResponseDeleteEvent-> do  
                    widgetDestroy dialog                
                    return Nothing
        case mbFileName of
            Nothing -> return ()
            Just fn -> entrySetText entry fn 
    notiRef <- standardNotifiers entry extractor label
    return ((castToWidget) alignment, injector, extractor, notiRef)    

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- | or deselected (if the positive Argument is False)
--

maybeEditor :: Default beta => Editor beta -> Bool -> String -> String -> Editor (Maybe beta)
maybeEditor childEditor positive boolLabel childLabel label = do
    childRef <- trace "newChildRef" $newIORef Nothing
    frame   <-  frameNew
    frameSetLabel frame label
    (boolFrame,inj1,ext1,notiRef1) <- boolEditor  boolLabel
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    vBox <- vBoxNew False 0
    boxPackStart vBox boolFrame PackNatural 0
--    boxPackStart vBox justFrame PackNatural 0
    containerAdd alignment frame
    containerAdd frame vBox    
    let injector =  \ v -> trace "injecting" $case v of
            Nothing ->  do
                inj1 (if positive then False else True)
                hasChild <- hasChildEditor childRef
                if hasChild 
                    then do
                        (justFrame,_,_,_) <- getChildEditor childRef childEditor childLabel
                        widgetHideAll justFrame
                    else return ()                                 
            Just val -> do
                hasChild <- hasChildEditor childRef
                (justFrame,inj2,_,_) <- getChildEditor childRef childEditor childLabel
                inj1 (if positive then True else False)
                if hasChild 
                    then do
                        widgetShowAll justFrame
                    else do
                        boxPackStart vBox justFrame PackNatural 0
                        widgetShowAll justFrame
                inj2 val
    let extractor = trace "extracting" $do
        bool <- ext1
        case bool of
            Nothing -> return Nothing
            Just bv | bv == positive -> do
                (_,_,ext2,_) <- getChildEditor childRef childEditor childLabel
                value <- ext2
                case value of
                    Nothing -> return Nothing
                    Just value -> return (Just (Just value))
            otherwise -> return (Just Nothing)
    let onClickedHandler = (do 
            mbBool <- ext1
            case mbBool of
                Just bool ->  
                    if bool == positive 
                        then do
                            hasChild <- hasChildEditor childRef
                            (justFrame,inj2,_,_) <- getChildEditor childRef childEditor childLabel
                            if hasChild 
                                then do
                                    widgetShowAll justFrame
                                else do
                                    boxPackStart vBox justFrame PackNatural 0
                                    widgetShowAll justFrame
                                    inj2 getDefault                     
                        else do
                            hasChild <- hasChildEditor childRef
                            if hasChild 
                                then do
                                    (justFrame,_,_,_) <- getChildEditor childRef childEditor childLabel
                                    widgetHideAll justFrame
                                else return ()        
                Nothing -> return ()) 
    registerHandler notiRef1 onClickedHandler "onClicked"
    notiRef <- standardNotifiers frame extractor label
    return ((castToWidget) alignment, injector, extractor, notiRef)    

getChildEditor :: IORef (Maybe (Widget, Injector alpha , Extractor alpha , IORef (Map String Notifier))) -> 
                    (Editor alpha ) -> String -> 
                        IO (Widget, Injector alpha , Extractor alpha , IORef (Map String Notifier))
getChildEditor childRef childEditor childLabel =  do
    mb <- trace "getChildEditor" $readIORef childRef
    case mb of
        Just editor -> return editor
        Nothing -> do
            let val = childEditor 
            editor <- childEditor childLabel
            writeIORef childRef (Just editor)
            return editor
            
            

hasChildEditor :: IORef (Maybe b) -> IO (Bool)
hasChildEditor childRef =  do
    mb <- readIORef childRef
    return (isJust mb) 

eitherOrEditor :: (Default alpha, Default beta) => (Editor alpha,String) -> (Editor beta,String) -> Editor (Either alpha beta)
eitherOrEditor (leftEditor,leftLabel) (rightEditor,rightLabel) label = do
    frame   <-  frameNew
    frameSetLabel frame ""
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    (boolFrame,inj1,ext1,notiRef1) <- boolEditor  label
    (leftFrame,inj2,ext2,notiRef2) <- leftEditor leftLabel
    (rightFrame,inj3,ext3,notiRef3) <- rightEditor rightLabel
    let injector =  \ v -> case v of
                            Left vl -> do
                              widgetShowAll leftFrame
                              widgetHideAll rightFrame  
                              inj2 vl
                              inj3 getDefault
                              inj1 True
                            Right vr  -> do
                              widgetHideAll leftFrame 
                              widgetShowAll rightFrame  
                              inj3 vr
                              inj2 getDefault
                              inj1 False
    let extractor = do
        mbbool <- ext1
        case mbbool of
            Nothing -> return Nothing 
            Just True   ->  do
                value <- ext2
                case value of
                    Nothing -> return Nothing
                    Just value -> return (Just (Left value))  
            Just False -> do
                value <- ext3
                case value of
                    Nothing -> return Nothing
                    Just value -> return (Just (Right value))      
    vBox <- vBoxNew False 1
    boxPackStart vBox boolFrame PackNatural 0
    boxPackStart vBox leftFrame PackNatural 0
    boxPackStart vBox rightFrame PackNatural 0
    containerAdd alignment frame
    containerAdd frame vBox      
    let onClickedHandler = (do 
            bool <- ext1
            case bool of
                Just True -> do
                    widgetShowAll leftFrame 
                    widgetHideAll rightFrame 
                Just False -> do
                    widgetShowAll rightFrame 
                    widgetHideAll leftFrame 
                Nothing -> return ())
    registerHandler notiRef1 onClickedHandler "onClicked"
    notiRef <- standardNotifiers frame extractor label
    return ((castToWidget) alignment, injector, extractor, notiRef)    

pairEditor :: (Editor alpha, String) -> (Editor beta, String) -> Direction -> Editor (alpha,beta)
pairEditor (fstEd, fstLabel) (sndEd, sndLabel) dir label = do
    frame   <-  frameNew
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    (fstFrame,inj1,ext1,notiRef1) <- fstEd fstLabel
    widgetSetName fstFrame "first"
    (sndFrame,inj2,ext2,notiRef2) <- sndEd sndLabel
    widgetSetName sndFrame "snd"
    box <- case dir of
        Horizontal -> do 
            b <- hBoxNew False 1
            return (castToBox b)
        Vertical -> do
            b <- vBoxNew False 1
            return (castToBox b)
    widgetSetName box "box"    
    boxPackStart box fstFrame PackGrow 0
    boxPackStart box sndFrame PackGrow 0
    if null label 
        then do
            containerAdd alignment frame
            containerAdd frame box
        else do
            containerAdd alignment box
    let injector = (\(f,s) -> inj1 f >> inj2 s)
    let extractor = do
        f <- ext1
        s <- ext2
        if isNothing f || isNothing s 
            then return Nothing 
            else return (Just (fromJust f, fromJust s))
    return ((castToWidget) alignment, injector, extractor, notiRef2) 

multisetEditor :: Show alpha => Editor alpha -> String -> Editor [alpha]
multisetEditor singleEditor labelS label =  do
    
    frame   <-  frameNew
    frameSetLabel frame label
    alignment <- alignmentNew xalign yalign xscale yscale
    alignmentSetPadding alignment paddingTop paddingBottom paddingLeft paddingRight
    vBox <- vBoxNew True 3
    vBox2 <- vBoxNew False 3
    (frameS,injS,extS,notS) <- singleEditor labelS     
    buttonBox <- hButtonBoxNew
    addButton <- buttonNewWithLabel "Add"
    removeButton <- buttonNewWithLabel "Remove"
    containerAdd buttonBox addButton
    containerAdd buttonBox removeButton

    listStore <- New.listStoreNew ([]:: [alpha])
    list <- New.treeViewNewWithModel listStore
    sel <- New.treeViewGetSelection list
    New.treeSelectionSetMode sel SelectionSingle
    renderer <- New.cellRendererTextNew
    col <- New.treeViewColumnNew
    New.treeViewAppendColumn list col    
    New.cellLayoutPackStart col renderer True
    New.cellLayoutSetAttributes col renderer listStore $ \row -> [ New.cellText := show row ]
    New.treeViewSetHeadersVisible list True

    boxPackStart vBox2 list PackGrow 0
    boxPackStart vBox buttonBox PackNatural 0
    boxPackEnd vBox frameS PackNatural 0
    boxPackEnd vBox2 vBox PackNatural 0
    containerAdd alignment frame
    containerAdd frame vBox2
    
    addButton `onClicked` do
        mbv <- extS
        case mbv of
            Just v -> New.listStoreAppend listStore v
            Nothing -> return ()
    removeButton `onClicked` do
        mbi <- New.treeSelectionGetSelected sel
        case mbi of
            Nothing -> return ()
            Just iter -> do
                [i] <- New.treeModelGetPath listStore iter 
                New.listStoreRemove listStore i
    let injector = \ la -> do
        New.listStoreClear listStore
        mapM_ (New.listStoreAppend listStore) la
    let extractor = do
        v <- listStoreGetValues listStore
        return (Just v)
    notiRef <- standardNotifiers list extractor label
    return ((castToWidget) alignment, injector, extractor, notiRef)
        
listStoreGetValues :: New.ListStore a -> IO [a]
listStoreGetValues listStore = do
    mbi <- New.treeModelGetIterFirst listStore
    getTail mbi 
    where getTail mbi = case mbi of
                            Nothing -> return []
                            Just iter -> do
                                [i] <- New.treeModelGetPath listStore iter
                                v <- New.listStoreGetValue listStore i
                                mbi2 <- New.treeModelIterNext listStore iter
                                rest <- getTail mbi2
                                return (v : rest)
        
versionEditor :: Editor Version
versionEditor label = do
    (wid,inj,ext,notiRef) <- stringEditor label
    let pinj v = inj (showVersion v)
    let pext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> do
                let l = (readP_to_S parseVersion) s
                if null l then
                    return Nothing
                    else return (Just (fst $head l))
    let handler = (do 
        v <- ext
        case v of
            Just _ -> return ()
            Nothing -> do
                md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                        $"Field " ++ label ++ " has invalid value. Please correct"
                dialogRun md
                widgetDestroy md
                return ())
    registerHandler notiRef handler "onFocusOut"
    return (wid, pinj, pext, notiRef)

standardNotifiers :: WidgetClass w => w -> Extractor beta -> String -> IO (IORef (Map String Notifier))
standardNotifiers w ext label = do
    notiRef <- newIORef Map.empty
    registerEvent  w notiRef (\ w f -> onFocusIn w (\e -> do f; return False)) "onFocusIn"
    registerEvent  w notiRef (\ w f -> onFocusOut w (\e -> do f; return False)) "onFocusOut"
--    let handler = (do 
--        v <- ext
--        case v of
--            Just _ -> return ()
--            Nothing -> do
--                md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
--                        $"Field " ++ label ++ " has invalid value. Please correct"
--                dialogRun md
--                widgetDestroy md
--                return ())
--    registerHandler notiRef handler "onFocusOut"
    return notiRef
 
registerEvent :: WidgetClass w =>  w -> IORef (Map String Notifier) -> (w -> IO () -> IO (ConnectId w)) 
                        -> String -> IO ()
registerEvent w notiRef registerFunc str = do
    registerFunc w (do
        noti <- readIORef notiRef
        case Map.lookup str noti of
            Nothing -> return ()
            Just handler -> do
                handler)
    return ()

registerHandler :: IORef (Map String Notifier) -> Notifier -> String -> IO ()
registerHandler notiRef handler string = do
    noti <- readIORef notiRef           
    case Map.lookup string noti of
        Nothing -> do
            let noti2 = Map.insert string handler noti
            writeIORef notiRef noti2 
        Just otherHandler -> do
            let noti2 = Map.insert string (handler >> otherHandler) noti
            writeIORef notiRef noti2 
       
         