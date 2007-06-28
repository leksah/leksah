--
-- | Module for saving, restoring and editing preferences
-- 


module Ghf.Preferences (
    readPrefs
,   writePrefs
--,   applyPrefs
,   editPrefs

,   prefsDescription
) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Text.PrettyPrint.HughesPJ as PP
import Control.Monad(foldM)
import Graphics.UI.Gtk hiding (afterToggleOverwrite)
import Control.Monad.Reader
import Data.Maybe(fromJust,isJust)
import Debug.Trace

import Ghf.Core

defaultPrefs = Prefs {
        showLineNumbers     =   True
    ,   rightMargin         =   Just 100
    ,   tabWidth            =   4
    ,   sourceCandy         =  Just("Default")
    ,   keymapName          =  "Default" 
    ,   defaultSize         =  (1024,800)}

data FieldDescription alpha =  FD {
        name                ::  String
    ,   comment             ::  String
    ,   fieldPrinter        ::  alpha -> PP.Doc
    ,   fieldParser         ::  alpha -> CharParser () alpha 
    ,   fieldEditor         ::  alpha -> IO Frame
}

field ::      String ->                     --name
              String ->                     --comment
              (beta -> PP.Doc) ->           --printer
              (CharParser () beta ) ->      --parser
              (alpha -> beta ) ->           --getter
              (beta -> alpha -> alpha ) ->  --setter
              (String -> (alpha -> beta ) -> (alpha -> IO (Frame))) -> 
              FieldDescription alpha 
field name comment printer parser getter setter widgetFunction =
    FD  name 
        comment
        (\ a -> (PP.text name PP.<> PP.colon) 
                PP.$$ (PP.nest 15 (printer (getter a)))        
                PP.$$ (PP.nest 5 (if null comment 
                                        then PP.empty 
                                        else PP.text $"--" ++ comment)))
        (\ a -> try (do
            symbol name
            colon
            value <- parser   
            return (setter value a)))
        (\ a -> widgetFunction name getter a)   
        
prefsDescription :: [FieldDescription Prefs] 
prefsDescription = [
        field "Show line numbers"
            "(True/False)" 
            (PP.text . show)
            boolParser
            showLineNumbers
            (\ b a -> (a{showLineNumbers = b}))
            boolEditWidget
    ,   field "Right margin"
            "Size or 0 for no right margin"
            (\a -> (PP.text . show) (case a of Nothing -> 0; Just i -> i))
            (do i <- intParser
                return (if i == 0 then Nothing else Just i))
            rightMargin
            (\b a -> a{rightMargin = b})
            (maybeWidget (intEditWidget 0.0 200.0 5.0))
    ,   field "Tab width" ""
            (PP.text . show) intParser
            tabWidth (\b a -> a{tabWidth = b})
            (intEditWidget 0.0 20.0 1.0)
    ,   field "Source candy" "Empty for do not use or the name of a candy file in a config dir)" 
            (\a -> PP.text (case a of Nothing -> ""; Just s -> s)) 
            (do id <- identifier
                return (if null id then Nothing else Just (id)))
            sourceCandy (\b a -> a{sourceCandy = b})
            (maybeWidget stringEditWidget)
    ,   field "Name of the keymap"  "The name of a keymap file in a config dir" 
            PP.text identifier
            keymapName (\b a -> a{keymapName = b})
            stringEditWidget
    ,   field "Window default size"
            "Default size of the main ghf window specified as pair (int,int)" 
            (PP.text.show) 
            (pairParser intParser)
            defaultSize (\(c,d) a -> a{defaultSize = (c,d)})
            (pairWidget (intEditWidget 0.0 3000.0 25.0))]


-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readPrefs :: FileName -> IO Prefs
readPrefs fn = do
    res <- parseFromFile (prefsParser defaultPrefs prefsDescription) fn
    case res of
        Left pe -> error $"Error reading prefs file " ++ show fn ++ " " ++ show pe
        Right r -> return r  

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

prefsParser :: Prefs -> [FieldDescription Prefs] -> CharParser () Prefs
prefsParser def descriptions = 
    let parsersF = map fieldParser descriptions in do     
        whiteSpace
        res <- applyFieldParsers def parsersF
        return res
        <?> "prefs parser" 

applyFieldParsers :: Prefs -> [Prefs -> CharParser () (Prefs)] -> CharParser () Prefs
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

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------

writePrefs :: FilePath -> Prefs -> IO ()
writePrefs fpath prefs = writeFile fpath (showPrefs prefs prefsDescription)

showPrefs :: a -> [FieldDescription a] -> String
showPrefs prefs prefsDesc = PP.render $
    foldl (\ doc (FD _ _ printer _ _) -> doc PP.$+$ printer prefs) PP.empty prefsDesc 

-- ------------------------------------------------------------
-- * Editing
-- ------------------------------------------------------------

editPrefs :: GhfAction
editPrefs = do
    p <- readGhf prefs
    res <- lift $editPrefs' p prefsDescription
    lift $putStrLn $show res

editPrefs' :: a -> [FieldDescription a] -> IO ()
editPrefs' prefs prefsDesc = do
    dialog  <- windowNew
    vb      <- vBoxNew False 12
    bb      <- hButtonBoxNew
    apply   <- buttonNewFromStock "gtk-apply"
    ok      <- buttonNewFromStock "gtk-ok"
    cancel  <- buttonNewFromStock "gtk-cancel"
    boxPackStart bb apply PackNatural 0
    boxPackStart bb ok PackNatural 0
    boxPackStart bb cancel PackNatural 0
   
    sbl <- mapM (\ (FD _ _ _ _ widgetF) -> widgetF prefs) prefsDesc 
    trace (show (length sbl)) return ()    
    mapM_ (\ sb -> boxPackStart vb sb PackNatural 0) sbl

    boxPackStart vb bb PackNatural 0
    containerAdd dialog vb
    widgetShowAll dialog    
    return ()
    


genericEditWidget :: Show beta => String -> (alpha -> beta) -> alpha -> IO (Frame)
genericEditWidget str getter dat = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame str
    entry   <-  entryNew
    entrySetText entry (show $getter dat)  
    containerAdd frame entry
    return frame            

stringEditWidget :: String -> (alpha -> String) -> alpha -> IO (Frame)
stringEditWidget str getter dat = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame str
    entry   <-  entryNew
    entrySetText entry (getter dat)  
    containerAdd frame entry
    return frame

intEditWidget :: Double -> Double -> Double -> String -> (alpha -> Int) -> alpha -> IO (Frame)
intEditWidget min max step str getter dat = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
    frameSetLabel frame str
    spin <- spinButtonNewWithRange min max step
    spinButtonSetValue spin (fromIntegral $getter dat) 
    containerAdd frame spin
    return frame
    
boolEditWidget :: String -> (alpha -> Bool) -> alpha -> IO (Frame)
boolEditWidget str getter dat = do
    frame   <-  frameNew
    frameSetShadowType frame ShadowNone
--    frameSetLabel frame str
    button   <-  checkButtonNewWithLabel str
    toggleButtonSetActive button (getter dat)  
    containerAdd frame button
    return frame            

maybeWidget :: (String -> (alpha -> beta) -> alpha -> IO (Frame)) -> 
        String -> (alpha -> Maybe beta) -> alpha -> IO (Frame)
maybeWidget justwidget str getter dat = do
    frame   <-  frameNew
    frameSetLabel frame str
    boolFrame <- boolEditWidget "" (isJust . getter) dat
    justFrame <- (if (isJust $getter dat) 
                    then justwidget "" (fromJust . getter) dat
                    else frameNew)
    vBox <- vBoxNew False 1
    boxPackStart vBox boolFrame PackNatural 0
    boxPackStart vBox justFrame PackNatural 0
    containerAdd frame vBox
    return frame    

pairWidget :: (String -> (alpha -> beta) -> alpha -> IO (Frame)) -> 
        String -> (alpha -> (beta,beta)) -> alpha -> IO (Frame)
pairWidget subwidget str getter dat = do
    frame   <-  frameNew
    frameSetLabel frame str
    firstFrame <- subwidget "" (fst . getter) dat
    secondFrame <- subwidget "" (fst . getter) dat
    vBox <- vBoxNew False 1
    boxPackStart vBox firstFrame PackNatural 0
    boxPackStart vBox secondFrame PackNatural 0
    containerAdd frame vBox
    return frame 

