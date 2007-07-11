--
-- | Module for saving, restoring and editing preferences
-- 

module Ghf.Editor.PropertyEditor (

    Getter
,   Setter
,   Injector
,   Extractor
,   Editor
,   Applicator

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

import Control.Monad(foldM)
import Graphics.UI.Gtk hiding (afterToggleOverwrite,Focus,onChanged)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.ModelView as New
import Text.ParserCombinators.Parsec hiding (Parser,label)
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

import Ghf.Core hiding(label,name)
import Ghf.PrinterParser
import qualified Text.PrettyPrint.HughesPJ as PP

class Default a where 
    getDefault       ::  a 

type Injector beta      =   beta -> IO()
type Extractor beta     =   IO(Maybe (beta))
  
type Notifier        =   IORef (Map String (Event ->IO Bool))

emptyNotifier        ::  IO (IORef (Map String (Event -> IO Bool)))
emptyNotifier        =   newIORef(Map.empty)

type Editor beta        =   Parameters -> IO(Widget, Injector beta, Extractor beta,Notifier)
type Applicator beta    =   beta -> GhfAction

data Parameters      =   Parameters {   
                         label      :: Maybe String
                     ,   direction  :: Maybe Direction
                     ,   showFrame  :: Maybe ShadowType
                     ,   outerAlignment  :: Maybe (Float,Float,Float,Float)
                                                -- | xalign yalign xscale yscale
                     ,   outerPadding    :: Maybe (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                     ,   innerAlignment  :: Maybe (Float,Float,Float,Float)
                                                -- | xalign yalign xscale yscale
                     ,   innerPadding    :: Maybe (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                     ,   spinRange       :: Maybe (Float,Float,Float) 
                                                --  | min max step
    }
                    
emptyParameters      =   Parameters Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Default Parameters where
    getDefault       =   Parameters (Just "") (Just Horizontal) (Just ShadowNone) 
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3)) 
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3)) Nothing

type MkFieldDescriptionE alpha beta =
              String ->                         --name
              Maybe String ->                         --synopsis  
              (Printer beta) ->                 
              (Parser beta) ->
              (Getter alpha beta) ->            
              (Setter alpha beta) ->            
              (Editor beta) ->
              (Applicator beta) ->
              FieldDescriptionE alpha

data FieldDescriptionE alpha =  FDE {
        name                ::  String
    ,   synopsis            ::  Maybe String
    ,   fieldEditor         ::  alpha -> IO (Widget, alpha -> IO(), alpha -> IO(alpha), Notifier)
    ,   fieldApplicator     ::  alpha -> alpha -> GhfAction
    }

mkFieldDescriptionE :: Eq beta => MkFieldDescriptionE alpha beta
mkFieldDescriptionE name synopsis printer parser getter setter editor applicator =
    FDE name 
        synopsis
        (\ dat -> do
            (widget, inj,ext,notiRef) <- editor (emptyParameters{label = (Just name)})
            inj (getter dat)
{--            let handler = (do 
                v <- ext
                case v of
                    Just _ -> return ()
                    Nothing -> do
                        md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                                $"Field " ++ name ++ " has invalid value. Please correct"
                        dialogRun md
                        widgetDestroy md
                        return ())
            registerHandler notiRef handler "onFocusOut"--}
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
-- * Editing
-- ------------------------------------------------------------

mkEditor :: Parameters -> (Widget -> Injector alpha ) -> Extractor alpha  -> Notifier -> Editor alpha       
mkEditor parameters injectorC extractor notifier = do
    let (xalign, yalign, xscale, yscale) = getParameter outerAlignment parameters
    outerAlignment <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter outerPadding parameters
    alignmentSetPadding outerAlignment paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew
    frameSetShadowType frame (getParameter showFrame) 
    containerAdd outerAlignment frame
    let (xalign, yalign, xscale, yscale) =  getParameter innerAlignment parameters
    innerAlignment <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter innerPadding parameters
    alignmentSetPadding innerAlignment paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlignment
    return ((castToWidget) outerAlignment, injectorC innerAlignment, extractor, notifier)  where 

getParameter selector parameters =  
    let individual = selector parameters in
        case individual of
            Just it -> it
            Nothing -> selector getDefault     
    
boolEditor :: Editor Bool
boolEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- newIORef emptyNotifier
    mkEditor parameters  
        (\widget bool -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    button <- checkButtonNewWithLabel label
                    containerAdd widget button
                    toggleButtonSetActive button bool
                    registerEvent button notifier onChanged "onChanged"
                    writeIORef coreRef (Just button)  
                Just button -> toggleButtonSetActive button bool)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just button -> do
                    r <- toggleButtonGetActive button        
                    return (Just r))
        notifier

stringEditor :: Editor String
stringEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- newIORef emptyNotifier
    mkEditor parameters  
        (\widget string -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    entry   <-  entryNew
                    containerAdd widget entry
                    entrySetText entry string
                    writeIORef coreRef (Just entry) 
                Just entry -> entrySetText entry string)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just entry -> do
                    r <- entryGetText entry       
                    return (Just r))
        notifier

multilineStringEditor :: Editor String
multilineStringEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- newIORef emptyNotifier
    mkEditor parameters  
        (\widget string -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    textView   <-  textViewNew
                    containerAdd widget textView
                    buffer <- textViewGetBuffer textView
                    textBufferSetText buffer string
                    writeIORef coreRef (Just textView) 
                Just textView -> do
                    buffer <- textViewGetBuffer textView
                    textBufferSetText buffer string)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just textView -> do
                    buffer <- textViewGetBuffer textView
                    start <- textBufferGetStartIter buffer
                    end <- textBufferGetEndIter buffer
                    r <- textBufferGetText buffer start end False
                    return (Just r))
        notifier

intEditor :: Editor Int
intEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- newIORef emptyNotifier
    mkEditor parameters  
        (\widget v -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    let (min, max, step) = getParameter spinRange parameters 
                    spin <- spinButtonNewWithRange min max step
                    containerAdd widget spin
                    spinButtonSetValue spin (fromIntegral v)
                    writeIORef coreRef (Just spin) 
                Just spin -> spinButtonSetValue spin (fromIntegral v))
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just spin -> do
                    newNum <- spinButtonGetValue spin
                    return (Just (truncate newNum)))
        notifier

genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor parameters = do
    (wid,inj,ext,notif) <- stringEditor name
    let ginj v = inj (show v)
    let gext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> 
                let l = read s in
                if null l then
                    return Nothing
                    else return (Just l)
    return (wid,ginj,gext,notif) 

staticSelectionEditor :: (Show beta, Eq beta) => [beta] -> Editor beta
staticSelectionEditor list parameters = do
    coreRef <- newIORef Nothing
    notifier <- newIORef emptyNotifier
    mkEditor parameters  
        (\widget ind -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    combo   <-  New.comboBoxNewText
                    New.comboBoxSetActive combo 1
                    mapM_ (\v -> New.comboBoxAppendText combo (show v)) list
                    containerAdd widget combo
                    New.comboBoxSetActive combo ind
                    writeIORef coreRef (Just combo)
                Just entry -> entrySetText entry string)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just combo -> do
                    ind <- New.comboBoxGetActive combo
                    return (Just (list !! ind)))
        notifier

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
fileEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- newIORef emptyNotifier
    mkEditor parameters  
        (\widget filePath -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    button <- buttonNewWithLabel "Select file"
                    entry   <-  entryNew
                    button `onClicked` (buttonHandler entry)   
                    box <- case getParameter direction of 
                                Horizontal  -> (castToBox) hBoxNew False 1
                                Vertical    -> (castToBox) vBoxNew False 1
                    boxPackStart box entry PackGrow 0
                    boxPackEnd box button PackNatural 0
                    containerAdd widget box
                    entrySetText entry filePath
                    writeIORef coreRef (Just entry) 
                Just entry -> entrySetText entry filePath)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just entry -> do
                    str <- entryGetText entry
                    dfe <- doesFileExist str
                    if dfe 
                        then return (Just str) 
                        else return Nothing )
        notifier where
    buttonHandler entry = do
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


-- ------------------------------------------------------------
-- * Composition editors
-- ------------------------------------------------------------

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- | or deselected (if the positive Argument is False)
--

maybeEditor :: Default beta => (Editor beta, Parameters) -> Bool -> String -> Editor (Maybe beta)
maybeEditor (childEdit, childParams) positive boolLabel parameters = do  
    coreRef <- newIORef Nothing
    childRef  <- newIORef Nothing
    notifier <- newIORef emptyNotifier
    mkEditor parameters  
        (\widget mbVal -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    be@(boolFrame,inj1,ext1,notifierBool) <- boolEditor emptyParameters
                    vBox <- vBoxNew False 0
                    boxPackStart vBox boolFrame PackNatural 0
                    containerAdd widget vBox
                    registerHandler notifierBool (onClickedHandler widget) "onClicked"
                    case mbVal of 
                        Nothing -> do
                            inj1 (if positive then False else True)
                        Just val -> do
                            (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams
                            boxPackEnd vBox childWidget PackNatural 0
                            widgetShowAll childWidget
                            inj1 (if positive then True else False)
                            inj2 val
                    writeIORef coreRef (Just (be,vBox)) 
                Just (be@(boolFrame,inj1,ext1,notiRef1),vBox) -> 
                    case mbVal of 
                        Nothing -> do
                            if hasChildEditor childRef 
                                then do
                                    (childWidget,_,_,_) <- getChildEditor childRef childEdit childParams
                                    inj1 (if positive then False else True)
                                    widgetHideAll childWidget
                                else inj1 (if positive then False else True)
                        Just val -> do
                            if hasChildEditor childRef 
                                then do
                                    (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams
                                    widgetShowAll childWidget
                                    inj2 val
                                else do
                                    (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams
                                    boxPackEnd vBox childWidget PackNatural 0
                                    widgetShowAll childWidget
                                    inj2 val)
        (do
            core <- readIORef coreRef
            case core of 
                Nothing  -> return Nothing
                Just be@(boolFrame,inj1,ext1,notiRef1) -> do
                    bool <- ext1
                    case bool of
                        Nothing -> return Nothing
                        Just bv | bv == positive -> do
                            (_,_,ext2,_) <- getChildEditor childRef childEdit childParams
                            value <- ext2
                            case value of
                                Nothing -> return Nothing
                                Just value -> return (Just (Just value))
                        otherwise -> return (Just Nothing))
        notifier  where
    onClickedHandler widget coreRef childRef = (do 
        core <- readIORef coreRef
        case core of 
            Nothing  -> error "Impossible"
            Just (be@(boolFrame,inj1,ext1,notiRef1),vBox) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->  
                        if not (bool == positive) 
                            then do
                                hasChild <- hasChildEditor childRef
                                if hasChild 
                                    then do
                                        (childWidget,_,_,_) <- getChildEditor childRef childEdit childParams
                                        widgetHideAll childWidget
                                    else return () 
                            else do     
                                hasChild <- hasChildEditor childRef
                                if hasChild 
                                    then do
                                        (childWidget,_,_,_) <- getChildEditor childRef childEdit childParams
                                        widgetShowAll childWidget  
                                    else do
                                        (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams
                                        boxPackEnd vBox childWidget PackNatural 0
                                        widgetShowAll childWidget
                                        inj2 getDefault        
                    Nothing -> return ()) 
    getChildEditor childRef childEditor childLabel =  do
        mb <- trace "getChildEditor" $readIORef childRef
        case mb of
            Just editor -> return editor
            Nothing -> do
                let val = childEditor 
                editor <- childEditor childLabel
                writeIORef childRef (Just editor)
                return editor
    hasChildEditor childRef =  do
        mb <- readIORef childRef
        return (isJust mb)

{--
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

--}
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
       
         