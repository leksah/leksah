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
,   Notifier

,   FieldDescriptionE(..)
,   Default(..)
,   emptyNotifier

,   Parameters(..)
,   emptyParameters

,   boolEditor
,   stringEditor
,   multilineStringEditor
,   intEditor
,   maybeEditor
,   pairEditor
--,   eitherOrEditor
,   genericEditor
,   staticSelectionEditor
,   fileEditor
--,   multisetEditor

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
import Data.List(unzip4,elemIndex,filter)
import Data.Unique
import Text.ParserCombinators.ReadP(readP_to_S)
import Debug.Trace

import Ghf.Core hiding(label,name)
import qualified Text.PrettyPrint.HughesPJ as PP

class Default a where 
    getDefault      ::  a 

type Injector beta     =   beta -> IO()
type Extractor beta    =   IO(Maybe (beta))

data EventSelector  =   OnClicked
                    |   FocusOut
    deriving (Eq,Ord,Show)
                     
-- Returning True: The event has been handles
-- Returning False: Usual handling should proceed  
type Notifier        =   EventSelector -> Unique -> Maybe (Event -> IO Bool) -> IO ()   
type RegFunc         =   (w -> (Event -> IO (Bool)) -> IO (ConnectId w))
type NotifierSt     =   (IORef (Map EventSelector 
                            (Maybe Widget,RegFunc,Maybe (ConnectId Widget),[(Unique, Event -> IO Bool)])))

declareEvent :: Maybe Widget -> EventSelector -> RegFunc -> NotifierSt -> IO() 
declareEvent mbWidget eventSel regFunc notifierState = do
    noti <- readIORef notifierState     
    case Map.lookup eventSel noti of
        Nothing -> do
            let noti2 = Map.insert eventSel (mbWidget, Just id,[]) noti
             writeIORef notifierState noti2
        Just _ -> error "editor has already declared event " ++ show eventSel  

activateEvent :: Widget -> EventSelector -> NotifierSt -> IO()
activateEvent w eventSel notifierState = do
    noti <- readIORef notifierState     
    case Map.lookup eventSel noti of
        Nothing -> error "editor has not declared event before activating it " ++ show eventSel   
        Just (Nothing,f,mbci,l) ->
            let noti2 = Map.insert eventSel (w,f,mbci,l) noti
             writeIORef notifierState noti2
        Just _ -> error "editor has already been activated " ++ show eventSel  
 
mkNotifier :: NotifierSt alpha -> Notifier
mkNotifier notifierState = notFunc where
    notFunc eventSel uni (Just handler) = do
        noti <- readIORef notifierState           
        case Map.lookup eventSel noti of
            Nothing -> error "editor does not support event " ++ show eventSel   
            Just (w,f,Just id,l) -> do
                let noti2 = Map.insert eventSel (w,f,Just id,(uni,handler):l) noti
                writeIORef notifierState noti2
            Just (Nothing,l) -> do
                registerFunc widget (\ e -> do
                    noti <- readIORef notifierState
                    case Map.lookup eventSel noti of
                        Nothing -> return True
                        Just (_,[]) -> return True
                        Just (_,handlers) -> do
                            boolList <- mapM (\f -> f e) (map snd handlers)
                            return (map foldl (&&) True boolList))    
                let noti2 = Map.insert eventSel (Just id,handler:l) noti
                writeIORef notifierState noti2
    notFunc eventSel uni Nothing = do
        noti <- readIORef notifierState           
        case Map.lookup eventSel noti of
            Nothing -> error "editor does not support event " ++ show eventSel   
            Just (Just id,l) -> do
                let l2 = filter (\(u,_) -> u /= uni) l
                if null l2
                    then do
                        signalDisconnect id
                        let noti2 = Map.insert eventSel Nothing noti
                        writeIORef notifierState noti2
                    else do
                        let noti2 = Map.insert eventSel (Just id,l2) noti
                        writeIORef notifierState noti2

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
                     ,   spinRange       :: Maybe (Double,Double,Double) 
                                                --  | min max step
    }
                    
emptyParameters      =   Parameters Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance Default Parameters where
    getDefault       =   Parameters (Just "") (Just Horizontal) (Just ShadowNone) 
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3)) 
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3)) (Just (0.0,10.0,1.0))

type MkFieldDescriptionE alpha beta =
              String ->                         --name
              Maybe String ->                         --synopsis  
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
mkFieldDescriptionE name synopsis getter setter editor applicator =
    FDE name 
        synopsis
        (\ dat -> do
            (widget, inj,ext,notiRef) <- editor (emptyParameters{label = (Just name)})
            inj (getter dat)
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

mkEditor :: (Container -> Injector alpha) -> Extractor alpha -> Notifier -> Editor alpha       
mkEditor injectorC extractor notifier parameters = do
    let (xalign, yalign, xscale, yscale) = getParameter outerAlignment parameters
    outerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter outerPadding parameters
    alignmentSetPadding outerAlig paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew
    frameSetShadowType frame (getParameter showFrame parameters) 
    case getParameter label parameters of
        "" -> return ()
        str -> frameSetLabel frame str 
    containerAdd outerAlig frame
    let (xalign, yalign, xscale, yscale) =  getParameter innerAlignment parameters
    innerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter innerPadding parameters
    alignmentSetPadding innerAlig paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlig
    return ((castToWidget) outerAlig, injectorC (castToContainer innerAlig), extractor, notifier)

getParameter :: (Parameters -> (Maybe beta)) -> Parameters -> beta    
getParameter selector parameters =  
    let individual = selector parameters in
        case individual of
            Just it ->  it
            Nothing ->  case selector getDefault of
                            Just it -> it
                            Nothing -> error "default parameter should not be Nothing"       

-- ------------------------------------------------------------
-- * Simple Editors
-- ------------------------------------------------------------

boolEditor :: Editor Bool
boolEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget bool -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    button <- checkButtonNewWithLabel (getParameter label parameters)
                    containerAdd widget button
                    toggleButtonSetActive button bool
                    registerEvent button notifier 
                        (\w h -> onClicked w (do h (Event True); return ())) "onClicked"
                    writeIORef coreRef (Just button)  
                Just button -> toggleButtonSetActive button bool)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just button -> do
                    r <- toggleButtonGetActive button        
                    return (Just r))
        notifier
        parameters{label = (Just "")}

stringEditor :: Editor String
stringEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor
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
        parameters 

multilineStringEditor :: Editor String
multilineStringEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor 
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
        parameters

intEditor :: Editor Int
intEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
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
        parameters

genericEditor :: (Show beta, Read beta) => Editor beta
genericEditor parameters = do
    (wid,inj,ext,notif) <- stringEditor parameters
    let ginj v = inj (show v)
    let gext = do
        s <- ext
        case s of
            Nothing -> return Nothing
            Just s -> 
                let l = read s in
                if null l then
                    return Nothing
                    else return (Just (head l))
    return (wid,ginj,gext,notif) 

staticSelectionEditor :: (Show beta, Eq beta) => [beta] -> Editor beta
staticSelectionEditor list parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget obj -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    combo   <-  New.comboBoxNewText
                    New.comboBoxSetActive combo 1
                    mapM_ (\v -> New.comboBoxAppendText combo (show v)) list
                    containerAdd widget combo
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> New.comboBoxSetActive combo i
                        Nothing -> return ()
                    writeIORef coreRef (Just combo)
                Just combo -> do
                    let ind = elemIndex obj list
                    case ind of
                        Just i -> New.comboBoxSetActive combo i
                        Nothing -> return ())
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just combo -> do
                    ind <- New.comboBoxGetActive combo
                    case ind of
                        Just i -> return (Just (list !! i))
                        Nothing -> return Nothing)
        notifier
        parameters

fileEditor :: Editor FilePath
fileEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget filePath -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    button <- buttonNewWithLabel "Select file"
                    entry   <-  entryNew
                    registerEvent button notifier 
                        (\w h -> onClicked w (do h (Event True); return ())) "onClicked"
                    registerHandler notifier (buttonHandler entry) "onClicked"
                    box <- case getParameter direction parameters of 
                                Horizontal  -> do
                                    r <- hBoxNew False 1
                                    return (castToBox r)
                                Vertical    -> do
                                    r <- vBoxNew False 1
                                    return (castToBox r)
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
        notifier 
        parameters                                              where
    buttonHandler entry = (\ e -> do
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
            Nothing -> return True
            Just fn -> do
                entrySetText entry fn
                return True) 


-- ------------------------------------------------------------
-- * Composition editors
-- ------------------------------------------------------------

pairEditor :: (Editor alpha, Parameters) -> (Editor beta, Parameters) -> Editor (alpha,beta)
pairEditor (fstEd,fstPara) (sndEd,sndPara) parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget (v1,v2) -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    fst@(fstFrame,inj1,ext1,notiRef1) <- fstEd fstPara
                    snd@(sndFrame,inj2,ext2,notiRef2) <- sndEd sndPara
                    box <- case getParameter direction parameters of
                        Horizontal -> do 
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box fstFrame PackGrow 0
                    boxPackStart box sndFrame PackGrow 0
                    containerAdd widget box
                    inj1 v1
                    inj2 v2
                    writeIORef coreRef (Just (fst,snd)) 
                Just ((_,inj1,_,_),(_,inj2,_,_)) -> do
                    inj1 v1
                    inj2 v2)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just ((_,_,ext1,_),(_,_,ext2,_)) -> do
                    r1 <- ext1
                    r2 <- ext2
                    if isJust r1 && isJust r2 
                        then return (Just (fromJust r1,fromJust r2))
                        else return Nothing)    
        notifier
        parameters

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- | or deselected (if the positive Argument is False)
--

maybeEditor :: Default beta => (Editor beta, Parameters) -> Bool -> String -> Editor (Maybe beta)
maybeEditor (childEdit, childParams) positive boolLabel parameters = do  
    coreRef <- newIORef Nothing
    childRef  <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor   
        (\widget mbVal -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    be@(boolFrame,inj1,ext1,notifierBool) <- boolEditor emptyParameters
                    box <- case getParameter direction parameters of
                        Horizontal -> do 
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box boolFrame PackNatural 0
                    containerAdd widget box
                    registerHandler notifierBool (onClickedHandler widget coreRef childRef) "onClicked"
                    case mbVal of 
                        Nothing -> do
                            inj1 (if positive then False else True)
                        Just val -> do
                            (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams
                            boxPackEnd box childWidget PackNatural 0
                            widgetShowAll childWidget
                            inj1 (if positive then True else False)
                            inj2 val
                    writeIORef coreRef (Just (be,box)) 
                Just (be@(boolFrame,inj1,ext1,notiRef1),box) -> do
                    hasChild <- hasChildEditor childRef 
                    case mbVal of 
                        Nothing -> do
                            if hasChild
                                then do
                                    (childWidget,_,_,_) <- getChildEditor childRef childEdit childParams
                                    inj1 (if positive then False else True)
                                    widgetHideAll childWidget
                                else inj1 (if positive then False else True)
                        Just val -> do
                            if hasChild
                                then do
                                    (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams
                                    widgetShowAll childWidget
                                    inj2 val
                                else do
                                    (childWidget,inj2,_,_) <- getChildEditor childRef childEdit childParams
                                    boxPackEnd box childWidget PackNatural 0
                                    widgetShowAll childWidget
                                    inj2 val)
        (do
            core <- readIORef coreRef
            case core of 
                Nothing  -> return Nothing
                Just (be@(boolFrame,inj1,ext1,notiRef1),_) -> do
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
        notifier  
        parameters                                          where
    onClickedHandler widget coreRef childRef = (\ event -> do 
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
                    Nothing -> return ()
                return True) 
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

eitherOrEditor :: (Default alpha, Default beta) => (Editor alpha, Parameters) -> 
                        (Editor beta, Parameters) -> Editor (Either alpha beta)
eitherOrEditor (leftEditor,leftParams) (rightEditor,rightParams)  parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget v -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    be@(boolFrame,inj1,ext1,notiRef1) <- boolEditor  parameters
                    le@(leftFrame,inj2,ext2,notiRef2) <- leftEditor leftParams
                    re@(rightFrame,inj3,ext3,notiRef3) <- rightEditor rightParams
                    box <- case getParameter direction parameters of
                        Horizontal -> do 
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)                    
                    boxPackStart box boolFrame PackNatural 0
                    boxPackStart box leftFrame PackNatural 0
                    boxPackStart box rightFrame PackNatural 0
                    containerAdd widget box
                    case v of
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
                    writeIORef coreRef (Just (be,le,re))  
                Just ((_,inj1,_,_),(leftFrame,inj2,_,_),(rightFrame,inj3,_,_)) -> do
                    case v of
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
                              inj1 False)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just ((_,_,ext1,_),(_,_,ext2,_),(_,_,ext3,_)) -> do
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
                                Just value -> return (Just (Right value))) 
        notifier
        parameters

multisetEditor :: Show alpha => (Editor alpha,Parameters) -> Editor [alpha]
multisetEditor (singleEditor,parameters) = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget v -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    (box,buttonBox) <- case getParameter direction parameters of
                        Horizontal -> do 
                            b  <- hBoxNew False 1
                            bb <- vButtonBoxNew
                            return (castToBox b,castToButtonBox bb)
                        Vertical -> do
                            b  <- vBoxNew False 1
                            bb <- hButtonBoxNew
                            return (castToBox b,castToButtonBox bb)    
                    (frameS,injS,extS,notS) <- singleEditor $getParameter label parameters  
                    addButton <- buttonNewWithLabel "Add"
                    removeButton <- buttonNewWithLabel "Remove"
{--    addButton `onClicked` do
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
                New.listStoreRemove listStore i--}
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
                    boxPackStart box list PackGrow 0
                    boxPackStart box buttonBox PackNatural 0
                    boxPackEnd box frameS PackNatural 0
                    containerAdd widget box
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) v
                    writeIORef coreRef (Just listStore) 
                Just listStore -> do
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) v)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just listStore -> do
                    v <- listStoreGetValue listStore
                    return (Just v))
        notifier
        parameters
                            where
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
    


        


{--
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
--} 
       
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