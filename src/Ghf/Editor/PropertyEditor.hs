--
-- | Module for editing options in a GUI dialog
-- 

module Ghf.Editor.PropertyEditor (
-- * Types
    Getter
,   Setter
,   Injector
,   Extractor
,   Editor
--,   Applicator
,   Notifier

,   FieldDescriptionE(..)
,   mkFieldE
,   Default(..)
,   emptyNotifier

,   Parameters(..)
,   emptyParams
,   getParameter

,   validate

,   boolEditor
,   stringEditor
,   multilineStringEditor
,   intEditor
,   maybeEditor
,   pairEditor
,   eitherOrEditor
,   genericEditor
,   staticSelectionEditor
--,   staticMultiselectionEditor
,   fileEditor
,   multisetEditor

) where

import Control.Monad(foldM)
import Graphics.UI.Gtk hiding (afterToggleOverwrite,Focus,onChanged)
import Graphics.UI.Gtk.SourceView
import Graphics.UI.Gtk.ModelView as New
import Text.ParserCombinators.Parsec hiding (Parser)
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
import qualified Text.PrettyPrint.HughesPJ as PP

import Ghf.Core

-- ---------------------------------------------------------------------
-- * Basic Types
--

--
-- | A type for getting a field of a record
--
type Getter alpha beta     =   alpha -> beta
--
-- | A type for setting the field of a record
--
type Setter alpha beta     =   beta -> alpha -> alpha

--
-- | A type for injecting a value into an editor
--
type Injector beta     =   beta -> IO()
--
-- | A type for extracting a value from an editor
--
type Extractor beta    =   IO(Maybe (beta))

--
-- | A type for handling a gtk event
-- | Returning True: The event has been handles
-- | Returning False: Usual handling should proceed  
type Handler        =   Event -> IO Bool

--
-- | A type to register or unregister a handler
-- | If the second argument is Left Handler the handler gets registered
-- | If the second argument is Right Unique the handler will be removed
-- | The returned unique value can be used for unregistering an event
type Notifier       =   EventSelector -> Either Handler Unique -> IO (Unique)   

--
-- | A type to describe an editor. 
-- | beta is the type of the individual field of the record
type Editor beta       =   Parameters -> IO(Widget, Injector beta, Extractor beta, Notifier)

--
-- | A constructor type for a field desciption 
--
type MkFieldDescriptionE alpha beta =
    Parameters ->                         
    (Getter alpha beta) ->            
    (Setter alpha beta) ->            
    (Editor beta) ->
    FieldDescriptionE alpha

--
-- | A type to describe a field of a record, which can be edited
-- | alpha is the type of the individual field of the record
data FieldDescriptionE alpha =  FDE {
        parameters  ::  Parameters
    ,   fieldEditor ::  alpha -> IO (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
    }

data EventSelector  =   Clicked
                    |   FocusOut -- |...
    deriving (Eq,Ord,Show)
--
-- | A class for providing default values for certain types of editors
--
class Default a where 
    getDefault      ::  a 

instance Default Int where 
    getDefault = 1

instance Default String where 
    getDefault = ""

instance Default a => Default (Either a b) 
    where 
        getDefault =  Left(getDefault) 

instance (Default alpha, Default beta) => Default (alpha,beta) 
    where getDefault = (getDefault,getDefault)


--type Applicator beta =   beta -> GhfAction ()

--
-- | A type for parameters for editors
--
data Parameters     =   Parameters  {   
                        paraName        :: Maybe String
                    ,   synopsis        :: Maybe String
                    ,   direction       :: Maybe Direction
                    ,   shadow          :: Maybe ShadowType
                    ,   outerAlignment  :: Maybe (Float,Float,Float,Float)
                                               -- | xalign yalign xscale yscale
                    ,   outerPadding    :: Maybe (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    ,   innerAlignment  :: Maybe (Float,Float,Float,Float)
                                                -- | xalign yalign xscale yscale
                    ,   innerPadding    :: Maybe (Int,Int,Int,Int)
                                                --  | paddingTop paddingBottom paddingLeft paddingRight
                    ,   minSize         :: Maybe (Int, Int)      
    }
                    
emptyParams     =   Parameters Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

--
-- | Convenience method to get a parameter, or if not set the default parameter
--
getParameter :: (Parameters -> (Maybe beta)) -> Parameters -> beta    
getParameter selector parameters =  
    let individual = selector parameters in
        case individual of
            Just it ->  it
            Nothing ->  case selector getDefault of
                            Just it -> it
                            Nothing -> error "default parameter should not be Nothing"       

instance Default Parameters where
    getDefault      =   Parameters (Just "") (Just "") (Just Horizontal) (Just ShadowNone) 
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3)) 
                            (Just (0.5, 0.5, 0.95, 0.95)) (Just (2, 5, 3, 3)) (Just (-1,-1))

--
-- | Convenience method to validate and extract fields
--
validate :: alpha -> [alpha -> Extractor alpha] -> IO (Maybe alpha) 
validate val getExts = do
    newVal <- foldM (\ a b -> case a of 
                                Just a -> b a
                                Nothing -> return Nothing) (Just val) getExts
    if isNothing newVal 
        then do
            md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                        $ "Fields have invalid value. Please correct"
            dialogRun md
            widgetDestroy md
            return Nothing
        else return newVal 

-- ------------------------------------------------------------
-- * Implementation of notifications
-- ------------------------------------------------------------

--
-- | A type for a function to register an event
--
type RegFunc        =   Widget -> Handler -> IO (ConnectId Widget)

--
-- | A type for the state of the notification system
--
type NotifierSt     =   IORef (Map EventSelector 
                            (Maybe Widget,RegFunc,Maybe (ConnectId Widget),[(Unique, Handler)]))

--
-- | Initial state of the notification system
--
emptyNotifier        ::  IO (NotifierSt)
emptyNotifier        =   newIORef(Map.empty)

--
-- | Declare that the event can be thrown from this editor
--
declareEvent :: Maybe Widget -> EventSelector -> RegFunc -> NotifierSt -> IO() 
declareEvent mbWidget eventSel regFunc notifierState = do
    noti <- readIORef notifierState     
    case Map.lookup eventSel noti of
        Nothing -> do
             let noti2 = Map.insert eventSel (mbWidget, regFunc, Nothing,[]) noti
             writeIORef notifierState noti2
        Just _ -> error $"editor has already declared event " ++ show eventSel  

--
-- | Activate the event after the widget has been constructed
--        
activateEvent :: Widget -> EventSelector -> NotifierSt -> IO()
activateEvent widget eventSel notifierState = do
    noti <- readIORef notifierState     
    case Map.lookup eventSel noti of
        Nothing -> error $"editor has not declared event before activating it " ++ show eventSel   
        Just (Nothing,registerFunc,Nothing,handlers) -> do
            cid <- registerFunc widget (\ e -> do
                noti <- readIORef notifierState
                case Map.lookup eventSel noti of
                    Nothing -> return True
                    Just (_,_,_,[]) -> return True
                    Just (_,_,_,handlers2) -> do
                        boolList <- mapM (\f -> f e) (map snd handlers2)
                        return (foldl (&&) True boolList))              
            let noti2 = Map.insert eventSel (Just widget,registerFunc,Just cid,handlers) noti
            writeIORef notifierState noti2
        Just _ -> error $"editor has already been activated " ++ show eventSel

--
-- | Constructor for a notifier
--  
mkNotifier :: NotifierSt -> Notifier
mkNotifier notifierState = notFunc 
    where
    notFunc :: EventSelector -> Either Handler Unique -> IO (Unique)   
    notFunc eventSel (Left handler) = do
        noti <- readIORef notifierState
        uni <- newUnique           
        case Map.lookup eventSel noti of
            Nothing -> error $"editor does not support event " ++ show eventSel   
            Just (Just widget,registerFunc,Nothing,handlers) 
                    -> error $"mkNotifier for activated event" ++ show eventSel
            Just (mbWidget, registerFunc, mbUnique, handlers) 
                    -> do   unique <- newUnique
                            let noti2 = Map.insert eventSel 
                                    (mbWidget,registerFunc,mbUnique,(uni,handler):handlers) noti
                            writeIORef notifierState noti2
                            return unique
    notFunc eventSel (Right uni) = do
        noti <- readIORef notifierState           
        case Map.lookup eventSel noti of
            Nothing -> error $"editor does not support event " ++ show eventSel   
            Just (mbWidget,regFunc,Just cid,l) -> do
                let l2 = filter (\(u,_) -> u /= uni) l
                if null l2
                    then do
                        signalDisconnect cid
                        let noti2 = Map.insert eventSel (mbWidget,regFunc,Nothing,[]) noti
                        writeIORef notifierState noti2
                    else do
                        let noti2 = Map.insert eventSel (mbWidget,regFunc,Just cid,l2) noti
                        writeIORef notifierState noti2
        return uni

-- ------------------------------------------------------------
-- * Implementation of editing
-- ------------------------------------------------------------

--
-- | Function to construct a field description 
--  
mkFieldE :: Eq beta => MkFieldDescriptionE alpha beta
mkFieldE parameters getter setter editor =
    FDE parameters
        (\ dat -> do
            (widget, inj,ext,notiRef) <- editor parameters
            inj (getter dat)
            return (widget,
                    (\a -> inj (getter a)), 
                    (\a -> do 
                        b <- ext
                        case b of
                            Just b -> return (Just (setter b a))
                            Nothing -> return Nothing),
                    notiRef))

-- | Function to construct an editor
--  
mkEditor :: (Container -> Injector alpha) -> Extractor alpha -> Notifier -> Editor alpha       
mkEditor injectorC extractor notifier parameters = do
    let (xalign, yalign, xscale, yscale) = getParameter outerAlignment parameters
    outerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter outerPadding parameters
    alignmentSetPadding outerAlig paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew
    frameSetShadowType frame (getParameter shadow parameters) 
    case getParameter paraName parameters of
        "" -> return ()
        str -> frameSetLabel frame str 
    containerAdd outerAlig frame
    let (xalign, yalign, xscale, yscale) =  getParameter innerAlignment parameters
    innerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter innerPadding parameters
    alignmentSetPadding innerAlig paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlig
    let (x,y) = getParameter minSize parameters
    widgetSetSizeRequest outerAlig x y
    return ((castToWidget) outerAlig, injectorC (castToContainer innerAlig), extractor, notifier)

-- ------------------------------------------------------------
-- * Simple Editors
-- ------------------------------------------------------------

instance ContainerClass Widget
instance BinClass Widget
instance ButtonClass Widget

--
-- | Editor for a boolean value in the form of a check button
--  
boolEditor :: Editor Bool
boolEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent Nothing Clicked (\w h -> w `onClicked` do  h (Event True); return ()) 
        notifier 
    mkEditor  
        (\widget bool -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    button <- checkButtonNewWithLabel (getParameter paraName parameters) 
                    containerAdd widget button
                    toggleButtonSetActive button bool
                    activateEvent (castToWidget button) Clicked notifier
                    writeIORef coreRef (Just button)  
                Just button -> toggleButtonSetActive button bool)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just button -> do
                    r <- toggleButtonGetActive button        
                    return (Just r))
        (mkNotifier notifier)
        parameters {paraName = Just " "} 

--
-- | Editor for a boolean value in the form of two radio buttons
--  
boolEditor2 :: String -> Editor Bool
boolEditor2 label2 parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent Nothing Clicked (\w h -> w `onClicked` do  h (Event True); return ()) 
        notifier 
    mkEditor  
        (\widget bool -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    box <- vBoxNew True 2
                    radio1 <- radioButtonNewWithLabel (getParameter paraName parameters) 
                    radio2 <- radioButtonNewWithLabelFromWidget radio1 label2
                    boxPackStart box radio1 PackGrow 2
                    boxPackStart box radio2 PackGrow 2
                    containerAdd widget box
                    if bool
                        then do
                            toggleButtonSetActive radio1 True
                        else do
                            toggleButtonSetActive radio2 True
                    activateEvent (castToWidget radio1) Clicked notifier
                    writeIORef coreRef (Just (radio1,radio2))  
                Just (radio1,radio2) -> 
                    if bool
                        then do
                            toggleButtonSetActive radio1 True
                        else do
                            toggleButtonSetActive radio2 True)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just (radio1,radio2) -> do
                    r <- toggleButtonGetActive radio1        
                    return (Just r))
        (mkNotifier notifier)
        parameters {paraName = Just " "} 

--
-- | Editor for a string in the form of a text entry
--  
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
        (mkNotifier notifier)
        parameters 

--
-- | Editor for a multiline string in the form of a multiline text entry
--  
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
        (mkNotifier notifier)
        parameters

--
-- | Editor for an integer in the form of a spin entry
--  
intEditor :: (Double,Double,Double) -> Editor Int
intEditor (min, max, step) parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget v -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
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
        (mkNotifier notifier)
        parameters

--
-- | Editor for for any value which is an instance of Read and Show in the form of a 
-- | text entry
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

--
-- | Editor for the selection of an element from a static list of elements in the
-- | form of a combo box
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
        (mkNotifier notifier)
        parameters


--
-- | Editor for the selection of some elements from a static list of elements in the
-- | form of a list box
{--
staticMultiselectionEditor :: (Show beta, Eq beta) => [beta] -> Editor [beta]
staticMultiselectionEditor list parameters = do
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
                        Just i -> return (Just [(list !! i)])
                        Nothing -> return Nothing)
        (mkNotifier notifier)
        parameters
--}


--
-- | Editor for the selection of a file path in the form of a text entry and a button,
-- | which opens a gtk file chooser
fileEditor :: Editor FilePath
fileEditor parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    declareEvent Nothing Clicked 
        (\widget handler -> do  widget `onClicked` do   
                                    handler (Event True)
                                    return ()) notifier 
    mkEditor  
        (\widget filePath -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    button <- buttonNewWithLabel "Select file"
                    entry   <-  entryNew
                    activateEvent (castToWidget button) Clicked notifier
                    uni <- newUnique 
                    (mkNotifier notifier) Clicked (Left (buttonHandler entry)) 
                   -- registerHandler notifier (buttonHandler entry) "onClicked"
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
        (mkNotifier notifier) 
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

--
-- | An editor which composes two subeditors
-- 
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
        (mkNotifier notifier)
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
                    be@(boolFrame,inj1,ext1,notifierBool) <- boolEditor 
                            emptyParams {paraName = Just boolLabel} 
                    box <- case getParameter direction parameters of
                        Horizontal -> do 
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)
                    boxPackStart box boolFrame PackNatural 0
                    containerAdd widget box
                    notifierBool Clicked (Left (onClickedHandler widget coreRef childRef))
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
        (mkNotifier notifier)  
        parameters 
    where
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
                                        inj2 getDefault 
                                        widgetShowAll childWidget
                    Nothing -> return ()
                return True) 
    getChildEditor childRef childEditor childLabel =  do
        mb <- readIORef childRef
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

--
-- | An editor with a subeditor which gets active, when a checkbox is selected
-- | or deselected (if the positive Argument is False)
eitherOrEditor :: (Default alpha, Default beta) => (Editor alpha, Parameters) -> 
                        (Editor beta, Parameters) -> String -> Editor (Either alpha beta)
eitherOrEditor (leftEditor,leftParams) (rightEditor,rightParams) label2 parameters = do
    coreRef <- newIORef Nothing
    notifier <- emptyNotifier
    mkEditor  
        (\widget v -> do 
            core <- readIORef coreRef
            case core of 
                Nothing  -> do
                    be@(boolFrame,inj1,ext1,notiRef1) <- boolEditor2  label2 parameters
                    le@(leftFrame,inj2,ext2,notiRef2) <- leftEditor leftParams
                    re@(rightFrame,inj3,ext3,notiRef3) <- rightEditor rightParams
                    notiRef1 Clicked (Left (onClickedHandler widget coreRef))
                    box <- case getParameter direction parameters of
                        Horizontal -> do 
                            b <- hBoxNew False 1
                            return (castToBox b)
                        Vertical -> do
                            b <- vBoxNew False 1
                            return (castToBox b)                    
                    boxPackStart box boolFrame PackNatural 0
                    containerAdd widget box
                    case v of
                        Left vl -> do
                          boxPackStart box leftFrame PackNatural 0
                          inj2 vl
                          inj3 getDefault
                          inj1 True
                        Right vr  -> do
                          boxPackStart box rightFrame PackNatural 0
                          inj3 vr
                          inj2 getDefault
                          inj1 False
                    writeIORef coreRef (Just (be,le,re,box))  
                Just ((_,inj1,_,_),(leftFrame,inj2,_,_),(rightFrame,inj3,_,_),box) -> do
                    case v of
                            Left vl -> do
                              containerRemove box rightFrame
                              boxPackStart box leftFrame PackNatural 0 
                              inj2 vl
                              inj3 getDefault
                              inj1 True
                            Right vr  -> do
                              containerRemove box leftFrame  
                              boxPackStart box rightFrame PackNatural 0
                              inj3 vr
                              inj2 getDefault
                              inj1 False)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just ((_,_,ext1,_),(_,_,ext2,_),(_,_,ext3,_),_) -> do
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
        (mkNotifier notifier)
        parameters {paraName = Just " "}
    where
    onClickedHandler widget coreRef = (\ event -> do 
        core <- readIORef coreRef
        case core of 
            Nothing  -> error "Impossible"
            Just (be@(_,_,ext1,_),(leftFrame,_,_,_),(rightFrame,_,_,_),box) -> do
                mbBool <- ext1
                case mbBool of
                    Just bool ->
                            if bool then do
                              containerRemove box rightFrame
                              boxPackStart box leftFrame PackNatural 0 
                              widgetShowAll box 
                            else do     
                              containerRemove box leftFrame  
                              boxPackStart box rightFrame PackNatural 0
                              widgetShowAll box
                    Nothing -> return ()
                return True) 

--
-- | An editor with a subeditor, of which a list of items can be selected 
multisetEditor :: (Show alpha, Default alpha)  => (Editor alpha,Parameters) -> Editor [alpha]
multisetEditor (singleEditor, sParams) parameters = do
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
                    (frameS,injS,extS,notS) <- singleEditor sParams 
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
                    boxPackStart box list PackNatural 0
                    boxPackStart box buttonBox PackNatural 0
                    boxPackEnd box frameS PackGrow 0
                    containerAdd widget box
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) v
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
                    writeIORef coreRef (Just listStore) 
                    injS getDefault
                Just listStore -> do
                    New.listStoreClear listStore
                    mapM_ (New.listStoreAppend listStore) v)
        (do core <- readIORef coreRef
            case core of 
                Nothing -> return Nothing  
                Just listStore -> do
                    v <- listStoreGetValues listStore
                    return (Just v))
        (mkNotifier notifier)
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

--    ,   fieldApplicator     ::  alpha -> alpha -> GhfAction
