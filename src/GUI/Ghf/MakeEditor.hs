-----------------------------------------------------------------------------
--
-- Module      :  GUI.Ghf.MakeEditor
-- Copyright   :  (c) Juergen Nicklisch-Franken (aka Jutaro)
-- License     :  GNU-GPL
--
-- Maintainer  :  Juergen Nicklisch-Franken <jnf at arcor.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- | Module for making editors out of descriptions
--
-----------------------------------------------------------------------------------

module GUI.Ghf.MakeEditor (

    FieldDescription(..)
,   mkField
,   emptyNotifier
,   EventSelector(..)
,   extractAndValidate
,   mkEditor

,   declareEvent
,   activateEvent
,   propagateEvent
,   mkNotifier

) where

import Graphics.UI.Gtk hiding (Event)
import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Unique

import GUI.Ghf.Parameters
import GUI.Ghf.EditorBasics

--
-- | A constructor type for a field desciption
--
type MkFieldDescription alpha beta =
    Parameters ->
    (Getter alpha beta) ->
    (Setter alpha beta) ->
    (Editor beta) ->
    FieldDescription alpha

--
-- | A type to describe a field of a record, which can be edited
-- | alpha is the type of the individual field of the record
data FieldDescription alpha =  FD {
        parameters  ::  Parameters
    ,   fieldEditor ::  alpha -> IO (Widget, Injector alpha , alpha -> Extractor alpha , Notifier)
    }

-- ------------------------------------------------------------
-- * Implementation of editing
-- ------------------------------------------------------------

--
-- | Function to construct a field description
--
mkField :: Eq beta => MkFieldDescription alpha beta
mkField parameters getter setter editor =
    FD parameters
        (\ dat -> do
            (widget, inj,ext,noti) <- editor parameters
            inj (getter dat)
            noti FocusOut (Left (\e -> do
                v <- ext
                case v of
                    Just _ -> do
                        return False
                    Nothing -> do
                        putStrLn "Validation Failure"
                        let message = case getParameterPrim paraName parameters of
                                        Just s -> "in field " ++ s
                                        Nothing -> "in unnamed field"
                        dia <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                            ("Validation Failure " ++ message)
                        dialogRun dia
                        widgetDestroy dia
                        return False))
            return (widget,
                    (\a -> inj (getter a)),
                    (\a -> do
                        b <- ext
                        case b of
                            Just b -> return (Just (setter b a))
                            Nothing -> return Nothing),
                    noti))

-- | Function to construct an editor
--
mkEditor :: (Container -> Injector alpha) -> Extractor alpha -> Notifier -> Editor alpha
mkEditor injectorC extractor notifier parameters = do
    let (xalign, yalign, xscale, yscale) = getParameter paraOuterAlignment parameters
    outerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter paraOuterPadding parameters
    alignmentSetPadding outerAlig paddingTop paddingBottom paddingLeft paddingRight
    frame   <-  frameNew
    frameSetShadowType frame (getParameter paraShadow parameters)
    case getParameter paraName parameters of
        "" -> return ()
        str -> frameSetLabel frame str
    containerAdd outerAlig frame
    let (xalign, yalign, xscale, yscale) =  getParameter paraInnerAlignment parameters
    innerAlig <- alignmentNew xalign yalign xscale yscale
    let (paddingTop, paddingBottom, paddingLeft, paddingRight) = getParameter paraInnerPadding parameters
    alignmentSetPadding innerAlig paddingTop paddingBottom paddingLeft paddingRight
    containerAdd frame innerAlig
    let (x,y) = getParameter paraMinSize parameters
    widgetSetSizeRequest outerAlig x y
    return ((castToWidget) outerAlig, injectorC (castToContainer innerAlig), extractor, notifier)

-- | Convenience method to validate and extract fields
--
extractAndValidate :: alpha -> [alpha -> Extractor alpha] -> [String] -> IO (Maybe alpha)
extractAndValidate val getExts fieldNames = do
    (newVal,errors) <- foldM (\ (val,errs) (ext,fn) -> do
        extVal <- ext val
        case extVal of
            Just nval -> return (nval,errs)
            Nothing -> return (val, (' ' : fn) : errs))
                (val,[]) (zip getExts fieldNames)
    if null errors
        then return (Just newVal)
        else do
            md <- messageDialogNew Nothing [] MessageWarning ButtonsClose
                        $ "The following fields have invalid values." ++
                            concat (reverse errors)
            dialogRun md
            widgetDestroy md
            return Nothing

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
declareEvent :: EventSelector -> RegFunc -> NotifierSt -> IO()
declareEvent eventSel regFunc notifierState = do
    noti <- readIORef notifierState
    case Map.lookup eventSel noti of
        Nothing -> do
             let noti2 = Map.insert eventSel (Nothing, regFunc, Nothing,[]) noti
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
                    Nothing -> return False
                    Just (_,_,_,[]) -> return False
                    Just (_,_,_,handlers2) -> do
                        boolList <- mapM (\f -> f e) (map snd handlers2)
                        return (foldl (&&) True boolList))
            let noti2 = Map.insert eventSel (Just widget,registerFunc,Just cid,handlers) noti
            writeIORef notifierState noti2
        Just _ -> error $"editor has already been activated " ++ show eventSel

--
-- | Propagate the event with the selector from notifier to notifierst
--
propagateEvent :: EventSelector -> Notifier -> NotifierSt -> IO()
propagateEvent eventSel notiFrom notifierState = do
    noti <- readIORef notifierState
    case Map.lookup eventSel noti of
        Nothing -> error $"can't propagte event which is not activated " ++ show eventSel
        Just (mbWidget,registerFunc,Nothing,handlers) -> do
            cid <- notiFrom eventSel (Left (\ e -> do
                noti <- readIORef notifierState
                case Map.lookup eventSel noti of
                    Nothing -> return False
                    Just (_,_,_,[]) -> return False
                    Just (_,_,_,handlers2) -> do
                        boolList <- mapM (\f -> f e) (map snd handlers2)
                        return (foldl (&&) True boolList)))
            let noti2 = Map.insert eventSel (mbWidget,registerFunc,Nothing,handlers) noti
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
                                    (mbWidget,registerFunc,mbUnique,handlers++[(uni,handler)]) noti
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




