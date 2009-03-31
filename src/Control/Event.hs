{-# OPTIONS_GHC -XMultiParamTypeClasses -XFunctionalDependencies #-}
-----------------------------------------------------------------------------
--
-- Module      :  Control.Event
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | A simple event mechanism
--
-------------------------------------------------------------------------------
module Control.Event (
    EventSelector
,   Event(..)
,   EventSource(..)
,   Handlers
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Unique
import Control.Monad

-- | Every event needs a selector, which should identify the type of event
class (Eq delta, Ord delta, Show delta) =>  EventSelector delta

-- | Every event needs to know its selector and its source
class EventSelector delta => Event beta delta | beta -> delta, delta -> beta where
    getSelector ::  beta -> delta

-- | This shows the implementation of the event mechnism
type Handlers beta gamma delta = Map delta [(Unique, beta  -> gamma beta)]

-- | Everything which is an event source needs this
class (Monad gamma, Event beta delta) => EventSource alpha beta gamma delta
        | alpha -> beta, alpha -> gamma where
    getHandlers     ::  alpha -> gamma (Handlers beta gamma delta)
    setHandlers     ::  alpha -> Handlers beta gamma delta -> gamma ()
    myUnique        ::  alpha -> gamma (Unique)

    -- | Reimplement this in instances to make triggering of events possible
    canTriggerEvent :: alpha -> delta -> Bool
    canTriggerEvent _ _             =   False

    -- | Returns the event, so that you may get values back from an event
    triggerEvent ::  alpha -> beta -> gamma beta
    triggerEvent o e    =
        if canTriggerEvent o (getSelector e)
            then do
                handlerMap  <-  getHandlers o
                let selector    =   getSelector e
                case selector `Map.lookup` handlerMap of
                    Nothing     ->  return e
                    Just l      ->  foldM (\e (_,ah) -> ah e) e (reverse l)
            else error $ "Can't trigger event " ++ show (getSelector e)

    -- | use Left to register and Right to unregister
    -- returns Unique if registration was successfull, else Nothing
    registerEvent   ::  alpha -> delta
        -> Either (beta -> gamma beta) Unique -> gamma (Maybe Unique)
    registerEvent o e (Left handler) =
        if canTriggerEvent o e
            then do
                handlerMap  <-  getHandlers o
                unique      <-  myUnique o
                let newHandlers =   case e `Map.lookup` handlerMap of
                                        Nothing -> Map.insert e [(unique,handler)] handlerMap
                                        Just l  -> Map.insert e ((unique,handler):l) handlerMap
                setHandlers o newHandlers
                return (Just unique)
            else return Nothing
    registerEvent o e (Right unique) =
        if canTriggerEvent o e
            then do
                handlerMap  <-  getHandlers o
                let newHandlers =   case e `Map.lookup` handlerMap of
                                        Nothing -> handlerMap
                                        Just l -> let newList = filter (\ (mu,_) -> mu /= unique) l
                                                  in  Map.insert e newList handlerMap
                setHandlers o newHandlers
                return (Just unique)
            else return Nothing

