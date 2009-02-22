{-# OPTIONS_GHC
    -XExistentialQuantification
    -XDeriveDataTypeable
    -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  Data.Binary.Shared
-- Copyright   :  2007-2009 Jürgen Nicklisch-Franken
-- License     :  GPL
--
-- Maintainer  :  Jutaro <jutaro@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- | Binary serializing with sharing
--
-----------------------------------------------------------------------------

module Data.Binary.Shared (
    BinaryShared(..)
,   encodeFileSer
,   encodeSer
,   decodeSer
) where

import Data.Typeable (typeRepKey,cast,Typeable(..))
import qualified Control.Monad.State as St  (StateT(..),get,put)
import Data.Map (Map(..))
import qualified Data.Map as Map  (empty,fromDistinctAscList,toAscList,Map(..),insert,lookup)
import Data.IntMap (IntMap(..))
import qualified Data.IntMap as IMap  (empty,IntMap(..),insert,lookup)
import qualified Data.Binary as Bin (getWord8,putWord8,Get(..),Binary(..))
import Data.Binary.Put (runPut,PutM(..),putWord64be)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (lift)
import MyMissing (forceJust)
import Control.Monad (liftM2,replicateM,liftM)
import qualified Data.Set as Set  (fromDistinctAscList,toAscList,Set(..))
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString(..))
import Control.Monad.State.Lazy (evalStateT)
import Data.Binary.Get (runGet,getWord64be)

-- | A class for storing Binary instances with shared nodes.
-- Cycles are not supported, cause put and get is a one path process.

class (Typeable alpha, Ord alpha, Eq alpha, Show alpha) => BinaryShared alpha  where
    -- | Encode a value in the Put monad.
    put :: alpha  -> PutShared

    putShared :: (alpha -> PutShared) -> alpha -> PutShared
    putShared fput v = do
        (dict, unique) <- St.get
        case (ObjC v) `Map.lookup` dict of
            Just i  -> lift (Bin.putWord8 0 >> putWord64be (fromIntegral i))
            Nothing -> do
                St.put (dict,unique + 1)
                lift (Bin.putWord8 1)
                lift (putWord64be (fromIntegral unique))
                fput v
                (dict2, unique2) <- St.get
                let newDict = Map.insert (ObjC v) unique dict2
                St.put (newDict,unique2)

    -- | Decode a value in the Get monad
    get :: GetShared alpha

    getShared :: GetShared alpha -> GetShared alpha
    getShared f = do
        dict <- St.get
        w <- lift Bin.getWord8
        case w of
            0 -> do
                i   <- lift (liftM fromIntegral (getWord64be))
                case  IMap.lookup i dict of
                    Just (ObjC obj) -> return (forceJust (cast obj)
                                            "Shared>>getShared: Cast failed")
                    Nothing -> error $ "Shared>>getShared : Dont find in Map " ++ show i
            1 -> do
                i   <- lift (liftM fromIntegral (getWord64be))
                obj <- f
                dict2 <- St.get
                St.put (IMap.insert i (ObjC obj) dict2)
                return obj
            _ -> error $ "Shared>>getShared : Encoding error"


-- * How to call this

encodeSer :: BinaryShared a => a -> L.ByteString
encodeSer v = runPut (evalStateT (put v) (Map.empty,0))

encodeFileSer :: BinaryShared a => FilePath -> a -> IO ()
encodeFileSer f v = L.writeFile f (encodeSer v)

decodeSer :: BinaryShared alpha  => L.ByteString -> alpha
decodeSer =  runGet (evalStateT get IMap.empty)

-- * The types needed internally

data Object = forall alpha. (Typeable alpha, Ord alpha, Eq alpha, Show alpha) => ObjC {unObj :: alpha}

instance Eq Object where
    (ObjC a) == (ObjC b) = if typeOf a /= typeOf b
                                then False
                                else (Just a) == cast b

instance Ord Object where
    compare (ObjC a) (ObjC b) = if typeOf a /= typeOf b
                                then compare ((unsafePerformIO . typeRepKey . typeOf) a)
                                                ((unsafePerformIO . typeRepKey . typeOf) b)
                                else compare (Just a) (cast b)

type PutShared = St.StateT (Map Object Int, Int) PutM ()
type GetShared = St.StateT (IntMap Object) Bin.Get

-----------
-- * Some standard instances, but very incomplete

instance BinaryShared a => BinaryShared [a] where
    put    = putShared (\l -> lift (Bin.put (length l)) >> mapM_ put l)
    get    = getShared (do
                n <- lift (Bin.get :: Bin.Get Int)
                replicateM n get)

instance (BinaryShared a) => BinaryShared (Maybe a) where
    put Nothing  = lift (Bin.putWord8 0)
    put (Just x) = lift (Bin.putWord8 1) >> put x
    get = do
        w <- lift (Bin.getWord8)
        case w of
            0 -> return Nothing
            _ -> liftM Just get

instance (BinaryShared a, BinaryShared b) => BinaryShared (a,b) where
    put (a,b)           = put a >> put b
    get                 = liftM2 (,) get get

instance BinaryShared a => BinaryShared (Set.Set a) where
    put s = put (Set.toAscList s)
    get   = liftM Set.fromDistinctAscList get

instance (BinaryShared k, BinaryShared e) => BinaryShared (Map.Map k e) where
    put m = put (Map.toAscList m)
    get   = liftM Map.fromDistinctAscList get

instance BinaryShared Char where
    put = lift . Bin.put
    get = lift Bin.get

instance BinaryShared Int where
    put = lift . Bin.put
    get = lift Bin.get

instance BinaryShared Integer where
    put = lift . Bin.put
    get = lift Bin.get

instance BinaryShared ByteString where
    put = lift . Bin.put
    get = lift Bin.get

