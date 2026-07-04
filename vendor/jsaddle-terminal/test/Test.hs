{-# LANGUAGE OverloadedStrings #-}
-- | Protocol scanner tests: frames must survive arbitrary re-chunking and
-- interleaving with arbitrary passthrough bytes (tmux splits %output at
-- arbitrary byte boundaries).  Uses a small deterministic LCG so there are
-- no extra test dependencies.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Word (Word8)
import System.Exit (exitFailure)

import Language.Javascript.JSaddle.Terminal.Protocol

-- deterministic pseudo-random stream
lcg :: Int -> [Int]
lcg seed = tail (iterate step seed)
  where step x = (x * 1103515245 + 12345) `mod` 2147483648

frameOf :: Int -> Frame
frameOf n = Frame ty payload
  where
    tys = [Hello, Batch, SyncReply, Log, Bye, Ack, Results, Sync, Close]
    ty = tys !! (n `mod` length tys)
    payload = BS.pack (map fromIntegral (take (n `mod` 200) (lcg n)))

garbageOf :: Int -> ByteString
garbageOf n = BS.pack (map (fromIntegral :: Int -> Word8)
                           (take (n `mod` 97) (lcg (n + 7))))

-- split a bytestring at pseudo-random boundaries
chunksOfRandom :: Int -> ByteString -> [ByteString]
chunksOfRandom seed = go (lcg seed)
  where
    go _ bs | BS.null bs = []
    go (r:rs) bs = let n = 1 + (r `mod` 13)
                   in BS.take n bs : go rs (BS.drop n bs)
    go [] _ = error "impossible"

check :: String -> Bool -> IO ()
check name ok = unless ok $ putStrLn ("FAIL: " <> name) >> exitFailure

main :: IO ()
main = do
  -- OSC: interleave garbage and frames (stream ends with a frame, so the
  -- scanner holds nothing back at the end), re-chunk, expect exact recovery.
  forM_ [1 .. 50 :: Int] $ \i -> do
    let fs = map frameOf [i * 10 + k | k <- [0 .. 5]]
        gs = map garbageOf [i * 20 + k | k <- [0 .. 5]]
        stream = mconcat (zipWith (\g f -> g <> encodeOsc f) gs fs)
        pieces = chunksOfRandom i stream
        step (accP, accF, st) c =
          let (p, f, st') = scanOsc st c in (accP <> p, accF <> f, st')
        (passOut, frames, _st) = foldl' step (mempty, [], emptyOscScan) pieces
    check ("osc frames " <> show i) (frames == fs)
    check ("osc passthrough " <> show i) (passOut == mconcat gs)
  -- RS: garbage dropped, frames recovered.
  forM_ [1 .. 50 :: Int] $ \i -> do
    let fs = map frameOf [i * 11 + k | k <- [0 .. 5]]
        -- RS-stream garbage must not contain RS itself (a bare RS starts a
        -- frame candidate that swallows bytes to the next LF; stray pane
        -- input containing RS is not a supported case).
        gs = map (BS.filter (/= 0x1e) . garbageOf)
                 [i * 21 + k | k <- [0 .. 5]]
        stream = mconcat (zipWith (\g f -> g <> encodeRs f) gs fs)
        pieces = chunksOfRandom (i + 3) stream
        step (accF, st) c = let (f, st') = scanRs st c in (accF <> f, st')
        (frames, _) = foldl' step ([], emptyRsScan) pieces
    check ("rs frames " <> show i) (frames == fs)
  putStrLn "All protocol tests passed."
