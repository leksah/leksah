{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | jsaddle-terminal demo: Breakout.
--
-- The whole game — state, physics, collisions — runs in Haskell (reflex);
-- only the rendered DOM crosses the tunnel.  It is deliberately light on the
-- wire: a 30 fps tick moves the ball (one element's style per frame), the
-- paddle updates only while an arrow is held, and a brick's @div@ is removed
-- (via 'listWithKey') the frame it's hit.  Control is the keyboard (← →, or
-- A/D; Space launches / restarts) — arrow keys carry no modifier, so the
-- tunnel's key-forwarding leaves them in the app rather than sending them to
-- leksah's global shortcuts.
module Main (main) where

import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Reflex.Dom.Core

import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.EventM (event, onSync)
import qualified GHCJS.DOM.EventM as EventM (preventDefault)
import GHCJS.DOM.GlobalEventHandlers (keyDown, keyUp)
import GHCJS.DOM.KeyboardEvent (getKey)

import qualified Language.Javascript.JSaddle.Terminal as JT

main :: IO ()
main = JT.run (mainWidget game)

-- Board / entity geometry (logical px).
boardW, boardH, ballR, paddleW, paddleH, paddleY :: Double
boardW = 560; boardH = 400; ballR = 7
paddleW = 90; paddleH = 12; paddleY = boardH - 30

cols, rows :: Int
cols = 9; rows = 5

marginX, marginTop, gap, brickH, brickW, paddleSpeed :: Double
marginX = 12; marginTop = 40; gap = 6; brickH = 20
brickW = (boardW - 2*marginX - fromIntegral (cols-1) * gap) / fromIntegral cols
paddleSpeed = 13

data Phase = Ready | Playing | Over Bool   -- Over True = won, False = lost
  deriving Eq

data St = St
  { cx, cy, vx, vy :: Double   -- ball centre + velocity
  , pcx            :: Double    -- paddle centre x
  , bricks         :: Map (Int, Int) ()
  , score, lives   :: Int
  , phase          :: Phase
  }

allBricks :: Map (Int, Int) ()
allBricks = M.fromList [ ((r, c), ()) | r <- [0 .. rows-1], c <- [0 .. cols-1] ]

initSt :: St
initSt = St
  { cx = boardW/2, cy = paddleY - ballR, vx = 0, vy = 0
  , pcx = boardW/2, bricks = allBricks, score = 0, lives = 3, phase = Ready }

data Ev = Tick (Set Text) | Launch

clampD :: Double -> Double -> Double -> Double
clampD lo hi = max lo . min hi

brickRect :: (Int, Int) -> (Double, Double, Double, Double)
brickRect (r, c) =
  ( marginX + fromIntegral c * (brickW + gap)
  , marginTop + fromIntegral r * (brickH + gap)
  , brickW, brickH )

leftHeld, rightHeld :: Set Text -> Bool
leftHeld  h = any (`S.member` h) ["ArrowLeft",  "a", "A"]
rightHeld h = any (`S.member` h) ["ArrowRight", "d", "D"]

step :: Ev -> St -> St
step Launch st = case phase st of
  Ready  -> st { phase = Playing, vx = 3, vy = -6
               , cx = pcx st, cy = paddleY - ballR }
  Over _ -> initSt
  Playing -> st
step (Tick held) st =
  let dir  = (if rightHeld held then 1 else 0) - (if leftHeld held then 1 else 0)
      pcx' = clampD (paddleW/2) (boardW - paddleW/2) (pcx st + dir * paddleSpeed)
      st'  = st { pcx = pcx' }
  in case phase st of
       Playing -> physics st'
       Ready   -> st' { cx = pcx', cy = paddleY - ballR }
       Over _  -> st'

physics :: St -> St
physics st =
  let r = ballR
      oldcy = cy st
      cx1 = cx st + vx st
      cy1 = cy st + vy st
      -- side + top walls
      (cx2, vx2)
        | cx1 - r < 0       = (r,          abs (vx st))
        | cx1 + r > boardW  = (boardW - r, negate (abs (vx st)))
        | otherwise         = (cx1,        vx st)
      (cy2, vy2)
        | cy1 - r < 0       = (r,          abs (vy st))
        | otherwise         = (cy1,        vy st)
      -- paddle
      pl = pcx st - paddleW/2
      pr = pcx st + paddleW/2
      hitPaddle = vy2 > 0 && cy2 + r >= paddleY && cy2 - r <= paddleY + paddleH
                          && cx2 >= pl && cx2 <= pr
      (cy3, vy3, vx3)
        | hitPaddle = let rel = clampD (-1) 1 ((cx2 - pcx st) / (paddleW/2))
                      in (paddleY - r, negate (abs vy2), rel * 7)
        | otherwise = (cy2, vy2, vx2)
      -- first brick the ball now overlaps
      hit = listToMaybe
        [ (k, rect) | (k, _) <- M.toList (bricks st)
                    , let rect = brickRect k, overlaps cx2 cy3 r rect ]
      (bricks', score', vx4, vy4) = case hit of
        Nothing -> (bricks st, score st, vx3, vy3)
        Just (k, (_, by, _, bh)) ->
          -- vertical hit if the ball was above/below the brick last frame
          let vertical = oldcy <= by || oldcy >= by + bh
          in ( M.delete k (bricks st), score st + 10
             , if vertical then vx3 else negate vx3
             , if vertical then negate vy3 else vy3 )
      st1 = st { cx = cx2, cy = cy3, vx = vx4, vy = vy4
               , bricks = bricks', score = score' }
  in if M.null bricks'          then st1 { phase = Over True }
     else if cy3 - r > boardH   then loseLife st1
     else st1

loseLife :: St -> St
loseLife st
  | lives st <= 1 = st { lives = 0, phase = Over False }
  | otherwise     = st { lives = lives st - 1, phase = Ready
                       , cx = pcx st, cy = paddleY - ballR, vx = 0, vy = 0 }

-- circle (cx,cy,r) vs axis-aligned rect (x,y,w,h)
overlaps :: Double -> Double -> Double -> (Double, Double, Double, Double) -> Bool
overlaps cx' cy' r (x, y, w, h) =
  let nx = clampD x (x+w) cx'
      ny = clampD y (y+h) cy'
      dx = cx' - nx; dy = cy' - ny
  in dx*dx + dy*dy <= r*r

game :: forall t m. MonadWidget t m => m ()
game = mdo
  doc <- currentDocumentUnchecked
  let gameKeys = ["ArrowLeft","ArrowRight","a","A","d","D"," "]
      keyH = do
        ke <- event
        k  <- getKey ke
        when (k `elem` gameKeys) EventM.preventDefault  -- don't scroll the frame
        return (k :: Text)
  keydownE <- wrapDomEvent doc (`onSync` keyDown) keyH
  keyupE   <- wrapDomEvent doc (`onSync` keyUp)   keyH
  heldD <- foldDyn ($) S.empty $ leftmost
    [ S.insert <$> ffilter (`elem` ["ArrowLeft","ArrowRight","a","A","d","D"]) keydownE
    , S.delete <$> keyupE ]
  tickE <- tickLossyFromPostBuildTime (1/30)
  let launchE = leftmost [ () <$ ffilter (== " ") keydownE, () <$ clickE ]
      evE = leftmost
        [ attachWith (\h _ -> Tick h) (current heldD) tickE
        , Launch <$ launchE ]
  stD <- foldDyn step initSt evE

  clickE <- elAttr "div" ("style" =: wrapStyle) $ do
    elAttr "div" ("style" =: hudStyle) $ dynText (hudText <$> stD)
    (boardEl, _) <- elAttr' "div" ("style" =: boardStyle) $ do
      _ <- listWithKey (bricks <$> stD) $ \k _ ->
             elAttr "div" ("style" =: brickStyle k) blank
      elDynAttr "div" (ballAttr   <$> stD) blank
      elDynAttr "div" (paddleAttr <$> stD) blank
      elAttr "div" ("style" =: overlayStyle) $ dynText (overlayText <$> stD)
    pure (domEvent Click boardEl)
  pure ()

-- Styles / text -------------------------------------------------------------

px :: Double -> Text
px d = T.pack (show (round d :: Int)) <> "px"

wrapStyle, hudStyle, boardStyle, overlayStyle :: Text
wrapStyle = "text-align:center;font-family:-apple-system,BlinkMacSystemFont,\
            \Segoe UI,sans-serif;color:#ddd;padding-top:10px;user-select:none"
hudStyle  = "font-size:14px;margin-bottom:8px;letter-spacing:0.3px"
boardStyle = "position:relative;margin:0 auto;overflow:hidden;\
             \background:#0b0b0f;border:2px solid #333;border-radius:4px;"
             <> "width:" <> px boardW <> ";height:" <> px boardH
overlayStyle = "position:absolute;inset:0;display:flex;align-items:center;\
               \justify-content:center;font-size:20px;color:#fff;\
               \text-shadow:0 1px 3px #000;pointer-events:none;text-align:center"

hudText :: St -> Text
hudText st = "Score " <> T.pack (show (score st))
          <> "   \8226   Lives " <> T.pack (show (lives st))
          <> "   \8226   \8592 \8594 or A/D to move"

overlayText :: St -> Text
overlayText st = case phase st of
  Playing  -> ""
  Ready    -> "Press Space (or click) to launch"
  Over True  -> "You win!  \8212  Space to play again"
  Over False -> "Game over  \8212  Space to play again"

brickStyle :: (Int, Int) -> Text
brickStyle k@(r, _) =
  let (x, y, w, h) = brickRect k
  in "position:absolute;pointer-events:none;border-radius:3px;left:" <> px x
     <> ";top:" <> px y <> ";width:" <> px w <> ";height:" <> px h
     <> ";background:" <> rowColor r

rowColor :: Int -> Text
rowColor r = ["#e6432f","#e6822f","#e6d02f","#57c94b","#4aa3ff"] !! (r `mod` 5)

ballAttr :: St -> Map Text Text
ballAttr st = "style" =:
  ( "position:absolute;pointer-events:none;border-radius:50%;background:#f5f5f5;"
    <> "left:" <> px (cx st - ballR) <> ";top:" <> px (cy st - ballR)
    <> ";width:" <> px (2*ballR) <> ";height:" <> px (2*ballR) )

paddleAttr :: St -> Map Text Text
paddleAttr st = "style" =:
  ( "position:absolute;pointer-events:none;border-radius:6px;background:#5ab0ff;"
    <> "left:" <> px (pcx st - paddleW/2) <> ";top:" <> px paddleY
    <> ";width:" <> px paddleW <> ";height:" <> px paddleH )
