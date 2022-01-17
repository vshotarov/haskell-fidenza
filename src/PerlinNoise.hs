{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module PerlinNoise (noise2d) where

import System.Random

type Vec2 = (Float,Float)

toVec2 :: (Int,Int) -> Vec2
toVec2 (x,y) = (fromIntegral x, fromIntegral y)

subVec2 :: Vec2 -> Vec2 -> Vec2
subVec2 (x1,y1) (x2,y2) = (x1-x2,y1-y2)

dotVec2 :: Vec2 -> Vec2 -> Float
dotVec2 (x1,y1) (x2,y2) = x1*x2 + y1*y2

getGrad :: Int -> StdGen -> (Int,Int) -> Vec2
getGrad w gen (x,y) = grads !! (y * w + x)
  where gradOptions = [(gx,gy) | gx <- [-1,1], gy <- [-1,1]]
        grads = map
                  (gradOptions !!)
                  $ randomRs (0, 3) gen

fade :: Float -> Float
fade t = t*t*t*(t*(t*6-15)+10)

lerp :: Float -> Float -> Float -> Float
lerp a b w = a + (b-a) * w

noise2d :: (Float,Float) -> Int -> StdGen -> Float
noise2d (x,y) w gen = raw * 0.5 + 0.5
  where (binX,binY) = (floor x, floor y)
        (xf,yf) = (x - fromIntegral binX, y - fromIntegral binY)
        verts = [(nx,ny) | nx <- [(binX),(binX+1)],
                           ny <- [(binY),(binY+1)]]
        grads = map (getGrad w gen) verts
        dists = map (subVec2 (x,y) . toVec2) verts
        [dpTL,dpBL,dpTR,dpBR] = zipWith dotVec2 grads dists
        (u,v) = (fade xf, fade yf)
        raw = lerp (lerp dpTL dpTR u)
                   (lerp dpBL dpBR u) v
