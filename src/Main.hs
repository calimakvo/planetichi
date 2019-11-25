module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Juicy

data PlanetPos = PlanetPos {
    x :: Float
  , y :: Float
  , vx :: Float
  , vy :: Float
  , img :: Picture
} deriving(Show, Eq)

mag :: Float
mag = 150

-- Write/s
wps :: Int
wps = 50

midTime :: Float
midTime = (1 / fromIntegral wps :: Float) / 2

initPlanetPos :: IO PlanetPos
initPlanetPos = do
  Just img <- loadJuicy "ichiban.jpg" -- ほんとはパターンマッチしろ
  return $ PlanetPos 0.85 0 0 1.63 img

drawPlanet :: PlanetPos -> Picture
drawPlanet p = translate ((x p) * mag) ((y p) * mag) $ (scale 0.4 0.4 (img p))

movePlanet :: ViewPort -> Float -> PlanetPos -> PlanetPos
movePlanet view t p =
    let r = sqrt $ (x p) ^ 2 + (y p) ^ 2
        ax = -(x p) / r ^ 3      -- x方向加速度
        ay = -(y p) / r ^ 3      -- y方向加速度
        dvx = ax * (t + midTime) -- 時刻tでのx方向速度を中間の速度から算出
        dvy = ay * (t + midTime) -- 時刻tでのy方向速度を中間の速度から算出
        nvx = (vx p) + dvx       -- 新しい速度を時刻tの速度から算出
        nvy = (vy p) + dvy       --
        nx = (x p) + nvx * t     -- 新しい位置を速度から算出
        ny = (y p) + nvy * t     --
    in PlanetPos nx ny nvx nvy (img p)

window :: Display
window = InWindow "Planet Ichiban Shiovori" (800, 640) (100, 100)

main :: IO()
main = do
    ps <- initPlanetPos
    simulate window white wps ps drawPlanet movePlanet
