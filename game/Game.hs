{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game where

import Apecs
import Apecs.Gloss
import Graphics.Gloss
import Apecs.Physics
import Control.Monad       ()
import Client

makeWorld "World" [''Physics, ''Camera]

------------------------borders----------------------------
mBorder, border1, border2, border3, border4, border5 :: Picture
mBorder = color red $ Polygon [(3.1,2.1),(-3.1,2.1),(-3.1,-2.1),(3.1, -2.1)]
border1 = Polygon [(-1.05, 0.05), (-1.95, 0.05), (-1.95, 0.95), (-1.05, 0.95)] 
            <> Polygon [(-0.95, -0.05), (-2.05, -0.05), (-2.05, 1.05), (-0.95, 1.05)]
border2 = Polygon [(0.05, 0), (0.05, 2.1), (-0.05, 2.1), (-0.05, 0)]
border3 = Polygon [(-1.95, -1), (-2.05, -1), (-2.05, -2.1), (-1.95, -2.1)]
border4 = Polygon [(0, -1.05), (0, -2.3), (-1, -2.3), (-1, -1.05)] 
            <> Polygon [(0.05, -1), (0.05, -2.1), (-1.05, -2.1), (-1.05, -1)]
border5 = Polygon [(0.95, 0.05), (0.95, -1.05), (2.05, -1.05), (2.05, 0.05)] <>
            Polygon [(1, 0), (1, -1), (2, -1), (2, 0)]

-----------------------light tiles-------------------------
tile1, tile4, tile5, tile6, tile7 :: Picture
tile1 = Polygon [(-2, 2), (-3, 2), (-3, 0), (-2, 0)]
tile4 = Polygon [(-1, -1), (-3, -1), (-3, -2), (-1, -2)]
tile5 = Polygon [(1, 1), (1, 2), (0, 2), (0, 1)]
tile6 = Polygon [(1, 0), (-1, 0), (-1, -1), (1, -1)]
tile7 = Polygon [(2, -1), (2, -2), (0, -2), (0, -1)]

-----------------------dark tiles--------------------------
tile2, tile3, tile8, tile9, tile10 :: Picture
tile2 = Polygon [(0, 0), (0, 2), (-2, 2), (-2, 0)]
tile3 = Polygon [(-1, 0), (-3, 0), (-3, -1), (-1, -1)]
tile8 = Polygon [(1, 0), (1, 1), (0, 1), (0, 0)]
tile9 = Polygon [(3, 2), (1, 2), (1, 0), (3, 0)]
tile10 = Polygon [(3, 0), (3, -2), (2, -2), (2, 0)]

window :: Display
window = InWindow "Hello World" (640, 640) (10, 10)

drawSampleImage :: IO ()
drawSampleImage = display window white mBorder




main :: IO ()
main = do
  Client.sendConnect
  Client.sendConnect
  Client.sendChangePosition 2 (2, 2)
  Client.sendShoot (2, 2) (4, 4)
