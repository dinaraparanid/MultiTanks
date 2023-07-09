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

module Field where

import Apecs
import Apecs.Gloss
import Apecs.Physics
import Control.Monad       ()

makeWorld "World" [''Physics, ''Camera]

------------------------borders----------------------------
mBorder :: Picture
mBorder = Polygon [(310, 210),(-310, 210),(-310, -210),(310, -210)]

border1 :: Picture
border1 = pictures [
  color (light black) $ Polygon [(-95, - 5), (-205, -5), (-205, 105), (-95, 105)]
  , color white $ Polygon [(-105, 5), (-195, 5), (-195, 95), (-105, 95)]
  ]

border2 :: Picture
border2 = Polygon [(5, 0), (5, 210), (-5, 210), (-5, 0)]

border3 :: Picture
border3 = Polygon [(-195, -100), (-205, -100), (-205, -210), (-195, -210)]

border4 :: Picture
border4 = pictures [
  color (light black) $ Polygon [(5, -95), (5, -210), (-105, -210), (-105, -95)]
  , color white $ Polygon [(-5, -105), (-5, -230), (-95, -230), (-95, -105)]
  ]

border5 :: Picture
border5 = pictures [
  color (light black) $ Polygon [(95, 5), (95, -105), (205, -105), (205, 5)]
  , color white $ Polygon [(105, -5), (105, -95), (195, -95), (195, -5)]
  ]

border6 :: Picture
border6 = Polygon [(200, -95), (200, -105), (310, -105), (310, -95)]

-----------------------light tiles-------------------------
tile1, tile4, tile5, tile6, tile7 :: Picture
tile1 = Polygon [(-200, 200), (-300, 200), (-300, 0), (-200, 0)]
tile4 = pictures [
  Polygon [(-100, -100), (-300, -100), (-300, -200), (-100, -200)]
  , translate (-250) (-150) $ color green $ circle 25 -- first player position hint
  ]
tile5 = Polygon [(100, 100), (100, 200), (0, 200), (0, 100)]
tile6 = Polygon [(100, 0), (-100, 0), (-100, -100), (100, -100)]
tile7 = Polygon [(200, -100), (200, -200), (0, -200), (0, -100)]

-----------------------dark tiles--------------------------
tile2, tile3, tile8, tile9, tile10 :: Picture
tile2 = Polygon [(0, 0), (0, 200), (-200, 200), (-200, 0)]
tile3 = Polygon [(-100, 0), (-300, 0), (-300, -100), (-100, -100)]
tile8 = Polygon [(100, 0), (100, 100), (0, 100), (0, 0)]
tile9 = Polygon [(300, 200), (100, 200), (100, 0), (300, 0)]
tile10 = pictures [
  Polygon [(300, 0), (300, -200), (200, -200), (200, 0)]
  , translate 250 (-150) $ color blue $ circle 25 -- second player position hint
  ]

darkColor :: Picture -> Picture
darkColor = color $ light $ light black

lightColor :: Picture -> Picture
lightColor = color $ light $ light $ light black

borderColor :: Picture -> Picture
borderColor =  color $ light black

mainWindow :: Display
mainWindow = InWindow "MultiTanks" (640, 640) (500, 250)

gameField :: Picture
gameField = pictures [borderColor mBorder
                    , darkColor tile2
                    , darkColor tile3
                    , darkColor tile8
                    , darkColor tile9
                    , darkColor tile10
                    , lightColor tile1
                    , lightColor tile4
                    , lightColor tile5
                    , lightColor tile6
                    , lightColor tile7
                    , border1
                    , borderColor border2
                    , borderColor border3
                    , border4
                    , border5
                    , borderColor border6]

waitPlayer2Field :: Picture
waitPlayer2Field = pictures [
    gameField
    , color green $ scale 0.5 0.5 $ translate (-400) 500 $ text "Wait Player 2"
    ]
