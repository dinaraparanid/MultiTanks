{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Field where

import           Apecs
import           Apecs.Gloss
import           Apecs.Physics
import           Control.Monad ()
import           GameState     (Coordinates)

makeWorld "World" [''Physics, ''Camera]

------------------------ Borders ----------------------------

-- | Stores left-down and right-up
--   coordinates of  rectangular zones,
--   occupied by borders
type BorderPolygon = (Coordinates, Coordinates)

-- | Checks wheather position is inside of the border zone
inBorder :: Coordinates -> BorderPolygon -> Bool
inBorder (x, y) ((xd, yd), (xu, yu)) = x >= xd && x <= xu && y >= yd && y <= yu

-- | Main game borders or end of the map
mainBorder :: Picture
mainBorder = Polygon [(310, 210), (-310, 210), (-310, -210), (310, -210)]

-- | Checks if position is outside of the level
beyoundMainBorder :: Coordinates -> Bool
beyoundMainBorder (x, y) = x <= -310 || x >= 310 || y <= -210 || y >= 210

border1 :: Picture
border1 = pictures [
  color (light black) $ Polygon [(-95, -5), (-205, -5), (-205, 105), (-95, 105)]
  , color white $ Polygon [(-105, 5), (-195, 5), (-195, 95), (-105, 95)]
  ]

border1Polygon :: BorderPolygon
border1Polygon = ((-205, -5), (-95, 105))

border2 :: Picture
border2 = Polygon [(5, 0), (5, 210), (-5, 210), (-5, 0)]

border2Polygon :: BorderPolygon
border2Polygon = ((-5, 0), (5, 210))

border3 :: Picture
border3 = Polygon [(-195, -100), (-205, -100), (-205, -210), (-195, -210)]

border3Polygon :: BorderPolygon
border3Polygon = ((-205, -210), (-195, -100))

border4 :: Picture
border4 = pictures [
  color (light black) $ Polygon [(5, -95), (5, -210), (-105, -210), (-105, -95)]
  , color white $ Polygon [(-5, -105), (-5, -230), (-95, -230), (-95, -105)]
  ]

border4Polygon :: BorderPolygon
border4Polygon = ((-105, -210), (5, -95))

border5 :: Picture
border5 = pictures [
  color (light black) $ Polygon [(95, 5), (95, -105), (205, -105), (205, 5)]
  , color white $ Polygon [(105, -5), (105, -95), (195, -95), (195, -5)]
  ]

border5Polygon :: BorderPolygon
border5Polygon = ((95, -105), (205, 5))

border6 :: Picture
border6 = Polygon [(200, -95), (200, -105), (310, -105), (310, -95)]

border6Polygon :: BorderPolygon
border6Polygon = ((200, -105), (310, -95))

borderPolygons :: [BorderPolygon]
borderPolygons = [border1Polygon
                , border2Polygon
                , border3Polygon
                , border4Polygon
                , border5Polygon
                , border6Polygon
                ]

-- | Checks wheather position is legal in terms of borders
beyoundBorders :: Coordinates -> Bool
beyoundBorders crds = beyoundMainBorder crds || any (\brd -> crds `inBorder` brd) borderPolygons

----------------------- Light Tiles -------------------------

tile1, tile4, tile5, tile6, tile7 :: Picture
tile1 = Polygon [(-200, 200), (-300, 200), (-300, 0), (-200, 0)]
tile4 = Polygon [(-100, -100), (-300, -100), (-300, -200), (-100, -200)]
tile5 = Polygon [(100, 100), (100, 200), (0, 200), (0, 100)]
tile6 = Polygon [(100, 0), (-100, 0), (-100, -100), (100, -100)]
tile7 = Polygon [(200, -100), (200, -200), (0, -200), (0, -100)]

----------------------- Dark Tiles --------------------------

tile2, tile3, tile8, tile9, tile10 :: Picture
tile2 = Polygon [(0, 0), (0, 200), (-200, 200), (-200, 0)]
tile3 = Polygon [(-100, 0), (-300, 0), (-300, -100), (-100, -100)]
tile8 = Polygon [(100, 0), (100, 100), (0, 100), (0, 0)]
tile9 = Polygon [(300, 200), (100, 200), (100, 0), (300, 0)]
tile10 = Polygon [(300, 0), (300, -200), (200, -200), (200, 0)]

darkColor :: Picture -> Picture
darkColor = color $ light $ light black

lightColor :: Picture -> Picture
lightColor = color $ light $ light $ light black

borderColor :: Picture -> Picture
borderColor =  color $ light black

----------------------- Fields --------------------------

mainWindow :: Display
mainWindow = InWindow "MultiTanks" (640, 640) (500, 250)

gameField :: Picture
gameField = pictures [borderColor mainBorder
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

player1WonField :: Picture
player1WonField = pictures [
    gameField
    , color green $ scale 0.5 0.5 $ translate (-400) 500 $ text "Player 1 Won!"
    ]

player2WonField :: Picture
player2WonField = pictures [
    gameField
    , color green $ scale 0.5 0.5 $ translate (-400) 500 $ text "Player 2 Won!"
    ]
