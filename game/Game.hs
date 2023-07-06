{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Game where

import           Apecs.Physics
import           Apecs.Physics.Gloss
import           Control.Monad       ()

makeWorld "World" [''Physics, ''Camera]

initialize :: SystemT World IO Entity
initialize = do
  set global (Camera (V2 0 1) 60, earthGravity)
  lineBody <- newEntity (StaticBody, Angle (-pi / 20))
  _ <- newEntity (Shape lineBody (hLine 6), Elasticity 0.9)
  ball <- newEntity (DynamicBody, Position (V2 0 3))
  newEntity (Shape ball (cCircle 0.5), Density 1, Elasticity 0.9)

disp :: Display
disp = InWindow "Hello World" (640, 640) (10, 10)

main :: IO ()
main = initWorld >>= runSystem (initialize >> simulate disp)
