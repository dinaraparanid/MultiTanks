{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}

module Game where

import Control.Concurrent.Async as Async
import Client
import Field
import GameState
import Apecs.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss (simulate)

fps :: Int
fps = 60

renderGame :: GameState -> Picture
renderGame (GameState []) = waitPlayer2Field
renderGame (GameState [_]) = waitPlayer2Field
renderGame (GameState _) = gameField

updateGame :: ViewPort -> Float -> GameState -> GameState
updateGame _ _ state = state

launchGame :: IO ()
launchGame = simulate mainWindow white fps initialGameState renderGame updateGame

main :: IO ()
main = do
    _ <- Async.concurrently launchClientHandler launchGame
    return ()
