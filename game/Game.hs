{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Game where

import           Apecs.Gloss
import           Client
import           Control.Concurrent.Async as Async
import           Control.Concurrent.STM   as STM
import           Data.Maybe               as Maybe
import           Data.Word                (Word8)
import           Field
import           GameState
import           Graphics.Gloss           as Gloss
import           System.IO.Unsafe         (unsafePerformIO)

fps :: Int
fps = 60

firstPlayerPic :: Picture
firstPlayerPic = color green $ circle 25

secondPlayerPic :: Picture
secondPlayerPic = color blue $ circle 25

renderGame :: SystemState -> Picture
renderGame (GameState [], _)                       = waitPlayer2Field
renderGame (GameState [_], _)                      = waitPlayer2Field
renderGame (GameState [(p1X, p1Y), (p2X, p2Y)], _) = pictures [
    gameField
    , translate (fromIntegral p1X) (fromIntegral p1Y) firstPlayerPic
    , translate (fromIntegral p2X) (fromIntegral p2Y) secondPlayerPic
    ]
renderGame _ = blank

wrapP1MovementWithIO ::
  TChan GameState.PlayerRequests
  -> Coordinates
  -> Coordinates
  -> PlayerRequests
  -> IO SystemState
wrapP1MovementWithIO requestChan newP1Crds p2Crds updCrdsRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    return (GameState [newP1Crds, p2Crds], PlayerState (Just 1) $ Just newP1Crds)

wrapP2MovementWithIO ::
  TChan GameState.PlayerRequests
  -> Coordinates
  -> Coordinates
  -> PlayerRequests
  -> IO SystemState
wrapP2MovementWithIO requestChan p1Crds newP2Crds updCrdsRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    return (GameState [p1Crds, newP2Crds], PlayerState (Just 2) $ Just newP2Crds)

handleMovementEvent ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Word8
  -> (Coordinates -> Coordinates)
  -> SystemState
handleMovementEvent requestChan (GameState [p1Crds, p2Crds], playerState) playerInd crdTransf = case playerInd of
    1 -> do
        let newP1Crds = crdTransf p1Crds
        let updCrdsRequest = GameState.UpdatePosition (UpdatePositionData 1 newP1Crds)
        unsafePerformIO $ wrapP1MovementWithIO requestChan newP1Crds p2Crds updCrdsRequest

    2 -> do
        let newP2Crds = crdTransf p2Crds
        let updCrdsRequest = GameState.UpdatePosition (UpdatePositionData 2 newP2Crds)
        unsafePerformIO $ wrapP2MovementWithIO requestChan p1Crds newP2Crds updCrdsRequest

    _ -> (GameState [p1Crds, p2Crds], playerState)

handleMovementEvent _ state _ _ = state

handleGameEvents :: TChan GameState.PlayerRequests -> Event -> SystemState -> SystemState
handleGameEvents requestChan (EventKey (SpecialKey KeyUp) _ _ _) (gameState, PlayerState (Just 1) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 1 (\(x, y) -> (x, y + 5))

handleGameEvents requestChan (EventKey (SpecialKey KeyUp) _ _ _) (gameState, PlayerState (Just 2) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 2 (\(x, y) -> (x, y + 5))

handleGameEvents requestChan (EventKey (SpecialKey KeyDown) _ _ _) (gameState, PlayerState (Just 1) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 1 (\(x, y) -> (x, y - 5))

handleGameEvents requestChan (EventKey (SpecialKey KeyDown) _ _ _) (gameState, PlayerState (Just 2) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 2 (\(x, y) -> (x, y - 5))

handleGameEvents requestChan (EventKey (SpecialKey KeyRight) _ _ _) (gameState, PlayerState (Just 1) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 1 (\(x, y) -> (x + 5, y))

handleGameEvents requestChan (EventKey (SpecialKey KeyRight) _ _ _) (gameState, PlayerState (Just 2) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 2 (\(x, y) -> (x + 5, y))

handleGameEvents requestChan (EventKey (SpecialKey KeyLeft) _ _ _) (gameState, PlayerState (Just 1) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 1 (\(x, y) -> (x - 5, y))

handleGameEvents requestChan (EventKey (SpecialKey KeyLeft) _ _ _) (gameState, PlayerState (Just 2) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just 1) crds) 2 (\(x, y) -> (x - 5, y))

handleGameEvents _ _ state                                            = state

updateGame :: TChan SystemState -> Float -> SystemState -> SystemState
updateGame eventQueue _ state = -- реализация этой штуки слишком ленива, чтобы догадаться использовать обычный readTChan
    Maybe.fromMaybe state $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

launchGame :: TChan SystemState -> TChan GameState.PlayerRequests -> IO ()
launchGame eventQueue requestChan = Gloss.play
  mainWindow
  white
  fps initialSystemState
  renderGame
  (handleGameEvents requestChan)
  (updateGame eventQueue)

main :: IO ()
main = do
    eventQueue <- STM.atomically STM.newTChan
    requestChan <- STM.atomically STM.newTChan
    _ <- Async.concurrently (launchClientHandler eventQueue requestChan) $ launchGame eventQueue requestChan
    return ()
