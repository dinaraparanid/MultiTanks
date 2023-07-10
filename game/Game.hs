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

translatedPlayerPic :: Maybe Coordinates -> Maybe Direction -> Picture -> Picture
translatedPlayerPic (Just (x, y)) (Just GameState.UpDir) pic = translate (fromIntegral x) (fromIntegral y) pic
translatedPlayerPic (Just (x, y)) (Just GameState.DownDir) pic = translate (fromIntegral x) (fromIntegral y) $ rotate 180.0 pic
translatedPlayerPic (Just (x, y)) (Just GameState.RightDir) pic = translate (fromIntegral x) (fromIntegral y) $ rotate 90.0 pic
translatedPlayerPic (Just (x, y)) (Just GameState.LeftDir) pic = translate (fromIntegral x) (fromIntegral y) $ rotate 270.0 pic
translatedPlayerPic _ _ pic = pic

renderGame :: Picture -> Picture -> SystemState -> Picture
renderGame _ _ (GameState [], _)                       = waitPlayer2Field
renderGame _ _ (GameState [_], _)                      = waitPlayer2Field
renderGame firstPlayerPic secondPlayerPic (GameState [(p1Dir, p1Crds), (p2Dir, p2Crds)], _) = pictures [
    gameField
    , translatedPlayerPic p1Crds p1Dir firstPlayerPic
    , translatedPlayerPic p2Crds p2Dir secondPlayerPic
    ]
renderGame _ _ _ = blank

wrapP1DirectionWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> PlayerRequests
  -> IO SystemState
wrapP1DirectionWithIO requestChan newP1Dir p2Dir p1Crds p2Crds updDirRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updDirRequest
    return (GameState [(Just newP1Dir, Just p1Crds), (Just p2Dir, Just p2Crds)], PlayerState (Just 1) (Just newP1Dir) $ Just p1Crds)

wrapP1MovementWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> PlayerRequests
  -> IO SystemState
wrapP1MovementWithIO requestChan p1Dir p2Dir newP1Crds p2Crds updCrdsRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    return (GameState [(Just p1Dir, Just newP1Crds), (Just p2Dir, Just p2Crds)], PlayerState (Just 1) (Just p1Dir) $ Just newP1Crds)

wrapP2DirectionWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> PlayerRequests
  -> IO SystemState
wrapP2DirectionWithIO requestChan p1Dir newP2Dir p1Crds p2Crds updDirRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updDirRequest
    return (GameState [(Just p1Dir, Just p1Crds), (Just newP2Dir, Just p2Crds)], PlayerState (Just 2) (Just newP2Dir) $ Just p2Crds)

wrapP2MovementWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> PlayerRequests
  -> IO SystemState
wrapP2MovementWithIO requestChan p1Dir p2Dir p1Crds newP2Crds updCrdsRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    return (GameState [(Just p1Dir, Just p1Crds), (Just p2Dir, Just newP2Crds)], PlayerState (Just 2) (Just p2Dir) $ Just newP2Crds)

handleDirectionEvent ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Word8
  -> (Direction -> Direction)
  -> SystemState
handleDirectionEvent requestChan (GameState [(Just p1Dir, Just p1Crds), (Just p2Dir, Just p2Crds)], playerState) playerInd dirTransf = case playerInd of
    1 -> do
        let newP1Dir = dirTransf p1Dir
        let updDirsRequest = GameState.UpdateDirection (UpdateDirectionData 1 newP1Dir)
        unsafePerformIO $ wrapP1DirectionWithIO requestChan newP1Dir p2Dir p1Crds p2Crds updDirsRequest

    2 -> do
        let newP2Dir = dirTransf p2Dir
        let updDirsRequest = GameState.UpdateDirection (UpdateDirectionData 2 newP2Dir)
        unsafePerformIO $ wrapP2DirectionWithIO requestChan p1Dir newP2Dir p1Crds p2Crds updDirsRequest

    _ -> curState

    where
        curState = (GameState [(Just p1Dir, Just p1Crds), (Just p2Dir, Just p2Crds)], playerState)

handleDirectionEvent _ state _ _ = state

handleMovementEvent ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Word8
  -> (Coordinates -> Coordinates)
  -> SystemState
handleMovementEvent requestChan (GameState [(Just p1Dir, Just p1Crds), (Just p2Dir, Just p2Crds)], playerState) playerInd crdTransf = case playerInd of
    1 -> do
        let newP1Crds = crdTransf p1Crds
        if beyoundBorders newP1Crds then curState else do
          let updCrdsRequest = GameState.UpdatePosition (UpdatePositionData 1 newP1Crds)
          unsafePerformIO $ wrapP1MovementWithIO requestChan p1Dir p2Dir newP1Crds p2Crds updCrdsRequest

    2 -> do
        let newP2Crds = crdTransf p2Crds
        if beyoundBorders newP2Crds then curState else do
          let updCrdsRequest = GameState.UpdatePosition (UpdatePositionData 2 newP2Crds)
          unsafePerformIO $ wrapP2MovementWithIO requestChan p1Dir p2Dir p1Crds newP2Crds updCrdsRequest

    _ -> curState

    where
        curState = (GameState [(Just p1Dir, Just p1Crds), (Just p2Dir, Just p2Crds)], playerState)

handleMovementEvent _ state _ _ = state

onLeftRightEvent :: SpecialKey -> Direction -> Direction
onLeftRightEvent KeyRight GameState.UpDir    = GameState.RightDir
onLeftRightEvent KeyLeft GameState.UpDir     = GameState.LeftDir
onLeftRightEvent KeyRight GameState.DownDir  = GameState.LeftDir
onLeftRightEvent KeyLeft GameState.DownDir   = GameState.RightDir
onLeftRightEvent KeyRight GameState.LeftDir  = GameState.UpDir
onLeftRightEvent KeyLeft GameState.LeftDir   = GameState.DownDir
onLeftRightEvent KeyRight GameState.RightDir = GameState.DownDir
onLeftRightEvent KeyLeft GameState.RightDir  = GameState.UpDir
onLeftRightEvent _ _                         = GameState.UpDir

handleGameEvents :: TChan GameState.PlayerRequests -> Event -> SystemState -> SystemState
------------------------------------------ On Up pressed ------------------------------------------
handleGameEvents requestChan (EventKey (SpecialKey KeyUp) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) playerInd (\(x, y) -> (x, y + 5))
handleGameEvents requestChan (EventKey (SpecialKey KeyUp) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) playerInd (\(x, y) -> (x, y - 5))
handleGameEvents requestChan (EventKey (SpecialKey KeyUp) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) playerInd (\(x, y) -> (x + 5, y))
handleGameEvents requestChan (EventKey (SpecialKey KeyUp) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) playerInd (\(x, y) -> (x - 5, y))

------------------------------------------ On Down pressed ------------------------------------------
handleGameEvents requestChan (EventKey (SpecialKey KeyDown) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) playerInd (\(x, y) -> (x, y - 2))
handleGameEvents requestChan (EventKey (SpecialKey KeyDown) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) playerInd (\(x, y) -> (x, y + 2))
handleGameEvents requestChan (EventKey (SpecialKey KeyDown) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) playerInd (\(x, y) -> (x - 2, y))
handleGameEvents requestChan (EventKey (SpecialKey KeyDown) Up _ _) (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) =
    handleMovementEvent requestChan (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) playerInd (\(x, y) -> (x + 2, y))

------------------------------------------ On Left/Right pressed ------------------------------------------
handleGameEvents requestChan (EventKey (SpecialKey KeyRight) Up _ _) (gameState, PlayerState (Just playerInd) dir crds) =
    handleDirectionEvent requestChan (gameState, PlayerState (Just playerInd) dir crds) playerInd $ onLeftRightEvent KeyRight
handleGameEvents requestChan (EventKey (SpecialKey KeyLeft) Up _ _) (gameState, PlayerState (Just playerInd) dir crds) =
    handleDirectionEvent requestChan (gameState, PlayerState (Just playerInd) dir crds) playerInd $ onLeftRightEvent KeyLeft

handleGameEvents _ _ state = state

updateGame :: TChan SystemState -> Float -> SystemState -> SystemState
updateGame eventQueue _ state = -- реализация этой штуки под капотом слишком ленива, чтобы использовать обычный readTChan
    Maybe.fromMaybe state $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

launchGame :: TChan SystemState -> TChan GameState.PlayerRequests -> Picture -> Picture -> IO ()
launchGame eventQueue requestChan firstPlayerPic secondPlayerPic = Gloss.play
  mainWindow
  white
  fps
  initialSystemState
  (renderGame firstPlayerPic secondPlayerPic)
  (handleGameEvents requestChan)
  (updateGame eventQueue)

main :: IO ()
main = do
    eventQueue <- STM.atomically STM.newTChan
    requestChan <- STM.atomically STM.newTChan
    firstPlayerTank <- loadBMP "./green_tank.bmp"
    secondPlayerTank <- loadBMP "./blue_tank.bmp"
    let firstPlayerPic = scale 0.1 0.1 firstPlayerTank
    let secondPlayerPic = scale 0.1 0.1 secondPlayerTank
    _ <- Async.concurrently (launchClientHandler eventQueue requestChan) $ launchGame eventQueue requestChan firstPlayerPic secondPlayerPic
    return ()
