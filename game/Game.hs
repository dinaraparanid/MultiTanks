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
translatedPlayerPic (Just (x, y)) (Just GameState.UpDir) pic =
  translate (fromIntegral x) (fromIntegral y) pic
translatedPlayerPic (Just (x, y)) (Just GameState.DownDir) pic =
  translate (fromIntegral x) (fromIntegral y) $ rotate 180.0 pic
translatedPlayerPic (Just (x, y)) (Just GameState.RightDir) pic =
  translate (fromIntegral x) (fromIntegral y) $ rotate 90.0 pic
translatedPlayerPic (Just (x, y)) (Just GameState.LeftDir) pic =
  translate (fromIntegral x) (fromIntegral y) $ rotate 270.0 pic
translatedPlayerPic _ _ pic = pic

translatedShotPic :: Maybe ShotData -> Picture
translatedShotPic (Just (ShotData 1 (x, y) _)) =
  translate
    (fromIntegral x)
    (fromIntegral y)
    $ color green
    $ thickCircle 1.0 5.0

translatedShotPic (Just (ShotData 2 (x, y) _)) =
  translate
    (fromIntegral x)
    (fromIntegral y)
    $ color blue
    $ thickCircle 1.0 5.0

translatedShotPic _ = blank

renderGame :: Picture -> Picture -> SystemState -> Picture
renderGame _ _ (GameState [], _)                                   = waitPlayer2Field
renderGame _ _ (GameState [_], _)                                  = waitPlayer2Field
renderGame _ _ (GameState [_, (Nothing, Nothing, Nothing)], _)     = player1WonField
renderGame _ _ (GameState [(Nothing, Nothing, Nothing), _], _)     = player2WonField
renderGame
  firstPlayerPic
  secondPlayerPic
  (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], _) =
    pictures [
      gameField
      , translatedPlayerPic p1Crds p1Dir firstPlayerPic
      , translatedPlayerPic p2Crds p2Dir secondPlayerPic
      , translatedShotPic p1Shot
      , translatedShotPic p2Shot
      ]
renderGame _ _ _ = blank

wrapP1DirectionWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> Maybe ShotData
  -> Maybe ShotData
  -> PlayerRequests
  -> IO SystemState
wrapP1DirectionWithIO requestChan newP1Dir p2Dir p1Crds p2Crds p1Shot p2Shot updDirRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updDirRequest
    return (GameState
             [(Just newP1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)]
             , PlayerState (Just 1) (Just newP1Dir) (Just p1Crds)
             )

wrapP1MovementWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> Maybe ShotData
  -> Maybe ShotData
  -> PlayerRequests
  -> IO SystemState
wrapP1MovementWithIO requestChan p1Dir p2Dir newP1Crds p2Crds p1Shot p2Shot updCrdsRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    return (GameState
             [(Just p1Dir, Just newP1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)]
             , PlayerState (Just 1) (Just p1Dir) (Just newP1Crds)
             )

wrapP1ShotWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> ShotData
  -> Maybe ShotData
  -> PlayerRequests
  -> IO SystemState
wrapP1ShotWithIO requestChan p1Dir p2Dir p1Crds p2Crds p1Shot p2Shot shootRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan shootRequest
    return (GameState
             [(Just p1Dir, Just p1Crds, Just p1Shot), (Just p2Dir, Just p2Crds, p2Shot)]
             , PlayerState (Just 1) (Just p1Dir) (Just p1Crds)
             )

wrapP2DirectionWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> Maybe ShotData
  -> Maybe ShotData
  -> PlayerRequests
  -> IO SystemState
wrapP2DirectionWithIO requestChan p1Dir newP2Dir p1Crds p2Crds p1Shot p2Shot updDirRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updDirRequest
    return (GameState
             [(Just p1Dir, Just p1Crds, p1Shot), (Just newP2Dir, Just p2Crds, p2Shot)]
             , PlayerState (Just 2) (Just newP2Dir) (Just p2Crds)
             )

wrapP2MovementWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> Maybe ShotData
  -> Maybe ShotData
  -> PlayerRequests
  -> IO SystemState
wrapP2MovementWithIO requestChan p1Dir p2Dir p1Crds newP2Crds p1Shot p2Shot updCrdsRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    return (GameState
             [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just newP2Crds, p2Shot)]
             , PlayerState (Just 2) (Just p2Dir) (Just newP2Crds)
             )

wrapP2ShotWithIO ::
  TChan GameState.PlayerRequests
  -> Direction
  -> Direction
  -> Coordinates
  -> Coordinates
  -> Maybe ShotData
  -> ShotData
  -> PlayerRequests
  -> IO SystemState
wrapP2ShotWithIO requestChan p1Dir p2Dir p1Crds p2Crds p1Shot p2Shot shootRequest = do
    _ <- STM.atomically $ STM.writeTChan requestChan shootRequest
    return (GameState
             [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, Just p2Shot)]
             , PlayerState (Just 2) (Just p2Dir) (Just p2Crds)
             )

handleDirectionEvent ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Word8
  -> (Direction -> Direction)
  -> SystemState
handleDirectionEvent
  requestChan
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], _)
  1 dirTransf = do
    let newP1Dir = dirTransf p1Dir
    let updDirsRequest = GameState.UpdateDirection (UpdateDirectionData 1 newP1Dir)
    unsafePerformIO $ wrapP1DirectionWithIO
      requestChan newP1Dir p2Dir p1Crds p2Crds p1Shot p2Shot updDirsRequest

handleDirectionEvent
  requestChan
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], _)
  2 dirTransf = do
    let newP2Dir = dirTransf p2Dir
    let updDirsRequest = GameState.UpdateDirection (UpdateDirectionData 2 newP2Dir)
    unsafePerformIO $ wrapP2DirectionWithIO
      requestChan p1Dir newP2Dir p1Crds p2Crds p1Shot p2Shot updDirsRequest

handleDirectionEvent _ state _ _ = state

handleMovementEvent ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Word8
  -> (Coordinates -> Coordinates)
  -> SystemState
handleMovementEvent
  requestChan
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
  1 crdTransf = if beyoundBorders newP1Crds then curState else nextState
  where
    curState = (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
    newP1Crds = crdTransf p1Crds
    updCrdsRequest = GameState.UpdatePosition (UpdatePositionData 1 newP1Crds)
    nextState = unsafePerformIO $ wrapP1MovementWithIO
      requestChan p1Dir p2Dir newP1Crds p2Crds p1Shot p2Shot updCrdsRequest

handleMovementEvent
  requestChan
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
  2 crdTransf = if beyoundBorders newP2Crds then curState else nextState
  where
    curState = (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
    newP2Crds = crdTransf p2Crds
    updCrdsRequest = GameState.UpdatePosition (UpdatePositionData 2 newP2Crds)
    nextState = unsafePerformIO $ wrapP2MovementWithIO
      requestChan p1Dir p2Dir p1Crds newP2Crds p1Shot p2Shot updCrdsRequest

handleMovementEvent _ state _ _ = state

handleShootEvent ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Word8
  -> (Maybe ShotData -> ShotData)
  -> SystemState
handleShootEvent
  requestChan
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
  1 shotTransf = do
    let (ShotData _ crds _) = newP1Shot
    if beyoundBorders crds then noShotState else nextState
  where
    noShotState = (GameState [(Just p1Dir, Just p1Crds, Nothing), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
    newP1Shot = shotTransf p1Shot
    shootRequest = GameState.Shoot newP1Shot
    nextState = unsafePerformIO $ wrapP1ShotWithIO
      requestChan p1Dir p2Dir p1Crds p2Crds newP1Shot p2Shot shootRequest

handleShootEvent
  requestChan
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
  2 shotTransf = do
    let (ShotData _ crds _) = newP2Shot
    if beyoundBorders crds then noShotState else nextState
  where
    noShotState = (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, Nothing)], playerState)
    newP2Shot = shotTransf p2Shot
    shootRequest = GameState.Shoot newP2Shot
    nextState = unsafePerformIO $ wrapP2ShotWithIO
      requestChan p1Dir p2Dir p1Crds p2Crds p1Shot newP2Shot shootRequest

handleShootEvent _ state _ _ = state

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
handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) =
    handleMovementEvent
    requestChan
    (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds)
    playerInd
    (\(x, y) -> (x, y + 5))

handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) =
    handleMovementEvent
    requestChan
    (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds)
    playerInd
    (\(x, y) -> (x, y - 5))

handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) =
    handleMovementEvent
      requestChan
      (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds)
      playerInd
      (\(x, y) -> (x + 5, y))

handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) =
    handleMovementEvent
      requestChan
      (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds)
      playerInd
      (\(x, y) -> (x - 5, y))

------------------------------------------ On Down pressed ------------------------------------------
handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) =
    handleMovementEvent
      requestChan
      (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds)
      playerInd
      (\(x, y) -> (x, y - 2))

handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) =
    handleMovementEvent
      requestChan
      (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds)
      playerInd
      (\(x, y) -> (x, y + 2))

handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) =
    handleMovementEvent
      requestChan
      (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds)
      playerInd
      (\(x, y) -> (x - 2, y))

handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) =
    handleMovementEvent
      requestChan
      (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds)
      playerInd
      (\(x, y) -> (x + 2, y))

------------------------------------------ On Left/Right pressed ------------------------------------------
handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyRight) Up _ _)
  (gameState, PlayerState (Just playerInd) dir crds) =
    handleDirectionEvent
      requestChan
      (gameState, PlayerState (Just playerInd) dir crds)
      playerInd
      $ onLeftRightEvent KeyRight

handleGameEvents
  requestChan
  (EventKey (SpecialKey KeyLeft) Up _ _)
  (gameState, PlayerState (Just playerInd) dir crds) =
    handleDirectionEvent
      requestChan
      (gameState, PlayerState (Just playerInd) dir crds)
      playerInd
      $ onLeftRightEvent KeyLeft

------------------------------------------ On Shoot pressed ------------------------------------------
handleGameEvents
  requestChan
  (EventKey (SpecialKey KeySpace) Up _ _)
  (gameState, PlayerState (Just playerInd) dir crds) =
    handleShootEvent
      requestChan
      (gameState, PlayerState (Just playerInd) dir crds)
      playerInd
      newShotOnNothing
  where
    newShotOnNothing mbShotData = case mbShotData of
      Nothing       -> ShotData playerInd (Maybe.fromJust crds) (Maybe.fromJust dir)
      Just shotData -> shotData

handleGameEvents _ _ state = state

updateShot :: Maybe ShotData -> Maybe ShotData
updateShot Nothing = Nothing
updateShot (Just (ShotData playerInd (x, y) dir)) =
  if beyoundBorders (x, y) then Nothing
  else case dir of
    GameState.UpDir    -> Just (ShotData playerInd (x, y + 5) dir)
    GameState.DownDir  -> Just (ShotData playerInd (x, y - 5) dir)
    GameState.RightDir -> Just (ShotData playerInd (x + 5, y) dir)
    GameState.LeftDir  -> Just (ShotData playerInd (x - 5, y) dir)

updateGame :: TChan SystemState -> TChan GameState.PlayerRequests -> Float -> SystemState -> SystemState
updateGame eventQueue requestChan _ (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 1) dir crds) =
  case fetchedState of
    (GameState [(fP1Dir, fP1Crds, fP1Shot), (fP2Dir, fP2Crds, fP2Shot)], _) -> do
      let newP1Shot = updateShot fP1Shot
      let newP2Shot = updateShot fP2Shot
      updateGame1_ requestChan fetchedState fP1Dir fP1Crds newP1Shot fP2Dir fP2Crds newP2Shot
    state -> state

  where
    curPlayerState = PlayerState (Just 1) dir crds
    curState = (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], curPlayerState)
    fetchedState = Maybe.fromMaybe curState $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

updateGame eventQueue requestChan _ (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 2) dir crds) =
  case fetchedState of
    (GameState [(fP1Dir, fP1Crds, fP1Shot), (fP2Dir, fP2Crds, fP2Shot)], _) -> do
      let newP1Shot = updateShot fP1Shot
      let newP2Shot = updateShot fP2Shot
      updateGame2_ requestChan fetchedState fP1Dir fP1Crds newP1Shot fP2Dir fP2Crds newP2Shot
    state -> state

  where
    curPlayerState = PlayerState (Just 2) dir crds
    curState = (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], curPlayerState)
    fetchedState = Maybe.fromMaybe curState $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

updateGame eventQueue _ _ curState =
  Maybe.fromMaybe curState $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

updateGame1_ ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Maybe Direction
  -> Maybe Coordinates
  -> Maybe ShotData
  -> Maybe Direction
  -> Maybe Coordinates
  -> Maybe ShotData
  -> SystemState
updateGame1_
  requestChan _
  (Just fP1Dir)
  (Just fP1Crds)
  (Just newP1Shot)
  (Just fP2Dir)
  (Just fP2Crds)
  newP2Shot = unsafePerformIO
    $ wrapP1ShotWithIO requestChan fP1Dir fP2Dir fP1Crds fP2Crds newP1Shot newP2Shot
    $ GameState.Shoot newP1Shot

updateGame1_
  _ (_, curPlayerState)
  fP1Dir
  fP1Crds
  Nothing
  fP2Dir
  fP2Crds
  newP2Shot = (GameState [(fP1Dir, fP1Crds, Nothing), (fP2Dir, fP2Crds, newP2Shot)], curPlayerState)

updateGame1_ _ fetchedState _ _ _ _ _ _ = fetchedState

updateGame2_ ::
  TChan GameState.PlayerRequests
  -> SystemState
  -> Maybe Direction
  -> Maybe Coordinates
  -> Maybe ShotData
  -> Maybe Direction
  -> Maybe Coordinates
  -> Maybe ShotData
  -> SystemState
updateGame2_
  requestChan _
  (Just fP1Dir)
  (Just fP1Crds)
  newP1Shot
  (Just fP2Dir)
  (Just fP2Crds)
  (Just newP2Shot) = unsafePerformIO
    $ wrapP2ShotWithIO requestChan fP1Dir fP2Dir fP1Crds fP2Crds newP1Shot newP2Shot
    $ GameState.Shoot newP2Shot

updateGame2_
  _ (_, curPlayerState)
  fP1Dir
  fP1Crds
  newP1Shot
  fP2Dir
  fP2Crds
  Nothing = (GameState [(fP1Dir, fP1Crds, newP1Shot), (fP2Dir, fP2Crds, Nothing)], curPlayerState)

updateGame2_ _ fetchedState _ _ _ _ _ _ = fetchedState

launchGame :: TChan SystemState -> TChan GameState.PlayerRequests -> Picture -> Picture -> IO ()
launchGame eventQueue requestChan firstPlayerPic secondPlayerPic = Gloss.play
  mainWindow
  white
  fps
  initialSystemState
  (renderGame firstPlayerPic secondPlayerPic)
  (handleGameEvents requestChan)
  (updateGame eventQueue requestChan)

main :: IO ()
main = do
    eventQueue <- STM.atomically STM.newTChan
    requestChan <- STM.atomically STM.newTChan

    firstPlayerTank <- loadBMP "./green_tank.bmp"
    secondPlayerTank <- loadBMP "./blue_tank.bmp"
    let firstPlayerPic = scale 0.1 0.1 firstPlayerTank
    let secondPlayerPic = scale 0.1 0.1 secondPlayerTank

    _ <- Async.concurrently
      (launchClientHandler eventQueue requestChan)
      $ launchGame eventQueue requestChan firstPlayerPic secondPlayerPic

    return ()
