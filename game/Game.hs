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

----------------------- Pictures and Rendering --------------------------

-- | Translates player's tank to the correct position
--   and turns it according to the direction
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

-- | Constructs shot picture from the given data.
--   In case if there is no shot data, blank image is used
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

-- | Renders game level with all players and events that are happen.
--   In case if second player has not connected,
--   wait for 2-nd player window is shown.
--   In case if either 1-st or 2-nd player has won,
--   victory screen of particular player is shown.
--   In other cases, typical game field is shown
renderGame ::
  Picture        -- ^ first player picture (already rotated and translated)
  -> Picture     -- ^ second player picture (already rotated and translated)
  -> SystemState -- ^ current system state
  -> Picture
renderGame _ _ (GameState [], _)                               = waitPlayer2Field
renderGame _ _ (GameState [_], _)                              = waitPlayer2Field
renderGame _ _ (GameState [_, (Nothing, Nothing, Nothing)], _) = player1WonField
renderGame _ _ (GameState [(Nothing, Nothing, Nothing), _], _) = player2WonField
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

----------------------- Message-Passing Wrappers --------------------------

-- | Wraps direction event writing for the 1-st player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP1DirectionWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ new player 1 direction
  -> Direction                   -- ^ current player 2 direction
  -> Coordinates                 -- ^ current player 1 position
  -> Coordinates                 -- ^ current player 2 position
  -> Maybe ShotData              -- ^ current player 1 shot state
  -> Maybe ShotData              -- ^ current player 2 shot state
  -> PlayerRequests              -- ^ update player's direction request to the server
  -> IO SystemState
wrapP1DirectionWithIO requestChan curSysStateHolder newP1Dir p2Dir p1Crds p2Crds p1Shot p2Shot updDirRequest = do
    STM.atomically $ STM.writeTChan requestChan updDirRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just newP1Dir, Just p1Crds, p1Shot)
      secondPlayerState = (Just p2Dir, Just p2Crds, p2Shot)
      newDirectionGameState = GameState [firstPlayerState, secondPlayerState]
      newDirectionPlayerState = PlayerState (Just 1) (Just newP1Dir) (Just p1Crds)
      systemState = (newDirectionGameState, newDirectionPlayerState)

-- | Wraps movement event writing for the 1-st player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP1MovementWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ current player 1 direction
  -> Direction                   -- ^ current player 2 direction
  -> Coordinates                 -- ^ new player 1 position
  -> Coordinates                 -- ^ current player 2 position
  -> Maybe ShotData              -- ^ current player 1 shot state
  -> Maybe ShotData              -- ^ current player 2 shot state
  -> PlayerRequests              -- ^ update player's movement request to the server
  -> IO SystemState
wrapP1MovementWithIO requestChan curSysStateHolder p1Dir p2Dir newP1Crds p2Crds p1Shot p2Shot updCrdsRequest = do
    STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just p1Dir, Just newP1Crds, p1Shot)
      secondPlayerState = (Just p2Dir, Just p2Crds, p2Shot)
      newMovementGameState = GameState [firstPlayerState, secondPlayerState]
      newMovementPlayerState = PlayerState (Just 1) (Just p1Dir) (Just newP1Crds)
      systemState = (newMovementGameState, newMovementPlayerState)

-- | Wraps shot event writing for the 1-st player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP1ShotWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ current player 1 direction
  -> Direction                   -- ^ current player 2 direction
  -> Coordinates                 -- ^ current player 1 position
  -> Coordinates                 -- ^ current player 2 position
  -> ShotData                    -- ^ new player 1 shot data
  -> Maybe ShotData              -- ^ current player 2 shot data
  -> PlayerRequests              -- ^ player's shoot state request to the server
  -> IO SystemState
wrapP1ShotWithIO requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds p1Shot p2Shot shootRequest = do
    STM.atomically $ STM.writeTChan requestChan shootRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just p1Dir, Just p1Crds, Just p1Shot)
      secondPlayerState = (Just p2Dir, Just p2Crds, p2Shot)
      newShotGameState = GameState [firstPlayerState, secondPlayerState]
      newShotPlayerState = PlayerState (Just 1) (Just p1Dir) (Just p1Crds)
      systemState = (newShotGameState, newShotPlayerState)

-- | Wraps shot cancelation event writin for the 1-st player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP1CancelShotWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ current player 1 direction
  -> Direction                   -- ^ current player 2 direction
  -> Coordinates                 -- ^ current player 1 position
  -> Coordinates                 -- ^ current player 2 position
  -> Maybe ShotData              -- ^ current player 2 shot data
  -> PlayerRequests              -- ^ cancel shoot request to the server
  -> IO SystemState
wrapP1CancelShotWithIO requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds p2Shot shootRequest = do
    STM.atomically $ STM.writeTChan requestChan shootRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just p1Dir, Just p1Crds, Nothing)
      secondPlayerState = (Just p2Dir, Just p2Crds, p2Shot)
      newCancelShotGameState = GameState [firstPlayerState, secondPlayerState]
      newCancelShotPlayerState = PlayerState (Just 1) (Just p1Dir) (Just p1Crds)
      systemState = (newCancelShotGameState, newCancelShotPlayerState)

-- | Wraps direction event writing for the 2-nd player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP2DirectionWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ current player 1 direction
  -> Direction                   -- ^ new player 2 direction
  -> Coordinates                 -- ^ current player 1 position
  -> Coordinates                 -- ^ current player 2 position
  -> Maybe ShotData              -- ^ current player 1 shot state
  -> Maybe ShotData              -- ^ current player 2 shot state
  -> PlayerRequests              -- ^ update player's direction request to the server
  -> IO SystemState
wrapP2DirectionWithIO requestChan curSysStateHolder p1Dir newP2Dir p1Crds p2Crds p1Shot p2Shot updDirRequest = do
    STM.atomically $ STM.writeTChan requestChan updDirRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just p1Dir, Just p1Crds, p1Shot)
      secondPlayerState = (Just newP2Dir, Just p2Crds, p2Shot)
      newDirectionGameState = GameState [firstPlayerState, secondPlayerState]
      newDirectionPlayerState = PlayerState (Just 2) (Just newP2Dir) (Just p2Crds)
      systemState = (newDirectionGameState, newDirectionPlayerState)

-- | Wraps movement event writing for the 2-nd player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP2MovementWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ current player 1 direction
  -> Direction                   -- ^ current player 2 direction
  -> Coordinates                 -- ^ current player 1 position
  -> Coordinates                 -- ^ new player 2 position
  -> Maybe ShotData              -- ^ current player 1 shot state
  -> Maybe ShotData              -- ^ current player 2 shot state
  -> PlayerRequests              -- ^ update player's movement request to the server
  -> IO SystemState
wrapP2MovementWithIO requestChan curSysStateHolder p1Dir p2Dir p1Crds newP2Crds p1Shot p2Shot updCrdsRequest = do
    STM.atomically $ STM.writeTChan requestChan updCrdsRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just p1Dir, Just p1Crds, p1Shot)
      secondPlayerState = (Just p2Dir, Just newP2Crds, p2Shot)
      newMovementGameState = GameState [firstPlayerState, secondPlayerState]
      newMovementPlayerState = PlayerState (Just 2) (Just p2Dir) (Just newP2Crds)
      systemState = (newMovementGameState, newMovementPlayerState)

-- | Wraps shot event writing for the 2-nd player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP2ShotWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ current player 1 direction
  -> Direction                   -- ^ current player 2 direction
  -> Coordinates                 -- ^ current player 1 position
  -> Coordinates                 -- ^ current player 2 position
  -> Maybe ShotData              -- ^ current player 1 shot data
  -> ShotData                    -- ^ new player 2 shot data
  -> PlayerRequests              -- ^ player's shoot state request to the server
  -> IO SystemState
wrapP2ShotWithIO requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds p1Shot p2Shot shootRequest = do
    STM.atomically $ STM.writeTChan requestChan shootRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just p1Dir, Just p1Crds, p1Shot)
      secondPlayerState = (Just p2Dir, Just p2Crds, Just p2Shot)
      newShotGameState = GameState [firstPlayerState, secondPlayerState]
      newShotPlayerState = PlayerState (Just 2) (Just p2Dir) (Just p2Crds)
      systemState = (newShotGameState, newShotPlayerState)


-- | Wraps cancel shot event writing for the 2-nd player
--   to the atomic channel with IO monad.
--   Additionally, updates current system state
wrapP2CancelShotWithIO ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Direction                   -- ^ current player 1 direction
  -> Direction                   -- ^ current player 2 direction
  -> Coordinates                 -- ^ current player 1 position
  -> Coordinates                 -- ^ current player 2 position
  -> Maybe ShotData              -- ^ current player 1 shot data
  -> PlayerRequests              -- ^ player's shoot state request to the server
  -> IO SystemState
wrapP2CancelShotWithIO requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds p1Shot shootRequest = do
    STM.atomically $ STM.writeTChan requestChan shootRequest
    STM.atomically $ STM.writeTVar curSysStateHolder systemState
    return systemState
    where
      firstPlayerState = (Just p1Dir, Just p1Crds, p1Shot)
      secondPlayerState = (Just p2Dir, Just p2Crds, Nothing)
      newCancelShotGameState = GameState [firstPlayerState, secondPlayerState]
      newCancelShotPlayerState = PlayerState (Just 2) (Just p2Dir) (Just p2Crds)
      systemState = (newCancelShotGameState, newCancelShotPlayerState)

----------------------- Event handlers --------------------------

-- | Updates positions for both players
---  by fetching them from the server
handleDirectionEvent ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> SystemState                 -- ^ current system state
  -> Word8                       -- ^ player's index (1 or 2)
  -> (Direction -> Direction)    -- ^ direction transformer
  -> SystemState
handleDirectionEvent
  requestChan
  curSysStateHolder
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], _)
  1 dirTransf = do
    let newP1Dir = dirTransf p1Dir
    let updDirsRequest = GameState.UpdateDirection (UpdateDirectionData 1 newP1Dir)

    -- Не придумал ничего достаточно элегантного и простого, чтобы этого избежать.
    -- Учитывая, что функция запускается реактивно, так или иначе придется использовать
    -- примитивы синхронизации или атомарность, иначе это всё не потокобезопасно.
    -- Другое решение, которое меня посетило - хранить запросы в SystemState,
    -- но это решение так же не принесет ничего существенного из-за реактивности
    -- (в чистых реактивных функциях всё равно придется вызывать IO/STM)
    unsafePerformIO $ wrapP1DirectionWithIO
      requestChan curSysStateHolder newP1Dir p2Dir p1Crds p2Crds p1Shot p2Shot updDirsRequest

handleDirectionEvent
  requestChan
  curSysStateHolder
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], _)
  2 dirTransf = do
    let newP2Dir = dirTransf p2Dir
    let updDirsRequest = GameState.UpdateDirection (UpdateDirectionData 2 newP2Dir)

    -- Про причины использования unsafePerformIO читать выше
    unsafePerformIO $ wrapP2DirectionWithIO
      requestChan curSysStateHolder p1Dir newP2Dir p1Crds p2Crds p1Shot p2Shot updDirsRequest

handleDirectionEvent _ _ state _ _ = state

-- | Updates positions for both players
---  by fetching them from the server
handleMovementEvent ::
  TChan GameState.PlayerRequests  -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState             -- ^ atomic holder for a current system's state
  -> SystemState                  -- ^ current system state
  -> Word8                        -- ^ player's index (1 or 2)
  -> (Coordinates -> Coordinates) -- ^ position transformer
  -> SystemState
handleMovementEvent
  requestChan
  curSysStateHolder
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
  1 crdTransf = if beyoundBorders newP1Crds then curState else nextState
  where
    firstPlayerState = (Just p1Dir, Just p1Crds, p1Shot)
    secondPlayerState = (Just p2Dir, Just p2Crds, p2Shot)
    curState = (GameState [firstPlayerState, secondPlayerState], playerState)
    newP1Crds = crdTransf p1Crds
    updCrdsRequest = GameState.UpdatePosition $ UpdatePositionData 1 newP1Crds
    nextState = unsafePerformIO $ wrapP1MovementWithIO -- О причинах использования unsafePerformIO читать выше
      requestChan curSysStateHolder p1Dir p2Dir newP1Crds p2Crds p1Shot p2Shot updCrdsRequest

handleMovementEvent
  requestChan
  curSysStateHolder
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], playerState)
  2 crdTransf = if beyoundBorders newP2Crds then curState else nextState
  where
    firstPlayerState = (Just p1Dir, Just p1Crds, p1Shot)
    secondPlayerState = (Just p2Dir, Just p2Crds, p2Shot)
    curState = (GameState [firstPlayerState, secondPlayerState], playerState)
    newP2Crds = crdTransf p2Crds
    updCrdsRequest = GameState.UpdatePosition $ UpdatePositionData 2 newP2Crds
    nextState = unsafePerformIO $ wrapP2MovementWithIO -- О причинах использования unsafePerformIO читать выше
      requestChan curSysStateHolder p1Dir p2Dir p1Crds newP2Crds p1Shot p2Shot updCrdsRequest

handleMovementEvent _ _ state _ _ = state

-- | Updates shot states for both players
--   by fetching them from the server
handleShootEvent ::
  TChan GameState.PlayerRequests  -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState             -- ^ atomic holder for a current system's state
  -> SystemState                  -- ^ current system state
  -> Word8                        -- ^ player's index (1 or 2)
  -> (Maybe ShotData -> ShotData) -- ^ shot data's transformer
  -> SystemState
handleShootEvent
  requestChan
  curSysStateHolder
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], _)
  1 shotTransf = if beyoundBorders crds then noShotState else shootState
  where
    cancelShootRequest = GameState.CancelShoot 1
    noShotState = unsafePerformIO $ wrapP1CancelShotWithIO
      requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds p2Shot cancelShootRequest

    newP1Shot = shotTransf p1Shot
    (ShotData _ crds _) = newP1Shot
    shootRequest = GameState.Shoot newP1Shot
    shootState = unsafePerformIO $ wrapP1ShotWithIO -- О причинах использования unsafePerformIO читать выше
      requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds newP1Shot p2Shot shootRequest

handleShootEvent
  requestChan
  curSysStateHolder
  (GameState [(Just p1Dir, Just p1Crds, p1Shot), (Just p2Dir, Just p2Crds, p2Shot)], _)
  2 shotTransf = if beyoundBorders crds then noShotState else nextState
  where
    cancelShootRequest = GameState.CancelShoot 2
    noShotState = unsafePerformIO $ wrapP2CancelShotWithIO
      requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds p1Shot cancelShootRequest

    newP2Shot = shotTransf p2Shot
    (ShotData _ crds _) = newP2Shot
    shootRequest = GameState.Shoot newP2Shot
    nextState = unsafePerformIO $ wrapP2ShotWithIO -- О причинах использования unsafePerformIO читать выше
      requestChan curSysStateHolder p1Dir p2Dir p1Crds p2Crds p1Shot newP2Shot shootRequest

handleShootEvent _ _ state _ _ = state

-- | Gets next player's direction after key was pressed
onLeftRightEvent ::
  SpecialKey   -- ^ pressed key
  -> Direction -- ^ current player's direction
  -> Direction
onLeftRightEvent KeyRight GameState.UpDir    = GameState.RightDir
onLeftRightEvent KeyLeft GameState.UpDir     = GameState.LeftDir
onLeftRightEvent KeyRight GameState.DownDir  = GameState.LeftDir
onLeftRightEvent KeyLeft GameState.DownDir   = GameState.RightDir
onLeftRightEvent KeyRight GameState.LeftDir  = GameState.UpDir
onLeftRightEvent KeyLeft GameState.LeftDir   = GameState.DownDir
onLeftRightEvent KeyRight GameState.RightDir = GameState.DownDir
onLeftRightEvent KeyLeft GameState.RightDir  = GameState.UpDir
onLeftRightEvent _ _                         = GameState.UpDir

-- | Handles game input events of the player.
--   Produces updated state on each valuable event's call
handleGameEvents ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState            -- ^ atomic holder for a current system's state
  -> Event                       -- ^ event that triggered the update
  -> SystemState                 -- ^ current system's state
  -> SystemState
------------------------------------------ On Up pressed ------------------------------------------
handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds)
      playerInd
      $ \(x, y) -> (x, y + 5)

handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds)
      playerInd
      $ \(x, y) -> (x, y - 5)

handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds)
      playerInd
      $ \(x, y) -> (x + 5, y)

handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyUp) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds)
      playerInd
      $ \(x, y) -> (x - 5, y)

------------------------------------------ On Down pressed ------------------------------------------
handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.UpDir) crds)
      playerInd
      $ \(x, y) -> (x, y - 2)

handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.DownDir) crds)
      playerInd
      $ \(x, y) -> (x, y + 2)

handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.RightDir) crds)
      playerInd
      $ \(x, y) -> (x - 2, y)

handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyDown) Up _ _)
  (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds) =
    handleMovementEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) (Just GameState.LeftDir) crds)
      playerInd
      $ \(x, y) -> (x + 2, y)

------------------------------------------ On Left/Right pressed ------------------------------------------
handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyRight) Up _ _)
  (gameState, PlayerState (Just playerInd) dir crds) =
    handleDirectionEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) dir crds)
      playerInd
      $ onLeftRightEvent KeyRight

handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeyLeft) Up _ _)
  (gameState, PlayerState (Just playerInd) dir crds) =
    handleDirectionEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) dir crds)
      playerInd
      $ onLeftRightEvent KeyLeft

------------------------------------------ On Shoot pressed ------------------------------------------
handleGameEvents
  requestChan
  curSysStateHolder
  (EventKey (SpecialKey KeySpace) Up _ _)
  (gameState, PlayerState (Just playerInd) dir crds) =
    handleShootEvent
      requestChan
      curSysStateHolder
      (gameState, PlayerState (Just playerInd) dir crds)
      playerInd
      newShotOnNothing
  where
    newShotOnNothing mbShotData = case mbShotData of
      Nothing       -> ShotData playerInd (Maybe.fromJust crds) (Maybe.fromJust dir)
      Just shotData -> shotData

handleGameEvents _ _ _ state = state

-- | Updates shot position on single time unit passed
updateShot :: Maybe ShotData -> Either ShotData UpdateShotError
updateShot Nothing = Right GameState.NoShot
updateShot (Just (ShotData playerInd (x, y) dir)) =
  if beyoundBorders (x, y) then Right GameState.OutOfBorders
  else case dir of
    GameState.UpDir    -> Left (ShotData playerInd (x, y + 5) dir)
    GameState.DownDir  -> Left (ShotData playerInd (x, y - 5) dir)
    GameState.RightDir -> Left (ShotData playerInd (x + 5, y) dir)
    GameState.LeftDir  -> Left (ShotData playerInd (x - 5, y) dir)

-- | Updates system's state by fetching states
--   from the server and move shots further
updateGame ::
  TChan SystemState                 -- ^ an event queue that updates game from server requests
  -> TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState               -- ^ atomic holder for a current system's state
  -> Float                          -- ^ current time passed from the start (unused)
  -> SystemState                    -- ^ current system's state
  -> SystemState
updateGame eventQueue requestChan curSysStateHolder _
  (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 1) dir crds) =
  case fetchedState of
    (GameState [(_, _, fP1Shot), (_, _, fP2Shot)], _) -> do
      let newP1Shot = updateShot fP1Shot
      updateGame1_ requestChan curSysStateHolder fetchedState newP1Shot fP2Shot
    state -> state

  where
    curPlayerState = PlayerState (Just 1) dir crds
    curState = (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], curPlayerState)
    fetchedState = Maybe.fromMaybe curState $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

updateGame eventQueue requestChan curSysStateHolder _
  (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 2) dir crds) =
  case fetchedState of
    (GameState [(_, _, fP1Shot), (_, _, fP2Shot)], _) -> do
      let newP2Shot = updateShot fP2Shot
      updateGame2_ requestChan curSysStateHolder fetchedState fP1Shot newP2Shot
    state -> state

  where
    curPlayerState = PlayerState (Just 2) dir crds
    curState = (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], curPlayerState)
    fetchedState = Maybe.fromMaybe curState $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

updateGame eventQueue _ _ _ curState =
  Maybe.fromMaybe curState $ unsafePerformIO $ STM.atomically $ STM.tryReadTChan eventQueue

-- | Updates 1-st player's shot state and sends it to the server
updateGame1_ ::
  TChan GameState.PlayerRequests     -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState                -- ^ atomic holder for a current system's state
  -> SystemState                     -- ^ fetched system's state
  -> Either ShotData UpdateShotError -- ^ 1-st player's shot data, updated after a time
  -> Maybe ShotData                  -- ^ current 2-nd player's shot data
  -> SystemState
updateGame1_
  requestChan
  curSysStateHolder
  (GameState [(Just fP1Dir, Just fP1Crds, _), (Just fP2Dir, Just fP2Crds, _)], _)
  (Left newP1Shot)
  newP2Shot = unsafePerformIO
    $ wrapP1ShotWithIO requestChan curSysStateHolder fP1Dir fP2Dir fP1Crds fP2Crds newP1Shot newP2Shot
    $ GameState.Shoot newP1Shot

updateGame1_
  requestChan
  curSysStateHolder
  (GameState [(Just fP1Dir, Just fP1Crds, _), (Just fP2Dir, Just fP2Crds, _)], _)
  (Right GameState.OutOfBorders)
  newP2Shot = unsafePerformIO
    $ wrapP1CancelShotWithIO requestChan curSysStateHolder fP1Dir fP2Dir fP1Crds fP2Crds newP2Shot
    $ GameState.CancelShoot 1

updateGame1_ _ _ fetchedState _ _ = fetchedState

-- | Updates 2-nd player's shot state and sends it to the server
updateGame2_ ::
  TChan GameState.PlayerRequests     -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState                -- ^ atomic holder for a current system's state
  -> SystemState                     -- ^ fetched system's state
  -> Maybe ShotData                  -- ^ current 1-st player's shot data
  -> Either ShotData UpdateShotError -- ^ 2-nd player's shot data, updated after a time
  -> SystemState
updateGame2_
  requestChan
  curSysStateHolder
  (GameState [(Just fP1Dir, Just fP1Crds, _), (Just fP2Dir, Just fP2Crds, _)], _)
  newP1Shot
  (Left newP2Shot) = unsafePerformIO
    $ wrapP2ShotWithIO requestChan curSysStateHolder fP1Dir fP2Dir fP1Crds fP2Crds newP1Shot newP2Shot
    $ GameState.Shoot newP2Shot

updateGame2_
  requestChan
  curSysStateHolder
  (GameState [(Just fP1Dir, Just fP1Crds, _), (Just fP2Dir, Just fP2Crds, _)], _)
  newP1Shot
  (Right GameState.OutOfBorders) = unsafePerformIO
    $ wrapP2CancelShotWithIO requestChan curSysStateHolder fP1Dir fP2Dir fP1Crds fP2Crds newP1Shot
    $ GameState.CancelShoot 2

updateGame2_ _ _ fetchedState _ _ = fetchedState

-- | Launches game rendering and update handling
launchGame ::
  TChan SystemState                 -- ^ an event queue that updates game from server requests
  -> TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState               -- ^ atomic holder for a current system's state
  -> Picture                        -- ^ first player picture (already rotated and translated)
  -> Picture                        -- ^ second player picture (already rotated and translated)
  -> IO ()
launchGame eventQueue requestChan curSysStateHolder firstPlayerPic secondPlayerPic = Gloss.play
  mainWindow
  white
  fps
  initialSystemState
  (renderGame firstPlayerPic secondPlayerPic)
  (handleGameEvents requestChan curSysStateHolder)
  (updateGame eventQueue requestChan curSysStateHolder)

main :: IO ()
main = do
    eventQueue <- STM.atomically STM.newTChan
    requestChan <- STM.atomically STM.newTChan
    curSysStateHolder <- STM.newTVarIO initialSystemState

    firstPlayerTank <- loadBMP "./green_tank.bmp"
    secondPlayerTank <- loadBMP "./blue_tank.bmp"
    let firstPlayerPic = scale 0.1 0.1 firstPlayerTank
    let secondPlayerPic = scale 0.1 0.1 secondPlayerTank

    _ <- Async.concurrently
      (launchClientHandler eventQueue requestChan curSysStateHolder)
      $ launchGame eventQueue requestChan curSysStateHolder firstPlayerPic secondPlayerPic

    return ()
