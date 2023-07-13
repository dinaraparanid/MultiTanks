module Server where

import           Control.Concurrent.Async  as Async
import           Data.ByteString           as Bytes
import           Data.Word                 (Word8)
import           GameState
import           Network.Socket
import           Network.Socket.ByteString as NetworkBytes
import           Utils

-- | Client/Server description:
-- Max 2 players
--
-- 1)
--    player 1 sends 0 - connect
--    server sends 6 - player1 found
-- 2)
--    player 2 sends 0 - connect
--    server sends 7 - player2 found
--    server sends 1 - set init position and start game
-- 3)
--    player sends 2 - upd move
--    server sends 1 - change move
-- 4)
--    player sends 8 - upd direction
--    server sends 9 - change direction
-- 5)
--    player sends 3 - shoot
--    ? server sends 4 - shot traectory, game continues
--    ? server sends 5 - kill, game finishes
-- 6)
--    player sends 10 - shoot done
--    server sends 11 - no shoot

-- | Parses bytes from datagram as a player request.
--   Supported formats described in the beggining of the file
parsePlayerRequest :: [Word8] -> Maybe GameState.PlayerRequests
parsePlayerRequest byteStr =
  case firstOrNothing byteStr of
    Just 0  -> Just GameState.Connect
    Just 2  -> GameState.UpdatePosition <$> Utils.bytesToUpdatePosition byteStr
    Just 8  -> GameState.UpdateDirection <$> Utils.bytesToUpdateDirection byteStr
    Just 3  -> GameState.Shoot <$> Utils.bytesToShotData byteStr
    Just 10 -> GameState.CancelShoot <$> Utils.bytesToCancelShoot byteStr
    _       -> Nothing

-- | Either a game state with the next
--   request to players, or an error message
type HandlePlayerRequestResult = Either (GameState, Maybe GameState.ServerRequests) String

-------------------------------- Event Handlers --------------------------------

-- | Updates current game's state with the given position's update
handleUpdatePositionRequest :: GameState -> UpdatePositionData -> HandlePlayerRequestResult
handleUpdatePositionRequest
  (GameState [(p1Addr, p1Dir, _, p1Shot), (p2Addr, p2Dir, p2Coords, p2Shot)])
  (UpdatePositionData 1 newP1Coords) = result
  where
    firstPlayerState = (p1Addr, p1Dir, newP1Coords, p1Shot)
    secondPlayerState = (p2Addr, p2Dir, p2Coords, p2Shot)
    changePosState = GameState [firstPlayerState, secondPlayerState]
    changePosRequest = Just $ GameState.ChangePosition $ UpdatePositionData 1 newP1Coords
    result = Left (changePosState, changePosRequest)

handleUpdatePositionRequest
  (GameState [(p1Addr, p1Dir, p1Coords, p1Shot), (p2Addr, p2Dir, _, p2Shot)])
  (UpdatePositionData 2 newP2Coords) = result
  where
    firstPlayerState = (p1Addr, p1Dir, p1Coords, p1Shot)
    secondPlayerState = (p2Addr, p2Dir, newP2Coords, p2Shot)
    changePosState = GameState [firstPlayerState, secondPlayerState]
    changePosRequest = Just $ GameState.ChangePosition $ UpdatePositionData 2 newP2Coords
    result = Left (changePosState, changePosRequest)

handleUpdatePositionRequest _ _ = Right "unknown update position request pattern"

-- | Updates current game's state with the given direction's update
handleUpdateDirectionRequest :: GameState -> UpdateDirectionData -> HandlePlayerRequestResult
handleUpdateDirectionRequest
  (GameState [(p1Addr, _, p1Coords, p1Shot), (p2Addr, p2Dir, p2Coords, p2Shot)])
  (UpdateDirectionData 1 newP1Dir) = result
  where
    firstPlayerState = (p1Addr, newP1Dir, p1Coords, p1Shot)
    secondPlayerState = (p2Addr, p2Dir, p2Coords, p2Shot)
    changeDirState = GameState [firstPlayerState, secondPlayerState]
    changeDirRequest = Just $ GameState.ChangeDirection $ UpdateDirectionData 1 newP1Dir
    result = Left (changeDirState, changeDirRequest)

handleUpdateDirectionRequest
  (GameState [(p1Addr, p1Dir, p1Coords, p1Shot), (p2Addr, _, p2Coords, p2Shot)])
  (UpdateDirectionData 2 newP2Dir) = result
  where
    firstPlayerState = (p1Addr, p1Dir, p1Coords, p1Shot)
    secondPlayerState = (p2Addr, newP2Dir, p2Coords, p2Shot)
    changeDirState = GameState [firstPlayerState, secondPlayerState]
    changeDirRequest = Just $ GameState.ChangeDirection $ UpdateDirectionData 2 newP2Dir
    result = Left (changeDirState, changeDirRequest)

handleUpdateDirectionRequest _ _ = Right "unknown update direction request pattern"

-- | Produces new game state,
--   when player's request is received
handlePlayerRequest ::
  GameState                         -- ^ current game state
  -> SockAddr                       -- ^ sender's (player's) address
  -> Maybe GameState.PlayerRequests -- ^ player's request
  -> HandlePlayerRequestResult
handlePlayerRequest state _ Nothing = Left (state, Nothing)

----------------------------------------- Player connections -----------------------------------------
handlePlayerRequest (GameState []) addr (Just GameState.Connect) = result
  where
    firstPlayerState = (addr, firstPlayerInitDirection, firstPlayerInitPosition, Nothing)
    firstPlayerFoundState = GameState [firstPlayerState]
    assignFirstPlayerRequest = Just GameState.AssignFirstPlayer
    result = Left (firstPlayerFoundState, assignFirstPlayerRequest)

handlePlayerRequest (GameState [player1]) addr (Just GameState.Connect) = result
  where
    secondPlayerState = (addr, secondPlayerInitDirection, secondPlayerInitPosition, Nothing)
    secondPlayerFoundState = GameState [player1, secondPlayerState]
    assignSecondPlayerRequest = Just GameState.AssignSecondPlayer
    result = Left (secondPlayerFoundState, assignSecondPlayerRequest)

handlePlayerRequest state _ (Just GameState.Connect) = Left (state, Nothing)

----------------------------------------- Update position/direction -----------------------------------------
handlePlayerRequest state _ (Just (GameState.UpdatePosition (UpdatePositionData playerInd newCoords))) =
  handleUpdatePositionRequest state $ UpdatePositionData playerInd newCoords

handlePlayerRequest state _ (Just (GameState.UpdateDirection (UpdateDirectionData playerInd newDir))) =
  handleUpdateDirectionRequest state $ UpdateDirectionData playerInd newDir

----------------------------------------- Shoot and kill -----------------------------------------
handlePlayerRequest
  (GameState [(p1Addr, p1Dir, (p1X, p1Y), p1Shot), (p2Addr, p2Dir, p2Crds, p2Shot)]) _
  (Just (GameState.Shoot (ShotData 2 (x, y) dir))) =
    if abs (p1X - x) < 25 && abs (p1Y - y) < 25 then gameOverState else shotResultState
  where
    p1Crds = (p1X, p1Y)
    firstPlayerState = (p1Addr, p1Dir, p1Crds, p1Shot)
    secondPlayerState = (p2Addr, p2Dir, p2Crds, p2Shot)
    curState = GameState [firstPlayerState, secondPlayerState]
    gameOverState = Left (curState, Just $ GameState.Kill 1)
    crds = (x, y)
    newP2Shot = ShotData 2 crds dir
    shotRequest = Just (GameState.Shot newP2Shot)
    shotState = GameState [firstPlayerState, (p2Addr, p2Dir, p2Crds, Just newP2Shot)]
    shotResultState = Left (shotState, shotRequest)

handlePlayerRequest
  (GameState [(p1Addr, p1Dir, p1Crds, p1Shot), (p2Addr, p2Dir, (p2X, p2Y), p2Shot)]) _
  (Just (GameState.Shoot (ShotData 1 (x, y) dir))) =
    if abs (p2X - x) < 25 && abs (p2Y - y) < 25 then gameOverState else shotResultState
  where
    p2Crds = (p2X, p2Y)
    firstPlayerState = (p1Addr, p1Dir, p1Crds, p1Shot)
    secondPlayerState = (p2Addr, p2Dir, p2Crds, p2Shot)
    curState = GameState [firstPlayerState, secondPlayerState]
    gameOverState = Left (curState, Just $ GameState.Kill 2)
    crds = (x, y)
    newP1Shot = ShotData 1 crds dir
    shotRequest = Just (GameState.Shot newP1Shot)
    shotState = GameState [(p1Addr, p1Dir, p1Crds, Just newP1Shot), secondPlayerState]
    shotResultState = Left (shotState, shotRequest)

handlePlayerRequest
  (GameState [(p1Addr, p1Dir, p1Crds, _), (p2Addr, p2Dir, p2Crds, p2Shot)]) _
  (Just (GameState.CancelShoot 1)) = result
  where
    removeShotRequest = Just $ GameState.RemoveShoot 1
    firstPlayerState = (p1Addr, p1Dir, p1Crds, Nothing)
    secondPlayerState = (p2Addr, p2Dir, p2Crds, p2Shot)
    shotCanceledState = GameState [firstPlayerState, secondPlayerState]
    result = Left (shotCanceledState, removeShotRequest)

handlePlayerRequest
  (GameState [(p1Addr, p1Dir, p1Crds, p1Shot), (p2Addr, p2Dir, p2Crds, _)]) _
  (Just (GameState.CancelShoot 2)) = result
  where
    removeShotRequest = Just $ GameState.RemoveShoot 2
    firstPlayerState = (p1Addr, p1Dir, p1Crds, p1Shot)
    secondPlayerState = (p2Addr, p2Dir, p2Crds, Nothing)
    shotCanceledState = GameState [firstPlayerState, secondPlayerState]
    result = Left (shotCanceledState, removeShotRequest)

handlePlayerRequest state _ _ = Left (state, Nothing)

-------------------------------- Data Senders --------------------------------

-- | Sends request to players
sendRequest ::
  Socket                            -- ^ sender socket
  -> GameState                      -- ^ current game state
  -> Maybe GameState.ServerRequests -- ^ server's request to send
  -> IO ()
sendRequest sock (GameState [(addr, _, _, _)]) (Just GameState.AssignFirstPlayer) = sendAssignFirstPlayer sock addr

sendRequest
  sock
  (GameState [(p1Addr, p1Dir, _, _), (p2Addr, p2Dir, _, _)])
  (Just GameState.AssignSecondPlayer) = do
    sendAssignSecondPlayer sock p2Addr
    sendChangeDirection sock p1Addr 2 p2Dir
    sendChangeDirection sock p2Addr 1 p1Dir

sendRequest
  sock
  (GameState [(p1Addr, _, _, _), (p2Addr, _, _, _)])
  (Just (GameState.ChangePosition (UpdatePositionData playerInd crds))) = case playerInd of
    1 -> sendChangePosition sock p2Addr 1 crds
    2 -> sendChangePosition sock p1Addr 2 crds
    _ -> return ()

sendRequest
  sock
  (GameState [(p1Addr, _, _, _), (p2Addr, _, _, _)])
  (Just (GameState.ChangeDirection (UpdateDirectionData playerInd dir))) = case playerInd of
    1 -> sendChangeDirection sock p2Addr 1 dir
    2 -> sendChangeDirection sock p1Addr 2 dir
    _ -> return ()

sendRequest
  sock
  (GameState [(p1Addr, _, _, _), (p2Addr, _, _, _)])
  (Just (GameState.Shot (ShotData playerInd crds dir))) = case playerInd of
    1 -> sendShot sock p2Addr $ ShotData 1 crds dir
    2 -> sendShot sock p1Addr $ ShotData 2 crds dir
    _ -> return ()

sendRequest
  sock
  (GameState [(p1Addr, _, _, _), (p2Addr, _, _, _)])
  (Just (GameState.RemoveShoot playerInd)) = case playerInd of
    1 -> sendRemoveShot sock p2Addr 1
    2 -> sendRemoveShot sock p1Addr 2
    _ -> return ()

sendRequest
  sock
  (GameState [(p1Addr, _, _, _), (p2Addr, _, _, _)])
  (Just (GameState.Kill playerInd)) = do
    sendKill sock p1Addr playerInd
    sendKill sock p2Addr playerInd

sendRequest _ _ _ = return ()

-- | Sends request to the client to register as the first player
sendAssignFirstPlayer ::
  Socket      -- ^ sender socket
  -> SockAddr -- ^ client's (player's) address
  -> IO ()
sendAssignFirstPlayer sock addr = sendAllTo sock (Bytes.pack [6]) addr >> putStrLn "First player found"

-- | Sends request to the client to register as the second player
sendAssignSecondPlayer ::
  Socket      -- ^ sender socket
  -> SockAddr -- ^ client's (player's) address
  -> IO ()
sendAssignSecondPlayer sock addr = sendAllTo sock (Bytes.pack [7]) addr >> putStrLn "Second player found"

-- | Sends request to update other player's position.
--   Note that this function should notify 1-st player
--   about new positions of the 2-nd player and vise versa
sendChangePosition ::
  Socket         -- ^ sender socket
  -> SockAddr    -- ^ client's (player's) address
  -> Word8       -- ^ index of player with an updated position
  -> Coordinates -- ^ new position
  -> IO ()
sendChangePosition sock addr playerInd (x, y) = do
  let commandBytes = Bytes.pack [1]
  let playerIndBytes = Bytes.pack [playerInd]
  let xBytes = Utils.intToByteString x
  let yBytes = Utils.intToByteString y
  let msg = commandBytes <> playerIndBytes <> xBytes <> yBytes
  sendAllTo sock msg addr >> putStrLn "New position is sent"

-- | Sends request to update other player's direction.
--   Note that this function should notify 1-st player
--   about new directions of the 2-nd player and vise versa
sendChangeDirection ::
  Socket       -- ^ sender socket
  -> SockAddr  -- ^ client's (player's) address
  -> Word8     -- ^ index of player with an updated position
  -> Direction -- ^ new direction
  -> IO ()
sendChangeDirection sock addr playerInd dir = do
  let commandBytes = Bytes.pack [9]
  let playerIndBytes = Bytes.pack [playerInd]
  let dirBytes = Bytes.pack [Utils.directionToByte dir]
  let msg = commandBytes <> playerIndBytes <> dirBytes
  sendAllTo sock msg addr >> putStrLn "New direction is sent"

-- | Sends request to update other player's shot state.
--   Note that this function should notify 1-st player
--   about the new shot state of the 2-nd player and vise versa
sendShot ::
  Socket      -- ^ sender socket
  -> SockAddr -- ^ client's (player's) address
  -> ShotData -- ^ new shot data
  -> IO ()
sendShot sock addr (ShotData playerInd (x, y) dir) = do
  let commandBytes = Bytes.pack [4]
  let playerIndBytes = Bytes.pack [playerInd]
  let xBytes = Utils.intToByteString x
  let yBytes = Utils.intToByteString y
  let dirBytes = Bytes.pack [Utils.directionToByte dir]
  let msg = commandBytes <> playerIndBytes <> xBytes <> yBytes <> dirBytes
  sendAllTo sock msg addr >> putStrLn "Player has shot, no one is killed"

-- | Sends request to update other player's killed state.
--   Note that this function should notify 1-st player
--   about the killed state of the 2-nd player and vise versa
sendKill ::
  Socket      -- ^ sender socket
  -> SockAddr -- ^ client's (player's) address
  -> Word8    -- ^ dead player's index
  -> IO ()
sendKill sock addr playerInd = do
  let commandBytes = Bytes.pack [5]
  let playerIndBytes = Bytes.pack [playerInd]
  let msg = commandBytes <> playerIndBytes
  sendAllTo sock msg addr >> putStrLn "Player is killed, game finished"

-- | Sends request to remove player's shot state.
--   Note that this function should notify 1-st player
--   about the cancelations of shot of the 2-nd player and vise versa
sendRemoveShot ::
  Socket      -- ^ sender socket
  -> SockAddr -- ^ client's (player's) address
  -> Word8    -- ^ dead player's index
  -> IO ()
sendRemoveShot sock addr playerInd = do
  let commandBytes = Bytes.pack [11]
  let playerIndBytes = Bytes.pack [playerInd]
  let msg = commandBytes <> playerIndBytes
  sendAllTo sock msg addr >> putStrLn "Shot is removed"

-------------------------------- Server Runners --------------------------------

-- | Starts an observer that fetches players' requests
launchGameEventLoop ::
  Socket       -- ^ sender socket
  -> GameState -- ^ current game's state
  -> IO ()
launchGameEventLoop sock state = do
  (byteStr, addr) <- NetworkBytes.recvFrom sock 4096
  print state
  Async.withAsync (handleRequest byteStr addr) Async.wait

  where
    playerRequest byteStr = parsePlayerRequest $ Bytes.unpack byteStr
    handleResult byteStr addr = handlePlayerRequest state addr $ playerRequest byteStr
    restart = launchGameEventLoop sock initialGameState
    continue = launchGameEventLoop sock
    handleRequest byteStr addr = case handleResult byteStr addr of
      Left (newState, request) -> do
        print newState
        print request
        sendRequest sock newState request

        case request of
          Just (GameState.Kill _) -> restart
          _                       -> continue newState

      Right err -> do
        putStrLn err
        continue state

-- | Binds socket to the server and launches it on http://0.0.0.0:8080
launchUDPServerForever :: IO ()
launchUDPServerForever = do
  addrInfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      bind sock $ addrAddress serverAddr
      putStrLn "Server is launched"
      launchGameEventLoop sock initialGameState

main :: IO ()
main = launchUDPServerForever
