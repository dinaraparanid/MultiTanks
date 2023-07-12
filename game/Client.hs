module Client where

import           Control.Concurrent.Async  as Async
import           Control.Concurrent.STM
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

-- | Parses bytes from datagram as a server request.
--   Supported formats described in the beggining of the file
parseServerRequest :: [Word8] -> Maybe GameState.ServerRequests
parseServerRequest byteStr =
  case firstOrNothing byteStr of
    Just 6 -> Just GameState.AssignFirstPlayer
    Just 7 -> Just GameState.AssignSecondPlayer
    Just 1 -> GameState.ChangePosition <$> Utils.bytesToChangePosition byteStr
    Just 9 -> GameState.ChangeDirection <$> Utils.bytesToChangeDirection byteStr
    Just 4 -> GameState.Shot <$> Utils.bytesToShotData byteStr
    Just 5 -> GameState.Kill <$> Utils.bytesToKill byteStr
    Just 11 -> GameState.RemoveShoot <$> Utils.bytesToRemoveShot byteStr
    _      -> Nothing

-- | Produces new game state,
--   when server request is received
handleServerRequest ::
  SystemState                       -- ^ current system state
  -> Maybe GameState.ServerRequests -- ^ reqeust to handle
  -> SystemState
--------------------------------- Assign players ---------------------------------
handleServerRequest (gameState, PlayerState Nothing Nothing Nothing) (Just GameState.AssignFirstPlayer) =
  (gameState, PlayerState (Just 1) Nothing Nothing)
handleServerRequest state (Just GameState.AssignFirstPlayer) = state

handleServerRequest (gameState, PlayerState Nothing Nothing Nothing) (Just GameState.AssignSecondPlayer) =
  (gameState, PlayerState (Just 2) Nothing Nothing)
handleServerRequest state (Just GameState.AssignSecondPlayer) = state

--------------------------------- Change position ---------------------------------
handleServerRequest
  (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, _, p2Shot)], PlayerState (Just 1) _ _)
  (Just (GameState.ChangePosition (UpdatePositionData 2 p2Crds))) = systemState
  where
    changePosGameState = GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, Just p2Crds, p2Shot)]
    changePosPlayerState = PlayerState (Just 1) p1Dir p1Crds
    systemState = (changePosGameState, changePosPlayerState)

handleServerRequest
  (GameState [(p1Dir, _, p1Shot), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 2) _ _)
  (Just (GameState.ChangePosition (UpdatePositionData 1 p1Crds))) = systemState
  where
    changePosGameState = GameState [(p1Dir, Just p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)]
    changePosPlayerState = PlayerState (Just 2) p2Dir p2Crds
    systemState = (changePosGameState, changePosPlayerState)

--------------------------------- Change direction ---------------------------------
handleServerRequest
  (GameState [], PlayerState (Just 1) _ _)
  (Just (GameState.ChangeDirection (UpdateDirectionData 2 p2Dir))) = systemState
  where
    p1Dir = Just GameState.firstPlayerInitDirection
    p1Crds = Just GameState.firstPlayerInitPosition
    p2Crds = Just GameState.secondPlayerInitPosition
    firstPlayerState = (p1Dir, p1Crds, Nothing)
    secondPlayerState = (Just p2Dir, p2Crds, Nothing)
    changeDirGameState = GameState [firstPlayerState, secondPlayerState]
    changeDirPlayerState = PlayerState (Just 1) p1Dir Nothing
    systemState = (changeDirGameState, changeDirPlayerState)

handleServerRequest
  (GameState [], PlayerState (Just 2) _ _)
  (Just (GameState.ChangeDirection (UpdateDirectionData 1 p1Dir))) = systemState
  where
    p2Dir = Just GameState.secondPlayerInitDirection
    p1Crds = Just GameState.firstPlayerInitPosition
    p2Crds = Just GameState.secondPlayerInitPosition
    firstPlayerState = (Just p1Dir, p1Crds, Nothing)
    secondPlayerState = (p2Dir, p2Crds, Nothing)
    changeDirGameState = GameState [firstPlayerState, secondPlayerState]
    changeDirPlayerState = PlayerState (Just 2) p2Dir Nothing
    systemState = (changeDirGameState, changeDirPlayerState)

handleServerRequest
  (GameState [(p1Dir, p1Crds, p1Shot), (_, p2Crds, p2Shot)], PlayerState (Just 1) _ _)
  (Just (GameState.ChangeDirection (UpdateDirectionData 2 p2Dir))) = systemState
  where
    changeDirGameState = GameState [(p1Dir, p1Crds, p1Shot), (Just p2Dir, p2Crds, p2Shot)]
    changeDirPlayerState = PlayerState (Just 1) p1Dir p1Crds
    systemState = (changeDirGameState, changeDirPlayerState)

handleServerRequest
  (GameState [(_, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 2) _ _)
  (Just (GameState.ChangeDirection (UpdateDirectionData 1 p1Dir))) = systemState
  where
    changeDirGameState = GameState [(Just p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, p2Shot)]
    changeDirPlayerState = PlayerState (Just 2) p2Dir p2Crds
    systemState = (changeDirGameState, changeDirPlayerState)

--------------------------------- Shot ---------------------------------
handleServerRequest
  (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, _)], PlayerState (Just 1) _ _)
  (Just (GameState.Shot (ShotData 2 crds dir))) = systemState
  where
    shotGameState = GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, Just (ShotData 2 crds dir))]
    shotPlayerState = PlayerState (Just 1) p1Dir p1Crds
    systemState = (shotGameState, shotPlayerState)

handleServerRequest
  (GameState [(p1Dir, p1Crds, _), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 2) _ _)
  (Just (GameState.Shot (ShotData 1 crds dir))) = systemState
  where
    shotGameState = GameState [(p1Dir, p1Crds, Just (ShotData 1 crds dir)), (p2Dir, p2Crds, p2Shot)]
    shotPlayerState = PlayerState (Just 2) p1Dir p1Crds
    systemState = (shotGameState, shotPlayerState)

--------------------------------- Cancel Shot ---------------------------------
handleServerRequest
  (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, _)], PlayerState (Just 1) _ _)
  (Just (GameState.RemoveShoot 2)) = systemState
  where
    shotGameState = GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, Nothing)]
    shotPlayerState = PlayerState (Just 1) p1Dir p1Crds
    systemState = (shotGameState, shotPlayerState)

handleServerRequest
  (GameState [(p1Dir, p1Crds, _), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 2) _ _)
  (Just (GameState.RemoveShoot 1)) = systemState
  where
    shotGameState = GameState [(p1Dir, p1Crds, Nothing), (p2Dir, p2Crds, p2Shot)]
    shotPlayerState = PlayerState (Just 2) p1Dir p1Crds
    systemState = (shotGameState, shotPlayerState)

--------------------------------- Kill ---------------------------------
handleServerRequest (GameState [_, p2Data], pd) (Just (GameState.Kill 1)) =
  (GameState [(Nothing, Nothing, Nothing), p2Data], pd)
handleServerRequest (GameState [p1Data, _], pd) (Just (GameState.Kill 2)) =
  (GameState [p1Data, (Nothing, Nothing, Nothing)], pd)

handleServerRequest state _ = state

-------------------------------- Data Senders --------------------------------

-- | Sends request to the server by the given address
sendRequest :: Socket -> SockAddr -> GameState.PlayerRequests -> IO ()
sendRequest sock addr GameState.Connect = sendConnect sock addr
sendRequest sock addr (GameState.UpdatePosition (UpdatePositionData playerInd coords)) =
  sendUpdatePosition sock addr playerInd coords
sendRequest sock addr (GameState.UpdateDirection (UpdateDirectionData playerInd dir)) =
  sendUpdateDirection sock addr playerInd dir
sendRequest sock addr (GameState.Shoot (ShotData playerInd curPos dir)) =
  sendShoot sock addr playerInd curPos dir
sendRequest sock addr (GameState.CancelShoot playerInd) = sendCancelShoot sock addr playerInd

-- | Sends connect state to the server.
--   In case if two players will
--   send their connection, game will start
sendConnect :: Socket -> SockAddr -> IO ()
sendConnect sock addr = sendAllTo sock (Bytes.pack [0]) addr >> putStrLn "Connect is sent"

-- | Sends updated client's position to the server
sendUpdatePosition ::
  Socket         -- ^ sender socket
  -> SockAddr    -- ^ server addres
  -> Word8       -- ^ player index (1 or 2)
  -> Coordinates -- ^ new position
  -> IO ()
sendUpdatePosition sock addr playerInd (x, y) = do
  let commandBytes = Bytes.pack [2]
  let playerIndBytes = Bytes.pack [playerInd]
  let xBytes = Utils.intToByteString x
  let yBytes = Utils.intToByteString y
  let msg = commandBytes <> playerIndBytes <> xBytes <> yBytes
  sendAllTo sock msg addr >> putStrLn "New position is sent"

-- | Sends updated client's direction to the server
sendUpdateDirection ::
  Socket       -- ^ sender socket
  -> SockAddr  -- ^ server addres
  -> Word8     -- ^ player index (1 or 2)
  -> Direction -- ^ new direction
  -> IO ()
sendUpdateDirection sock addr playerInd dir = do
  let commandBytes = Bytes.pack [8]
  let playerIndBytes = Bytes.pack [playerInd]
  let dirBytes = Bytes.pack [Utils.directionToByte dir]
  let msg = commandBytes <> playerIndBytes <> dirBytes
  sendAllTo sock msg addr >> putStrLn "New direction is sent"

-- | Sends client's shot to the server.
--   Note that game allows only single
--   shot per moment for a player
sendShoot ::
  Socket         -- ^ sender socket
  -> SockAddr    -- ^ server addres
  -> Word8       -- ^ player index (1 or 2)
  -> Coordinates -- ^ current shot's position
  -> Direction   -- ^ shot's direction
  -> IO ()
sendShoot sock addr playerInd (x, y) dir = do
  let commandBytes = Bytes.pack [3]
  let playerIndBytes = Bytes.pack [playerInd]
  let xBytes = Utils.intToByteString x
  let yBytes = Utils.intToByteString y
  let dirBytes = Bytes.pack [Utils.directionToByte dir]
  let msg = commandBytes <> playerIndBytes <> xBytes <> yBytes <> dirBytes
  sendAllTo sock msg addr >> putStrLn "Shoot is sent"

-- | Sends request to cancel shot,
--   when it reaches borders
sendCancelShoot ::
  Socket      -- ^ sender socket
  -> SockAddr -- ^ server addres
  -> Word8    -- ^ player index (1 or 2)
  -> IO ()
sendCancelShoot sock addr playerInd = do
  let commandBytes = Bytes.pack [10]
  let playerIndBytes = Bytes.pack [playerInd]
  let msg = commandBytes <> playerIndBytes
  sendAllTo sock msg addr >> putStrLn "Shot is canceled"

-- | Launches observer that fetches requests
--   from the server and sends new game states
--   into the event queue
launchClientEventLoop ::
  TChan SystemState    -- ^ an event queue that updates game from server requests
  -> TVar SystemState  -- ^ atomic holder for a current system's state
  -> Socket            -- ^ receiver socket
  -> IO ()
launchClientEventLoop gameEventQueue curSysStateHolder sock = do
  response <- NetworkBytes.recvFrom sock 4096
  state <- readTVarIO curSysStateHolder

  let (byteStr, _) = response
  print $ Bytes.unpack byteStr

  let newState = handleServerRequest state $ parseServerRequest $ Bytes.unpack byteStr
  _ <- atomically $ writeTChan gameEventQueue newState
  _ <- atomically $ writeTVar curSysStateHolder newState
  print newState

  case newState of
    (_, PlayerState Nothing Nothing Nothing) -> close sock -- end of game
    _                                        -> launchClientEventLoop gameEventQueue curSysStateHolder sock

-- | Launches observer that receives updates from the game
--   and sends all game events to the server
launchRequestReceiver ::
  TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> Socket                      -- ^ sender socket
  -> SockAddr                    -- ^ server address
  -> IO ()
launchRequestReceiver requestChan sock addr = do
  request <- atomically $ readTChan requestChan
  print request
  sendRequest sock addr request
  launchRequestReceiver requestChan sock addr

-- | Launches observer that initialises the workflow with the server
--   and starts both game event and client/server observerment
launchClientHandler ::
  TChan SystemState                 -- ^ an event queue that updates game from server requests
  -> TChan GameState.PlayerRequests -- ^ request channel that sends data to server after callback was done
  -> TVar SystemState               -- ^ atomic holder for a current system's state
  -> IO ()
launchClientHandler gameEventQueue requestChan curSysStateHolder = do
  addrInfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      let addr = addrAddress serverAddr
      sendConnect sock addr
      race_
        (launchClientEventLoop gameEventQueue curSysStateHolder sock)
        (launchRequestReceiver requestChan sock addr)
