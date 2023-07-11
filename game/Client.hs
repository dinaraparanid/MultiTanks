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

parseServerRequest :: [Word8] -> Maybe GameState.ServerRequests
parseServerRequest byteStr =
  case firstOrNothing byteStr of
    Just 6 -> Just GameState.AssignFirstPlayer
    Just 7 -> Just GameState.AssignSecondPlayer
    Just 1 -> GameState.ChangePosition <$> Utils.bytesToChangePosition byteStr
    Just 9 -> GameState.ChangeDirection <$> Utils.bytesToChangeDirection byteStr
    Just 4 -> GameState.Shot <$> Utils.bytesToShotData byteStr
    _      -> Nothing

handleServerRequest ::
  SystemState
  -> Maybe GameState.ServerRequests
  -> SystemState
--------------------------------- Assign player ---------------------------------
handleServerRequest (gameState, PlayerState Nothing Nothing Nothing) (Just GameState.AssignFirstPlayer) =
  (gameState, PlayerState (Just 1) Nothing Nothing)
handleServerRequest state (Just GameState.AssignFirstPlayer) = state

handleServerRequest (gameState, PlayerState Nothing Nothing Nothing) (Just GameState.AssignSecondPlayer) =
  (gameState, PlayerState (Just 2) Nothing Nothing)
handleServerRequest state (Just GameState.AssignSecondPlayer) = state

--------------------------------- Change position ---------------------------------
handleServerRequest
  (GameState [(p1Dir, _, p1Shot), (p2Dir, _, p2Shot)], PlayerState (Just 1) _ _)
  (Just (GameState.ChangePosition (ChangePositionData p1Crds p2Crds))) =
    (GameState
      [(p1Dir, Just p1Crds, p1Shot), (p2Dir, Just p2Crds, p2Shot)]
      , PlayerState (Just 1) p1Dir (Just p1Crds)
      )

handleServerRequest
  (GameState [(p1Dir, _, p1Shot), (p2Dir, _, p2Shot)], PlayerState (Just 2) _ _)
  (Just (GameState.ChangePosition (ChangePositionData p1Crds p2Crds))) =
    (GameState
      [(p1Dir, Just p1Crds, p1Shot), (p2Dir, Just p2Crds, p2Shot)]
      , PlayerState (Just 2) p2Dir (Just p2Crds)
      )

--------------------------------- Change direction ---------------------------------
handleServerRequest
  (GameState [], PlayerState (Just 1) _ _)
  (Just (GameState.ChangeDirection (ChangeDirectionData p1Dir p2Dir))) =
    (GameState
      [(Just p1Dir, Nothing, Nothing), (Just p2Dir, Nothing, Nothing)]
      , PlayerState (Just 1) (Just p1Dir) Nothing
      )

handleServerRequest
  (GameState [], PlayerState (Just 2) _ _)
  (Just (GameState.ChangeDirection (ChangeDirectionData p1Dir p2Dir))) =
    (GameState
      [(Just p1Dir, Nothing, Nothing), (Just p2Dir, Nothing, Nothing)]
      , PlayerState (Just 2) (Just p2Dir) Nothing
      )

handleServerRequest
  (GameState [(_, p1Crds, p1Shot), (_, p2Crds, p2Shot)], PlayerState (Just 1) _ _)
  (Just (GameState.ChangeDirection (ChangeDirectionData p1Dir p2Dir))) =
    (GameState
      [(Just p1Dir, p1Crds, p1Shot), (Just p2Dir, p2Crds, p2Shot)]
      , PlayerState (Just 1) (Just p1Dir) p1Crds
      )

handleServerRequest
  (GameState [(_, p1Crds, p1Shot), (_, p2Crds, p2Shot)], PlayerState (Just 2) _ _)
  (Just (GameState.ChangeDirection (ChangeDirectionData p1Dir p2Dir))) =
    (GameState
      [(Just p1Dir, p1Crds, p1Shot), (Just p2Dir, p2Crds, p2Shot)]
      , PlayerState (Just 2) (Just p2Dir) p2Crds
      )

--------------------------------- Shot ---------------------------------
handleServerRequest
  (GameState [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, _)], PlayerState (Just 1) _ _)
  (Just (GameState.Shot (ShotData 2 crds dir))) =
    (GameState
      [(p1Dir, p1Crds, p1Shot), (p2Dir, p2Crds, Just (ShotData 2 crds dir))]
      , PlayerState (Just 1) p1Dir p1Crds
      )

handleServerRequest
  (GameState [(p1Dir, p1Crds, _), (p2Dir, p2Crds, p2Shot)], PlayerState (Just 2) _ _)
  (Just (GameState.Shot (ShotData 1 crds dir))) =
    (GameState
      [(p1Dir, p1Crds, Just (ShotData 1 crds dir)), (p2Dir, p2Crds, p2Shot)]
      , PlayerState (Just 2) p1Dir p1Crds
      )

--------------------------------- Kill ---------------------------------
handleServerRequest _ (Just (GameState.Kill _)) = initialSystemState

handleServerRequest state _ = state

sendRequest :: Socket -> SockAddr -> GameState.PlayerRequests -> IO ()
sendRequest sock addr GameState.Connect = sendConnect sock addr
sendRequest sock addr (GameState.UpdatePosition (UpdatePositionData playerInd coords)) =
  sendUpdatePosition sock addr playerInd coords
sendRequest sock addr (GameState.UpdateDirection (UpdateDirectionData playerInd dir)) =
  sendUpdateDirection sock addr playerInd dir
sendRequest sock addr (GameState.Shoot (ShotData playerInd curPos dir)) =
  sendShoot sock addr playerInd curPos dir

sendConnect :: Socket -> SockAddr -> IO ()
sendConnect sock addr = sendAllTo sock (Bytes.pack [0]) addr >> putStrLn "Connect is sent"

sendUpdatePosition :: Socket -> SockAddr -> Word8 -> (Int, Int) -> IO ()
sendUpdatePosition sock addr playerInd (x, y) = do
  let commandBytes = Bytes.pack [2]
  let playerIndBytes = Bytes.pack [playerInd]
  let xBytes = Utils.intToByteString x
  let yBytes = Utils.intToByteString y
  let msg = commandBytes <> playerIndBytes <> xBytes <> yBytes
  sendAllTo sock msg addr >> putStrLn "New position is sent"

sendUpdateDirection :: Socket -> SockAddr -> Word8 -> Direction -> IO ()
sendUpdateDirection sock addr playerInd dir = do
  let commandBytes = Bytes.pack [8]
  let playerIndBytes = Bytes.pack [playerInd]
  let dirBytes = Bytes.pack [Utils.directionToByte dir]
  let msg = commandBytes <> playerIndBytes <> dirBytes
  sendAllTo sock msg addr >> putStrLn "New direction is sent"

sendShoot :: Socket -> SockAddr -> Word8 -> Coordinates -> Direction -> IO ()
sendShoot sock addr playerInd (x, y) dir = do
  let commandBytes = Bytes.pack [3]
  let playerIndBytes = Bytes.pack [playerInd]
  let xBytes = Utils.intToByteString x
  let yBytes = Utils.intToByteString y
  let dirBytes = Bytes.pack [Utils.directionToByte dir]
  let msg = commandBytes <> playerIndBytes <> xBytes <> yBytes <> dirBytes
  sendAllTo sock msg addr >> putStrLn "Shoot is sent"

launchClientEventLoop :: TChan SystemState -> Socket -> SystemState -> IO ()
launchClientEventLoop gameEventQueue sock state = do
  response <- NetworkBytes.recvFrom sock 4096

  let (byteStr, _) = response
  print $ Bytes.unpack byteStr

  let newState = handleServerRequest state $ parseServerRequest $ Bytes.unpack byteStr
  atomically $ writeTChan gameEventQueue newState
  print newState

  case newState of
    (_, PlayerState Nothing Nothing Nothing) -> close sock -- end of game
    _                                        -> launchClientEventLoop gameEventQueue sock newState

launchRequestReceiver :: TChan GameState.PlayerRequests -> Socket -> SockAddr -> IO ()
launchRequestReceiver requestChan sock addr = do
  request <- atomically $ readTChan requestChan
  print request
  sendRequest sock addr request
  launchRequestReceiver requestChan sock addr

launchClientHandler :: TChan SystemState -> TChan GameState.PlayerRequests -> IO ()
launchClientHandler gameEventQueue requestChan = do
  addrInfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      let addr = addrAddress serverAddr
      sendConnect sock addr
      race_
        (launchClientEventLoop gameEventQueue sock initialSystemState)
        (launchRequestReceiver requestChan sock addr)
