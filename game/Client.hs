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
--    player 1 send 0 - connect
--    player 2 send 0 - connect
--    server sends 1 - upd move or game start
-- 2)
--    player sends 2 - change move
--    server sends 1 - upd move
-- 3)
--    player sends 3 - shoot
--    ? server sends 4 - shot traectory, game continues
--    ? server sends 5 - kill, game finishes

parseServerRequest :: [Word8] -> Maybe GameState.ServerRequests
parseServerRequest byteStr =
  case firstOrNothing byteStr of
    Just 6 -> Just GameState.AssignFirstPlayer
    Just 7 -> Just GameState.AssignSecondPlayer
    Just 1 -> GameState.ChangePosition <$> Utils.bytesToChangePosition byteStr
    Just 4 -> GameState.Shot <$> Utils.bytesToShotData byteStr
    _      -> Nothing

handleServerRequest ::
  SystemState
  -> Maybe GameState.ServerRequests
  -> SystemState
handleServerRequest (gameState, PlayerState Nothing Nothing) (Just GameState.AssignFirstPlayer) =
  (gameState, PlayerState (Just 1) Nothing)
handleServerRequest state (Just GameState.AssignFirstPlayer) = state

handleServerRequest (gameState, PlayerState Nothing Nothing) (Just GameState.AssignSecondPlayer) =
  (gameState, PlayerState (Just 2) Nothing)
handleServerRequest state (Just GameState.AssignSecondPlayer) = state

handleServerRequest
  (_, PlayerState (Just 1) _)
  (Just (GameState.ChangePosition (ChangePositionData p1Crds p2Crds))) =
  (GameState [p1Crds, p2Crds], PlayerState (Just 1) $ Just p1Crds)

handleServerRequest
  (_, PlayerState (Just 2) _)
  (Just (GameState.ChangePosition (ChangePositionData p1Crds p2Crds))) =
  (GameState [p1Crds, p2Crds], PlayerState (Just 2) $ Just p2Crds)

handleServerRequest state (Just (GameState.Shot _)) = state

handleServerRequest _ (Just (GameState.Kill _)) = initialSystemState

handleServerRequest state _ = state

sendRequest :: Socket -> SockAddr -> GameState.PlayerRequests -> IO ()
sendRequest sock addr GameState.Connect = sendConnect sock addr
sendRequest sock addr (GameState.Shoot (ShotData playerInd (start, end))) =
  sendShoot sock addr playerInd start end
sendRequest sock addr (GameState.UpdatePosition (UpdatePositionData playerInd coords)) =
  sendUpdatePosition sock addr playerInd coords

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

sendShoot :: Socket -> SockAddr -> Word8 -> (Int, Int) -> (Int, Int) -> IO ()
sendShoot sock addr playerInd (xStart, yStart) (xEnd, yEnd) = do
  let commandBytes = Bytes.pack [3]
  let playerIndBytes = Bytes.pack [playerInd]
  let xStartBytes = Utils.intToByteString xStart
  let yStartBytes = Utils.intToByteString yStart
  let xEndBytes = Utils.intToByteString xEnd
  let yEndBytes = Utils.intToByteString yEnd
  let msg = commandBytes <> playerIndBytes <> xStartBytes <> yStartBytes <> xEndBytes <> yEndBytes
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
    (_, PlayerState Nothing Nothing) -> close sock -- end of game
    _                                -> launchClientEventLoop gameEventQueue sock newState

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
