module Client where

import           Control.Monad             (forever)
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
  PlayerState
  -> Maybe GameState.ServerRequests
  -> PlayerState
handleServerRequest (PlayerState Nothing Nothing) (Just GameState.AssignFirstPlayer) = PlayerState (Just 1) Nothing
handleServerRequest state (Just GameState.AssignFirstPlayer) = state

handleServerRequest (PlayerState Nothing Nothing) (Just GameState.AssignSecondPlayer) = PlayerState (Just 2) Nothing
handleServerRequest state (Just GameState.AssignSecondPlayer) = state

handleServerRequest (PlayerState (Just 1) _) (Just (GameState.ChangePosition (ChangePositionData p1Crds _))) =
  PlayerState (Just 1) $ Just p1Crds

handleServerRequest (PlayerState (Just 2) _) (Just (GameState.ChangePosition (ChangePositionData _ p2Crds))) =
  PlayerState (Just 2) $ Just p2Crds

handleServerRequest state (Just (GameState.Shot _)) = state

handleServerRequest _ (Just (GameState.Kill _)) = initialPlayerState

handleServerRequest state _ = state

sendRequest :: Socket -> SockAddr -> Maybe GameState.PlayerRequests -> IO ()
sendRequest sock addr (Just GameState.Connect) = sendConnect sock addr
sendRequest sock addr (Just (GameState.Shoot (ShotData playerInd (start, end)))) = sendShoot sock addr playerInd start end
sendRequest sock addr (Just (GameState.UpdatePosition (UpdatePositionData playerInd coords))) =
  sendUpdatePosition sock addr playerInd coords
sendRequest _ _ _ = return ()

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

runClientEventLoop :: Socket -> PlayerState -> IO ()
runClientEventLoop sock state = do
  response <- NetworkBytes.recvFrom sock 4096

  let (byteStr, _) = response
  print $ Bytes.unpack byteStr

  let newState = handleServerRequest state $ parseServerRequest $ Bytes.unpack byteStr
  print newState

  case newState of
    PlayerState Nothing Nothing -> close sock -- end of game
    _                           -> runClientEventLoop sock newState

launchClientHandler :: IO ()
launchClientHandler = do
  addrInfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      sendConnect sock $ addrAddress serverAddr
      forever $ runClientEventLoop sock initialPlayerState
