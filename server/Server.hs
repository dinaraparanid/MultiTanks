module Server where

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
--    player 1 sends 0 - connect
--    server sends 6 - player1 found
-- 2)
--    player 2 sends 0 - connect
--    server sends 7 - player2 found
--    server sends 1 - set init position and start game
-- 2)
--    player sends 2 - change move
--    server sends 1 - upd move
-- 3)
--    player sends 3 - shoot
--    ? server sends 4 - shot traectory, game continues
--    ? server sends 5 - kill, game finishes

parsePlayerRequest :: [Word8] -> Maybe GameState.PlayerRequests
parsePlayerRequest byteStr =
  case firstOrNothing byteStr of
    Just 0 -> Just GameState.Connect
    Just 2 -> GameState.UpdatePosition <$> Utils.bytesToUpdatePosition byteStr
    Just 3 -> GameState.Shoot <$> Utils.bytesToShotData byteStr
    _      -> Nothing

handlePlayerRequest ::
  GameState
  -> SockAddr
  -> Maybe GameState.PlayerRequests
  -> Either (GameState, Maybe GameState.ServerRequests) String

handlePlayerRequest state _ Nothing = Left (state, Nothing)

handlePlayerRequest (GameState []) addr (Just GameState.Connect) =
  Left (GameState [(addr, firstPlayerInitPosition)], Just GameState.AssignFirstPlayer)
handlePlayerRequest (GameState [player1]) addr (Just GameState.Connect) =
  Left (GameState [player1, (addr, secondPlayerInitPosition)], Just GameState.AssignSecondPlayer)
handlePlayerRequest state _ (Just GameState.Connect) = Left (state, Nothing)

handlePlayerRequest
  (GameState [(p1Addr, p1Coords), (p2Addr, p2Coords)])
  _
  (Just (GameState.UpdatePosition (UpdatePositionData playerInd newCoords))) =
  case playerInd of
    1 -> Left (GameState [(p1Addr, newCoords), (p2Addr, p2Coords)], Just $ GameState.ChangePosition $ ChangePositionData newCoords p2Coords)
    2 -> Left (GameState [(p1Addr, p1Coords), (p2Addr, newCoords)], Just $ GameState.ChangePosition $ ChangePositionData p1Coords newCoords)
    _ -> Right "unknown request pattern: expectede player in [1, 2]"

handlePlayerRequest state _ (Just (GameState.Shoot shotData)) = Left (state, Just (GameState.Shot shotData))
handlePlayerRequest _ _ _ = Right "unknown request or illegal game state pattern"

sendRequest :: Socket -> GameState -> Maybe GameState.ServerRequests -> IO ()
sendRequest sock (GameState [(addr, _)]) (Just GameState.AssignFirstPlayer) = sendAssignFirstPlayer sock addr

sendRequest sock (GameState [(p1Addr, p1Coords), (p2Addr, p2Coords)]) (Just GameState.AssignSecondPlayer) = do
  sendAssignSecondPlayer sock p2Addr
  sendChangePositions sock p1Addr p1Coords p2Coords
  sendChangePositions sock p2Addr p1Coords p2Coords

sendRequest sock (GameState [(p1Addr, _), (p2Addr, _)]) (Just (GameState.ChangePosition (ChangePositionData p1Coords p2Coords))) = do
  sendChangePositions sock p1Addr p1Coords p2Coords
  sendChangePositions sock p2Addr p1Coords p2Coords

sendRequest sock (GameState [(p1Addr, _), (p2Addr, _)]) (Just (GameState.Shot shotData)) = do
  sendShot sock p1Addr shotData
  sendShot sock p2Addr shotData

sendRequest sock (GameState [(p1Addr, _), (p2Addr, _)]) (Just (GameState.Kill playerInd)) = do
  sendKill sock p1Addr playerInd
  sendKill sock p2Addr playerInd

sendRequest _ _ _ = return ()

sendAssignFirstPlayer :: Socket -> SockAddr -> IO ()
sendAssignFirstPlayer sock addr = sendAllTo sock (Bytes.pack [6]) addr >> putStrLn "First player found"

sendAssignSecondPlayer :: Socket -> SockAddr -> IO ()
sendAssignSecondPlayer sock addr = sendAllTo sock (Bytes.pack [7]) addr >> putStrLn "Second player found"

sendChangePositions :: Socket -> SockAddr -> (Int, Int) -> (Int, Int) -> IO ()
sendChangePositions sock addr (x1, y1) (x2, y2) = do
  let commandBytes = Bytes.pack [1]
  let x1Bytes = Utils.intToByteString x1
  let y1Bytes = Utils.intToByteString y1
  let x2Bytes = Utils.intToByteString x2
  let y2Bytes = Utils.intToByteString y2
  let msg = commandBytes <> x1Bytes <> y1Bytes <> x2Bytes <> y2Bytes
  sendAllTo sock msg addr >> putStrLn "New positions are sent"

sendShot :: Socket -> SockAddr -> ShotData -> IO ()
sendShot sock addr (ShotData playerInd ((x1, y1), (x2, y2))) = do
  let commandBytes = Bytes.pack [4]
  let playerIndBytes = Bytes.pack [playerInd]
  let x1Bytes = Utils.intToByteString x1
  let y1Bytes = Utils.intToByteString y1
  let x2Bytes = Utils.intToByteString x2
  let y2Bytes = Utils.intToByteString y2
  let msg = commandBytes <> playerIndBytes <> x1Bytes <> y1Bytes <> x2Bytes <> y2Bytes
  sendAllTo sock msg addr >> putStrLn "Player has shot, no one is killed"

sendKill :: Socket -> SockAddr -> Word8 -> IO ()
sendKill sock addr playerInd = do
  let commandBytes = Bytes.pack [5]
  let playerIndBytes = Bytes.pack [playerInd]
  let msg = commandBytes <> playerIndBytes
  sendAllTo sock msg addr >> putStrLn "Player is killed, game finished"

runGameEventLoop :: Socket -> GameState -> IO ()
runGameEventLoop sock state = do
  (byteStr, addr) <- NetworkBytes.recvFrom sock 4096
  print state

  case handlePlayerRequest state addr $ parsePlayerRequest $ Bytes.unpack byteStr of
    Left (newState, request) -> do
      print newState
      sendRequest sock newState request
      runGameEventLoop sock newState

    Right err -> do
      putStrLn err
      return ()

runUDPServerForever :: IO ()
runUDPServerForever = do
  addrInfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      bind sock $ addrAddress serverAddr
      forever $ runGameEventLoop sock initialGameState

main :: IO ()
main = runUDPServerForever
