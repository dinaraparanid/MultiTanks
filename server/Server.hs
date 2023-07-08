module Server where

import Control.Monad             (forever)
import Data.ByteString           as Bytes
import Data.ByteString.Conversion as BytesConversion
import Data.Word                 (Word8)
import GameState
import Network.Socket
import Network.Socket.ByteString as NetworkBytes
import Utils

-- | Client/Server description:
-- Max 2 players
--
-- 1)
--    player 1 send 0 - connect
--    server sends 6 - player1 found
-- 2)
--    player 2 send 0 - connect
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
    Just 0  -> Just GameState.Connect
    Just 2  -> GameState.UpdatePosition <$> Utils.bytesToUpdatePosition byteStr
    Just 3  -> GameState.Shoot <$> Utils.bytesToShotData byteStr
    _  -> Nothing

handlePlayerRequest ::
  GameState 
  -> Maybe GameState.PlayerRequests
  -> Either (GameState, Maybe GameState.ServerRequests) String

handlePlayerRequest state Nothing = Left (state, Nothing)

handlePlayerRequest (GameState []) (Just GameState.Connect) =
  Left (GameState [firstPlayerInitPosition], Just GameState.AssignFirstPlayer)
handlePlayerRequest (GameState [player1]) (Just GameState.Connect) =
  Left (GameState [player1, secondPlayerInitPosition], Just GameState.AssignSecondPlayer)
handlePlayerRequest state (Just GameState.Connect) = Left (state, Nothing)

handlePlayerRequest (GameState [player1, player2]) (Just (GameState.UpdatePosition (UpdatePositionData playerInd newCoords))) =
  case playerInd of
    0 -> Left (GameState [newCoords, player2], Just $ GameState.ChangePosition $ ChangePositionData newCoords player2)
    1 -> Left (GameState [player1, newCoords], Just $ GameState.ChangePosition $ ChangePositionData player1 newCoords)
    _ -> Right "unknown request pattern: expectede player in [0, 1]"

handlePlayerRequest state (Just (GameState.Shoot shotData)) = Left (state, Just (GameState.Shot shotData))
handlePlayerRequest _ _ = Right "unknown request or illegal game state pattern"

sendRequest :: Socket -> SockAddr -> Maybe GameState.ServerRequests -> IO ()
sendRequest sock addr (Just GameState.AssignFirstPlayer) = sendAssignFirstPlayer sock addr
sendRequest sock addr (Just GameState.AssignSecondPlayer) = sendAssignSecondPlayer sock addr
sendRequest sock addr (Just (GameState.Shot shotData)) = sendShot sock addr shotData
sendRequest sock addr (Just (GameState.Kill playerInd)) = sendKill sock addr playerInd
sendRequest sock addr (Just (GameState.ChangePosition (ChangePositionData player1 player2))) =
  sendChangePositions sock addr player1 player2
sendRequest _ _ _ = return ()

sendAssignFirstPlayer :: Socket -> SockAddr -> IO ()
sendAssignFirstPlayer sock addr = sendAllTo sock (Bytes.pack [6]) addr >> putStrLn "First player found"

sendAssignSecondPlayer :: Socket -> SockAddr -> IO ()
sendAssignSecondPlayer sock addr = sendAllTo sock (Bytes.pack [7]) addr >> putStrLn "Second player found"

sendChangePositions :: Socket -> SockAddr -> (Int, Int) -> (Int, Int) -> IO ()
sendChangePositions sock addr (x1, y1) (x2, y2) = do
  let commandBytes = Bytes.pack [1]
  let x1Bytes = BytesConversion.toByteString' x1
  let y1Bytes = BytesConversion.toByteString' y1
  let x2Bytes = BytesConversion.toByteString' x2
  let y2Bytes = BytesConversion.toByteString' y2
  let msg = commandBytes <> x1Bytes <> y1Bytes <> x2Bytes <> y2Bytes 
  sendAllTo sock msg addr >> putStrLn "New positions are sent"

sendShot :: Socket -> SockAddr -> ShotData -> IO ()
sendShot sock addr (ShotData playerInd ((x1, y1), (x2, y2))) = do
  let commandBytes = Bytes.pack [4]
  let playerIndBytes = Bytes.pack [playerInd]
  let x1Bytes = BytesConversion.toByteString' x1
  let y1Bytes = BytesConversion.toByteString' y1
  let x2Bytes = BytesConversion.toByteString' x2
  let y2Bytes = BytesConversion.toByteString' y2
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
  response <- NetworkBytes.recvFrom sock 1024
  let (byteStr, addr) = response

  case handlePlayerRequest state $ parsePlayerRequest $ Bytes.unpack byteStr of
    Left (newState, request) -> do
      print newState
      sendRequest sock addr request
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
