module Server where

import           Control.Monad             (forever)
import           Data.ByteString           as Bytes
import           Data.ByteString.Conversion as BytesConversion
import           Data.Word                 (Word8)
import           GameState
import           Network.Socket
import           Network.Socket.ByteString as NetworkBytes
import           Utils

firstPlayerInitPosition, secondPlayerInitPosition :: (Int, Int)
firstPlayerInitPosition = (-250, -150)
secondPlayerInitPosition = (250, -150)

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

parsePlayerRequest :: [Word8] -> Maybe GameState.PlayerSignals
parsePlayerRequest byteStr =
  case firstOrNothing byteStr of
    Nothing -> Nothing
    Just 0  -> Just GameState.Connect
    Just 2  -> GameState.ChangePosition <$> bytesToChangePosition byteStr
    Just 3  -> Just GameState.Shoot
    Just _  -> Nothing

bytesToChangePosition :: [Word8] -> Maybe ChangePositionData
bytesToChangePosition [_, playerInd, x1, x2, x3, x4, y1, y2, y3, y4] = do
  xCoord <- BytesConversion.fromByteString $ Bytes.pack [x1, x2, x3, x4]
  yCoord <- BytesConversion.fromByteString $ Bytes.pack [y1, y2, y3, y4]
  Just $ ChangePositionData playerInd (xCoord, yCoord)
bytesToChangePosition _ = Nothing

handlePlayerRequest ::
  GameState 
  -> Maybe GameState.PlayerSignals 
  -> Either (GameState, Maybe GameState.GameSignals) String
handlePlayerRequest state Nothing = Left (state, Nothing)

handlePlayerRequest (GameState []) (Just GameState.Connect) = Left (GameState [firstPlayerInitPosition], Nothing)
handlePlayerRequest (GameState [player1]) (Just GameState.Connect) = Left (GameState [player1, secondPlayerInitPosition], Nothing)
handlePlayerRequest (GameState players) (Just GameState.Connect) = Left (GameState players, Just GameState.UpdatePositions)

handlePlayerRequest (GameState [player1, player2]) (Just (GameState.ChangePosition (ChangePositionData playerInd newCoords))) =
  case playerInd of
    0 -> Left (GameState [newCoords, player2], Just GameState.UpdatePositions)
    1 -> Left (GameState [player1, newCoords], Just GameState.UpdatePositions)
    _ -> Right "unknown byte pattern: expectede player in [0..1]"

handlePlayerRequest state (Just GameState.Shoot) = Left (state, Just GameState.Shot) -- TODO: shoot
handlePlayerRequest _ _ = Right "unknown signal or illegal game state pattern"

handleSignal :: Socket -> GameState -> Maybe GameState.GameSignals -> IO ()
handleSignal _ _ Nothing = return ()
handleSignal sock (GameState [player1, player2]) (Just GameState.UpdatePositions) = sendUpdatePositions sock player1 player2
handleSignal sock _ (Just GameState.Shot) = sendShot sock
handleSignal sock _ (Just GameState.Kill) = sendKill sock
handleSignal _ _ _ = return ()

sendUpdatePositions :: Socket -> (Int, Int) -> (Int, Int) -> IO ()
sendUpdatePositions sock (x1, y1) (x2, y2) = do
  let commandBytes = Bytes.pack [1]
  let x1Bytes = BytesConversion.toByteString' x1
  let y1Bytes = BytesConversion.toByteString' y1
  let x2Bytes = BytesConversion.toByteString' x2
  let y2Bytes = BytesConversion.toByteString' y2
  let msg = commandBytes <> x1Bytes <> y1Bytes <> x2Bytes <> y2Bytes 
  sendAll sock msg >> putStrLn "New positions are sent"

sendShot :: Socket -> IO ()
sendShot sock = sendAll sock (Bytes.pack [4]) >> putStrLn "Player has shot, no one is killed"

sendKill :: Socket -> IO ()
sendKill sock = sendAll sock (Bytes.pack [5]) >> putStrLn "Player is killed, game finished"

runGameEventLoop :: Socket -> GameState -> IO ()
runGameEventLoop sock state = do
  byteStr <- NetworkBytes.recv sock 4096
  case handlePlayerRequest state $ parsePlayerRequest $ Bytes.unpack byteStr of
    Left (newState, signal) -> do
      print newState
      handleSignal sock newState signal
      runGameEventLoop sock newState

    Right err -> do
      putStrLn err
      return ()

runUDPServerForever :: IO ()
runUDPServerForever = do
  addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      bind sock $ addrAddress serverAddr
      forever $ runGameEventLoop sock initialGameState

main :: IO ()
main = runUDPServerForever
