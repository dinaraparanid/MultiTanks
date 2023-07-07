module Server where

import           Control.Monad             (forever)
import           Data.ByteString           as Bytes
import           Data.ByteString.Conversion as BytesConversion
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
-- 2) server sends 1 to all (game start)
-- 3) player sends 2 - upd move
-- 4) player sends 3 - shoot
-- 5) server sends 4 - kill, game finished
parsePlayerRequest :: [Word8] -> Maybe GameState.PlayerSignals
parsePlayerRequest byteStr =
  case firstOrNothing byteStr of
    Nothing -> Nothing
    Just 0  -> Just GameState.Connect
    Just 2  -> GameState.ChangePosition <$> bytesToChangePosition byteStr
    Just 3  -> Just GameState.Shoot
    Just _  -> Nothing

bytesToChangePosition :: [Word8] -> Maybe ChangePositionData
bytesToChangePosition [_, playerInd, x1, x2, x3, x4, x5, x6, x7, x8, y1, y2, y3, y4, y5, y6, y7, y8] = do
  xCoord <- BytesConversion.fromByteString $ Bytes.pack [x1, x2, x3, x4, x5, x6, x7, x8]
  yCoord <- BytesConversion.fromByteString $ Bytes.pack [y1, y2, y3, y4, y5, y6, y7, y8]
  Just $ ChangePositionData playerInd (xCoord, yCoord)
bytesToChangePosition _ = Nothing

handlePlayerRequest :: GameState -> Maybe GameState.PlayerSignals -> Either GameState String
handlePlayerRequest state Nothing = Left state

handlePlayerRequest (GameState []) (Just GameState.Connect) = Left $ GameState [(0, 0)]
handlePlayerRequest (GameState [player1]) (Just GameState.Connect) = Left $ GameState [player1, (1, 1)]
handlePlayerRequest (GameState players) (Just GameState.Connect) = Left $ GameState players

handlePlayerRequest (GameState [player1, player2]) (Just (GameState.ChangePosition (ChangePositionData playerInd newCoords))) =
  case playerInd of
    0 -> Left $ GameState [newCoords, player2]
    1 -> Left $ GameState [player1, newCoords]
    _ -> Right "unknown byte pattern: expectede player in [0..1]"

handlePlayerRequest state (Just GameState.Shoot) = Left state -- TODO: shoot
handlePlayerRequest _ _ = Right "unknown signal or illegal game state pattern"

runGameEventLoop :: GameState -> Socket -> IO ()
runGameEventLoop state sock = do
  byteStr <- NetworkBytes.recv sock 4096
  case handlePlayerRequest state $ parsePlayerRequest $ Bytes.unpack byteStr of
    Left newState -> do
      print newState
      runGameEventLoop newState sock

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
      forever $ runGameEventLoop initialGameState sock

main :: IO ()
main = runUDPServerForever
