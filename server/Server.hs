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
--    player 1 send 0 - connect
--    player 2 send 0 - connect
-- 2) server sends 1 to all (game start)
-- 3) player sends 2 - upd move
-- 4) player sends 3 - shoot
-- 5) server sends 4 - kill, game finished
handlePlayerRequest :: [Word8] -> Maybe GameState.PlayerSignals
handlePlayerRequest byteStr =
  case firstOrNothing byteStr of
    Nothing -> Nothing
    Just 0  -> Just GameState.Connect
    Just 2  -> Just GameState.ChangePosition
    Just 3  -> Just GameState.Shoot
    Just _  -> Nothing

runUDPServerForever :: IO ()
runUDPServerForever = do
  addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      bind sock $ addrAddress serverAddr
      forever $ do
        byteStr <- NetworkBytes.recv sock 4096
        print $ handlePlayerRequest $ Bytes.unpack byteStr

main :: IO ()
main = runUDPServerForever
