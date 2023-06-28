module Server where

import Control.Monad (forever)
import Data.ByteString as Bytes
import Data.Word (Word8)
import Network.Socket
import Network.Socket.ByteString as NetworkBytes
import GameState
import Utils

handlePlayerRequest :: [Word8] -> Maybe GameState.PlayerSignals
handlePlayerRequest byteStr = case firstOrNothing byteStr of
    Nothing -> Nothing
    Just 0 -> Just GameState.Connect
    Just 1 -> Just GameState.ChangePosition
    Just 2 -> Just GameState.Shoot
    Just _ -> Nothing

runUDPServerForever :: IO ()
runUDPServerForever = do
  addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8080")

  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      bind sock (addrAddress serverAddr)
      forever $ do
        byteStr <- NetworkBytes.recv sock 4096
        print $ handlePlayerRequest $ Bytes.unpack byteStr

main :: IO ()
main = runUDPServerForever