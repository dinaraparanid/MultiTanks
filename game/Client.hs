module Client where

import           Data.ByteString            as Bytes
import           Data.ByteString.Conversion as BytesConversion
import           Network.Socket
import           Network.Socket.ByteString  as NetworkBytes
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

sendPlayerMsg :: Bytes.ByteString -> IO ()
sendPlayerMsg bytes = do
  addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return ()
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      sendAllTo sock bytes $ addrAddress serverAddr

sendConnect :: IO ()
sendConnect = sendPlayerMsg (Bytes.pack [0]) >> putStrLn "Connect is sent"

sendChangePosition :: Int -> (Int, Int) -> IO ()
sendChangePosition playerInd (x, y) = do
  let commandBytes = Bytes.pack [2]
  let playerIndBytes = BytesConversion.toByteString' playerInd
  let xBytes = BytesConversion.toByteString' x
  let yBytes = BytesConversion.toByteString' y
  let msg = commandBytes <> playerIndBytes <> xBytes <> yBytes
  sendPlayerMsg msg >> putStrLn "New position is sent"

sendShoot :: (Int, Int) -> (Int, Int) -> IO ()
sendShoot (xStart, yStart) (xEnd, yEnd) = do
  let commandBytes = Bytes.pack [3]
  let xStartBytes = BytesConversion.toByteString' xStart
  let yStartBytes = BytesConversion.toByteString' yStart
  let xEndBytes = BytesConversion.toByteString' xEnd
  let yEndBytes = BytesConversion.toByteString' yEnd
  let msg = commandBytes <> xStartBytes <> yStartBytes <> xEndBytes <> yEndBytes
  sendPlayerMsg msg >> putStrLn "Shoot is sent"
