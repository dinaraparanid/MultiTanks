module Client where

import           Data.ByteString            as Bytes
import           Data.ByteString.Conversion as BytesConversion
import           Network.Socket
import           Network.Socket.ByteString  as NetworkBytes
import           Utils

sendPlayerMsg :: Bytes.ByteString -> IO (Maybe Int)
sendPlayerMsg bytes = do
  addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8080")
  case Utils.firstOrNothing addrInfos of
    Nothing -> return Nothing
    Just serverAddr -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      res <- sendTo sock bytes $ addrAddress serverAddr
      return $ Just res

sendConnect :: IO ()
sendConnect = do
  sendRes <- sendPlayerMsg $ Bytes.pack [0]
  case sendRes of
    Nothing -> return ()
    Just _  -> putStrLn "Connect is sent"

sendChangePosition :: (Int, Int) -> IO ()
sendChangePosition (x, y) = do
  let commandBytes = Bytes.pack [2]
  let xBytes = BytesConversion.toByteString' x
  let yBytes = BytesConversion.toByteString' y
  let msg = commandBytes <> xBytes <> yBytes
  sendRes <- sendPlayerMsg msg
  case sendRes of
    Nothing -> return ()
    Just _  -> putStrLn "New position is sent"

sendShoot :: (Int, Int) -> (Int, Int) -> IO ()
sendShoot (xStart, yStart) (xEnd, yEnd) = do
  let commandBytes = Bytes.pack [3]
  let xStartBytes = BytesConversion.toByteString' xStart
  let yStartBytes = BytesConversion.toByteString' yStart
  let xEndBytes = BytesConversion.toByteString' xEnd
  let yEndBytes = BytesConversion.toByteString' yEnd
  let msg = commandBytes <> xStartBytes <> yStartBytes <> xEndBytes <> yEndBytes
  sendRes <- sendPlayerMsg msg
  case sendRes of
    Nothing -> return ()
    Just _  -> putStrLn "Shoot is sent"
