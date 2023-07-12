module Utils where

import           Data.ByteString            as Bytes
import           Data.ByteString.Conversion as BytesConversion
import           Data.Word                  (Word8)
import           GameState

-- | Gets first element or nothing
--
-- >>> inToByteString 5
-- Just 1
--
-- >>> firstOrNothing []
-- Nothing
firstOrNothing :: [a] -> Maybe a
firstOrNothing []    = Nothing
firstOrNothing (x:_) = Just x

-- | Converts Int to ByteString with size equal to 4,
--   by adding zeroes to the beggining of the byte string.
--   Note that standart implementation with toByteString
--   produces bytes that are necessary for byte representation,
--   in other words, it is not guaranted that output will be 4 length sized
intToByteString :: Int -> Bytes.ByteString
intToByteString x = do
  let bytes = Bytes.unpack $ BytesConversion.toByteString' x
  Bytes.pack $ Prelude.take (4 - Prelude.length bytes) (Prelude.map (const 0) [0 .. ]) ++ bytes

-- | Parses Int from bytes, trimming all zeroes from the beggining
bytesToInt :: [Word8] -> Maybe Int
bytesToInt bytes = BytesConversion.fromByteString $ Bytes.pack $ Prelude.dropWhile (== 0) bytes

-- | Parses UpdateData from bytes
bytesToChangePosition :: [Word8] -> Maybe UpdatePositionData
bytesToChangePosition [_, playerInd, x1, x2, x3, x4, y1, y2, y3, y4] = do
  xCoord <- bytesToInt [x1, x2, x3, x4]
  yCoord <- bytesToInt [y1, y2, y3, y4]
  Just $ UpdatePositionData playerInd (xCoord, yCoord)
bytesToChangePosition _ = Nothing

-- | Parses UpdateDirection from bytes
bytesToChangeDirection :: [Word8] -> Maybe UpdateDirectionData
bytesToChangeDirection [_, playerInd, 0] = Just $ UpdateDirectionData playerInd UpDir
bytesToChangeDirection [_, playerInd, 1] = Just $ UpdateDirectionData playerInd DownDir
bytesToChangeDirection [_, playerInd, 2] = Just $ UpdateDirectionData playerInd LeftDir
bytesToChangeDirection [_, playerInd, 3] = Just $ UpdateDirectionData playerInd RightDir
bytesToChangeDirection _ = Nothing

-- | Converts direction to byte representation
directionToByte :: Direction -> Word8
directionToByte GameState.UpDir    = 0
directionToByte GameState.DownDir  = 1
directionToByte GameState.LeftDir  = 2
directionToByte GameState.RightDir = 3

-- | Parses direction from a byte
byteToDirection :: Word8 -> Maybe Direction
byteToDirection 0 = Just GameState.UpDir
byteToDirection 1 = Just GameState.DownDir
byteToDirection 2 = Just GameState.LeftDir
byteToDirection 3 = Just GameState.RightDir
byteToDirection _ = Nothing

-- | Parses ShotData from bytes
bytesToShotData :: [Word8] -> Maybe ShotData
bytesToShotData [_, playerInd, x1, x2, x3, x4, y1, y2, y3, y4, dirB] = do
  xCoords <- bytesToInt [x1, x2, x3, x4]
  yCoords <- bytesToInt [y1, y2, y3, y4]
  dir <- byteToDirection dirB
  Just $ ShotData playerInd (xCoords, yCoords) dir
bytesToShotData _ = Nothing

-- | Parses player's index from bytes
bytesToPlayerInd :: [Word8] -> Maybe Word8
bytesToPlayerInd [_, playerInd] = Just playerInd
bytesToPlayerInd _              = Nothing

-- | Parses loser player's index from bytes
bytesToKill :: [Word8] -> Maybe Word8
bytesToKill = bytesToPlayerInd

-- | Parses undo shoot player's index from bytes
bytesToRemoveShot :: [Word8] -> Maybe Word8
bytesToRemoveShot = bytesToPlayerInd
