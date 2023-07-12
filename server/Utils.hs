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
bytesToUpdatePosition :: [Word8] -> Maybe UpdatePositionData
bytesToUpdatePosition [_, playerInd, x1, x2, x3, x4, y1, y2, y3, y4] = do
  xCoord <- bytesToInt [x1, x2, x3, x4]
  yCoord <- bytesToInt [y1, y2, y3, y4]
  Just $ UpdatePositionData playerInd (xCoord, yCoord)
bytesToUpdatePosition _ = Nothing

-- | Parses UpdateDirection from bytes
bytesToUpdateDirection :: [Word8] -> Maybe UpdateDirectionData
bytesToUpdateDirection [_, playerInd, 0] = Just $ UpdateDirectionData playerInd UpDir
bytesToUpdateDirection [_, playerInd, 1] = Just $ UpdateDirectionData playerInd DownDir
bytesToUpdateDirection [_, playerInd, 2] = Just $ UpdateDirectionData playerInd LeftDir
bytesToUpdateDirection [_, playerInd, 3] = Just $ UpdateDirectionData playerInd RightDir
bytesToUpdateDirection _ = Nothing

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
  xCrds <- bytesToInt [x1, x2, x3, x4]
  yCrds <- bytesToInt [y1, y2, y3, y4]
  dir <- byteToDirection dirB
  Just $ ShotData playerInd (xCrds, yCrds) dir
bytesToShotData _ = Nothing

-- | Parses undo shoot player's index from bytes
bytesToCancelShoot :: [Word8] -> Maybe Word8
bytesToCancelShoot [_, playerInd] = Just playerInd
bytesToCancelShoot _              = Nothing
