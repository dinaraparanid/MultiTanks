module Utils where

import           Data.ByteString            as Bytes
import           Data.ByteString.Conversion as BytesConversion
import           Data.Word                  (Word8)
import           GameState

firstOrNothing :: [a] -> Maybe a
firstOrNothing []    = Nothing
firstOrNothing (x:_) = Just x

intToByteString :: Int -> Bytes.ByteString
intToByteString x = do
  let bytes = Bytes.unpack $ BytesConversion.toByteString' x
  Bytes.pack $ Prelude.take (4 - Prelude.length bytes) (Prelude.map (const 0) [0 .. ]) ++ bytes

bytesToInt :: [Word8] -> Maybe Int
bytesToInt bytes = BytesConversion.fromByteString $ Bytes.pack $ Prelude.dropWhile (== 0) bytes

bytesToUpdatePosition :: [Word8] -> Maybe UpdatePositionData
bytesToUpdatePosition [_, playerInd, x1, x2, x3, x4, y1, y2, y3, y4] = do
  xCoord <- bytesToInt [x1, x2, x3, x4]
  yCoord <- bytesToInt [y1, y2, y3, y4]
  Just $ UpdatePositionData playerInd (xCoord, yCoord)
bytesToUpdatePosition _ = Nothing

bytesToUpdateDirection :: [Word8] -> Maybe UpdateDirectionData
bytesToUpdateDirection [_, playerInd, 0] = Just $ UpdateDirectionData playerInd UpDir
bytesToUpdateDirection [_, playerInd, 1] = Just $ UpdateDirectionData playerInd DownDir
bytesToUpdateDirection [_, playerInd, 2] = Just $ UpdateDirectionData playerInd LeftDir
bytesToUpdateDirection [_, playerInd, 3] = Just $ UpdateDirectionData playerInd RightDir
bytesToUpdateDirection _ = Nothing

directionToByte :: Direction -> Word8
directionToByte GameState.UpDir    = 0
directionToByte GameState.DownDir  = 1
directionToByte GameState.LeftDir  = 2
directionToByte GameState.RightDir = 3

bytesToShotData :: [Word8] -> Maybe ShotData
bytesToShotData [_, playerInd, x11, x12, x13, x14, y11, y12, y13, y14, x21, x22, x23, x24, y21, y22, y23, y24] = do
  x1Coords <- bytesToInt [x11, x12, x13, x14]
  y1Coords <- bytesToInt [y11, y12, y13, y14]
  x2Coords <- bytesToInt [x21, x22, x23, x24]
  y2Coords <- bytesToInt [y21, y22, y23, y24]
  Just $ ShotData playerInd ((x1Coords, y1Coords), (x2Coords, y2Coords))
bytesToShotData _ = Nothing
