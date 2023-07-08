module Utils where

import Data.Word (Word8)
import Data.ByteString            as Bytes
import Data.ByteString.Conversion as BytesConversion
import GameState

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

bytesToUpdatePosition :: [Word8] -> Maybe UpdatePositionData
bytesToUpdatePosition [_, playerInd, x1, x2, x3, x4, y1, y2, y3, y4] = do
  xCoord <- BytesConversion.fromByteString $ Bytes.pack [x1, x2, x3, x4]
  yCoord <- BytesConversion.fromByteString $ Bytes.pack [y1, y2, y3, y4]
  Just $ UpdatePositionData playerInd (xCoord, yCoord)
bytesToUpdatePosition _ = Nothing

bytesToShotData :: [Word8] -> Maybe ShotData
bytesToShotData [_, playerInd, x11, x12, x13, x14, y11, y12, y13, y14, x21, x22, x23, x24, y21, y22, y23, y24] = do
  x1Coords <- BytesConversion.fromByteString $ Bytes.pack [x11, x12, x13, x14]
  y1Coords <- BytesConversion.fromByteString $ Bytes.pack [y11, y12, y13, y14]
  x2Coords <- BytesConversion.fromByteString $ Bytes.pack [x21, x22, x23, x24]
  y2Coords <- BytesConversion.fromByteString $ Bytes.pack [y21, y22, y23, y24]
  Just $ ShotData playerInd ((x1Coords, y1Coords), (x2Coords, y2Coords))
bytesToShotData _ = Nothing