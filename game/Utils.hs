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

bytesToChangePosition :: [Word8] -> Maybe ChangePositionData
bytesToChangePosition [_, x11, x12, x13, x14, y11, y12, y13, y14, x21, x22, x23, x24, y21, y22, y23, y24] = do
  x1Coords <- bytesToInt [x11, x12, x13, x14]
  y1Coords <- bytesToInt [y11, y12, y13, y14]
  x2Coords <- bytesToInt [x21, x22, x23, x24]
  y2Coords <- bytesToInt [y21, y22, y23, y24]
  Just $ ChangePositionData (x1Coords, y1Coords) (x2Coords, y2Coords)
bytesToChangePosition _ = Just $ ChangePositionData (0, 0) (0, 0)

bytesToShotData :: [Word8] -> Maybe ShotData
bytesToShotData [_, playerInd, x11, x12, x13, x14, y11, y12, y13, y14, x21, x22, x23, x24, y21, y22, y23, y24] = do
  x1Coords <- bytesToInt [x11, x12, x13, x14]
  y1Coords <- bytesToInt [y11, y12, y13, y14]
  x2Coords <- bytesToInt [x21, x22, x23, x24]
  y2Coords <- bytesToInt [y21, y22, y23, y24]
  Just $ ShotData playerInd ((x1Coords, y1Coords), (x2Coords, y2Coords))
bytesToShotData _ = Nothing
