module GameState where

import Data.Word (Word8)
import Network.Socket (SockAddr)

type Coordinates = (Int, Int)

data UpdatePositionData = UpdatePositionData Word8 Coordinates
  deriving (Eq, Show)

data ShotData = ShotData Word8 (Coordinates, Coordinates)
  deriving (Eq, Show)

data PlayerRequests = Connect | UpdatePosition UpdatePositionData | Shoot ShotData
  deriving (Eq, Show)

data ChangePositionData = ChangePositionData Coordinates Coordinates
  deriving (Eq, Show)

data ServerRequests = AssignFirstPlayer | AssignSecondPlayer | ChangePosition ChangePositionData | Shot ShotData | Kill Word8
  deriving (Eq, Show)

newtype GameState = GameState [(SockAddr, Coordinates)]
  deriving (Eq, Show)

firstPlayerInitPosition, secondPlayerInitPosition :: (Int, Int)
firstPlayerInitPosition = (-250, -150)
secondPlayerInitPosition = (250, -150)

initialGameState :: GameState
initialGameState = GameState []