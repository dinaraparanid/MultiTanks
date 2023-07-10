module GameState where

import           Data.Word      (Word8)
import           Network.Socket (SockAddr)

type Coordinates = (Int, Int)

data Direction = UpDir | DownDir | LeftDir | RightDir
  deriving (Eq, Show)

data UpdatePositionData = UpdatePositionData Word8 Coordinates
  deriving (Eq, Show)

data UpdateDirectionData = UpdateDirectionData Word8 Direction
  deriving (Eq, Show)

data ShotData = ShotData Word8 (Coordinates, Coordinates)
  deriving (Eq, Show)

data PlayerRequests = Connect
                    | UpdatePosition UpdatePositionData
                    | UpdateDirection UpdateDirectionData
                    | Shoot ShotData
  deriving (Eq, Show)

data ChangePositionData = ChangePositionData Coordinates Coordinates
  deriving (Eq, Show)

data ChangeDirectionData = ChangeDirectionData Direction Direction
  deriving (Eq, Show)

data ServerRequests = AssignFirstPlayer
                    | AssignSecondPlayer
                    | ChangePosition ChangePositionData
                    | ChangeDirection ChangeDirectionData
                    | Shot ShotData | Kill Word8
  deriving (Eq, Show)

newtype GameState = GameState [(SockAddr, Direction, Coordinates)]
  deriving (Eq, Show)

firstPlayerInitPosition, secondPlayerInitPosition :: (Int, Int)
firstPlayerInitPosition = (-250, -150)
secondPlayerInitPosition = (250, -150)

firstPlayerInitDirection, secondPlayerInitDirection :: Direction
firstPlayerInitDirection = UpDir
secondPlayerInitDirection = LeftDir

initialGameState :: GameState
initialGameState = GameState []
