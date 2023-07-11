module GameState where

import           Data.Word (Word8)

type Coordinates = (Int, Int)

data Direction = UpDir | DownDir | LeftDir | RightDir
  deriving (Eq, Show)

data UpdatePositionData = UpdatePositionData Word8 Coordinates
  deriving (Eq, Show)

data UpdateDirectionData = UpdateDirectionData Word8 Direction
  deriving (Eq, Show)

data ShotData = ShotData Word8 Coordinates Direction
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

newtype GameState = GameState [(Maybe Direction, Maybe Coordinates, Maybe ShotData)]
  deriving (Eq, Show)

data PlayerState = PlayerState (Maybe Word8) (Maybe Direction) (Maybe Coordinates)
  deriving (Eq, Show)

type SystemState = (GameState, PlayerState)

initialPlayerState :: PlayerState
initialPlayerState = PlayerState Nothing Nothing Nothing

initialGameState :: GameState
initialGameState = GameState []

initialSystemState :: SystemState
initialSystemState = (initialGameState, initialPlayerState)
