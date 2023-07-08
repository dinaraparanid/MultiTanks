module PlayerState where

import Data.Word (Word8)

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

newtype GameState = GameState [Coordinates]
  deriving (Eq, Show)

data PlayerState = PlayerState (Maybe Word8) (Maybe Coordinates)
  deriving (Eq, Show)

initialPlayerState :: PlayerState
initialPlayerState = PlayerState Nothing Nothing