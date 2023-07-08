module GameState where

import Data.Word (Word8)

type Coordinates = (Int, Int)

data ChangePositionData = ChangePositionData Word8 Coordinates
  deriving (Eq, Show)

data PlayerSignals = Connect | ChangePosition ChangePositionData | Shoot
  deriving (Eq, Show)

data GameSignals = UpdatePositions | Shot | Kill
  deriving (Eq, Show)

newtype GameState = GameState [Coordinates]
  deriving (Eq, Show)

initialGameState :: GameState
initialGameState = GameState []