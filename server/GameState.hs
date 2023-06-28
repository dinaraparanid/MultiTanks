module GameState where

type Coordinates = (Float, Float)

data PlayerSignals = Connect | ChangePosition | Shoot
  deriving (Eq, Show)

data GameSignals = UpdatePositions | Shot | Damage | Kill
  deriving (Eq, Show)