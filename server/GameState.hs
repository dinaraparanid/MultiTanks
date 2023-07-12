module GameState where

import           Data.Word      (Word8)
import           Network.Socket (SockAddr)

type Coordinates = (Int, Int)

-- | Player's direction (changes on Left/Right keys)
data Direction = UpDir | DownDir | LeftDir | RightDir
  deriving (Eq, Show)

-- | Request to update player's position
data UpdatePositionData = UpdatePositionData Word8 Coordinates
  deriving (Eq, Show)

-- | Request to update player's direction
data UpdateDirectionData = UpdateDirectionData Word8 Direction
  deriving (Eq, Show)

-- | Shot data with player's index, current shot position and shot direction
data ShotData = ShotData Word8 Coordinates Direction
  deriving (Eq, Show)

-- | Player's request to the server
data PlayerRequests = Connect
                    | UpdatePosition UpdatePositionData
                    | UpdateDirection UpdateDirectionData
                    | Shoot ShotData
                    | CancelShoot Word8
  deriving (Eq, Show)

-- | Server's requests to players
data ServerRequests = AssignFirstPlayer
                    | AssignSecondPlayer
                    | ChangePosition UpdatePositionData
                    | ChangeDirection UpdateDirectionData
                    | Shot ShotData
                    | Kill Word8
                    | RemoveShoot Word8
  deriving (Eq, Show)

-- | Current game's state with directions, positions and shots for both players.
--   Note that game supports only single shot for player per moment
newtype GameState = GameState [(SockAddr, Direction, Coordinates, Maybe ShotData)]
  deriving (Eq, Show)

firstPlayerInitPosition, secondPlayerInitPosition :: (Int, Int)
firstPlayerInitPosition = (-250, -150)
secondPlayerInitPosition = (250, -150)

firstPlayerInitDirection, secondPlayerInitDirection :: Direction
firstPlayerInitDirection = UpDir
secondPlayerInitDirection = LeftDir

-- | Empty game's state with no players
initialGameState :: GameState
initialGameState = GameState []
