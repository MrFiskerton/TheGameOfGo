module Board  where

import Data.Array
import qualified Data.Map as Map

-- |
data Colour = Black | White deriving (Eq, Enum, Show)

-- | Each point on the grid may be colored black, white or empty.
data Piece = Empty | Stone Colour deriving (Eq, Show)

type Point = (Int, Int)
type Position = Point -> Piece


data Board p = Board { width   :: !Int
                     , height  :: !Int
                     , content :: Map.Map Point p
                     } deriving (Show, Functor, Eq, Ord)


-- | A turn is either a pass or a move
data Turn = Pass | Move Position



outOfBoundsMessage :: Board -> Point -> Maybe String

inBounds :: Position -> Board -> Bool

-- | Check if a stone is dead
isDead :: Position -> Board -> Bool

-- | Check if a turn is valid
isValid :: Turn -> Board -> Bool

-- | Put a new stone on the field
put :: Turn -> Board -> Board

-- |
kill :: Position -> Board -> Board

-- |
connected :: Position -> Board -> [Position]

-- | All surrounding stones of a current position
neighbours :: Position -> [Position]
neighbours (x, y) = [ (x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], abs(dx + dy) == 1 ]


