module Board (
    Point,
    --Colour,
    Piece(..), charpiece,
    Board(..), board, charboard,
    neighbours, boundedNeighbours, connected, liberties,
    set, put, get,
    inBounds
) where

import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set

type Point = (Int, Int)

-- | Each point on the grid may be colored black, white or empty.
-- data Colour = Black | White deriving (Eq, Enum, Show)
data Piece = Empty | White | Black deriving (Eq, Show, Ord)

-- | Make a piece from char
charpiece :: Char -> Maybe Piece
charpiece c
    | c == 'o' || c == '0' = Just White
    | c == 'x' || c == 'X' = Just Black
    -- | c == '.' = Just Empty
    | otherwise = Nothing

----------------------------------------------------------------------------------
data Board piece = Board {
                     width   :: !Int
                   , height  :: !Int
                   , content :: !(Map.Map Point piece)
                   }
                   deriving (Show, Eq, Ord)

-- | Make a board
board :: (Int, Int) -- ^ size: (width, height)
          -> Board piece
board (w, h)
    | w < 0 = error $ "Not positive width: " ++ show w
    | h < 0 = error $ "Not positive height: " ++ show h
    | otherwise = Board w h Map.empty
    -- | otherwise = Board {
    --     width = w,
    --     height = h,
    --     content = Map.fromList [ ((x, y), Empty) | x <- [1..w], y <- [1..h] ]
    -- }

-- |
charboard :: (Char -> Maybe piece) -- ^ char to piece interpretation
          -> [String]              -- ^ board
          -> Board piece
charboard char2piece array2D =
    -- | The nub function removes duplicate elements from a list.
    let w = case nub $ map length array2D of [] -> 0
                                             [w'] -> w'
                                             many -> error $ "Board width non unique" ++ show many
        h = length array2D
        array2board b arrayOfRows = foldl (\f (y, r) -> row2cell y f r) b (zip [1..] arrayOfRows)
        row2cell y b rowOfChars = foldl (\f (x, c) -> cell2piece x y f c) b (zip [1..] rowOfChars)
        cell2piece x y b char = set (x, y) (char2piece char) b
    -- | Reverce. Row 1 at the bottom
    in array2board (board (w, h)) (reverse array2D)

-- | Modify a board by just modifying its map.
liftB :: (Map.Map Point p -> Map.Map Point q) -> (Board p -> Board q)
liftB f (Board w h m) = Board w h (f m)

-- | Modify a board by combining two maps of board's.
liftB2 :: (Map.Map Point p -> Map.Map Point q -> Map.Map Point r)
       -> (Board p -> Board q -> Board r)
liftB2 operation (Board w h p) (Board _ _ q) = Board w h (p `operation` q)

----------------------------------------------------------------------------------

-- | All surrounding stones of a current position
neighbours :: Point -> [Point]
neighbours (x, y) = [ (x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], abs(dx + dy) == 1 ]

-- | Check if point contains in a board
inBounds :: Board p -> Point -> Bool
inBounds Board { width = w, height = h} (x, y)
    | x < 1 || y < 1 || x > w || y > h = False
    | otherwise = True

-- |
boundedNeighbours :: Board p -> Point -> [Point]
boundedNeighbours b point = filter (inBounds b) $ neighbours point

-- | Set a piece on the board in point. Record will be removed if the given piece is Nothing
set :: Point -> Maybe p -> Board p -> Board p
set point piece = liftB $ Map.alter (const piece) point

-- | Get the piece at the given position or 'Nothing'
-- if there is no stone there or the position is incorrect.
get :: Point -> Board p -> Maybe p
get p = Map.lookup p . content

-- | Put a piece on the board.
put :: Point -> p -> Board p -> Board p
put p = set p . Just

-- | Get the positions of all the pieces connected to the piece at the given point.
connected :: Eq p => Point -> Board p -> Set.Set Point
connected point b  = case get point b of Nothing    -> Set.empty
                                         Just owner -> recur owner Set.empty [point]
    where recur _ visited [] = visited
          recur owner visited (point' : pts) = let adjacent = boundedNeighbours b point'
                                                   inConnection x = x `Set.notMember` visited && get x b == Just owner
                                               in recur owner (Set.insert point' visited) (pts ++ filter inConnection adjacent)

-- | Get the liberties (breath) of the connected stones at the given point.
-- The liberty of a stone is an empty intersection adjacent to that stone or adjacent to a stone which is connected to that stone.
liberties :: Eq p => Point -> Board p -> Set.Set Point
liberties point b =
    let connect = Set.toList $ connected point b
        adjacent = map (Set.fromList . boundedNeighbours b) connect
        candidates = Set.unions adjacent
    in  Set.filter (isNothing . (`get` b)) candidates

-- -- | A turn is either a pass or a move
-- data Turn = Pass | Move Position



-- outOfBoundsMessage :: Board -> Point -> Maybe String

-- inBounds :: Position -> Board -> Bool

-- -- | Check if a stone is dead
-- isDead :: Position -> Board -> Bool

-- -- | Check if a turn is valid
-- isValid :: Turn -> Board -> Bool

-- -- | Put a new stone on the field
-- put :: Turn -> Board -> Board

-- -- |
-- kill :: Position -> Board -> Board



