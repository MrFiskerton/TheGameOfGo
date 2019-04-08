module Board (
    Point,
    PointOwner,
    Piece(..), charpiece,
    Board(..), board, charboard,
    neighbours, boundedNeighbours, hostileNeighbours, connected, liberties, enclosure,
    set, put, get, remove, move,  kill, capture,
    inBounds, isConnected, isDead, isHostile
) where

import Data.List (nub, nubBy)
import qualified Data.Map as Map
import Data.Maybe (isNothing, maybe)
import qualified Data.Set as Set

type Point = (Int, Int)

-- | Each point on the grid may be colored black, white or empty.
-- data Colour = Black | White deriving (Eq, Enum, Show)
data Piece = Empty | White | Black deriving (Eq, Show, Ord)

-- | A state type for enclosure
data PointOwner p = NoOne     -- ^ if there is a collision
                  | Single p  -- ^ only one player can reach
                  | Unknown

-- | Update owner state
updateOwner :: Eq p => PointOwner p -> Maybe p -> PointOwner p
updateOwner NoOne _ = NoOne
updateOwner (Single p) (Just q) | p == q    = Single p
                                | otherwise = NoOne
updateOwner Unknown (Just p) = Single p
updateOwner owner Nothing = owner

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

-- | Modify a board by modifying its map.
updateBoard :: (Map.Map Point p -> Map.Map Point q) -> (Board p -> Board q)
updateBoard f (Board w h m) = Board w h (f m)

-- | Set a piece on the board in point. Record will be removed if the given piece is Nothing
set :: Point -> Maybe p -> Board p -> Board p
set point piece = updateBoard $ Map.alter (const piece) point

-- | Get the piece at the given position or 'Nothing'
-- if there is no stone there or the position is incorrect.
get :: Point -> Board p -> Maybe p
get p = Map.lookup p . content

-- | Put a piece on the board.
put :: Point -> p -> Board p -> Board p
put p = set p . Just

-- | Remove a piece from the board.
remove :: Point -> Board p -> Board p
remove p = set p Nothing

-- | Get the positions of all the pieces connected to the piece at the given point.
connected :: Eq p => Point -> Board p -> Set.Set Point
connected point b  = case get point b of Nothing    -> Set.empty
                                         Just owner -> recur owner Set.empty [point]
    where recur _ visited [] = visited
          recur owner visited (point' : pts) = let adjacent = boundedNeighbours b point'
                                                   inConnection x = x `Set.notMember` visited && get x b == Just owner
                                               in recur owner (Set.insert point' visited) (pts ++ filter inConnection adjacent)

-- |
isConnected :: Eq p => Board p -> Point -> Point -> Bool
isConnected b p1 p2 = connected p1 b == connected p2 b

-- | Get the liberties (breath) of the connected stones at the given point.
-- The liberty of a stone is an empty intersection adjacent to that stone or adjacent to a stone which is connected to that stone.
liberties :: Eq p => Point -> Board p -> Set.Set Point
liberties point b =
    let connect = Set.toList $ connected point b
        adjacent = map (Set.fromList . boundedNeighbours b) connect
        candidates = Set.unions adjacent
    in  Set.filter (isNothing . (`get` b)) candidates

-- | Check if a stone is dead
isDead :: Eq p => Board p -> Point -> Bool
isDead b point = Set.null $ liberties point b -- null - Is this the empty set?

-- | Remove the connected stones at the given position
kill :: Eq p => Point -> Board p -> Board p
kill p b = updateBoard (flip (Set.foldr Map.delete) $ connected p b) b

-- | Capture the connected stones at the given position if it dead.
capture :: Eq p => Point -> Board p -> (Int, Board p)
capture p b
    | isDead b p = (amount, b')
    | otherwise = (0, b)
        where amount = Set.size ( connected p b)
              b' = kill p b

-- | The piece at the given point owned by another player
isHostile :: Eq p => Board p -> p -> Point -> Bool
-- maybe applies the second argument to the third, when it is Just x, otherwise returns the first argument.
isHostile b piece p = maybe False (/= piece) $ get p b

-- |
hostileNeighbours :: Eq p => Board p -> p -> Point -> [Point]
hostileNeighbours b piece p = filter (isHostile b piece) $ boundedNeighbours b p

-- | Modify a board by combining two maps.
combineBoards :: (Map.Map Point p -> Map.Map Point q -> Map.Map Point r)
              -> (Board p -> Board q -> Board r)
combineBoards operation (Board w h p) (Board _ _ q) = Board w h (p `operation` q)

-- | Put a stone for the given player at the given position and then capture opponents' stones and self-capture.
move :: Eq p => Point -> p -> Board p
     -> (Int, Board p) -- ^  The number of opponents' stones captured and new board
move p piece b =
    let b' = put p piece b
        groups = nubBy (isConnected b) $ hostileNeighbours b piece p
        (points, captures) = unzip $ map (`capture` b') groups
        totalPoints = sum points
        totalOpponentCaptures = foldl (combineBoards Map.intersection) b' captures
        (suicidesPoints, selfCapture) = capture p totalOpponentCaptures
        totalBoard = if suicidesPoints /= 0 then b else selfCapture -- Suicides does not prohibit.
    in  (totalPoints, totalBoard)

-- | Gets the owner of enclosure
-- Starting search from the given point, stopping at points occupied by stones.
-- A player owns the enclosure if all stones found during the search operation belong to that player,
-- and at least one such stone is found.
enclosure :: Eq p => Point -> Board p -- ^ Starting point and board
          -> (Maybe p, Set.Set Point) -- ^ Return a player who should receive points for the enclosure (or no one)
                                      -- and a set of all the points enclosed in the area
enclosure point b =
    let recur owner visited [] = (owner, visited)
        recur owner visited (p : pts) = recur owner' visited' pts'
            where   adjacent = boundedNeighbours b p
                    owner' = foldl updateOwner owner $ map (`get` b) adjacent
                    newpts = filter (\x -> x `Set.notMember` visited && isNothing (get x b)) adjacent
                    visited' = Set.unions [Set.singleton p, Set.fromList newpts, visited]
                    pts' = pts ++ newpts
    in case (get point b, recur Unknown Set.empty [point]) of
        (Nothing, (Single p, pset)) -> (Just p,  pset)
        (Nothing, (_, pset))        -> (Nothing, pset)
        (Just _, _)                 -> (Nothing, connected point b)

