-- https://en.wikipedia.org/wiki/Rules_of_Go

module Go (
    GoGameSession(..), gosession,
    Score(..), score, gamescore,
    isGameOver,
    InvalidMove(..), move, pass,
    nextqueue, nextplayer
) where

import qualified Board
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set

data GoGameSession p = GoGameSession {
    players      :: ![p],
    turnqueue    :: ![p],
    board        :: !(Board.Board p),
    boardhistory :: !(Set.Set (Board.Board p)),
    komi         :: !(Map.Map p Rational),
    captures     :: !(Map.Map p Int),
    passes       :: !(Set.Set p)
} deriving (Eq, Show)

-- |
gosession :: Ord p => Int -> Int -> [p] -> Map.Map p Rational -> GoGameSession p
gosession w h pl k =
    GoGameSession {
        players = pl,
        turnqueue = pl,
        board = Board.board (w, h),
        boardhistory = Set.singleton $ Board.board (w, h),
        komi = k `Map.union` Map.fromList (zip pl $ repeat 0), -- Set zero if not given
        captures = Map.fromList $ zip pl (repeat 0),
        passes = Set.empty
    }

data Score = Score {
        territorypoint :: !Int,
        capturepoint   :: !Int,
        komipoint      :: !Rational,
        total          :: !Rational
} deriving (Eq, Show)

-- | Get the score for the player
score :: Ord p => GoGameSession p -> p -> Score
score session player = Score { territorypoint = t, capturepoint = c, komipoint = k, total = total'}
    where   t = Set.size $ Map.findWithDefault Set.empty player $ Board.territories $ board session
            c = captures session Map.! player
            k = komi session Map.! player
            total' = fromIntegral t + fromIntegral c + k

-- | Evaluate scores for each player.
gamescore :: Ord p => GoGameSession p -> Map.Map p Score
gamescore session = Map.fromList $ zip pl $ map (score session) pl where pl = players session

-- | Check if game is over
isGameOver :: Ord p => GoGameSession p -> Bool
isGameOver session = Set.fromList (players session) `Set.isSubsetOf` passes session -- All players pass

-- |
nextqueue :: GoGameSession p -> (p, [p])
nextqueue GoGameSession{ turnqueue = next : q'}          = (next, q')
nextqueue GoGameSession{ players = p:ps, turnqueue = []} = (p, ps) -- fill queue
nextqueue _                                              = error "Game is over"

-- |
nextplayer :: Ord p => GoGameSession p -> Maybe p
nextplayer session | isGameOver session = Nothing
                   | otherwise = Just (fst $ nextqueue session)

-- |
-- Ko happends when a player may not play a move that causes the board to return to any previous state.
data InvalidMove = OutOfBounds | Suicide | Ko | Occupied | GameOver deriving (Eq, Show)

-- | A turn is either a pass or a move
-- data Turn = Pass | Move Position

-- | A move turn for the current player
move :: Ord p => Board.Point -> GoGameSession p -> Either InvalidMove (GoGameSession p)
move point session@GoGameSession{board = b, boardhistory = bhistory, captures = cap, passes = pas}
  | not $ Board.inBounds b point          = Left OutOfBounds
  | Board.get point board' /= Just player = Left Suicide -- in this case board' == board
  | isJust $ Board.get point b            = Left Occupied
  | board' `Set.member` bhistory          = Left Ko
  | isGameOver session                    = Left GameOver
  | otherwise = Right $ session { board = board'
                                , turnqueue = turnqueue'
                                , boardhistory = Set.insert board' bhistory
                                , captures = Map.adjust (+ capturepoints) player cap
                                , passes = Set.delete player pas
                                }
                        where
                            (player, turnqueue') = nextqueue session
                            (capturepoints, board') = Board.move point player b

-- | A pass turn for the current player
pass :: Ord p => GoGameSession p -> Either InvalidMove (GoGameSession p)
pass session | isGameOver session = Left GameOver
             | otherwise = let (player, queue') = nextqueue session
                           in Right $ session { turnqueue = queue'
                                              , passes = Set.insert player $ passes session
                                              }

