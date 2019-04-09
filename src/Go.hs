-- https://en.wikipedia.org/wiki/Rules_of_Go

module Go (
    GoGameSession(..), gosession,
    Score(..), score, gamescore
) where

import qualified Board
import qualified Data.Map as Map
import qualified Data.Set as Set

data GoGameSession p = GoGameSession {
    players      :: ![p],
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
gamescore :: Show p => Ord p => GoGameSession p -> Map.Map p Score
gamescore session = Map.fromList $ zip pl $ map (score session) pl where pl = players session

-- | A turn is either a pass or a move
-- data Turn = Pass | Move Position
