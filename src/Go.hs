-- https://en.wikipedia.org/wiki/Rules_of_Go

module Go where

import Board
import qualified Data.Set as Set

data GameSession p = GameSession {
    board  :: !(Board.Board p),
    komi   :: !Rational,
    passes :: !(Set.Set p)
} deriving (Eq, Show)

data Score = Score {
        territory :: !Int,
        captures  :: !Int,
        komipoint :: !Rational,
        total     :: !Rational
} deriving (Eq, Show)


-- -- | A turn is either a pass or a move
-- data Turn = Pass | Move Position
