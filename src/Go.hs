-- https://en.wikipedia.org/wiki/Rules_of_Go

module Go where

-- -- |
-- data Game = Game {

-- }

-- -- |
-- start :: Int -> Int -> Game

data Score = Score {
          fromTerritory :: !Int
        , fromCaptures  :: !Int
        , fromKomi      :: !Rational
    , total             :: !Rational
    }deriving (Eq, Show)

