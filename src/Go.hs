module Go where

-- |
data Game = Game {

}

-- |
start :: Int -> Int -> Game

data Score = Score {
          fromTerritory :: !Int
        , fromCaptures  :: !Int
        , fromKomi      :: !Rational
    , total             :: !Rational
    }deriving (Eq, Show)

