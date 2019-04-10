module Runner (
    start
) where

import qualified Board as B
import qualified Data.Map as Map
import Data.Ratio
import qualified Go as G
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Images (Images (..))

data Player = Player { name  :: !String
                     , komi  :: !Rational
                     , index :: !Int
                     }
                     deriving (Eq, Ord, Show)

-- |
goplayers :: [Player]
goplayers  = [ Player "Pig" 0 0
             , Player "Chicken" (6 + 1 % 2) 1
             ]

-- |
data GameState p = GameState { game       :: !(G.GoGameSession p)
                             , isGameOver :: !Bool
                             } deriving (Eq, Show)
initialState :: GameState Player
initialState = GameState { game = session, isGameOver = False} where
    session = G.gosession 19 19 goplayers (Map.fromList $ zip goplayers (map komi goplayers))

-- |
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)

-- |
start :: Images -> IO ()
start images =
    -- play :: Display -> Color -> Int -> world
    --      -> (world -> Picture)                   -- renderer
    --      -> (Event -> world -> world)            -- updater
    --      -> (Float -> world -> world) -> IO ()   -- handler
    play window background fps initialState (renderer images) handler updater
      where window = InWindow "The Game of Go" size position
            size = both (* round cellsize) fieldsize
            fieldsize = (21, 21)
            cellsize = 32 :: Float
            position = (300, 300)
            background = white
            fps = 30
-- |
viewPort = ViewPort (both (negate . (/ 2) . subtract 32) $ cellToScreen (18, 18)) 0 1 -- rotation , scale

score :: G.GoGameSession Player -> [String]
score session =
    map report $ Map.toList $ G.gamescore session
  where
    report (player, score) = concat
        [ name player, ": "
        ,        show      (G.territorypoint score), " (territory)"
        , " + ", show      (G.capturepoint score),   " (captures)"
        , " + ", showFloat (G.komipoint score),      " (komi)"
        , " = ", showFloat (G.total score)
        ]
    showFloat t = show (fromRational t :: Float)

-- |
renderer :: Images -> GameState Player -> Picture
renderer images state@GameState {game = session}
    | isGameOver state = pictures [ gameover, player1, player2]
    | otherwise = pictures  [ imgwood images
                            , drawgrid 32 (19 - 1) (19 - 1)
                            , drawpieces 32 images $ G.board session]
    where report = score session
          gameover = Translate (-170) (-20) $ Scale 0.5 0.5 $ Text "GameOver"
          player1 = Translate (-200) (-100) $ Scale 0.1 0.1 $ Text $ head report
          player2 = Translate (-200) (-150) $ Scale 0.1 0.1 $ Text $ report !! 1

type Size = Float

resize :: Size -> Path -> Path
resize k = fmap (\ (x, y) -> (x * k, y * k))

-- |
cellToScreen = both ((* 32) . fromIntegral)

-- |
gocellToScreen = both (subtract 16 . (* 32) . fromIntegral)
-- |
--screenToCell = both (round . (/ 32)) . invertViewPort viewPort
-- |
screenToGocell = both ((+ 1) . round . (/ 32) . (+ 16)) . invertViewPort viewPort

-- |
drawgrid :: Float -> Int -> Int -> Picture
drawgrid cellsize w h = applyViewPortToPicture viewPort $ pictures
    [uncurry translate (cellToScreen (x, y)) $
     color black $ rectangleWire cellsize cellsize
    | x <- [0 .. w - 1], y <- [0 .. h - 1]
    ]

drawpieces :: Float -> Images -> B.Board Player -> Picture
drawpieces cellsize images board = applyViewPortToPicture viewPort $ pictures
    [uncurry translate (gocellToScreen (x , y)) $
     drawcell x y
     | x <- [0 .. w - 1], y <- [0 .. w - 1]
    ]
    where   w = B.width board
            h = B.height board
            drawcell x y = case B.get (x + 1, y + 1) board of
                Just p  -> imgpieces images !! index p
                Nothing -> blank--color green $ rectangleSolid 29 29

-- grid k w h = color black $ Pictures $ fmap (line . resize k)
--      [ [(0, 0), (1, 0)]
--      , [(0, 0), (0, 1)]
--      ]

updater _ = id

handler (EventKey (MouseButton LeftButton) Down _ mouse) state@GameState{game = session}
    | isGameOver state = initialState
    | otherwise = case G.move cell session of
        Right s         -> GameState {game = s, isGameOver = False}
        Left G.GameOver -> GameState {game = session, isGameOver = True}
        Left _          -> state
    where cell = screenToGocell mouse

handler (EventKey (MouseButton RightButton) Down _ mouse) state@GameState{game = session} =
    case G.pass session of Right s         -> GameState {game = s, isGameOver = False}
                           Left G.GameOver -> GameState {game = session, isGameOver = True}
                           Left _          -> state

handler _ state = state
