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
goplayers  = [ Player "Black" 0 0
             , Player "White" (6 + 1 % 2) 1
             ]

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
    play window background fps game (renderer images) handler updater
      where window = InWindow "The Game of Go" size position
            size = both (* round cellsize) fieldsize
            fieldsize = (21, 21)
            cellsize = 32 :: Float
            position = (300, 300)
            k = 6 + 1 % 2
            game = G.gosession 19 19 goplayers (Map.fromList $ zip goplayers (map komi goplayers))
            background = white
            fps = 30
-- |
viewPort = ViewPort (both (negate . (/ 2) . subtract 32) $ cellToScreen (18, 18)) 0 1 -- rotation , scale

-- |
renderer :: Images -> G.GoGameSession Player -> Picture
renderer images session@G.GoGameSession {G.board = b} =  pictures
    [ imgwood images
    , drawgrid 32 (19 - 1) (19 - 1)
    , drawpieces 32 images b]

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

handler (EventKey (MouseButton LeftButton) Down _ mouse) session =
    case G.move cell session of Right s -> s
                                Left _  -> session
    where cell = screenToGocell mouse
handler (EventKey (MouseButton RightButton) Down _ mouse) session =
    case G.pass session of Right s -> s
                           Left _  -> session

handler _ session = session
