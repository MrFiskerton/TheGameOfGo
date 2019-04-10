module Runner (
    start
) where

import qualified Board as B
import qualified Data.Map as Map
import Data.Ratio
import Go
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Images (Images (..))

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
            fieldsize = (19, 19)
            cellsize = 32 :: Float
            position = (300, 300)
            k = 6 + 1 % 2
            game = gosession 19 19 [B.Black, B.White] $ Map.singleton B.White k
            background = white
            fps = 30
-- |
viewPort = ViewPort (both (negate . (/ 2) . subtract 32) $ cellToScreen (19, 19)) 0 1

-- |
renderer :: Images -> GoGameSession p -> Picture
renderer images session =  pictures [imgwood images, grid 32 19 19]

type Size = Float

resize :: Size -> Path -> Path
resize k = fmap (\ (x, y) -> (x * k, y * k))

-- |
cellToScreen = both ((* 32) . fromIntegral)

-- |
screenToCell = both (round . (/ 32)) . invertViewPort viewPort

-- |
grid :: Float -> Int -> Int -> Picture
grid cellsize w h = applyViewPortToPicture viewPort $ pictures
    [uncurry translate (cellToScreen (x, y)) $
     color black $
     rectangleWire cellsize cellsize | x <- [0 .. w - 1], y <- [0 .. h - 1]
    ]

-- grid k w h = color black $ Pictures $ fmap (line . resize k)
--      [ [(0, 0), (1, 0)]
--      , [(0, 0), (0, 1)]
--      ]

updater _ = id
handler _ = id
