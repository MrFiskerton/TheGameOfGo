module Runner (
    start
) where

import Graphics.Gloss
import Images (Images (..))

start :: Images -> IO ()
start images = display window background drawing
      where
      window = InWindow "Nice Window" (400, 400) (300, 300) -- size, position
      background = white
      drawing = imgwood images
