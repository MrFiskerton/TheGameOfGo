module Main where

import Graphics.Gloss

main :: IO ()
main = display window background drawing
      where
      window = InWindow "Nice Window" (400, 400) (300, 300) -- size, position
      background = white
      drawing = Translate (-170) (-20) -- shift the text to the middle of the window
                $ Scale 0.5 0.5        -- display it half the original size
                $ Text "Hello World"   -- text to display
