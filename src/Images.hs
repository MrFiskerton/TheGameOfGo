module Images (
    Images(..), load
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

data Images = Images { imgwood   :: Picture
                     , imgpieces :: [Picture] }

-- |
load :: IO Images
load = do
    Just wood <- loadJuicyPNG "res/wood_texture.png"
    Just piece_red <- loadJuicyPNG "res/piece_red.png"
    Just piece_yellow <- loadJuicyPNG "res/piece_yellow.png"
    return Images {
        imgwood = wood,
        imgpieces = [ scale 0.2 0.2 piece_red, scale 0.2 0.2 piece_yellow ]
    }
