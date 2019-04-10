module Main where

import qualified Images
import Runner

main :: IO ()
main = do
      content <- Images.load
      start content
