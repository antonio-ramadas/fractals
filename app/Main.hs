module Main where

import Fractals
import Data.Sixel (putSixel)

main :: IO ()
main = do
    putSixel neonHouse
