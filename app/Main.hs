module Main where

import Fractals
import LineGraphics
import Data.Sixel (putSixel)

main :: IO ()
main = do
  putStrLn "Koch snowflake"
  putSixel $ snowflake 7 white blue
  putStrLn "Pythagoras tree"
  putSixel $ tree 0.5 340 120 12 white red
  putStrLn "Hilbert space-filling curve"
  putSixel $ spaceFillingCurve (50, 750) 700 7 white magenta
  putStrLn "T-square"
  putSixel $ square (400, 400) 400 7 white orange
