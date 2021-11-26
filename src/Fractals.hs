module Fractals where

import Codec.Picture
import LineGraphics

someFunc :: IO ()
someFunc = putStrLn "someFunc"

house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
                 (730, 450), (700, 450), (700, 750)]

door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]

neonHouse :: Image PixelRGB8
neonHouse = convertRGB8 . ImageRGBA8 $
               drawPicture 800 800 black 2.0 [(lightgreen, house), (red, door)]
