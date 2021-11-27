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

----------------- Koch snowflake -----------------
kochLine :: Int -> Point -> Point -> Path
kochLine n pS pE
  | n <= 0 = []
  | otherwise = [pS] ++ kochLine (n - 1) pS p1
                     ++ kochLine (n - 1) p1 p2
                     ++ kochLine (n - 1) p2 p3
                     ++ kochLine (n - 1) p3 pE
                     ++ [pE]
  where
    l1@(_, p1) = scaleLine (1 / 3) (pS, pE)
    l2@(_, p3) = connectLine l1 l1
    (_, p2) = rotateLine ((5 / 3) * pi) l2

kochFlake :: Int -> Line -> Path
kochFlake n line = kochLine n p1 p2
                ++ kochLine n p2 p3
                ++ kochLine n p3 p1
  where
    [p1, p2, p3, _] = polygon 3 line

-- Example: mapM_ putSixel [snowflake n white blue | n <- [1..7]]
snowflake :: Int -> Colour -> Colour -> Image PixelRGB8
snowflake n colour backgroundColour = convertRGB8 . ImageRGBA8 $
  drawPicture 800 800 backgroundColour 3.0 [(colour, kochFlake n ((100, 225), (700, 225)))]
