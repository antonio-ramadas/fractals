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

----------------- Pythagoras tree -----------------
fractalTree :: Float -> Int -> Line -> Path
fractalTree factor n line = fractalTree' n line
  where
    fractalTree' 0 line = []
    fractalTree' n line
      = [p1, p4] ++ fractalTree' (n-1) (p4, p5) ++
                    fractalTree' (n-1) (p5, p3) ++
        [p3, p2]
      where
        -- flip direction of line
        flipLine :: Line -> Line
        flipLine (pS, pE) = (pE, pS)

        -- The square:
        --   4------3
        --   |      |
        --   |      |
        --   1------2
        [p2,p1,p4,p3,_] = polygon 4 $ flipLine line
        r               = flipLine (scaleLine 0.5 (p4, p3))
        (_, p5)         = rotateLine (factor * pi) r

-- Example: mapM_ putSixel [tree 0.5 340 120 n white blue | n <- [1..12]]
-- Another: mapM_ putSixel [tree 0.6 400 120 n white blue | n <- [1..12]]
-- Possible improvement: We could change factor every ith iteration so the trees grow a bit taller
tree :: Float -> Float -> Float -> Int -> Colour -> Colour -> Image PixelRGB8
tree factor x width n color backgroundColour = convertRGB8 . ImageRGBA8 $
  drawPicture 800 800 backgroundColour 3.0 [(color, fractalTree factor n ((x, 700), (x+width, 700)))]

----------------- Space-filling curve -----------------
-- Line is bottom line from a square (left to right)
hilbert :: Int -> Line -> Path
hilbert n ((xS, yS), (xE, _)) = hilbert' n 0 0 (xE - xS) 0 0
  where
    hilbert' 1 xDelta yDelta side start end
      | start == 0 && end == 0 = [q3, q2, q1, q4]
      | start == 1 && end == 1 = [q1, q4, q3, q2]
      | start == 0 && end == 1 = [q3, q4, q1, q2]
      | start == 1 && end == 0 = [q1, q2, q3, q4]
      where
        headroom = side / 5
        -- Quadrants
        q1 = (xS + xDelta + side - headroom, yS - yDelta - side + headroom)
        q2 = (xS + xDelta + headroom,        yS - yDelta - side + headroom)
        q3 = (xS + xDelta + headroom,        yS - yDelta - headroom)
        q4 = (xS + xDelta + side - headroom, yS - yDelta - headroom)

    -- start = 0 -> bottom left
    -- start = 1 -> upper right
    -- end = 0 -> bottom right
    -- end = 1 -> end left
    hilbert' n' xDelta yDelta side start end =
      hilbert' (n'-1) (xDelta + start * halfSide)     (yDelta + start * halfSide)     halfSide start     (1-end) ++
      hilbert' (n'-1) (xDelta + end * halfSide)       (yDelta + (1-end) * halfSide)   halfSide start     end ++
      hilbert' (n'-1) (xDelta + (1-start) * halfSide) (yDelta + (1-start) * halfSide) halfSide start     end ++
      hilbert' (n'-1) (xDelta + (1-end) * halfSide)   (yDelta + end * halfSide)       halfSide (1-start) end
      where
        halfSide = side / 2

-- Example: mapM_ putSixel [spaceFillingCurve (50, 750) 700 n white blue | n <- [1..7]]
spaceFillingCurve :: Point -> Float -> Int -> Colour -> Colour -> Image PixelRGB8
spaceFillingCurve (x, y) width n color backgroundColour = convertRGB8 . ImageRGBA8 $
  drawPicture 800 800 backgroundColour 3.0 [(color, hilbert n ((x,y), (x+width, y)))]
