module Fractals where

import Codec.Picture
import LineGraphics

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

----------------- T-square -----------------
data Quadrant = Q1 | Q2 | Q3 | Q4 | None
instance Eq Quadrant where
  (==) Q1 Q1 = True
  (==) Q2 Q2 = True
  (==) Q3 Q3 = True
  (==) Q4 Q4 = True
  (==) None None = True
  (==) _ _ = False

tSquare :: Int -> Point -> Float -> Path
tSquare n center side = result ++ [head result]
  where
    result = tSquare' n center side None

    -- The quadrant indicates which corner is occupied
    tSquare' 1 (x,y) size quadrant
      -- The order of the points is counter-clock wise starting from the occupied quadrant
      | quadrant == None = [(x-s, y+s), (x-s, y-s), (x+s, y-s), (x+s, y+s), (x-s, y+s)]
      | quadrant == Q1 = [(x, y-s), (x-s, y-s), (x-s, y+s), (x+s, y+s), (x+s, y)]
      | quadrant == Q2 = [(x-s, y), (x-s, y+s), (x+s, y+s), (x+s, y-s), (x, y-s)]
      | quadrant == Q3 = [(x, y+s), (x+s, y+s), (x+s, y-s), (x-s, y-s), (x-s, y)]
      | quadrant == Q4 = [(x+s, y), (x+s, y-s), (x-s, y-s), (x-s, y+s), (x, y+s)]
      where
        s = size / 2

    tSquare' n' (x,y) size quadrant
      | quadrant == None = fn4 ++ fn1 ++ fn2 ++ fn3
      | quadrant == Q1 =
        [(x, y-s), (x-hs, y-s)] ++ fn4 ++
        [(x-s, y-hs), (x-s, y+hs)] ++ fn1 ++
        [(x-hs, y+s), (x+hs, y+s)] ++ fn2 ++
        [(x+s, y+hs), (x+s, y)]
      | quadrant == Q2 =
        [(x-s, y), (x-s, y+hs)] ++ fn1 ++
        [(x-hs, y+s), (x+hs, y+s)] ++ fn2 ++
        [(x+s, y+hs), (x+s, y-hs)] ++ fn3 ++
        [(x+hs, y-s), (x, y-s)]
      | quadrant == Q3 =
        [(x, y+s), (x+hs, y+s)] ++ fn2 ++
        [(x+s, y+hs), (x+s, y-hs)] ++ fn3 ++
        [(x+hs, y-s), (x-hs, y-s)] ++ fn4 ++
        [(x-s, y-hs), (x-s, y)]
      | quadrant == Q4 =
        [(x+s, y), (x+s, y-hs)] ++ fn3 ++
        [(x+hs, y-s), (x-hs, y-s)] ++ fn4 ++
        [(x-s, y-hs), (x-s,y+hs)] ++ fn1 ++
        [(x-hs, y+s), (x, y+s)]
      where
        n'' = n' - 1
        s = size / 2
        hs = s / 2
        fn1 = tSquare' n'' (x-s, y+s) s Q1
        fn2 = tSquare' n'' (x+s, y+s) s Q2
        fn3 = tSquare' n'' (x+s, y-s) s Q3
        fn4 = tSquare' n'' (x-s, y-s) s Q4

-- Example:  mapM_ putSixel [square (400, 400) 400 n white blue | n <- [1..7]]
square :: Point -> Float -> Int -> Colour -> Colour -> Image PixelRGB8
square center width n color backgroundColour = convertRGB8 . ImageRGBA8 $
  drawPicture 800 800 backgroundColour 3.0 [(color, tSquare n center width)]
