-- Mostly copied from http://learn.hfm.io/fractals.html

module LineGraphics (
  Point, Vector, Line, Path, Picture, Colour,
  white, black, blue, red, green, orange, magenta, lightgreen, darkblue,
  connectLine, scaleLine, rotateLine, polygon, drawPicture,
) where

-- Rasterific
import Graphics.Rasterific hiding (Point, Vector, Line, Path, polygon)
import Graphics.Rasterific.Texture

-- JuicyPixels
import Codec.Picture

type Point   = (Float, Float)
type Vector  = (Float, Float)
type Line    = (Point, Point)
type Path    = [Point]
type Picture = [(Colour, Path)]
type Colour  = (Int, Int, Int, Int) -- red, green, blue, opacity

-- Predefined colours
white, black, blue, red, green, orange, magenta, lightgreen, darkblue :: Colour
white      = (200,  200, 255, 255)
black      = (  0,    0,   0, 255)
blue       = (  0,  110, 255, 255)
red        = (255,    0,   0, 255)
green      = (10,  255,  10,  235)
orange     = (255,  255,  0,  200)
magenta    = (153,    0, 153, 220)
lightgreen = ( 27,  230,  34, 255)
darkblue   = ( 24,   50, 194, 255)

-- Shifts a line to start at a given point
startLineFrom :: Point -> Line -> Line
startLineFrom startPoint@(x0, y0) ((xS, yS), (xE, yE))
  = (startPoint, (x0 + xE - xS, y0 + yE - yS))

-- Shifts second line to start where the first one ended
connectLine :: Line -> Line -> Line
connectLine (_, pE) line2 = startLineFrom pE line2

-- Scale line given a scale factor (start point does not move)
scaleLine :: Float -> Line -> Line
scaleLine factor ((x1, y1), (x2, y2))
  = ((x1, y1), (x' + x1, y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = factor * x0
    y' = factor * y0

-- Rotates a line where the start point is the reference of the axis
rotateLine :: Float -> Line -> Line
rotateLine alpha ((x1, y1), (x2, y2))
  = ((x1, y1), (x' + x1, y' + y1))
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x' = x0 * cos alpha - y0 * sin alpha
    y' = x0 * sin alpha + y0 * cos alpha

-- Creates a spiral with n lines along a given angle
spiral :: Float -> Float -> Int -> Line -> Path
spiral angle scaleFactor n line
  = spiral' n line
  where
    spiral' n line@(p1, p2)
      | n <= 0    = []
      | otherwise = p1 : spiral' (n - 1) newLine
      where
        newLine = connectLine line (scaleLine scaleFactor (rotateLine angle line))

-- Creates a _circle_ with n lines
polygon :: Int -> Line -> Path
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line
  where
    rotationAngle = (2 * pi) / (fromIntegral n)

-- Render a picture composed of coloured line paths with the specified line width.
drawPicture :: Int -> Int -> Colour -> Float -> Picture -> Image PixelRGBA8
drawPicture width height backgroundColour linewidth picture
  = renderDrawing width height (toColour backgroundColour) $
      mapM_ (\(col, path) -> withTexture (uniformTexture $ toColour col) (drawPath path)) picture
  where
    drawPath points    = stroke linewidth  JoinRound (CapRound, CapStraight 0) $
                           polyline (map (uncurry V2) points)
    toColour (a,b,c,d) = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
