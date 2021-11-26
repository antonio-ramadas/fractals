-- Mostly copied from http://learn.hfm.io/fractals.html

module LineGraphics (
  Point, Vector, Line, Path, Picture, Colour,
  white, black, blue, red, green, orange, magenta, lightgreen, darkblue,
  drawPicture,
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
--
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

-- Render a picture composed of coloured line paths with the specified line width.
--
drawPicture :: Int -> Int -> Colour -> Float -> Picture -> Image PixelRGBA8
drawPicture width height backgroundColour linewidth picture
  = renderDrawing width height (toColour backgroundColour) $
      mapM_ (\(col, path) -> withTexture (uniformTexture $ toColour col) (drawPath path)) picture
  where
    drawPath points    = stroke linewidth  JoinRound (CapRound, CapStraight 0) $
                           polyline (map (uncurry V2) points)
    toColour (a,b,c,d) = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)




