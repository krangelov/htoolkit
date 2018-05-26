-----------------------------------------------------------------------------------------
{-| Module      :  Canvas
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Widgets that can be (re)painted are an instance of the 'Paint' class. By
    setting the 'paint' attribute, a custom paint function can be specified.
    For example:

    > do w <- window [paint =: mypaint]
    >    ...
    > where
    >   mypaint canvas updateFrame updateAreas
    >     = do setCanvasPen canvas [color =: red]
    >          fillRect (rect (pt 10 10) (pt 20 20)) canvas
    >          setCanvasPen canvas [color =: blue, thickness =: 10]
    >          drawLine (pt 20 20) (pt 30 30) canvas

    A paint function (of type 'PaintFunction') takes three arguments, the canvas
    (of type 'Canvas'), the bounding rectangle of the update frame and all areas that
    need to be repainted. 
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Canvas
              ( 
              -- * Canvas
                Canvas, CanvasPen
              , setCanvasPen, getCanvasPen
              
              , paintInBitmap

              -- * Drawing primitives
              , drawPoint
              , drawString
              , drawLine
              , drawPolyline
              , drawRect 
              , fillRect
              , drawOval
              , drawEllipse
              , drawCircle
              , fillOval 
              , fillEllipse 
              , fillCircle 
              , drawCurve 
              , drawArc 
              , fillPie 
              , drawPolygon
              , fillPolygon
              , drawBitmap 
              
              -- * Transform
              , rotateCanvas
              , scaleCanvas
              , shearCanvas
              , translateCanvas
              
              -- * Font metrics
              , fontMetrics
              , fontCharWidth
              , fontStringWidth

              , canvasFontMetrics
              , canvasFontCharWidth
              , canvasFontStringWidth

              -- * Internal
              , withCanvas
              ) where

import qualified Graphics.UI.Port as Port
import Graphics.UI.Port( Pen(..) )
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Bitmap
import Control.Monad( when )

--------------------------------------------------------------------
--  Canvas
--------------------------------------------------------------------

-- | A canvas is an area on which you can draw objects.
data Canvas   = Canvas{ hcanvas :: CanvasHandle
                      , vpen    :: Var Pen
                      , vmode   :: BufferMode
                      }
data CanvasPen = CanvasPen (Var Pen) BufferMode

instance Drawn CanvasPen where
  pen  = newAttr (\(CanvasPen vpen _) -> getVar vpen)
                 (\(CanvasPen vpen _) -> setVar vpen)
                 
  bufferMode = readAttr "bufferMode" (\(CanvasPen _ vmode) -> return vmode)

instance HasFont CanvasPen where
  font = mapAttr penFont (\pen c -> pen{penFont=c}) pen

setCanvasPen :: Canvas -> [Prop CanvasPen] -> IO ()
setCanvasPen (Canvas handle vpen vmode) props = do
  set (CanvasPen vpen vmode) props
  pen <- getVar vpen  
  Port.changeCanvasPen pen handle

getCanvasPen :: Canvas -> Attr CanvasPen a -> IO a
getCanvasPen (Canvas handle vpen vmode) = get (CanvasPen vpen vmode)


-- | The font metrics (read-only). 
--
-- > do metrics <- get canvas (fontMetrics defaultFont)
--
fontMetrics :: Font -> Attr Canvas FontMetrics
fontMetrics font
  = readAttr "fontMetrics" $ \canvas -> Port.getFontMetrics font (hcanvas canvas)

-- | The width of a character in a certain font (read-only).
fontCharWidth :: Font -> Char -> Attr Canvas Int
fontCharWidth font c 
  = readAttr "fontCharWidth" $ \canvas -> Port.getFontCharWidth font c (hcanvas canvas)

-- | The width of a string in a certain font (read-only).
fontStringWidth :: Font -> String -> Attr Canvas Int
fontStringWidth font s 
  = readAttr "fontStringWidth" $ \canvas -> Port.getFontStringWidth font s (hcanvas canvas)

-- | The font metrics of the current drawing pencil (read-only).
canvasFontMetrics ::  Attr Canvas FontMetrics
canvasFontMetrics 
  = readAttr "penfontMetrics" $ \canvas -> Port.getPenFontMetrics (hcanvas canvas)

-- | The character width in the current pen font on a canvas (read-only). 
--
-- > do em <- get canvas (canvasFontCharWidth 'm')
canvasFontCharWidth ::Char -> Attr Canvas Int
canvasFontCharWidth c 
  = readAttr "penfontCharWidth" $ \canvas -> Port.getPenFontCharWidth c (hcanvas canvas)

-- | The string width in the current pen font on a canvas (read-only). 
canvasFontStringWidth :: String -> Attr Canvas Int
canvasFontStringWidth s 
  = readAttr "penfontStringWidth" $ \canvas -> Port.getPenFontStringWidth s (hcanvas canvas)

--  Paint on a primitive canvas. Just for internal use.
withCanvas :: BufferMode -> Pen -> CanvasHandle -> (Canvas -> IO a) -> IO a
withCanvas bmode pen handle f
  = do vpen <- newVar pen
       Port.withCanvas pen bmode handle (f (Canvas handle vpen bmode))

-- | The paintInBitmap executes the given function with canvas
-- associated with given Bitmap.
paintInBitmap :: Bitmap -> Pen -> (Canvas -> IO a) -> IO a
paintInBitmap bmp pen f = do
   Port.drawInBitmap bmp (\hcanvas -> withCanvas UnBuffered pen hcanvas f)

--------------------------------------------------------------------
-- Drawing primitives
--------------------------------------------------------------------

-- | Draws a point at the specified location.
drawPoint :: Point -> Canvas -> IO ()
drawPoint p can = Port.drawPoint p (hcanvas can)

-- | Draws the specified text string at the specified location.
drawString :: Point -> String -> Canvas -> IO ()
drawString p txt can = Port.drawString p txt (hcanvas can)

-- | Draws a line connecting the two points specified by coordinate pairs.
drawLine :: Point -> Point -> Canvas -> IO ()
drawLine p0 p1 can = Port.drawLine p0 p1 (hcanvas can)

-- | Draws a series of line segments that connect an list of points.
drawPolyline :: [Point] -> Canvas -> IO ()
drawPolyline points can = Port.drawPolyline points (hcanvas can)

-- | Draws a rectangle specified by a Rect.
drawRect :: Rect -> Canvas -> IO ()
drawRect frame can = Port.drawRect frame (hcanvas can)

-- | Fills the interior of a rectangle specified by a Rect.
fillRect :: Rect -> Canvas -> IO ()
fillRect frame can = Port.fillRect frame (hcanvas can)

-- | Draws an ellipse specified by a bounding rectangle.
drawOval :: Rect -> Canvas -> IO ()
drawOval frame can = Port.drawOval frame (hcanvas can)

-- | Draw an ellipse specified by a center point and the x- and y radius.
drawEllipse :: Point -> Int -> Int -> Canvas -> IO ()
drawEllipse (Point x y) rx ry can 
  = Port.drawOval (Rect (x-rx) (y-ry) (x+rx) (y+ry)) (hcanvas can)

-- | Draw an circle specified by a center point and the radius.
drawCircle :: Point -> Int -> Canvas -> IO ()
drawCircle (Point x y) r can
  = Port.drawOval (Rect (x-r) (y-r) (x+r) (y+r)) (hcanvas can)

-- | Fills the interior of an ellipse defined by a bounding rectangle specified by a Rect.
fillOval :: Rect -> Canvas -> IO ()
fillOval frame can = Port.fillOval frame (hcanvas can)

-- | Fills the interior of an ellipse specified by a center point and the x- and y radius.
fillEllipse :: Point -> Int -> Int -> Canvas -> IO ()
fillEllipse (Point x y) rx ry can = Port.fillOval (Rect (x-rx) (y-ry) (x+rx) (y+ry)) (hcanvas can)

-- | Fills the interior of a circle specified by a center point and the radius.
fillCircle :: Point -> Int -> Canvas -> IO ()
fillCircle (Point x y) r can = Port.fillOval (Rect (x-r) (y-r) (x+r) (y+r)) (hcanvas can)

-- | Draws an curve representing a portion of an ellipse specified by a Rect. The Float type
-- arguments specifies @start@ and @end@ angles in radians. The curve starts at an angle @start@
-- continuing in clockwise direction to the ending angle @end@.
drawCurve :: Rect -> Float -> Float -> Canvas -> IO ()
drawCurve frame start end can = Port.drawCurve frame start end (hcanvas can)

-- | Draw an arc. The expression (arc c rx ry start end [] canvas) draws a curve on the oval 
-- defined by the center point @c@, the x radius @rx@ and the y radius @ry@. The curve  starts 
-- at an angle @start@ (in radians) continuing in clockwise direction 
-- to the ending angle @end@ (in radians).
drawArc :: Point -> Int -> Int -> Float -> Float -> Canvas -> IO ()
drawArc (Point x y) rx ry start end can
  = Port.drawCurve (Rect (x-rx) (y-ry) (x+rx) (y+ry)) start end (hcanvas can)
  
-- | Fills the interior of a pie section defined by an ellipse specified by a by a center point and the x- and y radius
-- and two radial lines at angles @start@ and @end@. The Float arguments specifies the angles.
fillPie :: Point -> Int -> Int -> Float -> Float -> Canvas -> IO ()
fillPie (Point x y) rx ry start end can = Port.fillCurve (Rect (x-rx) (y-ry) (x+rx) (y+ry)) start end (hcanvas can)

-- | Draws a polygon defined by an list of points
drawPolygon :: [Point] -> Canvas -> IO ()
drawPolygon points can = Port.drawPolygon points (hcanvas can)

-- | Draws a polygon defined by an list of points
fillPolygon :: [Point] -> Canvas -> IO ()
fillPolygon points can = Port.fillPolygon points (hcanvas can)

-- | Draws the specified Bitmap at the specified location.
drawBitmap :: Point -> Bitmap -> Canvas -> IO ()
drawBitmap p bitmap can = Port.drawBitmap p bitmap (hcanvas can)

--------------------------------------------------------------------
-- Transformations
--------------------------------------------------------------------

-- | Rotate the canvas clockwise with an angle in radians.
rotateCanvas :: Double -> Canvas -> IO ()
rotateCanvas angle can = Port.rotateCanvas angle (hcanvas can)

-- | Scale the canvas with a horizontal and vertical factor.
scaleCanvas :: Double -> Double -> Canvas -> IO ()
scaleCanvas sx sy can = Port.scaleCanvas sx sy (hcanvas can)

-- | Shear the canvas in a horizontal and vertical direction.
shearCanvas :: Double -> Double -> Canvas -> IO ()
shearCanvas sx sy can = Port.shearCanvas sx sy (hcanvas can)

-- | Translate (or move) the canvas in a horizontal and vertical direction.
translateCanvas :: Double -> Double -> Canvas -> IO ()
translateCanvas dx dy can = Port.translateCanvas dx dy (hcanvas can)
