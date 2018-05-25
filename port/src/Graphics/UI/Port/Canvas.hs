-----------------------------------------------------------------------------------------
{-| Module      :  Canvas
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Drawing on a canvas.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Canvas
             ( -- * Canvas
               withCanvas
             , changeCanvasPen

             -- * Draw            
             , drawPoint
             , drawLine
             , drawChar
             , drawString
             , drawRect
             , drawOval
             , drawCurve
             , drawPolygon
             , drawPolyline
             , drawBitmap
             
             -- * Fill
             , fillRect
             , fillOval
             , fillCurve
             , fillPolygon

             -- * Transform
             , rotateCanvas
             , scaleCanvas
             , shearCanvas
             , translateCanvas
             
             -- * Pen
             , Pen(..), windowPen, dialogPen

             -- ** Font
             , getPenFontMetrics
             , getPenFontCharWidth
             , getPenFontStringWidth

             -- * Resolution
             , mmToVPixels, mmToHPixels
             ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Control.Exception( bracket )

import Graphics.UI.Port.Types
import Graphics.UI.Port.Font    -- for haddock and 'defaultFont'

{-----------------------------------------------------------------------------------------
  Canvas
-----------------------------------------------------------------------------------------}
-- | The pencil embodies common drawing attributes.
data Pen = Pen 
            { penSize       :: !Int          -- ^ Thickness of the pencil
            , penMode       :: !DrawMode
            , penColor      :: !Color
            , penBackColor  :: !Color
            , penBkDrawMode :: !Bool
            , penFont       :: !Font
            , penJoinStyle  :: !JoinStyle
            , penCapStyle   :: !CapStyle
            , penLineStyle  :: !LineStyle
            , penHatchStyle :: !HatchStyle
            }
            deriving Eq

-- | Create a pen with default drawing values for windows. That is:
--
-- @'Pen'@ 1 @'DrawCopy'@ @'textColor'@ @'windowColor'@ @'False'@ @'windowColor'@ @'JoinMiter'@ @'CapFlat'@ @'LineSolid'@ @'HatchSolid'@
windowPen :: Pen
windowPen
  = Pen 1 DrawCopy textColor windowColor False defaultFont JoinMiter CapFlat LineSolid HatchSolid

-- | Create a pen with default drawing values for dialogs. That is:
--
-- @'Pen'@ 1 @'DrawCopy'@ @'textColor'@ @'dialogColor'@ @'False'@ @'dialogFont'@ @'JoinMiter'@ @'CapFlat'@ @'LineSolid'@ @'HatchSolid'@
dialogPen :: Pen
dialogPen
  = Pen 1 DrawCopy textColor dialogColor False defaultFont JoinMiter CapFlat LineSolid HatchSolid



-- | Initialize a canvas and it's initial drawing pencil. Should be called before using
-- any of the drawing operations.
withCanvas :: Pen -> BufferMode -> CanvasHandle -> IO a -> IO a
withCanvas pen buffermode canvas action = do
    (withCFont (penFont pen) $ \cfont ->
     withCLineStyle (penLineStyle pen) $ \cline clinecount clinestyles ->
     withCHatchStyle (penHatchStyle pen) $ \chatch chatchbmp ->
     osInitCanvas  (toCInt (penSize pen))
                  (toCDrawMode (penMode pen)) 
                  (toCColor (penColor pen)) 
                  (toCColor (penBackColor pen)) 
                  (toCJoinStyle (penJoinStyle pen))
                  (toCCapStyle (penCapStyle pen))
                  cline clinecount clinestyles
                  (toCBool (penBkDrawMode pen))
                  chatch chatchbmp
                  cfont
                  canvas 
                  (toCBufferMode buffermode))
    r <- action
    osDoneCanvas canvas
    return r

foreign import ccall osInitCanvas :: CInt -> CInt -> CColor -> CColor
                                  -> CInt -> CInt 
                                  -> CInt -> CInt -> Ptr CUChar
                                  -> CBool 
                                  -> CInt -> BitmapHandle 
                                  -> FontHandle 
                                  -> CanvasHandle -> CBool -> IO ()

-- | Release any resources associated with the canvas. Must be called 
-- after 'initCanvas', when all drawing operations have been performed.
foreign import ccall osDoneCanvas :: CanvasHandle -> IO ()


{-----------------------------------------------------------------------------------------
  Pen
-----------------------------------------------------------------------------------------}

changeCanvasPen :: Pen -> CanvasHandle -> IO ()
changeCanvasPen pen canvas =
    withCFont (penFont pen) $ \cfont ->
    withCLineStyle (penLineStyle pen) $ \cline clinecount clinestyles ->
    withCHatchStyle (penHatchStyle pen) $ \chatch chatchbmp ->
    osChangeCanvasPen (toCInt (penSize pen))
                  (toCDrawMode (penMode pen)) 
                  (toCColor (penColor pen))
                  (toCColor (penBackColor pen))
                  (toCJoinStyle (penJoinStyle pen))
                  (toCCapStyle (penCapStyle pen))
                  cline clinecount clinestyles
                  (toCBool (penBkDrawMode pen))
                  chatch chatchbmp
                  cfont
                  canvas
foreign import ccall osChangeCanvasPen :: CInt -> CInt -> CColor -> CColor
                                  -> CInt -> CInt
                                  -> CInt -> CInt -> Ptr CUChar
                                  -> CBool
                                  -> CInt -> BitmapHandle
                                  -> FontHandle
                                  -> CanvasHandle -> IO ()


{-----------------------------------------------------------------------------------------
  Font & Text metrics
-----------------------------------------------------------------------------------------}

-- | The metrics for the current pen font. (See also 'getFontMetrics').
getPenFontMetrics :: CanvasHandle -> IO FontMetrics
getPenFontMetrics canvas
  = withCFontMetricsResult $ \pascent pdescent pmaxwidth pleading ->
    osGetPenFontMetrics canvas pascent pdescent pmaxwidth pleading
foreign import ccall osGetPenFontMetrics :: CanvasHandle -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | The the pixel width of a character in the current pen font. (See also 'getFontCharWidth').
getPenFontCharWidth :: Char -> CanvasHandle -> IO Int
getPenFontCharWidth c canvas
  = do cw <- osGetCharWidth (toCChar c) canvas
       return (fromCInt cw)
foreign import ccall osGetCharWidth :: CChar -> CanvasHandle -> IO CInt

-- | Get the pixel width of a string in the current pen font. (See also 'getFontStringWidth').
getPenFontStringWidth :: String -> CanvasHandle -> IO Int
getPenFontStringWidth str canvas
  = withCString str $ \cstr ->
    do cw <- osGetStringWidth cstr canvas
       return (fromCInt cw)
foreign import ccall osGetStringWidth :: CString -> CanvasHandle -> IO CInt


{-----------------------------------------------------------------------------------------
  
-----------------------------------------------------------------------------------------}
drawPoint :: Point -> CanvasHandle -> IO ()
drawPoint p canvas 
  = withCPoint p $ \x y ->
    osDrawPoint x y canvas
foreign import ccall osDrawPoint :: CInt -> CInt -> CanvasHandle -> IO ()

drawLine :: Point -> Point -> CanvasHandle -> IO ()
drawLine p0 p1 canvas 
  = withCPoint p0 $ \x0 y0 ->
    withCPoint p1 $ \x1 y1 ->
    osDrawLine x0 y0 x1 y1 canvas
foreign import ccall osDrawLine :: CInt -> CInt -> CInt -> CInt -> CanvasHandle -> IO ()

drawChar :: Point -> Char -> CanvasHandle -> IO ()
drawChar p c canvas 
  = withCPoint p $ \x y ->
    osDrawChar x y (toCChar c) canvas
foreign import ccall osDrawChar :: CInt -> CInt -> CChar -> CanvasHandle -> IO ()

drawString :: Point -> String -> CanvasHandle -> IO ()
drawString p s canvas 
  = withCPoint p $ \x y ->
    withCString s $ \cs ->
    osDrawString x y cs canvas
foreign import ccall osDrawString :: CInt -> CInt -> CString -> CanvasHandle -> IO ()


{-----------------------------------------------------------------------------------------
  Bitmaps
-----------------------------------------------------------------------------------------}
drawBitmap :: Point -> Bitmap -> CanvasHandle -> IO ()
drawBitmap p bitmap canvas 
  = withCPoint p $ \cx cy -> 
    withCBitmap bitmap $ \bh ->
    osDrawBitmap cx cy bh canvas
foreign import ccall osDrawBitmap :: CInt -> CInt -> BitmapHandle -> CanvasHandle -> IO ()


{-----------------------------------------------------------------------------------------
  Ovals
-----------------------------------------------------------------------------------------}
-- | Draw an oval bounded by a rectangle.
drawOval :: Rect -> CanvasHandle -> IO ()
drawOval rect canvas 
  = withCRect rect $ \x0 y0 x1 y1 ->
    osDrawOval x0 y0 x1 y1 canvas
foreign import ccall osDrawOval :: CInt -> CInt -> CInt -> CInt -> CanvasHandle -> IO ()

-- | Fill an oval bounded by a rectangle.
fillOval :: Rect -> CanvasHandle -> IO ()
fillOval  rect canvas
  = withCRect rect $ \x0 y0 x1 y1 ->
    osFillOval x0 y0 x1 y1 canvas
foreign import ccall osFillOval :: CInt -> CInt -> CInt -> CInt -> CanvasHandle -> IO ()

{-----------------------------------------------------------------------------------------
  Curves
-----------------------------------------------------------------------------------------}
-- | The expression (@drawCurve rect start end canvas@) draws a curve on the oval bounded by
-- by @rect@, starting at an angle @start@ (in radians) continuing in clockwise direction to the ending
-- angle @end@ (in radians).
drawCurve :: Rect -> Float -> Float -> CanvasHandle -> IO ()
drawCurve rect start end canvas 
  = withCRect rect $ \x0 y0 x1 y1 ->
    osDrawCurve x0 y0 x1 y1 start end (toCBool True) canvas
foreign import ccall osDrawCurve :: CInt -> CInt -> CInt -> CInt -> Float -> Float -> CBool -> CanvasHandle -> IO ()

-- | The expression (@fillCurve rect start end canvas@) fills a pie bounded by the line from 
-- the center of the oval bounded by @rect@ at an angle @start@ (in radians), continuing over
-- the oval in clockwise direction and back on the line to the center of the oval at an 
-- angle @end@ (in radians).
fillCurve :: Rect -> Float -> Float -> CanvasHandle -> IO ()
fillCurve  rect start end canvas
  = withCRect rect $ \x0 y0 x1 y1 ->
    osFillCurve x0 y0 x1 y1 start end (toCBool True) canvas
foreign import ccall osFillCurve :: CInt -> CInt -> CInt -> CInt -> Float -> Float -> CBool -> CanvasHandle -> IO ()


{-----------------------------------------------------------------------------------------
  Rectangles
-----------------------------------------------------------------------------------------}
drawRect :: Rect -> CanvasHandle -> IO ()
drawRect rect canvas 
  = withCRect rect $ \x0 y0 x1 y1 -> osDrawRect x0 y0 x1 y1 canvas
foreign import ccall osDrawRect :: CInt -> CInt -> CInt -> CInt -> CanvasHandle -> IO ()

fillRect :: Rect -> CanvasHandle -> IO ()
fillRect rect canvas
  = withCRect rect $ \x0 y0 x1 y1 -> osFillRect x0 y0 x1 y1 canvas
foreign import ccall osFillRect :: CInt -> CInt -> CInt -> CInt -> CanvasHandle -> IO ()

{-----------------------------------------------------------------------------------------
  Polygons
-----------------------------------------------------------------------------------------}
type PolygonHandle  = Ptr PH
data PH = PH

-- | Draw a poly line.
drawPolyline :: [Point] -> CanvasHandle -> IO ()
drawPolyline ps canvas
  = withCPolygon ps $ \cpoly ->
    osDrawPolyline cpoly canvas
foreign import ccall osDrawPolyline :: PolygonHandle -> CanvasHandle -> IO ()


-- | Draw a polygon. The polygon is automatically closed.
drawPolygon :: [Point] -> CanvasHandle -> IO ()
drawPolygon ps canvas
  = withCPolygon ps $ \cpoly ->
    osDrawPolygon cpoly canvas
foreign import ccall osDrawPolygon :: PolygonHandle -> CanvasHandle -> IO ()

-- | Draw a filled polygon.
fillPolygon :: [Point] -> CanvasHandle -> IO ()
fillPolygon ps canvas
  = withCPolygon ps $ \cpoly ->
    osFillPolygon cpoly canvas
foreign import ccall osFillPolygon :: PolygonHandle -> CanvasHandle -> IO ()

withCPolygon :: [Point] -> (PolygonHandle -> IO ()) -> IO ()
withCPolygon [] f
  = return ()
withCPolygon ps f
  = bracket initPolygon osDeletePolygon f
  where
    initPolygon 
      = do cpoly <- osCreatePolygon (toCInt (length ps))
           mapM_ (addPoint cpoly) ps
           return cpoly

    addPoint cpoly p  
      = withCPoint p $ \x y -> osAddPolygonPoint cpoly x y

foreign import ccall osCreatePolygon   :: CInt -> IO PolygonHandle
foreign import ccall osAddPolygonPoint :: PolygonHandle -> CInt -> CInt -> IO ()
foreign import ccall osDeletePolygon   :: PolygonHandle -> IO ()


{-----------------------------------------------------------------------------------------
  Misc
-----------------------------------------------------------------------------------------}
-- | Convert millimeters to on-screen vertical pixels. 
foreign import ccall "osMMtoVPixels" mmToVPixels :: Double -> IO Int

-- | Convert millimeters to on-screen horizontal pixels
foreign import ccall "osMMtoHPixels" mmToHPixels :: Double -> IO Int

{-----------------------------------------------------------------------------------------
  World transformations
-----------------------------------------------------------------------------------------}
-- | Rotate the canvas clockwise with an angle in radians.
foreign import ccall "osRotateCanvas" rotateCanvas :: Double -> CanvasHandle -> IO ()

-- | Scale the canvas with a horizontal and vertical factor.
foreign import ccall "osScaleCanvas" scaleCanvas :: Double -> Double -> CanvasHandle -> IO ()

-- | Shear the canvas in a horizontal and vertical direction.
foreign import ccall "osShearCanvas" shearCanvas :: Double -> Double -> CanvasHandle -> IO ()

-- | Translate (or move) the canvas in a horizontal and vertical direction.
foreign import ccall "osTranslateCanvas" translateCanvas :: Double -> Double -> CanvasHandle -> IO ()
