-----------------------------------------------------------------------------------------
{-| Module      :  Types
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Basic types.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Types
            (
            -- * Geometry
            -- ** Size
              Size(..), sz
            , sizeEncloses
            , maxSize, addh, addv, addSize, sizeDistance

            -- ** Point
            , Point(..), pt
	    , pointMove
	    , pointAdd, pointSub, pointScale

            -- ** Rectangle
            , Rect(..)

            -- *** Construction
            , rect
            , rectAt
            , rectOfSize
            , pointToRect

            -- *** Access
            , topLeft, topRight, bottomLeft, bottomRight
            , rectSize
            
            -- *** Calculations
            , rectMove, rectMoveTo, rectStretchTo
	    , rectUnion
	    , rectSect
	    , disjointRects
            , rectsDiff
            , centralPoint
            , centralRect
            , rectIsEmpty
            , pointInRect

            -- * Events

            -- ** Modifiers
            , Modifiers(..)
            , noneDown, noModifiers
            , justShift, justAlt, justControl
            
            -- ** Mouse events
            , MouseEvent(..)
            , mousePos, mouseModifiers

            -- ** Keyboard events
            , KeyboardEvent(..), Key(..)
            , keyboardKey, keyboardRepeat, keyModifiers
            
            -- * Document interface
            , DocumentInterface(..)
            
            -- * PositionType
            , PositionType(..)

            -- * WindowPosition
            , WindowPosition(..)

            -- * Render
            
            -- ** Colors
            , module Graphics.UI.Port.Colors

            -- ** Bitmaps
            , Bitmap
            , Codec(..)

            -- ** Canvas 
            , DrawMode(..)
            , BufferMode(..)
            , JoinStyle(..)
            , CapStyle(..)
            , LineStyle(..)
            , FillStyle(..)
            , Pen(..), windowPen, dialogPen

            -- ** Fonts
            , Font 
            , fontDef
            , FontDef(..), FontName, FontSize, FontWeight, FontStyle(..)
            , fontMinWeight, fontMaxWeight, fontNormalWeight, fontBoldWeight
            , FontMetrics(..)
            , fontLineHeight

            -- * Utility
            -- ** Variables
            , Var
            , newVar, getVar, setVar, updateVar, takeVar, putVar
             
            -- ** Misc.
            , bounded

            -- * Internal
            , WindowHandle
            , CanvasHandle
            , MenuHandle
            , ToolHandle
	        , ActionHandle
	        , IndicatorHandle
            , FontHandle
            , BitmapHandle
            , TimerHandle
            , nullHandle
            ) where

import Graphics.UI.Port.Types
import Graphics.UI.Port.Colors
import Graphics.UI.Port.Canvas(Pen(..), windowPen, dialogPen)
import Control.Concurrent.MVar
{--------------------------------------------------------------------
  Misc.
--------------------------------------------------------------------}

bounded :: Ord a => a -> a -> a -> a
bounded lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise = x

{--------------------------------------------------------------------
  Var
--------------------------------------------------------------------}
type Var a    = MVar a

newVar :: a -> IO (Var a)
newVar x      = newMVar x

takeVar, getVar :: Var a -> IO a
getVar        = readMVar
takeVar v     = takeMVar v

putVar :: Var a -> a -> IO ()
putVar v x    = putMVar v x

setVar :: Var a -> a -> IO ()
setVar v x    = do takeVar v; putVar v x

updateVar :: Var a -> (a -> a) -> IO a
updateVar v f = do x <- takeVar v; putVar v (f x); return x

{--------------------------------------------------------------------
  Color
--------------------------------------------------------------------}

getColorRGB :: Color -> (Int,Int,Int)
getColorRGB c
  = (fromIntegral $ colorRed c, fromIntegral $ colorGreen c, fromIntegral $ colorBlue c)
