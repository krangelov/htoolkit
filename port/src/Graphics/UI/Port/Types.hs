{-# LANGUAGE CPP, RankNTypes #-}

-- #hide
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
module Graphics.UI.Port.Types
            (
            -- * Geometry

            -- ** Points
              Point(..), pt, pointMove, pointAdd, pointSub, pointScale

            -- ** Sizes
            , Size(..), sz, sizeEncloses, maxSize, addh, addv, addSize, sizeDistance

            -- ** Rectangles
            , Rect(..), topLeft, topRight, bottomLeft, bottomRight
            , rect, rectAt, rectSize, rectOfSize, rectIsEmpty
            , pointInRect, rectMove, rectMoveTo, pointToRect, centralPoint, centralRect, rectStretchTo
            , disjointRects, rectsDiff, rectUnion, rectSect

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

            -- ** Fonts
            , Font
            , fontDef
            , FontDef(..), FontName, FontSize, FontWeight, FontStyle(..)
            , fontMinWeight, fontMaxWeight, fontNormalWeight, fontBoldWeight
            , FontMetrics(..)
            , fontLineHeight

            -- * Events

            -- ** Modifiers
            , Modifiers(..)
            , noneDown, justShift, justAlt, justControl, noModifiers

            -- ** Mouse events
            , MouseEvent(..)
            , mousePos, mouseModifiers

            -- ** Keyboard events
            , KeyboardEvent(..), Key(..), keyModifiers
            , keyboardKey, keyboardRepeat

            -- * Document interface
            , DocumentInterface(..)

            -- * PositionType
            , PositionType(..)

            -- * Window position
            , WindowPosition(..)

            -- * Primitive Handles
            , WindowHandle
            , CanvasHandle
            , MenuHandle
            , FontHandle
            , BitmapHandle
            , TimerHandle
            , ToolHandle
            , ActionHandle
            , IndicatorHandle
            , RowHandle
            , nullHandle

            -- * Marshalling to C
            , toCDrawMode, toCBufferMode
            , toCJoinStyle, toCCapStyle
            , withCLineStyle, withCFillStyle

            , withCBitmap, fromCBitmap

            , withCFont, fromCFont
            , withCFontDef, withCFontDefResult, fromCFontDef, fromCStyle, fromCWeight
            , withCFontMetricsResult, fromCFontMetrics

            , withCPoint, withCPointResult, fromCPoint
            , withCSize, withCSizeResult, fromCSize
            , withCRect, withCRectResult, fromCRect

            , fromCModifiers, toCModifiers
            , fromCKey, toCKey
            , fromCMouseEvent
            , fromCKeyboardEvent

            , toCDocumentInterface

            , toCPositionType, fromCPositionType

            , withCWindowPosition

            , fromCInt, toCInt
            , CWord, fromCWord, toCWord
            , CBool, fromCBool, toCBool
            , fromCChar, toCChar
            , withCStrings, peekCStrings, resultCString, resultCStrings
            , PortString, newPortString, withPortString, resultPortString
            ) where

import Foreign
import Foreign.C
import Foreign.Marshal.Alloc
import Control.Exception( bracket )
import Data.Bits
import Graphics.UI.Port.Colors

{-----------------------------------------------------------------------------------------
  Handles: should be Storable values.
-----------------------------------------------------------------------------------------}
-- | Abstract handle to a window
type WindowHandle = Ptr WH
data WH = WH

-- | Abstract handle to a drawing context
type CanvasHandle = Ptr CH
data CH = CH

-- | Abstract handle to a menu 
type MenuHandle   = Ptr MH
data MH = MH

-- | Abstract handle to a font
type FontHandle   = Ptr FH
data FH = FH

type GradientHandle = Ptr GH
data GH = GH

-- | Abstract handle to a bitmap
type BitmapHandle = Ptr BH
data BH = BH

-- | Abstract handle to a timer
type TimerHandle = Ptr TH
data TH = TH

-- | Abstract handle to a tool button
type ToolHandle = Ptr TLH
data TLH = TLH

-- | Abstract handle to an action
type ActionHandle = Ptr ACT
data ACT = ACT

-- | Abstract handle to a indicator in the status bar
type IndicatorHandle = Ptr IH
data IH = IH

-- | Abstract handle to a indicator in the status bar
type RowHandle = Ptr RH
data RH = RH

-- | A null handle. Use with care.
nullHandle :: Ptr a
nullHandle
  = nullPtr

{-----------------------------------------------------------------------------------------
  Point
-----------------------------------------------------------------------------------------}
-- | A point has an x and y coordinate. Coordinates are normally relative to the
-- upper-left corner of their view frame, where a positive x goes to the right and
-- a positive y to the bottom of the view.
data Point  = Point
    { px :: !Int -- ^ x component of a point.
    , py :: !Int -- ^ y component of a point.
    }
    deriving (Eq,Show,Read)

-- | Short function to construct a point.
pt :: Int -> Int -> Point
pt x y  = Point x y

pointMove :: Size -> Point -> Point
pointMove (Size w h) (Point x y) = Point (x + w) (y + h)

pointAdd :: Point -> Point -> Point
pointAdd (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

pointSub :: Point -> Point -> Point
pointSub (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

pointScale :: Int -> Point -> Point
pointScale v (Point x y) = Point (v*x) (v*y)


-- marshalling
withCPoint :: Point -> (CInt -> CInt -> IO a) -> IO a
withCPoint (Point x y) f
  = f (toCInt x) (toCInt y)

withCPointResult :: (Ptr CInt -> IO ()) -> IO Point
withCPointResult f
  = allocaArray 2 $ \cpoint ->
    do f cpoint
       [x,y] <- peekArray 2 cpoint
       return (fromCPoint x y)

fromCPoint :: CInt -> CInt -> Point
fromCPoint x y
  = Point (fromCInt x) (fromCInt y)

{-----------------------------------------------------------------------------------------
  Size
-----------------------------------------------------------------------------------------}
-- | A @Size@ has a width and height.
data Size   = Size
    { sw :: !Int -- ^ the width  of a size
    , sh :: !Int -- ^ the height of a size
    }
    deriving (Eq,Show,Read)

-- | Short function to construct a size
sz :: Int -> Int -> Size
sz w h  = Size w h

sizeEncloses :: Size -> Size -> Bool
sizeEncloses (Size w0 h0) (Size w1 h1)
  = (w0 >= w1) && (h0 >= h1)

maxSize :: Size -> Size -> Size
maxSize (Size w1 h1) (Size w2 h2) = Size (max w1 w2) (max h1 h2)

addh :: Size -> Size -> Size
addh (Size w1 h1) (Size w2 h2) = Size (w1+w2) (max h1 h2)

addv :: Size -> Size -> Size
addv (Size w1 h1) (Size w2 h2) = Size (max w1 w2) (h1+h2)

addSize :: Size -> Size -> Size
addSize (Size w1 h1) (Size w2 h2) = Size (w1+w2) (h1+h2)

sizeDistance :: Point -> Point -> Size
sizeDistance (Point x1 y1) (Point x2 y2) =
    Size (if x1 < x2 then x2-x1 else x1-x2) (if y1 < y2 then y2-y1 else y1-y2)

-- marshalling
withCSize :: Size -> (CInt -> CInt -> IO a) -> IO a
withCSize (Size w h) f
  = f (toCInt w) (toCInt h)

withCSizeResult :: (Ptr CInt -> IO ()) -> IO Size
withCSizeResult f
  = allocaArray 2 $ \csize ->
    do f csize
       [w,h] <- peekArray 2 csize
       return (fromCSize w h)

fromCSize :: CInt -> CInt -> Size
fromCSize w h
  = Size (fromCInt w) (fromCInt h)

{-----------------------------------------------------------------------------------------
  Rectangle
-----------------------------------------------------------------------------------------}
-- | A rectangle is defined by four points -- the left x coordinate, the top y coordinate,
-- the right x coordinate and the bottom y coordinate. Pixels lie between the cooridinates.
-- This means that a rectangle where all coordinates are the same, designates an empty area
-- (and not a single pixel). The pixel width of a rectangle is therefore simply the difference
-- between the right and left, or bottom and top coordinate.
data Rect   = Rect
    { left   :: !Int
    , top    :: !Int
    , right  :: !Int
    , bottom :: !Int
    }
    deriving (Eq,Show)

-- | The top left corner of the rectangle
topLeft :: Rect -> Point
topLeft (Rect l t r b)  = Point l t

-- | The top right corner of the rectangle
topRight :: Rect -> Point
topRight (Rect l t r b)  = Point r t

-- | The bottom left corner of the rectangle
bottomLeft :: Rect -> Point
bottomLeft (Rect l t r b)  = Point l b

-- | The bottom right corner of the rectangle
bottomRight :: Rect -> Point
bottomRight (Rect l t r b)  = Point r b


-- | Construct a (positive) rectangle between two (arbitraty) points.
rect :: Point -> Point -> Rect
rect (Point x0 y0) (Point x1 y1)
  = Rect (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1)

-- | Create a rectangle at a certain (upper-left) point with a certain size.
rectAt :: Point -> Size -> Rect
rectAt (Point x y) (Size w h)
  = Rect x y (x+w) (y+h)

-- | Get the size of a rectangle.
rectSize :: Rect -> Size
rectSize (Rect l t r b)
  = Size (r-l) (b-t)

-- | Create a rectangle of a certain size with the upper-left corner at ('pt' 0 0).
rectOfSize :: Size -> Rect
rectOfSize (Size w h)
  = Rect 0 0 w h
  
rectIsEmpty :: Rect -> Bool
rectIsEmpty (Rect l t r b) = l == r || t == b

pointInRect :: Point -> Rect -> Bool
pointInRect (Point x y) (Rect l t r b) = x >= l && x <= r && y >= t && y <= b

rectMove :: Size -> Rect -> Rect
rectMove (Size w h) (Rect l t r b)
  = Rect (l+w) (t+h) (r+w) (b+h)
  
rectMoveTo :: Point -> Rect -> Rect
rectMoveTo p rect
  = rectAt p (rectSize rect)

pointToRect :: Point -> Rect
pointToRect (Point x y) = Rect x y x y

centralPoint :: Rect -> Point
centralPoint (Rect l t r b) = Point ((l + r) `quot` 2) ((t + b) `quot` 2)

centralRect :: Rect -> Size -> Rect
centralRect (Rect l t r b) (Size w h) =
    let 
        u = r - l
        v = b - t
        x = (r + l) `quot` 2
        y = (b + t) `quot` 2
        dx = (min w u) `quot` 2
        dy = (min h v) `quot` 2
    in
        Rect (x - dx) (y - dy) (x + dx) (y + dy)


rectStretchTo :: Size -> Rect -> Rect
rectStretchTo (Size w h) (Rect l t r b)
  = Rect l t (l+w) (t+h)

disjointRects :: Rect -> Rect -> Bool
disjointRects (Rect l1 t1 r1 b1) (Rect l2 t2 r2 b2) = l1 >= r2 || b1 <= t2 || r1 <= l2 || t1 >= b2

rectsDiff :: Rect -> Rect -> [Rect]
rectsDiff rect1 rect2 = subtractFittingRect rect1 (rectSect rect1 rect2)
    where
    --    subtractFittingRect r1 r2 subtracts r2 from r1 assuming that r2 fits inside r1
        subtractFittingRect :: Rect -> Rect -> [Rect]
        subtractFittingRect (Rect l1 t1 r1 b1) (Rect l2 t2 r2 b2) =
            filter (not . rectIsEmpty) 
                [ Rect l1 t1 r1 t2
                , Rect l1 t2 l2 b2
                , Rect r2 t2 r1 b2
                , Rect l1 b2 r1 b1
                ]

rectUnion :: Rect -> Rect -> Rect
rectUnion (Rect x0 y0 x1 y1) (Rect x2 y2 x3 y3)
  = Rect (min x0 x2) (min y0 y2) (max x1 x3) (max y1 y3)
  
rectSect :: Rect -> Rect -> Rect
rectSect rect1@(Rect l1 t1 r1 b1) rect2@(Rect l2 t2 r2 b2)
    | disjointRects rect1 rect2    = Rect 0 0 0 0
    | otherwise            = Rect    (max l1 l2) (max t1 t2) (min r1 r2) (min b1 b2)


-- marshalling
withCRect :: Rect -> (CInt -> CInt -> CInt -> CInt -> IO a) -> IO a
withCRect (Rect x0 y0 x1 y1) f
  = f (toCInt (x0)) (toCInt (y0)) (toCInt (x1)) (toCInt (y1))

withCRectResult :: (Ptr CInt -> IO ()) -> IO Rect
withCRectResult f
  = allocaArray 4 $ \crect ->
    do f crect
       [x0,y0,x1,y1] <- peekArray 4 crect
       return (fromCRect x0 y0 x1 y1)

fromCRect :: CInt -> CInt -> CInt -> CInt -> Rect
fromCRect x0 y0 x1 y1
  = Rect (fromCInt x0) (fromCInt y0) (fromCInt x1) (fromCInt y1)


{-----------------------------------------------------------------------------------------
  Modifiers
-----------------------------------------------------------------------------------------}
-- | The @Modifiers@ indicate the meta keys that have been pressed ('True') or not ('False').
data Modifiers  = Modifiers
                  { shiftDown   :: !Bool   -- ^ shift key down
                  , controlDown :: !Bool   -- ^ control key down
                  , altDown     :: !Bool   -- ^ alt key down
                  }
                  deriving (Eq,Show)

-- | Construct a 'Modifiers' structure with no meta keys pressed.
noModifiers :: Modifiers
noModifiers = Modifiers False False False

-- | Construct a 'Modifiers' structure with just Shift meta key pressed.
justShift   :: Modifiers
justShift   = noModifiers{ shiftDown = True }

-- | Construct a 'Modifiers' structure with just Alt meta key pressed.
justAlt     :: Modifiers
justAlt     = noModifiers{ altDown = True }

-- | Construct a 'Modifiers' structure with just Ctrl meta key pressed.
justControl :: Modifiers
justControl = noModifiers{ controlDown = True }

-- | Test if no meta key was pressed.
noneDown :: Modifiers -> Bool
noneDown (Modifiers shift control alt) = not (shift || control || alt)



fromCModifiers :: CWord -> Modifiers
fromCModifiers ci
  = Modifiers (bitsSet i 0x01) (bitsSet i 0x02) (bitsSet i 0x04)
  where
    i = fromCWord ci
    bitsSet x mask = x .&. mask /= 0

toCModifiers :: Modifiers -> CWord
toCModifiers (Modifiers shift control alt)
  = toCWord ((mask 0x01 shift) + (mask 0x02 control) + (mask 0x04 alt))
  where
    mask m test   = if test then m else 0


{-----------------------------------------------------------------------------------------
  MouseEvent
-----------------------------------------------------------------------------------------}
-- | Mouse events
data MouseEvent
  =  MouseMove          !Point !Modifiers    -- ^ Mouse was moved over the client area of the window
  |  MouseRightDown   !Point !Modifiers    -- ^ Mouse right button goes down
  |  MouseRightUp     !Point !Modifiers    -- ^ Mouse right button goes up
  |  MouseLeftDown    !Point !Modifiers    -- ^ Mouse left button goes down
  |  MouseLeftUp      !Point !Modifiers    -- ^ Mouse left  button goes up
  |  MouseDoubleClick !Point !Modifiers    -- ^ Mouse left button is double clicked
  |  MouseDrag        !Point !Modifiers -- ^ Mouse is moved over the client area of the window and its left button is down
  deriving (Eq,Show)

-- | Extract the position from a 'MouseEvent'.
mousePos :: MouseEvent -> Point
mousePos mouseEvent
  = case mouseEvent of
      MouseMove    p m       -> p
      MouseRightDown p m  -> p
      MouseRightUp p m    -> p    
      MouseLeftDown p m   -> p   
      MouseLeftUp p m     -> p     
      MouseDoubleClick p m-> p
      MouseDrag p m       -> p       

-- | Extract the modifiers from a 'MouseEvent'.
mouseModifiers :: MouseEvent -> Modifiers
mouseModifiers mouseEvent
  = case mouseEvent of
      MouseMove    p m       -> m
      MouseRightDown p m  -> m
      MouseRightUp p m    -> m    
      MouseLeftDown p m   -> m   
      MouseLeftUp p m     -> m     
      MouseDoubleClick p m-> m
      MouseDrag p m       -> m

fromCMouseEvent :: CInt -> CInt -> CInt -> CWord -> MouseEvent
fromCMouseEvent cevent cx cy cmodifiers
  = fromCEvent cevent (fromCPoint cx cy) (fromCModifiers cmodifiers)
  where
    fromCEvent cevent
      = case fromCInt cevent of
          1 -> MouseMove
          4 -> MouseLeftDown
          5 -> MouseRightDown
          6 -> MouseDoubleClick
          7 -> MouseDrag
          8 -> MouseLeftUp
          9 -> MouseRightUp

{-----------------------------------------------------------------------------------------
  KeyboardEvent
-----------------------------------------------------------------------------------------}
-- | The KeyboardEvent type.
data KeyboardEvent
  =  KeyDown    !Key !Bool           -- ^ Key is down, boolean is True if repeated
  |  KeyUp      !Key                 -- ^ Key goes up
  |  KeyLost    !Key                 -- ^ The key was down when the widget lost focus
  deriving (Eq,Show)

-- | Extract the key from a 'KeyboardEvent'
keyboardKey :: KeyboardEvent -> Key
keyboardKey event
  = case event of
      KeyDown key repeat  -> key
      KeyUp   key         -> key
      KeyLost key         -> key

-- | Is this a key that is held down.
keyboardRepeat :: KeyboardEvent -> Bool
keyboardRepeat event
  = case event of
      KeyDown key repeat  -> repeat
      other               -> False

-- | Keyboard keys. 
-- A Shift-character combinations gets translated into an upper-case character and a Ctrl-character in a
-- control character. Alt-character combinations are normally processed by the system
-- to handle menu's etc.
data Key
  =  KeyBegin        Modifiers
  |  KeyClear        Modifiers
  |  KeyDelete        Modifiers
  |  KeyEnd        Modifiers
  |  KeyArrowDown    Modifiers    
  |  KeyArrowLeft    Modifiers    
  |  KeyArrowRight    Modifiers    
  |  KeyArrowUp        Modifiers
  |  KeyPageDown    Modifiers    
  |  KeyPageUp        Modifiers
  |  KeyEscape        Modifiers
  |  KeyEnter        Modifiers
  |  KeyTab        Modifiers
  |  KeyBackSpace    Modifiers
  |  KeyF1        Modifiers
  |  KeyF2        Modifiers
  |  KeyF3        Modifiers
  |  KeyF4        Modifiers
  |  KeyF5        Modifiers
  |  KeyF6        Modifiers
  |  KeyF7        Modifiers
  |  KeyF8        Modifiers
  |  KeyF9        Modifiers
  |  KeyF10        Modifiers
  |  KeyF11        Modifiers
  |  KeyF12        Modifiers
  |  KeyF13        Modifiers
  |  KeyF14        Modifiers
  |  KeyF15        Modifiers
  |  KeyHelp        Modifiers
  |  KeyChar     !Char
  |  KeyAltChar !Char
  |  KeyNull
  deriving (Eq,Show)
  
-- | Extract the modifiers from a key
keyModifiers :: Key -> Modifiers
keyModifiers (KeyBegin        mods) = mods
keyModifiers (KeyClear        mods) = mods
keyModifiers (KeyDelete        mods) = mods
keyModifiers (KeyEnd        mods) = mods
keyModifiers (KeyArrowDown    mods) = mods
keyModifiers (KeyArrowLeft    mods) = mods
keyModifiers (KeyArrowRight    mods) = mods
keyModifiers (KeyArrowUp    mods) = mods
keyModifiers (KeyPageDown    mods) = mods
keyModifiers (KeyPageUp        mods) = mods
keyModifiers (KeyEscape        mods) = mods
keyModifiers (KeyEnter        mods) = mods
keyModifiers (KeyTab        mods) = mods
keyModifiers (KeyBackSpace    mods) = mods
keyModifiers (KeyF1        mods) = mods
keyModifiers (KeyF2        mods) = mods
keyModifiers (KeyF3        mods) = mods
keyModifiers (KeyF4        mods) = mods
keyModifiers (KeyF5        mods) = mods
keyModifiers (KeyF6        mods) = mods
keyModifiers (KeyF7        mods) = mods
keyModifiers (KeyF8        mods) = mods
keyModifiers (KeyF9        mods) = mods
keyModifiers (KeyF10        mods) = mods
keyModifiers (KeyF11        mods) = mods
keyModifiers (KeyF12        mods) = mods
keyModifiers (KeyF13        mods) = mods
keyModifiers (KeyF14        mods) = mods
keyModifiers (KeyF15        mods) = mods
keyModifiers (KeyHelp        mods) = mods
keyModifiers (KeyChar        _        ) = noModifiers
keyModifiers (KeyAltChar   _        ) = noModifiers
keyModifiers (KeyNull               ) = noModifiers


fromCKey :: CInt -> CWord -> Key
fromCKey ci cmodifiers =
  let mods = fromCModifiers cmodifiers
  in case fromCInt ci of
      0    -> KeyNull
      8    -> KeyBackSpace  mods
      9    -> KeyTab        mods
      13   -> KeyEnter      mods
      27   -> KeyEscape     mods
      1003 -> KeyBegin      mods
      1004 -> KeyClear      mods
      1005 -> KeyDelete     mods
      1006 -> KeyArrowDown  mods
      1007 -> KeyEnd        mods
      1010 -> KeyF1         mods
      1011 -> KeyF2         mods
      1012 -> KeyF3         mods
      1013 -> KeyF4         mods
      1014 -> KeyF5         mods
      1015 -> KeyF6         mods
      1016 -> KeyF7         mods
      1017 -> KeyF8         mods
      1018 -> KeyF9         mods
      1019 -> KeyF10        mods
      1020 -> KeyF11        mods
      1021 -> KeyF12        mods
      1022 -> KeyF13        mods
      1023 -> KeyF14        mods
      1024 -> KeyF15        mods
      1025 -> KeyHelp       mods
      1026 -> KeyArrowLeft  mods
      1027 -> KeyPageDown   mods
      1028 -> KeyPageUp     mods
      1029 -> KeyArrowRight mods
      1030 -> KeyArrowUp    mods
      i    -> if i > 256
                then KeyAltChar (toEnum (i-256))
                else KeyChar    (toEnum i)

toCKey :: Key -> (CInt,CWord)
toCKey key = (toCInt keyCode, toCModifiers mods)
  where
    (keyCode, mods) = case key of
      KeyBackSpace  mods -> (8,    mods)
      KeyTab        mods -> (9,    mods)
      KeyEnter      mods -> (13,   mods)
      KeyEscape     mods -> (27,   mods)
      KeyBegin      mods -> (1003, mods)
      KeyClear      mods -> (1004, mods)
      KeyDelete     mods -> (1005, mods)
      KeyArrowDown  mods -> (1006, mods)
      KeyEnd        mods -> (1007, mods)
      KeyF1         mods -> (1010, mods)
      KeyF2         mods -> (1011, mods)
      KeyF3         mods -> (1012, mods)
      KeyF4         mods -> (1013, mods)
      KeyF5         mods -> (1014, mods)
      KeyF6         mods -> (1015, mods)
      KeyF7         mods -> (1016, mods)
      KeyF8         mods -> (1017, mods)
      KeyF9         mods -> (1018, mods)
      KeyF10        mods -> (1019, mods)
      KeyF11        mods -> (1020, mods)
      KeyF12        mods -> (1021, mods)
      KeyF13        mods -> (1022, mods)
      KeyF14        mods -> (1023, mods)
      KeyF15        mods -> (1024, mods)
      KeyHelp       mods -> (1025, mods)
      KeyArrowLeft  mods -> (1026, mods)
      KeyPageDown   mods -> (1027, mods)
      KeyPageUp     mods -> (1028, mods)
      KeyArrowRight mods -> (1029, mods)
      KeyArrowUp    mods -> (1030, mods)
      KeyChar c          -> (fromEnum c, noModifiers)
      KeyAltChar c       -> (fromEnum c+256, noModifiers)
      KeyNull            -> (0, noModifiers)


fromCKeyboardEvent :: CInt -> CInt -> CWord -> KeyboardEvent
fromCKeyboardEvent cevent ckey cmodifiers
  = fromCEvent cevent (fromCKey ckey cmodifiers)
  where
    fromCEvent cevent key
      = case fromCInt cevent of
          10 -> KeyDown key False
          11 -> KeyDown key True
          12 -> KeyUp   key
          13 -> KeyLost key

{-----------------------------------------------------------------------------------------
  Document interface
-----------------------------------------------------------------------------------------}
-- | The document interface type of interactive processes.
data    DocumentInterface
    = NDI                -- ^ No document interface
    | SDI                -- ^ Single document interface
    | MDI                -- ^ Multiple document interface
    deriving (Eq,Show)

toCDocumentInterface :: DocumentInterface -> CInt
toCDocumentInterface NDI = 0
toCDocumentInterface SDI = 1
toCDocumentInterface MDI = 2

-----------------------------------------------------------------------------------------
-- Drawmode
-----------------------------------------------------------------------------------------
-- | The drawing mode.
data DrawMode = DrawCopy       -- ^ Copy directly to the canvas
              | DrawInvert     -- ^ Invert all colors
              | DrawXor        -- ^ /xor/ the colors with the colors on the canvas. Doing this twice restores the original picture.
              deriving (Eq,Show)

toCDrawMode :: DrawMode -> CInt
toCDrawMode mode
  = case mode of
      DrawCopy      -> 0
      DrawInvert    -> 1
      DrawXor       -> 2

-----------------------------------------------------------------------------------------
-- PositionType
-----------------------------------------------------------------------------------------

data PositionType = PosLeft
                  | PosTop
                  | PosRight
                  | PosBottom
                  deriving (Eq,Show)

toCPositionType :: PositionType -> CInt
toCPositionType PosLeft   = 0
toCPositionType PosTop    = 1
toCPositionType PosRight  = 2
toCPositionType PosBottom = 3

fromCPositionType :: CInt -> PositionType
fromCPositionType 0 = PosLeft
fromCPositionType 1 = PosTop
fromCPositionType 2 = PosRight
fromCPositionType 3 = PosBottom

-----------------------------------------------------------------------------------------
-- WindowPosition
-----------------------------------------------------------------------------------------

data WindowPosition
  = WinPosExact Rect
  | WinPosCenter Size
  | WinPosCenterOnParent Size
  | WinPosMouse Size
  deriving (Eq,Show)

withCWindowPosition :: WindowPosition -> (CInt -> CInt -> CInt -> CInt -> CInt -> IO a) -> IO a
withCWindowPosition (WinPosExact rect) f = withCRect rect (f 0)
withCWindowPosition (WinPosCenter size) f = withCSize size (f 1 0 0)
withCWindowPosition (WinPosCenterOnParent size) f = withCSize size (f 2 0 0)
withCWindowPosition (WinPosMouse size) f = withCSize size (f 3 0 0)

{-----------------------------------------------------------------------------------------
  Buffermode & Pattern
-----------------------------------------------------------------------------------------}
-- | The drawing buffer.
data BufferMode = Buffered      -- ^ Draw via a buffer for smoother animations (/double buffering/)
                | UnBuffered    -- ^ Draw directly to the canvas
                deriving (Eq,Show)

toCBufferMode :: BufferMode -> CBool
toCBufferMode Buffered  = toCBool True
toCBufferMode other     = toCBool False


-- | The join style is applied to corners of figures (like rectangles).
data JoinStyle  = JoinBevel   -- ^ Cut off corners diagonally.
                | JoinMiter   -- ^ Leave it as is.
                | JoinRound   -- ^ Round off the corners.
                deriving (Eq,Show,Read)

toCJoinStyle :: JoinStyle -> CInt
toCJoinStyle join
  = case join of
      JoinBevel -> 0
      JoinMiter -> 1
      JoinRound -> 2


-- | The cap style is applied to the end points of lines.
data CapStyle   = CapRound    -- ^ Round: put a disc at the logical end point.
                | CapSquare   -- ^ Square: put a square at the logical end point.
                | CapFlat     -- ^ End flatly at the logical end point. (Doesn't stick out like square or round).
                deriving (Eq,Show,Read)

toCCapStyle :: CapStyle -> CInt
toCCapStyle cap
  = case cap of
      CapRound  -> 0
      CapSquare -> 1
      CapFlat   -> 2

-- | The line style is applied when drawing figures.
data LineStyle 
  = LineSolid               -- ^ Solid line
  | LineDash                -- ^ Dashed line
  | LineDot                 -- ^ Dotted line
  | LineDashDot             -- ^ Dash - Dot pattern.
  | LineDashDotDot          -- ^ Dash - Dot - Dot pattern.
  | LineCustomStyle [Word8] -- ^ Custom pattern: each element specifies the pixel length of a dash.
  deriving (Eq,Show,Read)

withCLineStyle :: LineStyle -> (CInt -> CInt -> Ptr CUChar -> IO a) -> IO a
withCLineStyle style f
  = case style of
      LineSolid       -> f 0 0 nullPtr
      LineDash        -> f 1 0 nullPtr
      LineDot         -> f 2 0 nullPtr
      LineDashDot     -> f 3 0 nullPtr
      LineDashDotDot  -> f 4 0 nullPtr
      LineCustomStyle xs 
        -> let n = length xs in
           do pstyles <- mallocArray n
              pokeArray pstyles (map fromIntegral xs)
              f 5 (toCInt n) pstyles           -- freed by library


-- | The 'FillStyle' is applied when filling an object. 
data FillStyle
  = FillSolid           -- ^ Solid pattern
  | HatchBDiagonal      -- ^ A 45-degree upward, left-to-right hatch
  | HatchFDiagonal      -- ^ A 45-degree downward, left-to-right hatch
  | HatchCross          -- ^ Horizontal and vertical cross-hatch
  | HatchDiagCross      -- ^ 45-degree crosshatch
  | HatchHorizontal     -- ^ Horizontal hatch 
  | HatchVertical       -- ^ Vertical hatch
  | GradientLinear Point Point             [(Double,Color)]  -- ^ Linear gradient between two points and with color stops defined with the list
  | GradientRadial (Point,Int) (Point,Int) [(Double,Color)]  -- ^ Radial gradient between two circles and with color stops defined with the list
  | FillBitmap Bitmap   -- ^ A bitmap pattern -- 8x8 pixel bitmaps are always supported.
  deriving Eq

withCFillStyle :: FillStyle -> (forall b . CInt -> Ptr b -> IO a) -> IO a
withCFillStyle style f
  = case style of
      FillSolid           -> f 0 nullHandle
      HatchBDiagonal      -> f 1 nullHandle
      HatchFDiagonal      -> f 2 nullHandle
      HatchCross          -> f 3 nullHandle
      HatchDiagCross      -> f 4 nullHandle
      HatchHorizontal     -> f 5 nullHandle
      HatchVertical       -> f 6 nullHandle
      GradientLinear p1 p2 stops  -> newLinearGradient p1 p2 stops >>= f 7
      GradientRadial c1 c2 stops  -> newRadialGradient c1 c2 stops >>= f 8
      FillBitmap bitmap   -> withCBitmap bitmap $ \handle -> f 9 handle

newLinearGradient (Point x1 y1) (Point x2 y2) stops = do
  g <- osNewLinearGradient (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2)
  mapM_ (\(offset,color) -> osAddGradientStop g (realToFrac offset) (toCColor color)) stops
  return g
foreign import ccall osNewLinearGradient :: CInt -> CInt -> CInt -> CInt -> IO GradientHandle

newRadialGradient (Point x1 y1, radius1) (Point x2 y2, radius2) stops = do
  g <- osNewRadialGradient (fromIntegral radius1) (fromIntegral x1) (fromIntegral y1) (fromIntegral radius2) (fromIntegral x2) (fromIntegral y2)
  mapM_ (\(offset,color) -> osAddGradientStop g (realToFrac offset) (toCColor color)) stops
  return g
foreign import ccall osNewRadialGradient :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO GradientHandle

foreign import ccall osAddGradientStop :: GradientHandle -> CDouble -> CColor -> IO ()

{-----------------------------------------------------------------------------------------
  Bitmaps
-----------------------------------------------------------------------------------------}
-- | A bitmap. Resources for a bitmap are automatically de-allocated.
-- Equality on bitmaps is only /shallow/ equality: i.e. are the bitmap handles the same.
data Bitmap = Bitmap (ForeignPtr BH)
            deriving Eq


-- | A Codec is an (image) format description.
data Codec  = Codec
              { codecName       :: !String     -- ^ Name of the codec.
              , codecDescr      :: !String     -- ^ Description of the code.
              , codecMime       :: !String     -- ^ MIME string of the file format (i.e. \"image\/bmp\")
              , codecExtensions :: ![String]   -- ^ File extensions used for this format
              , codecIsReadable :: !Bool       -- ^ Can decode files in this format            
              , codecIsWritable :: !Bool       -- ^ Can encode bitmaps in the format
              }
              deriving (Eq,Show)


withBitmapHandle :: Bitmap -> (BitmapHandle -> IO a) -> IO a
withBitmapHandle bm f
  = withCBitmap bm f

fromCBitmap :: BitmapHandle -> IO Bitmap
fromCBitmap bh
  = do bm <- newForeignPtr osDeleteBitmap bh
       return (Bitmap bm)
foreign import ccall "&osDeleteBitmap" osDeleteBitmap :: FinalizerPtr BH

withCBitmap :: Bitmap -> (BitmapHandle -> IO a) -> IO a
withCBitmap (Bitmap bm) f
  = withForeignPtr bm f

{-----------------------------------------------------------------------------------------
  Fonts
-----------------------------------------------------------------------------------------}
-- | Name of the font.
type FontName  = String

-- | Point size of a font.
type FontSize  = Int

-- | Font weight between 100 and 900, 400 is normal.
type FontWeight= Int

-- | A font. Resources for a font are automatically de-allocated.
data Font      = Font (ForeignPtr FH) FontDef 
               deriving Eq

fontDef :: Font -> FontDef
fontDef (Font handle def)
  = def


-- | A font definition.
data FontDef  = FontDef
        { fontName        :: !FontName        -- ^ The name of font
        , fontSize        :: !FontSize        -- ^ The size of font
                , fontWeight      :: !FontWeight        -- ^ Weight between 100 and 900, 400 is normal.
                , fontStyle       :: !FontStyle         -- ^ Style
                , fontUnderline   :: !Bool              -- ^ Underlined
                , fontStrikeOut   :: !Bool              -- ^ Striked out
        } 
                deriving Eq

-- | Font styles.
data FontStyle  = Roman | Italic | Oblique
                deriving (Eq,Ord)


fontMinWeight, fontNormalWeight, fontBoldWeight, fontMaxWeight :: Int
fontMinWeight    = 100
fontNormalWeight = 400
fontBoldWeight   = 700
fontMaxWeight    = 900

instance Show FontDef where
  show fd   = "font " ++ show (fontName fd) ++ 
              ", " ++ show (fontSize fd) ++ "pt" ++
              (if fontWeight fd /= fontNormalWeight then (", weight " ++ show (fontWeight fd)) else "") ++
              (if fontStyle fd /= Roman then (", " ++ show (fontStyle fd)) else "") ++
              (if fontUnderline fd then (", underlined") else "") ++
              (if fontStrikeOut fd then (", strike out") else "")

instance Show FontStyle where
  show Roman   = "roman"
  show Italic  = "italic"
  show Oblique = "oblique"


-- | The metrics of a font.
data FontMetrics  = FontMetrics
                  { fontAscent         :: !Int        -- ^ Distance between top and base line
                  , fontDescent        :: !Int        -- ^ Distance between bottom and base line
                  , fontLeading        :: !Int        -- ^ Distance between two text lines
                  , fontMaxWidth       :: !Int        -- ^ Max. character width including spacing
                  }

-- | Standard line height of a font is the sum of its leading, ascent and descent.
fontLineHeight :: FontMetrics -> Int
fontLineHeight fontMetrics = fontAscent fontMetrics + fontDescent fontMetrics

{-----------------------------------------------------------------------------------------
  Font marshalling
-----------------------------------------------------------------------------------------}
withCFont :: Font -> (FontHandle -> IO a) -> IO a
withCFont (Font fhandle def) f
  = withForeignPtr fhandle f

fromCFont :: FontDef -> FontHandle -> IO Font
fromCFont fontdef handle
  = do fhandle <- newForeignPtr osDeleteFont handle
       return (Font fhandle fontdef)
foreign import ccall "&osDeleteFont" osDeleteFont :: FinalizerPtr FH

toCStyle :: FontStyle -> Bool -> Bool -> CInt
toCStyle style underline strikeout
  = (case style of
       Oblique -> toCInt 2
       Italic  -> toCInt 1
       other   -> toCInt 0)
    .|. (if underline then 4 else 0)
    .|. (if strikeout then 8 else 0)

fromCStyle :: CInt -> (FontStyle,Bool,Bool)
fromCStyle ci
  = (case ci of
       2 -> Oblique
       1 -> Italic
       _ -> Roman
    , ci .&. 4 /= 0
    , ci .&. 8 /= 0
    )

toCWeight :: Int -> CInt
toCWeight w
  = toCInt (min fontMaxWeight (max fontMinWeight w))

fromCWeight :: CInt -> Int
fromCWeight w
  = min fontMaxWeight (max fontMinWeight (fromCInt w))

withCFontDef :: FontDef -> (CString -> CInt -> CInt -> CInt -> IO a) -> IO a
withCFontDef (FontDef name size weight style underline strikeout) f
  = withCString name $ \cname ->
    f cname (toCInt size) 
            (toCInt (min fontMaxWeight (max fontMinWeight weight))) 
            (toCStyle style underline strikeout)

withCFontDefResult :: (Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CBool -> Ptr CBool -> IO ()) -> IO FontDef
withCFontDefResult f
  = alloca $ \pcstr ->
    alloca $ \pcsize  ->
    alloca $ \pcweight ->
    alloca $ \pcstyle ->
    alloca $ \pcunderline ->
    alloca $ \pcstrikeout ->
    do poke pcunderline 0
       poke pcstrikeout 0 
       f pcstr pcsize pcweight pcstyle pcunderline pcstrikeout
       cname   <- peek pcstr
       csize   <- peek pcsize
       cweight <- peek pcweight
       cstyle  <- peek pcstyle
       cunderline <- peek pcunderline
       cstrikeout <- peek pcstrikeout
       fontDef <- fromCFontDef cname csize cweight cstyle cunderline cstrikeout
       free cname
       return fontDef

fromCFontDef :: CString -> CInt -> CInt -> CInt -> CBool -> CBool -> IO FontDef
fromCFontDef cname  csize cweight cstyle cunderline cstrikeout
  = do name <- peekCString cname
       let (style,underlined,striked) = fromCStyle cstyle
       return (FontDef name (fromCInt csize) (fromCWeight cweight) style underlined striked)

withCFontMetricsResult :: (Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()) -> IO FontMetrics
withCFontMetricsResult f
  = alloca $ \pcascent ->
    alloca $ \pcdescent ->
    alloca $ \pcleading ->
    alloca $ \pcmaxwidth ->
    do f pcascent pcdescent pcleading pcmaxwidth
       cascent   <- peek pcascent
       cdescent  <- peek pcdescent
       cleading  <- peek pcleading
       cmaxwidth <- peek pcmaxwidth
       return (fromCFontMetrics cascent cdescent cleading cmaxwidth)

fromCFontMetrics :: CInt -> CInt -> CInt -> CInt -> FontMetrics
fromCFontMetrics ca cd cl cm
  = FontMetrics (fromCInt ca) (fromCInt cd) (fromCInt cl) (fromCInt cm)

{-----------------------------------------------------------------------------------------
  Simple marshalling
-----------------------------------------------------------------------------------------}
toCInt :: Int -> CInt
toCInt i      = fromIntegral i

fromCInt :: CInt -> Int
fromCInt ci   = fromIntegral ci

type CWord  = CUInt

toCWord :: Int -> CWord
toCWord i      = fromIntegral i

fromCWord :: CWord -> Int
fromCWord ci   = fromIntegral ci


type CBool  = CInt

toCBool :: Bool -> CBool
toCBool b     = toCInt (if b then 1 else 0)

fromCBool :: CBool -> Bool
fromCBool cb  = (fromCInt cb /= 0)


toCChar :: Char -> CChar
toCChar c
  = fromIntegral (fromEnum c)

fromCChar :: CChar -> Char
fromCChar cc
  = toEnum (fromIntegral cc)

withCStrings :: [String] -> (Ptr CChar -> IO a) -> IO a
withCStrings []      io = io nullPtr
withCStrings strings io = allocaArray (memSize strings) $ \cbuffer -> do
    pokeStrings strings cbuffer
    io cbuffer
    where
        memSize = foldr (\x xs -> xs + length x + 1) 1

        pokeStrings []     cbuffer = poke cbuffer (castCharToCChar '\0')
        pokeStrings (s:ss) cbuffer = do
            pokeArray0 (castCharToCChar '\0') cbuffer (map castCharToCChar s)
            pokeStrings ss (cbuffer `plusPtr` (length s+1))

peekCStrings :: Ptr CChar -> IO [String]
peekCStrings cstrs
  | cstrs == nullPtr = return []
  | otherwise = do
       str <- peekCString cstrs
       if (null str)
        then return []
        else do strs <- peekCStrings (plusPtr cstrs (length str+1))
                return (str:strs)

-- | Convert and free a c-string.
resultCString :: IO CString -> IO String
resultCString io
  = bracket io free safePeekCString
  where
      safePeekCString ptr | ptr == nullPtr = return ""
                          | otherwise      = peekCString ptr

-- | Convert and free a c-string of c-strings.
resultCStrings :: IO (Ptr CChar) -> IO [String]
resultCStrings io
  = bracket io free peekCStrings


#ifdef WIN32_TARGET
type PortString = CWString
#else
type PortString = CString
#endif

newPortString :: String -> IO PortString
#ifdef WIN32_TARGET
newPortString = newCWString
#else
newPortString = newCString
#endif

withPortString :: String -> (PortString -> IO a) -> IO a
#ifdef WIN32_TARGET
withPortString = withCWString
#else
withPortString = withCString
#endif

-- | Convert and free a c-string.
resultPortString :: IO PortString -> IO String
resultPortString io
  = bracket io free safePeekPortString
  where
    safePeekPortString ptr | ptr == nullPtr = return ""
#ifdef WIN32_TARGET
                           | otherwise      = peekCWString ptr
#else
                           | otherwise      = peekCString ptr
#endif
