{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-----------------------------------------------------------------------------------------
{-| Module      :  Attributes
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

   Widgets @w@ can have attributes of type @a@ represented by the type @Attr w a@.
   An attribute set or read. An example of an attribute is @title@ with type:

   > title :: Attr Window String

   When an attribute is associated with a value, we call it a /property/ of type @Prop w@.
   Properties are constructed with the ('=:') function:

   > title =: "hello world"  :: Prop Window

   Properties can be set with the 'set' function:

   > set win [title =: "hello world"]  :: IO ()

   and attributes can be read with the 'get' function:

   > get win title  :: IO String

   The function 'get', 'set' and '(=:)' are polymorphic and work for all widgets, but
   the @title@ attribute just works for windows. Many attributes are defined for multiple
   widgets and are organised in type classes, for example 'Deadly' and 'Dimensions'.

   The ('~:') operator is used to transform an attribute with an update function.
   For example, the 'interval' on a timer can be doubled with:

   > set timer [interval ~: (*2)]
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Attributes
             (
             -- * Attributes and properties
               Attr, Prop
             , set1, set, get, with, (=:), (~:), (=::), (~::)

             -- * Generic attribute creators
	     , newAttr
	     , newStdAttr
	     , varAttr
	     , readAttr
             , writeAttr
	     , mapAttr
	     , mapAttrObj
	     , newProp
	     , mapProp

             -- * Common widget classes

             -- ** Dimensions
             , Dimensions
             , frame, position, size, width, height

             -- ** HasFont
             , HasFont, font

             -- ** Drawn
             , Drawn, bufferMode, pen
             , color, bgcolor, fillStyle
             , thickness, capstyle, linestyle, joinstyle
             , drawMode, bkDrawMode

             -- ** Titled
             , Titled, title, shortTitle

             -- ** Able
             , Able, enabled

             -- ** Tipped
             , Tipped, tooltip

             -- ** Accelerated
             , Accelerated, accel

             -- ** Positioned
             , Positioned, pos

             -- ** Visible
             , Visible, visible

             -- ** Selection
             , Checked, checked
             , Countable, count
             , CommandItems, items, appendItem, insertItem, removeItem, removeAllItems
             , SingleSelect, selected
             , MultiSelect, selection
             , RangedSelect, range, selectedPos

             -- ** Valued
             , Valued, value

             -- ** Icon
             , HasIcon, icon
             ) where

import Graphics.UI.GIO.Types

infixr 0 =:, =::
infixr 0 ~:, ~::

{--------------------------------------------------------------------
  Attributes
--------------------------------------------------------------------}
-- | Widgets @w@ can have attributes of type @a@. 
data Attr w a = Attr !(w -> IO a) !(w -> a -> IO ())

-- | Create a new attribute with a specified getter and setter function.
newAttr :: (w -> IO a) -> (w -> a -> IO ()) -> Attr w a
newAttr getter setter = Attr getter setter

-- | The 'newStdAttr' function is useful for creation of many standard attributes
newStdAttr :: (w -> h) -> (h -> IO a) -> (h -> a -> IO ()) -> Attr w a
newStdAttr map getter setter = Attr (getter . map) (setter . map)
  
newProp :: (w -> IO ()) -> (w -> IO a) -> (w -> a -> IO ()) -> Prop w
newProp action getter setter
  = Prop action (\w -> do oldx <- getter w; return (setter w oldx))

-- | A property of a widget @w@ is an attribute that
-- is already associated with a value. Properties are
-- constructed with the '(=:)' operator.
data Prop w   = Prop{ propSet     :: !(w -> IO ())
                    , propRestore :: !(w -> IO (IO ()))  -- a function that returns a restoration function :-)
                    }


-- | (@mapAttr get set attr@) maps an attribute of @Attr w a@ to
-- @Attr w b@ where (@get :: a -> b@) is used when the attribute is
-- requested and (@set :: a -> b -> a@) is applied to current
-- value when the attribute is set.
mapAttr :: (a -> b) -> (a -> b -> a) -> Attr w a -> Attr w b
mapAttr get set (Attr getter setter)
    = Attr (\w   -> do a <- getter w; return (get a))
           (\w b -> do a <- getter w; setter w (set a b))


-- | (@mapAttrObj f attr@) maps an attribute of @Attr w1 a@ to
-- @Attr w2 b@ where (@f :: w2 -> w1@).
mapAttrObj :: (w2 -> w1) -> Attr w1 a -> Attr w2 a
mapAttrObj f (Attr getter setter) = Attr (getter . f) (setter . f)

mapProp :: (w2 -> w1) -> Prop w1 -> Prop w2
mapProp f (Prop set restore) = Prop (set . f) (restore . f)

-- | Helper function to implement an attribute that just gets or sets a 'Var' member.
varAttr :: (w -> Var a) -> Attr w a
varAttr getvar
  = Attr (\w   -> getVar (getvar w))
         (\w x -> setVar (getvar w) x)


-- | Define a read-only attribute. Takes the name of the attribute as its first argument.
readAttr :: String -> (w -> IO a) -> Attr w a
readAttr name getter
  = Attr getter (\w x -> ioError (userError ("attribute '" ++ name ++ "' is read-only.")))

-- | Define a write-only attribute. Takes the name of the attribute as its first argument.
writeAttr :: String -> (w -> a -> IO ()) -> Attr w a
writeAttr name setter
  = Attr (\w -> ioError (userError ("attribute '" ++ name ++ "' is write-only."))) setter



-- | Set properties
set :: w -> [Prop w] -> IO ()
set w props
  = mapM_ (set1 w) props

set1 :: w -> Prop w -> IO ()
set1 w (Prop setter mkrestore) 
  = setter w

-- | Get an attribute
get :: w -> Attr w a -> IO a
get w (Attr getter setter)
  = getter w

-- | Set properties just for a certain computation. The previous values are automatically
-- restored on return.
with :: w -> [Prop w] -> IO a -> IO a
with w props io
  = foldr (with1 w) io props

with1 :: w -> Prop w -> IO a -> IO a
with1 w (Prop setter mkrestore) io
  = do restore <- mkrestore w
       setter w 
       x <- io
       restore
       return x

-- | Associate an attribute with a new value into a property.
(=:) :: Attr w a -> a -> Prop w
(=:) (Attr getter setter) x = newProp (\w -> setter w x) getter setter
          
-- | Apply an update function to an attribute.
(~:) :: Attr w a -> (a -> a) -> Prop w
(~:) (Attr getter setter) f = newProp (\w -> do x <- getter w; setter w (f x)) getter setter

-- | Set the value of an attribute with a function that takes the widget
-- itself as an argument.
(=::) :: Attr w a -> (w -> a) -> Prop w
(=::) (Attr getter setter) f = newProp (\w -> setter w (f w)) getter setter
          
-- | Set the value of an attribute with a function that takes the widget
-- itself and the current value of the attribute as arguments. 
(~::) :: Attr w a -> (w -> a -> a) -> Prop w
(~::) (Attr getter setter) f = newProp (\w -> do x <- getter w; setter w (f w x)) getter setter

{--------------------------------------------------------------------
  Classes
--------------------------------------------------------------------}
 
-- | Widgets with dimensions have a width, height and position. Only the
-- 'frame' method is not defaulted.
class Dimensions w where
  -- | The outer frame of a widget.
  frame    :: Attr w Rect
  
  -- | The upper-left corner.
  position :: Attr w Point
  position  = mapAttr (\f -> topLeft f) (\f p -> rectMoveTo p f) frame

  -- | The size.
  size     :: Attr w Size
  size      = mapAttr (\f -> rectSize f) (\f sz -> rectStretchTo sz f) frame
  
  -- | The width.
  width    :: Attr w Int
  width     = mapAttr (\(Size w h) -> w) (\(Size _ h) w -> Size w h) size

  -- | The height.
  height   :: Attr w Int
  height    = mapAttr (\(Size w h) -> h) (\(Size w _) h -> Size w h) size

-- | Widgets with a font.
class HasFont w where
  -- | The font.
  font :: Attr w Font

class HasFont w => Drawn w where

  -- | The buffering mode. If the window is buffered then all
  -- drawing operations are first performed to memory buffer and after
  -- that the buffer is copied to the output device.
  bufferMode :: Attr w BufferMode

  -- | The pen
  pen :: Attr w Pen
  
  -- | The (fore ground) color of the widget.
  color :: Attr w Color
  color = mapAttr penColor (\pen c -> pen{penColor=c}) pen
  
  -- | The back ground color.
  bgcolor :: Attr w Color
  bgcolor = mapAttr penBackColor (\pen c -> pen{penBackColor=c}) pen

  -- | The hatch style.
  fillStyle :: Attr w FillStyle
  fillStyle = mapAttr penFillStyle (\pen f -> pen{penFillStyle=f}) pen

  -- | The thickness of the drawing pencil.
  thickness :: Attr w Int
  thickness = mapAttr penSize (\pen n -> pen{penSize=n}) pen

  -- | The cap style.
  capstyle :: Attr w CapStyle
  capstyle = mapAttr penCapStyle (\pen s -> pen{penCapStyle=s}) pen
  
  -- | The line style.
  linestyle :: Attr w LineStyle
  linestyle = mapAttr penLineStyle (\pen s -> pen{penLineStyle=s}) pen
  
  -- | The join style.
  joinstyle :: Attr w JoinStyle
  joinstyle = mapAttr penJoinStyle (\pen s -> pen{penJoinStyle=s}) pen
  
  drawMode :: Attr w DrawMode
  drawMode = mapAttr penMode (\pen m -> pen{penMode=m}) pen
  
  bkDrawMode :: Attr w Bool
  bkDrawMode = mapAttr penBkDrawMode (\pen m -> pen{penBkDrawMode=m}) pen


-- | Widgets with a title.
class Titled w where
  -- | The title.
  title :: Attr w String
  -- | The short title. The @shortTitle@ attribute is used only in cases when
  -- one and the same widget must have two titles: one short and one more desctiptive.
  -- If the function is not overriden then the default value is a equal to 'title'
  shortTitle :: Attr w String
  shortTitle = title

-- | Widgets that can be enabled or disabled.
class Able w where
  -- | Enable, or disable, the widget.
  enabled :: Attr w Bool
  
-- | Widgets that have a tooltip.
class Tipped w where
  -- | The tip text
  tooltip :: Attr w String

-- | Widgets that has accelerator key.
class Accelerated w where
  -- | The accelerator. Set the property to (@KeyNull@) to remove the accelerator key.
  accel :: Attr w Key
  
-- | Widgets that has specified integer position.
class Positioned w where
  -- | The widget position
  pos :: Attr w Int

-- | Widgets that can be visible\/invisible.
class Visible w where
  visible :: Attr w Bool

-- | Widgets that can be checked.
class Checked w where
  -- | Check the widget
  checked :: Attr w Bool

-- | Countable widgets are these which contains countable finite set
-- of child widgets or items.
class Countable w where
  count :: Attr w Int -- ^ The attribute is read-only and returns the
                      -- count of items in the widget.

-- | Widgets that have selectable items, like popup controls.
class CommandItems w where
  items :: Attr (w a) [(String,a)]
  appendItem :: w a -> (String,a) -> IO ()
  insertItem :: w a -> Int -> (String,a) -> IO ()
  removeItem :: w a -> Int -> IO ()
  removeAllItems :: w a -> IO ()

-- | Widgets that have a single selection (like popup control).
class Countable w => SingleSelect w where
  selected  :: Attr w Int

-- | Widgets with a multiple selection (like multi selection list box).
class Countable w => MultiSelect w where
  selection :: Attr w [Int]

-- | Widgets that selects integer position inside the specified range.
class RangedSelect w where
  -- | The selection range
  range :: Attr w (Int, Int)
  
  -- | The selected position
  selectedPos :: Attr w Int

-- | Widgets that edit a value
class Valued w a | w -> a where
  -- The value
  value :: Attr w a

-- | Widgets which displays an icon.
class HasIcon w where
  icon :: Attr w (Maybe Bitmap)
