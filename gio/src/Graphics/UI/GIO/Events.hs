-----------------------------------------------------------------------------------------
{-| Module      :  Events
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Many widgets can respond to /events/.
    There are many basic events: 'mouse', 'keyboard', 'paint', etc. Not
    all widgets respond to every event and the events are divided in seperate classes,
    like 'Reactive' and 'Commanding'.

    Events are parametrised by the widget that responds to this event and the type of 
    their event handler. For example, the
    'activate' event has type:

    > activate :: Form w => Event w (IO ())

    This means that all widgets in the 'Form' class can respond to the 'activate' event
    with an 'IO' action.  The 'mouse' event handler takes a 
    'MouseEvent' as an argument and has type:

    > mouse :: Reactive w => Event w (MouseEvent -> IO ())

    A specific event handler can be set or read with the 'on' function. For example:

    > do w <- window [title =: "passive"]
    >    set w [on activate   =: set w [title =: "active"]]
    >    set w [on deactivate =: set w [title =: "deactive"]]

    For convenience, the 'mouse' and 'keyboard' events have a serie of /event filters/,
    'click', 'motion', 'enterKey', 'charKey', etc. These filters are write-only attributes
    but many can be active at the same time. However, all filters will be overwritten again
    if the basic event handler 'mouse' or 'keyboard' is set again. For example, the
    following program makes sense:

    > window [on mouse =: ..., on click =: ..., on motion =: ...]

    But in the following program, only the handler for 'mouse' will be called (*):

    > window [on clickRight =: ..., on drag =: ..., on mouse =: ...]

    
    (*) If you want to set the 'mouse' later but want to retain the old event filters,
    you can first read the current 'mouse' handler and call that explicitly in the
    new event handler (and the same for the 'keyboard' of course). This also the
    implementation technique used for the event filters themselves.

    > do w <- window [on click =: ..., on motion =: ...]
    >    prev <- get w (on mouse)
    >    set w [on mouse =: \m -> do{ prev m; ... }]
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Events
             (
             -- * Event
               Event
             , on, off
             , mapEvent
             , mapEventObj

             -- * Basic events
             
             -- ** Commanding
             , Commanding, command
             
             -- ** Dynamic update
             , DynamicUpdate, update
             
             -- ** Reactive
             , Reactive
             , mouse, keyboard, contextMenu
             
             -- ** Activity
             , Activity, activate, deactivate, isactive
             
             -- ** Resizeable
             , Resizeable, resize, resizeable
             
             -- ** Scrollable
             , Scrollable, scroll, domain, origin
             
             -- ** Dismissible
	     , Dismissible
	     , dismissWidget, dismiss
	     
	     -- ** Deadly
	     , Deadly
             , destroyWidget, destroy
             
             -- ** Paint
             , Paint, paint, repaint, paintIn
             , PaintFunction
               
             -- * Event filters
             -- ** Mouse filters
             , motion, drag
             , click, unclick, doubleClick
             , clickRight, unclickRight

             -- * Keyboard event filters
             , anyKey, key, charKey
             , enterKey,tabKey,escKey,helpKey
             , delKey,homeKey,endKey
             , pgupKey,pgdownKey
             , downKey,upKey,leftKey,rightKey

             -- * Generic event creators
             , newEvent
             , newStdEvent
             ) where

import qualified Graphics.UI.Port as Lib
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Canvas


{--------------------------------------------------------------------
   Basic events
--------------------------------------------------------------------}
-- | An event for a widget @w@ that expects an event handler of type @a@.
data Event w a  = Event !(Attr w a) !(Prop w)

-- | Get the event handler attribute for a certain event.
on :: Event w a -> Attr w a
on (Event attr off)
  = attr

off :: Event w a -> Prop w
off (Event attr off)
  = off

-- | Change the event type.
mapEvent :: (a -> b) -> (a -> b -> a) -> Event w a -> Event w b
mapEvent get set (Event attr off) = Event (mapAttr get set attr) off

-- | Change the object\'s type in the event.
mapEventObj :: (w2 -> w1) -> Event w1 a -> Event w2 a
mapEventObj f (Event attr off) = Event (mapAttrObj f attr) (mapProp f off)

{--------------------------------------------------------------------
   Event classes
--------------------------------------------------------------------}

-- | 'Commanding' widgets fire a 'command' event. Examples are buttons and menus.
class Commanding w where
  command :: Event w (IO ())

  
-- | The widgets which are members of 'DynamicUpdate' class has 'update'
-- event. The 'update' event is raised when the widget can update its state.
class DynamicUpdate w where
  update :: Event w (IO ())


-- | All widgets which the user can dynamicaly resize are members of Resizeable class.
class Resizeable w where
  resize     :: Event w (Size -> IO ())
  
  -- | Sets\/Gets whether the user can resize a widget.
  -- Widgets are user resizable by default.
  resizeable :: Attr  w Bool

-- | All widgets which can be activated\/deactivated are members of Activity class.
class Activity w where
  activate   :: Event w (IO ())
  deactivate :: Event w (IO ())
  isactive   :: Attr w Bool

class Scrollable w where
  -- | The event is generated when the user change the current
  -- position of the scollbars
  scroll :: Event w (Point -> IO ())

  -- | The size of view domain of the widget. If it is larger than
  -- the current view size, scroll bars will appear automatically.
  domain :: Attr  w Size
  
  -- | The current position of the scrollbars. The position is given
  -- relatively to the begining of the domain.
  origin :: Attr  w Point


-- | The Dismissible widgets can be dissmissed
class Dismissible w where
  -- | Dismiss a widget
  dismissWidget :: w -> IO Bool
  
  -- | The 'dismiss' event is called when the user tries to close the form.
  dismiss   :: Event w (IO ())


-- | The Deadly widgets can be destroyed.
class Deadly w where  
  destroyWidget :: w -> IO ()
  
  -- | The destroy event is triggered when a widget is destroied.
  destroy   :: Event w (IO ())


-- | Reactive widgets react to mouse and keyboard events
class Reactive w where
  mouse       :: Event w (MouseEvent -> IO ())
  keyboard    :: Event w (KeyboardEvent -> IO ())
  contextMenu :: Event w (Point -> Modifiers -> IO ())


-- | Widgets that can be repainted.
class Paint w where
  -- | The paint function.
  paint :: Event w PaintFunction
  
  -- | Explicitly force a 'paint' event.
  repaint :: w -> IO ()
  
  -- | The paintIn executes the given function with canvas
  -- associated with the given widget.
  paintIn :: w -> BufferMode -> (Canvas -> IO a) -> IO a


-- | A paint function takes a canvas, the update bound rectangle
-- and the areas that need to be repainted.
type PaintFunction = Canvas -> Rect -> [Rect] -> IO ()

{--------------------------------------------------------------------
   Mouse event filters
--------------------------------------------------------------------}
click :: Reactive w => Event w (Point -> IO ())
click
  = mouseFilter "click" filter
  where
    filter (MouseLeftDown point mod)  = noneDown mod  
    filter other                      = False

unclick :: Reactive w => Event w (Point -> IO ())
unclick
  = mouseFilter "unclick" filter
  where
    filter (MouseLeftUp point mod)  = noneDown mod  
    filter other                    = False

doubleClick :: Reactive w => Event w (Point -> IO ())
doubleClick
  = mouseFilter "doubleClick" filter
  where
    filter (MouseDoubleClick point mod) = noneDown mod  
    filter other                        = False

drag :: Reactive w => Event w (Point -> IO ())
drag
  = mouseFilter "drag" filter
  where
    filter (MouseDrag point mod)  = noneDown mod  
    filter other                  = False

motion :: Reactive w => Event w (Point -> IO ())
motion
  = mouseFilter "motion" filter
  where
    filter (MouseMove point mod)  = noneDown mod  
    filter other                  = False

clickRight :: Reactive w => Event w (Point -> IO ())
clickRight
  = mouseFilter "clickRight" filter
  where
    filter (MouseRightDown point mod)  = noneDown mod  
    filter other                       = False

unclickRight :: Reactive w => Event w (Point -> IO ())
unclickRight
  = mouseFilter "unclickRight" filter
  where
    filter (MouseRightUp point mod)  = noneDown mod  
    filter other                     = False

mouseFilter :: Reactive w => String -> (MouseEvent -> Bool) -> Event w (Point -> IO ())
mouseFilter name filter
  = mapEvent get set mouse
  where
    get prev x
      = ioError (userError ("Port.Events: the " ++ name ++ " event is write-only."))

    set prev new mouseEvent
      = do if (filter mouseEvent) then new (mousePos mouseEvent) else return ()
           prev mouseEvent


{--------------------------------------------------------------------
  Keyboard filter events
--------------------------------------------------------------------}
enterKey,tabKey,escKey,helpKey,delKey,homeKey,endKey :: Reactive w => Event w (IO ())
pgupKey,pgdownKey,downKey,upKey,leftKey,rightKey :: Reactive w => Event w (IO ())
enterKey  = key (KeyEnter      noModifiers)
tabKey    = key (KeyTab        noModifiers)
escKey    = key (KeyEscape     noModifiers)
helpKey   = key (KeyHelp       noModifiers)
delKey    = key (KeyDelete     noModifiers)
homeKey   = key (KeyBegin      noModifiers)
endKey    = key (KeyEnd        noModifiers)
pgupKey   = key (KeyPageUp     noModifiers)
pgdownKey = key (KeyPageDown   noModifiers)
downKey   = key (KeyArrowDown  noModifiers)
upKey     = key (KeyArrowUp    noModifiers)
leftKey   = key (KeyArrowLeft  noModifiers)
rightKey  = key (KeyArrowRight noModifiers)

charKey :: Reactive w => Char -> Event w (IO ())
charKey c
  = key (KeyChar c)

key :: Reactive w => Key -> Event w (IO ())
key k
  = keyboardFilter "key" filter
  where
    filter (KeyDown x False)  = k==x
    filter other              = False


anyKey :: Reactive w => Event w (Key -> IO ())
anyKey
  = keyboardFilter1 "anyKey" filter
  where
    filter (KeyDown x False)  = True
    filter other              = False


keyboardFilter :: Reactive w => String -> (KeyboardEvent -> Bool) -> Event w (IO ())
keyboardFilter name filter
  = mapEvent get set keyboard
  where
    get prev 
      = ioError (userError ("Port.Events: the " ++ name ++ " event is write-only."))

    set prev new keyboardEvent
      = do if (filter keyboardEvent) then new else return ()
           prev keyboardEvent

keyboardFilter1 :: Reactive w => String -> (KeyboardEvent -> Bool) -> Event w (Key -> IO ())
keyboardFilter1 name filter
  = mapEvent get set keyboard
  where
    get prev key
      = ioError (userError ("Port.Events: the " ++ name ++ " event is write-only."))

    set prev new keyboardEvent
      = do if (filter keyboardEvent) then new (keyboardKey keyboardEvent) else return ()
           prev keyboardEvent


--------------------------------------------------------------------
--   Generic event creators
--------------------------------------------------------------------

-- | Create a new event from a get and set function.
newEvent :: (w -> IO a) -> (w -> a -> IO ()) -> (w -> IO ()) -> Event w a
newEvent getter setter setdef
  = Event (newAttr getter setter) (newProp setdef getter setter)

-- | The 'newStdEvent' function is useful for creation of many standard events
newStdEvent :: (w -> h) -> (h -> IO a) -> (h -> a -> IO ()) -> (h -> IO ()) -> Event w a
newStdEvent map getHandler setHandler setDefHandler
  = newEvent (getHandler . map) (setHandler . map) (setDefHandler . map)
