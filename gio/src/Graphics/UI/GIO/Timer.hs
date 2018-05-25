-----------------------------------------------------------------------------------------
{-| Module      :  Timer
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    A timer is a nonvisual component that repeatedly measures a specified interval, 
    in milliseconds. Each time the specified 'interval' elapses, the system 
    generates the 'command' event. Because a timer's accuracy depends on the system 
    clock rate and other system specific factors, the time-out value is only approximate.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Timer
            ( Timer, timer, interval
            ) where


import qualified Graphics.UI.Port as Lib
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events


{--------------------------------------------------------------------
  Timers
--------------------------------------------------------------------}
-- | A timer generates a 'command' event on a specified milli-second 'interval'.
newtype Timer = Timer TimerHandle

getTimerHandle (Timer thandle) = thandle

-- | Create a new timer.
timer :: [Prop Timer] -> IO Timer
timer props
  = do vtimer <- Lib.createTimer 0
       let t = Timer vtimer
       set t props
       return t

-- | The milli-second interval of the timer. The interval should be greather than zero.
interval :: Attr Timer Int
interval = newStdAttr getTimerHandle Lib.getTimerInterval Lib.setTimerInterval

instance Able Timer where
  enabled = newStdAttr getTimerHandle Lib.getTimerEnabled Lib.setTimerEnabled
                  
instance Commanding Timer where
  command = newEvent (Lib.getTimerHandler . getTimerHandle) (Lib.setTimerHandler . getTimerHandle) (Lib.setTimerDefHandler . getTimerHandle)

instance Deadly Timer where
  destroyWidget t = Lib.destroyTimer (getTimerHandle t)
  destroy         = newStdEvent getTimerHandle Lib.getTimerDestroyHandler Lib.setTimerDestroyHandler Lib.setTimerDestroyDefHandler
