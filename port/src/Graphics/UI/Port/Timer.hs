-----------------------------------------------------------------------------------------
{-| Module      :  Handlers
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Timers
-}
-----------------------------------------------------------------------------------------

module Graphics.UI.Port.Timer
    ( createTimer, destroyTimer
    , setTimerInterval, getTimerInterval
    , setTimerEnabled,  getTimerEnabled
    , getAllTimerHandles, destroyAllTimers
    ) where

import Graphics.UI.Port.Types
import Graphics.UI.Port.Handlers(registerTimer, getAllTimerHandles)

-- | Create a timer with a handler that is called on a specified milli-second interval.
createTimer :: Int -> IO TimerHandle
createTimer interval = do
   htimer <- osCreateTimer interval
   registerTimer htimer
   return htimer
foreign import ccall osCreateTimer :: Int -> IO TimerHandle

-- | Destroy a timer and automatically unregister its event handler.
foreign import ccall "osDestroyTimer" destroyTimer :: TimerHandle -> IO ()

-- | Change the delay time for the timer
foreign import ccall "osSetTimerInterval" setTimerInterval :: TimerHandle -> Int -> IO ()

-- | Get the delay time for the timer
foreign import ccall "osGetTimerInterval" getTimerInterval :: TimerHandle -> IO Int 

-- | Enable\/disable timer
foreign import ccall "osSetTimerEnabled" setTimerEnabled :: TimerHandle -> Bool -> IO ()

-- | Returns True when the timer is enabled.
foreign import ccall "osGetTimerEnabled" getTimerEnabled :: TimerHandle -> IO Bool

-- Destroy all timers (called by quit).
destroyAllTimers :: IO ()
destroyAllTimers = getAllTimerHandles >>= mapM_ destroyTimer
