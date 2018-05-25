-----------------------------------------------------------------------------------------
{-| Module      :  StatusBar
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Defines an API for status bar creation.
    A status bar is a horizontal band at the bottom of an application window
    in which the application can display various kinds of status information.

-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.StatusBar
    ( -- * StatusBar
      setStatusBarVisible,  getStatusBarVisible
    , setStatusBarTitle,     getStatusBarTitle
    , getStatusBarIndicatorsCount
    , pushStatusBarContext, popStatusBarContext

      -- * Indicator
    , createIndicator
    , destroyIndicator
    , setIndicatorTitle, getIndicatorTitle
    , getIndicatorPos
    ) where

import Foreign
import Foreign.C
import Data.Maybe( fromMaybe )
import Graphics.UI.Port.Types

foreign import ccall "osSetStatusBarVisible" setStatusBarVisible :: Bool -> IO ()
foreign import ccall "osGetStatusBarVisible" getStatusBarVisible :: IO Bool

-- | Sets the status title without changing widget state; next call to
-- 'setStatusBarTitle' or 'pushStatusBarContext' will destroy this permanently.
setStatusBarTitle :: String -> IO ()
setStatusBarTitle title = withCString title osSetStatusBarTitle
foreign import ccall osSetStatusBarTitle :: CString -> IO ()

-- | Returns the current title of the status bar.
getStatusBarTitle :: IO String
getStatusBarTitle = resultCString osGetStatusBarTitle
foreign import ccall osGetStatusBarTitle :: IO CString

foreign import ccall "osGetStatusBarIndicatorsCount" getStatusBarIndicatorsCount :: IO Int

-- | Push a new status message onto the status bar stack and display it.
pushStatusBarContext :: String -> IO ()
pushStatusBarContext title = withCString title osPushStatusBarContext
foreign import ccall osPushStatusBarContext :: CString -> IO ()

-- | Remove current status message, and display previous status
-- message, if any.  It is fine to call this with an empty stack.
foreign import ccall "osPopStatusBarContext"  popStatusBarContext :: IO ()


--------------------------------------------------------------------
-- Status bar indicators
--------------------------------------------------------------------

createIndicator :: Maybe Int -> IO IndicatorHandle
createIndicator pos = osCreateIndicator (fromMaybe (-1) pos)
foreign import ccall osCreateIndicator :: Int -> IO IndicatorHandle

foreign import ccall "osDestroyIndicator" destroyIndicator :: IndicatorHandle -> IO ()

getIndicatorTitle :: IndicatorHandle -> IO String
getIndicatorTitle hwnd = resultCString (osGetIndicatorTitle hwnd)
foreign import ccall osGetIndicatorTitle :: IndicatorHandle -> IO CString

setIndicatorTitle :: IndicatorHandle -> String -> IO ()
setIndicatorTitle hwnd title = withCString title (osSetIndicatorTitle hwnd)
foreign import ccall osSetIndicatorTitle :: IndicatorHandle -> CString -> IO ()

foreign import ccall "osGetIndicatorPos" getIndicatorPos :: IndicatorHandle -> IO Int
