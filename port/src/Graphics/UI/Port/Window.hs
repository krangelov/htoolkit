-----------------------------------------------------------------------------------------
{-| Module      :  Window
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Windows.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Window
             (
             -- * Creation
               createWindow, createDialog
             -- * Operations
             , runDialog
             , dismissWindow, dismissAllWindows
             , destroyWindow, destroyAllWindows
             , invalidateWindowFrame
             , invalidateWindow
             , setDialogMinSize
             -- * Properties
             , setWindowVisible, getWindowVisible
             , setWindowPosition, getWindowRect
             , setWindowResizeable
             , setWindowColor
             , setWindowDomainSize
             , setWindowScrollOrigin, getWindowScrollOrigin
             , setWindowViewSize,  getWindowViewSize
             , setWindowTitle,     getWindowTitle
             , setWindowPageSize,  getWindowPageSize
             , setWindowLineSize,  getWindowLineSize
             , setWindowEnabled,   getWindowEnabled
             -- * Drawing
             , drawInWindow 
             ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.IO.Unsafe( unsafePerformIO )
import Control.Exception( bracket )
import Control.Concurrent.MVar
import Control.Monad(when)
import Graphics.UI.Port.PtrMap
import Graphics.UI.Port.Types
import Graphics.UI.Port.Canvas(withCanvas, windowPen, dialogPen)
import Graphics.UI.Port.Handlers( getAllWindowHandles, registerWindow, setWindowDismissHandler, setWindowPaintHandler )

{-----------------------------------------------------------------------------------------
  foreign imports
-----------------------------------------------------------------------------------------}
-- | Force a repaint the window.
foreign import ccall "osInvalidateWindow" invalidateWindow :: WindowHandle -> IO ()

-- | Sets the position of the window. The position can be specified using the exact bounding rectangle
-- or in a way relative to the parent window or to the screen.
setWindowPosition :: WindowHandle -> WindowPosition -> IO ()
setWindowPosition hwnd pos  = withCWindowPosition pos (osSetWindowPosition hwnd)
foreign import ccall osSetWindowPosition :: WindowHandle -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Gets the smallest rectangle which bounds the outer frame of the window. Use the 'setWindowPosition' function
-- to move\/resize the window
getWindowRect :: WindowHandle -> IO Rect
getWindowRect hwnd = withCRectResult (osGetWindowRect hwnd)
foreign import ccall osGetWindowRect :: WindowHandle -> Ptr CInt -> IO ()

-- | Force a repaint of a part of the window.
invalidateWindowFrame :: WindowHandle -> Rect -> IO ()
invalidateWindowFrame  hwnd rect
  = withCRect rect $ \x0 y0 x1 y1 ->
    osInvalidateWindowRect hwnd x0 y0 x1 y1
foreign import ccall osInvalidateWindowRect :: WindowHandle -> CInt -> CInt -> CInt -> CInt -> IO ()

-- | Set the minimal size of the dialog.
setDialogMinSize :: WindowHandle -> Size -> IO ()
setDialogMinSize hwnd size = withCSize size (osSetDialogMinSize hwnd)
foreign import ccall osSetDialogMinSize :: WindowHandle -> CInt -> CInt -> IO ()

-- | Create a new (invisible) window. If the function succeeds, the return value is a handle to the new window.
-- If the function fails, the return value is nullHandle.
createWindow :: IO WindowHandle
createWindow = do
    hwnd <- osCreateWindow
    when (hwnd == nullHandle) (ioError (userError "Window.createWindow can't create more windows."))
    registerWindow hwnd
    setWindowPaintHandler hwnd onpaint
    setWindowDismissHandler hwnd (destroyWindow hwnd)
    return hwnd
    where
        onpaint canvas rect = withCanvas windowPen UnBuffered canvas (return ())
foreign import ccall osCreateWindow  :: IO WindowHandle

-- | Create a new (invisible) dialog window.
createDialog :: WindowHandle           -- ^ Handle to the owner window of the dialog being created. 
                                       -- If this parameter is nullHandle or is a handle of a window instead of dialog
                                       -- then the dialog owner is the process window.
                -> IO WindowHandle     -- ^ If the function succeeds, the return value is a handle to the new dialog.
                                       -- If the function fails, the return value is nullHandle.
createDialog hparent = do
    hwnd <- osCreateDialog hparent
    setWindowPaintHandler hwnd onpaint
    setWindowDismissHandler hwnd (destroyWindow hwnd)
    return hwnd
    where
        onpaint canvas rect = withCanvas dialogPen UnBuffered canvas (return ())
foreign import ccall osCreateDialog :: WindowHandle -> IO WindowHandle

-- | Set the default foreground color, background color and hatch style.
setWindowColor :: WindowHandle -> Color -> Color -> FillStyle -> IO ()
setWindowColor hwnd foreColor backColor fill
  = withCFillStyle fill $ \cfill cfill_info ->
    osSetWindowColor hwnd (fromIntegral (toCColor foreColor)) (fromIntegral (toCColor backColor)) cfill cfill_info
foreign import ccall osSetWindowColor :: WindowHandle -> CInt -> CInt -> CInt -> Ptr a -> IO ()

-- | Get the text of the title bar.
getWindowTitle :: WindowHandle -> IO String
getWindowTitle hwnd = resultPortString (osGetWindowTitle hwnd)
foreign import ccall osGetWindowTitle :: WindowHandle -> IO PortString

-- | Set the text of the title bar.
setWindowTitle :: WindowHandle -> String -> IO ()
setWindowTitle hwnd title = withPortString title (osSetWindowTitle hwnd)
foreign import ccall osSetWindowTitle :: WindowHandle -> PortString -> IO ()

-- | Make the window resizeable or not.
setWindowResizeable :: WindowHandle -> Bool -> IO ()
setWindowResizeable hwnd resizeable
  = osSetWindowResizeable hwnd (toCBool resizeable)
foreign import ccall osSetWindowResizeable :: WindowHandle -> CBool -> IO ()

-- | Make the window visible\/invisible.
foreign import ccall "osSetWindowVisible" setWindowVisible :: WindowHandle -> Bool -> IO ()

-- | Retrieves whether is visible.
foreign import ccall "osGetWindowVisible" getWindowVisible :: WindowHandle -> IO Bool

-- | Run a modal dialog.
foreign import ccall "osRunDialog" runDialog :: WindowHandle -> IO ()

-- | Dismiss a window.
dismissWindow :: WindowHandle -> IO Bool
dismissWindow hwnd = fmap fromCBool (osDismissWindow hwnd)
foreign import ccall osDismissWindow :: WindowHandle -> IO CBool

dismissAllWindows :: IO Bool
dismissAllWindows = getAllWindowHandles >>= dismiss
    where
        dismiss [] = return True
        dismiss (h:hs) = do
            r <- dismissWindow h
            if r then dismiss hs else return False

-- | Destroy a window.
foreign import ccall "osDestroyWindow" destroyWindow :: WindowHandle -> IO ()

destroyAllWindows :: IO ()
destroyAllWindows = getAllWindowHandles >>= mapM_ destroyWindow

-- | Get the size of the view frame.
getWindowViewSize :: WindowHandle -> IO Size
getWindowViewSize hwnd
  = withCSizeResult (osGetWindowViewSize hwnd)
foreign import ccall osGetWindowViewSize :: WindowHandle -> Ptr CInt -> IO ()

-- | Set the size of the view frame.
setWindowViewSize :: WindowHandle -> Size -> IO ()
setWindowViewSize hwnd size
  = withCSize size $ \cw ch ->
    osSetWindowViewSize hwnd cw ch
foreign import ccall osSetWindowViewSize :: WindowHandle -> CInt -> CInt -> IO ()

-- | Set the size of the view domain, if the domain is larger than the
-- view size, scroll bars will appear automatically.
setWindowDomainSize :: WindowHandle -> Size -> IO ()
setWindowDomainSize hwnd size
  = withCSize size $ \cw ch ->
    osSetWindowDomainSize hwnd cw ch
foreign import ccall osSetWindowDomainSize :: WindowHandle -> CInt -> CInt -> IO ()

-- | Set the scroll origin for the specified window
setWindowScrollOrigin :: WindowHandle -> Point -> IO ()
setWindowScrollOrigin hwnd point
  = withCPoint point (osSetWindowScrollOrigin hwnd)
foreign import ccall osSetWindowScrollOrigin :: WindowHandle -> CInt -> CInt -> IO ()

-- | Get the scroll origin for the specified window
getWindowScrollOrigin :: WindowHandle -> IO Point
getWindowScrollOrigin hwnd
  = withCPointResult (osGetWindowScrollOrigin hwnd)
foreign import ccall osGetWindowScrollOrigin :: WindowHandle -> Ptr CInt -> IO ()

-- | Get the horizontal and vertical scroll distance for a /large/ scroll action.
getWindowPageSize :: WindowHandle -> IO Size
getWindowPageSize hwnd
  = withCSizeResult $ \csize ->
    osGetWindowPageSize hwnd csize
foreign import ccall osGetWindowPageSize :: WindowHandle -> Ptr CInt -> IO ()

-- | Set the horizontal and vertical scroll distance for a /large/ scroll action.
setWindowPageSize :: WindowHandle -> Size -> IO ()
setWindowPageSize hwnd size
  = withCSize size $ \cw ch ->
    osSetWindowPageSize hwnd cw ch
foreign import ccall osSetWindowPageSize :: WindowHandle -> CInt -> CInt -> IO ()

-- | Get the horizontal and vertical scroll distance for a /small/ scroll action.
getWindowLineSize :: WindowHandle -> IO Size
getWindowLineSize hwnd 
  = withCSizeResult $ \csize ->
    osGetWindowLineSize hwnd csize
foreign import ccall osGetWindowLineSize :: WindowHandle -> Ptr CInt -> IO ()

-- | Set the horizontal and vertical scroll distance for a /small/ scroll action.
setWindowLineSize :: WindowHandle -> Size -> IO ()
setWindowLineSize hwnd size
  = withCSize size $ \cw ch ->
    osSetWindowLineSize hwnd cw ch
foreign import ccall osSetWindowLineSize :: WindowHandle -> CInt -> CInt -> IO ()

foreign import ccall unsafe "osSetWindowEnabled" setWindowEnabled :: WindowHandle -> Bool -> IO ()
foreign import ccall unsafe "osGetWindowEnabled" getWindowEnabled :: WindowHandle -> IO Bool

-- | Draw directly on the window. In general however, one should
-- register a paint event handler for drawing in a window ('registerWindowPaint').
-- The function passed to drawInWindow should be wrapped with 'withCanvas' function.
drawInWindow :: WindowHandle -> (CanvasHandle -> IO a) -> IO a
drawInWindow hwindow f = bracket (osGetWindowCanvas hwindow) (osReleaseWindowCanvas hwindow) f
foreign import ccall osGetWindowCanvas :: WindowHandle -> IO CanvasHandle
foreign import ccall osReleaseWindowCanvas :: WindowHandle -> CanvasHandle -> IO ()
