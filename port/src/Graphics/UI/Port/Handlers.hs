-----------------------------------------------------------------------------------------
{-| Module      :  Handlers
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Event handlers are registered with @setXXXHandler@ for a particular widget.
    Allthough event handlers can be unregistered explicitly, all registered widget
    handlers will be automatically unregistered when a widget is destroyed.

    When 'quit' is called, it will close all windows, unregister all event handlers
    and destroy all timers.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Handlers
                (
                -- * Clean up
                 unregisterAllWindowHandlers

                -- * Timers
                ,registerTimer,  unregisterTimer,  getAllTimerHandles

                -- ** Events
                ,setTimerHandler,            setTimerDefHandler,            getTimerHandler
                ,setTimerDestroyHandler,     setTimerDestroyDefHandler,     getTimerDestroyHandler

                -- * Windows
                ,registerWindow, unregisterWindow, getAllWindowHandles

                -- ** Events
                ,setContainerReLayoutHandler,setContainerReLayoutDefHandler,getContainerReLayoutHandler
                ,setWindowDismissHandler,    setWindowDismissDefHandler,    getWindowDismissHandler
                ,setWindowDestroyHandler,    setWindowDestroyDefHandler,    getWindowDestroyHandler
                ,setWindowPaintHandler,      setWindowPaintDefHandler,      getWindowPaintHandler
                ,setWindowResizeHandler,     setWindowResizeDefHandler,     getWindowResizeHandler
                ,setWindowScrollHandler,     setWindowScrollDefHandler,     getWindowScrollHandler
                ,setWindowMouseHandler,      setWindowMouseDefHandler,      getWindowMouseHandler
                ,setWindowKeyboardHandler,   setWindowKeyboardDefHandler,   getWindowKeyboardHandler
                ,setWindowActivateHandler,   setWindowActivateDefHandler,   getWindowActivateHandler
                ,setWindowDeactivateHandler, setWindowDeactivateDefHandler, getWindowDeactivateHandler
                ,setWindowContextMenuHandler,setWindowContextMenuDefHandler,getWindowContextMenuHandler

                -- * Process events
                ,setProcessDismissHandler, setProcessDismissDefHandler, getProcessDismissHandler
                ,setProcessDestroyHandler, setProcessDestroyDefHandler, getProcessDestroyHandler

                -- * Control commands
                ,setControlCommandHandler, setControlCommandDefHandler, getControlCommandHandler

                -- * TrackBar Increment\/Decrement events
                ,setTrackBarIncrementHandler, setTrackBarIncrementDefHandler, getTrackBarIncrementHandler
                ,setTrackBarDecrementHandler, setTrackBarDecrementDefHandler, getTrackBarDecrementHandler

                -- * TreeView events
                ,setTreeViewGetterHandler, getTreeViewGetterHandler

                -- * Menu events
                ,setMenuDestroyHandler, setMenuDestroyDefHandler, getMenuDestroyHandler

                -- * ToolBar events
                ,setToolDestroyHandler, setToolDestroyDefHandler, getToolDestroyHandler

                -- * Action events
                ,setActionCommandHandler, setActionCommandDefHandler, getActionCommandHandler
                ,setActionUpdateHandler,  setActionUpdateDefHandler,  getActionUpdateHandler
                ,setActionDestroyHandler, setActionDestroyDefHandler, getActionDestroyHandler

                -- * Indicator events
                ,setIndicatorCommandHandler, setIndicatorCommandDefHandler, getIndicatorCommandHandler
                ,setIndicatorDestroyHandler, setIndicatorDestroyDefHandler, getIndicatorDestroyHandler

                -- ** Internals
                ,actionBitmaps, windowBitmaps
                ) where

import Prelude hiding (lookup)
import Graphics.UI.Port.Types
import Graphics.UI.Port.PtrMap

import Foreign.C
import Foreign.Ptr
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad( when )
import System.IO.Unsafe( unsafePerformIO )
import qualified Data.List as L

{-----------------------------------------------------------------------------------------
  General Register/Unregister/Invoke
-----------------------------------------------------------------------------------------}
safeio :: IO () -> IO ()
safeio io = io `catch` (print :: SomeException -> IO ())

invokeHandler :: Ptr a -> MVar (PtrMap a b) -> (b -> IO ()) -> IO ()
invokeHandler handle varMap f
  = do map <- readMVar varMap
       case lookup handle map of
         Just x  -> safeio (f x)
         Nothing -> return ()

setHandler :: Ptr a -> b -> MVar (PtrMap a b) -> IO ()
setHandler handle x varMap
  = do map <- takeMVar varMap
       putMVar varMap (insert handle x map)

setDefHandler :: Ptr a -> MVar (PtrMap a b) -> IO ()
setDefHandler handle varMap
  = do map <- takeMVar varMap
       putMVar varMap (delete handle map)

getHandler :: Ptr a -> b -> MVar (PtrMap a b) -> IO b
getHandler handle x varMap
  = do map <- readMVar varMap
       case lookup handle map of
         Just x  -> return x
         Nothing -> return x

{-----------------------------------------------------------------------------------------
  Unregister many.
-----------------------------------------------------------------------------------------}

-- | Unregister all event handlers and menu handlers associated with a specific window.
-- This function is automatically called when a window is destroyed.
unregisterAllWindowHandlers :: WindowHandle -> IO ()
unregisterAllWindowHandlers hwnd
  =  do setContainerReLayoutDefHandler hwnd
        setWindowDismissDefHandler hwnd
        setWindowDestroyDefHandler hwnd
        setWindowPaintDefHandler hwnd    
        setWindowResizeDefHandler hwnd   
        setWindowScrollDefHandler hwnd   
        setWindowMouseDefHandler hwnd
        setWindowKeyboardDefHandler hwnd
        setWindowDeactivateDefHandler hwnd
        setWindowActivateDefHandler hwnd 
        setControlCommandDefHandler  hwnd
        setTrackBarIncrementDefHandler hwnd
        setTrackBarDecrementDefHandler hwnd
        return ()

-----------------------------------------------------------------------------------------
--  Keep track of all windows
-----------------------------------------------------------------------------------------
{-# NOINLINE windows #-}
windows :: MVar [WindowHandle]
windows
  = unsafePerformIO (newMVar [])

-- | 'registerWindow' should be called by all top-level window creation functions. 
-- The windows are automatically unregistered when destroyed.
registerWindow :: WindowHandle -> IO ()
registerWindow hwnd
  = do hwnds <- takeMVar windows
       putMVar windows (hwnd : L.delete hwnd hwnds)

unregisterWindow :: WindowHandle -> IO ()
unregisterWindow hwnd
  = do hwnds <- takeMVar windows
       putMVar windows (L.delete hwnd hwnds)

-- | 'getAllWindowHandles' returns list of handles for all opened windows, sorted in Z-order.
getAllWindowHandles :: IO [WindowHandle]
getAllWindowHandles = readMVar windows

-----------------------------------------------------------------------------------------
-- WindowDestroy
--         When a window is destroyed, we need to unregister all its handlers in order to
--     avoid space leaks.
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowDestroy #-}
handlersWindowDestroy :: MVar (PtrMap WindowHandle (IO ()))
handlersWindowDestroy
  = unsafePerformIO (newMVar empty)

{-# NOINLINE windowBitmaps #-}
windowBitmaps :: MVar (PtrMap WindowHandle Bitmap)
windowBitmaps = unsafePerformIO (newMVar empty)

setWindowDestroyHandler :: WindowHandle -> IO () -> IO ()
setWindowDestroyHandler hwnd handler
  = setHandler hwnd handler handlersWindowDestroy

setWindowDestroyDefHandler :: WindowHandle -> IO ()
setWindowDestroyDefHandler hwnd 
  = setDefHandler hwnd handlersWindowDestroy
  
getWindowDestroyHandler :: WindowHandle  -> IO (IO ())
getWindowDestroyHandler hwnd
  = getHandler hwnd (return ()) handlersWindowDestroy

-- destroy also unregisters any registered event handlers to prevent space leaks.
-- Since destroy can be called recursively from the handler, we first unregister
-- the destroy handler explictly (so it is called only once).
handleWindowDestroy :: WindowHandle -> IO ()
handleWindowDestroy hwnd
  = do map <- readMVar handlersWindowDestroy
       unregisterAllWindowHandlers hwnd
       case lookup hwnd map of
         Nothing -> return ()
         Just io -> safeio io
       map <- takeMVar windowBitmaps
       putMVar windowBitmaps (delete hwnd map)
       unregisterWindow hwnd

-----------------------------------------------------------------------------------------
-- ContainerReLayout
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersContainerReLayout #-}
handlersContainerReLayout :: MVar (PtrMap WindowHandle (IO ()))
handlersContainerReLayout
  = unsafePerformIO (newMVar empty)

setContainerReLayoutHandler :: WindowHandle -> IO () -> IO ()
setContainerReLayoutHandler hwnd handler
  = setHandler hwnd handler handlersContainerReLayout

setContainerReLayoutDefHandler :: WindowHandle -> IO ()
setContainerReLayoutDefHandler hwnd 
  = setDefHandler hwnd handlersContainerReLayout

getContainerReLayoutHandler :: WindowHandle -> IO (IO ())
getContainerReLayoutHandler hwnd 
  = getHandler hwnd (return ()) handlersContainerReLayout
  
handleContainerReLayout :: WindowHandle -> IO ()
handleContainerReLayout hwnd
  = invokeHandler hwnd handlersContainerReLayout id

-----------------------------------------------------------------------------------------
-- WindowDismiss
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowDismiss #-}
handlersWindowDismiss :: MVar (PtrMap WindowHandle (IO ()))
handlersWindowDismiss
  = unsafePerformIO (newMVar empty)

setWindowDismissHandler :: WindowHandle -> IO () -> IO ()
setWindowDismissHandler hwnd handler
  = setHandler hwnd handler handlersWindowDismiss

setWindowDismissDefHandler :: WindowHandle -> IO ()
setWindowDismissDefHandler hwnd 
  = setDefHandler hwnd handlersWindowDismiss
  
getWindowDismissHandler :: WindowHandle -> IO (IO ())
getWindowDismissHandler hwnd
  = getHandler hwnd (return ()) handlersWindowDismiss

handleWindowDismiss :: WindowHandle -> IO ()
handleWindowDismiss hwnd
  = invokeHandler hwnd handlersWindowDismiss id

-----------------------------------------------------------------------------------------
-- ControlCommand
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersControlCommand #-}
handlersControlCommand :: MVar (PtrMap WindowHandle (IO ()))
handlersControlCommand
  = unsafePerformIO (newMVar empty)

setControlCommandHandler :: WindowHandle -> IO () -> IO ()
setControlCommandHandler hwnd handler
  = setHandler hwnd handler handlersControlCommand

setControlCommandDefHandler :: WindowHandle -> IO ()
setControlCommandDefHandler hwnd 
  = setDefHandler hwnd handlersControlCommand
  
getControlCommandHandler :: WindowHandle -> IO (IO ())
getControlCommandHandler hwnd
  = getHandler hwnd (return ()) handlersControlCommand

handleControlCommand :: WindowHandle -> IO ()
handleControlCommand hwnd
  = invokeHandler hwnd handlersControlCommand id

-----------------------------------------------------------------------------------------
-- TrackBar Increment/Decrement
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersTrackBarIncrement #-}
handlersTrackBarIncrement :: MVar (PtrMap WindowHandle (IO ()))
handlersTrackBarIncrement = unsafePerformIO (newMVar empty)

setTrackBarIncrementHandler :: WindowHandle -> IO () -> IO ()
setTrackBarIncrementHandler hwnd handler
  = setHandler hwnd handler handlersTrackBarIncrement

setTrackBarIncrementDefHandler :: WindowHandle -> IO ()
setTrackBarIncrementDefHandler hwnd 
  = setDefHandler hwnd handlersTrackBarIncrement
  
getTrackBarIncrementHandler :: WindowHandle -> IO (IO ())
getTrackBarIncrementHandler hwnd
  = getHandler hwnd (return ()) handlersTrackBarIncrement

handleTrackBarIncrement :: WindowHandle -> IO ()
handleTrackBarIncrement hwnd
  = invokeHandler hwnd handlersTrackBarIncrement id

{-# NOINLINE handlersTrackBarDecrement #-}
handlersTrackBarDecrement :: MVar (PtrMap WindowHandle (IO ()))
handlersTrackBarDecrement = unsafePerformIO (newMVar empty)

setTrackBarDecrementHandler :: WindowHandle -> IO () -> IO ()
setTrackBarDecrementHandler hwnd handler
  = setHandler hwnd handler handlersTrackBarDecrement

setTrackBarDecrementDefHandler :: WindowHandle -> IO ()
setTrackBarDecrementDefHandler hwnd 
  = setDefHandler hwnd handlersTrackBarDecrement
  
getTrackBarDecrementHandler :: WindowHandle -> IO (IO ())
getTrackBarDecrementHandler hwnd
  = getHandler hwnd (return ()) handlersTrackBarDecrement

handleTrackBarDecrement :: WindowHandle -> IO ()
handleTrackBarDecrement hwnd
  = invokeHandler hwnd handlersTrackBarDecrement id

-----------------------------------------------------------------------------------------
-- TreeViewGetter
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersTreeViewGetter #-}
handlersTreeViewGetter :: MVar (PtrMap WindowHandle (RowHandle -> CInt -> Ptr a -> IO CBool))
handlersTreeViewGetter
  = unsafePerformIO (newMVar empty)

setTreeViewGetterHandler :: WindowHandle -> (RowHandle -> CInt -> Ptr a -> IO CBool) -> IO ()
setTreeViewGetterHandler htreeview handler
  = setHandler htreeview handler handlersTreeViewGetter

getTreeViewGetterHandler :: WindowHandle -> IO (RowHandle -> CInt -> Ptr a -> IO CBool)
getTreeViewGetterHandler htreeview
  = getHandler htreeview (\_ _ _ -> return 0) handlersTreeViewGetter

handleTreeViewGetter :: WindowHandle -> RowHandle -> CInt -> Ptr a -> IO CBool
handleTreeViewGetter htreeview hrow col ptr
  = do map <- readMVar handlersTreeViewGetter
       case lookup htreeview map of
         Nothing -> return 0
         Just f  -> f hrow col ptr `catch` (\e -> print (e :: SomeException) >> return 0)

-----------------------------------------------------------------------------------------
-- WindowDeactivate
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowDeactivate #-}
handlersWindowDeactivate :: MVar (PtrMap WindowHandle (IO ()))
handlersWindowDeactivate
  = unsafePerformIO (newMVar empty)

setWindowDeactivateHandler :: WindowHandle -> IO () -> IO ()
setWindowDeactivateHandler hwnd handler
  = setHandler hwnd handler handlersWindowDeactivate

setWindowDeactivateDefHandler :: WindowHandle -> IO ()
setWindowDeactivateDefHandler hwnd
  = setDefHandler hwnd handlersWindowDeactivate

getWindowDeactivateHandler :: WindowHandle -> IO (IO ())
getWindowDeactivateHandler hwnd
  = getHandler hwnd (return ()) handlersWindowDeactivate

handleWindowDeactivate :: WindowHandle -> IO ()
handleWindowDeactivate hwnd
  = invokeHandler hwnd handlersWindowDeactivate id

-----------------------------------------------------------------------------------------
-- WindowActivate
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowActivate #-}
handlersWindowActivate :: MVar (PtrMap WindowHandle (IO ()))
handlersWindowActivate
  = unsafePerformIO (newMVar empty)

setWindowActivateHandler :: WindowHandle -> IO () -> IO ()
setWindowActivateHandler hwnd handler
  = setHandler hwnd handler handlersWindowActivate

setWindowActivateDefHandler :: WindowHandle -> IO ()
setWindowActivateDefHandler hwnd
  = setDefHandler hwnd handlersWindowActivate

getWindowActivateHandler :: WindowHandle -> IO (IO ())
getWindowActivateHandler hwnd
  = getHandler hwnd (return ()) handlersWindowActivate

handleWindowActivate :: WindowHandle -> IO ()
handleWindowActivate hwnd = do
    hwnds <- takeMVar windows
    putMVar windows (hwnd : L.delete hwnd hwnds)
    invokeHandler hwnd handlersWindowActivate id

-----------------------------------------------------------------------------------------
-- WindowPaint
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowPaint #-}
handlersWindowPaint :: MVar (PtrMap WindowHandle (CanvasHandle -> Rect -> IO ()))
handlersWindowPaint
  = unsafePerformIO (newMVar empty)

-- | Register a window repaint handler. The handler should not release the canvas handle argument.
setWindowPaintHandler :: WindowHandle -> (CanvasHandle -> Rect -> IO ()) -> IO ()
setWindowPaintHandler hwnd handler
  = setHandler hwnd handler handlersWindowPaint

setWindowPaintDefHandler :: WindowHandle -> IO ()
setWindowPaintDefHandler hwnd
  = setDefHandler hwnd handlersWindowPaint

getWindowPaintHandler :: WindowHandle -> IO (CanvasHandle -> Rect -> IO ())
getWindowPaintHandler hwnd
  = getHandler hwnd (\_ _ -> return ()) handlersWindowPaint

handleWindowPaint :: WindowHandle -> CanvasHandle -> CInt -> CInt -> CInt -> CInt -> IO ()
handleWindowPaint hwnd hcanvas cx0 cy0 cx1 cy1
  = invokeHandler hwnd handlersWindowPaint (\f -> f hcanvas (fromCRect cx0 cy0 cx1 cy1))

-----------------------------------------------------------------------------------------
--  WindowResize
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowResize #-}
handlersWindowResize :: MVar (PtrMap WindowHandle (Size -> IO ()))
handlersWindowResize
  = unsafePerformIO (newMVar empty)

setWindowResizeHandler :: WindowHandle -> (Size -> IO ()) -> IO ()
setWindowResizeHandler hwnd handler
  = setHandler hwnd handler handlersWindowResize

setWindowResizeDefHandler :: WindowHandle -> IO ()
setWindowResizeDefHandler hwnd 
  = setDefHandler hwnd handlersWindowResize

getWindowResizeHandler :: WindowHandle -> IO (Size -> IO ())
getWindowResizeHandler hwnd
  = getHandler hwnd (\_ -> return ()) handlersWindowResize

handleWindowResize :: WindowHandle -> CInt -> CInt -> IO ()
handleWindowResize hwnd cw ch
  = invokeHandler hwnd handlersWindowResize (\f -> f (fromCSize cw ch))

-----------------------------------------------------------------------------------------
--  WindowScroll
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowScroll #-}
handlersWindowScroll :: MVar (PtrMap WindowHandle (Point -> IO ()))
handlersWindowScroll
  = unsafePerformIO (newMVar empty)

setWindowScrollHandler :: WindowHandle -> (Point -> IO ()) -> IO ()
setWindowScrollHandler hwnd handler
  = setHandler hwnd handler handlersWindowScroll

setWindowScrollDefHandler :: WindowHandle -> IO ()
setWindowScrollDefHandler hwnd 
  = setDefHandler hwnd handlersWindowScroll
  
getWindowScrollHandler :: WindowHandle -> IO (Point -> IO ())
getWindowScrollHandler hwnd
  = getHandler hwnd (\_ -> return ()) handlersWindowScroll

handleWindowScroll :: WindowHandle -> CInt -> CInt -> IO ()
handleWindowScroll hwnd cx cy
  = invokeHandler hwnd handlersWindowScroll (\f -> f (fromCPoint cx cy))

-----------------------------------------------------------------------------------------
--  WindowMouse
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowMouse #-}
handlersWindowMouse :: MVar (PtrMap WindowHandle (MouseEvent -> IO ()))
handlersWindowMouse
  = unsafePerformIO (newMVar empty)

setWindowMouseHandler :: WindowHandle -> (MouseEvent -> IO ()) -> IO ()
setWindowMouseHandler hwnd handler
  = setHandler hwnd handler handlersWindowMouse

setWindowMouseDefHandler :: WindowHandle -> IO ()
setWindowMouseDefHandler hwnd 
  = setDefHandler hwnd handlersWindowMouse
  
getWindowMouseHandler :: WindowHandle -> IO (MouseEvent -> IO ())
getWindowMouseHandler hwnd
  = getHandler hwnd (\_ -> return ()) handlersWindowMouse

handleWindowMouse :: WindowHandle -> CInt -> CInt -> CInt -> CWord -> IO ()
handleWindowMouse hwnd cevent cx cy cmodifiers
  = invokeHandler hwnd handlersWindowMouse (\f -> f (fromCMouseEvent cevent cx cy cmodifiers))

-----------------------------------------------------------------------------------------
--  WindowKeyboard
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersWindowKeyboard #-}
handlersWindowKeyboard :: MVar (PtrMap WindowHandle (KeyboardEvent -> IO ()))
handlersWindowKeyboard
  = unsafePerformIO (newMVar empty)

setWindowKeyboardHandler :: WindowHandle -> (KeyboardEvent -> IO ()) -> IO ()
setWindowKeyboardHandler hwnd handler
  = setHandler hwnd handler handlersWindowKeyboard

setWindowKeyboardDefHandler :: WindowHandle -> IO ()
setWindowKeyboardDefHandler hwnd 
  = setDefHandler hwnd handlersWindowKeyboard

getWindowKeyboardHandler :: WindowHandle -> IO (KeyboardEvent -> IO ())
getWindowKeyboardHandler hwnd
  = getHandler hwnd (\_ -> return ()) handlersWindowKeyboard

handleWindowKeyboard :: WindowHandle -> CInt -> CInt -> CWord -> IO ()
handleWindowKeyboard hwnd cevent ckey cmodifiers
  = invokeHandler hwnd handlersWindowKeyboard (\f -> f (fromCKeyboardEvent cevent ckey cmodifiers))

-----------------------------------------------------------------------------------------
-- WindowContextMenu
-----------------------------------------------------------------------------------------
 
{-# NOINLINE handlersWindowContextMenu #-}
handlersWindowContextMenu :: MVar (PtrMap WindowHandle (Point -> Modifiers -> IO ()))
handlersWindowContextMenu
  = unsafePerformIO (newMVar empty)

setWindowContextMenuHandler :: WindowHandle -> (Point -> Modifiers -> IO ()) -> IO ()
setWindowContextMenuHandler hwnd handler
  = setHandler hwnd handler handlersWindowContextMenu

setWindowContextMenuDefHandler :: WindowHandle -> IO ()
setWindowContextMenuDefHandler hwnd 
  = setDefHandler hwnd handlersWindowContextMenu
  
getWindowContextMenuHandler :: WindowHandle -> IO (Point -> Modifiers -> IO ())
getWindowContextMenuHandler hwnd
  = getHandler hwnd (\p m -> return ()) handlersWindowContextMenu

handleWindowContextMenu :: WindowHandle -> CInt -> CInt -> CWord -> IO ()
handleWindowContextMenu hwnd cx cy cmods
  = invokeHandler hwnd handlersWindowContextMenu (\f -> f (fromCPoint cx cy) (fromCModifiers cmods))

-----------------------------------------------------------------------------------------
-- MenuDestroy
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersMenuDestroy #-}
handlersMenuDestroy :: MVar (PtrMap MenuHandle (IO ()))
handlersMenuDestroy
  = unsafePerformIO (newMVar empty)

setMenuDestroyHandler :: MenuHandle -> IO () -> IO ()
setMenuDestroyHandler hmenu handler
  = setHandler hmenu handler handlersMenuDestroy

setMenuDestroyDefHandler :: MenuHandle -> IO ()
setMenuDestroyDefHandler hmenu
  = setDefHandler hmenu handlersMenuDestroy

getMenuDestroyHandler :: MenuHandle -> IO (IO ())
getMenuDestroyHandler hmenu
  = getHandler hmenu (return ()) handlersMenuDestroy

handleMenuDestroy :: MenuHandle -> IO ()
handleMenuDestroy hmenu
  = do map <- takeMVar handlersMenuDestroy
       putMVar handlersMenuDestroy (delete hmenu map)
       case lookup hmenu map of
         Nothing -> return ()
         Just io -> safeio io

-----------------------------------------------------------------------------------------
-- ActionUpdate
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersActionUpdate #-}
handlersActionUpdate :: MVar (PtrMap ActionHandle (IO ()))
handlersActionUpdate
  = unsafePerformIO (newMVar empty)

setActionUpdateHandler :: ActionHandle -> IO () -> IO ()
setActionUpdateHandler haction handler
  = setHandler haction handler handlersActionUpdate

setActionUpdateDefHandler :: ActionHandle -> IO ()
setActionUpdateDefHandler haction
  = setDefHandler haction handlersActionUpdate

getActionUpdateHandler :: ActionHandle -> IO (IO ())
getActionUpdateHandler haction
  = getHandler haction (return ()) handlersActionUpdate

handleActionUpdate :: ActionHandle -> IO ()
handleActionUpdate haction
  = invokeHandler haction handlersActionUpdate id

-----------------------------------------------------------------------------------------
-- ActionCommand
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersActionCommand #-}
handlersActionCommand :: MVar (PtrMap ActionHandle (IO ()))
handlersActionCommand
  = unsafePerformIO (newMVar empty)

setActionCommandHandler :: ActionHandle -> IO () -> IO ()
setActionCommandHandler haction handler
  = setHandler haction handler handlersActionCommand

setActionCommandDefHandler :: ActionHandle -> IO ()
setActionCommandDefHandler haction
  = setDefHandler haction handlersActionCommand

getActionCommandHandler :: ActionHandle -> IO (IO ())
getActionCommandHandler haction
  = getHandler haction (return ()) handlersActionCommand

handleActionCommand :: ActionHandle -> IO ()
handleActionCommand haction
  = invokeHandler haction handlersActionCommand id

-----------------------------------------------------------------------------------------
-- ActionDestroy
-----------------------------------------------------------------------------------------

{-# NOINLINE actionBitmaps #-}
actionBitmaps :: MVar (PtrMap ActionHandle Bitmap)
actionBitmaps = unsafePerformIO (newMVar empty)

{-# NOINLINE handlersActionDestroy #-}
handlersActionDestroy :: MVar (PtrMap ActionHandle (IO ()))
handlersActionDestroy
  = unsafePerformIO (newMVar empty)

setActionDestroyHandler :: ActionHandle -> IO () -> IO ()
setActionDestroyHandler haction handler
  = setHandler haction handler handlersActionDestroy

setActionDestroyDefHandler :: ActionHandle -> IO ()
setActionDestroyDefHandler haction
  = setDefHandler haction handlersActionDestroy

getActionDestroyHandler :: ActionHandle -> IO (IO ())
getActionDestroyHandler haction
  = getHandler haction (return ()) handlersActionDestroy

handleActionDestroy :: ActionHandle -> IO ()
handleActionDestroy haction
  = do map <- takeMVar handlersActionDestroy
       bmps <- takeMVar actionBitmaps
       putMVar actionBitmaps (delete haction bmps)
       setActionCommandDefHandler haction
       setActionUpdateDefHandler haction
       putMVar handlersActionDestroy (delete haction map)
       case lookup haction map of
         Nothing -> return ()
         Just io -> safeio io

{-----------------------------------------------------------------------------------------
  ProcessDismiss
-----------------------------------------------------------------------------------------}

{-# NOINLINE handlersProcessDismiss #-}
handlersProcessDismiss :: MVar (IO ())
handlersProcessDismiss
  = unsafePerformIO (newMVar (return ()))

setProcessDismissHandler :: IO () -> IO ()
setProcessDismissHandler handler = do
    _ <- takeMVar handlersProcessDismiss
    putMVar  handlersProcessDismiss handler

setProcessDismissDefHandler :: IO ()
setProcessDismissDefHandler = do
    _ <- takeMVar handlersProcessDismiss
    putMVar  handlersProcessDismiss (return ())
    
getProcessDismissHandler :: IO (IO ())
getProcessDismissHandler =
    readMVar handlersProcessDismiss

handleProcessDismiss :: IO ()
handleProcessDismiss = readMVar handlersProcessDismiss >>= id

{-----------------------------------------------------------------------------------------
  ProcessDestroy
-----------------------------------------------------------------------------------------}

{-# NOINLINE handlersProcessDestroy #-}
handlersProcessDestroy :: MVar (IO ())
handlersProcessDestroy
  = unsafePerformIO (newMVar (return ()))

setProcessDestroyHandler :: IO () -> IO ()
setProcessDestroyHandler handler = do
    _ <- takeMVar handlersProcessDestroy
    putMVar  handlersProcessDestroy handler

setProcessDestroyDefHandler :: IO ()
setProcessDestroyDefHandler = do
    _ <- takeMVar handlersProcessDestroy
    putMVar  handlersProcessDestroy (return ())

getProcessDestroyHandler :: IO (IO ())
getProcessDestroyHandler =
    readMVar handlersProcessDestroy
    
handleProcessDestroy :: IO ()
handleProcessDestroy = readMVar handlersProcessDestroy >>= id

-----------------------------------------------------------------------------------------
--  Keep track of all timers
-----------------------------------------------------------------------------------------
{-# NOINLINE timers #-}
timers :: MVar [TimerHandle]
timers
  = unsafePerformIO (newMVar [])

registerTimer :: TimerHandle -> IO ()
registerTimer htimer
  = do htimers <- takeMVar timers
       putMVar timers (htimer : L.delete htimer htimers)

unregisterTimer :: TimerHandle -> IO ()
unregisterTimer htimer
  = do htimers <- takeMVar timers
       putMVar timers (L.delete htimer htimers)

-- | 'getAllTimerHandles' returns list of handles for all created timers.
getAllTimerHandles :: IO [TimerHandle]
getAllTimerHandles = readMVar timers

-----------------------------------------------------------------------------------------
--  TimerEvent
-----------------------------------------------------------------------------------------
{-# NOINLINE handlersTimer #-}
handlersTimer :: MVar (PtrMap TimerHandle (IO ()))
handlersTimer
  = unsafePerformIO (newMVar empty)

handleTimer :: TimerHandle -> IO ()
handleTimer htimer
  = invokeHandler htimer handlersTimer id

setTimerHandler :: TimerHandle -> IO () -> IO ()
setTimerHandler htimer handler
  = setHandler htimer handler handlersTimer

setTimerDefHandler :: TimerHandle -> IO ()
setTimerDefHandler htimer
  = setDefHandler htimer handlersTimer
  
getTimerHandler :: TimerHandle -> IO (IO ())
getTimerHandler htimer
  = getHandler htimer (return ()) handlersTimer

-----------------------------------------------------------------------------------------
-- TimerDestroy
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersTimerDestroy #-}
handlersTimerDestroy :: MVar (PtrMap TimerHandle (IO ()))
handlersTimerDestroy
  = unsafePerformIO (newMVar empty)

handleTimerDestroy :: TimerHandle -> IO ()
handleTimerDestroy htimer
  = do map <- takeMVar handlersTimerDestroy
       setTimerDefHandler htimer
       putMVar handlersTimerDestroy (delete htimer map)
       case lookup htimer map of
         Nothing -> return ()
         Just io -> safeio io
       unregisterTimer htimer

setTimerDestroyHandler :: TimerHandle -> IO () -> IO ()
setTimerDestroyHandler htimer handler
  = setHandler htimer handler handlersTimerDestroy

setTimerDestroyDefHandler :: TimerHandle -> IO ()
setTimerDestroyDefHandler htimer
  = setDefHandler htimer handlersTimerDestroy
  
getTimerDestroyHandler :: TimerHandle -> IO (IO ())
getTimerDestroyHandler htimer
  = getHandler htimer (return ()) handlersTimerDestroy

-----------------------------------------------------------------------------------------
-- ToolDestroy
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersToolDestroy #-}
handlersToolDestroy :: MVar (PtrMap ToolHandle (IO ()))
handlersToolDestroy
  = unsafePerformIO (newMVar empty)

setToolDestroyHandler :: ToolHandle -> IO () -> IO ()
setToolDestroyHandler htool handler
  = setHandler htool handler handlersToolDestroy

setToolDestroyDefHandler :: ToolHandle -> IO ()
setToolDestroyDefHandler htool
  = setDefHandler htool handlersToolDestroy

getToolDestroyHandler :: ToolHandle -> IO (IO ())
getToolDestroyHandler htool
  = getHandler htool (return ()) handlersToolDestroy

handleToolDestroy :: ToolHandle -> IO ()
handleToolDestroy htool
  = do map <- takeMVar handlersToolDestroy
       putMVar handlersToolDestroy (delete htool map)
       case lookup htool map of
         Nothing -> return ()
         Just io -> safeio io

-----------------------------------------------------------------------------------------
-- IndicatorCommand
-----------------------------------------------------------------------------------------

{-# NOINLINE handlersIndicatorCommand #-}
handlersIndicatorCommand :: MVar (PtrMap IndicatorHandle (IO ()))
handlersIndicatorCommand
  = unsafePerformIO (newMVar empty)

handleIndicatorCommand :: IndicatorHandle -> IO ()
handleIndicatorCommand hindicator
  = invokeHandler hindicator handlersIndicatorCommand id

setIndicatorCommandHandler :: IndicatorHandle -> IO () -> IO ()
setIndicatorCommandHandler hindicator handler
  = setHandler hindicator handler handlersIndicatorCommand

setIndicatorCommandDefHandler :: IndicatorHandle -> IO ()
setIndicatorCommandDefHandler hindicator
  = setDefHandler hindicator handlersIndicatorCommand

getIndicatorCommandHandler :: IndicatorHandle -> IO (IO ())
getIndicatorCommandHandler hindicator
  = getHandler hindicator (return ()) handlersIndicatorCommand

-----------------------------------------------------------------------------------------
-- IndicatorDestroy
-----------------------------------------------------------------------------------------

{-# NOINLINE indicatorBitmaps #-}
indicatorBitmaps :: MVar (PtrMap IndicatorHandle Bitmap)
indicatorBitmaps = unsafePerformIO (newMVar empty)

{-# NOINLINE handlersIndicatorDestroy #-}
handlersIndicatorDestroy :: MVar (PtrMap IndicatorHandle (IO ()))
handlersIndicatorDestroy
  = unsafePerformIO (newMVar empty)

setIndicatorDestroyHandler :: IndicatorHandle -> IO () -> IO ()
setIndicatorDestroyHandler hindicator handler
  = setHandler hindicator handler handlersIndicatorDestroy

setIndicatorDestroyDefHandler :: IndicatorHandle -> IO ()
setIndicatorDestroyDefHandler hindicator
  = setDefHandler hindicator handlersIndicatorDestroy

getIndicatorDestroyHandler :: IndicatorHandle -> IO (IO ())
getIndicatorDestroyHandler hindicator
  = getHandler hindicator (return ()) handlersIndicatorDestroy

handleIndicatorDestroy :: IndicatorHandle -> IO ()
handleIndicatorDestroy hindicator
  = do map <- takeMVar handlersIndicatorDestroy
       bmps <- takeMVar indicatorBitmaps
       putMVar indicatorBitmaps (delete hindicator bmps)
       setIndicatorCommandDefHandler hindicator
       putMVar handlersIndicatorDestroy (delete hindicator map)
       case lookup hindicator map of
         Nothing -> return ()
         Just io -> safeio io

{-----------------------------------------------------------------------------------------
  foreign exports
-----------------------------------------------------------------------------------------}
foreign export ccall handleWindowDismiss :: WindowHandle -> IO ()
foreign export ccall handleWindowDestroy :: WindowHandle -> IO ()
foreign export ccall handleWindowPaint :: WindowHandle -> CanvasHandle -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign export ccall handleWindowResize :: WindowHandle -> CInt -> CInt -> IO ()
foreign export ccall handleWindowScroll :: WindowHandle -> CInt -> CInt -> IO ()
foreign export ccall handleWindowMouse :: WindowHandle -> CInt -> CInt -> CInt -> CWord -> IO ()
foreign export ccall handleWindowKeyboard :: WindowHandle -> CInt -> CInt -> CWord -> IO ()
foreign export ccall handleWindowDeactivate :: WindowHandle -> IO ()
foreign export ccall handleWindowActivate :: WindowHandle -> IO ()
foreign export ccall handleContainerReLayout :: WindowHandle -> IO ()
foreign export ccall handleWindowContextMenu :: WindowHandle -> CInt -> CInt -> CWord -> IO ()
foreign export ccall handleControlCommand :: WindowHandle -> IO ()
foreign export ccall handleTrackBarIncrement :: WindowHandle -> IO ()
foreign export ccall handleTrackBarDecrement :: WindowHandle -> IO ()
foreign export ccall handleTreeViewGetter :: WindowHandle -> RowHandle -> CInt -> Ptr a -> IO CBool
foreign export ccall handleMenuDestroy :: MenuHandle -> IO ()
foreign export ccall handleToolDestroy :: ToolHandle -> IO ()
foreign export ccall handleActionCommand :: ActionHandle -> IO ()
foreign export ccall handleActionUpdate :: ActionHandle -> IO ()
foreign export ccall handleActionDestroy :: ActionHandle -> IO ()
foreign export ccall handleTimer :: TimerHandle -> IO ()
foreign export ccall handleTimerDestroy :: TimerHandle -> IO ()
foreign export ccall handleProcessDismiss :: IO ()
foreign export ccall handleProcessDestroy :: IO ()
foreign export ccall handleIndicatorCommand :: IndicatorHandle -> IO ()
foreign export ccall handleIndicatorDestroy :: IndicatorHandle -> IO ()
