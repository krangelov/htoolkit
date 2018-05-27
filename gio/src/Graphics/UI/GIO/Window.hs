-----------------------------------------------------------------------------------------
{-| Module      :  Window
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Windows
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Window (Window, window, dialog, view, windowPos, runDialog, showWindow, hideWindow) where

import qualified Graphics.UI.Port as Lib
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events
import Graphics.UI.GIO.Canvas
import Graphics.UI.GIO.Layout
import Control.Monad( when )

{--------------------------------------------------------------------

--------------------------------------------------------------------}
-- | A main window widget.
data Window  = Window{ hwnd         :: WindowHandle
                     , vdomain      :: Var Size
                     , vresizeable  :: Var Bool
                     , vautosize    :: Var Bool
                     , vpen         :: Var Pen
                     , vbufferMode  :: Var BufferMode
                     , vpaint       :: Var PaintFunction
                     , vlayout      :: Var Layout
                     }

-- | Create a window
window :: [Prop Window] -> IO Window
window props
  = do w <- Lib.createWindow >>= form windowPen
       set w props
       return w

-- | Create a modeless dialog box. If you want to make the dialog modal use 'runDialog' function.
dialog :: [Prop Window]
       -> Maybe Window  -- ^ The owner window of the dialog being created.
                        -- If this parameter is Nothing or is a @Just handle@ of a window instead of dialog
                        -- then the dialog owner is the process window. A dialog is always above its owner
                        -- in the z-order and the system automatically destroys a dialog when its owner is
                        -- destroyed. The dialog is automatically hidded when its owner is minimized.
       -> IO Window
dialog props mb_parent
  = do let hparent = maybe Lib.nullHandle hwindow mb_parent
       w <- Lib.createDialog hparent >>= form dialogPen
       set w props
       return w

-- | Blocks in a recursive main loop until the dialog is destroyed.
runDialog :: Window -> IO ()
runDialog w = Lib.runDialog (hwindow w)

-- | Activates the window and displays it in its current size and position.
showWindow :: Window -> IO ()
showWindow w = Lib.setWindowVisible (hwindow w) True

-- | Hides the window and activates another window.
hideWindow :: Window -> IO ()
hideWindow w = Lib.setWindowVisible (hwindow w) False

form :: Pen -> WindowHandle -> IO Window
form pen hwindow
  = do w <- do vpaint     <- newVar (\_ _ _ -> return ())
               vautosize  <- newVar True
               vlayout    <- newVar empty
               vdomain    <- newVar (sz 0 0)
               vresizeable<- newVar True
               vpen       <- newVar pen
               vbufferMode<- newVar UnBuffered
               return (Window hwindow vdomain vresizeable vautosize
                              vpen vbufferMode vpaint vlayout
                      )
       recolorWindow w
       set w [on relayout =: relayoutWindow w]
       -- just by setting a dummy paint function, we will at least intialize the canvas properly on a repaint
       set w [on paint =: (\_ _ _ -> return ())]
       return w

relayoutWindow :: Window -> IO ()
relayoutWindow w
  = do view   <- get w view
       domain <- get w domain
       lay    <- getVar (vlayout w)
       needed <- getLayoutSize lay
       let d1 = maxSize domain needed
           d2 = maxSize d1 view
       Lib.setDialogMinSize (hwindow w) d1
       Lib.setWindowDomainSize (hwindow w) d1
       layoutInRect (rectOfSize d2) lay
       return ()

recolorWindow w
  = do col   <- get w color
       bgcol <- get w bgcolor
       fill  <- get w fillStyle
       Lib.setWindowColor (hwindow w) col bgcol fill
       repaint w
       relayoutWindow w


instance Titled Window where
  title = newStdAttr hwindow Lib.getWindowTitle Lib.setWindowTitle

instance Scrollable Window where
  scroll = newStdEvent hwindow Lib.getWindowScrollHandler Lib.setWindowScrollHandler Lib.setWindowScrollDefHandler
  domain = newAttr (\w   -> getVar (vdomain w))
                   (\w x -> setVar (vdomain w) x >> relayoutWindow w)
  origin = newStdAttr hwindow Lib.getWindowScrollOrigin Lib.setWindowScrollOrigin

-- | The size of the current view area.
view :: Attr Window Size
view = newStdAttr hwindow Lib.getWindowViewSize Lib.setWindowViewSize

-- | The 'windowPos' attribute is a write only and allows to set the position of the window.
-- The position can be specified using the exact bounding rectangle or in a way relative
-- to the parent window or to the screen.
windowPos :: Attr Window WindowPosition
windowPos = writeAttr "windowPos" (\w pos -> Lib.setWindowPosition (hwindow w) pos)

instance Visible Window where
  visible = newStdAttr hwindow Lib.getWindowVisible Lib.setWindowVisible

instance Dismissible Window where
  dismissWidget w    = Lib.dismissWindow (hwindow w)
  dismiss            = newStdEvent hwindow Lib.getWindowDismissHandler Lib.setWindowDismissHandler Lib.setWindowDismissDefHandler

instance Deadly Window where
  destroyWidget w    = Lib.destroyWindow (hwindow w)
  destroy            = newStdEvent hwindow Lib.getWindowDestroyHandler Lib.setWindowDestroyHandler Lib.setWindowDestroyDefHandler

instance Dimensions Window where
  frame     = newStdAttr hwindow Lib.getWindowRect (\hwnd rect -> Lib.setWindowPosition hwnd (WinPosExact rect))

instance Drawn Window where
  pen = newAttr (getVar . vpen) (\w pen -> setVar (vpen w) pen >> recolorWindow w)

  bufferMode = newStdAttr vbufferMode getVar setVar

instance HasFont Window where
  font = mapAttr penFont (\pen c -> pen{penFont=c}) pen

instance Reactive Window where
  mouse       = newStdEvent hwindow Lib.getWindowMouseHandler       Lib.setWindowMouseHandler       Lib.setWindowMouseDefHandler
  keyboard    = newStdEvent hwindow Lib.getWindowKeyboardHandler    Lib.setWindowKeyboardHandler    Lib.setWindowKeyboardDefHandler
  contextMenu = newStdEvent hwindow Lib.getWindowContextMenuHandler Lib.setWindowContextMenuHandler Lib.setWindowContextMenuDefHandler

instance Activity Window where
  activate  = newStdEvent hwindow Lib.getWindowActivateHandler   Lib.setWindowActivateHandler   Lib.setWindowActivateDefHandler
  deactivate= newStdEvent hwindow Lib.getWindowDeactivateHandler Lib.setWindowDeactivateHandler Lib.setWindowDeactivateDefHandler
  isactive  = readAttr "isactive" (\w -> do
  			handles <- Lib.getAllWindowHandles
  			case handles of
  			   []     -> return False
  			   (x:xs) -> return (hwindow w == head handles))

instance Resizeable Window where
  resize    = newStdEvent hwindow Lib.getWindowResizeHandler     Lib.setWindowResizeHandler     Lib.setWindowResizeDefHandler
  resizeable
    = newAttr (\w   -> getVar (vresizeable w))
              (\w x -> do resize <- get w resizeable
                          when (x /= resize) (do Lib.setWindowResizeable (hwindow w) x
                                                 setVar (vresizeable w) x))

instance Paint Window where
  repaint w = do Lib.invalidateWindow (hwindow w) 
  paint     = newEvent (getVar . vpaint)
                       (\w h -> Lib.setWindowPaintHandler    (hwindow w) (wndpaint w h) >> setVar (vpaint w) h)
                       (\w   -> Lib.setWindowPaintDefHandler (hwindow w) >> setVar (vpaint w) (\_ _ _ -> return ()))
            where
              wndpaint w paintfun hcanvas updArea
                = do pen   <- get w pen
                     bmode <- get w bufferMode
                     withCanvas bmode pen hcanvas $ \can -> paintfun can updArea []
  paintIn w bmode f = do 
     pen   <- get w pen
     Lib.drawInWindow (hwindow w) (\hcanvas -> withCanvas bmode pen hcanvas f)

instance Able Window where
  enabled = newStdAttr hwindow Lib.getWindowEnabled Lib.setWindowEnabled

instance Container Window where
  layout 
    = writeAttr "layout" (\w c -> do
       let new_lay = pack c
       old_lay <- getVar (vlayout w)
       autosize <- get w autosize
       domain   <- get w domain
       needed   <- getLayoutSize new_lay
       let d = maxSize domain needed
       Lib.setWindowDomainSize (hwindow w) d
       when autosize (set w [view =: d])
       view     <- get w view
       updateControlsVisibility old_lay new_lay
       Lib.setDialogMinSize (hwindow w) d
       layoutInRect (rectOfSize (maxSize d view)) new_lay
       setVar (vlayout w) new_lay)
  autosize   = varAttr vautosize
  layoutSize = readAttr "layoutSize" (\w -> getVar (vlayout w) >>= getLayoutSize)
  relayout   = newStdEvent hwindow Lib.getContainerReLayoutHandler Lib.setContainerReLayoutHandler Lib.setContainerReLayoutDefHandler
  hwindow w  = hwnd w
