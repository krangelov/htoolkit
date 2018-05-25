{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Graphics.UI.GIO
import Control.Monad.Trans
import Data.IORef

data Picture 
  = NullPic
  | Pixel
  | Text String
  | PolyLine [Point]
  | Rectangle Size
  | Arc Size Double Double
  | Curve Point Point Point
--  | Raster Bitmap
  | Pen PenModifier Picture 
  | Move Offset Picture 
  | Transform Transformation Picture 
  | Tag Tag Picture 
  | Overlay Picture Picture 
  | ConstrainOverlay RelSize RelSize Picture Picture 
  | Clip Picture Picture 
  deriving (Read,Show)
  
data RelSize
  = None 
  | Fixed Bool Int
  | Prop Bool Double
  deriving (Read, Show)

type Tag = Int

data Offset 
  = OffDir CompassDirection 
  | OffPropX Double 
  | OffPropY Double 
  deriving (Read, Show) 
  
-- A.1 Graphical attributes 
-- The Pen constructor associates a set of graphical (attribute,value) pairs 
-- with a picture. The attributes currently supported are (the definition 
-- of the types used by some of the attributes have been elided for lack of 
-- space): 

type PenModifier = [PenAttr] 
data PenAttr
  = Foreground Color
  | LineStyle LineStyle -- dashed lines or not?
  | JoinStyle JoinStyle -- for polyline joints 
  | CapStyle CapStyle -- end point caps. 
  | Fill Bool -- fill picture or not? 
  | Invisible Bool -- should the picture be drawn? 
--  | Font Font -- what font to use.
--  | Function PenFunction -- blit op to eventually apply 
	deriving (Read, Show)
 
 
data Transformation
  = Identity
  | Scale Double Double
  | Rotate Double
  | Xlt Double Double
  | Combine Transformation Transformation
  deriving (Read, Show)

data CompassDirection
  = West
  | NorthWest
  | North
  | NorthEast
  | East
  | EastSouth
  | South
  | SouthWest
  | Centre
  deriving (Read, Show)
-------------------------------------------------------------------------

main = start "Picture" "1.0" SDI [] initPic

initPic = do
	bmpOpen  <- readBitmap "open.bmp" []
	
	mfile  <- menu [title =: "&File"] mainMenu
	mopen  <- menuitem [title =: "&Open",  accel =: KeyChar '\^O', icon =: Just bmpOpen] mfile
	mclose <- menuitem [title =: "&Close", enabled=:False] mfile
	menuline mfile
	menuitem [title =: "&Exit", on command =: halt] mfile
	
	set mopen [on command =: onFileOpen mclose]	
	
	return ()
	where
		onFileOpen mclose = do
			mb_fname <- runInputFileDialog "Open picture" [("Picture (*.pic)",["*.pic"])] Nothing
			case mb_fname of
				Nothing    -> return ()
				Just fname -> do
					bmpRLeft  <- readBitmap "rleft.bmp"  []
					bmpRRight <- readBitmap "rright.bmp" []
					bmpXPlus  <- readBitmap "xplus.bmp"  []
					bmpXMinus <- readBitmap "xminus.bmp" []
					bmpYPlus  <- readBitmap "yplus.bmp"  []
					bmpYMinus <- readBitmap "yminus.bmp" []
					
					(pic :: Picture) <- fmap read (readFile fname)

					ref <- newIORef (0,1,1)
					w <- window [title =: fname, view =: sz 400 400]
								
					mpic  <- menu [title =: "&Picture"] mainMenu
					menuitem [title =: "Rotate Left",  accel =: KeyChar '\^L', icon =: Just bmpRLeft,  on command =: onRotatePicture w (-pi/4) ref] mpic
					menuitem [title =: "Rotate Right", accel =: KeyChar '\^R', icon =: Just bmpRRight, on command =: onRotatePicture w ( pi/4) ref] mpic
					menuline mpic
					menuitem [title =: "ScaleX +", accel =: KeyChar 'X',   icon =: Just bmpXPlus,  on command =: onScalePicture w (2  ,1) pic ref] mpic
					menuitem [title =: "ScaleX -", accel =: KeyChar '\^X', icon =: Just bmpXMinus, on command =: onScalePicture w (0.5,1) pic ref] mpic
					menuline mpic
					menuitem [title =: "ScaleY +", accel =: KeyChar 'Y',   icon =: Just bmpYPlus,  on command =: onScalePicture w (1,2  ) pic ref] mpic
					menuitem [title =: "ScaleY -", accel =: KeyChar '\^Y', icon =: Just bmpYMinus, on command =: onScalePicture w (1,0.5) pic ref] mpic

					set mclose [enabled =: True, on command =: destroyWidget w]
					set w [ on destroy =: onDestroyPicWindow mpic mclose
					      , on resize  =: \s -> repaint w
					      , on paint   =: onPaint w pic ref
					      ]
					showWindow w
			where
				onDestroyPicWindow mpic mclose = do
					set mclose [enabled =: False, off command]
					destroyWidget mpic

		onRotatePicture w delta ref = do
			modifyIORef ref (\(angle,scalex,scaley) -> (angle+delta,scalex,scaley))
			repaint w
		onScalePicture  w (sx,sy) pic ref = do
			modifyIORef ref (\(angle,scalex,scaley) -> (angle,scalex*sx,scaley*sy))
			repaint w
		
		onPaint w pic ref can _ _ = do
			(angle,scalex,scaley) <- readIORef ref
			Size x y <- get w view
			translateCanvas (fromIntegral (x `quot` 2)) (fromIntegral (y `quot` 2)) can
			rotateCanvas angle can
			scaleCanvas scalex scaley can
			renderPicture can pic

renderPicture :: Canvas -> Picture -> IO ()
renderPicture can NullPic             = return ()
renderPicture can (Overlay pic1 pic2) = do
	renderPicture can pic1
	renderPicture can pic2
renderPicture can (Transform trans pic) = do
	applyTransformation trans
	renderPicture can pic
	undoTransformation  trans
	where
		applyTransformation :: Transformation -> IO ()
		applyTransformation Identity 	    = return ()
		applyTransformation (Combine t1 t2) = applyTransformation t1 >> applyTransformation t2
		applyTransformation (Rotate d)      = rotateCanvas d can
		applyTransformation (Scale dsx dsy) = scaleCanvas dsx dsy can
		applyTransformation (Xlt dx dy)     = translateCanvas dx dy can
		
		undoTransformation :: Transformation -> IO ()
		undoTransformation Identity 	    = return ()
		undoTransformation (Combine t1 t2)  = undoTransformation t2 >> undoTransformation t1
		undoTransformation (Rotate d)       = rotateCanvas (-d) can
		undoTransformation (Scale dsx dsy)  = scaleCanvas (1/dsx) (1/dsy) can
		undoTransformation (Xlt dx dy) 	    = translateCanvas dx dy can
renderPicture can (Text txt) 	   = drawString (pt 0 0) txt can
renderPicture can Pixel	 	   = drawPoint  (pt 0 0) can
renderPicture can (PolyLine lines) = drawPolyline lines can
renderPicture can (Rectangle rsize)= drawRect (rectOfSize rsize) can
