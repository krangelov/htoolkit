{-# LANGUAGE RecursiveDo #-}
module Main where

import Data.IORef
import Graphics.UI.GIO

type Tower = [Int]
type Moves = [Int]
data Towers
   = Towers
	{ moves	 :: !Moves
	, tower1 :: !Tower
	, tower2 :: !Tower
	, tower3 :: !Tower
	}

speed1 = 40
speed2 = 26
speed3 = 13
speed4 = 6
speed5 = 1

minDisks = 2
maxDisks = 20
xOffs	 = maxDisks * 10 + 1

--	Starting the program

main :: IO ()
main = start "Hanoi" "1.0" SDI [] $ mdo
	w <- window [domain =: Size 430 180, on paint =: onPaint ref]
	showWindow w

	ref <- newIORef (initTowers 0)

	mhanoi <- menu [title =: "&Hanoi"] mainMenu
	mrun <- menu [title =: "&Run (nr disks)"] mhanoi
	sequence [menuRadioItem [title =: show i, on command =: onRun i mrun mcont mhalt t w ref] mrun
	                        | i <- [minDisks..maxDisks]] >>= setMenuRadioGroup
	mhalt <- menuitem [title =: "Halt",     accel =: KeyChar '.', enabled =: False, on command =: onHalt mrun mcont mhalt t] mhanoi
	mcont <- menuitem [title =: "Continue", accel =: KeyChar ',', enabled =: False, on command =: onCont mrun mcont mhalt t] mhanoi
	mspeed <- menu [title =: "&Speed"] mhanoi
	mVerySlow <- menuRadioItem [title =: "V&ery Slow", accel =: KeyChar '\^A', on command =: onSetSpeed speed1 t] mspeed
	mSlow     <- menuRadioItem [title =: "&Slow",      accel =: KeyChar '\^S', on command =: onSetSpeed speed2 t] mspeed
	mNormal   <- menuRadioItem [title =: "&Normal",    accel =: KeyChar '\^D', on command =: onSetSpeed speed3 t] mspeed
	mFast     <- menuRadioItem [title =: "&Fast",      accel =: KeyChar '\^F', on command =: onSetSpeed speed4 t] mspeed
	mVeryFast <- menuRadioItem [title =: "&Very Fast", accel =: KeyChar '\^G', on command =: onSetSpeed speed5 t] mspeed
	setMenuRadioGroup [mVerySlow,mSlow,mNormal,mFast,mVeryFast]
	menuline mhanoi
	menuitem [title =: "E&xit", on command =: halt] mhanoi
	
	t <- timer [interval =: speed3, enabled =: False, on command =: onTimer mrun mhalt t w ref]
	return ()
	where
		-- The function for the Run command.
		onRun n mrun mcont mhalt timer w ref = do
			set mrun  [enabled =: False]
			set mcont [enabled =: False]
			set mhalt [enabled =: True]
			set timer [enabled =: True]
			writeIORef ref (initTowers n)
			repaint w

		-- The function for the Halt command.
		onHalt mrun mcont mhalt timer = do
			set mrun  [enabled =: True]
			set mcont [enabled =: True]
			set mhalt [enabled =: False]
			set timer [enabled =: False]

		-- The function for the Continue command.
		onCont mrun mcont mhalt timer = do
			set mrun  [enabled =: False]
			set mcont [enabled =: False]
			set mhalt [enabled =: True]
			set timer [enabled =: True]
		
		-- The timer function: take a move from the list of all moves and show it in the window.
		onTimer mrun mhalt timer w ref = do
			towers <- readIORef ref
			stepHanoi towers
			where
				stepHanoi towers@(Towers {moves=[]}) = do
					set mrun  [enabled =: True]
					set mhalt [enabled =: False]
					set timer [enabled =: False]
				stepHanoi towers = do
					paintIn w UnBuffered drawf
					writeIORef ref towers1
					where
						(drawf,towers1)	= changeTowers towers
		
						changeTowers :: Towers -> (Canvas -> IO (),Towers)
						changeTowers towers@(Towers {moves=(1:2:moves),tower1=(f1:r1),tower2=t2}) =
							(drawMove 1 2 f1 (length r1) (length t2),towers{moves=moves,tower1=r1,tower2=(f1:t2)})
						changeTowers towers@(Towers {moves=(1:3:moves),tower1=(f1:r1),tower3=t3}) =
							(drawMove 1 3 f1 (length r1) (length t3),towers{moves=moves,tower1=r1,tower3=(f1:t3)})
						changeTowers towers@(Towers {moves=(2:1:moves),tower2=(f2:r2),tower1=t1}) =
							(drawMove 2 1 f2 (length r2) (length t1),towers{moves=moves,tower2=r2,tower1=(f2:t1)})
						changeTowers towers@(Towers {moves=(2:3:moves),tower2=(f2:r2),tower3=t3}) =
							(drawMove 2 3 f2 (length r2) (length t3),towers{moves=moves,tower2=r2,tower3=(f2:t3)})
						changeTowers towers@(Towers {moves=(3:1:moves),tower3=(f3:r3),tower1=t1}) =
							(drawMove 3 1 f3 (length r3) (length t1),towers{moves=moves,tower3=r3,tower1=(f3:t1)})
						changeTowers towers@(Towers {moves=(3:2:moves),tower3=(f3:r3),tower2=t2}) =
							(drawMove 3 2 f3 (length r3) (length t2),towers{moves=moves,tower3=r3,tower2=(f3:t2)})

						drawMove :: Int -> Int -> Int -> Int -> Int -> Canvas -> IO ()
						drawMove start end disk lenfr lento can = do
							eraseDisk (Rect (fx-w) fy (fx+w) (fy+10)) can
							drawDisk  (Rect (tx-w) ty (tx+w) (ty+10)) can
							where
								tx	= end  *xOffs
								ty	= 10+10*(maxDisks-lento)
								fx	= start*xOffs
								fy	= 10+10*(maxDisks-lenfr)
								w	= disk *5

		-- The update function: redraw the towers
		onPaint ref can (Rect l t r b) _ = do
			twrs <- readIORef ref
			drawTowerPos 1 (tower1 twrs) can
			drawTowerPos 2 (tower2 twrs) can
			drawTowerPos 3 (tower3 twrs) can
			drawLine (Point l y) (Point r y) can
			where
				y = 20+10*maxDisks

		-- Set the speed of a (possibly running) Hanoi simulation.
		onSetSpeed :: Int -> Timer -> IO ()
		onSetSpeed speed timer = set timer [interval =: speed]


-- The initial Towers value, given the number of disks
initTowers :: Int -> Towers
initTowers nr_disks =
	Towers
	  { moves	= hanoi nr_disks 1 2 3
	  , tower1	= [1..nr_disks]
	  , tower2	= []
	  , tower3	= []
	  }
	where
		hanoi :: Int -> Int -> Int -> Int -> Moves	-- The function that calculates the list of disk moves
		hanoi n start end via
			| n==0		= []
			| otherwise	= hanoi m start via end ++ (start:end:hanoi m via end start)
			where
				m	= n-1

drawTowerPos :: Int -> Tower -> Canvas -> IO ()
drawTowerPos pos tower can = do
	drawTower pos (maxDisks-length tower) tower can
	drawName  pos                               can
	where
		drawTower :: Int -> Int -> Tower -> Canvas -> IO ()
		drawTower nr i (f:r) can = do
			drawDisk (Rect (x-w) y (x+w) (y+10)) can
			drawTower nr (i+1) r can
			where
				x	= nr*xOffs
				w	= f *5
				y	= 20+10*i
		drawTower _ _ [] can = return ()

		drawName :: Int -> Canvas -> IO ()
		drawName nr can
			| nr==1	= drawString (Point (xOffs-14)  y) "from" can
			| nr==2	= drawString (Point (2*xOffs-6) y) "to"   can
			| nr==3	= drawString (Point (3*xOffs-9) y) "via"  can
			where
				y = 35+10*maxDisks

drawDisk :: Rect -> Canvas -> IO ()
drawDisk rect can = do
	setCanvasPen can [color =: skyblue]
	fillRect rect can
	setCanvasPen can [color =: black]
	drawRect rect can
	
eraseDisk :: Rect -> Canvas -> IO ()
eraseDisk (Rect l t r b) can = do
	setCanvasPen can [color =: white]
	fillRect (Rect l t r (b-1)) can
