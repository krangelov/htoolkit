module Main where

import Graphics.UI.GIO
import Life
import Data.Char(ord,chr)
import Data.IORef

data Life = Life
   	{ gen       :: Generation
	, cellSize :: CellSize
	} deriving Show

initialLife = Life
	{ gen       = makeGeneration
	, cellSize = startCellSize
	}

main :: IO ()
main = start "Life" "1.0" SDI [] $ do
		ref <- newIORef initialLife

		let mydomain@(Size dw dh) = getViewDomain startCellSize
		    myorigin = Point (dw `div` 2) (dh `div` 2)
		w <- window [domain =: mydomain, origin =: myorigin, bgcolor =: black]
		set w [on mouse =: onMouse w ref, on paint =: onPaint ref, origin =: myorigin]
		showWindow w

		mfile <- menu  [title =: "File"] mainMenu
		menuitem [title =: "About LifeGameExample...", on command =: onAbout] mfile
		menuline mfile
		menuitem [title =: "Exit", on command =: halt] mfile

		mopts  <- menu [title =: "Options"] mainMenu
		msizes <- menu [title =: "Cell Size"] mopts
		items  <- sequence [menuRadioItem [ title =: mkTitle size
		                                  , on command =: onNewSize size w ref] msizes
				| size <- [1..8]]
		setMenuRadioGroup items
		set (items !! 6) [checked =: True]

		mcmd <- menu [title =: "Commands"] mainMenu
		merase <- menuitem [title =: "Erase Cells", accel =: KeyChar '\^E'] mcmd
		mplay  <- menuitem [title =: "Play",           accel =: KeyChar '\^P'] mcmd
		mhalt  <- menuitem [title =: "Halt",            accel =: KeyChar '\^H', enabled =: False] mcmd
		mstep  <- menuitem [title =: "Step",           accel =: KeyChar '\^S'] mcmd

		t <- timer [enabled =: False, interval =: 40, on command =: onTimer w ref]

		set merase [on command =: onErase w ref]
		set mplay [on command =: onPlay mplay mstep  mhalt t]
		set mhalt [on command =: onHalt mplay mstep  mhalt t]
		set mstep [on command =: onTimer w ref]
		where
			mkTitle size	= show size ++ " * " ++ show size

			onTimer w ref = do
				life <- readIORef ref
				let size = cellSize life
				let (next,died) = lifeGame (gen life)
				let render can = drawCells (drawCell size) next can >> drawCells (eraseCell size) died can
				writeIORef ref (life{gen=next})
				paintIn w UnBuffered render

			onAbout = do
				logo <- readBitmap "../images/logo.bmp" []
				runAboutDialog "Life" "1.0"
				                       "(C) Krasimir Angelov, 2003"
				                       "The Life is an example program\nfreely distributed with HToolkit"
				                       [] [] [] logo Nothing

			onMouse w ref (MouseLeftDown pos mods)
				| mods == justControl = do
					life <- readIORef ref
					let cell = makeLifeCell pos (cellSize life)
					writeIORef ref (life{gen=removeCell cell (gen life)})
					paintIn w UnBuffered (eraseCell (cellSize life) cell)
				| mods == noModifiers = do
					life <- readIORef ref
					let cell = makeLifeCell pos (cellSize life)
					writeIORef ref (life{gen=insertCell cell (gen life)})
					paintIn w UnBuffered (drawCell (cellSize life) cell)
			onMouse w ref (MouseDrag pos mods) = do
					life <- readIORef ref
					let cell = makeLifeCell pos (cellSize life)
					writeIORef ref (life{gen=insertCell cell (gen life)})
					paintIn w UnBuffered (drawCell (cellSize life) cell)
			onMouse w ref _ = return ()

			onPaint ref can _ _ = do
				life <- readIORef ref
				drawCells (drawCell (cellSize life)) (gen life) can

			-- onErase sets the current generation to empty and clears the window.
			onErase :: Window -> IORef Life -> IO ()
			onErase w ref = do
				life <- readIORef ref
				writeIORef ref (life{gen=makeGeneration})
				repaint w

			-- play starts the computation of successive generations given the current set of life cells.
			onPlay :: MenuItem -> MenuItem -> MenuItem -> Timer -> IO ()
			onPlay mplay mstep  mhalt timer = do
				set mplay   [enabled =: False]
				set mstep   [enabled =: False]
				set mhalt   [enabled =: True ]
				set timer   [enabled =: True ]

			-- onHalt stops the computation of successive generations, but does not change the current generation.
			onHalt :: MenuItem -> MenuItem -> MenuItem -> Timer -> IO ()
			onHalt mplay mstep  mhalt timer = do
				set mplay   [enabled =: True ]
				set mstep   [enabled =: True ]
				set mhalt   [enabled =: False]
				set timer   [enabled =: False]

			-- onNewSize changes the size in which life cells are rendered and redraws the window.
			onNewSize :: Int -> Window -> IORef Life -> IO ()
			onNewSize newSize w ref = do
				life <- readIORef ref
				oldOrigin <- get w origin
				let oldSize = cellSize life
				    newOrigin = Point ((px oldOrigin) `div` oldSize*newSize) ((py oldOrigin) `div` oldSize*newSize)
				set w [domain =: getViewDomain newSize, origin =: newOrigin]
				writeIORef ref (life{cellSize=newSize})

	-- Given the size in which to render life cells, getViewDomain calculates the corresponding ViewDomain:
			getViewDomain :: CellSize -> Size
			getViewDomain size =
				let Size w h = universe
				in Size (w*size) (h*size)

--	Program constants.

universe	= Size 2000 2000
startCellSize	= 8
