module WormShow
        ( drawGame
        , drawStep
        , drawAnimation
        , eraseSegment
        , wormBackGroundColour
        , nrAnimationSteps
        ) where

import	Graphics.UI.GIO
import	WormState
import  Control.Monad(when)

--	The drawing constants.
wormBackGroundColour = lightyellow
wormFontSize         = 12 :: Int
pointsPos            = Point 72  15
lifesPos             = Point 255  5
levelPos             = Point 465 15
cornerX              = 15 :: Int
cornerY              = 23 :: Int
segSize              = 4  :: Int
cellSize             = 10 :: Int
nrAnimationSteps     = 40 :: Int

--	Draw the game.
drawGame :: State -> Canvas -> IO ()
drawGame (State {gamelevel=Level {level=level,obstacles=obstacles}
                ,food=food
                ,points=points
                ,worm=worm
                ,lives=lives
                }) can = do
    drawBorders             can
    drawObstacles obstacles can
    drawPoints    points    can
    drawWorm      worm      can
    drawFood      food      can
    drawLevel     level     can
    drawLives     lives     can
    where
        drawObstacles :: [Obstacle] -> Canvas -> IO ()
        drawObstacles [] can = return ()
        drawObstacles obstacles can = do
            setCanvasPen can [color =: purple]
            mapM_ drawObstacle obstacles
            setCanvasPen can [color =: black]
            where
                drawObstacle :: Obstacle -> IO ()
                drawObstacle (Rect ltx lty rbx rby) =
                    fillRect (Rect lx ty rx by) can
                    where
                        lx = cornerX+cellSize*ltx-2
                        ty = cornerY+cellSize*lty-2
                        rx = cornerX+cellSize*rbx+2
                        by = cornerY+cellSize*rby+2

        drawPoints :: Points -> Canvas -> IO ()
        drawPoints points can = do
            setCanvasPen can [color =: magenta]
            drawString pointsPos{px=(px pointsPos) - 57} "Points: " can
            setCanvasPen can [color =: black]
            drawString pointsPos (show points) can

        drawWorm :: Worm -> Canvas -> IO ()
        drawWorm [] can = return ()
        drawWorm (head:rest) can = do
            mapM_ (\s -> drawSegment red s can) rest
            drawSegment lightgreen head can
            setCanvasPen can [color =: black]

        drawLevel :: Int -> Canvas -> IO ()
        drawLevel level can = do
            setCanvasPen can [color =: magenta]
            drawString levelPos{px=px levelPos-50} "Level: " can
            setCanvasPen can [color =: black]
            drawString levelPos (show level) can

        drawLives :: Lives -> Canvas -> IO ()
        drawLives lives can
            | lives /= 0 = drawLittleWorms lives can
            | otherwise  = do
            setCanvasPen can [color =: magenta]
            drawString (Point (lx-63) (ly+10)) "No more worms!" can
            setCanvasPen can [color =: black]
            where
                Point lx ly = lifesPos

                drawLittleWorms :: Lives -> Canvas -> IO ()
                drawLittleWorms lives can
                    | lives>0 = do
                        drawLittleWorm   lives can
                        drawLittleWorms (lives-1) can
                    | otherwise = do
                        setCanvasPen can [color =: magenta]
                        drawString (Point (lx-63) (ly+10)) "Worms:" can
                        setCanvasPen can [color =: black]
                    where
                        drawLittleWorm :: Int -> Canvas -> IO ()
                        drawLittleWorm n can = do
                            setCanvasPen can [thickness=:5, color =: red]
                            drawLine (Point x y) (Point (x+9) y) can
                            setCanvasPen can [color =: lightgreen]
                            drawLine (Point (x+9) y) (Point (x+14) y) can
                            setCanvasPen can [thickness=:1, color =: black]
                            where
                                x = lx+20*((n-1) `div` 2)
                                y = ly+ 7*((n-1) `mod` 2)

drawBorders :: Canvas -> IO ()
drawBorders can = do
    setCanvasPen can [color=:black, thickness=:3]
    drawRect (Rect (cornerX-3) (cornerY-3)
                   (cornerX+sizeX*cellSize+11) (cornerY+sizeY*cellSize+11)) can
    setCanvasPen can [thickness=:1]

drawSegment :: Color -> Segment -> Canvas -> IO ()
drawSegment c (Point x y) can = do
    setCanvasPen can [color=:c]
    fillCircle (Point (cornerX+cellSize*x) (cornerY+cellSize*y)) segSize can

eraseSegment :: Segment -> Canvas -> IO ()
eraseSegment segment = drawSegment wormBackGroundColour segment

drawFood :: Food -> Canvas -> IO ()
drawFood (Food _ (Point x y)) can = do
    setCanvasPen can [color=:magenta]
    fillRect (Rect x1 y1 (x1+6) (y1+6)) can
    setCanvasPen can [color=:black]
    where
        x1 = cornerX+cellSize*x-3
        y1 = cornerY+cellSize*y-3

eraseFood :: Food -> Canvas -> IO ()
eraseFood (Food _ (Point x y)) can = do
    setCanvasPen can [color=:yellow]
    fillRect (Rect x1 y1 (x1+6) (y1+6)) can
    setCanvasPen can [color=:black]
    where
        x1 = cornerX+cellSize*x-3
        y1 = cornerY+cellSize*y-3


--	Show a step of the worm.
drawStep :: State -> State -> Canvas -> IO ()
drawStep (State{food=oldfood,worm=oldworm}) (State{food=newfood
                                                  ,worm=newworm
                                                  ,points=points}) can = do
    eraseFood  oldfood can
    drawFood   newfood can
    drawString pointsPos (show points) can
    drawSegment  red (head oldworm) can
    drawSegment  lightgreen (head newworm) can
    when (length oldworm == length newworm) 
            (drawSegment wormBackGroundColour (last oldworm) can)
    setCanvasPen can [color=:black]


--	Close the Playfield between two levels.
drawAnimation :: Int -> Int -> Canvas -> IO ()
drawAnimation 40 1 can = drawBorders  can
drawAnimation n step can
    | step<0 = do
        setCanvasPen can [color=:wormBackGroundColour]
        fillRect (Rect (l-1) b     x y) can
        fillRect (Rect r     (t-1) x y) can
        setCanvasPen can [color=:black, thickness=:3]
        drawRect (Rect l t r b) can
    | otherwise = do
        setCanvasPen can [color=:wormBackGroundColour]
        fillRect (Rect (l-1) y     r b) can
        fillRect (Rect x     (t-1) r b) can
        setCanvasPen can [color=:black, thickness=:3]
        drawRect (Rect l t r b) can
    where
        l = cornerX-3
        t = cornerY-3
        r = l+w*n
        b = t+h*n
        x = r-step*(w+1)
        y = b-step*(h+1)
        w = (48+sizeX*cellSize) `div` nrAnimationSteps
        h = (48+sizeY*cellSize) `div` nrAnimationSteps
