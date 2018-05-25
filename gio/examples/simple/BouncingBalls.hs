module Main where

import Graphics.UI.GIO

main
  = start "Bouncing balls" "1.0" SDI [] balls
  
balls
  = do vballs <- newVar []
       
       w <- window [resizeable =: True, view =: sz maxX maxY, bufferMode =: Buffered]
       set w [ on paint =: paintBalls vballs
             , on click =: dropBall w vballs
             ]

       t <- timer [interval =: 20, on command =: nextBalls w vballs]

       set w [ on (charKey '-') =: set t [interval ~: \i -> i*2]
             , on (charKey '+') =: set t [interval ~: \i -> max 1 (i `div` 2)]
             , on (charKey 'p') =: set t [enabled ~: not]
             ]

       showWindow w
  where
    nextBalls w vballs
      = do updateVar vballs (filter (not.null) . map (drop 1))
           repaint w

    dropBall w vballs pt
      = do updateVar vballs (bouncing pt:)
           repaint w

    bouncing (Point x y)
      = map (\h -> Point x (maxH-h)) (bounce (maxH-y) 0)

    bounce h v
      | h <= 0 && v == 0     = []
      | h <= 0 && v  < 0     = bounce 0 ((-v)-2)
      | otherwise            = h : bounce (h+v) (v-1)
    
    
    paintBalls vballs can updframe updareas
      = do setCanvasPen can [color =: white]
           fillRect updframe can
           balls <- getVar vballs
           mapM_ (drawBall can) (map head (filter (not.null) balls))

    drawBall can pt
      = do setCanvasPen can [color =: red]
           fillEllipse pt radius radius can
           setCanvasPen can [color =: black]
           drawEllipse pt radius radius can


-- radius the ball, and the maximal x and y coordinates    
radius, maxX, maxY :: Int
maxY   = 300
maxX   = 300
radius = 10

-- the max. height is at most max. y minus the radius of a ball.
maxH :: Int
maxH   = maxY - radius
