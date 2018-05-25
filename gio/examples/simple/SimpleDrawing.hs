module Main where

import Graphics.UI.GIO

main
  = start "SimpleDrawing" "1.0" SDI [] $
    do sserif14b <- createFont (sansSerifFontDef{fontSize = 14, fontWeight = fontBoldWeight})
       w <- window [title =: "Hello world", width =: 600, height =: 600
                   ,on paint =: mypaint sserif14b
                   ]
       set w [on motion =: \p -> set w [title =: ("mouse is at " ++ show p)]]
       showWindow w
  where
    mypaint sserif14b c updFrame updArea
      = do setCanvasPen c [thickness =: 10, color =: blue]
           drawRect (rect (pt 50 50) (pt 200 200)) c
           
           setCanvasPen c [color =: red]
           drawPolyline [pt 150 250,pt 50 300,pt 200 300] c
           
           setCanvasPen c [color=:green, thickness =: 5]
           drawEllipse (pt 100 375) 50 25 c
           
           setCanvasPen c [color=:red]
           drawArc  (pt 100 450) 50 25 0 (0.5*pi) c
           fillRect (rect (pt 250 50)  (pt 450 200)) c
           drawPolygon   [pt 350 250,pt 250 300,pt 400 300] c
           
           setCanvasPen c [color=:green]
           fillEllipse (pt 300 375) 50 25 c
           
           setCanvasPen c [color=:red]
           fillPie (pt 300 450) 50 25 0 (0.5*pi) c

	   setCanvasPen c [thickness =: 15, color =: magenta]
           drawLine (pt 30 30) (pt 200 150) c
           
           setCanvasPen c [color =: yellow]
           fillCircle (pt 70 70) 10 c
           
           setCanvasPen c [color =: black]
           drawCircle (pt 70 70) 10 c

           setCanvasPen c [color =: blue]
           drawString (pt 100 100) "Some text" c

           setCanvasPen c [color =: black, font =: sserif14b]
           drawString (pt 120 120) "14pt bold sans serif text" c
