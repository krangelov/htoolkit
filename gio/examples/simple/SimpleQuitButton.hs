module Main where

import Graphics.UI.GIO

main = start "Quit demo" "1.0" SDI [] demo
 
demo :: IO ()
demo = do w <- window []
          q <- button [title =: "Quit", on command =: halt] w
          set w [layout =: pad 10 (center q)]
          showWindow w
