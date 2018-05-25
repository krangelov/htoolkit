module Main where

import Graphics.UI.GIO

main = start "DynamicLayout" "1.0" SDI [] demo     -- "start" initializes the GUI.

demo :: IO ()
demo = do
          w <- window [pen =: dialogPen, autosize =: False]
          e <- entry [] w
          b <- button [title =: "Ok"] w
	  c <- checkBox [title =: "Simple", on command =:: showHide w (fill e <<< b), checked =: True] w
          showWindow w
          where
                    showHide w lay c = do
                              chk <- get c checked
                              set w [layout =: pad 10 (if chk then pack c else c ^^^^ lay)]