module Main where

import Graphics.UI.GIO

main = start "hello world" "1.0" SDI [] $ do
	w <- window [view =: sz 200 200]
	spl <- hSplitter [] w
	btnOk     <- button [title =: "Ok"] spl
	btnCancel <- button [title =: "Cancel"] spl
	set spl [layout =: btnOk ^^^ btnCancel]
	set w [layout =: hfill (vfill spl)]
	showWindow w
