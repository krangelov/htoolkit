module Main where

import Graphics.UI.GIO

main = start "Able" "1.0" MDI [] demo

demo = do
	w1 <- window [title  =: "Slave",  view =: sz 200 100, domain =: sz 200 80]
	ent <- entry  [title =: "Test!"] w1
	btn <- button [title =: "Button"] w1
	set w1 [layout =: (hfix 80 ent <<< btn)]
	w2 <- window [title  =: "Master", view =: sz 200 80,  domain =: sz 200 80]
	bctrl <- button [] w2
	bwnd  <- button [] w2
	set w2 [layout =: (hfill bctrl ^^^ hfill bwnd)]
	enable "Control" bctrl ent
	enable "Window"  bwnd  w1
	showWindow w1
	showWindow w2
	return ()
	where
		enable name b w = do
			set w [enabled =: True]
			set b [title =: "Disable " ++ name, on command =: disable name b w]

		disable name b w = do
			set w [enabled =: False]
			set b [title =: "Enable  " ++ name, on command =: enable  name b w]
