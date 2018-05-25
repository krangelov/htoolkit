module Main (main) where

import Graphics.UI.GIO

main = start "Counter" "1.0" SDI [] $ do
	w <- window []
	ent <- entry [readOnly =: True, tooltip =: "Counter Value", title =: "0"] w
	bplus  <- button [title =: "-", tooltip =: "Decrement Value", on command =: upd (\x->x-1) ent] w
	bminus <- button [title =: "+", tooltip =: "Increment Value", on command =: upd (\x->x+1) ent] w
	set w [layout =: hfill ent ^^^ hcenter (pad 5 (fill bplus <<< fill bminus))]
	showWindow w
	where
		upd f ent = set ent [title ~: show . f . read]
