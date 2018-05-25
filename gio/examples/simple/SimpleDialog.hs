module Main where

import Graphics.UI.GIO

main
  = start "Simple dialogs" "SimpleDialogs 1.0" SDI [] $ do
      fm  <- menu [title =: "&Open"] mainMenu
      menuitem [title =: "Modal Dialog",    on command =: showDialog True  Nothing] fm
      menuitem [title =: "Modeless Dialog", on command =: showDialog False Nothing]  fm
  where
     showDialog b parent = do
     	d <- dialog [title =: "Dialog", frame =: Rect 100 100 500 500] parent
     	grp <- groupBox [title =: "Open dialog"] d
     	b1  <- button [title =: "Modal",    on command =: showDialog True  (Just d)] grp
     	b2  <- button [title =: "Modeless", on command =: showDialog False (Just d)] grp
     	set grp [layout =: pad 10 (hstretch b1 <<<< hstretch b2)]
     	chk <- checkBox [title =: "Resizeable", checked =: True, on command =: setResizeableOnOff d] d
     	bclr <- button [title =: "Set Background", on command =: setColor d] d
        set d [layout =: pad 10 (center (hstretch grp ^^^^ (chk <<<< bclr)))]
        (if b then runDialog else showWindow) d
        where
			setResizeableOnOff d = do
				b <- get d resizeable
				set d [resizeable =: not b]
			setColor d = do
				mb_color <- runColorDialog (Just d)
				case mb_color of
					Just c  -> set d [bgcolor =: c]
					Nothing -> return ()
