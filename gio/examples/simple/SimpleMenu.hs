module Main where

import Graphics.UI.GIO

main
  = start "Simple menu" "SimpleMenu 1.0" SDI [] $
    do w <- window [view =: sz 200 200]
       showWindow w

       bmpNew  <- readBitmap "res/new.bmp"  []
       bmpOpen <- readBitmap "res/open.bmp" []
       bmpSave <- readBitmap "res/save.bmp" []
       
       set sb [visible =: True]
       indicator [title =: "Click Me", on command =: messageAlert "CLICK!"] sb
       icolor <- indicator [title =: "NONE"] sb
       itrans <- indicator [title =: "NORM"] sb

       fm  <- menu [title =: "&File"] mainMenu
       menuitem [ title =: "&New"
                , tooltip =: "Create a new document"
                , accel =: KeyChar '\^N'
                , on command =: messageAlert "NEW"
                , icon =: Just bmpNew
                ] fm
       menuitem [ title =: "&Open"
                , tooltip =: "Open an existing document"
                , accel =: KeyChar '\^O'
                , on command =: messageAlert "OPEN"
                , icon =: Just bmpOpen
                ] fm
       menuitem [ title =: "&Close"
                , tooltip =: "Close the active document"
                ] fm       
       menuline fm
       menuitem [ title =: "&Save"
                , tooltip =: "Save the active document"
                , accel =: KeyChar '\^S'
                , on command =: messageAlert "SAVE"
                , icon =: Just bmpSave
                ] fm
       menuline fm
       menuitem [ title =: "&Exit"
                , tooltip =: "Quit the application"
                , on command =: halt
                ] fm

       mColor <- menu [title =: "&Color"] mainMenu
       menucheck [ title =: "&Transparent"
                 , tooltip =: "Set transparency on/off"
                 , accel =: KeyChar '\^T'
                 , on command =:: onToggleTransparent itrans
                 ] mColor
       menuline mColor
       mRed   <- menuRadioItem [ title =: "&Red"
                               , tooltip =: "Select red color"
                               , on command =: set icolor [title =: "RED"]
                               ] mColor
       mGreen <- menuRadioItem [ title =: "&Green"
                               , tooltip =: "Select green color"
                               , on command =: set icolor [title =: "GREEN"]
                               ] mColor
       mBlue  <- menuRadioItem [ title =: "&Blue"
                               , tooltip =: "Select blue color"
                               , on command =: set icolor [title =: "BLUE"]
                               ] mColor
       setMenuRadioGroup [mRed, mGreen, mBlue]
    where
       onToggleTransparent itrans tgl = do
          chk <- get tgl checked
          set itrans [title =: (if chk then "TRANSPARENT" else "NORM")]

