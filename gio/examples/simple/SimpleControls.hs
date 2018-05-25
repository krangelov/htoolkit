module Main where

import Graphics.UI.GIO

main = start "Controls" "1.0" SDI [] demo

demo :: IO ()
demo = do w <- window []
          nb <- notebook [] w

          -- Buttons page
          pg1 <- notebookPage [title =: "Buttons"] nb
          group <- groupBox [title =: "Buttons"] pg1
          b1pg1 <- button [title =: "Button1", on command =: messageAlert "Button1 clicked"] group
          b2pg1 <- button [title =: "Button2", on command =: messageAlert "Button2 clicked"] group
          set group [layout =: b1pg1 <<<< b2pg1]
          set pg1 [layout =: group]

          -- Edit page
          pg2 <- notebookPage [title =: "Edit"] nb
          lbl  <- label [title =: "Comments"] pg2
          edit <- entry [] pg2
          chk  <- checkBox [title =: "Read only", on command =: set edit [readOnly ~: not]] pg2
          set pg2 [layout =: (lbl <<<< (hglue <<< chk)) ^^^ hfill (vfill (edit))]

          -- radio buttons
          group   <- groupBox [title =: "Tabs"] w
          rtop    <- radioBox [title =: "top",    on command =: set nb [labelsPosition =: PosTop   ]] group
          rleft   <- radioBox [title =: "left",   on command =: set nb [labelsPosition =: PosLeft  ]] group
          rbottom <- radioBox [title =: "bottom", on command =: set nb [labelsPosition =: PosBottom]] group
          rright  <- radioBox [title =: "right",  on command =: set nb [labelsPosition =: PosRight ]] group
          setRadioBoxGroup [rtop,rleft,rbottom,rright]
          set group [layout =: vertical [rtop,rleft,rbottom,rright]]

          e <- entry [readOnly =: True, title =: "Hello, world!"] w
          set w [layout =: (group <<< vfill (hfill nb)) ^^^ hfill e]
          showWindow w
