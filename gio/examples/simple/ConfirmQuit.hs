module Main where

import Graphics.UI.GIO
import Control.Monad(when)

main
  = start "ConfirmQuit" "1.0" SDI [] $
    do w <- window [view =: sz 200 200]
       set w [on dismiss =: confirmQuit w]
       showWindow w
  where
    confirmQuit w
      = do yes <- messageQuestion "Invoked dismiss!\nDo you really want to quit?"
           when (yes) (destroyWidget w)
