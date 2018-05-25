module Main where

import Data.Char( isDigit, digitToInt )
import Graphics.UI.GIO


main
  = start "Calculator" "1.0" SDI [] calculator

calculator
  = do varst   <- newVar (0,id)
       w       <- window [resizeable =: True]
       display <- label [title =: "0"] w
       keys    <- mapM (\c -> button [title =: [c]
                                     ,on command =: cmd varst display c] w)  
                       "123+456-789*C0=/"
       set w [layout =: (pad 10 (hglue <<< display)) ^^^ grid (matrix 4 (map (hfix 50) keys))]
       showWindow w
  where
    matrix n []   = []
    matrix n xs   = take n xs : matrix n (drop n xs)

    cmd varst display c
      = do st <- getVar varst
           let st' = calc st c
           set display [title =: show (fst st')]
           setVar varst st'

    calc (d,accu) c
      = case c of
          'C' -> (0,id)
          '=' -> (accu d,const (accu d))
          '+' -> (0, (d+))
          '-' -> (0, (d-))
          '*' -> (0, (d*))
          '/' -> (0, (\x -> if x==0 then 0 else div d x))
          _   | isDigit c  -> (10*d + digitToInt c,accu)
              | otherwise  -> (0,id)          
