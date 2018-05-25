module Main where

import Graphics.UI.GIO

main = start "Progress" "1.0" SDI [] demo

maxSpeed    = 200 :: Int
maxProgress = 200 :: Int

demo = do
    w  <- window [view =: sz 800 100, domain =: sz 800 80]
    tm <- timer [interval =: maxSpeed `div` 2]
    prg <- hProgressBar True [range =: (0,maxProgress)] w
    lbl <- label [title =: "Speed"] w
    sld <- hslider [range =: (0,maxSpeed), selectedPos =: maxSpeed `div` 2] w
    set w [layout =: (hfill prg ^^^ (lbl <<< hfill sld))]
    set tm [on command =: set prg [selectedPos ~: next]]
    set sld [on command =: changeSpeed tm sld]
    set w [on destroy =: destroyWidget tm]
    showWindow w
    return ()
    where
        next x | x >= maxProgress  = 0
               | otherwise         = x+1

        changeSpeed tm sld = do
            pos <- get sld selectedPos
            set tm [interval =: maxSpeed-pos+20]
