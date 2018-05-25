{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Graphics.UI.GIO

main = 
  start "Conf" "1.0" SDI [] $ do
    (value :: Int) <- get pc (configKey "Counter.Value")
    (delta :: Int) <- get pc (configKeyDef "Counter.Delta" 1)
    set pc [configKey "Counter.Value" =: value+delta]
    prompt <- get pc (configKeyDef "Prompt" "Counter.Value=")
    w <- window []
    l <- label  [title =: prompt ++ show value] w
    set w [layout =: l]
    showWindow w
