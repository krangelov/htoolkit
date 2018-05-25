{--------------------------------------------------------------------------------
This program implements the "goodbye" demo as posted by John Meacham on
the Haskell GUI mailing list. The program is specified as:

I propose a simple program which pops up a window saying 'Hello World' 
with a button saying 'Bye' which you click and it changes the message 
to 'Goodbye'. if you click the button again the program exits.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.Port

main = start "Bye" "1.0" SDI demo
 
demo :: IO ()
demo 
  = do w <- createWindow
       setWindowDomainSize w (sz 0 0)                 -- no scroll bars needed
       setWindowViewSize w (sz 80 40)                 -- guess some size as Port has no layout manager
       
       l <- createLabel w
       setLabelText l "Hello World!"
       lsize <- getLabelRequestSize l                 -- get the minimal size of the label
       moveResizeControl l (rectAt (pt 0 0) lsize)    -- position in upperleft corner
       setControlVisible l True

       b <- createButton w
       setButtonText b "Bye"
       bsize <- getButtonRequestSize b                -- get the minimal size of the button
       moveResizeControl b (rectAt (pt 0 (sh lsize)) bsize)  -- position under the label
       setControlVisible b True

       setControlCommandHandler b (bye w l b)         -- register event handler
       setWindowVisible w True                        -- show it all
  where
    -- called on the first click with the window, label, and button as arguments.
    bye w l b
      = do setLabelText l "Goodbye"
           setControlCommandHandler b halt            -- overwrite the old event handler
