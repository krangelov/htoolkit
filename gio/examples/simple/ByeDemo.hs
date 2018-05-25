{--------------------------------------------------------------------------------
This program implements the "goodbye" demo as posted by John Meacham on
the Haskell GUI mailing list. The program is specified as:

I propose a simple program which pops up a window saying 'Hello World' 
with a button saying 'Bye' which you click and it changes the message 
to 'Goodbye'. if you click the button again the program exits.

Note that this demo also uses a nice layout: the label and button are centered
in the window with some padding around it. When the button is clicked the
first time, it calls "bye". This function changes the text of the label
and installs another event handler on the button that closes the main window.
(by default, GIO exits the gui when all windows are closed).
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.GIO

main = start "Bye!" "1.0" SDI [] demo     -- "start" initializes the GUI.
 
demo :: IO ()
demo = do w <- window []
          l <- label  [title =: "Hello World"] w
          b <- button [title =: "Bye"] w
          set w [layout =: pad 10 (center l ^^^^ center b)] 
          set b [on command =: bye l b]
	  showWindow w             
     where
       -- called on the first click, with the window, label, and button as arguments.
       bye l b
         = do set l [title =: "Goodbye"]      
              set b [on command =: halt]
