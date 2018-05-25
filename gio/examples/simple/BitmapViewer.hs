module Main where

import Graphics.UI.GIO

codecsToFilesFilter :: [Codec] -> [(String,[String])]
codecsToFilesFilter = map codeToFileFilter
  where
    codeToFileFilter :: Codec -> (String,[String])
    codeToFileFilter codec = (codecDescr codec,map (\e->"*."++e) (codecExtensions codec)) 
    
main
  = start "Bitmap Viewer" "1.0" MDI [] $
    do icoFileOpen <- readBitmap "../images/open-file-icon.png" []
       
       aclose <- action [title =: "&Close", enabled =: False]
       aopen  <- action [title =: "&Open",  icon =: Just icoFileOpen, on command =: cmdopen aclose]

       mfile  <- menu [title =: "&File"] mainMenu
       mopen  <- menuActionItem [] aopen  mfile
       mclose <- menuActionItem [] aclose mfile
       menuline mfile
       mexit  <- menuitem [title =: "&Exit", on command =: quit >> return ()] mfile

       mainBar <- toolBar "Main" PosTop 0 0 0 []
       toolActionButton [] aopen mainBar
       return ()
  where
    cmdopen aclose
      = do codes <- getAvailableCodecs
           fnames <- runInputFilesDialog "Open" (codecsToFilesFilter codes) Nothing
           mapM_ (openBitmapWindow aclose) fnames

    cmdclose w aclose
      = do set aclose [enabled =: False, off command]
           dismissWidget w
           return ()

    paintImage image can updFrame updAreas
      = do drawBitmap (pt 0 0) image can

    openBitmapWindow aclose fname = do
      bitmap <- readBitmap fname []
      bmpSize <- get bitmap size
      w <- window [ title  =: fname
                  , domain =: bmpSize
                  , view =: sz 300 300
                  , on paint =: paintImage bitmap
                  ]
      set aclose [enabled =: True, on command =: cmdclose w aclose]
      set w [ on activate   =: set aclose [enabled =: True, on command =: cmdclose w aclose]
            , on deactivate =: set aclose [enabled =: False, off command]
	        ]
      showWindow w
