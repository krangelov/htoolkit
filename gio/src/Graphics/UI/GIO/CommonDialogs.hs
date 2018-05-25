-----------------------------------------------------------------------------------------
{-| Module      :  CommonDialogs
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Common dialogs.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.CommonDialogs
            ( runDirectoryDialog
            , runInputFileDialog
            , runInputFilesDialog
            , runOutputFileDialog
            , runFontDialog
            , runColorDialog
            , runAboutDialog
            ) where


import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Layout
import Graphics.UI.GIO.Window
import qualified Graphics.UI.Port as Lib

{--------------------------------------------------------------------
  Just simple wrappers
--------------------------------------------------------------------}
-- | Run a dialog to select an input file. Returns 'Nothing' when cancelled.
runInputFileDialog :: String                 -- ^ The dialog title
                   -> [(String,[String])]    -- ^ Filter for acceptable file types. The filter is a
                                             -- list of pairs where the first element of pair is a
                                             -- filter name and the second element is a list of
                                             -- wildcards (example [(\"Haskell script\", [\"*.hs\", \"*.lhs\"])])
                   -> Maybe Window           -- ^ The owner window
                   -> IO (Maybe FilePath)    -- ^ The full qualified path to the selected file
runInputFileDialog title filter mb_owner = Lib.runInputFileDialog title filter (getWHandle mb_owner)

-- | Run a dialog to select one or more input files. Returns empty list when cancelled.
runInputFilesDialog :: String                -- ^ The dialog title
                   -> [(String,[String])]    -- ^ Filter for acceptable file types. The filter is a
                                             -- list of pairs where the first element of pair is a
                                             -- filter name and the second element is a list of
                                             -- wildcards (example [(\"Haskell script\", [\"*.hs\", \"*.lhs\"])])
                   -> Maybe Window           -- ^ The owner window
                   -> IO [FilePath]          -- ^ The list of full qualified paths for the selected files
runInputFilesDialog title filter mb_owner = Lib.runInputFilesDialog title filter (getWHandle mb_owner)

-- | Run a dialog to select an output file. Returns 'Nothing' when cancelled.
runOutputFileDialog :: String                -- ^ The dialog title
                    -> [(String,[String])]   -- ^ Filter for acceptable file types. The filter is a
                                             -- list of pairs where the first element of pair is a
                                             -- filter name and the second element is a list of
                                             -- wildcards (example [(\"Haskell script\", [\"*.hs\", \"*.lhs\"])])
                    -> FilePath              -- ^ The default file name
                    -> Maybe Window           -- ^ The owner window
                    -> IO (Maybe FilePath)   -- ^ The full qualified path to the selected file
runOutputFileDialog title filter fname mb_owner = Lib.runOutputFileDialog title filter fname (getWHandle mb_owner)

-- | Runs a dialog to select a directory. Returns 'Nothing' when cancelled.
runDirectoryDialog :: String                 -- ^ The dialog title
                   -> Maybe Window           -- ^ The owner window
                   -> IO (Maybe FilePath)    -- ^ The full qualified path to the selected directory
runDirectoryDialog title mb_owner = Lib.runDirectoryDialog title (getWHandle mb_owner)
  
-- | Run a dialog to select a font. Returns 'Nothing' when cancelled.
runFontDialog :: Maybe Window           -- ^ The owner window
              -> IO (Maybe FontDef)
runFontDialog mb_owner = Lib.runFontDialog (getWHandle mb_owner)

-- | Run a dialog to select a color. Returns 'Nothing' when cancelled.
runColorDialog :: Maybe Window           -- ^ The owner window
               -> IO (Maybe Color)
runColorDialog mb_owner = Lib.runColorDialog (getWHandle mb_owner)

runAboutDialog :: String       -- ^ application name
               -> String       -- ^ application version
	       -> String       -- ^ copyright
	       -> String       -- ^ comments
	       -> [String]     -- ^ authors
	       -> [String]     -- ^ documenters
	       -> String       -- ^ translator credits
	       -> Bitmap       -- ^ logo
	       -> Maybe Window -- ^ The owner window
	       -> IO ()
runAboutDialog appName appVersion copyright comments authors documenters tcredits logo mb_owner =
  Lib.runAboutDialog appName appVersion copyright comments authors documenters tcredits logo (getWHandle mb_owner)

getWHandle = maybe Lib.nullHandle hwindow
