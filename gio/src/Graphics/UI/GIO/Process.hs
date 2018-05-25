-----------------------------------------------------------------------------------------
{-| Module      :  Process
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The process object is an abstraction which provides access to
    attributes which are global for entire application.
-}
-------------------------------------------------------------------------------

module Graphics.UI.GIO.Process
	( Process, pc
	, start, quit, halt
	, ConfigKey(..), configKey, configKeyDef
	) where

import System.Directory
import Control.Exception
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Events
import qualified Graphics.UI.Port as Lib
import Graphics.UI.Port(ConfigKey(..))

data Process = Process

pc :: Process
pc = Process

instance Dismissible Process where
  dismissWidget p = quit
  dismiss         = newEvent (const Lib.getProcessDismissHandler) (const Lib.setProcessDismissHandler) (const Lib.setProcessDismissDefHandler)
  
instance Deadly Process where
  destroyWidget p = halt
  destroy         = newEvent (const Lib.getProcessDestroyHandler) (const Lib.setProcessDestroyHandler) (const Lib.setProcessDestroyDefHandler)

-- | Start the event loop. The event loop will continue until the 
-- process is destroyed.
start :: String            -- ^ Application title. Under Windows and Gnome this is
                           -- the title of the main window.
      -> String            -- ^ Application version. The version string usually
                           -- consists two substrings, separated with space. The first 
                           -- substring is the internal application name while the second
                           -- is the version. If the first string is skipped then the
                           -- internal name will be equal to the application title.
                           -- The internal name can contain only letters
                           -- and digits all other characters will be ignored.
      -> DocumentInterface -- ^ Document interface type for application
      -> [Prop Process]    -- ^ Properties
      -> IO ()             -- ^ Startup action
      -> IO ()
start appName appVersion di props io
  = do curdir <- getCurrentDirectory
       Lib.start appName appVersion di ((set pc props >> io) `catch` \err -> quit >> ioError err)
       setCurrentDirectory curdir

-- | The 'quit' function sends 'dismiss' event to all opened windows and if after that all windows are closed
-- then it will close the entire process. This function is the default handler for process\'s 'dismiss' event.
quit :: IO Bool
quit = Lib.quit

-- | 'halt' exits the main event loop, closes any windows and menus, destroys all timers,
-- unregisters all event handlers and terminates the process.
halt :: IO ()
halt = Lib.halt

configKey :: ConfigKey a => String -> Attr Process a
configKey name = newAttr (\p -> Lib.getConfigKey name) (\p v -> Lib.setConfigKey name v)

configKeyDef :: ConfigKey a => String -> a -> Attr Process a
configKeyDef name defvalue = newAttr (\p -> Lib.getConfigKeyDef name defvalue) (\p v -> Lib.setConfigKey name v)

