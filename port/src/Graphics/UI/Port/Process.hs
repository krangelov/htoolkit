-----------------------------------------------------------------------------------------
{-| Module      :  Process
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Process
    ( start, quit, halt
    ) where

import Graphics.UI.Port.Types
import Graphics.UI.Port.Window
import Graphics.UI.Port.Timer
import Graphics.UI.Port.Handlers
import Foreign
import Foreign.C
import System.Mem( performGC )
import Control.Monad(when)

-- | Start the event loop. The event loop will continue until the 
-- 'quit' function is called.
start :: String             -- ^ Application title. Under Windows and Gnome this is
                            -- the title of the main window.
      -> String             -- ^ Application version. The version string usually
                            -- consists two substrings, separated with space. The first 
                            -- substring is the internal application name while the second
                            -- is the version. If the first string is skipped then the
                            -- internal name will be equal to the application title.
                            -- The internal name can contain only letters
                            -- and digits all other characters will be ignored.
      -> DocumentInterface  -- ^ Document interface type for application
      -> IO ()              -- ^ Startup action
      -> IO ()
start name version di io =
  withCString name    $ \cname    ->
  withCString version $ \cversion -> do
    setProcessDismissHandler (quit >> return ())
    fptr_io <- wrapIO io
    osStart cname cversion (toCDocumentInterface di) fptr_io

-- | The 'quit' function sends @WindowDismiss@ event to all opened windows and if after that all windows are closed
-- then it will close the entire process. This function is the default handler for @ProcessDismiss@ event.
quit :: IO Bool
quit = do
    r <- dismissAllWindows
    when r $ do
        destroyAllTimers
        osQuit
        performGC                -- to release any foreign objects
    return r

-- | 'halt' exits the main event loop, closes any windows and menus, destroys all timers
-- and unregisters any event handlers.
halt :: IO ()
halt = do
    destroyAllWindows
    destroyAllTimers
    osQuit
    performGC                -- to release any foreign objects

foreign import ccall "wrapper"
  wrapIO :: IO () -> IO (FunPtr (IO ()))

foreign import ccall "osStart" osStart :: CString -> CString -> CInt -> FunPtr (IO ()) -> IO ()
foreign import ccall osQuit :: IO ()
