-----------------------------------------------------------------------------------------
{-| Module      :  CommonDialogs
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Standard file selection dialogs.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.CommonDialogs
           ( runDirectoryDialog
           , runInputFileDialog
           , runInputFilesDialog
           , runOutputFileDialog
           , runColorDialog
           , runFontDialog
           , runAboutDialog
           ) where

import Foreign
import Foreign.C
import Graphics.UI.Port.Types

-----------------------------------------------------------------------------------------
-- File Dialogs  
-----------------------------------------------------------------------------------------

-- | Run a dialog to select an input file. Returns 'Nothing' when cancelled.
runInputFileDialog :: String -> [(String,[String])] -> WindowHandle -> IO (Maybe FilePath)
runInputFileDialog title filter owner
  = withCString title $ \ctitle ->
    withCFilter filter $ \cfilter ->
    do cin <- osSelectInputFile ctitle cfilter owner
       maybeCString cin
foreign import ccall osSelectInputFile :: CString -> Ptr CChar -> WindowHandle -> IO CString

-- | Run a dialog to select multiple input files. Returns empty list when canceled.
runInputFilesDialog :: String -> [(String,[String])] -> WindowHandle -> IO [FilePath]
runInputFilesDialog title filter owner
  = withCString title $ \ctitle ->
    withCFilter filter $ \cfilter ->
    do cin <- osSelectInputFiles ctitle cfilter owner
       peekCStrings cin
foreign import ccall osSelectInputFiles :: CString -> Ptr CChar -> WindowHandle -> IO (Ptr CChar)

-- | Run a dialog to select an output file. Takes both a dialog title and a 
-- suggested filename as arguments. Returns 'Nothing' when cancelled.
runOutputFileDialog :: String -> [(String,[String])] -> FilePath -> WindowHandle -> IO (Maybe FilePath)
runOutputFileDialog title filter fname owner
  = withCString title $ \ctitle ->
    withCString fname $ \cname ->
    withCFilter filter $ \cfilter ->
    do cout <- osSelectOutputFile ctitle cfilter cname owner
       maybeCString cout
foreign import ccall osSelectOutputFile :: CString -> Ptr CChar -> CString -> WindowHandle -> IO CString

-- | Runs a dialog to select a directory. Returns 'Nothing' when cancelled.
runDirectoryDialog :: String -> WindowHandle -> IO (Maybe FilePath)
runDirectoryDialog title owner
  = withCString title $ \ctitle ->
    do cdir <- osSelectDirectory ctitle owner
       maybeCString cdir
foreign import ccall osSelectDirectory :: CString -> WindowHandle -> IO CString

maybeCString :: CString -> IO (Maybe String)
maybeCString cstr
  | cstr == nullPtr = return Nothing
  | otherwise       = do str <- peekCString cstr; free cstr; return (Just str)
  
withCFilter :: [(String,[String])] -> (Ptr CChar -> IO a) -> IO a
withCFilter filter io = 
  let filterSize []               = 2
      filterSize ((name,exts):rs) = (length name+1)+(foldr (\x n -> length x+1+n) 0 exts)+filterSize rs
      
      pokeFilter []               cfilter = do
        pokeElemOff cfilter 0 (castCharToCChar '\0')
        pokeElemOff cfilter 1 (castCharToCChar '\0')
      pokeFilter ((name,exts):rs) cfilter = do
        pokeArray0 (castCharToCChar '\0') cfilter (map castCharToCChar name)
        cfilter <- pokeExts name exts (cfilter `plusPtr` (length name+1))
        pokeFilter rs cfilter
        
      pokeExts name []    cfilter = error ("Filter \"" ++ name ++ "\" has empty list of file extensions")
      pokeExts name [ext] cfilter = do
        pokeArray0 (castCharToCChar '\0') cfilter (map castCharToCChar ext)
        return (cfilter `plusPtr` (length ext+1))
      pokeExts name (ext:exts) cfilter = do
        pokeArray0 (castCharToCChar ';')  cfilter (map castCharToCChar ext)
        pokeExts name exts (cfilter `plusPtr` (length ext+1))
  in
      allocaArray (filterSize filter) $ \cfilter -> do
        pokeFilter filter cfilter
        io cfilter


-----------------------------------------------------------------------------------------
-- Color selection dialog
-----------------------------------------------------------------------------------------

-- | Run a dialog to select a color. Returns 'Nothing' when cancelled.
runColorDialog :: WindowHandle -> IO (Maybe Color)
runColorDialog owner = alloca $ \cref -> do
    res <- osRunColorDialog cref owner
    if res 
      then do
        c <- peek cref
        return (Just (fromCColor c)) 
      else return Nothing
foreign import ccall osRunColorDialog :: Ptr CColor -> WindowHandle -> IO Bool 

-----------------------------------------------------------------------------------------
-- Font selection dialog
-----------------------------------------------------------------------------------------

-- | Run a dialog to select a font. Returns 'Nothing' when cancelled.
runFontDialog :: WindowHandle -> IO (Maybe FontDef)
runFontDialog owner =
    alloca $ \fnameref ->
    alloca $ \fsizeref ->
    alloca $ \fweightref ->
    alloca $ \fstyleref ->
    alloca $ \funderlineref ->
    alloca $ \fstrikeoutref -> do
        res <- osRunFontDialog fnameref fsizeref fweightref fstyleref funderlineref fstrikeoutref owner
        if res
         then do
            cname <- peek fnameref
            csize <- peek fsizeref
            cweight <- peek fweightref
            cstyle <- peek fstyleref
            cunderline <- peek funderlineref
            cstrikeout <- peek fstrikeoutref
            fontdef <- fromCFontDef cname csize cweight cstyle cunderline cstrikeout
            free cname
            return (Just fontdef)
         else return Nothing
foreign import ccall osRunFontDialog :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CBool -> Ptr CBool -> WindowHandle -> IO Bool

-----------------------------------------------------------------------------------------
-- About dialog
-----------------------------------------------------------------------------------------

runAboutDialog :: String   -- ^ application name
               -> String   -- ^ application version
           -> String   -- ^ copyright
           -> String   -- ^ comments
           -> [String] -- ^ authors
           -> [String] -- ^ documenters
           -> String   -- ^ translator credits
           -> Bitmap   -- ^ logo
           -> WindowHandle
           -> IO ()
runAboutDialog appName appVersion copyright comments authors documenters tcredits logo owner =
    withCString appName      $ \cAppName     ->
    withCString appVersion   $ \cAppVersion  ->
    withCString copyright    $ \cCopyright   ->
    withCString comments     $ \cComments    ->
    withCStrings authors     $ \cAuthors     ->
    withCStrings documenters $ \cDocumenters ->
    (if null tcredits then ($ nullPtr) else withCString tcredits) $ \cTCredits ->
    withCBitmap logo         $ \cBmp         ->
       osRunAboutDialog cAppName cAppVersion cCopyright cComments cAuthors cDocumenters cTCredits cBmp owner
foreign import ccall osRunAboutDialog :: CString -> CString -> CString -> CString -> Ptr CChar -> Ptr CChar -> CString -> BitmapHandle -> WindowHandle -> IO ()
