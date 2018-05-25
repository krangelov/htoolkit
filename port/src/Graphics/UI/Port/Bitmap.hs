-----------------------------------------------------------------------------------------
{-| Module      :  Bitmap
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Bitmaps.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Bitmap
           ( 
           -- * Construction
             createBitmap
           , readBitmap
           , writeBitmap, hWriteBitmap

           -- * Operations
           , getBitmapSize
           , setBitmapSize
           , drawInBitmap

           -- * Codec's
           , getAvailableCodecs
           ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import Graphics.UI.Port.Types
import Control.Monad(when)
import Control.Exception(bracket)
import System.IO
import System.IO.Error
import System.Posix.IO
import System.Posix.Types

{-----------------------------------------------------------------------------------------
  
-----------------------------------------------------------------------------------------}
-- | Create an empty bitmap of a certain size.
createBitmap :: Size -> IO Bitmap
createBitmap sz
  = withCSize sz $ \cw ch -> 
    do bh <- osCreateBitmap cw ch
       when (bh==nullHandle) (ioError (userError "Bitmap.createBitmap failed."))
       fromCBitmap bh
foreign import ccall osCreateBitmap :: CInt -> CInt -> IO BitmapHandle 


-- | Read a bitmap image from a file. Will automatically decode images like JPEG depending
-- on the platform capabilities. See also 'getAvailableCodecs'.
readBitmap :: FilePath -> IO Bitmap
readBitmap fname
  = withCString fname $ \cname ->
    alloca $ \pres ->
    do bh  <- osReadBitmap cname pres
       res <- peek pres
       case res of
        0 -> fromCBitmap bh
        1 -> ioError (mkIOError illegalOperationErrorType "Bitmap.readBitmap" Nothing (Just fname))
        2 -> ioError (mkIOError doesNotExistErrorType     "Bitmap.readBitmap" Nothing (Just ("decoder for \"" ++ fname ++ "\"")))
        3 -> ioError (mkIOError doesNotExistErrorType     "Bitmap.readBitmap" Nothing (Just fname))
        4 -> ioError (mkIOError permissionErrorType       "Bitmap.readBitmap" Nothing (Just fname))
        _ -> ioError (userError ("Bitmap.readBitmap: unable to open \"" ++ fname ++ "\""))
foreign import ccall osReadBitmap :: CString -> Ptr CInt -> IO BitmapHandle

-- | Write a bitmap image to file. Can select different formats with the supplied MIME
-- string. The image type \"image\/bmp\" is always supported. See also 'getAvailableCodecs'.
writeBitmap :: Bitmap -> FilePath -> String -> IO ()
writeBitmap bm fname mime 
  = withCString fname $ \cname ->
    withCBitmap bm    $ \bh    ->
    withCString mime  $ \cmime ->
    do res <- osWriteBitmap bh cmime cname
       case res of
        0 -> return ()
        1 -> ioError (mkIOError illegalOperationErrorType "writeBitmap" Nothing (Just fname))
        2 -> ioError (mkIOError doesNotExistErrorType     "writeBitmap" Nothing (Just ("\"" ++ mime ++ "\" encoder")))
        3 -> ioError (mkIOError permissionErrorType       "writeBitmap" Nothing (Just fname))
        _ -> ioError (userError ("Bitmap.writeBitmap: unable to write \"" ++ fname ++ "\""))
foreign import ccall osWriteBitmap :: BitmapHandle -> CString -> CString -> IO CInt

-- | Write a bitmap image to file. Can select different formats with the supplied MIME
-- string. The image type \"image\/bmp\" is always supported. See also 'getAvailableCodecs'.
hWriteBitmap :: Bitmap -> Handle -> String -> IO ()
hWriteBitmap bm handle mime 
  = withCBitmap bm    $ \bh    ->
    withCString mime  $ \cmime ->
    do fd <- handleToFd handle
       res <- osWriteBitmapHandle bh cmime fd
       case res of
        0 -> return ()
        1 -> ioError (mkIOError illegalOperationErrorType "hWriteBitmap" Nothing Nothing)
        2 -> ioError (mkIOError doesNotExistErrorType     "hWriteBitmap" Nothing (Just ("\"" ++ mime ++ "\" encoder")))
        3 -> ioError (mkIOError permissionErrorType       "hWriteBitmap" Nothing Nothing)
        _ -> ioError (userError ("Bitmap.hWriteBitmap: unable to write"))
foreign import ccall osWriteBitmapHandle :: BitmapHandle -> CString -> Fd -> IO CInt

-- | Draw to a bitmap.
-- The function passed to drawInWindow should be wrapped with 'withCanvas' function.
drawInBitmap :: Bitmap -> (CanvasHandle -> IO a) -> IO a
drawInBitmap bitmap f
  = withCBitmap bitmap $ \bh ->
    bracket (osGetBitmapCanvas bh) osReleaseBitmapCanvas f
foreign import ccall osGetBitmapCanvas  :: BitmapHandle -> IO CanvasHandle
foreign import ccall osReleaseBitmapCanvas :: CanvasHandle -> IO ()

-- | Get the size of a bitmap.
getBitmapSize :: Bitmap -> IO Size
getBitmapSize bitmap
  = withCBitmap bitmap $ \bh ->
    withCSizeResult (osGetBitmapSize bh)
foreign import ccall osGetBitmapSize :: BitmapHandle -> Ptr CInt -> IO ()

-- | Stretch a bitmap to a different size.
setBitmapSize :: Bitmap -> Size -> IO ()
setBitmapSize bitmap size
  = withCBitmap bitmap$ \bh ->
    withCSize size    $ \cw ch ->
    osSetBitmapSize bh cw ch
foreign import ccall osSetBitmapSize :: BitmapHandle -> CInt -> CInt -> IO ()


-- | Return all available codec's on this platform. 	
getAvailableCodecs :: IO [Codec]
getAvailableCodecs 
  = bracket osInitEncodersEnumerator osFreeEncodersEnumerator getCodecs
  where
    getCodecs enum
      = do done <- osSelectNextEncoder enum
           if (fromCBool done)
            then do name  <- resultCString (osGetCurrentEncoderName enum)
                    descr <- resultCString (osGetCurrentEncoderDescription enum)
                    mime  <- resultCString (osGetCurrentEncoderMime enum)
                    exts  <- resultCStrings (osGetCurrentEncoderExtensions enum) 
                    cread <- osGetCurrentEncoderReadable enum
                    cwrite<- osGetCurrentEncoderWritable enum
                    codecs<- getCodecs enum
                    return (Codec name descr mime (if null exts then [""] else exts) (fromCBool cread) (fromCBool cwrite) : codecs)
            else return []

type EnumHandle = Ptr EH
data EH = EH

foreign import ccall osInitEncodersEnumerator       :: IO EnumHandle
foreign import ccall osGetCurrentEncoderName        :: EnumHandle -> IO CString
foreign import ccall osGetCurrentEncoderDescription :: EnumHandle -> IO CString
foreign import ccall osGetCurrentEncoderMime        :: EnumHandle -> IO CString
foreign import ccall osGetCurrentEncoderExtensions  :: EnumHandle -> IO CString
foreign import ccall osGetCurrentEncoderReadable    :: EnumHandle -> IO CBool
foreign import ccall osGetCurrentEncoderWritable    :: EnumHandle -> IO CBool
foreign import ccall osSelectNextEncoder            :: EnumHandle -> IO CBool
foreign import ccall osFreeEncodersEnumerator       :: EnumHandle -> IO ()

