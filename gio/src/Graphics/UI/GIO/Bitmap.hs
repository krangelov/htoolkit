-----------------------------------------------------------------------------------------
{-| Module      :  Bitmap
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Bitmaps
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Bitmap
            ( 
            -- * Bitmaps
              createBitmap
            , readBitmap, writeBitmap, hWriteBitmap
            , getAvailableCodecs
            ) where


import qualified Graphics.UI.Port as Lib
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import System.IO


{--------------------------------------------------------------------
  Bitmaps
--------------------------------------------------------------------}

-- | Create an empty bitmap of a certain size.
createBitmap :: Size -> IO Bitmap
createBitmap = Lib.createBitmap

-- | Read a bitmap from file.
readBitmap :: FilePath -> [Prop Bitmap] -> IO Bitmap
readBitmap fname props
  = do b <- Lib.readBitmap fname
       set b props
       return b
       
-- | Write a bitmap image to file. Can select different formats with the supplied MIME
-- string. The image type \"image\/bmp\" is always supported. See also 'getAvailableCodecs'.
writeBitmap :: Bitmap -> FilePath -> String -> IO ()
writeBitmap = Lib.writeBitmap

-- | Write a bitmap image to a file handle. Can select different formats with the supplied MIME
-- string. The image type \"image\/bmp\" is always supported. See also 'getAvailableCodecs'.
hWriteBitmap :: Bitmap -> Handle -> String -> IO ()
hWriteBitmap = Lib.hWriteBitmap

-- | Return all available codec's on this platform. 	
getAvailableCodecs :: IO [Codec]
getAvailableCodecs = Lib.getAvailableCodecs

instance Dimensions Bitmap where
  frame   = newAttr (\b -> do sz <- Lib.getBitmapSize b; return (rectOfSize sz))
                    (\b r -> do Lib.setBitmapSize b (rectSize r))
