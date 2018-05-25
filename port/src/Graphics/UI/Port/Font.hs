-----------------------------------------------------------------------------------------
{-| Module      :  Font
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Fonts.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Font
        ( 
        -- * Fonts
          createFont
        , defaultFont

        -- * Metrics (on a certain canvas).
        , getFontMetrics
        , getFontCharWidth
        , getFontStringWidth
        
        -- * Enumerate fonts
        , getFontNames, getFontVariants 

        -- * Standard font definitions.
        , defaultFontDef
        , serifFontDef
        , sansSerifFontDef
        , nonProportionalFontDef
        , smallFontDef
        , symbolFontDef
        ) where

import Graphics.UI.Port.Types

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.ForeignPtr

import qualified Data.Map as Map
import Data.List( sort, nub )
import Control.Monad    ( when )
import Control.Exception ( bracket )
import System.IO.Unsafe ( unsafePerformIO )
import System.IO.Error  ( mkIOError, doesNotExistErrorType )


{-----------------------------------------------------------------------------------------
  Create
-----------------------------------------------------------------------------------------}
-- | Create a new font from a font definition.
createFont :: FontDef -> IO Font
createFont fontDef
  = withCFontDef fontDef $ \cname csize cweight cstyle ->
    do handle <- osCreateFont cname csize cweight cstyle
       when (nullPtr == handle) (ioError (mkIOError doesNotExistErrorType "createFont" Nothing (Just (show fontDef))))
       fromCFont fontDef handle
foreign import ccall osCreateFont :: CString -> CInt -> CInt -> CInt -> IO FontHandle



{-----------------------------------------------------------------------------------------
  Font properties
-----------------------------------------------------------------------------------------}

insertUniq :: (Ord a) => a -> [a] -> [a]
insertUniq a list@(b:x)
    | a<b       = a:list
    | a>b       = b:(insertUniq a x)
    | otherwise = list
insertUniq a _ = [a]
        
sortAndRemoveDuplicates :: (Ord a) => [a] -> [a]
sortAndRemoveDuplicates (e:es) = insertUniq e (sortAndRemoveDuplicates es)
sortAndRemoveDuplicates _ = []

-- | Enumerate all the available font names.
getFontNames :: IO [FontName]
getFontNames 
  = do names <- resultCStrings (osGetAvailableFontNames)
       return (sortAndRemoveDuplicates names)
foreign import ccall osGetAvailableFontNames :: IO CString;
          
-- | The expression (@getFontVariants fontname min max@) returns all avaiable font definitions
-- where the font name is @fontname@ and the font size is between @min@ and @max@ (inclusive).
-- The keys in the returned map are all posible combinations between weight and style,
-- and the value coresponding to them in the map is a list of sizes for which this combination is
-- available.
getFontVariants :: FontName -> FontSize -> FontSize -> IO (Map.Map (FontWeight, FontStyle) [FontSize])
getFontVariants fontname low high
    = withCString fontname $ \cname ->
      bracket (osGetAvailableFontVariants cname (toCInt low') (toCInt high')) free decodeVariants
    where
        low'  = max low  2
        high' = max high 2
        allFontSizes = [low'..high']
        
        fst3 (x,y,z) = x
        
        decodeVariants :: Ptr CInt -> IO (Map.Map (FontWeight, FontStyle) [FontSize])
        decodeVariants pints 
            | pints == nullPtr = ioError (mkIOError doesNotExistErrorType "getFontVariants" Nothing (Just ("\"" ++ fontname ++ "\" fonts family")))
        decodeVariants pints = do
            cweight <- peekElemOff pints 0
            if cweight == 0
              then return Map.empty
              else do cstyle  <- peekElemOff pints 1
                      csize   <- peekElemOff pints 2
                      variants <- decodeVariants (pints `plusPtr` (sizeOf cweight * 3))
                      let sizes = if csize == 0 then allFontSizes else [fromCInt csize]
                      return (Map.insertWith (foldr insertUniq) (fromCWeight cweight,fst3 (fromCStyle cstyle)) sizes variants)
foreign import ccall osGetAvailableFontVariants :: CString -> CInt -> CInt -> IO (Ptr CInt);

{-----------------------------------------------------------------------------------------
  Font metrics
-----------------------------------------------------------------------------------------}
-- | Get the font metrics of a specified font. 
getFontMetrics :: Font -> CanvasHandle -> IO FontMetrics
getFontMetrics font canvas
  = withCFont font $ \cfont ->
    withCFontMetricsResult $ \pascent pdescent pmaxwidth pleading ->
    osGetFontMetrics cfont canvas pascent pdescent pmaxwidth pleading
foreign import ccall osGetFontMetrics :: FontHandle -> CanvasHandle -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

-- | Get the pixel width of a character.
getFontCharWidth :: Font -> Char -> CanvasHandle -> IO Int
getFontCharWidth font c canvas
  = withCFont font $ \cfont ->
    do cw <- osGetFontCharWidth (toCChar c) cfont canvas
       return (fromCInt cw)
foreign import ccall osGetFontCharWidth :: CChar -> FontHandle -> CanvasHandle -> IO CInt

-- | Get the pixel width of a string.
getFontStringWidth :: Font -> String -> CanvasHandle -> IO Int
getFontStringWidth font str canvas
  = withCFont font $ \cfont ->
    withCString str $ \cstr ->
    do cw <- osGetFontStringWidth cstr cfont canvas
       return (fromCInt cw)
foreign import ccall osGetFontStringWidth :: CString -> FontHandle -> CanvasHandle -> IO CInt


{-----------------------------------------------------------------------------------------
  Default fonts
-----------------------------------------------------------------------------------------}
{-# NOINLINE defaultFont #-}
-- | The default window font.
defaultFont :: Font
defaultFont 
  = unsafePerformIO $
    createFont defaultFontDef

{-# NOINLINE defaultFontDef #-}
defaultFontDef :: FontDef
defaultFontDef 
  = unsafePerformIO $ 
    withCFontDefResult $ \pname psize pweight pstyle punderline pstrikeout -> 
    osDefaultFontDef pname psize pweight pstyle
foreign import ccall osDefaultFontDef :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

{-# NOINLINE serifFontDef #-}
serifFontDef :: FontDef
serifFontDef 
  = unsafePerformIO $ 
    withCFontDefResult $ \pname psize pweight pstyle punderline pstrikeout -> 
    osSerifFontDef pname psize pweight pstyle
foreign import ccall osSerifFontDef :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

{-# NOINLINE sansSerifFontDef #-}
sansSerifFontDef :: FontDef
sansSerifFontDef 
  = unsafePerformIO $ 
    withCFontDefResult $ \pname psize pweight pstyle punderline pstrikeout -> 
    osSansSerifFontDef pname psize pweight pstyle
foreign import ccall osSansSerifFontDef :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

{-# NOINLINE smallFontDef #-}
smallFontDef :: FontDef
smallFontDef 
  = unsafePerformIO $ 
    withCFontDefResult $ \pname psize pweight pstyle punderline pstrikeout -> 
    osSmallFontDef pname psize pweight pstyle
foreign import ccall osSmallFontDef :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

{-# NOINLINE nonProportionalFontDef #-}
nonProportionalFontDef :: FontDef
nonProportionalFontDef 
  = unsafePerformIO $ 
    withCFontDefResult $ \pname psize pweight pstyle punderline pstrikeout -> 
    osNonProportionalFontDef pname psize pweight pstyle
foreign import ccall osNonProportionalFontDef :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

{-# NOINLINE symbolFontDef #-}
symbolFontDef :: FontDef
symbolFontDef 
  = unsafePerformIO $ 
    withCFontDefResult $ \pname psize pweight pstyle punderline pstrikeout -> 
    osSymbolFontDef pname psize pweight pstyle
foreign import ccall osSymbolFontDef :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()



