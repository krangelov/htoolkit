-----------------------------------------------------------------------------------------
{-| Module      :  ToolBar
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    ToolBar.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.ToolBar
        (
        -- * Creation
          createToolBar
        , destroyToolBar
        , getToolBarButtonCount

        , insertToolButton
        , insertToolLine
        , getToolItemPos
        , destroyToolItem
        ) where

import Graphics.UI.Port.Types
import Graphics.UI.Port.Handlers 
import Graphics.UI.Port.PtrMap as PtrMap
import Foreign
import Foreign.C
import Control.Concurrent.MVar
import Data.Maybe
import System.IO.Unsafe( unsafePerformIO )

createToolBar :: String -> PositionType -> Int -> Int -> Int -> IO WindowHandle
createToolBar name place band_num band_position offset =
    withCString name (\cname -> osCreateToolBar cname (toCPositionType place) band_num band_position offset)
foreign import ccall osCreateToolBar :: CString -> CInt -> Int -> Int -> Int -> IO WindowHandle

foreign import ccall "osDestroyToolBar" destroyToolBar :: WindowHandle -> IO ()

foreign import ccall "osGetToolBarButtonCount" getToolBarButtonCount :: WindowHandle -> IO Int

insertToolButton :: ActionHandle -> WindowHandle -> Maybe Int -> IO ToolHandle
insertToolButton action toolBar pos = osInsertToolButton action toolBar (fromMaybe (-1) pos)
foreign import ccall osInsertToolButton :: ActionHandle -> WindowHandle -> Int -> IO ToolHandle

insertToolLine :: WindowHandle -> Maybe Int -> IO ToolHandle
insertToolLine toolBar pos = osInsertToolLine toolBar (fromMaybe (-1) pos)
foreign import ccall osInsertToolLine :: WindowHandle -> Int -> IO ToolHandle

foreign import ccall "osGetToolItemPos" getToolItemPos :: ToolHandle -> IO Int

foreign import ccall "osDestroyToolItem" destroyToolItem :: ToolHandle -> IO ()
