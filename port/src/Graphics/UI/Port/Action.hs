-----------------------------------------------------------------------------------------
{-| Module      :  Action
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    Actions represent operations that the user can be perform, along with some information how it
    should be presented in the interface. There are functions which allows to create menu and toolbar items
    from a given action. As well as the event that is fired when the action gets activated, the following also
    gets associated with the action: a label, an accelerator, a tooltip and a toolbar label (usually shorter than label).

    Apart from regular actions, there are check actions, which can be toggled between two states and
    radio actions, of which only one in a group can be in the 'selected' state.
-}
-----------------------------------------------------------------------------------------

module Graphics.UI.Port.Action
    ( createAction
    , createCheckAction
    , createRadioAction
    , createDropDownAction
    , setActionRadioGroup
    , setActionBitmap, getActionBitmap
    , setActionEnabled, getActionEnabled
    , setActionTip, getActionTip
    , setActionText, getActionText
    , setActionShortText, getActionShortText
    , setActionChecked, getActionChecked
    , setActionAccel, getActionAccel
    , destroyAction
    ) where

import Graphics.UI.Port.Types
import Graphics.UI.Port.Handlers
import Graphics.UI.Port.PtrMap as PtrMap
import Foreign
import Foreign.C
import Control.Concurrent.MVar

foreign import ccall "osCreateAction" createAction :: IO ActionHandle
foreign import ccall "osCreateCheckAction" createCheckAction :: IO ActionHandle
foreign import ccall "osCreateRadioAction" createRadioAction :: IO ActionHandle
foreign import ccall "osCreateDropDownAction" createDropDownAction :: MenuHandle -> IO ActionHandle

setActionRadioGroup :: [ActionHandle] -> IO ()
setActionRadioGroup handles = withArray0 nullHandle handles osSetActionRadioGroup
foreign import ccall osSetActionRadioGroup :: Ptr ActionHandle -> IO ()

setActionBitmap :: ActionHandle -> Maybe Bitmap -> IO ()
setActionBitmap haction (Just bmp) = do
  map <- takeMVar actionBitmaps
  withCBitmap bmp (osSetActionBitmap haction)
  putMVar actionBitmaps (insert haction bmp map)
setActionBitmap haction Nothing    = do
  map <- takeMVar actionBitmaps
  osSetActionBitmap haction nullPtr
  putMVar actionBitmaps (delete haction map)
foreign import ccall osSetActionBitmap :: ActionHandle -> BitmapHandle -> IO ()

getActionBitmap :: ActionHandle -> IO (Maybe Bitmap)
getActionBitmap haction = do
  map <- readMVar actionBitmaps
  return (PtrMap.lookup haction map)

foreign import ccall "osSetActionEnabled" setActionEnabled :: ActionHandle -> Bool -> IO ()
foreign import ccall "osGetActionEnabled" getActionEnabled :: ActionHandle -> IO Bool

setActionTip :: ActionHandle -> String -> IO ()
setActionTip haction tip = withCString tip (osSetActionTip haction)
foreign import ccall osSetActionTip :: ActionHandle -> CString -> IO ()

getActionTip :: ActionHandle -> IO String
getActionTip haction = resultCString (osGetActionTip haction)
foreign import ccall osGetActionTip :: ActionHandle -> IO CString

setActionText :: ActionHandle -> String -> IO ()
setActionText haction tip = withCString tip (osSetActionText haction)
foreign import ccall osSetActionText :: ActionHandle -> CString -> IO ()

getActionText :: ActionHandle -> IO String
getActionText haction = resultCString (osGetActionText haction)
foreign import ccall osGetActionText :: ActionHandle -> IO CString

setActionShortText :: ActionHandle -> String -> IO ()
setActionShortText haction tip = withCString tip (osSetActionShortText haction)
foreign import ccall osSetActionShortText :: ActionHandle -> CString -> IO ()

getActionShortText :: ActionHandle -> IO String
getActionShortText haction = resultCString (osGetActionShortText haction)
foreign import ccall osGetActionShortText :: ActionHandle -> IO CString

foreign import ccall "osSetActionChecked" setActionChecked :: ActionHandle -> Bool -> IO ()
foreign import ccall "osGetActionChecked" getActionChecked :: ActionHandle -> IO Bool

getActionAccel :: ActionHandle -> IO Key
getActionAccel haction
  = alloca $ \pckey ->
    alloca $ \pcmods ->
      do osGetActionAccel haction pckey pcmods
         ckey  <- peek pckey
         cmods <- peek pcmods
         return (fromCKey ckey cmods)
foreign import ccall osGetActionAccel :: ActionHandle -> Ptr CInt -> Ptr CWord -> IO ()

setActionAccel :: ActionHandle -> Key -> IO ()
setActionAccel haction key
  = let (ckey,cmods) = toCKey key
    in osSetActionAccel haction ckey cmods
foreign import ccall osSetActionAccel :: ActionHandle -> CInt -> CWord -> IO ()

foreign import ccall "osDestroyAction" destroyAction :: ActionHandle -> IO ()
