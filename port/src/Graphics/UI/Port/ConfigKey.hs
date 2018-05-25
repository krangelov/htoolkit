{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-----------------------------------------------------------------------------------------
{-| Module      :  ConfigKey
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.ConfigKey
    ( ConfigKey(..)
    ) where

import Foreign
import Foreign.C

-----------------------------------------------------------------------------------------
-- ConfigKey class definition
-----------------------------------------------------------------------------------------

class ConfigKey a where
  setConfigKey    :: String -> a -> IO ()
  getConfigKey    :: String -> IO a
  getConfigKeyDef :: String -> a -> IO a


-----------------------------------------------------------------------------------------
-- Class instance for String type
-----------------------------------------------------------------------------------------

instance ConfigKey String where
  setConfigKey name value =
    (withCString name  $ \cname ->
     withCString value $ \cvalue ->
       osSetConfigStringKey cname cvalue)
  
  getConfigKey name = getConfigKeyDef name ""
  
  getConfigKeyDef name defvalue =
    (withCString name $ \cname -> do
       ptr <- osGetConfigStringKey cname nullPtr
       (if ptr == nullPtr 
          then return defvalue
          else do
                value <- peekCString ptr
                free ptr
                return value))

foreign import ccall osGetConfigStringKey :: CString -> CString -> IO CString
foreign import ccall osSetConfigStringKey :: CString -> CString -> IO ()


-----------------------------------------------------------------------------------------
-- Class instance for Int type
-----------------------------------------------------------------------------------------

instance ConfigKey Int where
  setConfigKey name value =
    withCString name (\cname -> osSetConfigIntKey cname value)

  getConfigKey name = getConfigKeyDef name 0

  getConfigKeyDef name defvalue =
    withCString name (\cname -> osGetConfigIntKey cname defvalue)

foreign import ccall osGetConfigIntKey :: CString -> Int -> IO Int
foreign import ccall osSetConfigIntKey :: CString -> Int -> IO ()


-----------------------------------------------------------------------------------------
-- Class instance for Double type
-----------------------------------------------------------------------------------------

instance ConfigKey Double where
  setConfigKey name value =
    withCString name (\cname -> osSetConfigDoubleKey cname value)

  getConfigKey name = getConfigKeyDef name 0.0

  getConfigKeyDef name defvalue =
    withCString name (\cname -> osGetConfigDoubleKey cname defvalue)

foreign import ccall osGetConfigDoubleKey :: CString -> Double -> IO Double
foreign import ccall osSetConfigDoubleKey :: CString -> Double -> IO ()


-----------------------------------------------------------------------------------------
-- Class instance for Bool type
-----------------------------------------------------------------------------------------

instance ConfigKey Bool where
  setConfigKey name value =
    withCString name (\cname -> osSetConfigBoolKey cname value)

  getConfigKey name = getConfigKeyDef name False

  getConfigKeyDef name defvalue =
    withCString name (\cname -> osGetConfigBoolKey cname defvalue)

foreign import ccall osGetConfigBoolKey :: CString -> Bool -> IO Bool
foreign import ccall osSetConfigBoolKey :: CString -> Bool -> IO ()
