-----------------------------------------------------------------------------------------
{-| Module      :  Menu
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module contains all functions for creation and management of menus.
    A menu is a list of items that specify options or groups of options (a submenu).
    Clicking a menu item opens a submenu or causes the application to carry out a command.
    A menu is arranged in a hierarchy. At the top level of the hierarchy is the menu bar;
    which contains a list of menus, which in turn can contain submenus.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.Port.Menu
           (
           -- * Menu bar
             mainMenu
           -- * Sub menus
           , insertMenu
           , getMenuItemCount
           -- * Popup menus
           , createPopupMenu
           , trackPopupMenu
           -- * Menu items
           , insertMenuItem
           , insertMenuSeparatorItem
           -- * Common functions
           , destroyMenu
           , getMenuItemPos
           , setMenuItemEnabled, getMenuItemEnabled
           , setMenuLabel, getMenuLabel
           ) where

import Foreign
import Foreign.C
import Data.Maybe( fromMaybe )
import Control.Concurrent.MVar
import Graphics.UI.Port.Types
import Graphics.UI.Port.Handlers
import Graphics.UI.Port.PtrMap as PtrMap
import System.IO.Unsafe( unsafePerformIO )

-----------------------------------------------------------------------------------------
--  Menu bar
-----------------------------------------------------------------------------------------

-- | The main application menu (the menu bar)
mainMenu :: MenuHandle
mainMenu = nullHandle

-----------------------------------------------------------------------------------------
--  Sub menus
-----------------------------------------------------------------------------------------

-- | The 'insertMenu' function creates and inserts an item with submenu in the parent menu.
-- The created submenu is initially empty. You can insert or append menu items by using the
-- 'insertMenuItem', 'insertMenuCheckItem', 'insertMenuRadioItem' or 'insertMenuSeparatorItem'
-- functions. By using 'insertMenu' you can create nested submenus.
insertMenu :: MenuHandle                          -- ^ The handle of the parent menu.
                                                  -- Use the 'mainMenu' handle if you want to
                                                  -- place the menu in the application menu bar.
           -> Maybe Int                           -- ^ The position where to place the submenu.
                                                  -- or Nothing if you want to append it.
           -> IO MenuHandle                       -- ^ The handle of the created submenu.
insertMenu handle pos = osInsertMenu handle (fromMaybe (-1) pos)
foreign import ccall osInsertMenu :: MenuHandle -> Int -> IO MenuHandle

-- | The 'getMenuItemCount' function determines the number of items in the
-- specified popup or sub menu.
foreign import ccall "osGetMenuItemCount" getMenuItemCount :: MenuHandle -> IO Int

-----------------------------------------------------------------------------------------
--  Popup menus
-----------------------------------------------------------------------------------------

-- | The 'createPopupMenu' function creates a drop-down menu. The menu is initially empty.
-- You can insert or append menu items by using the 'insertMenuItem',
-- 'insertMenuCheckItem', 'insertMenuRadioItem' or 'insertMenuSeparatorItem' functions.
-- Using the 'insertMenu' you can create nested submenus.
foreign import ccall "osCreatePopupMenu" createPopupMenu :: IO MenuHandle -- ^ The handle of the created popup menu.

-- | The 'trackPopupMenu' function displays a shortcut menu at the specified location in the
-- window and tracks the selection of items on the menu.
trackPopupMenu :: MenuHandle                      -- ^ The handle of the popup menu
               -> WindowHandle                    -- ^ The handle of the window in which to popup the menu
               -> Point                           -- ^ The location inside the window at which to display the menu
               -> IO ()
trackPopupMenu handle hwnd pos = withCPoint pos (osTrackPopupMenu handle hwnd)
foreign import ccall osTrackPopupMenu :: MenuHandle -> WindowHandle -> CInt -> CInt -> IO ()

-----------------------------------------------------------------------------------------
--  Menu items
-----------------------------------------------------------------------------------------

-- | Add a menu item. An event handler for a menu item can be
-- installed with 'setMenuCommandHandler'.
insertMenuItem :: ActionHandle -> MenuHandle -> Maybe Int -> IO MenuHandle
insertMenuItem haction hmenu pos = osInsertMenuItem haction hmenu (fromMaybe (-1) pos)
foreign import ccall osInsertMenuItem :: ActionHandle -> MenuHandle -> Int -> IO MenuHandle

-- | Add a menu item separator line.
insertMenuSeparatorItem :: MenuHandle -> Maybe Int -> IO MenuHandle
insertMenuSeparatorItem handle pos = osInsertMenuSeparatorItem handle (fromMaybe (-1) pos)
foreign import ccall osInsertMenuSeparatorItem :: MenuHandle -> Int -> IO MenuHandle

-----------------------------------------------------------------------------------------
-- Common functions
-----------------------------------------------------------------------------------------

-- | The destroyMenu function deletes an item from the specified menu. If the menu item opens a menu or submenu,
-- this function destroys the handle to the menu or submenu and frees the memory used by the menu or submenu.
foreign import ccall "osDestroyMenu" destroyMenu :: MenuHandle -> IO ()

-- | Returns the position of the item in the parent menu
foreign import ccall "osGetMenuItemPos" getMenuItemPos :: MenuHandle -> IO Int

-- | Enable or disable a menu item.
foreign import ccall "osSetMenuItemEnabled" setMenuItemEnabled :: MenuHandle -> Bool -> IO ()

-- | returns True if the menu item is enabled.
foreign import ccall "osGetMenuItemEnabled" getMenuItemEnabled :: MenuHandle -> IO Bool

-- | Change the label of a menu item (or checkable menu item).
setMenuLabel :: MenuHandle -> String -> IO ()
setMenuLabel hmenu title = withCString title (osSetMenuLabel hmenu)
foreign import ccall osSetMenuLabel :: MenuHandle -> CString -> IO ()

-- | Returns the label of a menu item (or checkable menu item).
getMenuLabel :: MenuHandle -> IO String
getMenuLabel hmenu  = resultCString (osGetMenuLabel hmenu)
foreign import ccall osGetMenuLabel :: MenuHandle -> IO CString
