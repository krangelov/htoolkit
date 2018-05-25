-----------------------------------------------------------------------------------------
{-| Module      :  Menu
    Copyright   :  (c) Krasimir Angelov 2003
    License     :  BSD-style

    Maintainer  :  ka2_mail@yahoo.com
    Stability   :  provisional
    Portability :  portable

    The module contains all utilitites required for creation and management of menus.
    A menu is a list of items that specify options or groups of options (a submenu).
    Clicking a menu item opens a submenu or causes the application to carry out a command.
    A menu is arranged in a hierarchy. At the top level of the hierarchy is the menu bar;
    which contains a list of menus, which in turn can contain submenus.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.GIO.Menu
            ( -- * Menus
              Menu, mainMenu, menuAt, menu
            , popupMenu, trackPopupMenu
              -- * Menu items
	      -- ** Action based items
            , MenuActionItem, menuActionItemAt, menuActionItem, getMenuAction
              -- ** Command items
            , MenuItem, menuitemAt, menuitem
	      -- ** Checked items
            , MenuCheck, menucheckAt, menucheck
              -- ** Radio items
            , MenuRadioItem, menuRadioItemAt, menuRadioItem
            , setMenuRadioGroup
              -- ** Menu separator
            , MenuLine, menulineAt, menuline

              -- internal
            , hmenu
            ) where

import System.IO.Unsafe( unsafePerformIO )
import qualified Graphics.UI.Port as Lib
import Graphics.UI.GIO.Types
import Graphics.UI.GIO.Attributes
import Graphics.UI.GIO.Layout
import Graphics.UI.GIO.Events
import Graphics.UI.GIO.Window
import Graphics.UI.GIO.Action
import Graphics.UI.GIO.MenuType

{--------------------------------------------------------------------
  Menus
--------------------------------------------------------------------}

-- | The main application menu (the menu bar)
mainMenu :: Menu
mainMenu = Menu Lib.mainMenu

-- | The 'menuAt' function creates and inserts an item with submenu in the parent menu. 
-- The created submenu is initially empty. You can populate it with items by using the 
-- 'menuitemAt', 'menucheckAt', 'menuRadioItemAt' or 'menulineAt' functions.
-- Using the 'menuAt' you can create nested submenus.
menuAt :: Maybe Int                               -- ^ The position where to place the submenu
                                                  -- or Nothing if you want to append the item.
       -> [Prop Menu]                             -- ^ The setup of the menu attributes
       -> Menu                                    -- ^ The parent menu
       -> IO Menu                                 -- ^ The created sub menu
menuAt pos props parent
  = do m     <- do hmenu <- Lib.insertMenu (hmenu parent) pos
                   return (Menu hmenu)
       set m props
       return m

-- | The function is the same as the 'menuAt' function but always appends the item. The 'menu' function
-- is semantically equal to @menuAt Nothing@
menu :: [Prop Menu] -> Menu -> IO Menu
menu = menuAt Nothing

-- | The 'popupMenu' function creates a popup menu. The menu is initially empty.
-- You can populate it with items by using the 'menuitemAt', 'menucheckAt',
-- 'menuRadioItemAt' or 'menulineAt' functions. Using the 'menuAt' you can create nested submenus.
popupMenu :: [Prop Menu] -> IO Menu
popupMenu props
  = do m     <- do hmenu <- Lib.createPopupMenu
                   return (Menu hmenu)
       set m props
       return m

-- | The 'trackPopupMenu' function displays a popup menu at the specified location in the
-- window and tracks the selection of items on the menu.
trackPopupMenu :: Menu                      	  -- ^ The popup menu
               -> Window                          -- ^ The window in which to popup the menu
               -> Point                           -- ^ The location inside the window at which to display the menu
               -> IO ()
trackPopupMenu menu window pos = Lib.trackPopupMenu (hmenu menu) (hwindow window) pos

instance Able Menu where
  enabled   = newStdAttr hmenu Lib.getMenuItemEnabled Lib.setMenuItemEnabled

instance Titled Menu where
  title = newStdAttr hmenu Lib.getMenuLabel Lib.setMenuLabel

instance Countable Menu where
  count = readAttr "count" (Lib.getMenuItemCount . hmenu)

instance Positioned Menu where
  pos = readAttr "pos" (Lib.getMenuItemPos . hmenu)

instance Deadly Menu where
  destroyWidget m = Lib.destroyMenu (hmenu m)
  destroy         = newStdEvent hmenu Lib.getMenuDestroyHandler Lib.setMenuDestroyHandler Lib.setMenuDestroyDefHandler

--------------------------------------------------------------------
--  Action based menu items
--------------------------------------------------------------------

data MenuActionItem a = MenuActionItem !MenuHandle !a
hitem (MenuActionItem h action) = h

menuActionItemAt :: IsAction a => Maybe Int -> [Prop (MenuActionItem a)] -> a -> Menu -> IO (MenuActionItem a)
menuActionItemAt pos props action menu = do
	mitem <- do
		hitem <- Lib.insertMenuItem (haction action) (hmenu menu) pos
		return (MenuActionItem hitem action)
	set mitem props
	return mitem

menuActionItem :: IsAction a => [Prop (MenuActionItem a)] -> a -> Menu -> IO (MenuActionItem a)
menuActionItem = menuActionItemAt Nothing

getMenuAction :: MenuActionItem a -> a
getMenuAction (MenuActionItem h action) = action

instance Able a => Able (MenuActionItem a) where
  enabled   = mapAttrObj getMenuAction enabled

instance Commanding a => Commanding (MenuActionItem a) where
  command = mapEventObj getMenuAction command

instance DynamicUpdate a => DynamicUpdate (MenuActionItem a) where
  update = mapEventObj getMenuAction update

instance Titled a => Titled (MenuActionItem a) where
  title = mapAttrObj getMenuAction title

instance HasIcon a => HasIcon (MenuActionItem a) where
  icon  = mapAttrObj getMenuAction icon

instance Tipped a => Tipped (MenuActionItem a) where
  tooltip = mapAttrObj getMenuAction tooltip

instance Checked a => Checked (MenuActionItem a) where
  checked = mapAttrObj getMenuAction checked

instance Accelerated a => Accelerated (MenuActionItem a) where
  accel = mapAttrObj getMenuAction accel

instance Positioned (MenuActionItem a) where
  pos = readAttr "pos" (Lib.getMenuItemPos . hitem)

instance Deadly (MenuActionItem a) where
  destroyWidget m = Lib.destroyMenu (hitem m)
  destroy         = newStdEvent hitem Lib.getMenuDestroyHandler Lib.setMenuDestroyHandler Lib.setMenuDestroyDefHandler

--------------------------------------------------------------------
--  Menu items
--------------------------------------------------------------------
-- | Menu items are labeled entries in a menu.
type MenuItem = MenuActionItem Action

-- | Create a menu item and insert it at specified position.
menuitemAt :: Maybe Int -> [Prop MenuItem] -> Menu -> IO MenuItem
menuitemAt pos props menu = do
	act <- action []
	menuActionItemAt pos props act menu

-- | Create a menu item and appends it to parent menu.
menuitem :: [Prop MenuItem] -> Menu -> IO MenuItem
menuitem = menuitemAt Nothing

--------------------------------------------------------------------
--  Menu radio items
--------------------------------------------------------------------

-- | Radio menu items are labeled entries in a menu with bookmark.
-- Sometimes, a group of menu items corresponds to a set of mutually exclusive options.
-- In this case, you can indicate the selected option by using a selected radio menu item.
-- To check a menu item use the 'checked' attribute.
type MenuRadioItem = MenuActionItem RadioAction

-- | Create a radio menu item and insert it at specified position.
menuRadioItemAt :: Maybe Int                      -- ^ The position where to place the item
                                                  -- or Nothing if you want to append it.
       -> [Prop MenuRadioItem]                    -- ^ The setup of the radio item attributes
       -> Menu                                    -- ^ The parent menu
       -> IO MenuRadioItem                        -- ^ The created radio item
menuRadioItemAt pos props menu = do
	act <- radioAction []
	menuActionItemAt pos props act menu

-- | The function is the same as the 'menuRadioItemAt' function but always appends the item. The 'menuRadioItem'
-- function is semantically equal to @menuRadioItemAt Nothing@
menuRadioItem :: [Prop MenuRadioItem] -> Menu -> IO MenuRadioItem
menuRadioItem = menuRadioItemAt Nothing

-- | The 'setMenuRadioGroup' function specifies a set of mutually exclusive options.
setMenuRadioGroup :: [MenuRadioItem] -> IO ()
setMenuRadioGroup items = setActionRadioGroup (map getMenuAction items)

--------------------------------------------------------------------
--  Checked menu items
--------------------------------------------------------------------
-- | Checked menu items are labeled entries in a menu with check mark.
-- Applications typically check or clear a menu item to indicate whether
-- an option is in effect.
type MenuCheck = MenuActionItem CheckAction

-- | Create a check menu item and insert it at specified position.
menucheckAt :: Maybe Int                      -- ^ The position where to place the item
                                              -- or Nothing if you want to append it.
            -> [Prop MenuCheck]               -- ^ The setup of the item attributes
            -> Menu                           -- ^ The parent menu
            -> IO MenuCheck                   -- ^ The created checked item
menucheckAt pos props menu = do
	act <- checkAction []
	menuActionItemAt pos props act menu

-- | The function is the same as the 'menucheckAt' function but always appends the item. The 'menucheck'
-- function is semantically equal to @menucheckAt Nothing@
menucheck :: [Prop MenuCheck] -> Menu -> IO MenuCheck
menucheck = menucheckAt Nothing

--------------------------------------------------------------------
--  Menu separator
--------------------------------------------------------------------

-- | Menu separator item
newtype MenuLine = MenuLine MenuHandle
hline (MenuLine h) = h

-- | Insert a menu seperator line at specified position.
menulineAt :: Maybe Int -> Menu -> IO MenuLine
menulineAt pos menu
  = do hitem  <- Lib.insertMenuSeparatorItem (hmenu menu) pos
       return (MenuLine hitem)

-- | Append a menu seperator line
menuline :: Menu -> IO MenuLine
menuline = menulineAt Nothing

instance Positioned MenuLine where
  pos = readAttr "pos" (Lib.getMenuItemPos . hline)

instance Deadly MenuLine where
  destroyWidget m = Lib.destroyMenu (hline m)
  destroy         = newStdEvent hline Lib.getMenuDestroyHandler Lib.setMenuDestroyHandler Lib.setMenuDestroyDefHandler
