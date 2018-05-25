-- #hide
module Graphics.UI.GIO.MenuType where

import Graphics.UI.GIO.Types

-- | A menu is a widget that contains menu items or other (sub) menus.
-- The top level menu is always a 'mainMenu'.
newtype Menu = Menu {hmenu :: MenuHandle}
