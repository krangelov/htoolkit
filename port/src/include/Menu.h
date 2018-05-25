#ifndef MENU_H
#define MENU_H

#include "Types.h"

MenuHandle osCreatePopupMenu();
void osTrackPopupMenu(MenuHandle handle, WindowHandle hwnd, int x, int y);

MenuHandle osInsertMenu(MenuHandle parent, int pos);
MenuHandle osInsertMenuItem(ActionHandle haction, MenuHandle hmenu, int pos);
MenuHandle osInsertMenuSeparatorItem(MenuHandle parent, int pos);

void osDestroyMenu(MenuHandle handle);

int osGetMenuItemCount(MenuHandle handle);

void osSetMenuItemEnabled(MenuHandle item, BOOL bState);
BOOL osGetMenuItemEnabled(MenuHandle item);

void  osSetMenuLabel(MenuHandle item, char* title);
char *osGetMenuLabel(MenuHandle item);

int osGetMenuItemPos(MenuHandle handle);

#endif
