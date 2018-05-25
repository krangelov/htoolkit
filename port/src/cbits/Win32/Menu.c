#include "Menu.h"
#include "Window.h"
#include "DockBar.h"
#include "Internals.h"
#include "Handlers_stub.h"


#define CHECK_MENU_TYPE(handle,mask,ret) \
   if (((handle ? handle->type : MENU_SUBMENU) & (mask)) == 0) \
   { \
   		printf("Invalid menu handle type."); \
   		return ret; \
   }

#define CHECK_MENU_TYPE_V(handle,mask) \
   if (((handle ? handle->type : MENU_SUBMENU) & (mask)) == 0) \
   { \
   		printf("Invalid menu handle type."); \
   		return; \
   }

static void updateMenuBar(MenuHandle parent)
{
	if (!parent)
	{
		RelayoutFrameBars();
		DrawMenuBar(ghWndFrame);
	}
}

MenuHandle osCreatePopupMenu()
{
	MenuHandle handle;
	FrameData *pFrameData;

	pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	handle = newMenuHandle(pFrameData->pActionsMap, NULL, -1, MENU_POPUP, NULL);

	if (handle)
		handle->hMenu = CreatePopupMenu();

	return handle;
};

void osTrackPopupMenu(MenuHandle menu, WindowHandle hWnd, int x, int y)
{
	POINT pos;

	pos.x = x;
	pos.y = y;
	ClientToScreen(hWnd, &pos);

	TrackPopupMenu(menu->hMenu, TPM_LEFTALIGN | TPM_TOPALIGN, pos.x, pos.y, 0, ghWndFrame, NULL);
}

MenuHandle osInsertMenu(MenuHandle parent, int pos)
{
	MenuHandle handle;
	FrameData *pFrameData;

	CHECK_MENU_TYPE(parent, MENU_SUBMENU | MENU_POPUP, NULL);

	pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	handle = newMenuHandle(pFrameData->pActionsMap, parent, pos, MENU_SUBMENU, NULL);

	if (handle)
	{
		handle->hMenu = CreateMenu();

		InsertMenu(getParentHMENU(handle),getMenuPos(pFrameData->pActionsMap, handle),MF_BYPOSITION | MF_POPUP,(UINT_PTR)handle->hMenu,"");

		updateMenuBar(parent);
	}

	return handle;
};

MenuHandle osInsertMenuItem(ActionHandle action, MenuHandle parent, int pos)
{
	MENUITEMINFO mii;
	MenuHandle handle;
	FrameData *pFrameData;

	CHECK_MENU_TYPE(parent, MENU_SUBMENU | MENU_POPUP, NULL);

	pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

	mii.cbSize = sizeof(mii);
	mii.fMask  = MIIM_ID | MIIM_STATE | MIIM_TYPE;
	mii.wID    = action->id;
	mii.fState = action->enabled ? MFS_ENABLED : MFS_DISABLED;
	mii.fType  = MFT_OWNERDRAW;

	handle = NULL;
	switch (action->type)
	{
		case ACTION_NORMAL:
			handle = newMenuHandle(pFrameData->pActionsMap, parent, pos, MENU_ITEM, action);
			break;
		case ACTION_CHECK:
			handle = newMenuHandle(pFrameData->pActionsMap, parent, pos, MENU_CHECK_ITEM, action);
			break;
		case ACTION_RADIO:
			handle = newMenuHandle(pFrameData->pActionsMap, parent, pos, MENU_RADIO_ITEM, action);
			mii.fType |= MFT_RADIOCHECK;
			break;
		case ACTION_DROPDOWN:
			handle = newMenuHandle(pFrameData->pActionsMap, parent, pos, MENU_SUBMENU, action);
			mii.fMask |= MIIM_SUBMENU;
			mii.hSubMenu = action->menu->hMenu;
			break;
	}

	InsertMenuItem(getParentHMENU(handle),getMenuPos(pFrameData->pActionsMap, handle),TRUE,&mii);

	updateMenuBar(parent);
	return handle;
};


MenuHandle osInsertMenuSeparatorItem(MenuHandle parent, int pos)
{
	MenuHandle handle;
	FrameData *pFrameData;

	CHECK_MENU_TYPE(parent, MENU_SUBMENU | MENU_POPUP, NULL);

	pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	handle = newMenuHandle(pFrameData->pActionsMap, parent, pos, MENU_SEPARATOR, NULL);

	if (handle)
	{
		InsertMenu(getParentHMENU(handle),getMenuPos(pFrameData->pActionsMap, handle),MF_BYPOSITION | MF_SEPARATOR,0,NULL);
		updateMenuBar(parent);
	}

	return handle;
}

void osDestroyMenu(MenuHandle handle)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

	while (handle->children)
		osDestroyMenu(handle->children);

	handleMenuDestroy(handle);

	if (handle->type != MENU_POPUP)
	{
		DeleteMenu(getParentHMENU(handle), getMenuPos(pFrameData->pActionsMap, handle), MF_BYPOSITION);
		updateMenuBar(handle->parent);
	}

	deleteMenuHandle(pFrameData->pActionsMap, handle);
}

int osGetMenuItemCount(MenuHandle handle)
{
	FrameData *pFrameData;

	CHECK_MENU_TYPE(handle, MENU_SUBMENU | MENU_POPUP, 0);

	pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

	return getChildrenCount(pFrameData->pActionsMap, handle);
}

void osSetMenuItemEnabled(MenuHandle handle, BOOL bState)
{
	if (handle->type != MENU_POPUP)
	{
		FrameData *pFrameData;

		CHECK_MENU_TYPE_V(handle, MENU_RADIO_ITEM | MENU_CHECK_ITEM | MENU_ITEM | MENU_SUBMENU);

		pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

		EnableMenuItem(getParentHMENU(handle), getMenuPos(pFrameData->pActionsMap, handle), (bState ? MF_ENABLED : MF_GRAYED) | MF_BYPOSITION);
		updateMenuBar(handle->parent);
	}
};

BOOL osGetMenuItemEnabled(MenuHandle handle)
{
	if (handle->type != MENU_POPUP)
	{
		MENUITEMINFO mii;
		FrameData *pFrameData;

		CHECK_MENU_TYPE(handle, MENU_RADIO_ITEM | MENU_CHECK_ITEM | MENU_ITEM | MENU_SUBMENU, FALSE);

		pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

		mii.cbSize = sizeof(mii);
		mii.fMask = MIIM_STATE;
		GetMenuItemInfo(getParentHMENU(handle), getMenuPos(pFrameData->pActionsMap, handle), TRUE, &mii);
		return (mii.fState & MFS_DISABLED) == 0;
	}
	else
		return TRUE;
};

char *osGetMenuLabel(MenuHandle handle)
{
	if (handle->type != MENU_POPUP)
	{
		int pos;
		HMENU hParent;
		char *s;
		MENUITEMINFO mii;
		FrameData *pFrameData;

		CHECK_MENU_TYPE(handle, MENU_SUBMENU | MENU_RADIO_ITEM | MENU_CHECK_ITEM | MENU_ITEM, NULL);

		pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
		pos = getMenuPos(pFrameData->pActionsMap, handle);
		hParent = getParentHMENU(handle);

		memset(&mii,0,sizeof(mii));
		mii.cbSize     = sizeof(mii);
		mii.fMask      = MIIM_STRING;
		mii.fType      = MFT_STRING;
		mii.dwTypeData = NULL;
		mii.cch        = 0;
		GetMenuItemInfo(hParent, pos, TRUE, &mii);
		mii.cch++;
		mii.dwTypeData = malloc(mii.cch);

		if (mii.dwTypeData)
		{
			GetMenuItemInfo(hParent, pos, TRUE, &mii);

			s = mii.dwTypeData;
			while (*s && *s != '\t') s++;
			*s = 0;
		}

		return mii.dwTypeData;
	}
	else
		return NULL;
}

void osSetMenuLabel(MenuHandle handle, char *title)
{
	if (handle->type != MENU_POPUP)
	{
		MENUITEMINFO mii;
		FrameData *pFrameData;

		CHECK_MENU_TYPE_V(handle, MENU_SUBMENU | MENU_RADIO_ITEM | MENU_CHECK_ITEM | MENU_ITEM);

		pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);

		memset(&mii,0,sizeof(mii));
		mii.cbSize     = sizeof(mii);
		mii.fMask      = MIIM_STRING;
		mii.fType      = MFT_STRING;
		mii.dwTypeData = title;
		mii.cch        = strlen(title);
		SetMenuItemInfo(getParentHMENU(handle), getMenuPos(pFrameData->pActionsMap, handle), TRUE, &mii);

		updateMenuBar(handle->parent);
	}
}

int osGetMenuItemPos(MenuHandle handle)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtr(ghWndFrame,GWLP_USERDATA);
	return getMenuPos(pFrameData->pActionsMap, handle);
}
