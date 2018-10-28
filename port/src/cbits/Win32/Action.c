#include "Action.h"
#include "Window.h"
#include "DockBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

ActionHandle osCreateAction()
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	return newActionHandle(pFrameData->pActionsMap, ACTION_NORMAL);
}

ActionHandle osCreateCheckAction()
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	return newActionHandle(pFrameData->pActionsMap, ACTION_CHECK);
}

ActionHandle osCreateRadioAction()
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	return newActionHandle(pFrameData->pActionsMap, ACTION_RADIO);
}

ActionHandle osCreateDropDownAction(MenuHandle menu)
{
	ActionHandle action;
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	action = newActionHandle(pFrameData->pActionsMap, ACTION_DROPDOWN);
	action->menu = menu;
	return action;
}

void osSetActionRadioGroup(ActionHandle *handles)
{
	ActionHandle *phandle, handle, child;

	if (!handles || *handles == NULL)
		return;

	phandle=handles;
	for (;;)
	{
		handle = *phandle;

		child = handle->nextInGroup;
		while (child->nextInGroup != handle)
			child = child->nextInGroup;
		child->nextInGroup = handle->nextInGroup;

		phandle++;
		if (*phandle)
			handle->nextInGroup = *phandle;
		else
		{
			handle->nextInGroup = *handles;
			break;
		}
	}
}

void osSetActionBitmap(ActionHandle action, BitmapHandle bitmap)
{
	ToolHandle tool;
	FrameData *pFrameData;

	pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	action->bitmap = bitmap;

	tool = action->toolProxies;
	while (tool)
	{
		TBADDBITMAP tbab;
		TBBUTTONINFO tbbi;

		if (bitmap)
		{
			tbab.hInst = NULL;
			tbab.nID   = (UINT_PTR) bitmap->hBitmap;
			tbbi.iImage = SendMessage(tool->hToolBar, TB_ADDBITMAP, 1, (LPARAM) &tbab);
		}
		else
			tbbi.iImage = I_IMAGENONE;

		tbbi.cbSize = sizeof(tbbi);
		tbbi.dwMask = TBIF_IMAGE;
		SendMessage(tool->hToolBar, TB_SETBUTTONINFO, action->id, (LPARAM) &tbbi);

		tool = tool->nextInAction;
	}

	RelayoutFrameBars();
}

void osSetActionEnabled(ActionHandle action, BOOL enabled)
{
	ToolHandle tool;
	MenuHandle menu;
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	action->enabled = enabled;

	tool = action->toolProxies;
	while (tool)
	{
		SendMessage(tool->hToolBar, TB_ENABLEBUTTON, action->id, (LPARAM) enabled);

		tool = tool->nextInAction;
	}

	menu = action->menuProxies;
	while (menu)
	{
		EnableMenuItem(getParentHMENU(menu), getMenuPos(pFrameData->pActionsMap, menu), (enabled ? MF_ENABLED : MF_GRAYED) | MF_BYPOSITION);

		menu = menu->nextInAction;
	}
}

BOOL osGetActionEnabled(ActionHandle action)
{
	return action->enabled;
}

void osSetActionTip(ActionHandle action, char *text)
{
	free(action->tooltip);
	action->tooltip = strdup(text);
}

char *osGetActionTip(ActionHandle action)
{
	return strdup(action->tooltip);
}

void osSetActionText(ActionHandle action, char *text)
{
	free(action->title);
	action->title = strdup(text);
}

char *osGetActionText(ActionHandle action)
{
	return strdup(action->title);
}

void osSetActionShortText(ActionHandle action, char *text)
{
	ToolHandle tool;

	free(action->short_title);
	action->short_title = (text && *text) ? strdup(text) : NULL;

	tool = action->toolProxies;
	while (tool)
	{
		TBBUTTONINFO tbbi;
		tbbi.cbSize = sizeof(tbbi);

		tbbi.dwMask  = TBIF_STYLE;
		tbbi.fsStyle = TBSTYLE_BUTTON;
		SendMessage(tool->hToolBar, TB_GETBUTTONINFO, action->id, (LPARAM) &tbbi);

		tbbi.dwMask  = TBIF_TEXT | TBIF_STYLE;
		tbbi.fsStyle = (text && *text) ? (tbbi.fsStyle | BTNS_SHOWTEXT) : (tbbi.fsStyle & ~BTNS_SHOWTEXT);
		tbbi.pszText = text;
		tbbi.cchText = strlen(text);
		SendMessage(tool->hToolBar, TB_SETBUTTONINFO, action->id, (LPARAM) &tbbi);

		tool = tool->nextInAction;
	}

	RelayoutFrameBars();
}

char *osGetActionShortText(ActionHandle action)
{
	return strdup(action->short_title);
}

void osSetActionChecked(ActionHandle action, BOOL checked)
{
	ToolHandle tool;
	ActionHandle actionInGroup;

	actionInGroup = action;
	do
	{
		actionInGroup->checked = checked;

		tool = actionInGroup->toolProxies;
		while (tool)
		{
			int nState = SendMessage(tool->hToolBar, TB_GETSTATE, actionInGroup->id, 0);

			if (checked)
				nState = nState | TBSTATE_CHECKED;
			else
				nState = nState & ~TBSTATE_CHECKED;

			SendMessage(tool->hToolBar,TB_SETSTATE,actionInGroup->id,MAKELONG(nState, 0));

			tool = tool->nextInAction;
		}


		actionInGroup = actionInGroup->nextInGroup;
		checked = FALSE;
	}
	while (actionInGroup != action);

	handleActionCommand(action);
};

BOOL osGetActionChecked(ActionHandle action)
{
	return action->checked;
};

void osSetActionAccel(ActionHandle action, int key, unsigned int mods)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	updateAccelTable(pFrameData->pActionsMap, action, key, mods);
};

void osGetActionAccel(ActionHandle action, int *key, unsigned int *mods)
{
	*key = action->key;
	*mods = action->keyMods;
};

void osActivateAction(int id)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);
	ActionHandle action = getActionHandle(pFrameData->pActionsMap, id);

	if (action)
	{
		switch (action->type)
		{
		case ACTION_NORMAL:
			handleActionCommand(action);
			break;
		case ACTION_CHECK:
			osSetActionChecked(action, !action->checked);
			break;
		case ACTION_RADIO:
			osSetActionChecked(action, TRUE);
			break;
		case ACTION_DROPDOWN:
			break;
		}
	}
}

void osDestroyAction(ActionHandle action)
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	while (action->toolProxies)
		osDestroyToolItem(action->toolProxies);

	while (action->menuProxies)
		osDestroyMenu(action->menuProxies);

	handleActionDestroy(action);

	deleteActionHandle(pFrameData->pActionsMap, action);
}

void osDestroyAllActions()
{
	FrameData *pFrameData = (FrameData *) GetWindowLongPtrW(ghWndFrame,GWLP_USERDATA);

	for (;;)
	{
		int i;
		ActionHandle action;

		for (i = 0; i < HASH_TABLE_SIZE; i++)
		{
			action = pFrameData->pActionsMap->HashTable[i];
			if (action)
				break;
		}

		if (!action)
			break;


		while (action)
		{
			osDestroyAction(action);
			action = pFrameData->pActionsMap->HashTable[i];
		}
	}
}
