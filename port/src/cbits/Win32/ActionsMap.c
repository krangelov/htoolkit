#include "ActionsMap.h"
#include "Window.h"
#include "Internals.h"
#include "Handlers_stub.h"

ActionsMap *newActionsMap()
{
	ActionsMap *pMap;

	pMap = (ActionsMap *) malloc(sizeof(ActionsMap));
	if (!pMap)
		return NULL;

	pMap->nNextActionID=0;
	pMap->menus        = NULL;
	pMap->popupMenus   = NULL;
	pMap->pFreeList    = NULL;
	pMap->pBlocks      = NULL;
	pMap->nAccelCount  = 0;
	pMap->hAccelTable  = NULL;

	memset(pMap->HashTable, 0, sizeof(ActionHandle)*HASH_TABLE_SIZE);

	return pMap;
}

void deleteActionsMap(ActionsMap *pMap)
{
	Block *p, *pNext;

	pMap->pFreeList = NULL;

	p = pMap->pBlocks;
	while (p != NULL)
	{
		pNext = p->pNext;
		free(p);
		p = pNext;
	}

	if (pMap->hAccelTable)
		DestroyAcceleratorTable(pMap->hAccelTable);

	free(pMap);
}

ActionHandle newActionHandle(ActionsMap *pMap, enum ACTION_TYPE type)
{
	int i, nHash;
	ActionHandle action;

	// it doesn't exist, add a new Association
	if (pMap->pFreeList == NULL)
	{
		// add another block
		Block *p = (Block *) malloc(sizeof(Block) + HASH_BLOCK_SIZE * sizeof(struct ActionHandle));
		if (!p) return NULL;
		p->pNext = pMap->pBlocks;
		pMap->pBlocks = p;  // change head (adds in reverse order for simplicity)

		// chain them into free list
		action = (ActionHandle) (p+1);

		// free in reverse order to make it easier to debug
		action += HASH_BLOCK_SIZE - 1;
		for (i = HASH_BLOCK_SIZE-1; i >= 0; i--, action--)
		{
			action->next = pMap->pFreeList;
			pMap->pFreeList = action;
		}
	}

	action = pMap->pFreeList;
	pMap->pFreeList = action->next;


	action->id = ++pMap->nNextActionID;
	action->type = type;
	action->title = NULL;
	action->short_title = NULL;
	action->tooltip = NULL;
	action->bitmap = NULL;
	action->enabled = TRUE;
	action->checked = FALSE;
	action->key = 0;
	action->keyMods = 0;

	action->menu = NULL;

	action->menuProxies = NULL;
	action->toolProxies = NULL;

	// by default the action is in its own group
	action->nextInGroup = action;

	// put into hash table
	nHash = (action->id >> 4) % HASH_TABLE_SIZE;
	action->next = pMap->HashTable[nHash];
	pMap->HashTable[nHash] = action;

	return action;
}

void deleteActionHandle(ActionsMap *pMap, ActionHandle action)
{
	int nHash;
	ActionHandle *prev;

	nHash = (action->id >> 4) % HASH_TABLE_SIZE;
	prev  = &(pMap->HashTable[nHash]);

	while (*prev)
	{
		if (*prev == action)
		{
			*prev = action->next;
			break;
		}

		prev  = &(*prev)->next;
	}

	action->next = pMap->pFreeList;
	pMap->pFreeList = action;

	free(action->title);
	free(action->short_title);
	free(action->tooltip);
}

MenuHandle newMenuHandle(ActionsMap *pMap, MenuHandle parent, int pos, MENU_TYPE type, ActionHandle action)
{
	MenuHandle handle, *prev;

	handle = malloc(sizeof(struct MenuHandle));

	handle->parent   = parent;
	handle->children = NULL;
	handle->sibling  = NULL;
	handle->type     = type;
	handle->hMenu    = NULL;
	handle->action   = action;

	if (action)
	{
		// link the button to the chain
		handle->nextInAction = action->menuProxies;
		action->menuProxies = handle;
	}

	if (type != MENU_POPUP)
	{
		if (parent)
			prev  = &parent->children;
		else
			prev  = &pMap->menus;
	}
	else
		prev  = &pMap->popupMenus;

	if (pos < 0)
		while (*prev)
			prev  = &(*prev)->sibling;
	else
		while (*prev && pos > 0)
		{
			prev  = &(*prev)->sibling;
			pos--;
		}
	handle->sibling = *prev;
	*prev = handle;

	return handle;
}

void deleteMenuHandle(ActionsMap *pMap, MenuHandle handle)
{
	MenuHandle *prev;

	while (handle->children)
		deleteMenuHandle(pMap, handle->children);

	if (handle->type != MENU_POPUP)
	{
		if (handle->parent)
			prev  = &handle->parent->children;
		else
			prev  = &pMap->menus;
	}
	else
		prev  = &pMap->popupMenus;

	while (*prev != handle)
		prev  = &(*prev)->sibling;
	*prev = handle->sibling;

	if (handle->action)
	{
		prev  = &handle->action->menuProxies;
		while (*prev != handle)
			prev  = &(*prev)->nextInAction;
		*prev = handle->nextInAction;
	}
}

ActionHandle getActionHandle(ActionsMap *pMap, UINT id)
{
	ActionHandle action;
	unsigned int nHash;

	nHash = (id >> 4) % HASH_TABLE_SIZE;

	for (action = pMap->HashTable[nHash]; action != NULL; action = action->next)
	{
		if (action->id == id)
			return action;
	}

	return NULL;
}

void updateAccelTable(ActionsMap *pMap, ActionHandle action, int key, int mods)
{
	if ( action->key && !key) pMap->nAccelCount--;
	if (!action->key &&  key) pMap->nAccelCount++;

	action->key     = key;
	action->keyMods = mods;

	if (pMap->hAccelTable)
	{
		DestroyAcceleratorTable(pMap->hAccelTable);
		pMap->hAccelTable = NULL;
	}
};

HACCEL getAccelTableFromMap(ActionsMap *pMap)
{
	int i,k;
	ActionHandle action;
	ACCEL *pAccel;

	if (pMap->hAccelTable)
		return pMap->hAccelTable;

	if (pMap->nAccelCount == 0)
		return NULL;

	pAccel = malloc(sizeof(ACCEL)*pMap->nAccelCount);
	if (!pAccel)
		return NULL;

	for (k = 0, i = 0; k < pMap->nAccelCount; i++)
	{
		action = pMap->HashTable[i];
		while (action)
		{
			pAccel[k].cmd = action->id;

			if (action->key > 0 && action->key < 256)
			{
				pAccel[k].key   = action->key;
				pAccel[k].fVirt = 0;
				k++;
			}
			else
				if (action->key > 256 && action->key < 512)
				{
					pAccel[k].key   = action->key-256;
					pAccel[k].fVirt = FALT;
					k++;
				}
				else
				{
				 	pAccel[k].fVirt = (((action->keyMods & shiftBIT) ? FSHIFT   : 0) |
				                       ((action->keyMods & ctrlBIT ) ? FCONTROL : 0) |
				            		   ((action->keyMods & altBIT  ) ? FALT     : 0) | FVIRTKEY);

				    switch (action->key)
				    {
				    case kbUp:       pAccel[k++].key = VK_UP;     break;
					case kbDown:     pAccel[k++].key = VK_DOWN;   break;
					case kbLeft:     pAccel[k++].key = VK_LEFT;   break;
					case kbRight:    pAccel[k++].key = VK_RIGHT;  break;
					case kbPgUp:     pAccel[k++].key = VK_PRIOR;  break;
					case kbPgDown:   pAccel[k++].key = VK_NEXT;   break;
					case kbEnd:      pAccel[k++].key = VK_END;    break;
					case kbBegin:    pAccel[k++].key = VK_HOME;   break;
					case kbBackSpace:pAccel[k++].key = VK_BACK;   break;
					case kbDelete:   pAccel[k++].key = VK_DELETE; break;
					case kbEnter:    pAccel[k++].key = VK_RETURN; break;
					case kbEscape:   pAccel[k++].key = VK_ESCAPE; break;
					case kbTab:      pAccel[k++].key = VK_TAB;    break;
					case kbHelp:     pAccel[k++].key = VK_HELP;   break;
					case kbF1:       pAccel[k++].key = VK_F1;     break;
					case kbF2:       pAccel[k++].key = VK_F2;     break;
					case kbF3:       pAccel[k++].key = VK_F3;     break;
					case kbF4:       pAccel[k++].key = VK_F4;     break;
					case kbF5:       pAccel[k++].key = VK_F5;     break;
					case kbF6:       pAccel[k++].key = VK_F6;     break;
					case kbF7:       pAccel[k++].key = VK_F7;     break;
					case kbF8:       pAccel[k++].key = VK_F8;     break;
					case kbF9:       pAccel[k++].key = VK_F9;     break;
					case kbF10:      pAccel[k++].key = VK_F10;    break;
					case kbF11:      pAccel[k++].key = VK_F11;    break;
					case kbF12:      pAccel[k++].key = VK_F12;    break;
					case kbClear:    pAccel[k++].key = VK_CLEAR;  break;
					}
				}

			action = action->next;
		}
	}

	pMap->hAccelTable = CreateAcceleratorTable(pAccel, pMap->nAccelCount);

	free(pAccel);

	return pMap->hAccelTable;
};

int getMenuPos(ActionsMap *pMap, MenuHandle handle)
{
	int pos;
	MenuHandle h;

	if (handle->type != MENU_POPUP)
		h = handle->parent ? handle->parent->children : pMap->menus;
	else
		h = pMap->popupMenus;

	pos = 0;
	while (h && h != handle)
	{
		pos++;
		h = h->sibling;
	}

	return pos;
};

HMENU getParentHMENU(MenuHandle handle)
{
	if (handle->type != MENU_POPUP)
		if (handle->parent == NULL)
			return GetMenu(ghWndFrame);
		else
			return handle->parent->hMenu;
	else
		return NULL;
}

int getChildrenCount(ActionsMap *pMap, MenuHandle handle)
{
	int count;
	MenuHandle child;

	count = 0;
	child = handle ? handle->children : pMap->menus;
	while (child != NULL)
	{
		count++;
		child = child->sibling;
	}

	return count;
}

MenuHandle getNthChild(ActionsMap *pMap, MenuHandle handle, int index)
{
	MenuHandle child;

	child = handle ? handle->children : pMap->menus;
	while (child != NULL && index > 0)
	{
		index--;
		child = child->sibling;
	}

	return child;
}
