#ifndef MENU_ITEMS_MAP_H
#define MENU_ITEMS_MAP_H

#include "Types.h"

#define HASH_BLOCK_SIZE 10
#define HASH_TABLE_SIZE 17

enum MENU_TYPE
	{ MENU_POPUP       = 1
	, MENU_SUBMENU     = 2
	, MENU_SEPARATOR   = 4
	, MENU_ITEM        = 8
	, MENU_CHECK_ITEM  = 16
	, MENU_RADIO_ITEM  = 32
	};

typedef enum MENU_TYPE MENU_TYPE;

enum ACTION_TYPE { ACTION_NORMAL, ACTION_CHECK, ACTION_RADIO, ACTION_DROPDOWN };

struct ActionHandle
{
	// Unique identity
	UINT id;

	// chain;
	ActionHandle next;				// next action in the hash table
	ActionHandle nextInGroup;		// next action in the radio group

	// Attributes
	enum ACTION_TYPE type;			// the action type
	char *title;
	char *short_title;
	char *tooltip;
	BitmapHandle bitmap;
	BOOL enabled : 1;         // TRUE if the action is enabled
	BOOL checked : 1;         // TRUE if the action is checked

	MenuHandle menu;

	// Accelerator
	int key, keyMods;

	// the collection of all proxies (menu items or toolbar buttons) associated to this action
	MenuHandle menuProxies;
	ToolHandle toolProxies;
};

struct ToolHandle
{
	HWND hToolBar;				// toolbar handle
	ToolHandle nextInAction;	// the link to the next tool with the same action
	ActionHandle action;		// the associated action
};

struct MenuHandle
{
	struct MenuHandle *parent;
	struct MenuHandle *children;
	struct MenuHandle *sibling;

	HMENU hMenu;
	MENU_TYPE type;

	MenuHandle nextInAction;	// the link to the next menu item with the same action
	ActionHandle action;		// the associated action
};

typedef struct Block_tag
{
	struct Block_tag *pNext;
} Block;

typedef struct
{
	int nNextActionID;

	struct MenuHandle *menus;
	struct MenuHandle *popupMenus;

	Block* pBlocks;
	ActionHandle pFreeList;
	ActionHandle HashTable[HASH_TABLE_SIZE];
	HACCEL hAccelTable;
	int nAccelCount;
} ActionsMap;

ActionsMap *newActionsMap();
void deleteActionsMap(ActionsMap *pMap);
ActionHandle newActionHandle(ActionsMap *pMap, enum ACTION_TYPE type);
void deleteActionHandle(ActionsMap *pMap, ActionHandle handle);
ActionHandle getActionHandle(ActionsMap *pMap, UINT id);
void updateAccelTable(ActionsMap *pMap, ActionHandle item, int key, int mods);
HACCEL getAccelTableFromMap(ActionsMap *pMap);

MenuHandle newMenuHandle(ActionsMap *pMap, MenuHandle parent, int pos, MENU_TYPE type, ActionHandle action);
void deleteMenuHandle(ActionsMap *pMap, MenuHandle handle);
int getMenuPos(ActionsMap *pMap, MenuHandle handle);
HMENU getParentHMENU(MenuHandle handle);
int getChildrenCount(ActionsMap *pMap, MenuHandle handle);
MenuHandle getNthChild(ActionsMap *pMap, MenuHandle handle, int index);

#endif
