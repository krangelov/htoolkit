#include "Menu.h"
#include "Window.h"
#include "Action.h"
#include "Internals.h"
#include "Handlers_stub.h"

static NSMenu *getMenu(MenuHandle parent)
{
	NSMenu *parentMenu;
	if (parent == NULL) {
		parentMenu = [NSApp mainMenu];
	} else {
		parentMenu = parent.submenu;
	}
	return parentMenu;
};

MenuHandle osCreatePopupMenu()
{
	printf("osCreatePopupMenu\n");
	return NULL;
}

void osTrackPopupMenu(MenuHandle handle, WindowHandle hwnd, int x, int y)
{
	printf("osTrackPopupMenu\n");
}

MenuHandle osInsertMenu(MenuHandle parent, int pos)
{
	NSMenu *parentMenu = getMenu(parent);
	NSMenuItem *item = 
		(pos < 0) ? 
			[parentMenu addItemWithTitle: @""
			                      action: NULL
			               keyEquivalent: @""] :
			[parentMenu insertItemWithTitle: @""
			                         action: NULL
			                  keyEquivalent: @""
			                        atIndex: (parentMenu == [NSApp mainMenu]) ? pos+1 : pos] ;

	[parentMenu setSubmenu: [NSMenu new]
                forItem: item];

	return item;
};

MenuHandle osInsertMenuItem(ActionHandle action, MenuHandle parent, int pos)
{
	NSMenu *parentMenu = getMenu(parent);
	NSMenuItem *item =
		(pos < 0) ?
			[parentMenu addItemWithTitle: action.title
			                      action: @selector(menuItemClicked:)
			               keyEquivalent: @""] :
			[parentMenu insertItemWithTitle: action.title
			                         action: @selector(menuItemClicked:)
			                  keyEquivalent: @""
			                        atIndex: (parentMenu == [NSApp mainMenu]) ? pos+1 : pos] ;
	[action addMenuItem: item];
	[item setRepresentedObject: action];

	return item;
};

MenuHandle osInsertMenuSeparatorItem(MenuHandle parent, int pos)
{
	NSMenu *parentMenu = getMenu(parent);

	NSMenuItem *item = [NSMenuItem separatorItem];
    (pos < 0) ? 
			[parentMenu addItem: item] :
			[parentMenu insertItem: item
			               atIndex: pos] ;
    return item;
}

void osDestroyMenu(MenuHandle handle)
{
	printf("osDestroyMenu\n");
}

int osGetMenuItemCount(MenuHandle handle)
{
	NSMenu *parentMenu = getMenu(handle);
	return parentMenu.numberOfItems;
}

void osSetMenuItemEnabled(MenuHandle item, BOOL bState)
{
	[item setEnabled: bState];
};

BOOL osGetMenuItemEnabled(MenuHandle item)
{
	return item.isEnabled;
};

void osSetMenuLabel(MenuHandle item, char* title)
{
	char *src = title;
	char *dst = title;
	while (*src) {
		if (*src == '&') {
			src++;
			if (*src == '&')
				dst++;
		} else {
			*dst++ = *src++;
		}
	}
	*dst++ = 0;

	NSString *nsTitle = [NSString stringWithUTF8String: title];
	[item setTitle: nsTitle];

	NSMenu *submenu = item.submenu;
	if (submenu != NULL) {
		[submenu setTitle: nsTitle];
	}
}

char *osGetMenuLabel(MenuHandle item)
{
	return strdup([item.title UTF8String]);
};

int osGetMenuItemPos(MenuHandle handle)
{
	NSMenu *parentMenu = getMenu(handle);
	return [parentMenu indexOfItem: handle];
}
