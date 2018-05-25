#include "Action.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include "Window.h"

// The pointer to the first live action. All actions are linked together using the "next_action" field
static ActionHandle first_action = NULL;

@implementation NSActionHandle

- (id)init: (ActionType) actionType {
	if (self = [super init]) {
		_type = actionType;
		_title = @"";
		_short_title = NULL;
		_tooltip = NULL;
		_bitmap = NULL;
		_enabled = true;
		_checked = false;
		_menuItems = NULL;

		_menu = NULL;

		// link the action
		_next_action = first_action;
		first_action = self;

		// by default the action is in its own group
		_next_action_in_group = self;
        return self;
    } else
        return nil;
}

- (void)setTitle: (NSString*) title {
	[title retain];
	_title = title;
	if (_menuItems != NULL) {
		for (int i = 0; i < _menuItems.count; i++) {
			NSMenuItem *item = [_menuItems objectAtIndex: i];
			item.title = title;
		}
	}
}

- (void)setChecked: (BOOL) checked {
	NSActionHandle *action = self;
	do {
		action->_checked = (action == self) ? checked : false;
		if (action->_menuItems != NULL) {
			for (int i = 0; i < action->_menuItems.count; i++) {
				NSMenuItem *item = [action->_menuItems objectAtIndex: i];
				item.state = action->_checked ? NSOnState : NSOffState;
			}
		}

		action = action->_next_action_in_group;
	} while (action != self);
}

- (void)addMenuItem: (NSMenuItem *) item {
	if (_menuItems == NULL) {
		_menuItems = [[NSMutableArray alloc] init];
	}
	[_menuItems addObject: item];
}

+ (void) setActionRadioGroup: (ActionHandle *) handles {
	ActionHandle *phandle, handle, child;

	if (!handles || *handles == NULL)
		return;

	phandle=handles;
	for (;;)
	{
		handle = *phandle;

		child = handle->_next_action_in_group;
		while (child->_next_action_in_group != handle)
			child = child->_next_action_in_group;
		child->_next_action_in_group = handle->_next_action_in_group;

		phandle++;
		if (*phandle)
			handle->_next_action_in_group = *phandle;
		else
		{
			handle->_next_action_in_group = *handles;
			break;
		}
	}
}

- (void)dealloc {
	handleActionDestroy(self);

	if (_menuItems != NULL) {
		[_menuItems release]; _menuItems = NULL;
	}

	if (_title != NULL) {
		[_title release]; _title = NULL;
	}

	if (_short_title != NULL) {
		[_short_title release]; _short_title = NULL;
	}

	if (_tooltip != NULL) {
		[_tooltip release]; _tooltip = NULL;
	}

	if (_bitmap != NULL) {
		[_bitmap release]; _bitmap = NULL;
	}

	if (_menu != NULL) {
		[_menu release]; _menu = NULL;
	}

	ActionHandle child = _next_action_in_group;
	while (child->_next_action_in_group != self)
		child = child->_next_action_in_group;
	child->_next_action_in_group = _next_action_in_group;

	[super dealloc];
}

@end

ActionHandle osCreateAction()
{
	return [[NSActionHandle alloc] init: ACTION_NORMAL];
}

ActionHandle osCreateCheckAction()
{
	return [[NSActionHandle alloc] init: ACTION_CHECK];
}

ActionHandle osCreateRadioAction()
{
	return [[NSActionHandle alloc] init: ACTION_RADIO];
}

ActionHandle osCreateDropDownAction(MenuHandle menu)
{
	ActionHandle action = [[NSActionHandle alloc] init: ACTION_DROPDOWN];
	action.menu = menu;
	return action;
}

void osSetActionRadioGroup(ActionHandle *handles)
{
	[NSActionHandle setActionRadioGroup: handles];
}

void osSetActionBitmap(ActionHandle action, BitmapHandle bitmap)
{
	action.bitmap = bitmap;
}

void osSetActionEnabled(ActionHandle action, BOOL enabled)
{
	action.enabled = enabled;
}

BOOL osGetActionEnabled(ActionHandle action)
{
	return action.enabled;
}

void osSetActionTip(ActionHandle action, char *text)
{
	action.tooltip = [NSString stringWithUTF8String: text];
}

char *osGetActionTip(ActionHandle action)
{
	return strdup([action.tooltip UTF8String]);
}

void osSetActionText(ActionHandle action, char *text)
{
	char *src = text;
	char *dst = text;
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

	action.title = [NSString stringWithUTF8String: text];
}

char *osGetActionText(ActionHandle action)
{
	return strdup([action.title UTF8String]);
}

void osSetActionShortText(ActionHandle action, char *text)
{
	action.short_title = [NSString stringWithUTF8String: text];
}

char *osGetActionShortText(ActionHandle action)
{
	return strdup([action.short_title UTF8String]);
}

void osSetActionChecked(ActionHandle action, BOOL checked)
{
	action.checked = checked;
};

BOOL osGetActionChecked(ActionHandle action)
{
	return action.checked;
};

void osSetActionAccel(ActionHandle action, int key, unsigned int mods)
{
	printf("osSetActionAccel\n");
};

void osGetActionAccel(ActionHandle action, int *key, unsigned int *mods)
{
	printf("osGetActionAccel\n");
};

void osDestroyAction(ActionHandle action)
{
	printf("osDestroyAction\n");
}

void osDestroyAllActions()
{
	printf("osDestroyAllActions\n");
}
