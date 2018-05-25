#include "Action.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include "Window.h"
#include <gdk/gdkkeysyms.h>

// The pointer to the first live action. All actions are linked together using the "next_action" field
static ActionHandle first_action = NULL;

static ActionHandle create_action(enum ActionType type)
{
	ActionHandle action = malloc(sizeof(struct ActionHandle));

	action->type = type;
	action->title = NULL;
	action->short_title = NULL;
	action->tooltip = NULL;
	action->bitmap = NULL;
	action->enabled = TRUE;
	action->checked = FALSE;
	action->busy = FALSE;
	action->key = 0;
	action->keyMods = 0;

	action->menu = NULL;

	action->proxies = NULL;

	// link the action
	action->next_action = first_action;
	first_action = action;

	// by default the action is in its own group
	action->next_action_in_group = action;

	return action;
}

static gboolean find_accel_by_widget(GtkAccelKey *key, GClosure *closure, gpointer data)
{
	return (closure->data == data);
}

void menu_item_set_accel(GtkWidget *item, ActionHandle action)
{
	int key;
	GdkModifierType modifier;
	GtkAccelKey *accel_key;
	GtkAccelGroup *accel_group = (GtkAccelGroup *) gtk_accel_groups_from_object (G_OBJECT(gFrameWidget))->data;

	accel_key = gtk_accel_group_find(accel_group, find_accel_by_widget, item);
	if (accel_key) gtk_widget_remove_accelerator(item, accel_group, accel_key->accel_key, accel_key->accel_mods);

	if (action->key == 0)
		return;

	modifier = 0;
	if (action->keyMods & shiftBIT) modifier |= GDK_SHIFT_MASK;
	if (action->keyMods & ctrlBIT)  modifier |= GDK_CONTROL_MASK;
	if (action->keyMods & altBIT)   modifier |= GDK_MOD1_MASK;

	switch (action->key)
	{
	case kbBackSpace: key = GDK_BackSpace; break;
	case kbTab:       key = GDK_Tab;       break;
	case kbEnter:     key = GDK_Return;    break;
	case kbEscape:    key = GDK_Escape;    break;
	case kbBegin:     key = GDK_Begin;     break;
	case kbClear:	  key = GDK_Clear;     break;
	case kbDelete:    key = GDK_Delete;    break;
	case kbDown:      key = GDK_Down;      break;
	case kbEnd:       key = GDK_End;       break;
	case kbF1:        key = GDK_F1;        break;
	case kbF2:        key = GDK_F2;        break;
	case kbF3:        key = GDK_F3;        break;
	case kbF4:        key = GDK_F4;        break;
	case kbF5:        key = GDK_F5;        break;
	case kbF6:        key = GDK_F6;        break;
	case kbF7:        key = GDK_F7;        break;
	case kbF8:        key = GDK_F8;        break;
	case kbF9:        key = GDK_F9;        break;
	case kbF10:       key = GDK_F10;       break;
	case kbF11:       key = GDK_F11;       break;
	case kbF12:       key = GDK_F12;       break;
	case kbF13:       key = GDK_F13;       break;
	case kbF14:       key = GDK_F14;       break;
	case kbF15:       key = GDK_F15;       break;
	case kbHelp:      key = GDK_Help;      break;
	case kbLeft:      key = GDK_Left;      break;
	case kbPgDown:    key = GDK_Page_Down; break;
	case kbPgUp:      key = GDK_Page_Up;   break;
	case kbRight:     key = GDK_Right;     break;
	case kbUp:        key = GDK_Up;        break;
	default:
		if (action->key >= 256)
		{
			key = action->key - 256;
			modifier = GDK_MOD1_MASK;
		}

	  	if (iscntrl(action->key))
		{
			key = action->key + '@';
			modifier |= GDK_CONTROL_MASK;
		}
		else
			if (isupper(action->key))
				modifier |= GDK_SHIFT_MASK;
			else
				key = toupper(action->key);
		break;
	}

	gtk_widget_add_accelerator(item, "activate",
				   accel_group,
				   key,
				   modifier,
				   GTK_ACCEL_VISIBLE);
};

ActionHandle osCreateAction()
{
	return create_action(ACTION_NORMAL);
}

ActionHandle osCreateCheckAction()
{
	return create_action(ACTION_CHECK);
}

ActionHandle osCreateRadioAction()
{
	return create_action(ACTION_RADIO);
}

ActionHandle osCreateDropDownAction(MenuHandle menu)
{
	ActionHandle action = create_action(ACTION_DROPDOWN);
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

		child = handle->next_action_in_group;
		while (child->next_action_in_group != handle)
			child = child->next_action_in_group;
		child->next_action_in_group = handle->next_action_in_group;

		phandle++;
		if (*phandle)
			handle->next_action_in_group = *phandle;
		else
		{
			handle->next_action_in_group = *handles;
			break;
		}
	}
}

void osSetActionBitmap(ActionHandle action, BitmapHandle bitmap)
{
	GSList *list;
	GtkWidget *proxy, *image;

	action->bitmap = bitmap;
	for (list = action->proxies; list != NULL; list=list->next)
	{
		proxy = (GtkWidget *) list->data;
		if (GTK_IS_IMAGE_MENU_ITEM(proxy))
		{
			image = gtk_image_menu_item_get_image(GTK_IMAGE_MENU_ITEM(proxy));

			if (action->bitmap)
			{
				if (!image)
				{
					image = gtk_image_new_from_pixbuf(action->bitmap->pixbuf);
					gtk_widget_show(image);
					gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(proxy), image);
				}
				else
					gtk_image_set_from_pixbuf(GTK_IMAGE(image),action->bitmap->pixbuf);
			}
			else
			{
				if (image)
				{
					gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(proxy), NULL);
				}
			}
		}
		else
			if (GTK_IS_TOOL_BUTTON(proxy))
			{
				image = gtk_tool_button_get_icon_widget (GTK_TOOL_BUTTON(proxy));
				gtk_image_set_from_pixbuf(GTK_IMAGE(image), action->bitmap->pixbuf);
			}
	}
}

void osSetActionEnabled(ActionHandle action, BOOL enabled)
{
	GSList *list;
	GtkWidget *proxy;

	action->enabled = enabled;
	for (list = action->proxies; list != NULL; list=list->next)
	{
		proxy = (GtkWidget *) list->data;
		gtk_widget_set_sensitive(proxy,action->enabled);
	}
}

BOOL osGetActionEnabled(ActionHandle action)
{
	return action->enabled;
}

void osSetActionTip(ActionHandle action, char *text)
{
	GSList *list;
	GtkWidget *proxy;

	free(action->tooltip);
	action->tooltip = strdup(text);

	for (list = action->proxies; list != NULL; list=list->next)
	{
		proxy = (GtkWidget *) list->data;

		if (GTK_IS_TOOL_BUTTON(proxy))
		{
			GtkWidget *toolbar = gtk_widget_get_parent(proxy);
			gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(proxy), GTK_TOOLBAR(toolbar)->tooltips, text, NULL);
		}
	}
}

char *osGetActionTip(ActionHandle action)
{
	return strdup(action->tooltip);
}

void osSetActionText(ActionHandle action, char *text)
{
	GSList *list;
	GtkWidget *proxy;
	char *short_text;

	free(action->title);
	action->title = strdup(text);

	text = toMnemonicString(action->title);
	short_text = toMnemonicString(action->short_title);
	for (list = action->proxies; list != NULL; list=list->next)
	{
		proxy = (GtkWidget *) list->data;

		if (GTK_IS_MENU_ITEM(proxy))
			gtk_label_set_text_with_mnemonic(GTK_LABEL(GTK_BIN(proxy)->child), text);
		else
			if (GTK_IS_TOOL_BUTTON(proxy))
				gtk_tool_button_set_label(GTK_TOOL_BUTTON(proxy), short_text ? short_text : text);
	}
	rfree(short_text);
	rfree(text);
}

char *osGetActionText(ActionHandle action)
{
	return strdup(action->title);
}

void osSetActionShortText(ActionHandle action, char *text)
{
	GSList *list;
	GtkWidget *proxy;

	free(action->short_title);
	action->short_title = (text && *text) ? strdup(text) : NULL;

	text = toMnemonicString(action->short_title ? action->short_title : action->title);
	for (list = action->proxies; list != NULL; list=list->next)
	{
		proxy = (GtkWidget *) list->data;

		if (GTK_IS_TOOL_BUTTON(proxy))
			gtk_tool_button_set_label(GTK_TOOL_BUTTON(proxy), text);
	}
	rfree(text);
}

char *osGetActionShortText(ActionHandle action)
{
	return strdup(action->short_title);
}

void osSetActionChecked(ActionHandle action, BOOL checked)
{
	GSList *list;
	GtkWidget *proxy;
	ActionHandle action_in_group;

	action_in_group = action;
	do
	{
		action_in_group->busy  = TRUE;
		action_in_group->checked = checked;

		for (list = action_in_group->proxies; list != NULL; list=list->next)
		{
			proxy = (GtkWidget *) list->data;

			if (GTK_IS_CHECK_MENU_ITEM(proxy))
				gtk_check_menu_item_set_active (
					GTK_CHECK_MENU_ITEM (proxy),
					action_in_group->checked);
			else
				if (GTK_IS_TOGGLE_TOOL_BUTTON(proxy))
					gtk_toggle_tool_button_set_active  (
						GTK_TOGGLE_TOOL_BUTTON(proxy),
						action_in_group->checked);
		}

		action_in_group->busy = FALSE;

		action_in_group = action_in_group->next_action_in_group;
		checked = FALSE;
	}
	while (action_in_group != action);

	handleActionCommand(action);
};

BOOL osGetActionChecked(ActionHandle action)
{
	return action->checked;
};

void osSetActionAccel(ActionHandle action, int key, unsigned int mods)
{
	GSList *list;
	GtkWidget *proxy;

	action->key = key;
	action->keyMods = mods;

	for (list = action->proxies; list != NULL; list=list->next)
	{
		proxy = (GtkWidget *) list->data;

		if (GTK_IS_MENU_ITEM(proxy))
			menu_item_set_accel(proxy, action);
	}
};

void osGetActionAccel(ActionHandle action, int *key, unsigned int *mods)
{
	*key = action->key;
	*mods = action->keyMods;
};

void osDestroyAction(ActionHandle action)
{
	ActionHandle act;

	if (action->menu)
		gtk_widget_destroy(action->menu);

	// destroy all proxies associated with the action
	while (action->proxies)
		gtk_widget_destroy((GtkWidget *) action->proxies->data);

	handleActionDestroy(action);

	if (first_action == action)
		first_action = action->next_action;
	else
	{
		for (act = first_action; act != NULL; act=act->next_action)
		{
			if (act->next_action == action)
			{
				act->next_action = action->next_action;
				break;
			}
		}
	}

	free(action->title);
	free(action->short_title);
	free(action->tooltip);
	free(action);
}

void osDestroyAllActions()
{
	while (first_action)
		osDestroyAction(first_action);
}
