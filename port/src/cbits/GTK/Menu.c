#include "Menu.h"
#include "Window.h"
#include "Action.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include <gdk/gdkkeysyms.h>

static const char *apphelper_statusbar_hint = "apphelper_statusbar_hint";

static void menu_destroy(GtkWidget *menu)
{
	handleMenuDestroy(menu);

	gchar *hint = g_object_get_data(G_OBJECT(menu), apphelper_statusbar_hint);
	if (hint) free(hint);

	g_object_set_data(G_OBJECT(menu), apphelper_statusbar_hint, NULL);
}

static void menu_item_put_hint_in_statusbar(GtkWidget *menuitem, ActionHandle action)
{
	if (action && action->tooltip)
	{
		if (GNOME_APP(gFrameWidget)->statusbar)
			gnome_appbar_push(GNOME_APPBAR(GNOME_APP(gFrameWidget)->statusbar), action->tooltip);
	}
}

static void menu_item_remove_hint_from_statusbar(GtkWidget *menu, ActionHandle action)
{
	if (action && action->tooltip)
	{
		if (GNOME_APP(gFrameWidget)->statusbar)
			gnome_appbar_pop(GNOME_APPBAR(GNOME_APP(gFrameWidget)->statusbar));
	}
}

static void menu_item_activate(GtkMenuItem *menuitem, ActionHandle action)
{
	if (action->busy)
		return;

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

static void menu_item_destroy(GtkWidget *menu, ActionHandle action)
{
	handleMenuDestroy(menu);
	action->proxies = g_slist_remove(action->proxies, menu);
}

static MenuHandle getMenu(MenuHandle parent)
{
	if (!parent)
	{
		if (!gMenuBar)
		{
			gMenuBar = gtk_menu_bar_new();
			gtk_widget_show(gMenuBar);
			gnome_app_set_menus(GNOME_APP(gFrameWidget), GTK_MENU_BAR(gMenuBar));
		}

		parent = gMenuBar;
	}
	else
		if (GTK_IS_MENU_ITEM(parent))
			parent = gtk_menu_item_get_submenu(GTK_MENU_ITEM(parent));

	return parent;
};

static int recalcPos(GtkWidget *menu, int pos)
{
	int new_pos;
	GList *children;

	children = GTK_MENU_SHELL(menu)->children;

	if (pos >= 0)
	{
		new_pos = 0;
		while (children && pos > 0)
		{
			pos--;
			new_pos++;
			children = children->next;
		}

		if (!children)
			new_pos = (gDocumentInterface == 1 || menu != gMenuBar) ? -1 : new_pos-1;
	}
	else
	{
		new_pos = (gDocumentInterface == 1 || menu != gMenuBar) ? -1 : g_list_length(children)-1;
	}

	return new_pos;
}

MenuHandle osCreatePopupMenu()
{
	GtkWidget *popUpMenu = gtk_menu_new();
	gtk_signal_connect (GTK_OBJECT(popUpMenu), "destroy",
		GTK_SIGNAL_FUNC(menu_destroy),
		NULL);
	return popUpMenu;
}

struct PositioningParams
{
	int x, y;
	GtkWidget *widget;
};

static void menu_positioning(GtkMenu *menu, gint *x, gint *y, gboolean *push_in, gpointer user_data)
{
	struct PositioningParams *params = (struct PositioningParams *) user_data;

	gdk_window_get_origin(params->widget->window, x, y);
	*x += params->x;
	*y += params->y;

	*push_in = TRUE;
	free(params);
}

void osTrackPopupMenu(MenuHandle handle, WindowHandle hwnd, int x, int y)
{
	struct PositioningParams *params = malloc(2*sizeof(struct PositioningParams));
	if (params)
	{
		params->x = x;
		params->y = y;
		params->widget = hwnd;
		gtk_menu_popup(GTK_MENU(handle), NULL, NULL, menu_positioning, params, 0, gtk_get_current_event_time());
	}
}

MenuHandle osInsertMenu(MenuHandle hmenu, int pos)
{
	GtkWidget *menu;
	GtkWidget *menuItem = gtk_menu_item_new_with_mnemonic("");
	GtkWidget *popUpMenu = gtk_menu_new();
	gtk_menu_item_set_submenu(GTK_MENU_ITEM(menuItem), popUpMenu);

	gtk_signal_connect (GTK_OBJECT(menuItem), "destroy",
		GTK_SIGNAL_FUNC(menu_destroy),
		NULL);

	menu = getMenu(hmenu);
	pos = recalcPos(menu, pos);

	gtk_menu_shell_insert(GTK_MENU_SHELL(menu), menuItem, pos);
	gtk_widget_show_all(menuItem);
	return menuItem;
};

MenuHandle osInsertMenuItem(ActionHandle action, MenuHandle parent, int pos)
{
	GtkWidget *menu, *popUpMenu;
	GtkWidget *label, *menuItem;
	gchar *text;

	switch (action->type)
	{
	case ACTION_NORMAL:
		menuItem = gtk_image_menu_item_new();
		if (action->bitmap)
			gtk_image_menu_item_set_image(GTK_IMAGE_MENU_ITEM(menuItem),
			                              gtk_image_new_from_pixbuf(action->bitmap->pixbuf));
		break;
	case ACTION_CHECK:
		menuItem = gtk_check_menu_item_new();
		gtk_check_menu_item_set_active  (GTK_CHECK_MENU_ITEM(menuItem), action->checked);
		break;
	case ACTION_RADIO:
		menuItem = gtk_check_menu_item_new();
		gtk_check_menu_item_set_draw_as_radio(GTK_CHECK_MENU_ITEM(menuItem), TRUE);
		gtk_check_menu_item_set_active  (GTK_CHECK_MENU_ITEM(menuItem), action->checked);
		break;
	case ACTION_DROPDOWN:
		text = toMnemonicString(action->title ? action->title : "");
		menuItem = gtk_menu_item_new_with_mnemonic(text);
		rfree(text);
		if (action->menu)
			gtk_menu_item_set_submenu(GTK_MENU_ITEM(menuItem), action->menu);
		break;
	}

	gtk_widget_set_sensitive(menuItem,action->enabled);

	if (action->type != ACTION_DROPDOWN)
	{
		label = gtk_accel_label_new("");
		text = toMnemonicString(action->title ? action->title : "");
		gtk_label_set_text_with_mnemonic(GTK_LABEL(label)	, text);
		rfree(text);
		gtk_container_add(GTK_CONTAINER(menuItem), label);
		gtk_accel_label_set_accel_widget(GTK_ACCEL_LABEL(label), menuItem);

		menu_item_set_accel(menuItem, action);

		gtk_signal_connect (GTK_OBJECT(menuItem), "activate",
			GTK_SIGNAL_FUNC(menu_item_activate),
			action);
	}

	gtk_signal_connect (GTK_OBJECT(menuItem), "destroy",
			GTK_SIGNAL_FUNC(menu_item_destroy),
			action);
	gtk_signal_connect (GTK_OBJECT(menuItem), "select",
 			GTK_SIGNAL_FUNC(menu_item_put_hint_in_statusbar),
 			action);
	gtk_signal_connect (GTK_OBJECT(menuItem), "deselect",
 			GTK_SIGNAL_FUNC(menu_item_remove_hint_from_statusbar),
 			action);

	menu = getMenu(parent);
	pos = recalcPos(menu, pos);

	gtk_menu_shell_insert(GTK_MENU_SHELL(menu), menuItem, pos);
	gtk_widget_show_all(menuItem);

	// add the item to the list of all menu items associated to this action
	action->proxies = g_slist_append(action->proxies, menuItem);

	return menuItem;
};

MenuHandle osInsertMenuSeparatorItem(MenuHandle parent, int pos)
{
	GtkWidget *menu;
	GtkWidget *item =  gtk_separator_menu_item_new();
	gtk_signal_connect (GTK_OBJECT(item), "destroy",
			GTK_SIGNAL_FUNC(handleMenuDestroy),
			NULL);

	menu = getMenu(parent);
	pos = recalcPos(menu, pos);

	gtk_menu_shell_insert(GTK_MENU_SHELL(getMenu(parent)), item, pos);
	gtk_widget_show(item);
	return item;
}

void osDestroyMenu(MenuHandle handle)
{
	gtk_widget_destroy(handle);
}

int osGetMenuItemCount(MenuHandle handle)
{
	int count;
	GList *children;

	count = 0;
	children = GTK_MENU_SHELL(getMenu(handle))->children;
	while (children)
	{
		count++;
		children = children->next;
	}

	return count;
}

void osSetMenuItemEnabled(MenuHandle item, BOOL bState)
{
	if (GTK_IS_MENU_ITEM(item))
		gtk_widget_set_sensitive(item,bState);
};

BOOL osGetMenuItemEnabled(MenuHandle item)
{
	if (GTK_IS_MENU_ITEM(item))
		return GTK_WIDGET_SENSITIVE(item);
	else
		return TRUE;
};

void osSetMenuLabel(MenuHandle item, char* title)
{
	if (GTK_IS_MENU_ITEM(item))
	{
		gchar *szText = toMnemonicString(title);
		gtk_label_set_text_with_mnemonic(GTK_LABEL(GTK_BIN(item)->child), szText);
		rfree(szText);
	}
}

char *osGetMenuLabel(MenuHandle item)
{
	if (GTK_IS_MENU_ITEM(item))
		return fromMnemonicString(gtk_label_get_text(GTK_LABEL(GTK_BIN(item)->child)));
	else
		return NULL;
};

int osGetMenuItemPos(MenuHandle handle)
{
	int pos;
	GList *children;

	pos = 0;
	children = GTK_MENU_SHELL(getMenu(handle))->children;
	while (children && children->data != handle)
	{
		pos++;
		children = children->next;
	}

	return (children ? pos : -1);
}
