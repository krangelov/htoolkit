#include "ToolBar.h"
#include "Action.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include "gtkdropdowntoolbutton.h"

static void tool_item_clicked(GtkWidget *widget, ActionHandle action)
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

static void tool_item_destroy(GtkWidget *tool, ActionHandle action)
{
	handleToolDestroy(tool);
	action->proxies = g_slist_remove(action->proxies, tool);
}

WindowHandle osCreateToolBar(char *name, PositionType place, int band_num, int band_position, int offset)
{
	GtkWidget *toolbar;
	BonoboDockPlacement placement;

	toolbar = gtk_toolbar_new();
	gtk_toolbar_set_show_arrow(GTK_TOOLBAR(toolbar),FALSE);
	gtk_signal_connect (GTK_OBJECT(toolbar), "destroy",
		GTK_SIGNAL_FUNC(handleWindowDestroy),
		NULL);

	switch (place)
	{
	case PosLeft:   placement = BONOBO_DOCK_LEFT;   break;
	case PosTop:    placement = BONOBO_DOCK_TOP;    break;
	case PosRight:  placement = BONOBO_DOCK_RIGHT;  break;
	case PosBottom: placement = BONOBO_DOCK_BOTTOM; break;
	default: return NULL;
	}

	gnome_app_add_toolbar(GNOME_APP(gFrameWidget),
                                             GTK_TOOLBAR(toolbar),
                                             name,
                                             BONOBO_DOCK_ITEM_BEH_NORMAL,
                                             placement,
                                             band_num,
                                             band_position,
                                             offset);

	return toolbar;
}

void osDestroyToolBar(GtkWidget *toolbar)
{
	gtk_widget_destroy(toolbar);
}

int osGetToolBarButtonCount(WindowHandle toolbar)
{
	return gtk_toolbar_get_n_items(GTK_TOOLBAR(toolbar));
}

ToolHandle osInsertToolButton(ActionHandle action, WindowHandle toolbar, int pos)
{
	GtkToolItem *item;
	GtkWidget *image;
	char *text;

	image = gtk_image_new();
	if (action->bitmap)
		gtk_image_set_from_pixbuf(GTK_IMAGE(image), action->bitmap->pixbuf);

	switch (action->type)
	{
	case ACTION_NORMAL:
		item = gtk_tool_button_new(image, "");
		break;
	case ACTION_CHECK:
	case ACTION_RADIO:
		item = gtk_toggle_tool_button_new();
		gtk_tool_button_set_icon_widget(GTK_TOOL_BUTTON(item), image);
		gtk_toggle_tool_button_set_active(GTK_TOGGLE_TOOL_BUTTON(item), action->checked);
		break;
	case ACTION_DROPDOWN:
		item = gtk_drop_down_tool_button_new(image, "", NULL);
		if (action->menu)
			gtk_drop_down_tool_button_set_menu(GTK_DROP_DOWN_TOOL_BUTTON(item), action->menu);
		break;
	}

	gtk_widget_set_sensitive(GTK_WIDGET(item),action->enabled);

	gtk_tool_button_set_use_underline (GTK_TOOL_BUTTON(item), TRUE);
	text = toMnemonicString(action->short_title ? action->short_title : action->title ? action->title : "");
	gtk_tool_button_set_label(GTK_TOOL_BUTTON(item), text);
	rfree(text);

	if (action->tooltip)
		gtk_tool_item_set_tooltip(GTK_TOOL_ITEM(item), GTK_TOOLBAR(toolbar)->tooltips, action->tooltip, NULL);

	if (action->type != ACTION_DROPDOWN)
	{
		gtk_signal_connect (GTK_OBJECT(item), "clicked",
			GTK_SIGNAL_FUNC(tool_item_clicked),
			action);
	}

	gtk_signal_connect (GTK_OBJECT(item), "destroy",
		GTK_SIGNAL_FUNC(tool_item_destroy),
		action);

	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), item, pos);
	gtk_widget_show_all(GTK_WIDGET(item));

	// add the button to the list of all menu items associated to this action
	action->proxies = g_slist_append(action->proxies, item);

	return GTK_WIDGET(item);
}

ToolHandle osInsertToolLine(WindowHandle toolbar, int pos)
{
	GtkToolItem *item = gtk_separator_tool_item_new();
	gtk_signal_connect (GTK_OBJECT(item), "destroy",
		GTK_SIGNAL_FUNC(handleToolDestroy),
		NULL);
	gtk_toolbar_insert(GTK_TOOLBAR(toolbar), item, pos);
	gtk_widget_show_all(GTK_WIDGET(item));
	return GTK_WIDGET(item);
}

void osDestroyToolItem(ToolHandle toolItem)
{
	gtk_widget_destroy(toolItem);
}

int osGetToolItemPos(ToolHandle toolItem)
{
	GtkWidget *toolbar = gtk_widget_get_parent(toolItem);
	return gtk_toolbar_get_item_index(GTK_TOOLBAR(toolbar), GTK_TOOL_ITEM(toolItem));
}
