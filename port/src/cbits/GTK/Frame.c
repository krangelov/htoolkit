#include "Types.h"
#include "Internals.h"
#include "Window.h"
#include "ConfigKey.h"
#include "Handlers_stub.h"
#include <gdk/gdkkeysyms.h>

char *gAppTitle = NULL;
char *gAppName = NULL;
char *gAppVersion = NULL;
GtkWidget *gFrameWidget = NULL;
GtkWidget *gClientWidget = NULL;
GtkWidget *gMenuBar = NULL;
int gFrameWidth = 0, gFrameHeight = 0;

static void frame_close_page_handler(GtkWidget *client)
{
	GtkWidget *window = gtk_notebook_get_nth_page(GTK_NOTEBOOK(client),  gtk_notebook_get_current_page(GTK_NOTEBOOK(client)));
	if (window) handleWindowDismiss(window);
}

static void frame_notebook_top_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_TOP);
}

static void frame_notebook_bottom_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_BOTTOM);
}

static void frame_notebook_left_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_LEFT);
}

static void frame_notebook_right_handler(GtkWidget *client)
{
	gtk_notebook_set_tab_pos(GTK_NOTEBOOK(client), GTK_POS_RIGHT);
}

static void frame_switch_page_handler(GtkNotebook *notebook,GtkNotebookPage *page,gint page_num,gpointer user_data)
{
	// send deactivate message for old
	gint old_page_num = g_list_index(notebook->children, notebook->cur_page);
	handleWindowDeactivate(gtk_notebook_get_nth_page(notebook, old_page_num));

	// send activate message for new
	handleWindowActivate(gtk_notebook_get_nth_page(notebook, page_num));
}

static gboolean frame_delete_handler(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
	if (widget->window)
	{
		char *keyName = strdup("HToolkit.FrameState");
		osSetConfigIntKey(keyName, (int) gdk_window_get_state(widget->window));
		free(keyName);
	}

	handleProcessDismiss();
	return gtk_true();
}

static void frame_destroy_handler(GtkWidget *widget, GdkEvent *event, gpointer user_data)
{
	if (gDocumentInterface == 1)
		free(gWindowName);
}



BOOL gInKey = FALSE;
int gCurChar = 0;

int CheckVirtualKeyCode (int keycode)
{
	switch (keycode)
	{
		case GDK_Up:		return kbUp;
		case GDK_Down:		return kbDown;
		case GDK_Left:		return kbLeft;
		case GDK_Right:		return kbRight;
		case GDK_Page_Up:	return kbPgUp;
		case GDK_Page_Down: return kbPgDown;
		case GDK_End:		return kbEnd;
		case GDK_Begin:		return kbBegin;
		case GDK_BackSpace:	return kbBackSpace;
		case GDK_Delete:	return kbDelete;
		case GDK_Tab:		return kbTab;
		case GDK_Return:	return kbEnter;
		case GDK_Escape:	return kbEscape;
		case GDK_Help:		return kbHelp;
		case GDK_F1:		return kbF1;
		case GDK_F2:		return kbF2;
		case GDK_F3:		return kbF3;
		case GDK_F4:		return kbF4;
		case GDK_F5:		return kbF5;
		case GDK_F6:		return kbF6;
		case GDK_F7:		return kbF7;
		case GDK_F8:		return kbF8;
		case GDK_F9:		return kbF9;
		case GDK_F10:		return kbF10;
		case GDK_F11:		return kbF11;
		case GDK_F12:		return kbF12;
		case GDK_Clear:		return kbClear;
	}

	return 0;
}

gboolean frame_key_press_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	int c;
	int modifiers = 0;
	
	if (gDocumentInterface == 1)
		widget = gClientWidget;
	else
		widget = gtk_notebook_get_nth_page(GTK_NOTEBOOK(gClientWidget),
		                                                       gtk_notebook_get_current_page(GTK_NOTEBOOK(gClientWidget)));

	if (!widget)
		return gtk_false();

	if (event->state & GDK_SHIFT_MASK) modifiers |= shiftBIT;
	if (event->state & GDK_CONTROL_MASK) modifiers |= ctrlBIT;
	if (event->state & GDK_MOD1_MASK) modifiers |= altBIT;

	if (event->length > 0)
	{
		c = event->string[0];
		if (modifiers & altBIT) c += 256;
	}
	else
		c = CheckVirtualKeyCode(event->keyval);

	if (!c) return gtk_false();

	if (gInKey)

	{
		if (gCurChar == c)
			handleWindowKeyboard(widget, evKeyStillDown, gCurChar, modifiers);
		else
		{
			handleWindowKeyboard(widget, evKeyUp, gCurChar, modifiers);
			gCurChar = c;
			handleWindowKeyboard(widget, evKeyDown, gCurChar, modifiers);
		}
	}
	else
	{
		gCurChar = c;
		handleWindowKeyboard(widget, evKeyDown, gCurChar, modifiers);
		gInKey = TRUE;
	}

	return gtk_false();
};

gboolean frame_key_release_handler(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
	if (gInKey)
	{
		int modifiers = 0;

		if (gDocumentInterface == 1)
			widget = gClientWidget;
		else
			widget = gtk_notebook_get_nth_page(GTK_NOTEBOOK(gClientWidget),
			                                                       gtk_notebook_get_current_page(GTK_NOTEBOOK(gClientWidget)));

		if (!widget)
			return gtk_false();

		if (event->state & GDK_SHIFT_MASK) modifiers |= shiftBIT;
		if (event->state & GDK_CONTROL_MASK) modifiers |= ctrlBIT;
		if (event->state & GDK_MOD1_MASK) modifiers |= altBIT;

		handleWindowKeyboard(widget, evKeyUp, gCurChar, modifiers);
		gInKey = FALSE;
		gCurChar = 0;
	}

	return gtk_false();
};

static void sdiframe_focus_in_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	if (gClientWidget)
	  handleWindowActivate(gClientWidget);
}

static void sdiframe_focus_out_handler(GtkWidget *widget, GdkEventFocus *event, gpointer user_data)
{
	if (gClientWidget)
	{
		if (gInKey)
		{
			handleWindowKeyboard(gClientWidget, evKeyUp, gCurChar, 0);

			gInKey = FALSE;
			gCurChar = 0;
		}

		handleWindowDeactivate(gClientWidget);
	}
}


void createMDIFrame()
{
	GtkWidget *notebook_menu, *menu_item, *pages_menu;
	GtkWidget *statusBar;
	GSList *group;

	/* Create the window. */
	gFrameWidget = gnome_app_new(gAppName, gAppTitle);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "delete-event",
			GTK_SIGNAL_FUNC(frame_delete_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "destroy",
			GTK_SIGNAL_FUNC(frame_destroy_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "key-press-event",
			GTK_SIGNAL_FUNC(frame_key_press_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "key-release-event",
			GTK_SIGNAL_FUNC(frame_key_release_handler),
			NULL);

	/* Create the menubar. */
	gMenuBar = gtk_menu_bar_new();
	gnome_app_set_menus(GNOME_APP(gFrameWidget), GTK_MENU_BAR(gMenuBar));

	/* Create client(notebook) */
	gClientWidget = gtk_notebook_new();
	gtk_notebook_set_scrollable(GTK_NOTEBOOK(gClientWidget), gtk_true());
	gtk_notebook_popup_enable(GTK_NOTEBOOK(gClientWidget));
	gtk_signal_connect (GTK_OBJECT(gClientWidget), "switch-page",
			GTK_SIGNAL_FUNC(frame_switch_page_handler),
			NULL);
	gnome_app_set_contents(GNOME_APP(gFrameWidget),gClientWidget);

	gtk_window_maximize(GTK_WINDOW(gFrameWidget));
	gtk_widget_show_all(gFrameWidget);

	gnome_app_enable_layout_config (GNOME_APP(gFrameWidget), TRUE);

	/* Create "Pages" menu */
	pages_menu = gtk_menu_new();

	menu_item = gtk_menu_item_new_with_label("Pages");
	gtk_menu_item_set_submenu(GTK_MENU_ITEM (menu_item), pages_menu);
	gtk_widget_show_all(menu_item);

	gtk_menu_bar_insert(GTK_MENU_BAR(gMenuBar), menu_item, 0);

	notebook_menu = gtk_menu_new();

	menu_item = gtk_radio_menu_item_new_with_label(NULL, "Top");
	gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(menu_item), gtk_true());
	group = gtk_radio_menu_item_get_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_top_handler), gClientWidget);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_radio_menu_item_new_with_label(group, "Bottom");
	group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_bottom_handler), gClientWidget);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_radio_menu_item_new_with_label(group, "Left");
	group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_left_handler), gClientWidget);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_radio_menu_item_new_with_label(group, "Right");
	group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menu_item));
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_notebook_right_handler), gClientWidget);
	gtk_menu_append(GTK_MENU(notebook_menu), menu_item);

	menu_item = gtk_menu_item_new_with_label("Notebook");
	gtk_menu_item_set_submenu(GTK_MENU_ITEM (menu_item), notebook_menu);
	gtk_menu_append(GTK_MENU(pages_menu), menu_item);

	menu_item = gtk_menu_item_new();
	gtk_menu_append(GTK_MENU(pages_menu), menu_item);

	menu_item = gtk_menu_item_new_with_label("Close page");
	gtk_signal_connect_object (GTK_OBJECT (menu_item), "activate",
		GTK_SIGNAL_FUNC (frame_close_page_handler), gClientWidget);
	gtk_menu_append(GTK_MENU(pages_menu), menu_item);

	gtk_window_add_accel_group (GTK_WINDOW (gFrameWidget), gtk_accel_group_new());
	
	gtk_widget_show_all(pages_menu);
	
	
	statusBar = gnome_appbar_new(FALSE, TRUE, GNOME_PREFERENCES_NEVER);
	gnome_app_set_statusbar(GNOME_APP(gFrameWidget), statusBar);
 	gtk_widget_hide(statusBar);
}

static gboolean sdiframe_configure_event_handler(GtkWidget *widget, GdkEventConfigure *event, gpointer user_data)
{
	gFrameWidth  = event->width;
	gFrameHeight = event->height;
	return FALSE;
};

void createSDIFrame()
{
	GtkWidget *statusBar;
	
	/* Create the window. */
	gFrameWidget = gnome_app_new(gAppName, gAppTitle);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "delete-event",
			GTK_SIGNAL_FUNC(frame_delete_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "destroy",
			GTK_SIGNAL_FUNC(frame_destroy_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "key-press-event",
			GTK_SIGNAL_FUNC(frame_key_press_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "key-release-event",
			GTK_SIGNAL_FUNC(frame_key_release_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "focus-in-event",
			GTK_SIGNAL_FUNC(sdiframe_focus_in_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "focus-out-event",
			GTK_SIGNAL_FUNC(sdiframe_focus_out_handler),
			NULL);
	gtk_signal_connect (GTK_OBJECT(gFrameWidget), "configure-event",
			GTK_SIGNAL_FUNC(sdiframe_configure_event_handler),
			NULL);

	gtk_window_get_size(GTK_WINDOW(gFrameWidget), &gFrameWidth, &gFrameHeight);

	gtk_window_add_accel_group (GTK_WINDOW (gFrameWidget), gtk_accel_group_new());

	gtk_widget_show_all(gFrameWidget);

	gnome_app_enable_layout_config (GNOME_APP(gFrameWidget), TRUE);
	
	
	statusBar = gnome_appbar_new(FALSE, TRUE, GNOME_PREFERENCES_NEVER);
	gnome_app_set_statusbar(GNOME_APP(gFrameWidget), statusBar);
 	gtk_widget_hide(statusBar);
}
