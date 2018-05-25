#include "StatusBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

void osSetStatusBarVisible(BOOL visible)
{
	if (visible)
		gtk_widget_show(GNOME_APP(gFrameWidget)->statusbar);
	else
		gtk_widget_hide(GNOME_APP(gFrameWidget)->statusbar);
}

BOOL osGetStatusBarVisible()
{
	return GTK_WIDGET_VISIBLE(GNOME_APP(gFrameWidget)->statusbar);
}

void osPushStatusBarContext(char *title)
{
	  gnome_appbar_push(GNOME_APPBAR(GNOME_APP(gFrameWidget)->statusbar), title);
}

void osPopStatusBarContext()
{
	gnome_appbar_pop(GNOME_APPBAR(GNOME_APP(gFrameWidget)->statusbar));
}

char *osGetStatusBarTitle()
{
	GtkWidget *label = gnome_appbar_get_status(GNOME_APPBAR(GNOME_APP(gFrameWidget)->statusbar));
	return strdup(gtk_label_get_text(GTK_LABEL(label)));
}

void osSetStatusBarTitle(char *title)
{
	gnome_appbar_set_status(GNOME_APPBAR(GNOME_APP(gFrameWidget)->statusbar), title);
}

int osGetStatusBarIndicatorsCount()
{
	int count;
	GList *children;

	children = gtk_container_get_children(GTK_CONTAINER(GNOME_APP(gFrameWidget)->statusbar));
	count = g_list_length(children);
	g_list_free (children);

	return count-1;
}

static void calc_pack_end_count(GtkWidget *child, gpointer data)
{
	gboolean expand;
	gboolean fill;
	guint padding;
	GtkPackType pack_type;

	 gtk_box_query_child_packing(GTK_BOX(gtk_widget_get_parent(child)), child,
                                             &expand, &fill, &padding, &pack_type);

	if (pack_type == GTK_PACK_END)
		(*((int *) data))++;
}

static gboolean indicator_button_press_handler(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
	if (event->type == GDK_2BUTTON_PRESS && event->button == 1)
		handleIndicatorCommand((IndicatorHandle) user_data);

	return TRUE;
}


IndicatorHandle osCreateIndicator(int index)
{
	GtkWidget *label, *frame, *ebox;

	if (index >= 0)
	{
		int count = 0;
		gtk_container_foreach(GTK_CONTAINER(GNOME_APP(gFrameWidget)->statusbar), calc_pack_end_count, &count);
		index = count-index;
	}
	else
		index = 0;

	// create the frame for the indicator
	frame = gtk_frame_new (NULL);
	gtk_frame_set_shadow_type (GTK_FRAME(frame), GTK_SHADOW_IN);
	gtk_signal_connect (GTK_OBJECT(frame), "destroy",
		GTK_SIGNAL_FUNC(handleIndicatorDestroy),
		NULL);

	// create the event box inside the frame
	ebox = gtk_event_box_new();
	gtk_widget_set_events (ebox, GDK_BUTTON2_MOTION_MASK);
	gtk_signal_connect (GTK_OBJECT(ebox), "button-press-event",
			GTK_SIGNAL_FUNC(indicator_button_press_handler),
			frame);
	gtk_container_add (GTK_CONTAINER(frame), ebox);

	// create the label inside the event box
	label = gtk_label_new("");
	gtk_misc_set_alignment (GTK_MISC(label), 0.0, 0.0);
	gtk_container_add (GTK_CONTAINER(ebox), label);

	gtk_box_pack_end(GTK_BOX(GNOME_APP(gFrameWidget)->statusbar), frame, FALSE, TRUE, 0);
	gtk_box_reorder_child(GTK_BOX(GNOME_APP(gFrameWidget)->statusbar), frame, index);
	gtk_widget_show_all(frame);

	return frame;
}

void osDestroyIndicator(IndicatorHandle indicator)
{
	gtk_widget_destroy(indicator);
}

char *osGetIndicatorTitle(IndicatorHandle indicator)
{
	return strdup(gtk_label_get_text(GTK_LABEL(GTK_BIN(GTK_BIN(indicator)->child)->child)));
}

void osSetIndicatorTitle(IndicatorHandle indicator, char *title)
{
	gtk_label_set_text(GTK_LABEL(GTK_BIN(GTK_BIN(indicator)->child)->child), title);
}

int osGetIndicatorPos(IndicatorHandle indicator)
{
	int pos;
	GList *children;

	pos = 0;
	for (children = gtk_container_get_children(GTK_CONTAINER(GNOME_APP(gFrameWidget)->statusbar));
	      children != NULL;
	      children = g_list_delete_link(children, children))
	{
		if ((IndicatorHandle) children->data == indicator)
			break;
		pos++;
	}

 	g_list_free (children);

	return pos-1;
}
