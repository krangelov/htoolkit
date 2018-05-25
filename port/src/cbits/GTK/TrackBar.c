#include "TrackBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

static void track_increment(GtkWidget *widget, gpointer data)
{
	handleTrackBarIncrement((WindowHandle) data);
}

static void track_decrement(GtkWidget *widget, gpointer data)
{
	handleTrackBarDecrement((WindowHandle) data);
}

WindowHandle osCreateHorzTrackBar(WindowHandle window)
{
	GtkWidget *trackBar;
	GtkWidget *left_button, *right_button;

	trackBar = gtk_hbox_new(TRUE, 0);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), trackBar);

	left_button = gtk_button_new();
	gtk_container_set_border_width(GTK_CONTAINER(left_button), 0);
	gtk_signal_connect (GTK_OBJECT(left_button), "clicked",
			GTK_SIGNAL_FUNC(track_decrement),
			trackBar);
	gtk_container_add(GTK_CONTAINER(left_button),  gtk_arrow_new(GTK_ARROW_LEFT,GTK_SHADOW_NONE));
	gtk_widget_show_all(left_button);
	gtk_box_pack_start(GTK_BOX(trackBar),left_button ,TRUE,TRUE,0);

	right_button = gtk_button_new();
	gtk_container_set_border_width(GTK_CONTAINER(right_button), 0);
	gtk_signal_connect (GTK_OBJECT(right_button), "clicked",
			GTK_SIGNAL_FUNC(track_increment),
			trackBar);
	gtk_container_add(GTK_CONTAINER(right_button), gtk_arrow_new(GTK_ARROW_RIGHT,GTK_SHADOW_NONE));
	gtk_widget_show_all(right_button);
	gtk_box_pack_end  (GTK_BOX(trackBar),right_button,TRUE,TRUE,0);

	return trackBar;
};

WindowHandle osCreateVertTrackBar(WindowHandle window)
{
	GtkWidget *trackBar;
	GtkWidget *up_button, *down_button;

	trackBar = gtk_vbox_new(TRUE, 0);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), trackBar);

	up_button = gtk_button_new();
	gtk_signal_connect (GTK_OBJECT(up_button), "clicked",
			GTK_SIGNAL_FUNC(track_increment),
			trackBar);
	gtk_container_add(GTK_CONTAINER(up_button),  gtk_arrow_new(GTK_ARROW_UP,GTK_SHADOW_NONE));
	gtk_widget_show_all(up_button);
	gtk_box_pack_start(GTK_BOX(trackBar),up_button,  TRUE,TRUE,0);

	down_button = gtk_button_new();
	gtk_signal_connect (GTK_OBJECT(down_button), "clicked",
			GTK_SIGNAL_FUNC(track_decrement),
			trackBar);
	gtk_container_add(GTK_CONTAINER(down_button), gtk_arrow_new(GTK_ARROW_DOWN,GTK_SHADOW_NONE));
	gtk_widget_show_all(down_button);
	gtk_box_pack_end  (GTK_BOX(trackBar),down_button,TRUE,TRUE,0);

	return trackBar;
};

void osGetTrackBarReqSize(WindowHandle trackBar, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(trackBar, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}
