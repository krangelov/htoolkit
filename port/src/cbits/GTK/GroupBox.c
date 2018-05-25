#include "GroupBox.h"
#include "Internals.h"
#include "Handlers_stub.h"
#include "Window.h"

WindowHandle osCreateGroupBox(WindowHandle window)
{
	GtkWidget *groupbox, *layout;

	layout = port_layout_new(NULL,NULL);
	groupbox = gtk_frame_new("");
	gtk_container_add(GTK_CONTAINER(groupbox), layout);
	gtk_widget_show(layout);
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), groupbox);

	return groupbox;
};

void osGetGroupBoxBordersSize(WindowHandle groupbox, int *res)
{
	GtkRequisition requisition;

 	gtk_widget_size_request (GTK_FRAME(groupbox)->label_widget, &requisition);

	res[0] = 0;
	res[1] = 0;
	res[2] = (GTK_CONTAINER (groupbox)->border_width + GTK_WIDGET (groupbox)->style->xthickness) * 2;
	res[3] = MAX (0, requisition.height - GTK_WIDGET (groupbox)->style->xthickness) +
                                    (GTK_CONTAINER (groupbox)->border_width + GTK_WIDGET (groupbox)->style->ythickness) * 2;
}

char *osGetGroupBoxText(WindowHandle groupbox)
{
	return strdup(gtk_frame_get_label(GTK_FRAME(groupbox)));
};

void osSetGroupBoxText(WindowHandle groupbox, char *txt)
{
	  gtk_frame_set_label(GTK_FRAME(groupbox), txt);
};
