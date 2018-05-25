#include "Splitter.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateSplitter(WindowHandle window, BOOL isVert, /* out */ WindowHandle *panes)
{
	GtkWidget *paned;

	if (isVert)
		paned = gtk_vpaned_new();
	else
		paned = gtk_hpaned_new();

	panes[0] = create_generic_window();
	gtk_widget_show_all(panes[0]);
	gtk_paned_add1(GTK_PANED(paned), panes[0]);
	gtk_container_child_set(GTK_CONTAINER(paned), panes[0], "shrink", FALSE, NULL);

	panes[1] = create_generic_window();
	gtk_widget_show_all(panes[1]);
	gtk_paned_add2(GTK_PANED(paned), panes[1]);
	gtk_container_child_set(GTK_CONTAINER(paned), panes[1], "shrink", FALSE, NULL);

	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), paned);

	return paned;
}

void osGetSplitterReqSize(WindowHandle splitter, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(splitter, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}

void osGetSplitterRange(WindowHandle splitter, int *range)
{
	range[0] = GTK_PANED(splitter)->min_position;
	range[1] = GTK_PANED(splitter)->max_position;
}

void osSetSplitterPosition(WindowHandle splitter, int pos)
{
	gtk_paned_set_position(GTK_PANED(splitter), pos);
}

int osGetSplitterPosition(WindowHandle splitter)
{
	return gtk_paned_get_position(GTK_PANED(splitter));
}
