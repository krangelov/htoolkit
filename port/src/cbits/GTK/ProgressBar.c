#include "ProgressBar.h"
#include "Internals.h"
#include "Handlers_stub.h"

WindowHandle osCreateHorzProgressBar(WindowHandle window, BOOL bSmooth)
{
	GtkWidget *bar;

	bar = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(bar), GTK_PROGRESS_LEFT_TO_RIGHT);
	gtk_progress_bar_set_bar_style(GTK_PROGRESS_BAR(bar), bSmooth ? GTK_PROGRESS_CONTINUOUS : GTK_PROGRESS_DISCRETE);

	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), bar);

	return bar;
};

WindowHandle osCreateVertProgressBar(WindowHandle window, BOOL bSmooth)
{
	GtkWidget *bar;

	bar = gtk_progress_bar_new();
	gtk_progress_bar_set_orientation(GTK_PROGRESS_BAR(bar), GTK_PROGRESS_BOTTOM_TO_TOP);
	gtk_progress_bar_set_bar_style(GTK_PROGRESS_BAR(bar), bSmooth ? GTK_PROGRESS_CONTINUOUS : GTK_PROGRESS_DISCRETE);

	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), bar);

	return bar;
};

void osGetProgressBarReqSize(WindowHandle bar, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(bar, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}

void osSetProgressBarFraction(WindowHandle bar, int minPos, int maxPos, int pos)
{
	if (maxPos!=minPos)
	{
		gdouble fraction = ((gdouble) pos)/((gdouble) (maxPos-minPos));
		gtk_progress_bar_set_fraction(GTK_PROGRESS_BAR(bar), fraction);
	}

	gtk_progress_bar_set_discrete_blocks(GTK_PROGRESS_BAR(bar), maxPos-minPos);
}

int osGetProgressBarFraction(WindowHandle bar, int minPos, int maxPos)
{
	return floor(gtk_progress_bar_get_fraction(GTK_PROGRESS_BAR(bar))*(maxPos-minPos)+0.5);;
}
