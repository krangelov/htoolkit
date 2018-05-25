#include "CommonDialogs.h"
#include "Internals.h"

BOOL osRunColorDialog(unsigned int *color, WindowHandle owner)
{
	gint result;
	GdkColor c;
	GtkWidget *color_dialog;

	color_dialog = gtk_color_selection_dialog_new("Color");
	
	if (owner)
		owner = gtk_widget_get_toplevel(owner);
	else
		owner = gFrameWidget;
	gtk_window_set_transient_for(GTK_WINDOW(color_dialog), GTK_WINDOW(owner));

	result = gtk_dialog_run(GTK_DIALOG(color_dialog));
	gtk_color_selection_get_current_color(GTK_COLOR_SELECTION(GTK_COLOR_SELECTION_DIALOG(color_dialog)->colorsel), &c);
	gtk_widget_destroy (color_dialog);

	if (result != GTK_RESPONSE_OK)
		return FALSE;

	*color = (c.red/257) | (c.green/257 << 8) | (c.blue/257 << 16);
	return TRUE;
}
