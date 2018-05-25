#include "CommonDialogs.h"
#include "Internals.h"

BOOL osRunFontDialog(char **fname, int *fsize, int *fweight, int *fstyle, BOOL *funderline, BOOL *fstrikeout, WindowHandle owner)
{
	GtkWidget *font_dialog;

	font_dialog = gtk_font_selection_dialog_new("Font");
	
	if (owner)
		owner = gtk_widget_get_toplevel(owner);
	else
		owner = gFrameWidget;
	gtk_window_set_transient_for(GTK_WINDOW(font_dialog), GTK_WINDOW(owner));

	if (gtk_dialog_run(GTK_DIALOG(font_dialog)) == GTK_RESPONSE_OK)
	{
		const gchar *font_name;
		PangoFontDescription *font_descr;

		font_name = gtk_font_selection_dialog_get_font_name(GTK_FONT_SELECTION_DIALOG(font_dialog));
		font_descr = pango_font_description_from_string(font_name);

		*fname = strdup(pango_font_description_get_family(font_descr));
		*fsize = pango_font_description_get_size(font_descr)/PANGO_SCALE;
		*fweight = pango_font_description_get_weight(font_descr);

		switch (pango_font_description_get_style(font_descr))
		{
		case PANGO_STYLE_NORMAL:  *fstyle =  0; break;
		case PANGO_STYLE_OBLIQUE: *fstyle =  1; break;
		case PANGO_STYLE_ITALIC:  *fstyle =  2; break;
		default:                  *fstyle = -1; break;
		}

		*funderline = FALSE;
		*fstrikeout = FALSE;

		pango_font_description_free(font_descr);

		return TRUE;
	}
	gtk_widget_destroy (font_dialog);

	return FALSE;
}
