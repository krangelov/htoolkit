#include "Label.h"
#include "Internals.h"
#include "Handlers_stub.h"


WindowHandle osCreateLabel(WindowHandle window)
{
	GtkWidget *text;

	text = gtk_label_new("");
	gtk_label_set_line_wrap(GTK_LABEL(text), gtk_false());
	port_layout_put(PORT_LAYOUT(GTK_BIN(window)->child), text);

	return text;
};

void osGetLabelReqSize(WindowHandle label, int *res)
{
	GtkRequisition requisition;

	gtk_widget_size_request(label, &requisition);

	res[0] = requisition.width;
	res[1] = requisition.height;
}

char *osGetLabelText(WindowHandle label)
{
  	return fromMnemonicString(gtk_label_get_text(GTK_LABEL(label)));
};

void osSetLabelText(WindowHandle label, char *txt)
{
	gchar *szText = toMnemonicString(txt);
	gtk_label_set_text_with_mnemonic(GTK_LABEL(label), szText);
	rfree(szText);
	osForceContainerReLayout(label);
};

void osChangeLabelFont(WindowHandle label, FontHandle font)
{
	gtk_widget_modify_font(label, font->font_descr);
	osForceContainerReLayout(label);
};
